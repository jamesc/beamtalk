#!/usr/bin/env python3
"""Convert stdlib expression tests (// =>) to TestCase/BUnit format.

Usage: python3 convert_tests.py [--dry-run] [file.bt ...]
  Without args: converts all non-bootstrap files in tests/stdlib/
  With --dry-run: prints output without writing files
"""

import os
import re
import sys
import textwrap

# Bootstrap-critical files that must remain as expression tests
BOOTSTRAP_FILES = {
    'arithmetic.bt', 'booleans.bt', 'equality.bt', 'errors.bt', 'exceptions.bt',
    'custom_exceptions.bt', 'erlang_exceptions.bt', 'integer_test.bt', 'float.bt',
    'string_methods.bt', 'string_ops.bt', 'symbol.bt', 'literals.bt',
    'object_methods.bt', 'value_types.bt',
}

# Files where bare word results are strings (from string operations)
STRING_CONTEXT_FILES = {
    'string_interpolation.bt', 'string_new_methods.bt', 'string_quote_escape.bt',
    'unicode.bt', 'regex.bt', 'file_io.bt', 'file_stream.bt',
    'language_features_doc.bt', 'object_reflection.bt', 'protoobject_manual_test.bt',
    'object_protocol.bt',
}

# Known Beamtalk class names (for distinguishing from string values)
KNOWN_CLASSES = {
    'Integer', 'Float', 'Number', 'String', 'Symbol', 'Boolean', 'True', 'False',
    'UndefinedObject', 'Object', 'Block', 'Metaclass', 'ProtoObject',
    'Dictionary', 'Set', 'Array', 'Collection', 'OrderedCollection',
    'Character', 'DateTime', 'Regex', 'Random', 'Reference', 'Port', 'Pid',
    'Association', 'Tuple', 'Stream', 'TestCase', 'TestResult', 'TestRunner',
    'Error', 'Exception', 'Actor', 'Future', 'System', 'File', 'FileStream',
    'Behaviour', 'Class', 'CompiledMethod', 'Magnitude', 'SmallInteger',
    'LargePositiveInteger', 'LargeNegativeInteger', 'Fraction',
    'Counter', 'MathHelper', 'RecursiveActor', 'ClassVarCounter',
    'SealedCounter', 'Validator', 'Calculator', 'Point',
}


def escape_beamtalk_string(s):
    """Escape a string for use as a Beamtalk string literal.
    
    In Beamtalk:
    - Double quotes are escaped by doubling: ""
    - Curly braces are escaped with backslash: \\{ and \\}
    """
    s = s.replace('"', '""')
    s = s.replace('{', '\\{')
    s = s.replace('}', '\\}')
    return f'"{s}"'


DISPLAY_PATTERNS = [
    re.compile(r'^a [A-Z]'),           # a Counter, a Block, a TestCase
    re.compile(r'.+ -> .+'),           # 42 -> answer (Association)
    re.compile(r'^Set\('),             # Set(1, 2, 3)
    re.compile(r'^Stream\('),          # Stream(from: 1)
    re.compile(r'^Regex\('),           # Regex([0-9]+)
    re.compile(r'^#Actor<'),           # #Actor<Counter,_>
    re.compile(r'^#Future<'),          # #Future<_>
    re.compile(r'^#ErlangModule<'),    # #ErlangModule<lists>
    re.compile(r'^a Block'),           # a Block (without /N)
    re.compile(r'^getValue =>'),       # getValue => ^self.value (source display)
    re.compile(r"^File\."),            # File.lines(...)
]


def classify_expected(val, source_file, expression=''):
    """Classify an expected value and return (kind, beamtalk_expr).
    
    Kinds: 'equals', 'assert_true', 'deny', 'error', 'wildcard'
    """
    val = val.strip()
    expr = expression.strip()
    
    # Wildcard
    if val == '_':
        return ('wildcard', None)
    
    # Error assertion
    if val.startswith('ERROR:'):
        kind = val[6:].strip()
        return ('error', f'#{kind}')
    
    # Boolean
    if val == 'true':
        return ('assert_true', None)
    if val == 'false':
        return ('deny', None)
    
    # nil
    if val == 'nil':
        return ('equals', 'nil')
    
    # Number (integer or float)
    if re.match(r'^-?\d+$', val):
        return ('equals', val)
    if re.match(r'^-?\d+\.\d+$', val):
        return ('equals', val)
    
    # Symbol (starts with # but not #{ or #( or display patterns like #Actor, #ErlangModule)
    if val.startswith('#') and not val.startswith('#{') and not val.startswith('#(') \
       and not val.startswith('#Actor') and not val.startswith('#Future') \
       and not val.startswith('#MapSet') and not val.startswith('#Set') \
       and not val.startswith('#ErlangModule'):
        return ('equals', val)
    
    # List display [1,2,3] → convert to Beamtalk array literal #(1, 2, 3)
    if val.startswith('['):
        bt_val = convert_list_to_array(val)
        if bt_val is not None:
            return ('equals', bt_val)
        return ('wildcard', None)
    
    # Map display #{...} → use as-is (valid Beamtalk syntax)
    if val.startswith('#{'):
        return ('equals', val)
    if val == '#{}':
        return ('equals', val)
    
    # Tuple display {1,2} → wildcard (complex conversion)
    if val.startswith('{'):
        return ('wildcard', None)
    
    # Display-only patterns
    for pattern in DISPLAY_PATTERNS:
        if pattern.match(val):
            return ('wildcard', None)
    
    # Class names — detect based on expression context
    if re.match(r'^[A-Z][a-zA-Z]*$', val):
        # Expression ends with 'class' → returns class object
        if re.search(r'\bclass$', expr):
            return ('equals', val)
        # Expression IS a class name (class reference) → class object
        if re.match(r'^[A-Z][a-zA-Z]*$', expr):
            return ('equals', val)
        # Expression uses subclasses/superclass → class
        if re.search(r'\b(superclass|subclasses|allSubclasses)\b', expr):
            return ('equals', val)
        # Known class in other context (e.g., method return) → class object
        if val in KNOWN_CLASSES:
            return ('equals', escape_beamtalk_string(val))
        # Otherwise treat the result as a string (e.g., from printString)
        return ('equals', escape_beamtalk_string(val))
    
    # Quoted string content (starts with " — rare, from string_quote_escape.bt)
    if val.startswith('"'):
        return ('equals', escape_beamtalk_string(val))
    
    # Empty string (empty expected value after // =>)
    if val == '':
        return ('equals', '""')
    
    # Block display (a Block/N) - already handled by DISPLAY_PATTERNS
    if 'Block/' in val:
        return ('wildcard', None)
    
    # Bare words and phrases
    fname = os.path.basename(source_file)
    is_string_context = fname in STRING_CONTEXT_FILES
    
    if is_string_context:
        return ('equals', escape_beamtalk_string(val))
    
    # Single lowercase word — check if it's likely an atom or string
    if re.match(r'^[a-z_][a-zA-Z0-9_]*$', val):
        # Error kind returns symbol
        if re.search(r'\bkind\b', expr):
            return ('equals', f'#{val}')
        # Default: treat as string (format_result displays binaries without quotes)
        return ('equals', escape_beamtalk_string(val))
    
    # Multi-word phrases → string
    return ('equals', escape_beamtalk_string(val))


def convert_list_to_array(list_str):
    """Convert Erlang list display [1,2,3] to Beamtalk array #(1, 2, 3)."""
    if list_str == '[]':
        return '#()'
    
    # Simple case: [elem1, elem2, ...] with basic elements
    if not list_str.startswith('[') or not list_str.endswith(']'):
        return None
    
    inner = list_str[1:-1]
    
    # Handle nested structures by tracking bracket depth
    elements = split_balanced(inner, ',')
    if elements is None:
        return None
    
    converted = []
    for elem in elements:
        elem = elem.strip()
        c = convert_element(elem)
        if c is None:
            return None
        converted.append(c)
    
    return f'#({", ".join(converted)})'


def convert_element(elem):
    """Convert a single element from Erlang display to Beamtalk literal."""
    elem = elem.strip()
    
    # Number
    if re.match(r'^-?\d+(\.\d+)?$', elem):
        return elem
    
    # Boolean/nil
    if elem in ('true', 'false', 'nil'):
        return elem
    
    # Quoted string (with Erlang quotes) — need to escape braces for Beamtalk
    if elem.startswith('"') and elem.endswith('"'):
        inner = elem[1:-1]
        inner = inner.replace('"', '""')
        inner = inner.replace('{', '\\{')
        inner = inner.replace('}', '\\}')
        return f'"{inner}"'
    
    # Symbol (already has #)
    if elem.startswith('#') and not elem.startswith('#{') and not elem.startswith('#('):
        return elem
    
    # Nested list
    if elem.startswith('['):
        return convert_list_to_array(elem)
    
    # Nested map
    if elem.startswith('#{'):
        return elem
    
    # Bare atom (lowercase word) in list context
    if re.match(r'^[a-z_][a-zA-Z0-9_]*$', elem):
        return f'#{elem}'
    
    # Can't convert
    return None


def split_balanced(s, sep):
    """Split string by separator respecting bracket nesting."""
    result = []
    current = []
    depth = 0
    in_string = False
    
    i = 0
    while i < len(s):
        c = s[i]
        if c == '"' and not in_string:
            in_string = True
            current.append(c)
        elif c == '"' and in_string:
            # Check for doubled delimiter
            if i + 1 < len(s) and s[i + 1] == '"':
                current.append(c)
                current.append('"')
                i += 2
                continue
            in_string = False
            current.append(c)
        elif in_string:
            current.append(c)
        elif c in '([{':
            depth += 1
            current.append(c)
        elif c in ')]}':
            depth -= 1
            current.append(c)
        elif c == sep and depth == 0:
            result.append(''.join(current))
            current = []
        else:
            current.append(c)
        i += 1
    
    if current or result:
        result.append(''.join(current))
    
    return result


def needs_parens(expr):
    """Check if an expression needs wrapping in parens for assert:equals:.
    
    In Beamtalk, keyword messages in the expression would conflict with
    the assert:equals: keyword message. Unary and binary messages are fine.
    """
    expr = expr.strip()
    
    # Simple variable reference
    if re.match(r'^[a-z_][a-zA-Z0-9_]*$', expr):
        return False
    
    # Simple literal (number, boolean, nil)
    if re.match(r'^-?\d+(\.\d+)?$', expr):
        return False
    if expr in ('true', 'false', 'nil'):
        return False
    
    # String literal
    if expr.startswith('"') and expr.endswith('"'):
        return False
    
    # Symbol literal
    if re.match(r'^#[a-zA-Z_][a-zA-Z0-9_]*$', expr):
        return False
    
    # Already wrapped in parens
    if expr.startswith('(') and expr.endswith(')'):
        # Check balanced
        depth = 0
        for c in expr:
            if c == '(': depth += 1
            elif c == ')': depth -= 1
            if depth == 0 and c == ')':
                break
        if depth == 0:
            return False
    
    # Class name (single uppercase word)
    if re.match(r'^[A-Z][a-zA-Z]*$', expr):
        return False
    
    # Check if expression contains keyword messages (word followed by colon)
    # This would conflict with assert:equals:
    # Scan for colon-terminated words outside strings and brackets
    if contains_keyword_message(expr):
        return True
    
    # Pure array or map literal (check balanced brackets)
    if expr.startswith('#(') and is_balanced_brackets(expr, '(', ')'):
        return False
    if expr.startswith('#{') and is_balanced_brackets(expr, '{', '}'):
        return False
    
    # Everything else needs parens to be safe
    return True


def contains_keyword_message(expr):
    """Check if expression contains a keyword message (word:) outside strings/brackets."""
    in_string = False
    depth = 0
    i = 0
    while i < len(expr):
        c = expr[i]
        if c == '"':
            if in_string:
                if i + 1 < len(expr) and expr[i + 1] == '"':
                    i += 2
                    continue
                in_string = False
            else:
                in_string = True
        elif not in_string:
            if c in '([{':
                depth += 1
            elif c in ')]}':
                depth -= 1
            elif c == ':' and depth == 0:
                # Check if preceded by a word character (keyword message)
                if i > 0 and (expr[i-1].isalnum() or expr[i-1] == '_'):
                    # But not := (assignment)
                    if i + 1 < len(expr) and expr[i + 1] == '=':
                        pass  # := assignment, not keyword
                    else:
                        return True
        i += 1
    return False


def is_balanced_brackets(expr, open_ch, close_ch):
    """Check if expression is a single balanced bracket expression."""
    if not expr.startswith('#' + open_ch):
        return False
    depth = 0
    in_string = False
    for i, c in enumerate(expr):
        if c == '"':
            in_string = not in_string
        elif not in_string:
            if c == open_ch:
                depth += 1
            elif c == close_ch:
                depth -= 1
                if depth == 0:
                    return i == len(expr) - 1
    return False


def wrap_expr(expr):
    """Wrap expression in parens if needed."""
    if needs_parens(expr):
        return f'({expr})'
    return expr


def make_block_expr(expr):
    """Wrap expression in a block for should:raise:."""
    return f'[{expr}]'


def parse_test_file(filepath):
    """Parse a stdlib expression test file into structured data."""
    with open(filepath) as f:
        lines = f.readlines()
    
    result = {
        'copyright': [],
        'description': [],
        'load_files': [],
        'sections': [],  # list of {'name': str, 'items': [{'expr': str, 'expected': str, 'comment': str}]}
    }
    
    current_section = {'name': None, 'items': [], 'comments': []}
    i = 0
    
    while i < len(lines):
        line = lines[i].rstrip('\n')
        stripped = line.strip()
        
        # Copyright header (first few lines)
        if i < 3 and (stripped.startswith('// Copyright') or stripped.startswith('// SPDX')):
            result['copyright'].append(stripped)
            i += 1
            continue
        
        # Empty line
        if not stripped:
            i += 1
            continue
        
        # @load directive
        if stripped.startswith('// @load'):
            path = stripped[8:].strip()
            if path:
                result['load_files'].append(path)
            i += 1
            continue
        
        # Section header (=== ... ===)
        if stripped.startswith('// ==') and '===' in stripped:
            i += 1
            # Next line should be the section name
            if i < len(lines):
                name_line = lines[i].strip()
                if name_line.startswith('//'):
                    name = name_line.lstrip('/ ').strip()
                    if current_section['items'] or current_section['name']:
                        result['sections'].append(current_section)
                    current_section = {'name': name, 'items': [], 'comments': []}
                    # Skip closing === line
                    i += 1
                    if i < len(lines) and '===' in lines[i]:
                        i += 1
                    continue
            continue
        
        # Description comment (before first section/expression)
        if stripped.startswith('//') and not stripped.startswith('// =>'):
            comment_text = stripped[2:].strip()
            if not result['sections'] and not current_section['items']:
                if not result['description'] or comment_text:
                    result['description'].append(comment_text)
            else:
                current_section['comments'].append(comment_text)
            i += 1
            continue
        
        # Expression line (not a comment, not empty)
        if not stripped.startswith('//'):
            expression = stripped
            expr_comment = ''
            
            # Grab any preceding comments as the expression's description
            if current_section['comments']:
                expr_comment = current_section['comments'][-1]
                current_section['comments'] = current_section['comments'][:-1]
            
            # Look for assertion on next line
            i += 1
            if i < len(lines):
                next_line = lines[i].strip()
                if next_line.startswith('// =>'):
                    expected = next_line[5:].strip()
                    current_section['items'].append({
                        'expr': expression,
                        'expected': expected,
                        'comment': expr_comment,
                    })
                    i += 1
                    continue
            
            # No assertion - orphaned expression (skip)
            continue
        
        i += 1
    
    # Add last section
    if current_section['items'] or current_section['name']:
        result['sections'].append(current_section)
    
    # If no sections were found, create a default one
    if not result['sections']:
        result['sections'] = [{'name': None, 'items': [], 'comments': []}]
    
    return result


def section_to_method_name(section_name, index, used_names):
    """Convert a section name to a test method name."""
    if not section_name:
        name = f'testSection{index + 1}'
    else:
        # Clean up section name
        name = section_name
        # Remove parenthetical references like (BT-524)
        name = re.sub(r'\(BT-\d+[^)]*\)', '', name)
        name = re.sub(r'\(ADR \d+[^)]*\)', '', name)
        name = name.strip()
        
        # Convert to camelCase
        # Replace non-alphanumeric with spaces
        name = re.sub(r'[^a-zA-Z0-9]+', ' ', name)
        words = name.split()
        if not words:
            name = f'testSection{index + 1}'
        else:
            name = 'test' + ''.join(w.capitalize() for w in words)
    
    # Ensure unique
    base = name
    suffix = 2
    while name in used_names:
        name = f'{base}{suffix}'
        suffix += 1
    used_names.add(name)
    
    return name


def wrap_keyword_val(val):
    """Wrap a value if it would be ambiguous as a keyword argument.
    
    Float literals like 1.0 need parens because N.0 is parsed as
    integer N + statement separator + integer 0.
    """
    if re.match(r'^-?\d+\.0$', val):
        return f'({val})'
    return val


def generate_assertion(item, source_file):
    """Generate a TestCase assertion statement from an expression+expected pair."""
    expr = item['expr']
    expected = item['expected']
    
    kind, bt_val = classify_expected(expected, source_file, expr)
    
    if kind == 'wildcard':
        # Just execute the expression
        return expr
    elif kind == 'error':
        return f'self should: {make_block_expr(expr)} raise: {bt_val}'
    elif kind == 'assert_true':
        return f'self assert: {wrap_expr(expr)}'
    elif kind == 'deny':
        return f'self deny: {wrap_expr(expr)}'
    elif kind == 'equals':
        # Check if expression is an assignment
        assign_match = re.match(r'^([a-zA-Z_][a-zA-Z0-9_]*)\s*:=\s*(.+)$', expr)
        wrapped_val = wrap_keyword_val(bt_val)
        if assign_match:
            var_name = assign_match.group(1)
            rhs = assign_match.group(2).strip()
            return f'{var_name} := {rhs}.\nself assert: {var_name} equals: {wrapped_val}'
        else:
            return f'self assert: {wrap_expr(expr)} equals: {wrapped_val}'
    
    return expr


def generate_test_file(parsed, source_file):
    """Generate the TestCase test file content."""
    fname = os.path.basename(source_file)
    base = fname.replace('.bt', '')
    
    # Generate class name
    class_name = to_class_name(base) + 'Test'
    
    # Build file content
    lines = []
    
    # Copyright header
    lines.append('// Copyright 2026 James Casey')
    lines.append('// SPDX-License-Identifier: Apache-2.0')
    lines.append('')
    
    # Description (first non-empty line only)
    if parsed['description']:
        for d in parsed['description']:
            if d:
                lines.append(f'// {d}')
                break
        lines.append('')
    
    # @load directives
    for load_path in parsed['load_files']:
        lines.append(f'// @load {load_path}')
    if parsed['load_files']:
        lines.append('')
    
    # Class definition
    lines.append(f'TestCase subclass: {class_name}')
    lines.append('')
    
    # Generate test methods
    used_names = set()
    for idx, section in enumerate(parsed['sections']):
        if not section['items']:
            continue
        
        method_name = section_to_method_name(section['name'], idx, used_names)
        
        # Method header
        lines.append(f'  {method_name} =>')
        
        # Generate assertions
        for i, item in enumerate(section['items']):
            assertion = generate_assertion(item, source_file)
            
            # Add comment if present
            if item['comment']:
                lines.append(f'    // {item["comment"]}')
            
            # Add assertion with proper statement separator
            is_last = (i == len(section['items']) - 1)
            assertion_lines = assertion.split('\n')
            
            for j, aline in enumerate(assertion_lines):
                is_very_last = is_last and (j == len(assertion_lines) - 1)
                if is_very_last:
                    lines.append(f'    {aline}')
                elif j < len(assertion_lines) - 1:
                    # Multi-line assertion: intermediate lines get period
                    lines.append(f'    {aline}')
                else:
                    # Last line of non-last item: needs period separator
                    lines.append(f'    {aline}.')
        
        lines.append('')
    
    # Remove trailing empty lines
    while lines and lines[-1] == '':
        lines.pop()
    
    return '\n'.join(lines) + '\n'


def to_class_name(base):
    """Convert a file base name to a class name."""
    # Handle special cases
    name_map = {
        'json': 'Json',
        'pid': 'Pid',
        'datetime': 'DateTime',
        'file_io': 'FileIo',
        'file_stream': 'FileStream',
        'regex': 'Regex',
        'unicode': 'Unicode',
        'test_case_test': 'TestCaseAssertion',
    }
    if base in name_map:
        return name_map[base]
    
    # Convert snake_case to CamelCase
    parts = base.split('_')
    return ''.join(p.capitalize() for p in parts)


def output_filename(source_file):
    """Generate the output filename for a test file."""
    fname = os.path.basename(source_file)
    base = fname.replace('.bt', '')
    
    # Handle plurals and conventions
    # test/collection_test.bt (not collections_test.bt)
    if base.endswith('s') and not base.endswith('ss') and base not in (
        'cascades', 'class_variables', 'blocks', 'closures_advanced',
        'stack_frames', 'unary_messages', 'keyword_messages',
    ):
        # Keep as-is for most files
        pass
    
    return f'test/{base}_test.bt'


def main():
    dry_run = '--dry-run' in sys.argv
    args = [a for a in sys.argv[1:] if not a.startswith('--')]
    
    stdlib_dir = 'tests/stdlib'
    
    if args:
        files = args
    else:
        files = sorted(
            os.path.join(stdlib_dir, f)
            for f in os.listdir(stdlib_dir)
            if f.endswith('.bt') and f not in BOOTSTRAP_FILES
        )
    
    total_assertions = 0
    total_files = 0
    
    for filepath in files:
        fname = os.path.basename(filepath)
        if fname in BOOTSTRAP_FILES:
            print(f'SKIP (bootstrap): {fname}')
            continue
        
        parsed = parse_test_file(filepath)
        
        # Count assertions
        assertion_count = sum(len(s['items']) for s in parsed['sections'])
        total_assertions += assertion_count
        
        if assertion_count == 0:
            print(f'SKIP (no assertions): {fname}')
            continue
        
        output = generate_test_file(parsed, filepath)
        outpath = output_filename(filepath)
        
        if dry_run:
            print(f'\n{"="*60}')
            print(f'FILE: {outpath} (from {fname}, {assertion_count} assertions)')
            print(f'{"="*60}')
            print(output)
        else:
            os.makedirs(os.path.dirname(outpath), exist_ok=True)
            with open(outpath, 'w') as f:
                f.write(output)
            print(f'OK: {fname} → {outpath} ({assertion_count} assertions)')
        
        total_files += 1
    
    print(f'\nTotal: {total_files} files, {total_assertions} assertions')


if __name__ == '__main__':
    main()
