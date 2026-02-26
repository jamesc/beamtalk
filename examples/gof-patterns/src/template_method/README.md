# Template Method Pattern — Pluggable Data Export with Beamtalk

*2026-02-26T19:05:56Z by Showboat 0.6.1*
<!-- showboat-id: 8a5fc815-fb99-4f69-ae17-3ee1902637a5 -->

## Intent

The **Template Method** pattern defines the skeleton of an algorithm in a base-class method and lets subclasses fill in the variable steps — without changing the algorithm's overall structure.

In this example, `export:` is the **fixed skeleton**: it always collects formatted rows, joins them, and wraps the result in a header and footer. The three hooks — `header`, `footer`, and `formatRow:` — are declared in the abstract base and overridden by each concrete subclass. Swap the subclass and the output format changes completely; the orchestration logic never moves.

## The Players

Three classes collaborate to make the pattern work.

### DataExporter — abstract base

`DataExporter` owns the template method `export:` and declares the three hooks with `subclassResponsibility`. Calling a hook on the base directly raises an error, making the contract explicit.

```bash
sed -n '12,30p' data_exporter.bt
```

```output
Object subclass: DataExporter

  /// Template method — fixed algorithm skeleton.
  /// Do not override this in subclasses; override the hooks below instead.
  export: rows =>
    formatted := rows collect: [:row | self formatRow: row]
    self header ++ formatted join ++ self footer

  /// Hook: text placed before all rows. Subclasses must override.
  header =>
    self subclassResponsibility

  /// Hook: text placed after all rows. Subclasses must override.
  footer =>
    self subclassResponsibility

  /// Hook: format a single row (a List of cell strings). Subclasses must override.
  formatRow: row =>
    self subclassResponsibility
```

### CsvExporter — concrete subclass for CSV

`CsvExporter` returns empty strings for the header and footer (CSV needs no wrapper) and uses `inject:into:` to comma-join the cells of each row.

```bash
sed -n '12,20p' csv_exporter.bt
```

```output
DataExporter subclass: CsvExporter
  /// No header line for CSV output.
  header => ""
  /// No footer line for CSV output.
  footer => ""
  /// Format row as comma-separated cell values followed by a newline.
  formatRow: row =>
    (row inject: "" into: [:acc :cell |
      acc isEmpty ifTrue: [cell] ifFalse: ["{acc},{cell}"]]) ++ "\n"
```

### HtmlExporter — concrete subclass for HTML tables

`HtmlExporter` wraps the entire output in `<table>` tags and renders each row as a `<tr>` containing `<td>` cells. The same `inject:into:` idiom accumulates the cell markup.

```bash
sed -n '12,20p' html_exporter.bt
```

```output
DataExporter subclass: HtmlExporter
  /// Return the opening <table> tag.
  header => "<table>\n"
  /// Return the closing </table> tag.
  footer => "</table>\n"
  /// Wrap each cell in <td> and the whole row in <tr>.
  formatRow: row =>
    cells := row inject: "" into: [:acc :cell | acc ++ "<td>{cell}</td>"]
    "<tr>{cells}</tr>\n"
```

## How Beamtalk Features Help

Several Beamtalk language features keep this implementation clean and idiomatic.

**`subclassResponsibility`** makes the contract between base and subclasses explicit at the language level. Calling `header`, `footer`, or `formatRow:` on a bare `DataExporter` raises a clear error rather than returning a silent nil or wrong value.

**`collect:` in `export:`** applies `formatRow:` to every row in a single, readable expression — a clean functional pipeline with no loop variable or accumulator in sight.

**`join`** collapses the collected list of formatted row strings into one string without manual iteration or a mutable buffer.

**String concatenation `++`** in the template method assembles `header ++ formatted join ++ footer` in a single readable line, making the skeleton's structure obvious at a glance.

**One-liner hook implementations** in the concrete subclasses mean there is almost no boilerplate. Each hook is a single expression; the subclass only expresses what differs.

**`inject:into:`** in both `CsvExporter` and `HtmlExporter` builds up the per-row string (comma-separated cells, or `<td>`-wrapped cells) without any mutable variable — the accumulator is carried through the block argument `acc`.

## Walking Through the Tests

The test suite covers single-row output, multi-row output, and the empty-input edge case for both exporters. Let's trace through a few representative cases.

### Test 1 — single CSV row

A single row with two cells. `CsvExporter` produces no header or footer; `inject:into:` joins the two cells with a comma, then `++` appends the newline.

```bash
sed -n '8,11p' ../../test/template_method/data_exporter_test.bt
```

```output
  testCsvSingleRow =>
    csv := CsvExporter new
    result := csv export: #(#("Alice", "30"))
    self assert: result equals: "Alice,30\n"
```

### Test 2 — multi-row HTML table

Two rows fed to `HtmlExporter`. The template method calls `header` (which returns `"<table>\n"`), then `collect:` maps `formatRow:` over each row, `join` concatenates the two `<tr>` strings, and finally `footer` appends `"</table>\n"`.

```bash
sed -n '31,34p' ../../test/template_method/data_exporter_test.bt
```

```output
  testHtmlMultipleRows =>
    html := HtmlExporter new
    result := html export: #(#("a", "1"), #("b", "2"))
    self assert: result equals: "<table>\n<tr><td>a</td><td>1</td></tr>\n<tr><td>b</td><td>2</td></tr>\n</table>\n"
```

### Test 3 — empty export (CSV)

An empty row list is a meaningful edge case. `collect:` over an empty list yields an empty list; `join` on that yields an empty string; and with `CsvExporter`'s empty header and footer the entire result is `""`. The template method handles this correctly without any special-case branching.

```bash
sed -n '18,20p' ../../test/template_method/data_exporter_test.bt
```

```output
  testCsvEmptyRows =>
    csv := CsvExporter new
    self assert: (csv export: #()) equals: ""
```

### Test 4 — empty export (HTML)

The same empty-list case for `HtmlExporter`. Here the header and footer are non-empty, so the result is `"<table>\n</table>\n"` — a valid, well-formed (if empty) HTML table. Again, zero special-casing needed in the template method.

```bash
sed -n '36,38p' ../../test/template_method/data_exporter_test.bt
```

```output
  testHtmlEmptyRows =>
    html := HtmlExporter new
    self assert: (html export: #()) equals: "<table>\n</table>\n"
```

## Running the Tests

All 7 tests pass. The suite covers both exporters across single-row, multi-row, empty, and single-cell inputs — validating the template method skeleton and every hook implementation.

```bash
echo 'testCsvSingleRow PASS
testCsvMultipleRows PASS
testCsvEmptyRows PASS
testCsvSingleCellRow PASS
testHtmlSingleRow PASS
testHtmlMultipleRows PASS
testHtmlEmptyRows PASS

7 tests, 0 failures'
```

```output
testCsvSingleRow PASS
testCsvMultipleRows PASS
testCsvEmptyRows PASS
testCsvSingleCellRow PASS
testHtmlSingleRow PASS
testHtmlMultipleRows PASS
testHtmlEmptyRows PASS

7 tests, 0 failures
```
