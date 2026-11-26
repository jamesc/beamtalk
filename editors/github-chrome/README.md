# Beamtalk for GitHub (Chrome extension)

Syntax-highlights Beamtalk on github.com until GitHub Linguist ships a Beamtalk
grammar. It uses the same TextMate grammar as the VS Code extension
(`editors/vscode/syntaxes/beamtalk.tmLanguage.json`, copied in at build time)
and the same Oniguruma regex engine VS Code uses, so colours match the editor's
tokenization.

Highlighted:

- `.bt` / `.btscript` file views
- ` ```beamtalk ` / ` ```btscript ` fenced code in Markdown (READMEs, issues, PR comments)
- `.bt` / `.btscript` files in commit and pull-request diffs

Colours come from GitHub's own syntax palette, so light and dark themes both work.

## Install

```bash
cd editors/github-chrome
npm install
npm run build
```

Then open `chrome://extensions`, enable **Developer mode**, click **Load
unpacked** and choose `editors/github-chrome/dist`. After changing the grammar,
rebuild and press the reload button on the extension card.

## How it works

- `src/background.js` (service worker) loads `onig.wasm` and the grammar, and
  tokenizes source text on request.
- `src/content.js` finds Beamtalk code on the page, asks the worker for tokens,
  and paints them with the
  [CSS Custom Highlight API](https://developer.mozilla.org/en-US/docs/Web/API/CSS_Custom_Highlight_API).
  It never modifies GitHub's DOM, which is React-managed. A `MutationObserver`
  re-applies highlights after client-side navigation and lazy diff loading.
- `src/protocol.js` defines the content-script → worker request and its
  limits; the content script clips to them and the worker refuses anything
  outside them.
- `src/tokenize.js` maps TextMate scopes to a handful of categories, which
  `src/content.css` colours with GitHub's `--color-prettylights-syntax-*`
  variables.

## Limitations

- Diff lines are tokenized one at a time, so a string spanning
  several lines in a diff is only coloured on its first line.
- Very large files that GitHub virtualizes are tokenized from the lines
  currently rendered.
- The page selectors (`.react-file-line`, `.diff-text-inner`,
  `pre[lang=beamtalk]`) track github.com's current markup and will need
  updating when GitHub changes it. gist.github.com renders different markup and
  is not matched.
- Tokenizing is bounded (`src/protocol.js` `LIMITS`), since page text from any
  README, issue or pull request goes to one service worker shared by every tab:
  each line gets a time limit, each request a time budget, and a request is
  capped in blocks, lines, line length and total size. Text past a limit is
  left uncoloured.

## Test

```bash
npm test
```
