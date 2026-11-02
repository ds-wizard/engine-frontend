// The code field's contract with CodeMirror, checked without a browser.
//
// The thing worth guarding is Tab. Both existing editors bind it to indent,
// which makes the editor a keyboard trap: focus goes in and cannot come out
// (2.1.2). CodeMirror's default keymap already indents with Mod-] and Mod-[,
// so there is nothing to gain from the binding and a whole page to lose.
//
// The rest is the language list, which is the union of what the two existing
// editors know between them — one component, not one per application.

const assert = require('assert')
const fs = require('fs')
const path = require('path')

// Comments removed first: this file's own reasoning mentions the things it
// forbids, and a guard that trips over an explanation of itself is a guard
// nobody keeps.
const source = fs
    .readFileSync(path.join(__dirname, '../js/code-editor.js'), 'utf8')
    .split('\n')
    .filter((line) => !/^\s*(\/\/|\*|\/\*)/.test(line))
    .join('\n')
const { LANGUAGES, CONTENT_TYPES, languageForContentType, highlightStyle } = require('../js/code-editor')

// Not a stylistic objection: any of these would put Tab back on the editor.
assert.ok(!/indentWithTab/.test(source), 'Tab must not indent — it is the way out of the editor')
assert.ok(!/keymap\.of/.test(source), 'a keymap here would have to be justified against 2.1.2')
assert.ok(!/outline:\s*none/.test(source), 'the focus ring is not the editor\'s to remove')

assert.deepStrictEqual(
    Object.keys(LANGUAGES).sort(),
    ['css', 'html', 'jinja2', 'json', 'python', 'xml'],
    'the languages both existing editors know between them'
)

for (const [name, build] of Object.entries(LANGUAGES)) {
    assert.strictEqual(typeof build, 'function', `${name} must be built on demand`)
    assert.ok(build(), `${name} must produce an extension`)
}

// The document-template editor maps content types to languages; the mapping
// lives with the languages so the two cannot drift.
for (const [contentType, language] of Object.entries(CONTENT_TYPES)) {
    assert.ok(LANGUAGES[language], `${contentType} maps to ${language}, which is not a language`)
    assert.strictEqual(languageForContentType(contentType), language)
}

// An unknown content type is plain text, not an error: code that is not
// highlighted still reads as code.
assert.strictEqual(languageForContentType('application/octet-stream'), '')

// Wrapping is on unless it is turned off, and it is turned off without losing
// the cursor: a compartment, reconfigured, rather than a new state.
assert.ok(/new Compartment\(\)/.test(source), 'wrapping must live in a compartment')
assert.ok(/reconfigure\(this\._wrapExtension\(\)\)/.test(source), 'changing wrap must reconfigure, not rebuild')
assert.ok(/'wrap'/.test(source), 'wrap must be an observed attribute')
assert.ok(/getAttribute\('wrap'\) === 'off' \? \[\] : EditorView\.lineWrapping/.test(source), 'wrapping is on unless it is turned off')

// Colors come from roles, so the highlight style hands out class names and
// never a hex — that is what lets dark mode happen in the stylesheet.
assert.ok(!/color:\s*['"]#/.test(source), 'syntax colors are roles, not literals')
assert.ok(highlightStyle, 'the highlight style must exist')

console.log('ui code editor contract OK')
