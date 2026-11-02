// The markdown field's toolbar, checked without a browser.
//
// The thing worth guarding is the keyboard. The toolbar this replaces gives
// every one of its buttons tabIndex = -1 and nothing else — no roving index, no
// arrow keys — so none of them can be reached without a pointer, the switch to
// the markdown source included. Here exactly one item is reachable by Tab and
// the arrows move along the row, which is the APG toolbar pattern and is also
// what keeps a dozen buttons from standing between a form's label and its text.

const assert = require('assert')
const fs = require('fs')
const path = require('path')
const { COMMANDS, MODES, DEFAULT_WORDS, nextIndex, wordsFrom } = require('../js/markdown-editor')

// The switch between writing and reading is `Ui.ModeSwitch`, shared with the
// knowledge model editor's template box and styled in one place. The element
// builds it in JavaScript, so nothing else would notice if the two drifted —
// and a switch with the wrong class name is invisible rather than broken.
const editorSource = fs.readFileSync(path.join(__dirname, '../js/markdown-editor.js'), 'utf8')
const switchStyles = fs.readFileSync(path.join(__dirname, '../scss/components/_mode-switch.scss'), 'utf8')

for (const className of ['ui-mode-switch', 'ui-mode-switch__option']) {
    assert.ok(
        editorSource.includes(`'${className}'`),
        `the markdown editor has to build its mode switch as .${className}`
    )
    assert.ok(
        switchStyles.includes(`.${className} `) || switchStyles.includes(`.${className} {`),
        `.${className} has to be styled in _mode-switch.scss`
    )
}

const commands = COMMANDS.filter((command) => !command.separator)

// Every command is a toggle, so every one of them has to be able to say
// whether it is on: a fill is not something a screen reader reads (4.1.2).
for (const command of commands) {
    assert.ok(command.key, 'every command needs a key')
    assert.ok(command.label, `${command.key} needs a label — it becomes the accessible name`)
    assert.ok(command.icon || command.text, `${command.key} needs something to show`)
    assert.strictEqual(typeof command.run, 'function', `${command.key} needs a command`)
    assert.strictEqual(typeof command.active, 'function', `${command.key} must report whether it is on`)
}

const keys = commands.map((command) => command.key)
assert.strictEqual(new Set(keys).size, keys.length, 'command keys must be unique')

// The two that ask for a value do it inline. A prompt() cannot be styled,
// cannot be described, and belongs to no field.
for (const key of ['link', 'image']) {
    const command = commands.find((entry) => entry.key === key)
    assert.ok(command.prompt, `${key} must ask for its value inline`)
    assert.ok(command.prompt.label, `${key}'s question needs a label`)
}

assert.deepStrictEqual(MODES, ['rich', 'markdown'])

// Every name the toolbar shows can be given in the page's language: each
// command, both modes, and the English defaults for the command labels are the
// labels the commands carry, so an element with no words says what it did.
for (const command of commands) {
    assert.strictEqual(DEFAULT_WORDS[command.key], command.label, `${command.key} needs a default word`)
}
for (const mode of MODES) {
    assert.ok(DEFAULT_WORDS[mode], `the ${mode} mode needs a default word`)
}
assert.strictEqual(wordsFrom(null).bold, 'Bold')
assert.strictEqual(wordsFrom('not json').bold, 'Bold', 'unreadable words fall back to English')
assert.strictEqual(wordsFrom('{"bold":"Tučné","italic":""}').bold, 'Tučné')
assert.strictEqual(wordsFrom('{"bold":"Tučné","italic":""}').italic, 'Italic', 'an empty word falls back')
assert.strictEqual(wordsFrom('{"toolbar":"Formátování popisu"}').toolbar, 'Formátování popisu')

// The arrows wrap: a row of buttons has no end worth stopping at.
assert.strictEqual(nextIndex(4, 3, 'ArrowRight'), 0)
assert.strictEqual(nextIndex(4, 0, 'ArrowLeft'), 3)
assert.strictEqual(nextIndex(4, 2, 'Home'), 0)
assert.strictEqual(nextIndex(4, 1, 'End'), 3)
assert.strictEqual(nextIndex(4, 2, 'ArrowUp'), 2, 'an unrelated key moves nothing')
assert.strictEqual(nextIndex(0, 0, 'ArrowRight'), 0, 'an empty toolbar has nowhere to go')

// Nothing but typing may report a change. TipTap's setEditable emits an update
// unless told not to, and the update handler passes updates on to Elm as edits:
// without `false` every form with a markdown field was "changed" as it loaded.
// And an update that leaves the markdown as it was is not passed on.
{
    const fs = require('fs')
    const path = require('path')
    const source = fs
        .readFileSync(path.join(__dirname, '../js/markdown-editor.js'), 'utf8')
        .split('\n')
        .filter((line) => !/^\s*(\/\/|\*|\/\*)/.test(line))
        .join('\n')

    const setEditableCalls = source.match(/setEditable\(([^)]*)\)/g) || []
    assert.ok(setEditableCalls.length > 0, 'the editor is made read-only through setEditable')
    for (const call of setEditableCalls) {
        assert.ok(/,\s*false\s*\)$/.test(call), `${call} must not emit an update`)
    }

    assert.ok(/if \(next === this\._value\) return/.test(source), 'an update that changes nothing is not sent on')
}

console.log('ui markdown editor contract OK')
