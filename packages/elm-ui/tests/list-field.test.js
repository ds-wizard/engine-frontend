// A list field's new item opens on its first field — checked without a browser.
//
// The seam is between Elm and the DOM: the add is a form message, the item is
// rendered a frame later, and what gets focus is decided by the markup. None of
// that is visible to elm-test. This builds just enough of a DOM to press Add,
// render an item afterwards, and see what took focus.

const assert = require('assert')
const { ownRows, firstField, install } = require('../js/list-field')

// A minimal element: classes, children, a parent, and the three queries used.
function el(tag, { classes = [], type, checked = false, disabled = false, children = [] } = {}) {
    const node = {
        tag,
        type,
        checked,
        disabled,
        parent: null,
        children: [],
        focused: false,
        classList: { contains: (name) => classes.includes(name) },
        focus() { focused.push(node) },
        append(child) { child.parent = node; node.children.push(child) },
        descendants() { return node.children.flatMap((child) => [child, ...child.descendants()]) },
        matches(selector) { return selector.split(',').some((part) => matchOne(node, part.trim())) },
        querySelector(selector) { return node.descendants().find((d) => d.matches(selector)) || null },
        closest(selector) {
            for (let current = node; current; current = current.parent) {
                if (current.matches(selector)) return current
            }
            return null
        }
    }
    children.forEach((child) => node.append(child))
    return node
}

// Enough of CSS for the selectors in list-field.js.
function matchOne(node, selector) {
    if (selector === '.ui-list-field__add button') {
        return node.tag === 'button' && !!(node.parent && node.parent.closest('.ui-list-field__add'))
    }
    if (selector.startsWith('.')) return node.classList.contains(selector.slice(1))
    if (selector === 'fieldset') return node.tag === 'fieldset'
    if (selector === 'input[type="radio"]:checked:not(:disabled)') {
        return node.tag === 'input' && node.type === 'radio' && node.checked && !node.disabled
    }
    // The field selector: typeable controls that are not disabled.
    if (selector.startsWith('input:not([type="hidden"])')) return node.tag === 'input' && node.type !== 'hidden' && !node.disabled
    if (selector.startsWith('select')) return node.tag === 'select' && !node.disabled
    if (selector.startsWith('textarea')) return node.tag === 'textarea' && !node.disabled
    return false
}

let focused = []

function row(...fields) {
    return el('li', { classes: ['ui-list-field__row'], children: [el('div', { classes: ['ui-list-field__content'], children: fields })] })
}

function listField(rows) {
    const button = el('button')
    const list = el('fieldset', {
        classes: ['ui-list-field'],
        children: [
            el('legend'),
            el('ol', { classes: ['ui-list-field__rows'], children: rows }),
            el('div', { classes: ['ui-list-field__add'], children: [button] })
        ]
    })
    return { list, button, rowsElement: list.children[1] }
}

// ownRows counts only the list's own items, not those of a list inside one.
{
    const inner = listField([row(el('input', { type: 'text' }))])
    const outer = listField([row(el('input', { type: 'text' }), inner.list)])
    assert.strictEqual(ownRows(outer.list).length, 1)
    assert.strictEqual(ownRows(inner.list).length, 1)
}

// firstField skips disabled and hidden inputs and enters a radio group on its chosen option.
{
    const text = el('input', { type: 'text' })
    assert.strictEqual(firstField(row(el('input', { type: 'hidden' }), el('input', { type: 'text', disabled: true }), text)), text)

    const chosen = el('input', { type: 'radio', checked: true })
    const group = el('fieldset', { children: [el('input', { type: 'radio' }), chosen] })
    assert.strictEqual(firstField(row(group)), chosen)

    const first = el('input', { type: 'radio' })
    const unchosen = el('fieldset', { children: [first, el('input', { type: 'radio' })] })
    assert.strictEqual(firstField(row(unchosen)), first)
}

// Pressing Add focuses the first field of the item that appears afterwards.
{
    let listener = null
    const observers = []
    const timers = []
    const doc = { addEventListener: (type, fn) => { if (type === 'click') listener = fn } }
    const win = {
        MutationObserver: class {
            constructor(fn) { this.fn = fn; this.active = false; observers.push(this) }
            observe() { this.active = true }
            disconnect() { this.active = false }
        },
        setTimeout: (fn) => timers.push(fn),
        clearTimeout: () => {}
    }
    install(doc, win)

    const mutate = () => observers.filter((o) => o.active).forEach((o) => o.fn())

    // The item appears a frame after the press.
    focused = []
    const { button, rowsElement } = listField([row(el('input', { type: 'text' }))])
    listener({ target: button })
    mutate()
    assert.deepStrictEqual(focused, [], 'nothing is focused before the item exists')

    const newField = el('input', { type: 'text' })
    rowsElement.append(row(newField))
    mutate()
    assert.deepStrictEqual(focused, [newField], 'the new item\'s first field is focused')

    mutate()
    assert.strictEqual(focused.length, 1, 'it stops watching once it has focused')

    // An add that never happens gives up, and a later change does not take focus.
    focused = []
    const other = listField([])
    listener({ target: other.button })
    timers.forEach((fn) => fn())
    other.rowsElement.append(row(el('input', { type: 'text' })))
    mutate()
    assert.deepStrictEqual(focused, [], 'nothing is focused after giving up')

    // A click that is not on an add button is ignored.
    const before = observers.length
    listener({ target: el('input', { type: 'text' }) })
    assert.strictEqual(observers.length, before)
}

console.log('ui list field contract OK')
