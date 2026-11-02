// An invalid submit focuses the first invalid field — checked without a browser.
//
// The seam is between Elm and the DOM: the submit is a form message, the errors
// are rendered a frame later, and what gets focus is decided by the markup. This
// builds just enough of a DOM to submit, render errors afterwards, and see what
// took focus and was scrolled to.

const assert = require('assert')
const { firstInvalid, install } = require('../js/form-errors')

let focused = []
let scrolled = []

// A minimal element: classes, attributes, children, a parent, and the queries used.
function el(tag, { classes = [], type, invalid = false, checked = false, disabled = false, children = [] } = {}) {
    const node = {
        tag,
        type,
        invalid,
        checked,
        disabled,
        isConnected: true,
        parent: null,
        children: [],
        classList: { contains: (name) => classes.includes(name) },
        focus(options) { focused.push({ node, options }) },
        scrollIntoView(options) { scrolled.push({ node, options }) },
        append(child) { child.parent = node; node.children.push(child) },
        descendants() { return node.children.flatMap((child) => [child, ...child.descendants()]) },
        matches(selector) { return selector.split(/,(?![^(]*\))/).some((part) => matchOne(node, part.trim())) },
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

// Enough of CSS for the selectors in form-errors.js.
function matchOne(node, selector) {
    if (selector === '[aria-invalid="true"]') return node.invalid
    if (selector === 'form') return node.tag === 'form'
    if (selector === '.ui-form-unsaved__actions button') {
        return node.tag === 'button' && !!(node.parent && node.parent.closest('.ui-form-unsaved__actions'))
    }
    if (selector.startsWith('.')) return node.classList.contains(selector.slice(1))
    if (selector === 'input:checked:not(:disabled)') return node.tag === 'input' && node.checked && !node.disabled
    if (selector.startsWith('input:not([type="hidden"])')) return node.tag === 'input' && node.type !== 'hidden' && !node.disabled
    if (selector.startsWith('select')) return node.tag === 'select' && !node.disabled
    if (selector.startsWith('textarea')) return node.tag === 'textarea' && !node.disabled
    if (selector === 'button:not(:disabled)') return node.tag === 'button' && !node.disabled
    return false
}

function field(control, { error = false } = {}) {
    return el('div', { classes: ['ui-field'], children: [control, ...(error ? [el('p', { classes: ['ui-field__error'] })] : [])] })
}

function doc(...children) {
    const root = el('body', { children })
    return Object.assign(root, { listeners: {}, addEventListener(type, fn) { root.listeners[type] = fn } })
}

// The first invalid control in document order.
{
    const first = el('input', { type: 'text', invalid: true })
    const d = doc(field(el('input', { type: 'text' })), field(first, { error: true }), field(el('input', { type: 'text', invalid: true }), { error: true }))
    assert.strictEqual(firstInvalid(null, d), first)
}

// A group's message comes first, and its checked option is what is focused.
{
    const chosen = el('input', { type: 'radio', checked: true })
    const group = el('fieldset', { classes: ['ui-field'], children: [el('input', { type: 'radio' }), chosen, el('p', { classes: ['ui-field__error'] })] })
    const d = doc(group, field(el('input', { type: 'text', invalid: true }), { error: true }))
    assert.strictEqual(firstInvalid(null, d), chosen)
}

// A dialog's form is searched before the page under it.
{
    const behind = el('input', { type: 'text', invalid: true })
    const inDialog = el('input', { type: 'text', invalid: true })
    const form = el('form', { children: [field(inDialog, { error: true })] })
    const d = doc(field(behind, { error: true }), form)
    assert.strictEqual(firstInvalid(form, d), inDialog)
}

// Nothing wrong, nothing found.
assert.strictEqual(firstInvalid(null, doc(field(el('input', { type: 'text' })))), null)

// Each way of submitting focuses and scrolls to the error rendered after it, and only then.
{
    const frames = []
    const timers = []
    const win = {
        setTimeout: (fn) => timers.push(fn),
        requestAnimationFrame: (fn) => frames.push(fn),
        matchMedia: () => ({ matches: false })
    }
    const name = el('input', { type: 'text' })
    const form = el('form', { children: [field(name)] })
    const save = el('button')
    const discard = el('button')
    const d = doc(form, el('div', { classes: ['ui-form-unsaved'], children: [el('div', { classes: ['ui-form-unsaved__actions'], children: [discard, save] })] }))
    install(d, win)

    // The event's handlers run, then the frame that renders what they did.
    const run = () => {
        timers.splice(0).forEach((fn) => fn())
        name.invalid = true
        frames.splice(0).forEach((fn) => fn())
    }
    const submits = [
        ['submit', () => d.listeners.submit({ target: form })],
        ['unsaved bar Save', () => d.listeners.click({ target: save })],
        ['shortcut', () => d.listeners.keydown({ key: 'Enter', ctrlKey: true, target: name })],
        ['Mac shortcut', () => d.listeners.keydown({ key: 'Enter', metaKey: true, target: d })]
    ]
    for (const [what, submit] of submits) {
        name.invalid = false
        focused = []
        scrolled = []
        submit()
        assert.deepStrictEqual(focused, [], `${what}: nothing is focused before the errors are rendered`)
        run()
        assert.deepStrictEqual(focused, [{ node: name, options: { preventScroll: true } }], `${what}: the invalid field is focused`)
        assert.deepStrictEqual(scrolled, [{ node: name, options: { block: 'center', behavior: 'smooth' } }], `${what}: and scrolled to`)
    }

    // A valid submit leaves focus where it is.
    focused = []
    d.listeners.submit({ target: form })
    timers.splice(0).forEach((fn) => fn())
    name.invalid = false
    frames.splice(0).forEach((fn) => fn())
    assert.deepStrictEqual(focused, [])

    // Plain Enter and clicks outside the bar do not count as a submit.
    d.listeners.keydown({ key: 'Enter', target: name })
    d.listeners.click({ target: el('button') })
    assert.strictEqual(timers.length, 0)
}

console.log('ui form errors contract OK')
