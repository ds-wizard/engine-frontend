// A mode switch's arrow keys move focus and the choice together — checked
// without a browser, on just enough of a DOM.

const assert = require('assert')
const { targetIndex, onKeydown } = require('../js/mode-switch')

let log = []

function option(name, { disabled = false } = {}) {
    const node = {
        name,
        disabled,
        group: null,
        closest(selector) {
            if (selector === '.ui-mode-switch__option') return node
            if (selector === '[data-ui-mode-switch]') return node.group
            return null
        },
        focus() { log.push(`focus ${name}`) },
        click() { log.push(`click ${name}`) }
    }
    return node
}

function group(options, { fromElm = true } = {}) {
    const node = { querySelectorAll: () => options }
    options.forEach((item) => { item.group = fromElm ? node : null })
    return options
}

function press(target, key, extra = {}) {
    log = []
    const event = {
        key,
        target,
        defaultPrevented: false,
        altKey: false,
        ctrlKey: false,
        metaKey: false,
        shiftKey: false,
        ...extra,
        preventDefault() { event.defaultPrevented = true }
    }
    onKeydown(event)
    return event
}

// The keys, around the ends.
assert.strictEqual(targetIndex(2, 0, 'ArrowRight'), 1)
assert.strictEqual(targetIndex(2, 1, 'ArrowRight'), 0)
assert.strictEqual(targetIndex(3, 0, 'ArrowLeft'), 2)
assert.strictEqual(targetIndex(3, 1, 'ArrowDown'), 2)
assert.strictEqual(targetIndex(3, 1, 'ArrowUp'), 0)
assert.strictEqual(targetIndex(3, 1, 'Home'), 0)
assert.strictEqual(targetIndex(3, 1, 'End'), 2)

// Right from Template focuses and chooses Preview; Left goes back.
{
    const [template, preview] = group([option('template'), option('preview')])
    const event = press(template, 'ArrowRight')
    assert.deepStrictEqual(log, ['focus preview', 'click preview'])
    assert.ok(event.defaultPrevented)

    press(preview, 'ArrowLeft')
    assert.deepStrictEqual(log, ['focus template', 'click template'])
}

// A disabled option is passed over.
{
    const [a, , c] = group([option('a'), option('b', { disabled: true }), option('c')])
    press(a, 'ArrowRight')
    assert.deepStrictEqual(log, ['focus c', 'click c'])
}

// Other keys, modified keys, keys already handled and switches not drawn from
// Elm are left alone.
{
    const [a] = group([option('a'), option('b')])
    assert.ok(!press(a, 'Enter').defaultPrevented)
    press(a, 'ArrowRight', { shiftKey: true })
    press(a, 'ArrowRight', { defaultPrevented: true })
    assert.deepStrictEqual(log, [])

    const [own] = group([option('own'), option('other')], { fromElm: false })
    assert.ok(!press(own, 'ArrowRight').defaultPrevented)
    assert.deepStrictEqual(log, [])
}

console.log('mode-switch: ok')
