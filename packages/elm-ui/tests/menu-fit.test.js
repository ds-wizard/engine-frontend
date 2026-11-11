// An open menu fits the window (packages/elm-ui/js/menu-fit.js), checked without a
// browser: the height a list is given for where it opens.

const assert = require('assert')
const { maxHeight, overLeft, opensAbove, placeAll, MARGIN, MIN_HEIGHT } = require('../js/menu-fit')

// A short menu is left as it is.
assert.strictEqual(maxHeight(100, 200, 800), null)

// A long one stops a margin above the bottom of the window, and scrolls.
assert.strictEqual(maxHeight(100, 1200, 800), 800 - 100 - MARGIN)

// Exactly filling the room is fitting.
assert.strictEqual(maxHeight(100, 800 - 100 - MARGIN, 800), null)

// Opened near the bottom, it keeps a few items' height rather than none.
assert.strictEqual(maxHeight(780, 600, 800), MIN_HEIGHT)

// A list laid over the page ends where its trigger ends...
assert.strictEqual(overLeft({ left: 700, right: 726 }, 300, 1400, false), 426)

// ...unless that runs past the window's start, when it starts where the trigger does.
assert.strictEqual(overLeft({ left: 100, right: 126 }, 300, 1400, false), 100)

// Right to left, the end edge is the trigger's left one.
assert.strictEqual(overLeft({ left: 700, right: 726 }, 300, 1400, true), 700)

// Never off the window, whichever edge it is aligned to.
assert.strictEqual(overLeft({ left: 1390, right: 1416 }, 300, 1400, true), 1400 - 300 - MARGIN)

// A field's popup in a dialog opens below its field when it fits there...
assert.strictEqual(opensAbove(300, 300, 600), false)

// ...and above only when it does not and there is more room above...
assert.strictEqual(opensAbove(400, 300, 600), true)

// ...else below, where its list scrolls.
assert.strictEqual(opensAbove(400, 300, 200), false)

// A field's popup in a dialog is placed only once the dialog has stopped
// animating: while it moves, it is what the popup is placed against, and a
// popup placed then lands as far off as the dialog is from the window's corner.
// A dialog with a combobox, its field 300px wide at (100, 200), whose
// animations the test settles.
function dialogWithPopup() {
    let animations = []
    const field = { getBoundingClientRect: () => ({ left: 100, right: 400, top: 200, bottom: 240, width: 300 }) }
    const list = { style: {}, offsetHeight: 160 }
    const combobox = { querySelector: (s) => (s === '.ui-combobox__trigger-row' ? field : null) }
    const body = { getBoundingClientRect: () => ({ top: 100, bottom: 700 }) }
    const dialog = { getAnimations: () => animations }
    const popup = {
        hidden: false,
        style: {},
        offsetHeight: 200,
        closest: (s) => ({ '.ui-combobox': combobox, '.ui-dialog__body': body, dialog }[s] || null),
        querySelector: (s) => (s === '.ui-combobox__listbox' ? list : null),
        // Where it is on the screen: while the dialog animates, its left and
        // top count from the dialog's corner, here 300px from the window's.
        getBoundingClientRect: () => {
            const offset = animations.some((a) => a.playState === 'running') ? 300 : 0
            return { left: parseFloat(popup.style.left) + offset, top: parseFloat(popup.style.top) + offset }
        },
    }
    const animate = () => {
        let settle
        const finished = new Promise((resolve, reject) => { settle = { finish: resolve, cancel: reject } })
        finished.catch(() => {})
        const animation = { playState: 'running', finished }
        animations = [...animations, animation]
        return {
            finish: () => { animation.playState = 'finished'; settle.finish() },
            cancel: () => { animation.playState = 'idle'; settle.cancel(new Error('canceled')) },
        }
    }
    return { popup, animate }
}

const tick = () => new Promise((resolve) => setImmediate(resolve))

;(async () => {
    global.window = { innerHeight: 800 }

    // Not animating: placed at once, under the field and as wide as it.
    {
        const { popup } = dialogWithPopup()
        global.document = { querySelectorAll: (s) => (s.includes('ui-combobox__popup') ? [popup] : []) }
        placeAll()
        assert.deepStrictEqual(
            [popup.style.left, popup.style.top, popup.style.inlineSize, popup.style.visibility],
            ['100px', '244px', '300px', '']
        )
    }

    // Animating: not shown and not placed until the animation has finished —
    // but hidden by its opacity, so the search field in it can still take the
    // focus the combobox gives it as it opens...
    {
        const { popup, animate } = dialogWithPopup()
        global.document = { querySelectorAll: (s) => (s.includes('ui-combobox__popup') ? [popup] : []) }
        const opening = animate()
        placeAll()
        placeAll()
        assert.deepStrictEqual([popup.style.opacity, popup.style.pointerEvents], ['0', 'none'])
        assert.notStrictEqual(popup.style.visibility, 'hidden')
        assert.strictEqual(popup.style.left, undefined)
        opening.finish()
        await tick()
        assert.deepStrictEqual(
            [popup.style.left, popup.style.top, popup.style.visibility, popup.style.opacity, popup.style.pointerEvents],
            ['100px', '244px', '', '', '']
        )
    }

    // ...or canceled, which rejects its `finished` rather than resolving it.
    {
        const { popup, animate } = dialogWithPopup()
        global.document = { querySelectorAll: (s) => (s.includes('ui-combobox__popup') ? [popup] : []) }
        const opening = animate()
        placeAll()
        opening.cancel()
        await tick()
        assert.deepStrictEqual([popup.style.left, popup.style.top, popup.style.visibility], ['100px', '244px', ''])
    }

    console.log('menu-fit: ok')
})().catch((error) => {
    console.error(error)
    process.exit(1)
})
