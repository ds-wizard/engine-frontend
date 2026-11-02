// An open menu fits the window (packages/elm-ui/js/menu-fit.js), checked without a
// browser: the height a list is given for where it opens.

const assert = require('assert')
const { maxHeight, overLeft, opensAbove, MARGIN, MIN_HEIGHT } = require('../js/menu-fit')

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

console.log('menu-fit: ok')
