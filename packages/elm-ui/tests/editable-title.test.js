// A page title's field selects its name as it takes focus, and no other field
// does — checked without a browser, on just enough of a DOM.

const assert = require('assert')
const { onFocusin } = require('../js/editable-title')

function field(isTitle) {
    const node = {
        selected: false,
        matches: (selector) => isTitle && selector === '[data-ui-editable-title]',
        select() { node.selected = true }
    }
    return node
}

{
    const title = field(true)
    onFocusin({ target: title })
    assert.ok(title.selected)
}

{
    const other = field(false)
    onFocusin({ target: other })
    assert.ok(!other.selected)
}

// Focus on something that is not an element, the document, is left alone.
onFocusin({ target: {} })
onFocusin({ target: null })

console.log('editable-title: ok')
