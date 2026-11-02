// A list field's new item opens on its first field.
//
// Pressing "Add" appends an item and leaves focus on the button, so a keyboard
// user has to find the item that just appeared, and everybody has to click into
// it. Adding is asked for so that something can be typed; this puts the caret
// there.
//
// The add is the application's own message (an elm-form Append in Common),
// handled in each page's update, and the item is rendered on the next frame —
// so there is no one place in Elm to send a focus from, and no id the new item
// is known by. One listener on the document instead: when a list field's add
// button is pressed (a click, or Enter or Space, which click it), it counts that
// list's own items and watches the list until there is one more, then focuses
// the first field of the last item. It gives up after a second, so an add that
// did not happen (a failed validation, a disabled button) does not take focus
// later on.

const FIELD =
    'input:not([type="hidden"]):not(:disabled), select:not(:disabled), textarea:not(:disabled), [contenteditable="true"]'

const GIVE_UP_AFTER = 1000

// The list's own items — not those of a list inside one of them.
function ownRows(list) {
    const rows = Array.from(list.children).find((child) => child.classList.contains('ui-list-field__rows'))
    if (!rows) return []
    return Array.from(rows.children).filter((child) => child.classList.contains('ui-list-field__row'))
}

// The first field of an item. A radio group is entered on its chosen option,
// as Tab would enter it.
function firstField(row) {
    const field = row.querySelector(FIELD)
    if (!field) return null
    if (field.type !== 'radio') return field

    const group = field.closest('fieldset') || row
    return group.querySelector('input[type="radio"]:checked:not(:disabled)') || field
}

function install(doc, win) {
    doc.addEventListener('click', (event) => {
        const button = event.target.closest && event.target.closest('.ui-list-field__add button')
        if (!button) return

        const list = button.closest('.ui-list-field')
        if (!list) return

        const before = ownRows(list).length

        const attempt = () => {
            const rows = ownRows(list)
            if (rows.length <= before) return

            stop()
            const field = firstField(rows[rows.length - 1])
            if (field) field.focus()
        }

        const observer = new win.MutationObserver(attempt)
        observer.observe(list, { childList: true, subtree: true })
        const timeout = win.setTimeout(stop, GIVE_UP_AFTER)

        function stop() {
            observer.disconnect()
            win.clearTimeout(timeout)
        }
    })
}

if (typeof document !== 'undefined' && typeof window !== 'undefined') {
    install(document, window)
}

module.exports = { FIELD, ownRows, firstField, install }
