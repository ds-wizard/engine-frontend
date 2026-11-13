// The name of a page title being renamed (`Ui.EditableTitle`) is selected as
// the field opens, so typing replaces it and an arrow key keeps it — as a
// rename does everywhere else. A view cannot select text, so this does, for
// the field the title focuses when editing starts.

function onFocusin(event) {
    const target = event.target
    if (target && target.matches && target.matches('[data-ui-editable-title]')) {
        target.select()
    }
}

if (typeof document !== 'undefined') {
    document.addEventListener('focusin', onFocusin)
}

module.exports = { onFocusin }
