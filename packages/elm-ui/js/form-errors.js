// An invalid submit focuses the first field that is wrong, and scrolls it into
// view.
//
// Without it a Save pressed at the bottom of a long form, or in the unsaved bar,
// with the wrong field out of sight, seems to do nothing. It belongs to the ways
// a form is submitted rather than to each page's update, so no form can be
// without it:
//
//   - a `form` element's submit — the submit button of a form's footer or of a
//     dialog's form, and Enter in a field;
//   - a button of the unsaved bar (`.ui-form-unsaved__actions`), which is
//     outside the form and so a plain button;
//   - Cmd+Enter or Ctrl+Enter, the submit shortcut (`Ui.Form.submitShortcut`).
//
// The submit is the application's: it validates and renders the errors on the
// next animation frame. So the check waits for the event to be handled
// everywhere, then for that frame, and looks once: a valid form has nothing to
// focus, and a later error (from the server, say) must not pull focus to itself
// seconds afterwards. Discard in the bar clears the errors, so it finds nothing.
//
// The first match in document order of either an invalid control or an error
// message — inside the submitted form first, so a dialog's form does not send
// focus to the page under it. Most controls carry aria-invalid, and come before
// their message; a radio or checkbox group does not — the group is a fieldset,
// and the inputs in it are not individually wrong — so its message is what is
// found, and focus goes to the checked option, or the first one.

const INVALID = '[aria-invalid="true"], .ui-field__error'
const FOCUSABLE = 'input:not([type="hidden"]):not(:disabled), select:not(:disabled), textarea:not(:disabled), [contenteditable="true"]'

// The control to focus: the first invalid one in the scope, then in the
// document, or the control of the first error message found instead.
function firstInvalid(scope, doc) {
    const found = (scope && scope.isConnected && scope.querySelector(INVALID)) || doc.querySelector(INVALID)
    if (!found) return null
    return found.matches('.ui-field__error') ? controlFor(found) : found
}

function controlFor(message) {
    const field = message.closest('.ui-field, .ui-list-field, .ui-field__stack')
    if (!field) return null
    return field.querySelector('input:checked:not(:disabled)')
        || field.querySelector(FOCUSABLE)
        || field.querySelector('button:not(:disabled)')
}

function install(doc, win) {
    // After every listener of the event, the submit's among them, and then
    // after the frame that renders what it did.
    function afterRender(scope) {
        win.setTimeout(function () {
            win.requestAnimationFrame(function () {
                const target = firstInvalid(scope, doc)
                if (!target) return

                target.focus({preventScroll: true})

                const reduce = win.matchMedia('(prefers-reduced-motion: reduce)').matches
                target.scrollIntoView({block: 'center', behavior: reduce ? 'auto' : 'smooth'})
            })
        })
    }

    doc.addEventListener('submit', function (event) {
        afterRender(event.target)
    })

    doc.addEventListener('click', function (event) {
        if (event.target.closest && event.target.closest('.ui-form-unsaved__actions button')) afterRender(null)
    })

    doc.addEventListener('keydown', function (event) {
        if (event.key === 'Enter' && (event.metaKey || event.ctrlKey)) {
            afterRender(event.target.closest ? event.target.closest('form') : null)
        }
    })
}

if (typeof document !== 'undefined') {
    install(document, window)
}

module.exports = {firstInvalid, install}
