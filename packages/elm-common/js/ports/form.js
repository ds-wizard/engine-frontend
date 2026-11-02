// Focus the first field of a form that has just opened — a create page.
//
// The form may not be there yet: a page that loads what its form offers renders
// a loading state first. So it watches the document for the form, and stops as
// soon as it has focused a field, after a few seconds, or when it is asked again
// (null cancels). A page left before its form appeared must not pull focus back
// to itself afterwards.
//
// An invalid submit focusing the first invalid field is not a port: it follows
// every submit, in packages/elm-ui/js/form-errors.js.
module.exports = function (app) {
    app.ports.commonFormFocusFirstField?.subscribe(focusFirstField)

    // A text-like control, not a switch or a choice: the field a form opens on
    // is the one you type the thing's name into.
    const typeable = 'input:not([type="hidden"]):not([type="checkbox"]):not([type="radio"]):not(:disabled):not([readonly]), select:not(:disabled), textarea:not(:disabled):not([readonly])'

    const giveUpAfter = 5000

    let pending = null

    // Every request replaces the one before it, including one still waiting
    // for its first frame.
    let currentRequest = 0

    function stopWaiting() {
        if (!pending) return
        pending.observer.disconnect()
        window.clearTimeout(pending.timeout)
        pending = null
    }

    function focusFirstField(containerId) {
        stopWaiting()
        const request = ++currentRequest
        if (!containerId) return

        const attempt = function () {
            if (request !== currentRequest) return true

            const container = document.getElementById(containerId)
            const field = container && container.querySelector(typeable)
            if (!field) return false

            stopWaiting()
            field.focus()
            return true
        }

        // Elm renders on the next frame, so the form is usually there then.
        window.requestAnimationFrame(function () {
            if (attempt() || request !== currentRequest) return

            const observer = new MutationObserver(attempt)
            observer.observe(document.body, {childList: true, subtree: true})
            pending = {observer, timeout: window.setTimeout(stopWaiting, giveUpAfter)}
        })
    }
}
