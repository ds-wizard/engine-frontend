// A custom element around the native <dialog>, which opens it as a modal while
// it has the `open` attribute.
//
// A custom element rather than a port: there is no message ordering to get
// wrong, and it composes with the virtual DOM. The dialog is its child, drawn by
// the view like everything in it, and the element only opens and closes it:
// what the virtual DOM has drawn stays where the virtual DOM put it. (It does
// not make the dialog and move what it was given into it: an element kept from
// one page to the next for another dialog would then get what is new beside
// the dialog, on the page.) The browser gives us focus
// trapping, Escape, the top layer and an inert background for free, and all
// four are easy to get subtly wrong by hand.
//
// Enter does what the dialog is for, Escape cancels it. The browser focuses
// the first control on opening, and in a dialog with no field that is Cancel —
// the first button of the footer — so Enter, which presses the focused button,
// closed the dialog instead of confirming it. So a first focus that lands in
// the footer goes on to the primary action, and Enter pressed on something that
// is not a control of its own (the text, the title) presses it too.
//
// A dialog with a form or a field in it is saved by Cmd+Enter on a Mac and
// Ctrl+Enter elsewhere, from anywhere in it, and never by Enter. Its fields
// have an Enter of their own — a picker takes the highlighted suggestion, a
// select its option — so Enter would save from some fields and not from
// others, and nobody could tell which. So Enter presses nothing for it, and
// Enter in an input does not submit the form: the browser's implicit
// submission is the keydown's default, prevented once the field has had the
// key. A field that takes the shortcut for itself (a comment box) prevents it
// first, and keeps it. The shortcut presses the primary action, so a form is
// submitted as the button submits it, and it goes no further than the dialog:
// a page under it with a shortcut of its own is not saved as well.

// The footer's primary action: the confirm, the submit, the dangerous one.
const PRIMARY = '.ui-dialog__footer :is(.ui-button--primary, .ui-button--danger)'

// What has an Enter of its own, or is a control Enter would act on.
const OWN_ENTER = 'a[href], button, input, select, textarea, summary, [contenteditable=""], [contenteditable="true"], [role]:not([role="dialog"]):not([role="document"]):not([role="none"]):not([role="presentation"]), ui-code-editor, ui-markdown-editor, ui-date-picker'

// What makes a dialog a form. A read-only input is something to copy, not to
// fill in.
const FIELD = 'form, input:not([type="hidden"]):not([readonly]), select, textarea:not([readonly]), [contenteditable=""], [contenteditable="true"], ui-code-editor, ui-markdown-editor, ui-date-picker'

// Which modifier the shortcut takes depends on the platform: metaKey is Cmd on
// a Mac and the Windows key elsewhere. navigator.platform is deprecated but
// still the most widely available answer; userAgentData is used where it exists.
function isMac() {
    const platform = (navigator.userAgentData && navigator.userAgentData.platform) || navigator.platform || ''
    return /mac/i.test(platform)
}

function isSubmitShortcut(event) {
    if (event.altKey || event.shiftKey) return false
    return isMac() ? event.metaKey && !event.ctrlKey : event.ctrlKey && !event.metaKey
}

// The open dialogs, the topmost last. A modal dialog makes everything outside it
// inert, so a key pressed outside every dialog was pressed with the focus lost:
// the focused control was taken out of the page — a file upload's button gives
// way to the chosen file — and the focus fell to the body. Such a key goes to
// the topmost dialog, as though it had been pressed in it.
const openDialogs = []

document.addEventListener('keydown', (event) => {
    const top = openDialogs[openDialogs.length - 1]
    const dialog = top && top._dialog
    if (!dialog || dialog.contains(event.target)) return
    if (event.target !== document.body && event.target !== document.documentElement) return
    top._onKeydown(event)
})

function primaryAction(dialog) {
    const buttons = dialog.querySelectorAll(PRIMARY)
    return buttons.length ? buttons[buttons.length - 1] : null
}

class UiDialog extends HTMLElement {
    static get observedAttributes() {
        return ['open']
    }

    // Looked up each time, not kept: the view owns the dialog.
    get _dialog() {
        for (const child of this.children) {
            if (child.localName === 'dialog') return child
        }
        return null
    }

    connectedCallback() {
        if (!this._connected) {
            this._connected = true

            // Escape is handled by the browser; we only forward the outcome so Elm
            // can update its own model rather than the DOM drifting out of sync.
            // `close` does not bubble, so it is caught on its way down.
            this._onClose = (event) => {
                if (event.target !== this._dialog) return
                this._forget()
                this.dispatchEvent(new CustomEvent('dialogClose', {bubbles: true}))
            }
            this.addEventListener('close', this._onClose, true)

            // Clicking the backdrop closes, matching every other dialog on the web.
            this._onClick = (event) => {
                const dialog = this._dialog
                if (dialog && event.target === dialog) dialog.close()
            }
            this.addEventListener('click', this._onClick)

            this._onKeydown = (event) => {
                const dialog = this._dialog
                if (!dialog || event.key !== 'Enter' || event.defaultPrevented || event.isComposing) return

                const target = event.target
                const primary = primaryAction(dialog)

                if (isSubmitShortcut(event)) {
                    event.preventDefault()
                    event.stopPropagation()
                    if (primary && !primary.disabled) primary.click()
                    return
                }
                if (event.metaKey || event.ctrlKey || event.altKey || event.shiftKey) return

                if (dialog.querySelector(FIELD)) {
                    if (target.tagName === 'INPUT') event.preventDefault()
                    return
                }
                if (target !== dialog && target.closest(OWN_ENTER)) return
                if (!primary || primary.disabled) return

                event.preventDefault()
                primary.click()
            }
            this.addEventListener('keydown', this._onKeydown)
        }

        this._sync()
    }

    disconnectedCallback() {
        this._forget()
    }

    attributeChangedCallback() {
        this._sync()
    }

    _forget() {
        const index = openDialogs.indexOf(this)
        if (index !== -1) openDialogs.splice(index, 1)
    }

    _focusPrimary() {
        // Only when the browser's choice fell on the footer: a field, an
        // autofocus or anything else before it is where the dialog wants to
        // start.
        const focused = document.activeElement
        if (!focused || !this._dialog.contains(focused) || !focused.closest('.ui-dialog__footer')) return

        const primary = primaryAction(this._dialog)
        if (primary && primary !== focused && !primary.disabled) primary.focus()
    }

    _sync() {
        const dialog = this._dialog
        if (!this.isConnected || !dialog) return

        const shouldBeOpen = this.hasAttribute('open')

        if (shouldBeOpen && !dialog.open) {
            // showModal, not show: only the modal form makes the background
            // inert and puts the dialog in the top layer.
            dialog.showModal()
            openDialogs.push(this)
            this._focusPrimary()
        } else if (!shouldBeOpen && dialog.open) {
            this._forget()
            dialog.close()
        }
    }
}

if (!customElements.get('ui-dialog')) {
    customElements.define('ui-dialog', UiDialog)
}
