// A custom element wrapping the native <dialog>.
//
// A custom element rather than a port: there is no message ordering to get
// wrong, and it composes with the virtual DOM. The browser gives us focus
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
    if (!top || top._dialog.contains(event.target)) return
    if (event.target !== document.body && event.target !== document.documentElement) return
    top._onKeydown(event)
})

function primaryAction(dialog) {
    const buttons = dialog.querySelectorAll(PRIMARY)
    return buttons.length ? buttons[buttons.length - 1] : null
}

class UiDialog extends HTMLElement {
    static get observedAttributes() {
        return ['open', 'data-labelledby', 'data-label']
    }

    connectedCallback() {
        if (this._dialog) return

        this._dialog = document.createElement('dialog')
        this._dialog.className = 'ui-dialog'

        while (this.firstChild) {
            this._dialog.appendChild(this.firstChild)
        }
        this.appendChild(this._dialog)

        // Escape is handled by the browser; we only forward the outcome so Elm
        // can update its own model rather than the DOM drifting out of sync.
        this._onClose = () => {
            this._forget()
            this.dispatchEvent(new CustomEvent('dialogClose', { bubbles: true }))
        }
        this._dialog.addEventListener('close', this._onClose)

        // Clicking the backdrop closes, matching every other dialog on the web.
        this._onClick = (event) => {
            if (event.target === this._dialog) this._dialog.close()
        }
        this._dialog.addEventListener('click', this._onClick)

        this._onKeydown = (event) => {
            if (event.key !== 'Enter' || event.defaultPrevented || event.isComposing) return

            const target = event.target
            const primary = primaryAction(this._dialog)

            if (isSubmitShortcut(event)) {
                event.preventDefault()
                event.stopPropagation()
                if (primary && !primary.disabled) primary.click()
                return
            }
            if (event.metaKey || event.ctrlKey || event.altKey || event.shiftKey) return

            if (this._dialog.querySelector(FIELD)) {
                if (target.tagName === 'INPUT') event.preventDefault()
                return
            }
            if (target !== this._dialog && target.closest(OWN_ENTER)) return
            if (!primary || primary.disabled) return

            event.preventDefault()
            primary.click()
        }
        this._dialog.addEventListener('keydown', this._onKeydown)

        this._sync()
    }

    disconnectedCallback() {
        if (!this._dialog) return
        this._forget()
        this._dialog.removeEventListener('close', this._onClose)
        this._dialog.removeEventListener('click', this._onClick)
        this._dialog.removeEventListener('keydown', this._onKeydown)
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
        if (!this._dialog) return

        // The dialog is named by its title. Elm can only reach this element,
        // not the dialog it creates, so the reference is forwarded.
        const labelledBy = this.getAttribute('data-labelledby')
        if (labelledBy) {
            this._dialog.setAttribute('aria-labelledby', labelledBy)
        } else {
            this._dialog.removeAttribute('aria-labelledby')
        }

        // A dialog whose content is all the caller's draws no title to point at,
        // so it carries its name instead. A dialog with no name at all is
        // announced as "dialog" and nothing more.
        const label = this.getAttribute('data-label')
        if (label) {
            this._dialog.setAttribute('aria-label', label)
        } else {
            this._dialog.removeAttribute('aria-label')
        }

        const shouldBeOpen = this.hasAttribute('open')

        if (shouldBeOpen && !this._dialog.open) {
            // showModal, not show: only the modal form makes the background
            // inert and puts the dialog in the top layer.
            this._dialog.showModal()
            openDialogs.push(this)
            this._focusPrimary()
        } else if (!shouldBeOpen && this._dialog.open) {
            this._dialog.close()
            this._forget()
        }
    }
}

if (!customElements.get('ui-dialog')) {
    customElements.define('ui-dialog', UiDialog)
}
