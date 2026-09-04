/**
 * will process each incoming keydown event and try to match them to the list given in the shortcut attribute
 **/
export class ShortcutElement extends HTMLElement {
    connectedCallback () {
        // when the element is created: add some listeners to the body
        // we'll take them out when it's all over
        this.listener = (evt) => {
            const event = evt
            const matchedShortcuts = this.shortcuts.filter(
                ({ baseKey, alt, shift, ctrl, meta }) =>
                    event.key?.toLowerCase() === baseKey?.toLowerCase() &&
                    (alt == null || alt === event.altKey) &&
                    (shift == null || shift === event.shiftKey) &&
                    (ctrl == null || ctrl === event.ctrlKey) &&
                    (meta == null || meta === event.metaKey)
            ) // now we have all the shortcuts that match the current event

            if (matchedShortcuts.length === 0) {
                return
            }

            // While a modal is open, only shortcuts belonging to the topmost open modal may
            // fire, so that a shortcut does not also trigger an action on the page behind it.
            // All modals share the same z-index, so the last one in the document is the topmost.
            // `contains` is true for the node itself, which is what modals with shortcuts are.
            const openModals = document.querySelectorAll('.modal-cover.visible')
            const topModal = openModals[openModals.length - 1]
            if (topModal && !topModal.contains(this)) {
                return
            }

            matchedShortcuts.forEach(({ name }) => {
                const isSubmitShortcut = (event.ctrlKey || event.metaKey) && event.key?.toLowerCase() === 'enter'

                // If the key event comes from an editable area (textarea, input or
                // contenteditable element), don't intercept it unless it's a submit
                // shortcut (ctrl/meta+Enter). This avoids preventing default
                // behavior in rich text editors and other editable fields.
                let targetElement = event.target
                // If the target is a text node, try to get a parent element
                if (targetElement && targetElement.nodeType && targetElement.nodeType === Node.TEXT_NODE) {
                    targetElement = targetElement.parentElement
                }

                const isInputElement = event.target instanceof HTMLTextAreaElement || event.target instanceof HTMLInputElement
                const isInContentEditable = targetElement && (targetElement.isContentEditable || (typeof targetElement.closest === 'function' && !!targetElement.closest('[contenteditable="true"]')))

                if (!isSubmitShortcut && (isInputElement || isInContentEditable)) {
                    return
                }

                event.preventDefault()
                event.stopPropagation()
                this.dispatchEvent(
                    new CustomEvent('shortcut', {
                        bubbles: false,
                        detail: {
                            name,
                            event
                        }
                    })
                )
            })
        }
        document.body.addEventListener('keydown', this.listener, { capture: true })
    }

    disconnectedCallback () {
        document.body.removeEventListener('keydown', this.listener, {
            capture: true
        })
    }
}

customElements.define('shortcut-element', ShortcutElement)
