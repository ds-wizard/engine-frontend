/**
 * will process each incoming keydown event and try to match them to the list given in the shortcut attribute
 **/
export class ShortcutElement extends HTMLElement {
    connectedCallback () {
        // when the element is created: add some listeners to the body
        // we'll take them out when it's all over
        this.listener = (evt) => {
            const event = evt
            this.shortcuts
                .filter(
                    ({ baseKey, alt, shift, ctrl, meta }) =>
                        event.key.toLowerCase() === baseKey.toLowerCase() &&
                        (alt == null || alt === event.altKey) &&
                        (shift == null || shift === event.shiftKey) &&
                        (ctrl == null || ctrl === event.ctrlKey) &&
                        (meta == null || meta === event.metaKey)
                ) // now we have all the shortcuts that match the current event
                .map(({ name }) => {
                    if (event.target instanceof HTMLTextAreaElement) {
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