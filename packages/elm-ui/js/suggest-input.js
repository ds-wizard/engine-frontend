// A text field that suggests values it does not insist on.
//
// The existing wizard drew the same list under the affiliation field on the
// sign-up screen: the value is free text, and the suggestions are there to save
// typing a name someone else in the organization has already typed. What it did
// not have was a way in from the keyboard — the list answered the mouse only —
// and a `datalist`, which is what the design system had instead, is a different
// box in every browser and unstyleable in all of them.
//
// The element wraps the input the field already renders rather than making one
// of its own: everything the field wires up — its id, its label, its error, its
// `aria-describedby` — stays where it is, and this adds the popup and the
// keyboard around it. A value it sets is announced with an `input` event, which
// is the event typing sends, so the form hears about it the same way.
//
// The interaction is the combobox pattern: the input keeps focus throughout and
// a cursor moves through the options with the arrow keys
// (`aria-activedescendant`), Enter takes the one under the cursor, Escape closes
// the list and Tab leaves with whatever is typed. Arrowing moves the cursor and
// not the value — a value that changed under every arrow press would re-filter
// the list being arrowed through.
//
// The keys are heard on the element, on the way down to the input, so a key
// this uses is this one's before the field's own handler sees it. A tag field
// adds on Enter; the Enter that chooses a suggestion must not also add the half
// word typed before it, and listeners on the input itself run in the order they
// were added, which is not an order to rely on.
//
// The list can change while the field is in use — a caller fetching matches for
// what is typed — so a list that arrives while the field wants one opens. It
// wants one from focus or typing until it is dismissed: Escape, a choice, Enter,
// Tab or leaving the field.

// What is offered for what has been typed: every suggestion that contains it,
// whatever the case, and all of them for an empty field. The existing wizard's
// rule, kept so the same typing finds the same names.
function matches(query, suggestions) {
    const needle = (query || '').trim().toLowerCase()
    if (needle === '') return suggestions.slice()
    return suggestions.filter((suggestion) => suggestion.toLowerCase().includes(needle))
}

// Where the cursor goes. It wraps, so the end of the list is one press from the
// start of it, and -1 — nothing under the cursor — enters at either end.
function nextIndex(current, delta, count) {
    if (count === 0) return -1
    if (current < 0) return delta > 0 ? 0 : count - 1
    return (current + delta + count) % count
}

// Defined inside a function so the module can be required where there is no
// DOM — the guard script checks the two rules above without a browser.
function define() {
    class SuggestInput extends HTMLElement {
        constructor() {
            super()
            this._suggestions = []
            this._shown = []
            this._active = -1
            this._open = false
            this._wanted = false
        }

        static get observedAttributes() {
            return ['suggestions', 'label']
        }

        connectedCallback() {
            if (this._list) return

            this._input = this.querySelector('input')
            if (!this._input) return

            this._list = document.createElement('ul')
            this._list.className = 'ui-suggest__list'
            this._list.id = (this._input.id || 'ui-suggest') + '-suggestions'
            this._list.setAttribute('role', 'listbox')
            this._list.hidden = true
            this.appendChild(this._list)

            // The browser's own autofill draws over the list and says the same
            // kind of thing less well.
            this._input.setAttribute('autocomplete', 'off')
            this._input.setAttribute('role', 'combobox')
            this._input.setAttribute('aria-autocomplete', 'list')
            this._input.setAttribute('aria-expanded', 'false')
            this._input.setAttribute('aria-controls', this._list.id)

            this._onInput = () => this._want()
            this._onFocus = () => this._want()
            this._onBlur = () => this._dismiss()
            this._onKeyDown = (event) => this._keyDown(event)
            this._onMouseDown = (event) => this._mouseDown(event)

            this._input.addEventListener('input', this._onInput)
            this._input.addEventListener('focus', this._onFocus)
            this._input.addEventListener('blur', this._onBlur)
            this.addEventListener('keydown', this._onKeyDown, true)
            this._list.addEventListener('mousedown', this._onMouseDown)

            this._readSuggestions()
        }

        disconnectedCallback() {
            if (!this._input) return

            this._input.removeEventListener('input', this._onInput)
            this._input.removeEventListener('focus', this._onFocus)
            this._input.removeEventListener('blur', this._onBlur)
            this.removeEventListener('keydown', this._onKeyDown, true)
            if (this._list) this._list.removeEventListener('mousedown', this._onMouseDown)

            this._input = null
            this._list = null
        }

        attributeChangedCallback() {
            this._readSuggestions()
            if (this._wanted && this._input) this._openWith(this._input.value)
        }

        _readSuggestions() {
            try {
                const given = JSON.parse(this.getAttribute('suggestions') || '[]')
                this._suggestions = Array.isArray(given) ? given.filter((one) => typeof one === 'string') : []
            } catch (error) {
                // A list that cannot be read is no list: the field is still a
                // field, and typing into it still works.
                this._suggestions = []
            }

            if (this._list) this._list.setAttribute('aria-label', this.getAttribute('label') || 'Suggestions')
        }

        _openWith(query) {
            if (!this._list) return

            this._shown = matches(query, this._suggestions)
            this._active = -1
            this._render()

            this._open = this._shown.length > 0
            this._list.hidden = !this._open
            this._input.setAttribute('aria-expanded', String(this._open))
            this._input.removeAttribute('aria-activedescendant')
        }

        _want() {
            this._wanted = true
            this._openWith(this._input.value)
        }

        // Closed until the field is focused or typed into again, whatever
        // arrives in the meantime.
        _dismiss() {
            this._wanted = false
            this._close()
        }

        _close() {
            if (!this._list) return

            this._open = false
            this._active = -1
            this._list.hidden = true
            this._input.setAttribute('aria-expanded', 'false')
            this._input.removeAttribute('aria-activedescendant')
        }

        _render() {
            this._list.textContent = ''

            this._shown.forEach((suggestion, index) => {
                const option = document.createElement('li')
                option.className = 'ui-suggest__option'
                option.id = this._list.id + '-' + index
                option.setAttribute('role', 'option')
                option.setAttribute('aria-selected', 'false')
                option.textContent = suggestion
                this._list.appendChild(option)
            })
        }

        _moveTo(index) {
            const options = Array.from(this._list.children)

            options.forEach((option, i) => {
                const active = i === index
                option.classList.toggle('is-active', active)
                option.setAttribute('aria-selected', String(active))
            })

            this._active = index

            if (index < 0) {
                this._input.removeAttribute('aria-activedescendant')
                return
            }

            this._input.setAttribute('aria-activedescendant', options[index].id)

            // The list scrolls when it is longer than its box, and a cursor out
            // of sight is no cursor.
            if (typeof options[index].scrollIntoView === 'function') {
                options[index].scrollIntoView({ block: 'nearest' })
            }
        }

        _keyDown(event) {
            if (event.target !== this._input) return

            if (event.key === 'ArrowDown' || event.key === 'ArrowUp') {
                if (!this._open) this._want()
                if (!this._open) return

                event.preventDefault()
                this._moveTo(nextIndex(this._active, event.key === 'ArrowDown' ? 1 : -1, this._shown.length))
                return
            }

            if (event.key === 'Enter' && this._open && this._active >= 0) {
                // Taken here, so the form does not submit on the press that was
                // choosing a suggestion, and the field's own Enter — a tag
                // field's add — does not act on it too.
                event.preventDefault()
                event.stopPropagation()
                this._commit(this._shown[this._active])
                return
            }

            // Enter with nothing under the cursor is the field's — a submit, an
            // add — and whatever the list was offering for the old text is
            // done with.
            if (event.key === 'Enter') {
                this._dismiss()
                return
            }

            if (event.key === 'Escape' && this._open) {
                // Kept from whatever else takes Escape — a dialog around the
                // field closes on it, and the list is what was being dismissed.
                event.preventDefault()
                event.stopPropagation()
                this._dismiss()
                return
            }

            if (event.key === 'Tab') this._dismiss()
        }

        _mouseDown(event) {
            const option = event.target.closest('.ui-suggest__option')
            if (!option) return

            // The press would otherwise take focus off the field and close the
            // list before the click landed on it.
            event.preventDefault()
            this._commit(option.textContent)
        }

        _commit(value) {
            this._input.value = value
            // The event typing sends, so whatever listens for typing hears this
            // the same way — for Elm, the form's own `onInput`.
            this._input.dispatchEvent(new Event('input', { bubbles: true }))
            this._dismiss()
            this._input.focus()
        }
    }

    customElements.define('ui-suggest', SuggestInput)
}

if (typeof customElements !== 'undefined' && typeof HTMLElement !== 'undefined') {
    define()
}

module.exports = { matches, nextIndex }
