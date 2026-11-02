// Date, date-and-time and time fields, on flatpickr.
//
// The element owns the input rather than taking one from Elm: flatpickr mutates
// the input's value and inserts a second one for the alternate format, and
// nothing good comes of two systems patching the same node. Elm passes the
// value in as a property and hears about changes through an event, which is the
// same contract the existing date pickers use.
//
// What is different from the existing ones, and why:
//
//   allowInput   The existing pickers leave the input readonly, which is
//                flatpickr's default. That makes the calendar the only way to
//                set a date, and the calendar's days are not in the tab order —
//                so a keyboard user cannot fill the field at all. Typing is the
//                accessible path (2.1.1), and the calendar is the shortcut.
//
//   ArrowDown    With allowInput on, flatpickr stops handling the arrow keys
//                while focus is in the input, so the grid becomes unreachable
//                the other way. One listener puts focus on the selected day,
//                after which flatpickr's own arrow handling takes over.
//
//   ARIA         flatpickr labels its days and its year box and stops there.
//                The calendar gets a dialog role and a name, the input says
//                what it has and how to open it, and the month header announces
//                itself when it changes (4.1.2, 4.1.3).
//
//   No toggle    There was a button to open the calendar, and it was in the way
//                twice over: clicking or focusing the input already opens it, so
//                the button did nothing the field did not — and opening *from*
//                the button left focus on the button, where none of the
//                keyboard handling below applies. The calendar glyph stays as a
//                glyph, which is what it was doing usefully.

const flatpickr = require('flatpickr').default

// The value the field carries is the machine-readable one, and it does not
// depend on where the reader is: a date is a calendar date, a time is a wall
// clock time, and a date *and* time is an instant, which is only unambiguous
// with a zone. The visible text is the local rendering of that.
const MODES = {
    date: {
        placeholder: 'YYYY-MM-DD',
        options: { dateFormat: 'Y-m-d' }
    },
    datetime: {
        placeholder: 'YYYY-MM-DD HH:MM',
        options: {
            dateFormat: 'Z',
            altInput: true,
            altFormat: 'Y-m-d H:i',
            enableTime: true,
            time_24hr: true,
            minuteIncrement: 1
        }
    },
    time: {
        placeholder: 'HH:MM',
        options: {
            dateFormat: 'H:i',
            enableTime: true,
            noCalendar: true,
            time_24hr: true,
            minuteIncrement: 1
        }
    }
}

function reducedMotion() {
    return (
        typeof window !== 'undefined' &&
        typeof window.matchMedia === 'function' &&
        window.matchMedia('(prefers-reduced-motion: reduce)').matches
    )
}

function optionsFor(mode) {
    const config = MODES[mode] || MODES.date

    return Object.assign({}, config.options, {
        wrap: true,
        allowInput: true,
        animate: !reducedMotion(),
        // Appended to the body rather than to this element, so an overflow
        // container cannot clip it. It is repositioned while anything scrolls,
        // which is the cost of that choice.
        static: false
    })
}

// Where a calendar belongs: inside the modal dialog its field is in, and
// otherwise on the body. A modal dialog is in the top layer and makes
// everything outside it inert, so a calendar on the body would be painted under
// the dialog and would refuse the clicks meant for it.
function calendarHost(element, body) {
    return element.closest('dialog') || body
}

// How an open calendar is shown, given where its field is and what the browser
// can do. In a dialog it goes in the top layer as a popover: the dialog is a
// scrolling box, so a calendar laid out inside it is clipped by it and drifts
// against the field as it scrolls. A top-layer box is positioned against the
// page, which is what flatpickr measures — so on that path flatpickr's own
// positioning is left alone. Without the popover API the calendar stays laid
// out in the dialog, clipped as before but reachable.
function calendarPlacement({ inDialog, canPopover }) {
    if (!inDialog) return 'page'
    return canPopover ? 'popover' : 'dialog'
}

// Where it sits inside that host. flatpickr measures from the page, which is
// right for the body and wrong for a dialog — a dialog is positioned, so the
// calendar's offsets are measured from its padding box. Under the field, unless
// it would run off the bottom of the window and there is room above it, which
// is the choice flatpickr makes on the page.
function calendarOffset({ field, host, hostBorder, calendarHeight, viewportHeight }) {
    const below = field.bottom + 2
    const above = field.top - calendarHeight - 2
    const flip = below + calendarHeight > viewportHeight && above > 0

    return {
        top: (flip ? above : below) - host.top - hostBorder.top,
        left: field.left - host.left - hostBorder.left,
        flip
    }
}

function placeholderFor(mode) {
    return (MODES[mode] || MODES.date).placeholder
}

// What the field says of its own accord. Elm passes the page's language in the
// `words` attribute as JSON: `clear` is the whole name of the clear button
// ("Clear start date"), and `invalidValue` is the message for a stored value the
// picker cannot read, with `{value}` where the value goes. Without them, or
// with an unreadable attribute, the English below — built from `label`.
function wordsFrom(attribute, label) {
    const words = {
        clear: 'Clear ' + (label || 'date'),
        invalidValue: 'The stored value "{value}" is not a valid date.'
    }

    if (!attribute) return words

    try {
        const given = JSON.parse(attribute)
        for (const key of Object.keys(words)) {
            if (given && typeof given[key] === 'string' && given[key] !== '') words[key] = given[key]
        }
    } catch (error) {
        // English, as without the attribute.
    }

    return words
}

// Defined inside a function so that the module can be required where there is
// no DOM — the guard script checks the value formats without a browser, and a
// bare `extends HTMLElement` would throw before it got the chance.
function define() {
    class DatePicker extends HTMLElement {
        constructor() {
            super()
            this._pickerValue = ''
            this._instance = null
            this._input = null
            this._hostDialog = null
            this._placement = null
        }

        get pickerValue() {
            return this._pickerValue
        }

        set pickerValue(value) {
            if (this._pickerValue === value) return
            this._pickerValue = value
            if (!this._instance) return
            // false: setting the value from the outside is not a change the outside
            // needs telling about, and saying so would loop.
            this._instance.setDate(value, false)
        }

        static get observedAttributes() {
            return ['input-id', 'describedby', 'placeholder', 'invalid', 'disabled', 'required', 'label', 'words']
        }

        attributeChangedCallback() {
            this._syncInput()
        }

        connectedCallback() {
            if (this._instance) return

            const mode = this.getAttribute('mode') || 'date'

            this.innerHTML =
                '<span class="ui-date-picker__field">' +
                '<input type="text" class="ui-input ui-date-picker__input" data-input>' +
                '<span class="ui-date-picker__icon" aria-hidden="true"></span>' +
                '<button type="button" class="ui-date-picker__clear" data-clear hidden></button>' +
                '</span>'

            this._input = this.querySelector('[data-input]')
            this._clear = this.querySelector('[data-clear]')
            this._icon = this.querySelector('.ui-date-picker__icon')

            this._clear.innerHTML = '<i class="ui-icon fas fa-xmark" aria-hidden="true"></i>'
            this._icon.innerHTML =
                '<i class="ui-icon fas ' +
                (mode === 'time' ? 'fa-clock' : 'fa-calendar-days') +
                '"></i>'

            this._instance = flatpickr(this, Object.assign(optionsFor(mode), {
                onReady: (dates, value, instance) => this._onReady(instance),
                onOpen: () => this._onOpen(),
                onClose: () => this._onClose(),
                onChange: (dates, value) => this._onChange(value),
                errorHandler: (error) => this._onError(error)
            }))

            this._instance.setDate(this._pickerValue, false)
            this._syncInput()
        }

        // The picker is torn down with the element: it binds document listeners and
        // appends the calendar to the body, neither of which goes away on its own.
        disconnectedCallback() {
            this._hidePopover()
            this._unwatchScroll()

            if (this._instance) {
                this._instance.destroy()
                this._instance = null
            }

            this._input = null
            this._hostDialog = null
            this.innerHTML = ''
        }

        // The input flatpickr shows. With an alternate format it is a second input
        // that flatpickr made, and it carries none of the first one's identity —
        // which is why every attribute is applied here rather than in the markup.
        get _visibleInput() {
            if (!this._instance) return this._input
            return this._instance.altInput || this._instance.input
        }

        _onReady(instance) {
            // onReady fires from inside the flatpickr() call, before the
            // constructor has returned and before this._instance is assigned.
            // Everything below reads the instance, and the one that matters is
            // the visible input: with an alternate format the original input is
            // hidden and a second one is what the reader sees, so an id or a
            // listener put on the first goes nowhere.
            this._instance = instance

            const calendar = instance.calendarContainer
            const id = (this.getAttribute('input-id') || 'ui-date-picker') + '-calendar'

            calendar.id = id
            calendar.setAttribute('role', 'dialog')
            calendar.setAttribute('aria-modal', 'false')
            calendar.setAttribute('aria-label', this.getAttribute('label') || 'Calendar')
            calendar.classList.add('ui-flatpickr')

            // The month and year change under the reader without anything being
            // said. Polite, because it is a consequence of what they just pressed.
            const header = calendar.querySelector('.flatpickr-current-month')
            if (header) {
                header.setAttribute('aria-live', 'polite')
                header.setAttribute('aria-atomic', 'true')
            }

            // On the input, because the input is what opens it. All three are
            // global properties, so they are valid on a plain text box —
            // aria-expanded is not, which is why the state is not announced
            // here and the hint says how to open it instead.
            const input = instance.altInput || instance.input
            input.setAttribute('aria-haspopup', 'dialog')
            input.setAttribute('aria-controls', id)
            input.setAttribute('aria-keyshortcuts', 'ArrowDown')

            this._syncInput()
            this._visibleInput.addEventListener('keydown', (event) => this._onInputKeyDown(event))
            calendar.addEventListener('keydown', (event) => this._onCalendarKeyDown(event))
        }

        // Down or up from the input moves into the picker. flatpickr stops
        // handling the arrows once the input can be typed into, so without this
        // everything past the text field is reachable by pointer only.
        _onInputKeyDown(event) {
            if (event.key !== 'ArrowDown' && event.key !== 'ArrowUp') return
            if (!this._instance) return

            event.preventDefault()

            if (!this._instance.isOpen) this._instance.open()

            const target = this._firstStop()
            if (target) target.focus()
        }

        // Where the picker starts: the selected day, or today, or the first day
        // that can be chosen — and the hour when there is no calendar at all.
        _firstStop() {
            const instance = this._instance
            const calendar = instance.calendarContainer

            if (instance.config.noCalendar) return instance.hourElement

            return (
                calendar.querySelector('.flatpickr-day.selected') ||
                calendar.querySelector('.flatpickr-day.today') ||
                calendar.querySelector('.flatpickr-day:not(.flatpickr-disabled)')
            )
        }

        // Tab out of the calendar goes nowhere useful on its own: the calendar
        // is appended to the body, so the next thing in document order is the
        // end of the document and focus lands in the browser's own chrome.
        // flatpickr handles Shift+Tab from the grid and not Tab.
        //
        // Forward from the grid goes to the time, if there is one — it is the
        // next thing in the picker and nothing else reaches it — and otherwise
        // back to the field, from where the next Tab carries on through the
        // form.
        _onCalendarKeyDown(event) {
            if (event.key !== 'Tab' || event.shiftKey) return

            const instance = this._instance
            if (!instance) return

            const time = instance.timeContainer
            if (time && time.contains(event.target)) return

            event.preventDefault()

            if (time && instance.hourElement) {
                instance.hourElement.focus()
                return
            }

            instance.close()
            this._visibleInput.focus()
        }

        _onOpen() {
            this._claimCalendar()

            // After flatpickr has placed it: `onOpen` is announced before the
            // calendar is positioned, so anything measured here is overwritten
            // a line later. A frame, which still runs before the paint that
            // first shows it. Only on the path where we do the measuring —
            // a popover is positioned against the page, as flatpickr assumes.
            if (this._placement === 'dialog') requestAnimationFrame(() => this._positionInHost())

            this._watchScroll()
        }

        // Decided every time the calendar opens rather than once at mount: the
        // dialog element moves its content into the `dialog` it makes, and the
        // same field can be opened inside one and outside one.
        _claimCalendar() {
            if (!this._instance) return

            const host = calendarHost(this, document.body)
            this._hostDialog = host === document.body ? null : host

            const calendar = this._instance.calendarContainer
            if (calendar.parentNode !== host) host.appendChild(calendar)

            this._placement = calendarPlacement({
                inDialog: Boolean(this._hostDialog),
                canPopover: typeof calendar.showPopover === 'function'
            })

            if (this._placement === 'popover') this._showPopover(calendar)
        }

        // The top layer, where the dialog itself is: nothing the dialog does to
        // its own overflow can clip it, and it is painted over the dialog
        // rather than under it. Manual, because what opens and closes it is the
        // field, not a click anywhere on the page.
        _showPopover(calendar) {
            calendar.setAttribute('popover', 'manual')

            try {
                calendar.showPopover()
            } catch (error) {
                // Already showing, or shown from a state the browser refuses:
                // the calendar is still in the dialog and still usable.
            }
        }

        _hidePopover() {
            if (this._placement !== 'popover') return

            const calendar = this._instance && this._instance.calendarContainer
            this._placement = null
            if (!calendar) return

            try {
                calendar.hidePopover()
            } catch (error) {
                // Not showing: nothing to take back.
            }

            calendar.removeAttribute('popover')
        }

        _onClose() {
            this._hidePopover()
            this._unwatchScroll()
        }

        // The calendar hangs off the body, so anything that scrolls underneath it
        // leaves it behind. Capture, because the scrolling is usually a panel
        // rather than the window.
        _watchScroll() {
            if (this._scrollListener) return

            this._scrollListener = () => {
                if (!this._instance || !this._instance.isOpen) return

                if (this._placement === 'dialog') this._positionInHost()
                else this._instance._positionCalendar(this._visibleInput)
            }

            window.addEventListener('scroll', this._scrollListener, true)
            window.addEventListener('resize', this._scrollListener)
        }

        _unwatchScroll() {
            if (!this._scrollListener) return

            window.removeEventListener('scroll', this._scrollListener, true)
            window.removeEventListener('resize', this._scrollListener)
            this._scrollListener = null
        }

        _positionInHost() {
            if (!this._hostDialog || !this._instance) return

            const calendar = this._instance.calendarContainer
            const offset = calendarOffset({
                field: this._visibleInput.getBoundingClientRect(),
                host: this._hostDialog.getBoundingClientRect(),
                hostBorder: { top: this._hostDialog.clientTop, left: this._hostDialog.clientLeft },
                calendarHeight: calendar.offsetHeight,
                viewportHeight: window.innerHeight
            })

            calendar.style.top = offset.top + 'px'
            calendar.style.left = offset.left + 'px'
            calendar.style.right = 'auto'
            calendar.classList.toggle('arrowTop', !offset.flip)
            calendar.classList.toggle('arrowBottom', offset.flip)
        }

        _onChange(value) {
            this._removeError()
            this._pickerValue = value
            this._showControls()
            this.dispatchEvent(new CustomEvent('uiDatePickerChanged'))
        }

        // One slot at the end of the field, and two things that could sit in
        // it. The glyph says what kind of field this is while it is empty;
        // once it has a value the value says that, and the slot is worth more
        // as a way to empty it again.
        _showControls() {
            const hasValue = Boolean(this._pickerValue)

            // Clearing removes the button that was just pressed. Focus would go
            // with it — to the body, which is nowhere — so it goes back to the
            // field, which is where the next thing anyone does will happen.
            if (!hasValue && this._clear.contains(document.activeElement)) {
                this._visibleInput.focus()
            }

            this._clear.hidden = !hasValue
            this._icon.hidden = hasValue
        }

        _syncInput() {
            const input = this._visibleInput
            if (!input) return

            const id = this.getAttribute('input-id')
            if (id) {
                input.id = id
                input.name = id
            }

            const describedby = this.getAttribute('describedby')
            if (describedby) input.setAttribute('aria-describedby', describedby)
            else input.removeAttribute('aria-describedby')

            const mode = this.getAttribute('mode') || 'date'
            input.placeholder = this.getAttribute('placeholder') || placeholderFor(mode)

            input.classList.add('ui-input', 'ui-date-picker__input')
            input.classList.toggle('is-invalid', this.hasAttribute('invalid'))

            if (this.hasAttribute('invalid')) input.setAttribute('aria-invalid', 'true')
            else input.removeAttribute('aria-invalid')

            input.disabled = this.hasAttribute('disabled')
            input.required = this.hasAttribute('required')
            this._clear.disabled = input.disabled
            this._showControls()

            const words = wordsFrom(this.getAttribute('words'), this.getAttribute('label'))
            this._clear.setAttribute('aria-label', words.clear)
        }

        // A stored value the picker cannot parse. Saying so beats showing an empty
        // field, which reads as "there is nothing here" rather than "this is wrong".
        _onError(error) {
            const value = error.toString().replace('Error: Invalid date provided: ', '')
            if (!value) return

            this._removeError()
            this._errorElement = document.createElement('p')
            this._errorElement.className = 'ui-field__error'
            const words = wordsFrom(this.getAttribute('words'), this.getAttribute('label'))
            this._errorElement.textContent = words.invalidValue.split('{value}').join(value)
            this.appendChild(this._errorElement)
        }

        _removeError() {
            if (!this._errorElement) return
            this._errorElement.remove()
            this._errorElement = null
        }
    }

    customElements.define('ui-date-picker', DatePicker)
}

if (typeof customElements !== 'undefined' && typeof HTMLElement !== 'undefined') {
    define()
}

module.exports = { MODES, calendarHost, calendarOffset, calendarPlacement, optionsFor, placeholderFor, wordsFrom }
