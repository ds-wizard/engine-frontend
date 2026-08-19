const flatpickr = require('flatpickr').default


class DatePicker extends HTMLElement {
    constructor(options) {
        super()
        this._options = options
        this._datePickerValue = ''
        this._input = null
    }

    get datePickerValue() {
        return this._datePickerValue
    }

    set datePickerValue(dateValue) {
        if (this._datePickerValue === dateValue) return
        this._datePickerValue = dateValue
        if (!this._instance) return
        this._instance.setDate(dateValue)
    }

    static get observedAttributes() {
        return ['input-id']
    }

    attributeChangedCallback(name, oldValue, newValue) {
        if (name === 'input-id' && oldValue !== newValue) {
            this._syncInputId()
        }
    }

    connectedCallback() {
        const wrapper = document.createElement('div')
        wrapper.innerHTML = '<input type="text" class="form-control form-control-flatpickr" data-input>'
        this.appendChild(wrapper)
        this._input = wrapper.querySelector('input')
        this._syncInputId()

        this._options.wrap = true
        this._options.onChange = (selectedDates, dateStr) => {
            this._removeErrorElement()
            this._datePickerValue = dateStr
            this.dispatchEvent(new CustomEvent('datePickerChanged'))
        }
        this._options.errorHandler = (error) => {
            const value = error.toString().replace('Error: Invalid date provided: ', '')

            if (value) {
                this._removeErrorElement()
                this._errorElement = document.createElement('div')
                this._errorElement.classList.add('alert', 'alert-warning')
                this._errorElement.innerHTML = `<i class="fa fas fa-exclamation-triangle"></i> The saved value "${value}" is invalid.`
                this.appendChild(this._errorElement)
            }
        }

        this._instance = flatpickr(wrapper, this._options)
        this._instance.setDate(this._datePickerValue)
    }

    _syncInputId() {
        if (!this._input) return

        const id = this.getAttribute('input-id')
        if (id) {
            this._input.id = id
            this._input.name = id
        } else {
            this._input.removeAttribute('id')
            this._input.removeAttribute('name')
        }
    }

    _removeErrorElement() {
        if (this._errorElement) {
            this.removeChild(this._errorElement)
        }
    }
}


customElements.define('date-picker', class extends DatePicker {
    constructor() {
        super({})
    }
})

customElements.define('date-picker-utc', class extends DatePicker {
    constructor() {
        let today = new Date()

        super({
            dateFormat: 'Z',
            altInput: true,
            altFormat: 'Y-m-d',
            enable: [(date) => date > today]
        })
    }
})

customElements.define('datetime-picker', class extends DatePicker {
    constructor() {
        super({
            enableTime: true,
            time_24hr: true
        })
    }
})

customElements.define('time-picker', class extends DatePicker {
    constructor() {
        super({
            enableTime: true,
            time_24hr: true,
            noCalendar: true
        })
    }
})
