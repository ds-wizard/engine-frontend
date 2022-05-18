const flatpickr = require('flatpickr').default


class DatePicker extends HTMLElement {
    constructor(options) {
        super()
        this._options = options
        this._datePickerValue = ''
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

    connectedCallback() {
        const wrapper = document.createElement('div')
        wrapper.classList.add('input-group')
        wrapper.innerHTML = '<input type="text" class="form-control" data-input><div class="input-group-append"><button class="btn btn-outline-secondary" data-clear><i class="fa fas fa-times"></i></button>'
        this.appendChild(wrapper)

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
