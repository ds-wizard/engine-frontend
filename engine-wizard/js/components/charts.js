customElements.define('chart-radar', class extends HTMLElement {
    constructor() {
        super();
        this._chartData = null
    }

    get chartData() {
        return this._chartData
    }

    set chartData(chartData) {
        if (this._chartData === chartData) return
        this._chartData = chartData
        if (!this._chart) return
        this._chart.data = this._chartData
        this._chart.update()
    }

    connectedCallback() {
        const canvas = document.createElement('canvas')
        this.appendChild(canvas)

        const ctx = canvas.getContext('2d')

        this._chart = new Chart(ctx, {
            type: 'radar',
            data: this._chartData,
            options: {
                animation: {
                    duration: 0
                },
                aspectRatio: 2,
                plugins: {
                    legend: {
                        display: false
                    },
                },
                scales: {
                    r : {
                        min: 0,
                        max: 1,
                        ticks: {
                            stepSize: .25,
                            display: false
                        }
                    },
                }
            }
        })
    }
})