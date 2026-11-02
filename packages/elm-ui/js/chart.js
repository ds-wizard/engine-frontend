// Chart.js behind a custom element.
//
// The existing wrapper draws a bare <canvas> with no role, no name and no text
// alternative, and its twenty colors are hard-coded hexes. A screen reader is
// given nothing at all, and reading a value means hovering it, which a keyboard
// cannot do.
//
// The element here draws the picture and nothing else: the name, the summary and
// the table of numbers are the Elm side's, because the data is already there and
// a text alternative generated from the same values cannot drift from what is
// drawn. See Ui.Chart.
//
// What is different inside:
//
//   Colors are roles. They are read from the custom properties at paint time
//   and re-read when the theme changes — data-theme on the document element,
//   the system's own preference while the page follows it, and a re-seeded
//   brand (theme.js announces ui-theme-change when it injects one) — so a chart
//   follows light and dark and a tenant's color like everything else instead
//   of carrying its own palette.
//
//   Nothing animates. Chart.js animates by default, and a chart is rebuilt
//   whenever Elm hands it data again — which a page does on any change of its
//   own, not only when the numbers change — so a chart that grew into place
//   grew again every time something else on the page moved. A chart is read,
//   and still is what it is read as (and 2.3.3 asks for no less).

// chart.js/auto's CommonJS build *is* the constructor, with the named exports
// hung off it; its ESM build has a default. Taking one and assuming the other
// leaves Chart undefined, which surfaces as "Chart is not a constructor" the
// first time an element is connected — so take whichever is there.
const chartModule = require('chart.js/auto')
const Chart = chartModule.Chart || chartModule.default || chartModule

const TYPES = {
    bar: 'bar',
    line: 'line',
    doughnut: 'doughnut',
    radar: 'radar'
}

const SERIES = 8

// What theme.js dispatches on the document after injecting derived brand roles.
const THEME_EVENT = 'ui-theme-change'

// The roles, resolved. getPropertyValue returns whatever the cascade settled on,
// which is what makes a theme switch a re-read rather than a rebuild.
function role(name, fallback) {
    if (typeof getComputedStyle !== 'function' || typeof document === 'undefined') return fallback

    const value = getComputedStyle(document.documentElement).getPropertyValue(name)
    return value && value.trim() ? value.trim() : fallback
}

function palette(read = role) {
    const colors = []

    for (let index = 1; index <= SERIES; index += 1) {
        colors.push(read('--ui-chart-' + index, '#0074a3'))
    }

    return colors
}

// Every color a chart is painted in, read afresh for each paint.
function colors(read = role) {
    return {
        series: palette(read),
        text: read('--ui-text-muted', '#5b5e61'),
        grid: read('--ui-border-subtle', '#e0e3e7'),
        surface: read('--ui-surface', '#ffffff'),
        brand: read('--ui-brand', '#2563eb'),
        brandStrong: read('--ui-brand-strong', '#2461e9')
    }
}

// A role at a lower opacity, for a radar's fill: the shape has to show without
// hiding the grid under it. Roles are hex in this system, and the runtime theme
// writes hex too; rgb() is handled for an override written that way. Anything
// else is returned untouched rather than guessed at.
function translucent(color, alpha) {
    const value = String(color).trim()

    let match = /^#([0-9a-f])([0-9a-f])([0-9a-f])$/i.exec(value)
    if (match) {
        const [r, g, b] = match.slice(1).map((digit) => parseInt(digit + digit, 16))
        return `rgba(${r}, ${g}, ${b}, ${alpha})`
    }

    match = /^#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$/i.exec(value)
    if (match) {
        const [r, g, b] = match.slice(1).map((pair) => parseInt(pair, 16))
        return `rgba(${r}, ${g}, ${b}, ${alpha})`
    }

    match = /^rgba?\(\s*([\d.]+)[\s,]+([\d.]+)[\s,]+([\d.]+)/i.exec(value)
    if (match) {
        return `rgba(${match[1]}, ${match[2]}, ${match[3]}, ${alpha})`
    }

    return value
}

// How far from a point the pointer may be and still get its value: half the
// system's 24px hit-target floor (--ui-hit-min, WCAG 2.5.8), so every point
// catches a 24px circle however small it is drawn. The drawn point stays small —
// it is a mark on a line, not a button — and grows a little under the pointer so
// the catch is visible.
const POINT_HIT_RADIUS = 12
const POINT_RADIUS = 3
const POINT_HOVER_RADIUS = 5

// The Chart.js configuration for one chart, from its attributes, its data and
// the resolved colors. Pure, so the contract test can check it without a
// canvas.
function buildConfig({ chartType, data, legend, xTitle, yTitle }, paint) {
    const type = TYPES[chartType] || 'bar'
    const categories = data.categories || []
    const range = data.range || null

    const datasets = (data.series || []).map((series, index) => {
        if (type === 'radar') return radarDataset(series, index, paint)

        // A doughnut is one series whose *slices* are the categories, so its
        // colors vary within the dataset rather than between datasets.
        const color =
            type === 'doughnut'
                ? categories.map((_, slice) => paint.series[slice % paint.series.length])
                : paint.series[index % paint.series.length]

        const points =
            type === 'line'
                ? { pointRadius: POINT_RADIUS, pointHoverRadius: POINT_HOVER_RADIUS, pointHitRadius: POINT_HIT_RADIUS }
                : {}

        return Object.assign(
            {
                label: series.label,
                data: series.values,
                backgroundColor: color,
                borderColor: type === 'line' ? color : paint.surface,
                borderWidth: type === 'doughnut' ? 2 : 1,
                fill: false,
                tension: 0.2
            },
            points
        )
    })

    let scales = {}
    if (type === 'radar') scales = { r: radialScale(range, paint) }
    else if (type !== 'doughnut') scales = cartesianScales(xTitle, yTitle, range, paint)

    return {
        type,
        data: { labels: categories, datasets },
        options: {
            animation: false,
            // A radar is round, and squeezed into a letterbox its spokes are
            // too short to read.
            aspectRatio: type === 'radar' ? 1 : 2,
            plugins: {
                legend: {
                    display: legend,
                    labels: { color: paint.text }
                }
            },
            scales
        }
    }
}

// The first series of a radar is the brand's: the line and the points in
// brand-strong, which Ui.Theme.Derive holds at 4.5:1 on the extreme surface for
// any seed, so the shape clears the 3:1 a meaningful graphic needs (1.4.11); the
// fill is the brand itself, translucent, and carries nothing the line does not.
// Further series take the chart palette in order, so no two share a color.
function radarDataset(series, index, paint) {
    const line = index === 0 ? paint.brandStrong : paint.series[(index - 1) % paint.series.length]
    const fill = index === 0 ? paint.brand : line

    return {
        label: series.label,
        data: series.values,
        borderColor: line,
        backgroundColor: translucent(fill, 0.2),
        pointBackgroundColor: line,
        pointBorderColor: paint.surface,
        pointHoverBackgroundColor: line,
        pointHoverBorderColor: paint.surface,
        pointRadius: POINT_RADIUS,
        pointHoverRadius: POINT_HOVER_RADIUS,
        pointHitRadius: POINT_HIT_RADIUS,
        borderWidth: 2,
        fill: true
    }
}

// The rings are quarters of the range — four of them, 0.25 to 1 for a measure —
// and they carry no numbers, as the wizard's did not: a number on each ring sits
// on the spokes and on the shape, and the exact values are in the data table the
// element renders beside the canvas. Without a range Chart.js picks its own.
function radialScale(range, paint) {
    return {
        min: range ? range.min : undefined,
        max: range ? range.max : undefined,
        beginAtZero: true,
        grid: { color: paint.grid },
        angleLines: { color: paint.grid },
        // The spoke names, which are what the shape is read against.
        pointLabels: { color: paint.text },
        ticks: {
            display: false,
            stepSize: range ? (range.max - range.min) / 4 : undefined
        }
    }
}

function cartesianScales(xTitle, yTitle, range, paint) {
    const axis = (title) => ({
        ticks: { color: paint.text },
        grid: { color: paint.grid },
        title: title ? { display: true, text: title, color: paint.text } : { display: false }
    })

    const y = axis(yTitle)
    if (range) {
        y.min = range.min
        y.max = range.max
    }

    return { x: axis(xTitle), y }
}

// Takes the constructor so the contract test can hand it a stand-in: a canvas
// is the one thing Node cannot provide.
function define(ChartImpl = Chart) {
    class ChartElement extends HTMLElement {
        constructor() {
            super()
            this._data = null
            this._chart = null
        }

        static get observedAttributes() {
            return ['chart-type', 'x-title', 'y-title', 'legend']
        }

        attributeChangedCallback() {
            this._rebuild()
        }

        get chartData() {
            return this._data
        }

        set chartData(value) {
            this._data = value
            this._rebuild()
        }

        connectedCallback() {
            if (this._canvas) return

            this._canvas = document.createElement('canvas')
            this._appendCanvas()
            this._watchTheme()
            this._rebuild()
        }

        disconnectedCallback() {
            this._unwatchTheme()

            if (this._chart) {
                this._chart.destroy()
                this._chart = null
            }

            this.innerHTML = ''
            this._canvas = null
        }

        _appendCanvas() {
            // The canvas is inside an element that already carries role="img"
            // and a name, so it is presentational and must not be announced
            // again — Chart.js would otherwise leave its fallback text to be
            // read as the whole of the chart.
            this._canvas.setAttribute('aria-hidden', 'true')
            this.appendChild(this._canvas)
        }

        _rebuild() {
            if (!this._canvas || !this._data) return

            if (this._chart) {
                this._chart.destroy()
                this._chart = null
            }

            this._chart = new ChartImpl(this._canvas.getContext('2d'), this._config())
        }

        // Re-read rather than recompute: the values come from the cascade, so
        // switching theme, the system switching it and a new brand being
        // injected are the same event. All three are watched for every chart
        // type, and all three are let go on disconnect.
        _watchTheme() {
            this._onThemeChange = () => this._repaint()

            this._observer = new MutationObserver(this._onThemeChange)
            this._observer.observe(document.documentElement, {
                attributes: true,
                attributeFilter: ['data-theme']
            })

            this._media = window.matchMedia('(prefers-color-scheme: dark)')
            this._media.addEventListener('change', this._onThemeChange)

            document.addEventListener(THEME_EVENT, this._onThemeChange)
        }

        _unwatchTheme() {
            if (this._observer) this._observer.disconnect()
            if (this._media) this._media.removeEventListener('change', this._onThemeChange)
            if (this._onThemeChange) document.removeEventListener(THEME_EVENT, this._onThemeChange)

            this._observer = null
            this._media = null
            this._onThemeChange = null
        }

        _repaint() {
            if (!this._chart) return

            const config = this._config()
            this._chart.data = config.data
            this._chart.options = config.options
            this._chart.update('none')
        }

        _config() {
            return buildConfig(
                {
                    chartType: this.getAttribute('chart-type'),
                    data: this._data,
                    legend: this.getAttribute('legend') !== 'false',
                    xTitle: this.getAttribute('x-title'),
                    yTitle: this.getAttribute('y-title')
                },
                colors()
            )
        }
    }

    customElements.define('ui-chart', ChartElement)
    return ChartElement
}

if (typeof customElements !== 'undefined' && typeof HTMLElement !== 'undefined') {
    define()
}

module.exports = { Chart, TYPES, SERIES, THEME_EVENT, palette, colors, translucent, buildConfig, define }
