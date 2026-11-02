// The chart wrapper's contract, checked without a browser.
//
// Two things are worth guarding, and neither is visible to elm-test or
// elm-review:
//
//   The colors are roles. The wrapper this replaces carries twenty hard-coded
//   hexes, so its charts stay the same in dark mode and ignore a tenant's theme
//   entirely. Reading them from the custom properties is what makes a chart
//   follow the rest of the system, and it is one line away from being undone.
//
//   The palette is eight long and the element knows it. A ninth series wraps
//   round to the first rather than landing on undefined, which is what a missing
//   modulo looks like on screen: two series in the same color.

const assert = require('assert')
const fs = require('fs')
const path = require('path')

const source = fs
    .readFileSync(path.join(__dirname, '../js/chart.js'), 'utf8')
    .split('\n')
    .filter((line) => !/^\s*(\/\/|\*|\/\*)/.test(line))
    .join('\n')

const { Chart, TYPES, SERIES, THEME_EVENT, palette, translucent, buildConfig, define } = require('../js/chart')

// The import shape, which is the one thing here that fails at run time and
// nowhere else: chart.js/auto's CommonJS build is the constructor itself and its
// ESM build has a default, so reaching for the wrong one leaves Chart undefined
// and every chart throws "Chart is not a constructor" on connect.
assert.strictEqual(typeof Chart, 'function', 'Chart must resolve to a constructor')
assert.ok(Chart.prototype, 'Chart must be constructible')

assert.deepStrictEqual(Object.keys(TYPES).sort(), ['bar', 'doughnut', 'line', 'radar'])

// Every type maps to a Chart.js type rather than being passed through, so an
// unknown one cannot reach the library.
for (const [name, type] of Object.entries(TYPES)) {
    assert.strictEqual(typeof type, 'string', `${name} must map to a Chart.js type`)
}

assert.strictEqual(SERIES, 8, 'the palette and --ui-chart-N must agree')

// Without a document, every color falls back rather than becoming undefined —
// a chart drawn in undefined is a chart drawn in black.
const colors = palette()
assert.strictEqual(colors.length, SERIES)
for (const color of colors) {
    assert.ok(/^#[0-9a-f]{6}$/i.test(color), `${color} is not a color`)
}

// The roles themselves: named here so renaming one in _roles.scss without
// renaming it here is a failure rather than a chart that silently goes black.
assert.ok(/--ui-chart-/.test(source), 'series colors come from --ui-chart-N')
assert.ok(/--ui-text-muted/.test(source), 'labels and ticks take the muted text role')
assert.ok(/--ui-border-subtle/.test(source), 'grid lines take the subtle border role')
assert.ok(/--ui-brand-strong/.test(source), "a radar's line takes the brand's strong role")
assert.ok(/--ui-brand'/.test(source), "a radar's fill takes the brand role")
// Hexes are allowed only as the fallback argument to a role lookup, which is
// what happens when there is no document to read from. A palette assembled
// from literals is the thing being replaced.
for (const match of source.matchAll(/'#[0-9a-f]{6}'/gi)) {
    const line = source.slice(0, match.index).split('\n').pop()
    assert.ok(
        /(role|read)\('--ui-/.test(line),
        `${match[0]} is a literal color outside a role fallback`
    )
}

// Redrawn when the theme changes, or a chart keeps light-mode colors in dark.
assert.ok(/MutationObserver/.test(source), 'a theme switch must repaint the chart')
assert.ok(/prefers-color-scheme/.test(source), 'the system switching must repaint it too')

// ---------------------------------------------------------------------------
// The radar, as configured. Colors come in from a stand-in for the cascade, so
// each one is traceable to the role it was read from.

const paint = {
    series: ['#000001', '#000002', '#000003', '#000004', '#000005', '#000006', '#000007', '#000008'],
    text: '#111111',
    grid: '#222222',
    surface: '#333333',
    brand: '#f15a24',
    brandStrong: '#c33d00'
}

const radar = buildConfig(
    {
        chartType: 'radar',
        data: {
            categories: ['Findability', 'Accessibility', 'Interoperability'],
            series: [
                { label: 'Measure', values: [0.5, 0.25, 1] },
                { label: 'Last year', values: [0.2, 0.2, 0.4] }
            ],
            range: { min: 0, max: 1 }
        },
        legend: false
    },
    paint
)

assert.strictEqual(radar.type, 'radar')
assert.deepStrictEqual(radar.data.labels, ['Findability', 'Accessibility', 'Interoperability'])
assert.strictEqual(radar.options.animation, false, 'a radar does not animate')
assert.strictEqual(radar.options.plugins.legend.display, false)

// The first series is the brand's: line and points in brand-strong, the fill
// the brand, translucent — so a re-seeded brand is what the shape is drawn in.
const [first, second] = radar.data.datasets
assert.strictEqual(first.borderColor, '#c33d00', 'the line is brand-strong')
assert.strictEqual(first.pointBackgroundColor, '#c33d00', 'the points are brand-strong')
assert.strictEqual(first.backgroundColor, 'rgba(241, 90, 36, 0.2)', 'the fill is the brand, translucent')
assert.strictEqual(first.fill, true)
// A point is small to look at and 24px to point at — the hit-target floor — so
// the value comes up without the pointer having to land on the dot itself.
assert.strictEqual(first.pointHitRadius, 12, 'a radar point catches the pointer 12px out: a 24px target')
assert.ok(first.pointHoverRadius > first.pointRadius, 'a point grows under the pointer so the catch is visible')
// A second series takes the palette from its start, not the brand again.
assert.strictEqual(second.borderColor, '#000001')

// The spokes run over the range given, in the system's roles.
const r = radar.options.scales.r
assert.strictEqual(r.min, 0)
assert.strictEqual(r.max, 1)
assert.strictEqual(r.grid.color, '#222222', 'rings take the subtle border role')
assert.strictEqual(r.angleLines.color, '#222222', 'spokes take the subtle border role')
assert.strictEqual(r.pointLabels.color, '#111111', 'spoke names take the muted text role')
// Four rings, a quarter of the range apart, and no numbers on them — the
// wizard's radar: the exact values are in the data table beside the canvas.
assert.strictEqual(r.ticks.display, false, 'the rings carry no numbers')
assert.strictEqual(r.ticks.stepSize, 0.25, 'four rings over 0..1: 0.25, 0.5, 0.75 and 1')
assert.ok(!('x' in radar.options.scales), 'a radar has no cartesian axes')

// Without a range the scale is left to the data, from zero.
const unranged = buildConfig(
    { chartType: 'radar', data: { categories: ['a', 'b', 'c'], series: [{ label: 's', values: [1, 2, 3] }] } },
    paint
)
assert.strictEqual(unranged.options.scales.r.min, undefined)
assert.strictEqual(unranged.options.scales.r.beginAtZero, true)
assert.strictEqual(unranged.options.scales.r.ticks.stepSize, undefined, 'without a range Chart.js picks the rings')

// No chart animates: one is rebuilt whenever its page renders again, and a
// chart growing into place each time is a chart that moves while it is read.
assert.strictEqual(unranged.options.animation, false, 'a radar does not animate')
const stillBar = buildConfig({ chartType: 'bar', data: { series: [] } }, paint)

// A line's points have the same reach as a radar's; a bar or a slice is its own
// target and takes none.
const pointed = buildConfig({ chartType: 'line', data: { series: [{ label: 's', values: [1, 2] }] } }, paint)
assert.strictEqual(pointed.data.datasets[0].pointHitRadius, 12, 'a line point catches the pointer 12px out')
const barred = buildConfig({ chartType: 'bar', data: { series: [{ label: 's', values: [1, 2] }] } }, paint)
assert.strictEqual(barred.data.datasets[0].pointHitRadius, undefined, 'a bar has no points to reach')
assert.strictEqual(stillBar.options.animation, false, 'a bar does not animate')

// The same range fixes a bar chart's value axis; an unknown type is a bar.
const bar = buildConfig({ chartType: 'nonsense', data: { series: [], range: { min: 0, max: 1 } } }, paint)
assert.strictEqual(bar.type, 'bar')
assert.strictEqual(bar.options.scales.y.max, 1)

assert.strictEqual(translucent('#abc', 0.5), 'rgba(170, 187, 204, 0.5)')
assert.strictEqual(translucent(' #F15A24 ', 0.2), 'rgba(241, 90, 36, 0.2)')
assert.strictEqual(translucent('rgb(1 2 3)', 0.2), 'rgba(1, 2, 3, 0.2)')
assert.strictEqual(translucent('rgb(1, 2, 3)', 0.2), 'rgba(1, 2, 3, 0.2)')

// ---------------------------------------------------------------------------
// The element follows the theme while it is open: data-theme changing on the
// document element, the system preference changing, and a brand injected by
// theme.js each repaint it from freshly read roles — for every chart type — and
// disconnecting lets go of all three. A minimal stand-in for the browser, since
// a canvas is the one thing Node cannot provide.

{
    const roles = {}
    const resetRoles = () => {
        Object.assign(roles, {
            '--ui-brand': '#2563eb',
            '--ui-brand-strong': '#2461e9',
            '--ui-text-muted': '#5b5e61',
            '--ui-border-subtle': '#e0e3e7',
            '--ui-surface': '#ffffff',
            '--ui-chart-1': '#0074a3'
        })
    }

    const observers = []
    const mediaListeners = new Set()
    const documentListeners = new Map()

    global.HTMLElement = class {
        constructor() {
            this._attributes = {}
            this.children = []
        }
        getAttribute(name) {
            return name in this._attributes ? this._attributes[name] : null
        }
        setAttribute(name, value) {
            this._attributes[name] = String(value)
        }
        appendChild(child) {
            this.children.push(child)
        }
    }
    global.MutationObserver = class {
        constructor(callback) {
            this.callback = callback
            this.connected = false
            observers.push(this)
        }
        observe(target, options) {
            assert.strictEqual(target, global.document.documentElement, 'the observer watches <html>')
            assert.deepStrictEqual(options.attributeFilter, ['data-theme'])
            this.connected = true
        }
        disconnect() {
            this.connected = false
        }
    }
    global.document = {
        documentElement: {},
        createElement: () => ({ setAttribute() {}, getContext: () => ({}) }),
        addEventListener: (type, listener) => documentListeners.set(listener, type),
        removeEventListener: (type, listener) => documentListeners.delete(listener)
    }
    global.window = {
        matchMedia: (query) => ({
            matches: false,
            addEventListener: (type, listener) => {
                if (query === '(prefers-color-scheme: dark)' && type === 'change') mediaListeners.add(listener)
            },
            removeEventListener: (type, listener) => mediaListeners.delete(listener)
        })
    }
    global.getComputedStyle = () => ({ getPropertyValue: (name) => roles[name] || '' })

    const charts = []
    class FakeChart {
        constructor(context, config) {
            this.data = config.data
            this.options = config.options
            this.updates = 0
            this.destroyed = false
            charts.push(this)
        }
        update() {
            this.updates += 1
        }
        destroy() {
            this.destroyed = true
        }
    }

    let ElementClass = null
    global.customElements = {
        define: (name, cls) => {
            assert.strictEqual(name, 'ui-chart')
            ElementClass = cls
        }
    }

    define(FakeChart)

    const lineOf = (chart) => chart.data.datasets[0].borderColor
    const labelsOf = (chart) => chart.options.plugins.legend.labels.color

    for (const type of ['radar', 'bar', 'line', 'doughnut']) {
        charts.length = 0
        observers.length = 0
        mediaListeners.clear()
        documentListeners.clear()
        resetRoles()

        const element = new ElementClass()
        element.setAttribute('chart-type', type)
        element.connectedCallback()
        element.chartData = { categories: ['a', 'b', 'c'], series: [{ label: 's', values: [1, 2, 3] }] }

        const chart = charts[charts.length - 1]
        assert.ok(chart, `${type}: a chart is drawn`)

        // The application switches to dark: data-theme changes on <html>.
        roles['--ui-text-muted'] = '#b3b8be'
        roles['--ui-brand-strong'] = '#5d92ff'
        roles['--ui-chart-1'] = '#53c3ff'
        assert.strictEqual(observers.length, 1)
        assert.ok(observers[0].connected, `${type}: watching data-theme`)
        observers[0].callback([])
        assert.strictEqual(chart.updates, 1, `${type}: a data-theme change repaints`)
        assert.strictEqual(labelsOf(chart), '#b3b8be', `${type}: labels re-read on a data-theme change`)
        if (type === 'radar') assert.strictEqual(lineOf(chart), '#5d92ff', 'radar: the line re-read in the dark brand')
        if (type === 'line') assert.strictEqual(lineOf(chart), '#53c3ff', 'line: the series re-read in the dark palette')

        // The system switching while the page follows it.
        roles['--ui-text-muted'] = '#5b5e61'
        assert.strictEqual(mediaListeners.size, 1, `${type}: listening to prefers-color-scheme`)
        for (const listener of mediaListeners) listener({ matches: false })
        assert.strictEqual(chart.updates, 2, `${type}: the system switching repaints`)
        assert.strictEqual(labelsOf(chart), '#5b5e61', `${type}: labels re-read when the system switches`)

        // A brand injected by theme.js.
        roles['--ui-brand-strong'] = '#c33d00'
        roles['--ui-brand'] = '#f15a24'
        const themeListeners = [...documentListeners].filter(([, eventType]) => eventType === THEME_EVENT)
        assert.strictEqual(themeListeners.length, 1, `${type}: listening for ${THEME_EVENT}`)
        themeListeners[0][0]()
        assert.strictEqual(chart.updates, 3, `${type}: a new brand repaints`)
        if (type === 'radar') {
            assert.strictEqual(lineOf(chart), '#c33d00', 'radar: the line follows a re-seeded brand')
            assert.strictEqual(chart.data.datasets[0].backgroundColor, 'rgba(241, 90, 36, 0.2)')
        }

        // Disconnecting lets go of all three, and of the chart.
        element.disconnectedCallback()
        assert.ok(!observers[0].connected, `${type}: the observer is disconnected`)
        assert.strictEqual(mediaListeners.size, 0, `${type}: the media listener is removed`)
        assert.strictEqual(documentListeners.size, 0, `${type}: the theme event listener is removed`)
        assert.ok(chart.destroyed, `${type}: the chart is destroyed`)
    }

    // theme.js is where the event comes from; the two names must agree.
    const themeSource = fs.readFileSync(path.join(__dirname, '../js/theme.js'), 'utf8')
    assert.ok(themeSource.includes(`'${THEME_EVENT}'`), `theme.js must dispatch ${THEME_EVENT} after injecting a brand`)
}


console.log('ui chart contract OK')
