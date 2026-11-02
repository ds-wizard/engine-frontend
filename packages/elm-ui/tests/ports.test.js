// Exercises packages/elm-ui/js/theme.js against a minimal DOM.
//
// This covers the seam that broke the mode switch: Elm emits CSS with
// selectors, so it must be injected as a <style> element rather than set as a
// style attribute, and data-theme must land on the document element because
// every role in _roles.scss is scoped to :root. Neither half is visible to
// elm-test, and neither is visible to elm-review.

function element(tag) {
    return {
        tag,
        id: '',
        textContent: '',
        attrs: {},
        setAttribute(key, value) {
            this.attrs[key] = value
        },
        removeAttribute(key) {
            delete this.attrs[key]
        }
    }
}

const head = { children: [], appendChild(node) { this.children.push(node) } }
const documentElement = element('html')
// A real body exists, so putting data-theme here fails the assertion rather
// than throwing — that was the original bug and it should read as a failure.
const body = element('body')
const stored = {}
// What theme.js announces, with the stylesheet as it stood at that moment: a
// chart repainting on the event has to find the new roles already in place.
const dispatched = []

global.CustomEvent = class {
    constructor(type) {
        this.type = type
    }
}

global.document = {
    head,
    body,
    documentElement,
    getElementById: (id) => head.children.find((child) => child.id === id) || null,
    createElement: element,
    dispatchEvent: (event) => {
        const style = head.children.find((child) => child.id === 'ui-theme')
        dispatched.push({ type: event.type, css: style ? style.textContent : null })
    }
}

global.localStorage = {
    setItem: (key, value) => { stored[key] = value },
    removeItem: (key) => { delete stored[key] },
    getItem: (key) => (key in stored ? stored[key] : null)
}

const ports = {}
const app = {
    ports: {
        uiSetThemePort: { subscribe: (fn) => { ports.theme = fn } },
        uiSetModePort: { subscribe: (fn) => { ports.mode = fn } },
        uiCopyToClipboard: { subscribe: (fn) => { ports.copy = fn } }
    }
}

require('../js/theme.js')(app)

const css = ':root,:root[data-theme="light"]{--ui-brand:#f15a24;}'
ports.theme(css)
ports.mode('dark')

const style = head.children.find((child) => child.id === 'ui-theme')

const checks = [
    ['theme is injected as a style element', Boolean(style)],
    ['style holds the emitted CSS verbatim', Boolean(style) && style.textContent === css],
    [
        'a new theme is announced as ui-theme-change, after the style holds it',
        dispatched.length === 1 && dispatched[0].type === 'ui-theme-change' && dispatched[0].css === css
    ],
    ['data-theme lands on the document element', documentElement.attrs['data-theme'] === 'dark'],
    ['data-theme is not put on body, where :root rules would miss it', body.attrs['data-theme'] === undefined],
    ['mode is persisted for the no-flash script', stored['ui-mode'] === 'dark']
]

ports.mode('system')
checks.push(['system removes the attribute', documentElement.attrs['data-theme'] === undefined])
checks.push(['system clears the stored choice', stored['ui-mode'] === undefined])

ports.theme(':root{--ui-brand:red;}')
checks.push([
    're-theming reuses one style element',
    head.children.filter((child) => child.id === 'ui-theme').length === 1
])

// The clipboard port, and the fallback in particular: the async API is missing on
// insecure origins and in a few older browsers, and a copy button that silently
// does nothing is the kind of failure nobody reports.
const written = []
const selected = []

// defineProperty rather than assignment: Node defines its own navigator as a
// getter, and assigning to it is quietly ignored — which would make the two
// checks below pass for the wrong reason.
function setNavigator(value) {
    Object.defineProperty(global, 'navigator', { value, configurable: true, writable: true })
}

setNavigator({})
document.createElement = (tag) => {
    const node = element(tag)
    node.style = { cssText: '' }
    node.select = function () { selected.push(this.value) }
    node.remove = function () {}
    return node
}
document.body.appendChild = () => {}
document.execCommand = (command) => written.push(command)

require('../js/copy.js')(app)

ports.copy('dsw:root:2.4.0')
checks.push(['without navigator.clipboard it falls back', written.includes('copy')])
checks.push(['the fallback copies the value it was given', selected.includes('dsw:root:2.4.0')])

setNavigator({
    clipboard: {
        writeText: (value) => {
            written.push('async:' + value)
            return Promise.resolve()
        }
    }
})

ports.copy('second value')
checks.push(['with navigator.clipboard it uses it', written.includes('async:second value')])
checks.push([
    'and does not also run the fallback',
    selected.filter((value) => value === 'second value').length === 0
])

const failures = checks.filter(([, ok]) => !ok)

if (failures.length > 0) {
    console.error('ui port contract:')
    failures.forEach(([name]) => console.error(`  FAIL  ${name}`))
    process.exit(1)
}

console.log('ui port contract OK')
