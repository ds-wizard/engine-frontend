// An open menu fits the window.
//
// A menu opens below its trigger (`Ui.Menu`, `.ui-menu__list`), and a long one —
// a filter with many options — would run past the bottom of the window, out of
// reach of anything but scrolling the page. Where it would, it is made as high
// as the room below the trigger and its items scroll inside it.
//
// The room depends on where the trigger is and on the window, which CSS cannot
// know, so the list's height is set when it appears, when what is in it changes
// (a search filter's results) and when the window changes size.
//
// A list laid over the page (`.ui-filter-menu--header`, a column's filter in a
// table's header) is fixed to the window, so the table's scrolling region does
// not clip it, and is placed here: under its trigger and end-aligned to it, or
// start-aligned where that would run past the window's start edge. A browser's
// own placement of a fixed box with no inset ignores how far the table is
// scrolled sideways, so it is placed again whenever anything scrolls.
//
// A field's popup in a dialog (a combobox's search and list, a text field's
// suggestions) is fixed to the window too. The dialog's body is the part that
// scrolls, and a popup inside it was cut off at the dialog's edge; fixed, it
// reaches past the dialog over the backdrop. It is as wide as its field and
// opens below it, or above it where there is more room there, and its list
// scrolls where neither has room for all of it. A field scrolled out of the
// body's view takes its popup with it rather than leaving it hanging over the
// header or the footer.

const LIST = 'ui-menu__list'
const OVER = 'ui-filter-menu--header'
const POPUP = '.ui-dialog :is(.ui-combobox__popup, .ui-suggest__list)'
const MARGIN = 16
const MIN_HEIGHT = 160
const MIN_LIST_HEIGHT = 96
const GAP = 4

// The height a list may take: all of it when it fits below its top, otherwise
// what is left down to the margin — but never less than a few items, so a menu
// opened at the very bottom of the window is still a menu.
function maxHeight(top, height, windowHeight) {
    const room = Math.max(windowHeight - top - MARGIN, MIN_HEIGHT)
    return height > room ? room : null
}

// Where a list laid over the page starts, from the window's left edge: its end
// edge at the trigger's end edge, unless that takes it past the window's start
// edge, when its start edge is at the trigger's instead; never off the window.
function overLeft(trigger, width, windowWidth, rtl) {
    const end = rtl ? trigger.left : trigger.right - width
    const start = rtl ? trigger.right - width : trigger.left
    const fits = (left) => left >= MARGIN && left + width <= windowWidth - MARGIN
    const left = fits(end) ? end : start
    return Math.max(MARGIN, Math.min(left, windowWidth - width - MARGIN))
}

// Whether a popup opens above its field: only when it does not fit below and
// there is more room above.
function opensAbove(height, below, above) {
    return height > below && above > below
}

// The field a popup in a dialog belongs to, the scrolling part it is in, how
// far it is from the field (the gap its own styles leave) and what in it
// scrolls when it is too high.
function popupParts(popup) {
    const combobox = popup.closest('.ui-combobox')
    const field = combobox
        ? combobox.querySelector('.ui-combobox__trigger-row')
        : popup.parentElement && popup.parentElement.querySelector('input')
    return {
        field,
        body: popup.closest('.ui-dialog__body') || popup.closest('.ui-dialog'),
        gap: combobox ? 4 : 2,
        list: combobox ? popup.querySelector('.ui-combobox__listbox') : popup,
    }
}

function placeInDialog(popup) {
    if (popup.hidden) return
    const {field, body, gap, list} = popupParts(popup)
    if (!field || !body) return

    const rect = field.getBoundingClientRect()
    const view = body.getBoundingClientRect()
    popup.style.visibility = rect.bottom < view.top || rect.top > view.bottom ? 'hidden' : ''

    if (list) list.style.maxBlockSize = ''
    popup.style.inlineSize = `${rect.width}px`
    popup.style.left = `${rect.left}px`
    popup.style.top = `${rect.bottom + gap}px`

    const below = window.innerHeight - rect.bottom - gap - MARGIN
    const above = rect.top - gap - MARGIN
    const height = popup.offsetHeight
    const up = opensAbove(height, below, above)
    const room = up ? above : below

    if (list && height > room) {
        list.style.maxBlockSize = `${Math.max(list.offsetHeight - (height - room), MIN_LIST_HEIGHT)}px`
    }
    if (up) popup.style.top = `${rect.top - gap - popup.offsetHeight}px`

    // While the dialog is still animating in, its transform makes it what the
    // popup is fixed to rather than the window: the popup is moved by however
    // far it landed from where it was put, and placed again once the dialog
    // has settled.
    const landed = popup.getBoundingClientRect()
    const top = parseFloat(popup.style.top)
    if (Math.abs(landed.left - rect.left) > 0.5 || Math.abs(landed.top - top) > 0.5) {
        popup.style.left = `${2 * rect.left - landed.left}px`
        popup.style.top = `${2 * top - landed.top}px`
    }
}

function place(list) {
    const host = list.closest(`.${OVER}`)
    const trigger = host && host.querySelector('button')
    if (!trigger) return
    const rect = trigger.getBoundingClientRect()
    const rtl = getComputedStyle(list).direction === 'rtl'
    list.style.left = `${overLeft(rect, list.offsetWidth, window.innerWidth, rtl)}px`
    list.style.top = `${rect.bottom + GAP}px`
}

function fit(list) {
    list.style.maxBlockSize = ''
    const rect = list.getBoundingClientRect()
    const height = maxHeight(rect.top, rect.height, window.innerHeight)
    list.style.maxBlockSize = height === null ? '' : `${height}px`
    list.style.overflowY = height === null ? '' : 'auto'
}

function placeAll() {
    document.querySelectorAll(`.${OVER} .${LIST}`).forEach(place)
    document.querySelectorAll(POPUP).forEach(placeInDialog)
}

function fitAll() {
    placeAll()
    document.querySelectorAll(`.${LIST}`).forEach(fit)
}

// A menu, or a field's popup in a dialog: what a popup's height changes with is
// what is in it, and a text field's suggestions come and go by `hidden`.
const WATCHED = `.${LIST}, ${POPUP}`

function touchesMenu(mutation) {
    const target = mutation.target
    if (target.nodeType === Node.ELEMENT_NODE && target.closest(WATCHED)) return true
    return [...mutation.addedNodes].some((node) =>
        node.nodeType === Node.ELEMENT_NODE && (node.matches(WATCHED) || node.querySelector(WATCHED)))
}

let scheduled = false

function schedule() {
    if (scheduled) return
    scheduled = true
    requestAnimationFrame(() => {
        scheduled = false
        fitAll()
    })
}

if (typeof document !== 'undefined') {
    new MutationObserver((mutations) => {
        if (mutations.some(touchesMenu)) {
            // Before the list is painted where the browser put it.
            placeAll()
            schedule()
        }
    }).observe(document.documentElement, {childList: true, subtree: true, attributes: true, attributeFilter: ['hidden']})

    window.addEventListener('resize', schedule)
    document.addEventListener('scroll', placeAll, {capture: true, passive: true})

    // A dialog that has finished opening is no longer what its popups are
    // fixed to.
    document.addEventListener('transitionend', (event) => {
        if (event.target.classList && event.target.classList.contains('ui-dialog')) placeAll()
    })
}

module.exports = {maxHeight, overLeft, opensAbove, MARGIN, MIN_HEIGHT}
