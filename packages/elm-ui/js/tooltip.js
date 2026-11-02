// Escape dismisses tooltips.
//
// WCAG 1.4.13 (Content on Hover or Focus) requires content revealed on hover or
// focus to be dismissible without moving the pointer or the focus. A CSS-only
// tooltip cannot do that, and giving every tooltip its own Elm state would be a
// lot of machinery for a label.
//
// One flag on the root instead: Escape suppresses tooltips, and the next
// pointer or keyboard move restores them.

const SUPPRESSED = 'data-tooltips-suppressed'

function suppress() {
    document.documentElement.setAttribute(SUPPRESSED, '')
    document.addEventListener('pointermove', restore, { once: true })
    document.addEventListener('keydown', restoreOnOtherKey)
}

function restore() {
    document.documentElement.removeAttribute(SUPPRESSED)
    document.removeEventListener('keydown', restoreOnOtherKey)
}

function restoreOnOtherKey(event) {
    if (event.key !== 'Escape') restore()
}

document.addEventListener('keydown', (event) => {
    if (event.key === 'Escape') suppress()
})
