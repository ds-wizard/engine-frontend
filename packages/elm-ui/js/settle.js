// Whether an element is animating, and a way to wait until it is not.
//
// A dialog opens moved and scaled. Anything that measures a field inside it
// while it moves measures where the field is on the way, not where it ends up,
// and anything fixed inside it is placed against the dialog rather than the
// window. What is placed against a field in a dialog — a combobox's popup
// (menu-fit.js), a calendar (date-picker.js) — waits for this.

function isAnimating(element) {
    return typeof element.getAnimations === 'function'
        && element.getAnimations().some((animation) => animation.playState !== 'finished' && animation.playState !== 'idle')
}

// Resolves once every animation the element has now is over: finished, or
// canceled — `allSettled`, because a canceled animation rejects its `finished`.
// At once when there are none, or the browser cannot tell.
function settled(element) {
    if (typeof element.getAnimations !== 'function') return Promise.resolve()
    return Promise.allSettled(element.getAnimations().map((animation) => animation.finished)).then(() => undefined)
}

module.exports = { isAnimating, settled }
