// The arrow keys of a mode switch (`Ui.ModeSwitch`).
//
// The switch is a radio group, and a radio group is one Tab stop whose arrow
// keys move to the next option and choose it. The view does the first part —
// only the chosen option is in the tab order — but moving focus is not
// something a view can do, so the keys are handled here: the option they land
// on is focused and clicked, and the click is the choice the page hears.
//
// Only the switches drawn from Elm (`data-ui-mode-switch`): the markdown
// editor draws its own inside its toolbar, which has its keys already.

const KEYS = ['ArrowRight', 'ArrowDown', 'ArrowLeft', 'ArrowUp', 'Home', 'End']

// Where a key moves from `current` among `count` options, around the ends.
function targetIndex(count, current, key) {
    switch (key) {
        case 'ArrowRight':
        case 'ArrowDown':
            return (current + 1) % count
        case 'ArrowLeft':
        case 'ArrowUp':
            return (current - 1 + count) % count
        case 'Home':
            return 0
        case 'End':
            return count - 1
        default:
            return current
    }
}

function onKeydown(event) {
    if (event.defaultPrevented || !KEYS.includes(event.key)) return
    if (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey) return

    const option = event.target.closest && event.target.closest('.ui-mode-switch__option')
    const group = option && option.closest('[data-ui-mode-switch]')
    if (!group) return

    const options = Array.from(group.querySelectorAll('.ui-mode-switch__option')).filter((item) => !item.disabled)
    const current = options.indexOf(option)
    if (current === -1) return

    event.preventDefault()
    const target = options[targetIndex(options.length, current, event.key)]
    if (target === option) return

    target.focus()
    target.click()
}

if (typeof document !== 'undefined') {
    document.addEventListener('keydown', onKeydown)
}

module.exports = { targetIndex, onKeydown }
