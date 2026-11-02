// A color field's clear button gives focus back to the hex.
//
// Pressing it empties the field, and an empty field has no button, so focus
// would go with it — to the body, which is nowhere. The date picker does the
// same for its own clear button. Elm cannot move focus from a view, and a port
// for one press would be a lot of machinery, so one listener does it, before
// Elm re-renders and takes the button away.

document.addEventListener('click', (event) => {
    const button = event.target instanceof Element ? event.target.closest('.ui-color__clear') : null
    if (!button) return

    button.closest('.ui-color__hex')?.querySelector('input')?.focus()
})
