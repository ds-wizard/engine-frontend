// The date field's contract with flatpickr, checked without a browser.
//
// Two things are worth a guard here, and neither is visible to elm-test or to
// elm-review:
//
//   The value formats. They are what the server sees. A date is a calendar
//   date, a time is a wall-clock time, and a date *and* time is an instant with
//   a zone — change one of these by accident and the field keeps working while
//   what it stores stops meaning the same thing.
//
//   allowInput. flatpickr leaves its input readonly by default, which makes the
//   calendar the only way to fill the field — and the calendar's days are not
//   in the tab order, so a keyboard user cannot fill it at all. This one option
//   is the difference between the field conforming to 2.1.1 and not.

//   Where the calendar hangs. A modal dialog is in the top layer and makes the
//   rest of the page inert: a calendar left on the body is painted under the
//   dialog and cannot be clicked, which is a field that does not work at all.

const assert = require('assert')
const { MODES, calendarHost, calendarOffset, calendarPlacement, optionsFor, placeAgainOnceSettled, placeholderFor, wordsFrom } = require('../js/date-picker')

assert.deepStrictEqual(Object.keys(MODES).sort(), ['date', 'datetime', 'time'])

// A calendar date: no zone, because a birthday is not an instant.
assert.strictEqual(optionsFor('date').dateFormat, 'Y-m-d')
assert.strictEqual(optionsFor('date').enableTime, undefined)

// An instant, shown to the reader in their own zone.
assert.strictEqual(optionsFor('datetime').dateFormat, 'Z')
assert.strictEqual(optionsFor('datetime').altInput, true)
assert.strictEqual(optionsFor('datetime').altFormat, 'Y-m-d H:i')
assert.strictEqual(optionsFor('datetime').time_24hr, true)

// A wall-clock time, with no calendar attached to it.
assert.strictEqual(optionsFor('time').dateFormat, 'H:i')
assert.strictEqual(optionsFor('time').noCalendar, true)

for (const mode of Object.keys(MODES)) {
    assert.strictEqual(optionsFor(mode).allowInput, true, `${mode} must be typeable`)
    assert.strictEqual(optionsFor(mode).wrap, true, `${mode} must use the element's own input`)
    assert.ok(placeholderFor(mode), `${mode} must say what it expects`)
}

// An unknown mode falls back rather than producing a picker with no format at
// all, which would silently store whatever flatpickr defaults to.
assert.strictEqual(optionsFor('nonsense').dateFormat, optionsFor('date').dateFormat)

// The words are the page's when it gives them and English otherwise, and an
// unreadable attribute is the same as none — a field must never lose its
// clear button's name to a typo.
assert.strictEqual(wordsFrom(null, 'start date').clear, 'Clear start date')
assert.strictEqual(wordsFrom('{', 'start date').clear, 'Clear start date')
assert.strictEqual(wordsFrom('{"clear":"Vymazat datum začátku"}', 'start date').clear, 'Vymazat datum začátku')
assert.ok(wordsFrom(null, 'date').invalidValue.includes('{value}'), 'the invalid-value message needs a place for the value')

// A field inside a dialog gives its calendar to that dialog; one that is not
// leaves it on the body, where it has always been.
const body = { body: true }
const dialog = { dialog: true }
assert.strictEqual(calendarHost({ closest: () => dialog }, body), dialog)
assert.strictEqual(calendarHost({ closest: () => null }, body), body)

// In a dialog the calendar goes in the top layer, because the dialog is a
// scrolling box that would clip it and would scroll it away from its field.
// Without the popover API it is laid out in the dialog instead — clipped, as it
// was, but reachable. Outside a dialog nothing changes.
assert.strictEqual(calendarPlacement({ inDialog: true, canPopover: true }), 'popover')
assert.strictEqual(calendarPlacement({ inDialog: true, canPopover: false }), 'dialog')
assert.strictEqual(calendarPlacement({ inDialog: false, canPopover: true }), 'page')

// Inside a dialog the offsets are measured from the dialog's padding box, not
// from the page: a field 40px down a dialog that starts 100px down the window,
// with a 1px border, sits 2px under the field at 40 - 1 + 2.
const inDialog = calendarOffset({
    field: { top: 120, bottom: 140, left: 130 },
    host: { top: 100, left: 100 },
    hostBorder: { top: 1, left: 1 },
    calendarHeight: 300,
    viewportHeight: 800
})
assert.deepStrictEqual(inDialog, { top: 41, left: 29, flip: false })

// No room under the field: above it instead, which is what flatpickr does on
// the page.
const flipped = calendarOffset({
    field: { top: 600, bottom: 620, left: 130 },
    host: { top: 100, left: 100 },
    hostBorder: { top: 1, left: 1 },
    calendarHeight: 300,
    viewportHeight: 800
})
assert.strictEqual(flipped.flip, true)
assert.strictEqual(flipped.top, 600 - 300 - 2 - 100 - 1)

// Not above it when there is no room there either — a calendar off the top of
// the window is worse than one that runs off the bottom of it.
assert.strictEqual(calendarOffset({
    field: { top: 10, bottom: 30, left: 0 },
    host: { top: 0, left: 0 },
    hostBorder: { top: 0, left: 0 },
    calendarHeight: 300,
    viewportHeight: 200
}).flip, false)

// A calendar opened while its dialog is still opening is placed again once
// the dialog has settled, finished or canceled; one opened in a dialog that
// has, or on the page, is left where flatpickr put it.
function dialogAnimating(playState, finished) {
    finished.catch(() => {})
    return { getAnimations: () => [{ playState, finished }] }
}

;(async () => {
    const tick = () => new Promise((resolve) => setImmediate(resolve))
    const placed = []
    const place = (what) => () => placed.push(what)

    placeAgainOnceSettled(dialogAnimating('running', Promise.resolve()), place('finished'))
    placeAgainOnceSettled(dialogAnimating('running', Promise.reject(new Error('canceled'))), place('canceled'))
    placeAgainOnceSettled(dialogAnimating('finished', Promise.resolve()), place('settled already'))
    placeAgainOnceSettled({}, place('cannot tell'))
    placeAgainOnceSettled(null, place('on the page'))
    await tick()
    assert.deepStrictEqual(placed.sort(), ['canceled', 'finished'])

    console.log('ui date picker contract OK')
})().catch((error) => {
    console.error(error)
    process.exit(1)
})
