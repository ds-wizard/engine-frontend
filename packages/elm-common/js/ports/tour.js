// Guided tours on driver.js, for applications on the design system.
//
// What driver.js is told (Common.Components.Tour explains why each matters):
//
// - The popover is themed by packages/elm-common/scss/components/_tour.scss, and its
//   buttons wear Ui.Button's classes, so they look and focus like every other
//   button — no arrows in the labels for a screen reader to read out as
//   "right arrow".
// - "Step 2 of 5" is translated and is part of the dialog's description.
// - The close button is named in the reader's language.
// - Focus lands on Next (or Done) on every step, and returns to where it was
//   when the tour ends — or to the page heading, if that element was re-rendered
//   away in the meantime.
// - The overlay is the modal backdrop role, and there is no animation when the
//   reader prefers reduced motion.
// - A step whose element is not on the page is left out: what it points at is
//   not there for this reader (a navigation item they do not have, a choice the
//   page does not offer), and a popover pointing at nothing would describe it
//   anyway.
const {driver} = require('driver.js')
const {waitForElement} = require('../utils.js')

module.exports = function (app) {
    app.ports.commonDriveTour?.subscribe(drive)

    function drive(config) {
        if (!config.steps || config.steps.length === 0) return

        function startDriver() {
            const steps = config.steps.filter((step) => !step.element || isOnPage(step.element))
            if (steps.length === 0) return

            const reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches
            const returnFocusTo = document.activeElement

            const driverObj = driver({
                animate: !reduceMotion,
                smoothScroll: !reduceMotion,
                allowKeyboardControl: true,
                disableActiveInteraction: true,
                showProgress: true,
                progressText: config.progressText,
                nextBtnText: config.nextBtnText,
                prevBtnText: config.prevBtnText,
                doneBtnText: config.doneBtnText,
                popoverClass: 'c-tour',
                // Inline style on the overlay path, which accepts a custom
                // property: the backdrop role already carries its own alpha.
                overlayColor: 'var(--ui-surface-backdrop)',
                overlayOpacity: 1,
                steps,
                onPopoverRender: (popover) => {
                    popover.closeButton.setAttribute('aria-label', config.closeBtnText)

                    popover.previousButton.classList.add('ui-button', 'ui-button--secondary', 'ui-button--sm')
                    popover.nextButton.classList.add('ui-button', 'ui-button--primary', 'ui-button--sm')

                    // The progress is part of what the dialog says about
                    // itself, read after the step's text.
                    popover.progress.id = 'c-tour-progress'
                    const describedBy = [popover.description.id, popover.progress.id].filter(Boolean).join(' ')
                    popover.wrapper.setAttribute('aria-describedby', describedBy)

                    // driver.js focuses the first focusable element after this
                    // hook returns — the close button. Next is the one to press.
                    window.requestAnimationFrame(() => popover.nextButton.focus())
                },
                onDestroyStarted: () => {
                    if (!driverObj.hasNextStep() || window.confirm(config.skipTourText)) {
                        driverObj.destroy()
                    }
                },
                onDestroyed: () => {
                    // After driver.js's own attempt, which targets whatever was
                    // focused when the tour started and does nothing if that
                    // element has since left the document.
                    window.requestAnimationFrame(() => {
                        if (returnFocusTo && returnFocusTo !== document.body && returnFocusTo.isConnected) {
                            returnFocusTo.focus({preventScroll: true})
                        } else {
                            document.querySelector(config.fallbackFocus)?.focus({preventScroll: true})
                        }
                    })
                    app.ports.commonOnTourDone?.send(config.tourId)
                }
            })
            driverObj.drive()
        }

        function isOnPage(selector) {
            try {
                return document.querySelector(selector) !== null
            } catch {
                // A selector this browser cannot read: kept, as before.
                return true
            }
        }

        setTimeout(() => {
            if (config.steps[0].element) {
                waitForElement(config.steps[0].element, true, startDriver)
            } else {
                startDriver()
            }
        }, config.delay)
    }
}
