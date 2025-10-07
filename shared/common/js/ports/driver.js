const {driver} = require('driver.js')
const {waitForElement} = require('../utils.js')

module.exports = (app) => {
    app.ports.drive?.subscribe(drive)

    function drive(config) {
        function startDriver() {
            const driverObj = driver({
                disableActiveInteraction: true,
                showProgress: true,
                progressText: '{{current}}/{{total}}',
                nextBtnText: `${config.nextBtnText} →`,
                prevBtnText: `← ${config.prevBtnText}`,
                doneBtnText: config.doneBtnText,
                steps: config.steps,
                onDestroyStarted: () => {
                    if (!driverObj.hasNextStep() || window.confirm(config.skipTourText)) {
                        driverObj.destroy()
                    }
                },
                onDestroyed: () => {
                    app.ports.onTourDone?.send(config.tourId)
                },
                onPopoverRender: (popover, {config, state}) => {
                    popover.previousButton.classList.add('btn', 'btn-sm', 'btn-outline-secondary')
                    popover.nextButton.classList.add('btn', 'btn-sm', 'btn-primary')
                }
            })
            driverObj.drive()
        }

        setTimeout(() => {
            if (config.steps[0].element) {
                waitForElement(config.steps[0].element, true,() => {
                    startDriver()
                })
            } else {
                startDriver()
            }
        }, config.delay)
    }
}
