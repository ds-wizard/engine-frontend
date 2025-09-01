const createIntegration = require('./create-integration')

module.exports = function (app) {
    function eventHandler(event, unbind) {
        if (event.data.type === 'actionResult') {
            app.ports.gotActionData.send(event.data)
            unbind()
        }
    }

    createIntegration({
        openPort: app.ports.openActionPort,

        windowDefaultWidth: 660,
        windowDefaultHeight: 360,

        eventHandler: eventHandler,
    })
}
