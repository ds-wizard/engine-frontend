const createIntegration = require('./create-integration')

module.exports = function (app) {
    function eventHandler(event, unbind) {
        if (event.data.type === 'import') {
            app.ports.gotImporterData.send(event.data.events)
            unbind()
        }
    }

    createIntegration({
        openPort: app.ports.openImporterPort,

        windowDefaultWidth: 660,
        windowDefaultHeight: 360,

        eventHandler: eventHandler,
    })
}
