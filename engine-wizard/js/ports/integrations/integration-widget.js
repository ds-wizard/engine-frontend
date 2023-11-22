const createIntegration = require('./create-integration')

module.exports = function (app) {
    function eventHandler(event, unbind) {
        if (event.data.type === 'selection') {
            event.data.id = "" + event.data.id
            event.data.name = "" + event.data.name

            app.ports.gotIntegrationWidgetData.send(event.data)
            unbind()
        }
    }

    createIntegration({
        openPort: app.ports.openIntegrationWidgetPort,

        windowDefaultWidth: 1200,
        windowDefaultHeight: 800,

        eventHandler: eventHandler,
    })
}
