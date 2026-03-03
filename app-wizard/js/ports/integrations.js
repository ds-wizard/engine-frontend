const registerIntegrationWidgetPorts = require('./integrations/integration-widget')

module.exports = function (app) {
    registerIntegrationWidgetPorts(app)
}
