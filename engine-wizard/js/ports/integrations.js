const registerActionPorts = require('./integrations/action')
const registerImporterPorts = require('./integrations/importer')
const registerIntegrationWidgetPorts = require('./integrations/integration-widget')

module.exports = function (app) {
    registerActionPorts(app)
    registerImporterPorts(app)
    registerIntegrationWidgetPorts(app)
}
