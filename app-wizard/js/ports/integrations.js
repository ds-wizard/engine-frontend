const registerImporterPorts = require('./integrations/importer')
const registerIntegrationWidgetPorts = require('./integrations/integration-widget')

module.exports = function (app) {
    registerImporterPorts(app)
    registerIntegrationWidgetPorts(app)
}
