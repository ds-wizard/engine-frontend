'use strict'


// initialize elm app
var program = require('./elm/Wizard.elm')

var registerChartPorts = require('./ports/chart')
var registerCopyPorts = require('./ports/copy')
var registerImportPorts = require('./ports/import')
var registerPageUnloadPorts = require('./ports/page-unload')
var registerScrollPorts = require('./ports/scroll')
var registerSessionPorts = require('./ports/session')


function getApiUrl() {
    if (window.wizard && window.wizard['apiUrl']) return window.wizard['apiUrl']
    return 'http://localhost:3000'
}

function getProvisioningUrl() {
    if (window.wizard && window.wizard['provisioningUrl']) return window.wizard['provisioningUrl']
    return false
}

function getLocalProvisioning() {
    if (window.wizard && window.wizard['provisioning']) return window.wizard['provisioning']
    return null
}


function loadApp(config, provisioning) {
    var app = program.Elm.Wizard.init({
        node: document.body,
        flags: {
            seed: Math.floor(Math.random() * 0xFFFFFFFF),
            session: JSON.parse(localStorage.session || null),
            apiUrl: getApiUrl(),
            config: config,
            provisioning: provisioning,
            localProvisioning: getLocalProvisioning(),
        }
    })

    registerChartPorts(app)
    registerCopyPorts(app)
    registerImportPorts(app)
    registerPageUnloadPorts(app)
    registerScrollPorts(app)
    registerSessionPorts(app)
}


function jsonp(src) {
    var script = document.createElement('script')
    script.src = src
    document.head.appendChild(script)
    return script
}


window.onload = function () {
    var configCallbackMethod = 'configCallback'
    var configScript = jsonp(getApiUrl() + '/configuration?callback=' + configCallbackMethod)


    window[configCallbackMethod] = function (config) {
        var provisioningUrl = getProvisioningUrl()
        if (provisioningUrl !== false) {
            var provisioningCallbackMethod = 'provisioningCallback'
            var provisioningScript = jsonp(provisioningUrl + '?callback=' + provisioningCallbackMethod)

            window[provisioningCallbackMethod] = function (provisioning) {
                delete window[provisioningCallbackMethod]
                document.head.removeChild(provisioningScript)
                loadApp(config, provisioning)
            }

        } else {
            loadApp(config, null)
        }

        delete window[configCallbackMethod]
        document.head.removeChild(configScript)
    }
}
