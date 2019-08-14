'use strict'


// initialize elm app
var program = require('./elm/Main.elm')

var registerChartPorts = require('./ports/chart')
var registerImportPorts = require('./ports/import')
var registerPageUnloadPorts = require('./ports/page-unload')
var registerScrollPorts = require('./ports/scroll')
var registerSessionPorts = require('./ports/session')


function getApiUrl() {
    if (window.dsw && window.dsw['apiUrl']) return window.dsw['apiUrl']
    return 'http://localhost:3000'
}

function getProvisioningUrl() {
    if (window.dsw && window.dsw['provisioningUrl']) return window.dsw['provisioningUrl']
    return false
}


function loadApp(config, provisioning) {
    var app = program.Elm.Main.init({
        node: document.body,
        flags: {
            seed: Math.floor(Math.random() * 0xFFFFFFFF),
            session: JSON.parse(localStorage.session || null),
            apiUrl: getApiUrl(),
            config: config,
            provisioning: provisioning
        }
    })

    registerChartPorts(app)
    registerImportPorts(app)
    registerPageUnloadPorts(app)
    registerScrollPorts(app)
    registerSessionPorts(app)
}


function jsonp(src) {
    var script = document.createElement('script')
    script.src = src
    document.body.appendChild(script)
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
                document.body.removeChild(provisioningScript)
                loadApp(config, provisioning)
            }

        } else {
            loadApp(config, null)
        }

        delete window[configCallbackMethod]
        document.body.removeChild(configScript)
    }
}
