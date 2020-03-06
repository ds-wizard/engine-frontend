'use strict'

var program = require('./elm/WizardResearch.elm')
var config = window.wizardResearch

function getConfigVar(key, fallback) {
    if (config && config[key]) return config[key]
    return fallback
}

function getApiUrl() {
    return getConfigVar('apiUrl', 'http://localhost:3000')
}

function getProvisioningUrl() {
    return getConfigVar('provisioningUrl', false)
}

function getLocalProvisioning() {
    getConfigVar('provisioning', null)
}


function loadApp(config, provisioning) {
    var app = program.Elm.WizardResearch.init({
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
