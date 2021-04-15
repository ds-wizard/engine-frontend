'use strict'

var program = require('./elm/Registry.elm')

var registerCopyPorts = require('../engine-shared/ports/copy')

function getApiUrl() {
    if (window.registry && window.registry['apiUrl']) return window.registry['apiUrl']
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


function loadApp(provisioning) {
    var app = program.Elm.Registry.init({
        flags: {
            apiUrl: getApiUrl(),
            credentials: JSON.parse(localStorage.getItem('credentials')),
            provisioning: provisioning,
            localProvisioning: getLocalProvisioning(),
        }
    })

    app.ports.saveCredentials.subscribe(function (credentials) {
        localStorage.setItem('credentials', JSON.stringify(credentials))
    })

    registerCopyPorts(app)
}

function jsonp(src) {
    var script = document.createElement('script')
    script.src = src
    document.head.appendChild(script)
    return script
}


window.onload = function () {
    var provisioningUrl = getProvisioningUrl()
    if (provisioningUrl !== false) {
        var provisioningCallbackMethod = 'provisioningCallback'
        var provisioningScript = jsonp(provisioningUrl + '?callback=' + provisioningCallbackMethod)

        window[provisioningCallbackMethod] = function (provisioning) {
            delete window[provisioningCallbackMethod]
            document.head.removeChild(provisioningScript)
            loadApp(provisioning)
        }
    } else {
        loadApp(null)
    }
}
