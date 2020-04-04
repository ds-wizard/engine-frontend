'use strict'

var axios = require('axios')
var axiosRetry = require('axios-retry')

var program = require('./elm/WizardResearch.elm')
var config = window.wizardResearch
var sessionKey = 'session'

axiosRetry(axios, {
    retries: 3,
    retryDelay: function (retryCount) {
        return retryCount * 1000
    }
})


function getConfigVar(key, fallback) {
    if (config && config[key]) return config[key]
    return fallback
}

function getApiUrl() {
    return getConfigVar('apiUrl', 'http://localhost:3000')
}

function getConfigUrl() {
    return getApiUrl() + '/configs/bootstrap'
}

function getProvisioningUrl() {
    return getConfigVar('provisioningUrl', false)
}

function getLocalProvisioning() {
    return getConfigVar('provisioning', null)
}

function getBootstrapErrorHTML() {
    return '<div><h1>Bootstrap Error</h1><p>Application cannot load configuration.<br>Please, contact the administrator.</p></div>'
}


function loadApp(config, provisioning) {
    var app = program.Elm.WizardResearch.init({
        node: document.body,
        flags: {
            seed: Math.floor(Math.random() * 0xFFFFFFFF),
            session: JSON.parse(localStorage.getItem(sessionKey) || null),
            apiUrl: getApiUrl(),
            config: config,
            provisioning: provisioning,
            localProvisioning: getLocalProvisioning(),
        }
    })

    app.ports.storeSession.subscribe(function(session) {
        localStorage.setItem(sessionKey, JSON.stringify(session))
    })

    app.ports.clearSession.subscribe(function() {
        localStorage.removeItem(sessionKey)
    })
}

window.onload = function () {
    var promises = [axios.get(getConfigUrl())]
    var provisioningUrl = getProvisioningUrl()
    if (provisioningUrl) {
        promises.push(axios.get(provisioningUrl))
    }

    axios.all(promises)
        .then(function (results) {
            var config = results[0].data
            var provisioning = provisioningUrl ? results[1].data : null
            loadApp(config, provisioning)
        })
        .catch(function (err) {
            document.body.innerHTML = getBootstrapErrorHTML()
        })
}
