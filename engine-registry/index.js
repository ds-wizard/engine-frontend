'use strict'


const axios = require('axios').default
const axiosRetry = require('axios-retry')

const program = require('./elm/Registry.elm')

const registerCopyPorts = require('../engine-shared/ports/copy')


axiosRetry(axios, {
    retries: 3,
    retryDelay: function (retryCount) {
        return retryCount * 1000
    }
})

function apiUrl() {
    if (window.registry && window.registry['apiUrl']) return window.registry['apiUrl']
    return 'http://localhost:3000'
}

function configUrl() {
    return apiUrl() + '/configs/bootstrap'
}

function localProvisioning() {
    if (window.wizard && window.wizard['provisioning']) return window.wizard['provisioning']
    return null
}

function bootstrapErrorHTML(errorCode) {
    const title = 'Bootstrap Error'
    const message = errorCode ? 'Server responded with an error code ' + errorCode + '.' : 'Configuration cannot be loaded due to server unavailable.'
    return '<div class="full-page-illustrated-message"><img src="/img/illustrations/undraw_bug_fixing.svg"><div><h1>' + title + '</h1><p>' + message + '<br>Please, contact the application provider.</p></div></div>'
}


function loadApp(config) {
    var app = program.Elm.Registry.init({
        flags: {
            apiUrl: apiUrl(),
            config: config,
            credentials: JSON.parse(localStorage.getItem('credentials')),
            localProvisioning: localProvisioning(),
        }
    })

    app.ports.saveCredentials.subscribe(function (credentials) {
        localStorage.setItem('credentials', JSON.stringify(credentials))
    })

    registerCopyPorts(app)
}


window.onload = function () {
    axios.get(configUrl())
        .then(function (config) {
            loadApp(config.data)
        })
        .catch(function (err) {
            const errorCode = err.response ? err.response.status : null
            document.body.innerHTML = bootstrapErrorHTML(errorCode)
        })


    // const promises = [axios.get(configUrl())]
    //
    // const hasProvisioning = !!provisioningUrl()
    // if (hasProvisioning) {
    //     promises.push(axios.get(provisioningUrl()))
    // }
    //
    //
    //
    // if (provisioningUrl !== false) {
    //     var provisioningCallbackMethod = 'provisioningCallback'
    //     var provisioningScript = jsonp(provisioningUrl + '?callback=' + provisioningCallbackMethod)
    //
    //     window[provisioningCallbackMethod] = function (provisioning) {
    //         delete window[provisioningCallbackMethod]
    //         document.head.removeChild(provisioningScript)
    //         loadApp(provisioning)
    //     }
    // } else {
    //     loadApp(null)
    // }
}
