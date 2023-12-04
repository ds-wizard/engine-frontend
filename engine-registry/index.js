'use strict'


const axios = require('axios').default
const axiosRetry = require('axios-retry').default

const program = require('./elm/Registry.elm')

const registerCopyPorts = require('../engine-shared/ports/copy')


axiosRetry(axios, {
    retries: 3,
    retryDelay: function (retryCount) {
        return retryCount * 1000
    }
})

function apiUrl() {
    if (window.app && window.app['apiUrl']) return window.app['apiUrl']
    return 'http://localhost:3000'
}

function appTitle() {
    if (window.app && window.app['appTitle']) return window.app['appTitle']
    return null
}

function configUrl() {
    return apiUrl() + '/configs/bootstrap'
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
            appTitle: appTitle(),
            config: config,
            credentials: JSON.parse(localStorage.getItem('credentials')),
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
}
