'use strict'

var axios = require('axios')
var axiosRetry = require('axios-retry')

var program = require('./elm/Wizard.elm')

var cookies = require('./ports/cookies')
var registerChartPorts = require('./ports/chart')
var registerImportPorts = require('./ports/import')
var registerPageUnloadPorts = require('./ports/page-unload')
var registerRefreshPorts = require('./ports/refresh')
var registerScrollPorts = require('./ports/scroll')
var registerSessionPorts = require('./ports/session')
var registerCopyPorts = require('../engine-shared/ports/copy')
var registerWebsocketPorts = require('../engine-shared/ports/WebSocket')
var registerIntegrationWidgetPorts = require('./ports/integrationWidget')


axiosRetry(axios, {
    retries: 3,
    retryDelay: function (retryCount) {
        return retryCount * 1000
    }
})

function getPdfSupport() {
    function hasAcrobatInstalled() {
        function getActiveXObject(name) {
            try {
                return new ActiveXObject(name)
            } catch (e) {
            }
        }

        return getActiveXObject('AcroPDF.PDF') || getActiveXObject('PDF.PdfCtrl')
    }

    function isIos() {
        return /iPad|iPhone|iPod/.test(navigator.userAgent) && !window.MSStream
    }

    return !!(navigator.mimeTypes['application/pdf'] || hasAcrobatInstalled() || isIos())
}


function apiUrl() {
    if (window.wizard && window.wizard['apiUrl']) return window.wizard['apiUrl']
    return 'http://localhost:3000'
}

function configUrl() {
    return apiUrl() + '/configs/bootstrap'
}

function provisioningUrl() {
    if (window.wizard && window.wizard['provisioningUrl']) return window.wizard['provisioningUrl']
    return false
}

function localProvisioning() {
    if (window.wizard && window.wizard['provisioning']) return window.wizard['provisioning']
    return null
}

function bootstrapErrorHTML() {
    return '<div class="full-page-illustrated-message"><img src="/img/illustrations/undraw_bug_fixing.svg"><div><h1>Bootstrap Error</h1><p>Application cannot load configuration.<br>Please, contact the administrator.</p></div></div>'
}

function clientUrl() {
    return window.location.protocol + '//' + window.location.host
}

function updateStyles(config) {
    if (config.customization && config.customization.styleUrl) {
        var style = document.querySelector('[rel="stylesheet"]')
        style.setAttribute('href', config.customization.styleUrl)
    }
}

function loadApp(config, provisioning) {
    updateStyles(config)

    var app = program.Elm.Wizard.init({
        node: document.body,
        flags: {
            seed: Math.floor(Math.random() * 0xFFFFFFFF),
            session: JSON.parse(localStorage.session || null),
            apiUrl: apiUrl(),
            clientUrl: clientUrl(),
            config: config,
            provisioning: provisioning,
            localProvisioning: localProvisioning(),
            navigator: {
                pdf: getPdfSupport()
            },
            gaEnabled: cookies.getGaEnabled(),
            cookieConsent: cookies.getCookieConsent()
        }
    })

    registerChartPorts(app)
    registerCopyPorts(app)
    registerImportPorts(app)
    registerPageUnloadPorts(app)
    registerRefreshPorts(app)
    registerScrollPorts(app)
    registerSessionPorts(app)
    registerWebsocketPorts(app)
    registerIntegrationWidgetPorts(app)
    cookies.registerCookiePorts(app)

    cookies.init()
}

window.onload = function () {
    var promises = [axios.get(configUrl())]
    var hasProvisioning = !!provisioningUrl()
    if (hasProvisioning) {
        promises.push(axios.get(provisioningUrl()))
    }

    axios.all(promises)
        .then(function (results) {
            var config = results[0].data
            var provisioning = hasProvisioning ? results[1].data : null
            loadApp(config, provisioning)
        })
        .catch(function (err) {
            document.body.innerHTML = bootstrapErrorHTML()
        })
}
