'use strict'

var axios = require('axios')
var axiosRetry = require('axios-retry')

var program = require('./elm/Wizard.elm')

var cookies = require('./ports/cookies')
var registerChartPorts = require('./ports/chart')
var registerConsolePorts = require('./ports/console')
var registerImportPorts = require('./ports/import')
var registerPageUnloadPorts = require('./ports/page-unload')
var registerRefreshPorts = require('./ports/refresh')
var registerScrollPorts = require('./ports/scroll')
var registerSessionPorts = require('./ports/session')
var registerCopyPorts = require('../engine-shared/ports/copy')
var registerWebsocketPorts = require('../engine-shared/ports/WebSocket')
var registerIntegrationWidgetPorts = require('./ports/integrationWidget')

var defaultStyleUrl

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
    var clientUrl = (window.wizard && window.wizard['clientUrl']) || window.location.origin
    return apiUrl() + '/configs/bootstrap?clientUrl=' + encodeURIComponent(clientUrl)
}

function provisioningUrl() {
    if (window.wizard && window.wizard['provisioningUrl']) return window.wizard['provisioningUrl']
    return false
}

function localProvisioning() {
    if (window.wizard && window.wizard['provisioning']) return window.wizard['provisioning']
    return null
}

function bootstrapErrorHTML(errorCode) {
    const message = errorCode ? 'Server responded with an error code ' + errorCode + '.' : 'Configuration cannot be loaded due to server unavailable.'
    return '<div class="full-page-illustrated-message"><img src="/img/illustrations/undraw_bug_fixing.svg"><div><h1>Bootstrap Error</h1><p>' + message + '<br>Please, contact the administrator.</p></div></div>'
}

function clientUrl() {
    return window.location.protocol + '//' + window.location.host
}

function setStyles(config, cb) {
    var customizationEnabled = config.feature && config.feature.clientCustomizationEnabled
    var styleUrl = customizationEnabled && config.lookAndFeel && config.lookAndFeel.styleUrl ? config.lookAndFeel.styleUrl : defaultStyleUrl
    var link = document.createElement('link')
    link.setAttribute("rel", "stylesheet")
    link.setAttribute("type", "text/css")
    link.onload = cb
    link.setAttribute("href", styleUrl)
    document.getElementsByTagName("head")[0].appendChild(link)
}

function getApiUrl(config) {
    if (config.cloud && config.cloud.enabled && config.cloud.serverUrl) {
        return config.cloud.serverUrl
    }
    return apiUrl()
}

function loadApp(config, provisioning) {
    setStyles(config, function () {

        var app = program.Elm.Wizard.init({
            node: document.body,
            flags: {
                seed: Math.floor(Math.random() * 0xFFFFFFFF),
                session: JSON.parse(localStorage.session || null),
                apiUrl: getApiUrl(config),
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
        registerConsolePorts(app)
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
    })
}

window.onload = function () {
    var style = document.querySelector('[rel="stylesheet"]')
    defaultStyleUrl = style.getAttribute('href')
    style.remove()

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
            var errorCode = err.response ? err.response.status : null

            setStyles({}, function () {
                document.body.innerHTML = bootstrapErrorHTML(errorCode)
            })
        })
}
