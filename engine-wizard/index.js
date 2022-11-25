'use strict'

const axios = require('axios').default
const axiosRetry = require('axios-retry')

const program = require('./elm/Wizard.elm')

const datetimePickers = require('./js/components/datetime-pickers')
const charts = require('./js/components/charts')

const cookies = require('./js/ports/cookies')
const registerConsolePorts = require('./js/ports/console')
const registerCopyPorts = require('../engine-shared/ports/copy')
const registerDownloadPorts = require('./js/ports/download')
const registerImportPorts = require('./js/ports/import')
const registerImporterPorts = require('./js/ports/importer')
const registerIntegrationWidgetPorts = require('./js/ports/integrationWidget')
const registerLocalStoragePorts = require('./js/ports/localStorage')
const registerPageUnloadPorts = require('./js/ports/page-unload')
const registerRefreshPorts = require('./js/ports/refresh')
const registerScrollPorts = require('./js/ports/scroll')
const registerSessionPorts = require('./js/ports/session')
const registerWebsocketPorts = require('../engine-shared/ports/WebSocket')

let defaultStyleUrl

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
    const clientUrl = (window.wizard && window.wizard['clientUrl']) || window.location.origin
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
    const title = errorCode ? (errorCode === 423 ? 'Plan expired' : 'Bootstrap Error') : 'Bootstrap Error'
    const message = errorCode ? (errorCode === 423 ? 'The application does not have any active plan.' : 'Server responded with an error code ' + errorCode + '.') : 'Configuration cannot be loaded due to server unavailable.'
    return '<div class="full-page-illustrated-message"><img src="/img/illustrations/undraw_bug_fixing.svg"><div><h1>' + title + '</h1><p>' + message + '<br>Please, contact the application provider.</p></div></div>'
}

function clientUrl() {
    return window.location.protocol + '//' + window.location.host
}

function setStyles(config, cb) {
    const customizationEnabled = config.feature && config.feature.clientCustomizationEnabled
    const styleUrl = customizationEnabled && config.lookAndFeel && config.lookAndFeel.styleUrl ? config.lookAndFeel.styleUrl : defaultStyleUrl
    const link = document.createElement('link')
    link.setAttribute('rel', 'stylesheet')
    link.setAttribute('type', 'text/css')
    link.onload = cb
    link.setAttribute('href', styleUrl)
    document.getElementsByTagName('head')[0].appendChild(link)

    if (!window.wizard) window.wizard = {}
    window.wizard.styleUrl = styleUrl[0] === '/' ? window.location.origin + styleUrl : styleUrl
}

function getApiUrl(config) {
    if (config.cloud && config.cloud.enabled && config.cloud.serverUrl) {
        return config.cloud.serverUrl
    }
    return apiUrl()
}

function loadApp(config, locale, provisioning) {
    setStyles(config, function () {
        const flags = {
            seed: Math.floor(Math.random() * 0xFFFFFFFF),
            session: JSON.parse(localStorage.session || null),
            selectedLocale: JSON.parse(localStorage.locale || null),
            apiUrl: getApiUrl(config),
            clientUrl: clientUrl(),
            config: config,
            provisioning: provisioning,
            localProvisioning: localProvisioning(),
            navigator: {
                pdf: getPdfSupport()
            },
            gaEnabled: cookies.getGaEnabled(),
            cookieConsent: cookies.getCookieConsent(),
        }

        if (Object.keys(locale).length > 0) {
            flags.locale = locale
        }

        const app = program.Elm.Wizard.init({
            node: document.body,
            flags: flags,
        })

        registerConsolePorts(app)
        registerCopyPorts(app)
        registerDownloadPorts(app)
        registerImportPorts(app)
        registerImporterPorts(app)
        registerIntegrationWidgetPorts(app)
        registerLocalStoragePorts(app)
        registerPageUnloadPorts(app)
        registerRefreshPorts(app)
        registerScrollPorts(app)
        registerSessionPorts(app)
        registerWebsocketPorts(app)
        cookies.registerCookiePorts(app)

        cookies.init()
    })
}

window.onload = function () {
    const style = document.querySelector('[rel="stylesheet"]')
    defaultStyleUrl = style.getAttribute('href')
    style.remove()

    const locale = localStorage.locale ? JSON.parse(localStorage.locale) : navigator.language
    const promises = [
        axios.get(configUrl()),
        axios.get(apiUrl() + '/configs/locales/' + locale).catch(() => {
            return {data: {}}
        })
    ]
    const hasProvisioning = !!provisioningUrl()
    if (hasProvisioning) {
        promises.push(axios.get(provisioningUrl()))
    }

    axios.all(promises)
        .then(function (results) {
            const config = results[0].data
            const locale = results[1].data
            const provisioning = hasProvisioning ? results[2].data : null
            loadApp(config, locale, provisioning)
        })
        .catch(function (err) {
            const errorCode = err.response ? err.response.status : null

            setStyles({}, function () {
                document.body.innerHTML = bootstrapErrorHTML(errorCode)
            })
        })
}
