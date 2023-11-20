'use strict'

const axios = require('axios').default
const axiosRetry = require('axios-retry')

const program = require('./elm/Wizard.elm')

const charts = require('./js/components/charts')
const codeEditor = require('./js/components/code-editor')
const datetimePickers = require('./js/components/datetime-pickers')

const cookies = require('./js/ports/cookies')
const registerConsolePorts = require('./js/ports/console')
const registerCopyPorts = require('../engine-shared/ports/copy')
const registerDomPorts = require('./js/ports/dom')
const registerDownloadPorts = require('./js/ports/download')
const registerImportPorts = require('./js/ports/import')
const registerImporterPorts = require('./js/ports/importer')
const registerIntegrationWidgetPorts = require('./js/ports/integrationWidget')
const registerLocalStoragePorts = require('./js/ports/localStorage')
const registerPageUnloadPorts = require('./js/ports/page-unload')
const registerRefreshPorts = require('./js/ports/refresh')
const registerSessionPorts = require('./js/ports/session')
const registerThemePorts = require('../engine-shared/ports/theme')
const registerWebsocketPorts = require('../engine-shared/ports/WebSocket')


const sessionKey = 'session/wizard'

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


function defaultApiUrl() {
    if (window.wizard && window.wizard['apiUrl']) return window.wizard['apiUrl']
    return 'http://localhost:3000'
}

function configUrl(apiUrl) {
    const clientUrl = (window.wizard && window.wizard['clientUrl']) || (window.location.origin + '/wizard')
    return (apiUrl || defaultApiUrl()) + '/configs/bootstrap?clientUrl=' + encodeURIComponent(clientUrl)
}

function localeUrl(apiUrl) {
    const locale = localStorage.locale ? JSON.parse(localStorage.locale) : navigator.language
    const clientUrl = (window.wizard && window.wizard['clientUrl']) || (window.location.origin + '/wizard')
    return (apiUrl || defaultApiUrl()) + '/configs/locales/' + locale + '?clientUrl=' + encodeURIComponent(clientUrl)
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
    return '<div class="full-page-illustrated-message"><img src="/wizard/img/illustrations/undraw_bug_fixing.svg"><div><h1>' + title + '</h1><p>' + message + '<br>Please, contact the application provider.</p></div></div>'
}

function clientUrl() {
    return window.location.protocol + '//' + window.location.host
}

function getApiUrl(config) {
    if (config.cloud && config.cloud.enabled && config.cloud.serverUrl) {
        return config.cloud.serverUrl
    }
    return defaultApiUrl()
}

function loadApp(config, locale, provisioning) {
    const flags = {
        seed: Math.floor(Math.random() * 0xFFFFFFFF),
        session: JSON.parse(localStorage.getItem(sessionKey)),
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
    registerDomPorts(app)
    registerDownloadPorts(app)
    registerImportPorts(app)
    registerImporterPorts(app)
    registerIntegrationWidgetPorts(app)
    registerLocalStoragePorts(app)
    registerPageUnloadPorts(app)
    registerRefreshPorts(app)
    registerSessionPorts(app, sessionKey, ['session/admin'])
    registerThemePorts(app)
    registerWebsocketPorts(app)
    cookies.registerCookiePorts(app)

    cookies.init()
}

window.onload = function () {
    const session = JSON.parse(localStorage.getItem(sessionKey))
    const token = session?.token?.token
    const headers = token ? { headers: {'Authorization': `Bearer ${token}`}} : {}
    const apiUrl = session?.apiUrl

    const promises = [
        axios.get(configUrl(apiUrl), headers),
        axios.get(localeUrl(apiUrl)).catch(() => {
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
            if (Math.floor(errorCode / 100) === 4 && session !== null) {
                localStorage.removeItem(sessionKey)
                window.location.reload()
            } else {
                document.body.innerHTML = bootstrapErrorHTML(errorCode)
            }
        })
}
