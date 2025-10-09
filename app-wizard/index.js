'use strict'

const axios = require('axios').default
const axiosRetry = require('axios-retry').default

const appConfig = require('./js/app-config')
const {bootstrapErrorHTML, housekeepingHTML, notSeededHTML} = require('./js/bootstrap-error')

const program = require('./elm/Wizard.elm')

require('./js/components/charts')
require('./js/components/code-editor')
require('../shared/common/js/components/datetime-pickers')
require('./js/components/shortcut-element')

const cookies = require('./js/ports/cookies')
const registerConsolePorts = require('./js/ports/console')
const registerCopyPorts = require('../shared/common/js/ports/copy')
const registerDomPorts = require('../shared/common/js/ports/dom')
const registerDownloadPorts = require('../shared/common/js/ports/file')
const registerDriverPorts = require('../shared/common/js/ports/driver')
const registerImportPorts = require('./js/ports/import')
const registerIntegrationPorts = require('./js/ports/integrations')
const registerLocalePorts = require('../shared/common/js/ports/locale')
const registerLocalStoragePorts = require('../shared/common/js/ports/local-storage')
const registerSessionPorts = require('./js/ports/session')
const registerThemePorts = require('../shared/common/js/ports/theme')
const registerWebsocketPorts = require('../shared/common/js/ports/websocket')
const registerWindowPorts = require('../shared/common/js/ports/window')


const sessionKey = 'session/wizard'
const appSessionKey = 'session/app'

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

function isMac() {
    return /Macintosh|MacIntel|MacPPC|Mac68K/.test(navigator.userAgent)
}

function getApiUrl(config) {
    if (config.cloud && config.cloud.enabled && config.cloud.serverUrl) {
        return config.cloud.serverUrl
    }
    return appConfig.getDefaultApiUrl()
}

function getBootstrapConfigUrl(apiUrl) {
    if (!apiUrl) {
        const bootstrapConfigPath = '/configs/bootstrap?clientUrl=' + encodeURIComponent(appConfig.getClientUrl())
        return appConfig.getDefaultApiUrl() + bootstrapConfigPath
    }
    return apiUrl + '/configs/bootstrap'
}

function getLocaleUrl(apiUrl) {
    apiUrl = apiUrl || appConfig.getDefaultApiUrl()
    if (appConfig.isAdminEnabled()) {
        apiUrl = appConfig.getAdminApiUrl() || apiUrl.replace('/wizard-api', '/admin-api')
        return apiUrl + '/locales/current/content?module=wizard'
    }
    return apiUrl + '/locales/current/content'
}


function loadApp(config, locale) {
    const flags = {
        seed: Math.floor(Math.random() * 0xFFFFFFFF),
        session: JSON.parse(localStorage.getItem(sessionKey)),
        apiUrl: getApiUrl(config),
        clientUrl: appConfig.getClientUrl(),
        webSocketThrottleDelay: appConfig.getWebSocketThrottleDelay(),
        config: config,
        navigator: {
            pdf: getPdfSupport(),
            isMac: isMac(),
        },
        gaEnabled: cookies.getGaEnabled(),
        cookieConsent: cookies.getCookieConsent(),
        guideLinks: appConfig.getGuideLinks(),
        maxUploadFileSize: appConfig.getMaxUploadFileSize(),
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
    registerDriverPorts(app)
    registerImportPorts(app)
    registerIntegrationPorts(app)
    registerLocalePorts(app)
    registerLocalStoragePorts(app)
    registerSessionPorts(app, sessionKey, [appSessionKey])
    registerThemePorts(app)
    registerWebsocketPorts(app)
    registerWindowPorts(app)
    cookies.registerCookiePorts(app)

    cookies.init()
}

function createBootstrapConfigRequest() {
    const session = JSON.parse(localStorage.getItem(sessionKey))
    const token = session?.token?.token
    const requestConfig = token ? {headers: {'Authorization': `Bearer ${token}`}} : {}
    const apiUrl = session?.apiUrl

    return axios.get(getBootstrapConfigUrl(apiUrl), requestConfig)
}

function createLocaleRequest() {
    const session = JSON.parse(localStorage.getItem(appConfig.isAdminEnabled() ? appSessionKey : sessionKey))
    const token = session?.token?.token
    const requestConfig = token ? {headers: {'Authorization': `Bearer ${token}`}} : {}
    const apiUrl = session?.apiUrl

    return axios.get(getLocaleUrl(apiUrl), requestConfig)
}


window.onload = function () {
    const session = JSON.parse(localStorage.getItem(sessionKey))

    const defaultRetryTime = 2
    const maxRetryTime = 15

    let retryTime = defaultRetryTime

    function showMessageAndRetry(getMessage) {
        if (retryTime <= defaultRetryTime) {
            document.body.innerHTML = getMessage()
        }

        setTimeout(() => {
            retryTime = Math.min(maxRetryTime, retryTime + 1)
            load()
        }, retryTime * 1000)
    }

    function load() {
        const promises = [
            createBootstrapConfigRequest(),
            createLocaleRequest()
        ]

        axios.all(promises)
            .then(function (results) {
                if (results[0].data.type === 'HousekeepingInProgressClientConfig') {
                    showMessageAndRetry(housekeepingHTML)
                } else {
                    document.body.innerHTML = ''
                    const config = results[0].data
                    const locale = results[1].data
                    loadApp(config, locale)
                }
            })
            .catch(function (err) {
                const response = err.response
                if (response?.data?.error?.code === 'error.validation.not_seeded_tenant') {
                    showMessageAndRetry(notSeededHTML)
                } else {
                    const errorCode = response ? err.response.status : null
                    const appSession = localStorage.getItem(appSessionKey)

                    if (Math.floor(errorCode / 100) === 4 && (session !== null || appSession !== null)) {
                        localStorage.removeItem(sessionKey)
                        localStorage.removeItem(appSessionKey)
                        window.location.reload()
                    } else {
                        document.body.innerHTML = bootstrapErrorHTML(errorCode)
                    }
                }
            })
    }

    load()
}
