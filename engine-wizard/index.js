'use strict'

const axios = require('axios').default
const axiosRetry = require('axios-retry').default

const appConfig = require('./js/app-config')
const {bootstrapErrorHTML, housekeepingHTML, notSeededHTML} = require('./js/bootstrap-error')

const program = require('./elm/Wizard.elm')

require('./js/components/charts')
require('./js/components/code-editor')
require('./js/components/datetime-pickers')
require('./js/components/shortcut-element')

const cookies = require('./js/ports/cookies')
const registerBrowserPorts = require('./js/ports/browser')
const registerConsolePorts = require('./js/ports/console')
const registerCopyPorts = require('../engine-shared/ports/copy')
const registerDomPorts = require('./js/ports/dom')
const registerDownloadPorts = require('./js/ports/download')
const registerImportPorts = require('./js/ports/import')
const registerIntegrationPorts = require('./js/ports/integrations')
const registerLocalStoragePorts = require('./js/ports/local-storage')
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
        apiUrl = apiUrl.replace('/wizard-api', '/admin-api')
        return apiUrl + '/locales/current/content?module=wizard'
    }
    return apiUrl + '/locales/current/content'
}


function loadApp(config, locale, provisioning) {
    const flags = {
        seed: Math.floor(Math.random() * 0xFFFFFFFF),
        session: JSON.parse(localStorage.getItem(sessionKey)),
        selectedLocale: JSON.parse(localStorage.locale || null),
        apiUrl: getApiUrl(config),
        clientUrl: appConfig.getClientUrl(),
        webSocketThrottleDelay: appConfig.getWebSocketThrottleDelay(),
        config: config,
        provisioning: provisioning,
        localProvisioning: appConfig.getProvisioning(),
        navigator: {
            pdf: getPdfSupport()
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

    registerBrowserPorts(app)
    registerConsolePorts(app)
    registerCopyPorts(app)
    registerDomPorts(app)
    registerDownloadPorts(app)
    registerImportPorts(app)
    registerIntegrationPorts(app)
    registerLocalStoragePorts(app)
    registerPageUnloadPorts(app)
    registerRefreshPorts(app)
    registerSessionPorts(app, sessionKey, ['session/app'])
    registerThemePorts(app)
    registerWebsocketPorts(app)
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
    const session = JSON.parse(localStorage.getItem(appConfig.isAdminEnabled() ? 'session/app' : sessionKey))
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
        const hasProvisioning = appConfig.hasProvisioning()
        if (hasProvisioning) {
            promises.push(axios.get(appConfig.getProvisioningUrl()))
        }

        axios.all(promises)
            .then(function (results) {
                if (results[0].data.type === 'HousekeepingInProgressClientConfig') {
                    showMessageAndRetry(housekeepingHTML)
                } else {
                    const config = results[0].data
                    const locale = results[1].data
                    const provisioning = hasProvisioning ? results[2].data : null
                    loadApp(config, locale, provisioning)
                }
            })
            .catch(function (err) {
                const response = err.response
                if (response?.data?.error?.code === 'error.validation.not_seeded_tenant') {
                    showMessageAndRetry(notSeededHTML)
                } else {
                    const errorCode = response ? err.response.status : null
                    if (Math.floor(errorCode / 100) === 4 && session !== null) {
                        localStorage.removeItem(sessionKey)
                        window.location.reload()
                    } else {
                        document.body.innerHTML = bootstrapErrorHTML(errorCode)
                    }
                }
            })
    }

    load()
}
