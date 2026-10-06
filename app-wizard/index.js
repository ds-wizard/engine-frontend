'use strict'

const axios = require('axios').default
const axiosRetry = require('axios-retry').default

const appConfig = require('./js/app-config')
const {bootstrapErrorHTML, housekeepingHTML, notSeededHTML} = require('../shared/common/js/bootstrap-error')('wizard')
const {createNavigatorData} = require('../shared/common/js/navigator')

const program = require('./elm/Wizard.elm')

require('./js/components/charts')
require('./js/components/code-editor')
require('../shared/common/js/components/datetime-pickers')
require('../shared/common/js/components/markdown-editor')
require('../shared/common/js/components/shortcut-element')

const cookies = require('./js/ports/cookies')
const registerConsolePorts = require('./js/ports/console')
const registerCopyPorts = require('../shared/common/js/ports/copy')
const registerDomPorts = require('../shared/common/js/ports/dom')
const registerDownloadPorts = require('../shared/common/js/ports/file')
const registerDriverPorts = require('../shared/common/js/ports/driver')
const registerFormUtilsPorts = require('../shared/common/js/ports/form-utils')
const registerImportPorts = require('./js/ports/import')
const registerLocalePorts = require('../shared/common/js/ports/locale')
const registerLocalStoragePorts = require('../shared/common/js/ports/local-storage')
const {getSession, clearSession, registerSessionPorts, sessionApiUrl, toApiUrlBase} = require('../shared/common/js/ports/session')
const registerThemePorts = require('../shared/common/js/ports/theme')
const registerWebsocketPorts = require('../shared/common/js/ports/websocket')
const registerWindowPorts = require('../shared/common/js/ports/window')


const appName = 'wizard'

axiosRetry(axios, {
    retries: 3,
    retryDelay: function (retryCount) {
        return retryCount * 1000
    }
})

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

function getLocaleUrl(session) {
    if (appConfig.isAdminEnabled()) {
        const adminApiUrl = appConfig.getAdminApiUrl()
            || sessionApiUrl(session, 'admin')
            || appConfig.getDefaultApiUrl().replace('/wizard-api', '/admin-api')
        return adminApiUrl + '/locales/current/content?module=wizard'
    }
    return (sessionApiUrl(session, appName) || appConfig.getDefaultApiUrl()) + '/locales/current/content'
}


function loadApp(config, locale, plugins) {
    const apiUrl = getApiUrl(config)

    const flags = {
        seed: Math.floor(Math.random() * 0xFFFFFFFF),
        session: getSession(),
        apiUrl: apiUrl,
        apiUrlBase: toApiUrlBase(apiUrl, appName) || '',
        clientUrl: appConfig.getClientUrl(),
        webSocketThrottleDelay: appConfig.getWebSocketThrottleDelay(),
        config: config,
        navigator: createNavigatorData(),
        gaEnabled: cookies.getGaEnabled(),
        cookieConsent: cookies.getCookieConsent(),
        guideLinks: appConfig.getGuideLinks(),
        maxUploadFileSize: appConfig.getMaxUploadFileSize(),
        newsUrl: appConfig.getNewsUrl(),
        urlCheckerUrl: appConfig.getUrlCheckerUrl(),
        plugins: plugins.map(p => initPlugin(config, p)),
        aiAssistantAvailable: appConfig.isAiAssistantAvailable()
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
    registerFormUtilsPorts(app)
    registerImportPorts(app)
    registerLocalePorts(app)
    registerLocalStoragePorts(app)
    registerSessionPorts(app)
    registerThemePorts(app)
    registerWebsocketPorts(app)
    registerWindowPorts(app)
    cookies.registerCookiePorts(app)

    cookies.init()
}

function createRequestConfig(session) {
    const token = session?.token?.token
    return token ? {headers: {'Authorization': `Bearer ${token}`}} : {}
}

function createBootstrapConfigRequest(session) {
    return axios.get(getBootstrapConfigUrl(sessionApiUrl(session, appName)), createRequestConfig(session))
}

function createLocaleRequest(session) {
    return axios.get(getLocaleUrl(session), createRequestConfig(session))
}

function initPlugin(config, plugin) {
    const pluginSettings = (config.pluginSettings && config.pluginSettings[plugin.uuid]) || null
    const pluginUserSettings = (config.user && config.user.pluginSettings && config.user.pluginSettings[plugin.uuid]) || null
    return plugin.init(pluginSettings, pluginUserSettings)
}

async function importPlugin(plugin) {
    const file = plugin.enabled ? 'plugin.js' : 'manifest.js'
    const pluginUrl = plugin.url.endsWith('/') ? plugin.url : plugin.url + '/';
    const url = pluginUrl + file
    const module = await import(/* webpackIgnore: true */ url)
    return {
        uuid: plugin.uuid,
        init: module.default
    }
}

async function loadPlugins(config) {
    const plugins = config.plugins || []
    const modules = await Promise.allSettled(plugins.map(importPlugin))
    const loaded = []
    for (const r of modules) {
        if (r.status === 'fulfilled') loaded.push(r.value);
        else console.error('Error loading plugin:', r.reason);
    }
    return loaded
}

window.onload = function () {
    const session = getSession()

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
            createBootstrapConfigRequest(session),
            createLocaleRequest(session)
        ]

        axios.all(promises)
            .then(function (results) {
                if (results[0].data.type === 'HousekeepingInProgressClientConfig') {
                    showMessageAndRetry(housekeepingHTML)
                } else {
                    document.body.innerHTML = ''
                    const config = results[0].data
                    const locale = results[1].data

                    loadPlugins(config)
                        .then(plugins => {
                            loadApp(config, locale, plugins)
                        })
                        .catch(err => {
                            console.error("Error loading plugins:", err);
                            loadApp(config, locale, [])
                            window.dispatchEvent(new CustomEvent('bootstrapConfigLoad', {detail: config}))
                        })
                }
            })
            .catch(function (err) {
                const response = err.response
                if (response?.data?.error?.code === 'error.validation.not_seeded_tenant') {
                    showMessageAndRetry(notSeededHTML)
                } else {
                    const errorCode = response ? err.response.status : null

                    if (Math.floor(errorCode / 100) === 4 && session !== null) {
                        clearSession()
                        window.location.reload()
                    } else {
                        document.body.innerHTML = bootstrapErrorHTML(errorCode)
                    }
                }
            })
    }

    load()
}
