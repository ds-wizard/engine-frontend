'use strict'

// How a client built on the design system starts: it asks the API for the
// bootstrap configuration and the locale, loads the plugins, then starts its
// Elm program with the flags every such client takes (Common.Data.Flags) and
// registers the ports they share. Clients differ only in which program they
// hand in:
//
//     require('../../packages/elm-common/js/app')(require('./elm/Dsw.elm').Elm.Dsw)
//
// A client with ports of its own passes them as the second argument:
// `flags(config)` returns flags to add to the shared ones, and
// `registerPorts(app, config)` subscribes its ports once the program runs.
// `workspaceBootstrap: true` has the second part of the configuration read
// here, once, before the program starts.

const axios = require('axios').default
const axiosRetry = require('axios-retry').default

const appConfig = require('./app-config')
const {bootstrapErrorHTML, housekeepingHTML, notSeededHTML} = require('./bootstrap-error')()
const {createNavigatorData} = require('./navigator')

// Custom elements: the design system's own wrappers for the charts, the code
// editor, the date pickers and the markdown editor.
require('./components/shortcut-element')
require('../../elm-ui/js/ui-dialog')
require('../../elm-ui/js/tooltip')
require('../../elm-ui/js/menu-fit')
require('../../elm-ui/js/form-errors')
require('../../elm-ui/js/list-field')
require('../../elm-ui/js/suggest-input')
require('../../elm-ui/js/date-picker')
require('../../elm-ui/js/color-field')
require('../../elm-ui/js/markdown-editor')
require('../../elm-ui/js/code-editor')
require('../../elm-ui/js/chart')

const registerConsolePorts = require('./ports/console')
const registerCopyPorts = require('./ports/copy')
const registerDownloadPorts = require('./ports/file')
const registerLocalePorts = require('./ports/locale')
const registerLocalStoragePorts = require('./ports/local-storage')
const {getSession, clearSession, registerSessionPorts, sessionApiUrl} = require('./ports/session')
const registerCommonDomPorts = require('./ports/dom')
const registerCommonFormPorts = require('./ports/form')
const registerCommonTourPorts = require('./ports/tour')
const registerUiCopyPort = require('../../elm-ui/js/copy')
const registerUiThemePorts = require('../../elm-ui/js/theme')
const sideNavStorage = require('../../elm-ui/js/side-nav-storage')
const registerWebsocketPorts = require('./ports/websocket')
const registerWindowPorts = require('./ports/window')


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
        const bootstrapConfigPath = '/bootstrap?clientUrl=' + encodeURIComponent(appConfig.getClientUrl())
        return appConfig.getDefaultApiUrl() + bootstrapConfigPath
    }
    return apiUrl + '/bootstrap'
}

function getWorkspaceBootstrapUrl(apiUrl) {
    return (apiUrl || appConfig.getDefaultApiUrl()) + '/bootstrap/workspace'
}

function translationLanguage(locale) {
    const header = locale && locale.locale_data && locale.locale_data.messages && locale.locale_data.messages['']
    return header && typeof header.lang === 'string' ? header.lang : null
}

function getLocaleUrl(session) {
    return (sessionApiUrl(session) || appConfig.getDefaultApiUrl()) + '/locales/current/content'
}


// The stylesheet index.ejs preloads, applied as a stylesheet in the head —
// where neither the bootstrap, which empties the body, nor Elm, which takes the
// body over, can remove it. It is preloaded rather than linked so that it does
// not hold up the first paint and the loader shows at once; the application
// starts only once it has loaded, so the application never paints unstyled.
//
// A preload that failed still resolves: an application without its stylesheet
// is worse than one with it, and better than none at all.
function applyStylesheets() {
    const preloads = Array.from(document.querySelectorAll('link[data-app-stylesheet]'))

    return Promise.all(preloads.map(function (preload) {
        return new Promise(function (resolve) {
            const sheet = document.createElement('link')
            sheet.rel = 'stylesheet'
            sheet.href = preload.href
            sheet.addEventListener('load', resolve, { once: true })
            sheet.addEventListener('error', resolve, { once: true })
            document.head.appendChild(sheet)
        })
    }))
}

const stylesheetsApplied = applyStylesheets()


function loadApp(config, workspaceConfig, locale, plugins) {
    const extension = clientExtension || {}
    const apiUrl = getApiUrl(config)

    const flags = {
        seed: Math.floor(Math.random() * 0xFFFFFFFF),
        session: getSession(),
        apiUrl: apiUrl,
        clientUrl: appConfig.getClientUrl(),
        webSocketThrottleDelay: appConfig.getWebSocketThrottleDelay(),
        config: config,
        workspaceConfig: workspaceConfig,
        navigator: createNavigatorData(),
        guideLinks: appConfig.getGuideLinks(),
        maxUploadFileSize: appConfig.getMaxUploadFileSize(),
        newsUrl: appConfig.getNewsUrl(),
        urlCheckerUrl: appConfig.getUrlCheckerUrl(),
        privacyPolicyUrl: appConfig.getPrivacyPolicyUrl(),
        termsOfServiceUrl: appConfig.getTermsOfServiceUrl(),
        plugins: plugins.map(p => initPlugin(config, p)),
        // The collapsed flag lives in the session; only the
        // width someone dragged the navigation to is kept here.
        sideNavWidth: sideNavStorage.read().sideNavWidth,
        // The appearance the pre-paint script in index.ejs has already applied,
        // so the switch in the account menu starts on the one that is showing.
        uiMode: readUiMode(),
        ...(extension.flags ? extension.flags(config) : {})
    }

    if (Object.keys(locale).length > 0) {
        flags.locale = locale
    }

    // The page's language is the translation's, where it says one (its PO's
    // Language header); English otherwise.
    document.documentElement.lang = translationLanguage(locale) || 'en'

    const elmApp = elmProgram.init({
        node: document.body,
        flags: flags,
    })

    // Elm drops ports the program never uses, and the shared registration
    // modules subscribe unconditionally. A port that is not there yet is a
    // feature not built yet, not a reason to stop the application starting.
    const app = withOptionalPorts(elmApp)

    registerConsolePorts(app)
    registerCommonDomPorts(app)
    registerCommonFormPorts(app)
    registerCommonTourPorts(app)
    registerCopyPorts(app)
    registerDownloadPorts(app)
    registerLocalePorts(app)
    registerLocalStoragePorts(app)
    registerSessionPorts(app)
    registerUiCopyPort(app)
    registerUiThemePorts(app)
    registerWebsocketPorts(app)
    registerWindowPorts(app)
    sideNavStorage.register(app)

    if (extension.registerPorts) {
        extension.registerPorts(app, config)
    }
}

function readUiMode() {
    try {
        return localStorage.getItem('ui-mode')
    } catch (e) {
        return null
    }
}

function withOptionalPorts(elmApp) {
    const ports = elmApp.ports || {}
    const missing = {subscribe: () => {}, unsubscribe: () => {}, send: () => {}}
    return {
        ports: new Proxy(ports, {
            get: (target, name) => (name in target ? target[name] : missing),
            has: (target, name) => name in target
        })
    }
}

function createRequestConfig(session) {
    const token = session?.token?.token
    return token ? {headers: {'Authorization': `Bearer ${token}`}} : {}
}

function createBootstrapConfigRequest(session) {
    return axios.get(getBootstrapConfigUrl(sessionApiUrl(session)), createRequestConfig(session))
}

function createLocaleRequest(session) {
    return axios.get(getLocaleUrl(session), createRequestConfig(session))
}

// A plugin registers its custom elements when it is initialized, and an
// element cannot be registered twice: each plugin is initialized once for the
// life of the page, whatever asks for it again. It is given the settings the
// bootstrap has; an element reads the ones in force from its attributes.
const initializedPlugins = new Map()

function initPlugin(config, plugin) {
    if (initializedPlugins.has(plugin.uuid)) {
        return initializedPlugins.get(plugin.uuid)
    }
    const pluginSettings = (config.pluginSettings && config.pluginSettings[plugin.uuid]) || null
    const pluginUserSettings = (config.user && config.user.pluginSettings && config.user.pluginSettings[plugin.uuid]) || null
    const initialized = plugin.init(pluginSettings, pluginUserSettings)
    initializedPlugins.set(plugin.uuid, initialized)
    return initialized
}

// The second part of the configuration, for a client that reads it once at
// start-up (`workspaceBootstrap` in its extension) and for somebody signed in.
// A client whose workspace changes while it runs reads it itself. A failed
// read is a failed start, like the first part's: the application does not start
// on the defaults in its place.
function loadWorkspaceConfig(session) {
    const signedIn = Boolean(session?.token?.token)
    if (!signedIn || !(clientExtension && clientExtension.workspaceBootstrap)) {
        return Promise.resolve(null)
    }
    return axios.get(getWorkspaceBootstrapUrl(sessionApiUrl(session)), createRequestConfig(session))
        .then(response => response.data)
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

let elmProgram = null
let clientExtension = null

module.exports = function start(program, extension) {
    elmProgram = program
    clientExtension = extension
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

                        return loadWorkspaceConfig(session).then(workspaceConfig => {
                            loadPlugins(config)
                                .then(plugins => stylesheetsApplied.then(() => {
                                    loadApp(config, workspaceConfig, locale, plugins)
                                }))
                                .catch(err => {
                                    console.error("Error loading plugins:", err);
                                    stylesheetsApplied.then(() => {
                                        loadApp(config, workspaceConfig, locale, [])
                                        window.dispatchEvent(new CustomEvent('bootstrapConfigLoad', {detail: config}))
                                    })
                                })
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
}
