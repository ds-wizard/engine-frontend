'use strict'

// The registry's web client, on the design system. It asks the registry what it
// offers (`/bootstrap`), applies the stylesheet, and starts the Elm
// program with the flags `Registry.Data.AppState` reads. The ports are the
// shared ones it uses: the session, the appearance, copying, and the form and
// focus helpers.

const axios = require('axios').default
const axiosRetry = require('axios-retry').default

const {createNavigatorData} = require('../../packages/elm-common/js/navigator')
const {getSession, registerSessionPorts} = require('../../packages/elm-common/js/ports/session')
const registerCommonDomPorts = require('../../packages/elm-common/js/ports/dom')
const registerCommonFormPorts = require('../../packages/elm-common/js/ports/form')
const registerConsolePorts = require('../../packages/elm-common/js/ports/console')
const registerUiCopyPort = require('../../packages/elm-ui/js/copy')
const registerUiThemePorts = require('../../packages/elm-ui/js/theme')

require('../../packages/elm-ui/js/ui-dialog')
require('../../packages/elm-ui/js/tooltip')
require('../../packages/elm-ui/js/menu-fit')
require('../../packages/elm-ui/js/mode-switch')
require('../../packages/elm-ui/js/form-errors')

const program = require('./elm/Registry.elm')

axiosRetry(axios, {
    retries: 3,
    retryDelay: function (retryCount) {
        return retryCount * 1000
    }
})


function apiUrl() {
    return (window.app && window.app.apiUrl) || window.location.origin + '/api'
}

function readUiMode() {
    try {
        return localStorage.getItem('ui-mode')
    } catch (e) {
        return null
    }
}

// See apps/dsw/index.ejs: the stylesheet is preloaded and applied here, and the
// program starts once it has loaded, so it never paints unstyled.
function applyStylesheets() {
    const preloads = Array.from(document.querySelectorAll('link[data-app-stylesheet]'))
    return Promise.all(preloads.map(function (preload) {
        return new Promise(function (resolve) {
            const sheet = document.createElement('link')
            sheet.rel = 'stylesheet'
            sheet.href = preload.href
            sheet.addEventListener('load', resolve, {once: true})
            sheet.addEventListener('error', resolve, {once: true})
            document.head.appendChild(sheet)
        })
    }))
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

function start(config) {
    const session = getSession()
    // A session of another API is not this registry's.
    const ownSession = session && session.apiUrl === apiUrl() ? session : null

    // The loader goes before the program takes the body over.
    document.body.innerHTML = ''
    const elmApp = program.Elm.Registry.init({
        node: document.body,
        flags: {
            apiUrl: apiUrl(),
            appTitle: (window.app && window.app.appTitle) || null,
            config: config,
            session: ownSession,
            navigator: createNavigatorData(),
            uiMode: readUiMode()
        }
    })

    const app = withOptionalPorts(elmApp)
    registerConsolePorts(app)
    registerCommonDomPorts(app)
    registerCommonFormPorts(app)
    registerSessionPorts(app)
    registerUiCopyPort(app)
    registerUiThemePorts(app)
}

function startWithoutConfig() {
    // The program says the registry cannot start: flags it cannot read.
    document.body.innerHTML = ''
    program.Elm.Registry.init({node: document.body, flags: {apiUrl: apiUrl()}})
}

window.onload = function () {
    Promise.all([axios.get(apiUrl() + '/bootstrap'), applyStylesheets()])
        .then(function ([response]) {
            start(response.data)
        })
        .catch(function () {
            applyStylesheets().then(startWithoutConfig)
        })
}
