const sessionKey = 'session/app'

// The per-app sessions used before Admin and Wizard shared a single user token, with the app whose
// API URL they used to store. They are read once to migrate an existing login and removed whenever
// the session is cleared.
const legacySessions = [
    {key: 'session/app', app: 'admin'},
    {key: 'session/wizard', app: 'wizard'}
]


module.exports = {
    getSession,
    clearSession,
    registerSessionPorts,
    sessionApiUrl,
    toApiUrlBase,
}


function parseSession(value) {
    try {
        return JSON.parse(value)
    } catch (e) {
        return null
    }
}

function migrateLegacySession() {
    let migratedSession = null

    for (const {key, app} of legacySessions) {
        const legacySession = parseSession(localStorage.getItem(key))

        if (!migratedSession && legacySession && legacySession.token && legacySession.token.token) {
            migratedSession = {
                token: legacySession.token,
                sidebarCollapsed: !!legacySession.sidebarCollapsed,
                rightPanelCollapsed: legacySession.rightPanelCollapsed !== false,
                fullscreen: false,
                apiUrlBase: toApiUrlBase(legacySession.apiUrl, app) || '',
                v10: true
            }
        }

        if (key !== sessionKey) {
            localStorage.removeItem(key)
        }
    }

    return migratedSession
}

function hasStoredSession() {
    return legacySessions.some(function ({key}) {
        return localStorage.getItem(key) !== null
    })
}

function getSession() {
    const session = parseSession(localStorage.getItem(sessionKey))

    if (session && session.v10) {
        return session
    }

    if (!hasStoredSession()) {
        // Nothing to migrate and nothing to clean up, do not write on an anonymous load
        return null
    }

    const migratedSession = migrateLegacySession()

    if (migratedSession) {
        storeSession(migratedSession)
    } else {
        clearSession()
    }

    return migratedSession
}

function storeSession(session) {
    localStorage.setItem(sessionKey, JSON.stringify(session))
}

function clearSession() {
    localStorage.removeItem(sessionKey)
    legacySessions.forEach(function ({key}) {
        localStorage.removeItem(key)
    })
}

function clearSessionAndReload() {
    clearSession()
    location.reload()
}

function registerSessionPorts(app) {
    app.ports.storeSession?.subscribe(storeSession)
    app.ports.clearSession?.subscribe(clearSession)
    app.ports.clearSessionAndReload?.subscribe(clearSessionAndReload)
}


// All the apps share one API base: <base>/admin-api, <base>/wizard-api, <base>/analytics-api, …
// The base is part of the session so that the next load knows where to bootstrap from without
// asking for the client config first.

function apiUrlSuffix(app) {
    return '/' + app + '-api'
}

// Returns null for an API URL that does not follow the shared layout. There is no base to share in
// that case and the apps fall back to their configured defaults.
function toApiUrlBase(apiUrl, app) {
    const suffix = apiUrlSuffix(app)
    return apiUrl && apiUrl.endsWith(suffix) ? apiUrl.slice(0, -suffix.length) : null
}

function sessionApiUrl(session, app) {
    return session?.apiUrlBase ? session.apiUrlBase + apiUrlSuffix(app) : null
}
