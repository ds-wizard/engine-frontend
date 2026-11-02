// The session the clients keep in local storage: the token, a few layout flags,
// and the API it belongs to, so that the next load knows where to bootstrap
// from without asking for the client config first.
//
// There is one API for everything, so the session names it as it is. Sessions
// stored by the clients that had one API per application (`session/app`,
// `session/wizard`) cannot be used with it and are removed.

const sessionKey = 'session'
const legacySessionKeys = ['session/app', 'session/wizard']


module.exports = {
    getSession,
    clearSession,
    registerSessionPorts,
    sessionApiUrl
}


function parseSession(value) {
    try {
        return JSON.parse(value)
    } catch (e) {
        return null
    }
}

function removeLegacySessions() {
    legacySessionKeys.forEach(function (key) {
        localStorage.removeItem(key)
    })
}

function getSession() {
    removeLegacySessions()

    const session = parseSession(localStorage.getItem(sessionKey))
    return session && session.token && typeof session.apiUrl === 'string' ? session : null
}

function storeSession(session) {
    localStorage.setItem(sessionKey, JSON.stringify(session))
}

function clearSession() {
    localStorage.removeItem(sessionKey)
    removeLegacySessions()
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

function sessionApiUrl(session) {
    return session?.apiUrl || null
}
