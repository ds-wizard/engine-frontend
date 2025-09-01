module.exports = function (app) {
    app.ports?.saveSession?.subscribe(saveSession)
    app.ports?.clearSession?.subscribe(clearSession)

    function saveSession(session) {
        localStorage.setItem('session', JSON.stringify(session))
    }

    function clearSession(session) {
        localStorage.removeItem('session')
    }
}
