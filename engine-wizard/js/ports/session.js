module.exports = function (app, sessionKey) {
    app.ports.storeSession.subscribe(storeSession);
    app.ports.clearSession.subscribe(clearSession);
    app.ports.clearSessionAndReload.subscribe(clearSessionAndReload);


    function storeSession(session) {
        localStorage[sessionKey] = JSON.stringify(session);
    }


    function clearSession() {
        localStorage.removeItem(sessionKey);
    }


    function clearSessionAndReload() {
        clearSession()
        location.reload()
    }
};
