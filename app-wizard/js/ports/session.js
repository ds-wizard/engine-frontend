module.exports = function (app, sessionKey, otherSessionKeys) {
    app.ports.storeSession.subscribe(storeSession);
    app.ports.clearSession.subscribe(clearSession);
    app.ports.clearSessionAndReload.subscribe(clearSessionAndReload);


    function storeSession(session) {
        localStorage[sessionKey] = JSON.stringify(session);
    }


    function clearSession() {
        localStorage.removeItem(sessionKey);
        if (otherSessionKeys) {
            otherSessionKeys.forEach(function (otherSessionKey) {
                localStorage.removeItem(otherSessionKey)
            })
        }
    }


    function clearSessionAndReload() {
        clearSession()
        location.reload()
    }
};
