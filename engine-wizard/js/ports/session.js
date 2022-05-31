module.exports = function (app) {
    app.ports.storeSession.subscribe(storeSession);
    app.ports.clearSession.subscribe(clearSession);
    app.ports.clearSessionAndReload.subscribe(clearSessionAndReload);


    function storeSession(session) {
        localStorage.session = JSON.stringify(session);
    }


    function clearSession() {
        localStorage.removeItem('session');
    }


    function clearSessionAndReload() {
        clearSession()
        location.reload()
    }
};
