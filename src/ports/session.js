module.exports = function (app) {
    app.ports.storeSession.subscribe(storeSession);
    app.ports.clearSession.subscribe(clearSession);


    function storeSession(session) {
        localStorage.session = JSON.stringify(session);
    }


    function clearSession() {
        localStorage.removeItem('session');
    }
};
