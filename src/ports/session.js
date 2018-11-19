module.exports = function (app) {
    app.ports.storeSession.subscribe(storeSession);
    app.ports.clearSession.subscribe(clearSession);


    function storeSession(session) {
        localStorage.session = JSON.stringify(session);
    }


    function clearSession() {
        localStorage.removeItem('session');
    }


    window.addEventListener("storage", function (event) {
        if (event.storageArea === localStorage && event.key === "session") {
            app.ports.onSessionChange.send(event.newValue);
        }
    });
};
