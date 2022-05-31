module.exports = function (app) {
    if (app.ports.setUnloadMessage) {
        app.ports.setUnloadMessage.subscribe(setUnloadMessage);
    }

    if (app.ports.clearUnloadmessage) {
        app.ports.clearUnloadMessage.subscribe(clearUnloadMessage);
    }

    if (app.ports.alert) {
        app.ports.alert.subscribe(alert);
    }


    function setUnloadMessage(msg) {
        window.onbeforeunload = function (e) {
            e.preventDefault();
            e.returnValue = '';
            return msg;
        };

        var skipAlert = false;

        window.onpopstate = function () {
            window.history.go(1);

            if (skipAlert) {
                skipAlert = false;
            } else {
                window.alert(msg);
                skipAlert = true;
            }
        };
    }

    function clearUnloadMessage() {
        window.onbeforeunload = undefined;
    }

    function alert(msg) {
        window.alert(msg);
    }
};
