module.exports = function (app) {
    app.ports.alert?.subscribe(alert)
    app.ports.refresh?.subscribe(refresh)
    app.ports.historyBack?.subscribe(historyBack)
    app.ports.setUnloadMessage?.subscribe(setUnloadMessage)
    app.ports.clearUnloadMessage?.subscribe(clearUnloadMessage)

    function alert(msg) {
        window.alert(msg)
    }

    function refresh() {
        window.location.reload()
    }

    function historyBack(fallbackUrl) {
        if (window.history.length > 2) {
            app.ports.historyBackCallback?.send('')
        } else {
            app.ports.historyBackCallback?.send(fallbackUrl)
        }
    }

    function setUnloadMessage(msg) {
        window.onbeforeunload = function (e) {
            e.preventDefault()
            e.returnValue = ''
            return msg
        }
    }

    function clearUnloadMessage() {
        window.onbeforeunload = undefined
    }
}
