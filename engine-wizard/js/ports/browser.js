module.exports = function (app) {
    app.ports.historyBack.subscribe(historyBack)

    function historyBack(fallbackUrl) {
        if (window.history.length > 2) {
            app.ports.historyBackCallback.send('')
        } else {
            app.ports.historyBackCallback.send(fallbackUrl)
        }
    }
}
