module.exports = function (app) {
    app.ports.setUnloadMessage.subscribe(setUnloadMessage)
    app.ports.clearUnloadMessage.subscribe(clearUnloadMessage)
    app.ports.alert.subscribe(alert)


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

    function alert(msg) {
        window.alert(msg)
    }
}
