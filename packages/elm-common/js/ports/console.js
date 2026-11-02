module.exports = function (app) {
    app.ports.consoleError.subscribe(consoleError)

    function consoleError(error) {
        console.error(error)
    }
}
