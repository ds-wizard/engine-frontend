module.exports = function (app) {
    app.ports.refresh.subscribe(refresh)

    function refresh() {
        window.location.reload()
    }
}
