module.exports = (app) => {
    app.ports.setThemePort?.subscribe(setTheme)

    function setTheme(value) {
        document.body.setAttribute('style', value)
    }
}
