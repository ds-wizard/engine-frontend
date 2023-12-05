module.exports = function (app) {
    app.ports.localStorageGet.subscribe(localStorageGet)
    app.ports.localStorageSet.subscribe(localStorageSet)

    function localStorageGet(key) {
        var value = localStorage.getItem(key)

        window.requestAnimationFrame(() => {
            app.ports.localStorageData.send({
                key: key,
                value: JSON.parse(value)
            })
        })
    }

    function localStorageSet(data) {
        localStorage.setItem(data.key, JSON.stringify(data.value))
    }
}
