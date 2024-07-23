module.exports = function (app) {
    app.ports.localStorageGet?.subscribe(localStorageGet)
    app.ports.localStorageGetAndRemove?.subscribe(localStorageGetAndRemove)
    app.ports.localStorageSet?.subscribe(localStorageSet)
    app.ports.localStorageRemove?.subscribe(localStorageRemove)

    function localStorageGet(key) {
        var value = localStorage.getItem(key)

        window.requestAnimationFrame(() => {
            app.ports.localStorageData.send({
                key: key,
                value: JSON.parse(value)
            })
        })
    }

    function localStorageGetAndRemove(key) {
        var value = localStorage.getItem(key)
        localStorage.removeItem(key)

        window.requestAnimationFrame(() => {
            app.ports.localStorageData.send({
                key: key,
                value: value ? JSON.parse(value) : null
            })
        })
    }

    function localStorageSet(data) {
        localStorage.setItem(data.key, JSON.stringify(data.value))
    }

    function localStorageRemove(key) {
        localStorage.removeItem(key)
    }
}
