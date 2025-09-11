module.exports = (app) => {
    app.ports.localStorageSetItem?.subscribe(setItem)
    app.ports.localStorageGetItem?.subscribe(getItem)
    app.ports.localStorageGetAndRemoveItem?.subscribe(getAndRemoveItem)
    app.ports.localStorageRemoveItem?.subscribe(removeItem)

    function setItem({key, value}) {
        localStorage[key] = JSON.stringify(value)
    }

    function getItem(key) {
        let value = localStorage[key]

        if (value) {
            value = JSON.parse(value)
        }

        window.requestAnimationFrame(() => {
            app.ports.localStorageGotItem?.send({key, value})
        })
    }

    function getAndRemoveItem(key) {
        let value = localStorage[key]

        if (value) {
            value = JSON.parse(value)
        }

        localStorage.removeItem(key)

        window.requestAnimationFrame(() => {
            app.ports.localStorageGotItem?.send({key, value})
        })
    }

    function removeItem(key) {
        localStorage.removeItem(key)
    }
}
