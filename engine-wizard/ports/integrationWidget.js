module.exports = function (app) {
    app.ports.openIntegrationWidget.subscribe(openIntegrationWidget)

    var lastListener

    function openIntegrationWidget({path, requestUrl}) {
        if (lastListener) {
            unbind()
        }

        var widgetOrigin = getWidgetOrigin(requestUrl)
        var popup = window.open(requestUrl, 'popup', getWindowFeatures())
        var handler = (event) => {
            if (event.data.type === 'ready') {
                popup.postMessage({type: 'path', path}, requestUrl)
            } else if (event.data.type === 'selection') {
                if (event.origin !== widgetOrigin) {
                    return
                }

                event.data.id = "" + event.data.id
                event.data.name = "" + event.data.name

                app.ports.gotIntegrationWidgetValue.send(event.data)
                unbind()
            }
        }

        bind(handler)
    }

    function bind(handler) {
        window.addEventListener('message', handler, false)
        lastListener = handler
    }

    function unbind() {
        window.removeEventListener('message', lastListener)
        lastListener = null
    }

    function getWidgetOrigin(url) {
        var l = document.createElement('a')
        l.href = url
        var origin = [l.protocol, l.hostname].join('//')
        return l.port ? [origin, l.port].join(':') : origin
    }

    function getWindowFeatures() {
        var width = Math.min(1200, window.screen.width - 300)
        var height = Math.min(800, window.screen.height - 200)
        var left = (window.screen.width - width) / 2
        var top = (window.screen.height - height) / 2
        return 'width=' + width + ",height=" + height + ",top=" + top + ",left=" + left
    }
}
