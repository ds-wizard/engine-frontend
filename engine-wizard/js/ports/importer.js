module.exports = function (app) {
    app.ports.openImporter.subscribe(openImporter)

    var lastListener

    function openImporter(requestUrl) {
        if (lastListener) {
            unbind()
        }

        var widgetOrigin = getWidgetOrigin(requestUrl)
        var popup = window.open(requestUrl, 'popup', getWindowFeatures())
        var handler = (event) => {
            if (event.data.type === 'ready') {
                popup.postMessage({
                    type: 'ready',
                    styleUrl: window.wizard.styleUrl
                }, requestUrl)
            } else if (event.data.type === 'Import') {
                if (event.origin !== widgetOrigin) {
                    return
                }

                app.ports.gotImporterData.send(event.data.events)
            }
        }
        bind(handler)
    }

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
    var width = Math.min(660, window.screen.width - 300)
    var height = Math.min(360, window.screen.height - 200)
    var left = (window.screen.width - width) / 2
    var top = (window.screen.height - height) / 2
    return 'width=' + width + ",height=" + height + ",top=" + top + ",left=" + left
}

