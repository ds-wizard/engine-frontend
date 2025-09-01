function getOrigin(url) {
    const l = document.createElement('a')
    l.href = url
    const origin = [l.protocol, l.hostname].join('//')
    return l.port ? [origin, l.port].join(':') : origin
}

function getWindowFeatures(defaultWidth, defaultHeight) {
    const width = Math.min(defaultWidth, window.screen.width - 300)
    const height = Math.min(defaultHeight, window.screen.height - 200)
    const left = (window.screen.width - width) / 2
    const top = (window.screen.height - height) / 2
    return 'width=' + width + ",height=" + height + ",top=" + top + ",left=" + left
}

function getStyleUrl() {
    const style = document.querySelector('[rel="stylesheet"]')
    return window.location.origin + style.getAttribute('href')
}

function createIntegration(config) {
    config.openPort.subscribe(open)

    let lastListener

    function bind(handler) {
        window.addEventListener('message', handler, false)
        lastListener = handler
    }

    function unbind() {
        if (lastListener) {
            window.removeEventListener('message', lastListener)
            lastListener = null
        }
    }

    function open(integrationConfig) {
        unbind()

        const origin = getOrigin(integrationConfig.url)
        const windowFeatures = getWindowFeatures(config.windowDefaultWidth, config.windowDefaultHeight)
        const popup = window.open(integrationConfig.url, 'popup', windowFeatures)
        const handler = function (event) {
            if (event.origin !== origin) {
                return
            }

            if (event.data.type === 'ready') {
                popup.postMessage({
                    type: 'ready',
                    styleUrl: getStyleUrl(),
                    theme: integrationConfig.theme,
                    data: integrationConfig.data
                }, integrationConfig.url)
            } else {
                config.eventHandler(event, function () {
                    unbind()
                })
            }
        }
        bind(handler)
    }
}

module.exports = createIntegration
