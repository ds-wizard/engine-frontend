module.exports = function (app) {
    if (app.ports.wsOpen) app.ports.wsOpen.subscribe(wsOpen)
    if (app.ports.wsClose) app.ports.wsClose.subscribe(wsClose)
    if (app.ports.wsSend) app.ports.wsSend.subscribe(wsSend)
    if (app.ports.wsPing) app.ports.wsPing.subscribe(wsPing)

    const websockets = {}
    const queues = {}
    const eventListeners = {}

    function wsOpen(url) {
        if (!websockets[url]) {
            const ws = new WebSocket(url)
            websockets[url] = ws

            function sendMsg(type, data) {
                if (app.ports.wsMessage) {
                    app.ports.wsMessage.send({url: url, type: type, data: data})
                }
            }

            eventListeners[url] = {
                'open': function (event) {
                    sendMsg('open', {})

                    if (queues[url]) {
                        while (queues[url].length > 0) {
                            wsSend([url, queues[url].shift()])
                        }
                    }
                },
                'message': function (event) {
                    sendMsg('message', JSON.parse(event.data))
                },
                'close': function (event) {
                    var reconnect = !event.wasClean && websockets[url]

                    if (reconnect) {
                        delete websockets[url]
                        setTimeout(function() { wsOpen(url) }, 500)
                    } else {
                        sendMsg('close', null)
                    }
                },
                'error': function (event) {
                }
            }

            bindEventListeners(ws, eventListeners[url])
        }
    }

    function wsSend(payload) {
        var websocket = websockets[payload[0]]
        if (websocket && websocket.readyState === 1) {
            websockets[payload[0]].send(JSON.stringify(payload[1]))
        } else {
            queueMessage(payload[0], payload[1])
        }
    }

    function wsPing(url) {
        if (websockets[url] && websockets[url].readyState === 1) {
            websockets[url].send('ping')
        }
    }

    function wsClose(url) {
        if (websockets[url]) {
            if (eventListeners[url]) {
                unbindEventListeners(websockets[url], eventListeners[url])
                delete eventListeners[url]
            }
            websockets[url].close()
            delete websockets[url]
        }

        if (queues[url]) {
            delete queues[url]
        }
    }

    function queueMessage(url, message) {
        if (!queues[url]) {
            queues[url] = []
        }
        queues[url].push(message)
    }

    function bindEventListeners(ws, eventListeners) {
        for (const [event, eventListener] of Object.entries(eventListeners)) {
            ws.addEventListener(event, eventListener)
        }
    }

    function unbindEventListeners(ws, eventListeners) {
        for (const [event, eventListener] of Object.entries(eventListeners)) {
            ws.removeEventListener(event, eventListener)
        }
    }
}
