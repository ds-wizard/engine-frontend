module.exports = function (app) {
    if (app.ports.wsOpen) app.ports.wsOpen.subscribe(wsOpen)
    if (app.ports.wsClose) app.ports.wsClose.subscribe(wsClose)
    if (app.ports.wsSend) app.ports.wsSend.subscribe(wsSend)
    if (app.ports.wsPing) app.ports.wsPing.subscribe(wsPing)

    var websockets = {}

    function wsOpen(url) {
        if (!websockets[url]) {
            var ws = new WebSocket(url)
            websockets[url] = ws

            function sendMsg(type, data) {
                if (app.ports.wsMessage) {
                    app.ports.wsMessage.send({url: url, type: type, data: data})
                }
            }

            ws.addEventListener('open', function (event) {
                sendMsg('open', {})
            })

            ws.addEventListener('message', function (event) {
                sendMsg('message', JSON.parse(event.data))
            })

            ws.addEventListener("close", function (event) {
                var reconnect = !event.wasClean && websockets[url]

                if (reconnect) {
                    delete websockets[url]
                    wsOpen(url)
                } else {
                    sendMsg('close', null)
                }
            })

            ws.addEventListener("error", function (event) {

            })
        }
    }

    function wsSend(payload) {
        websockets[payload[0]].send(JSON.stringify(payload[1]))
    }

    function wsPing(url) {
        if (websockets[url]) {
            websockets[url].send('ping')
        }
    }

    function wsClose(url) {
        if (websockets[url]) {
            websockets[url].close()
            delete websockets[url]
        }
    }
}
