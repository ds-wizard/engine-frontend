// Websockets are known by a key (their path under the API URL); the URL, with
// the token, comes with every open. When a connection drops, the port waits and
// then asks Elm to open it again, so the new attempt carries the current token.

// The server ended the connection for good: normally, on lost access or a queue
// overflow, or on a message over its limit.
const FINAL_CLOSE_CODES = [1000, 1008, 1009]

// The server is shutting down; another one takes the connection straight away.
const GOING_AWAY = 1001

const OPEN = 1

const MIN_DELAY = 500
const MAX_DELAY = 30000

module.exports = function (app) {
    app.ports.wsOpen?.subscribe(wsOpen)
    app.ports.wsClose?.subscribe(wsClose)
    app.ports.wsSend?.subscribe(wsSend)
    app.ports.wsPing?.subscribe(wsPing)

    const websockets = {}
    const queues = {}
    const eventListeners = {}
    const attempts = {}
    const timers = {}

    function sendMsg(key, type, data) {
        if (app.ports.wsMessage) {
            app.ports.wsMessage.send({key: key, type: type, data: data})
        }
    }

    function wsOpen({key, url}) {
        if (websockets[key]) return

        clearTimeout(timers[key])
        delete timers[key]

        const ws = new WebSocket(toWebSocketUrl(url))
        websockets[key] = ws

        eventListeners[key] = {
            'open': function () {
                attempts[key] = 0
                sendMsg(key, 'open', {})

                if (queues[key]) {
                    while (queues[key].length > 0) {
                        wsSend([key, queues[key].shift()])
                    }
                }
            },
            'message': function (event) {
                sendMsg(key, 'message', JSON.parse(event.data))
            },
            'close': function (event) {
                unbindEventListeners(ws, eventListeners[key])
                delete eventListeners[key]
                delete websockets[key]

                if (FINAL_CLOSE_CODES.includes(event.code)) {
                    delete attempts[key]
                    delete queues[key]
                    sendMsg(key, 'close', null)
                } else {
                    const attempt = attempts[key] || 0
                    attempts[key] = attempt + 1
                    const delay = event.code === GOING_AWAY && attempt === 0 ? 0 : reconnectDelay(attempt)
                    timers[key] = setTimeout(function () {
                        delete timers[key]
                        sendMsg(key, 'reconnect', null)
                    }, delay)
                }
            },
            'error': function () {
            }
        }

        bindEventListeners(ws, eventListeners[key])
    }

    function wsSend([key, message]) {
        const websocket = websockets[key]
        if (websocket && websocket.readyState === OPEN) {
            websocket.send(JSON.stringify(message))
        } else {
            queueMessage(key, message)
        }
    }

    function wsPing(key) {
        if (websockets[key] && websockets[key].readyState === OPEN) {
            websockets[key].send('ping')
        }
    }

    function wsClose(key) {
        if (websockets[key]) {
            if (eventListeners[key]) {
                unbindEventListeners(websockets[key], eventListeners[key])
                delete eventListeners[key]
            }
            websockets[key].close()
            delete websockets[key]
        }

        clearTimeout(timers[key])
        delete timers[key]
        delete attempts[key]
        delete queues[key]
    }

    function queueMessage(key, message) {
        if (!queues[key]) {
            queues[key] = []
        }
        queues[key].push(message)
    }
}

// Exponential, from 500 ms to 30 s, each delay somewhere between half and the
// whole of its step, so that clients dropped together do not return together.
function reconnectDelay(attempt) {
    const step = Math.min(MAX_DELAY, MIN_DELAY * Math.pow(2, attempt))
    return Math.max(MIN_DELAY, step / 2 + Math.random() * step / 2)
}

// The API URL may be relative or http(s); the socket takes ws(s).
function toWebSocketUrl(url) {
    const wsUrl = new URL(url, window.location.href)
    wsUrl.protocol = wsUrl.protocol === 'https:' ? 'wss:' : 'ws:'
    return wsUrl.toString()
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
