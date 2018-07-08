'use strict'

// get images to the build
require('./img/crc-logo.png')


// initialize elm app
var Elm = require('./elm/Main.elm')

var app = Elm.Main.fullscreen({
    seed: Math.floor(Math.random() * 0xFFFFFFFF),
    session: JSON.parse(localStorage.session || null)
})

// initialize ports to use local storage
app.ports.storeSession.subscribe(function(session) {
    localStorage.session = JSON.stringify(session)
})

app.ports.clearSession.subscribe(function() {
    localStorage.removeItem('session')
})

window.addEventListener("storage", function(event) {
    if (event.storageArea === localStorage && event.key === "session") {
        app.ports.onSessionChange.send(event.newValue)
    }
})
