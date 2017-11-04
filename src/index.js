'use strict'

require('font-awesome-sass-loader')

// get index.html to the build
require('./index.html')

// get images to the build
require('./img/elixir-logo.png')
require('./img/elixir-logo@2x.png')

// initialize elm app
var Elm = require('./elm/Main.elm')
var mountNode = document.getElementById('main')

var app = Elm.Main.embed(mountNode, {
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
