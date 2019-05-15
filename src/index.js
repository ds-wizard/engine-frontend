'use strict'


// initialize elm app
var program = require('./elm/Main.elm')

var registerChartPorts = require('./ports/chart')
var registerImportPorts = require('./ports/import')
var registerPageUnloadPorts = require('./ports/page-unload')
var registerScrollPorts = require('./ports/scroll')
var registerSessionPorts = require('./ports/session')


function getApiUrl() {
    if (window.dsw && window.dsw['apiUrl']) return window.dsw['apiUrl']
    return 'http://localhost:3000'
}


function loadApp(config) {
    var app = program.Elm.Main.init({
        node: document.body,
        flags: {
            seed: Math.floor(Math.random() * 0xFFFFFFFF),
            session: JSON.parse(localStorage.session || null),
            apiUrl: getApiUrl(),
            config: config
        }
    })

    registerChartPorts(app)
    registerImportPorts(app)
    registerPageUnloadPorts(app)
    registerScrollPorts(app)
    registerSessionPorts(app)
}


window.onload = function () {
    var callbackMethod = 'callback'
    var script = document.createElement('script')
    script.src = getApiUrl() + '/configuration?callback=callback'
    document.body.appendChild(script)

    window[callbackMethod] = function (config) {
        delete window[callbackMethod]
        document.body.removeChild(script)
        loadApp(config)
    }
}
