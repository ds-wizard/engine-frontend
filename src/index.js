'use strict'


// initialize elm app
var program = require('./elm/Main.elm')

var registerChartPorts = require('./ports/chart')
var registerImportPorts = require('./ports/import')
var registerPageUnloadPorts = require('./ports/page-unload')
var registerScrollPorts = require('./ports/scroll')
var registerSessionPorts = require('./ports/session')


function getConfigValue(key, defaultValue) {
    if (window.dsw && window.dsw[key]) {
        return window.dsw[key]
    }
    return defaultValue
}


var app = program.Elm.Main.init({
    node: document.body,
    flags: {
        seed: Math.floor(Math.random() * 0xFFFFFFFF),
        session: JSON.parse(localStorage.session || null),
        apiUrl: getConfigValue('apiUrl', 'http://localhost:3000'),
        appTitle: getConfigValue('appTitle', 'Data Stewardship Wizard'),
        appTitleShort: getConfigValue('appTitleShort', 'DS Wizard'),
        welcomeWarning: getConfigValue('welcomeWarning', null),
        welcomeInfo: getConfigValue('welcomeInfo', null)
    }
})

registerChartPorts(app)
registerImportPorts(app)
registerPageUnloadPorts(app)
registerScrollPorts(app)
registerSessionPorts(app)
