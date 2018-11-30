'use strict';


// initialize elm app
var program = require('./elm/Main.elm');
var registerImportPorts = require('./ports/import');
var registerSessionPorts = require('./ports/session');
var registerScrollPorts = require('./ports/scroll');

var app = program.Elm.Main.init({
    node: document.body,
    flags: {
        seed: Math.floor(Math.random() * 0xFFFFFFFF),
        session: JSON.parse(localStorage.session || null)
    }
});

registerSessionPorts(app);
registerImportPorts(app);
registerScrollPorts(app);
