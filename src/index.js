'use strict';

// get images to the build
require('./img/crc-logo.png');
require('./img/book-preview.png');


// initialize elm app
var program = require('./elm/Main.elm');
var registerImportPorts = require('./ports/import');
var registerSessionPorts = require('./ports/session');

var app = program.Elm.Main.init({
    node: document.body,
    flags: {
        seed: Math.floor(Math.random() * 0xFFFFFFFF),
        session: JSON.parse(localStorage.session || null)
    }
});

registerSessionPorts(app);
registerImportPorts(app);
