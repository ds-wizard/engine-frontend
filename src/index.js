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

var app = Elm.Main.embed(mountNode)
