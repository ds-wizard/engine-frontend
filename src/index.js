'use strict'

require('font-awesome-sass-loader')
require('./index.html')

var Elm = require('./elm/Main.elm')
var mountNode = document.getElementById('main')

var app = Elm.Main.embed(mountNode)
