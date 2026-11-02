'use strict'

// The client. Everything but the program is in packages/elm-common.
// It has one workspace, which it never names, so what a signed-in user works
// with is read once, with the rest of the configuration.
require('../../packages/elm-common/js/app')(require('./elm/Dsw.elm').Elm.Dsw, {
    workspaceBootstrap: true
})
