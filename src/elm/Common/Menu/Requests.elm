module Common.Menu.Requests exposing (..)

import Common.Menu.Models exposing (BuildInfo, buildInfoDecoder)
import Http
import Requests exposing (apiUrl)


getBuildInfo : Http.Request BuildInfo
getBuildInfo =
    Http.get (apiUrl "") buildInfoDecoder
