module Common.Menu.Models exposing (BuildInfo, Model, buildInfoDecoder, clientBuildInfo, initialModel)

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (required)


type alias Model =
    { reportIssueOpen : Bool
    , profileMenuDropdownState : Dropdown.State
    , aboutOpen : Bool
    , apiBuildInfo : ActionResult BuildInfo
    }


type alias BuildInfo =
    { version : String
    , builtAt : String
    }


initialModel : Model
initialModel =
    { reportIssueOpen = False
    , profileMenuDropdownState = Dropdown.initialState
    , aboutOpen = False
    , apiBuildInfo = Unset
    }


clientBuildInfo : BuildInfo
clientBuildInfo =
    { version = "{version}"
    , builtAt = "{builtAt}"
    }


buildInfoDecoder : Decoder BuildInfo
buildInfoDecoder =
    Decode.succeed BuildInfo
        |> required "version" Decode.string
        |> required "builtAt" Decode.string
