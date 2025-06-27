module Wizard.Api.TypeHints exposing (fetchTypeHints)

import Json.Decode as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Api.Request as Request exposing (ToMsg)
import Wizard.Api.Models.Event as Event exposing (Event)
import Wizard.Api.Models.TypeHint as TypeHint exposing (TypeHint)
import Wizard.Common.AppState as AppState exposing (AppState)


fetchTypeHints : AppState -> Maybe String -> List Event -> String -> String -> ToMsg (List TypeHint) msg -> Cmd msg
fetchTypeHints appState mbPackageId events questionUuid q =
    let
        strToMaybe str =
            if String.isEmpty str then
                Nothing

            else
                Just str

        data =
            E.object
                [ ( "packageId", E.maybe E.string <| Maybe.andThen strToMaybe mbPackageId )
                , ( "events", E.list Event.encode events )
                , ( "questionUuid", E.string questionUuid )
                , ( "q", E.string q )
                ]
    in
    Request.post (AppState.toServerInfo appState) "/typehints" (D.list TypeHint.decoder) data
