module Wizard.Api.TypeHints exposing
    ( fetchTypeHints
    , fetchTypeHintsLegacy
    , testTypeHints
    )

import Common.Api.Request as Request exposing (ToMsg)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Json.Encode.Extra as E
import Uuid exposing (Uuid)
import Wizard.Api.Models.Event as Event exposing (Event)
import Wizard.Api.Models.TypeHint as TypeHint exposing (TypeHint)
import Wizard.Api.Models.TypeHintLegacy as TypeHintLegacy exposing (TypeHintLegacy)
import Wizard.Api.Models.TypeHintRequest as TypeHintRequest exposing (TypeHintRequest)
import Wizard.Api.Models.TypeHintTestResponse as TypeHintTestResponse exposing (TypeHintTestResponse)
import Wizard.Data.AppState as AppState exposing (AppState)


fetchTypeHints : AppState -> TypeHintRequest -> ToMsg (List TypeHint) msg -> Cmd msg
fetchTypeHints appState typeHintRequest =
    let
        data =
            TypeHintRequest.encode typeHintRequest
    in
    Request.post (AppState.toServerInfo appState) "/type-hints" (D.list TypeHint.decoder) data


fetchTypeHintsLegacy : AppState -> Maybe String -> List Event -> String -> String -> ToMsg (List TypeHintLegacy) msg -> Cmd msg
fetchTypeHintsLegacy appState mbPackageId events questionUuid q =
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
    Request.post (AppState.toServerInfo appState) "/type-hints-legacy" (D.list TypeHintLegacy.decoder) data


testTypeHints : AppState -> Uuid -> String -> String -> Dict String String -> ToMsg TypeHintTestResponse msg -> Cmd msg
testTypeHints appState branchUuid integrationUuid q variables =
    let
        data =
            E.object
                [ ( "branchUuid", Uuid.encode branchUuid )
                , ( "integrationUuid", E.string integrationUuid )
                , ( "q", E.string q )
                , ( "variables", E.dict identity E.string variables )
                ]
    in
    Request.post (AppState.toServerInfo appState) "/type-hints/test" TypeHintTestResponse.decoder data
