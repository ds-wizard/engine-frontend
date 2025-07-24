module Wizard.Api.TypeHints exposing (fetchTypeHints, testTypeHints)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Api.Request as Request exposing (ToMsg)
import Uuid exposing (Uuid)
import Wizard.Api.Models.Event as Event exposing (Event)
import Wizard.Api.Models.KnowledgeModel.Integration.KeyValuePair as KeyValuePair exposing (KeyValuePair)
import Wizard.Api.Models.TypeHint as TypeHint exposing (TypeHint)
import Wizard.Api.Models.TypeHintTestResponse as TypeHintTestResponse exposing (TypeHintTestResponse)
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


testTypeHints : AppState -> Uuid -> String -> String -> List KeyValuePair -> ToMsg TypeHintTestResponse msg -> Cmd msg
testTypeHints appState branchUuid integrationUuid q variables =
    --let
    --    variablesDict =
    --        List.map KeyValuePair.toTuple variables
    --            |> Dict.fromList
    --
    --    data =
    --        E.object
    --            [ ( "branchUuid", Uuid.encode branchUuid )
    --            , ( "integrationUuid", E.string integrationUuid )
    --            , ( "q", E.string q )
    --            , ( "variables", E.dict identity E.string variablesDict )
    --            ]
    --in
    --Request.post (AppState.toServerInfo appState) "/typehints/test" TypeHintTestResponse.decoder data
    let
        serverInfo =
            { apiUrl = "http://localhost:3000"
            , token = Nothing
            }
    in
    Request.get serverInfo "/typehints/remote-error" TypeHintTestResponse.decoder
