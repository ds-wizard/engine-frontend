module Common.Api.TypeHints exposing
    ( fetchTypeHints
    , testTypeHints
    )

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.TypeHint as TypeHint exposing (TypeHint)
import Common.Api.Models.TypeHintRequest as TypeHintRequest exposing (TypeHintRequest)
import Common.Api.Models.TypeHintTestResponse as TypeHintTestResponse exposing (TypeHintTestResponse)
import Common.Api.Request as Request exposing (ToMsg)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Uuid exposing (Uuid)


fetchTypeHints : Context ctx -> TypeHintRequest -> ToMsg (List TypeHint) msg -> Cmd msg
fetchTypeHints appState typeHintRequest =
    let
        data =
            TypeHintRequest.encode typeHintRequest
    in
    Request.post (Context.toServerInfo appState) "/type-hints" (D.list TypeHint.decoder) data


testTypeHints : Context ctx -> Uuid -> String -> String -> Dict String String -> ToMsg TypeHintTestResponse msg -> Cmd msg
testTypeHints appState kmEditorUuid integrationUuid q variables =
    let
        data =
            E.object
                [ ( "knowledgeModelEditorUuid", Uuid.encode kmEditorUuid )
                , ( "integrationUuid", E.string integrationUuid )
                , ( "q", E.string q )
                , ( "variables", E.dict identity E.string variables )
                ]
    in
    Request.post (Context.toServerInfo appState) "/type-hints/test" TypeHintTestResponse.decoder data
