module Common.Api.TypeHints exposing (fetchTypeHints)

import Common.Api exposing (ToMsg, jwtFetch)
import Common.AppState exposing (AppState)
import FormEngine.Model exposing (TypeHint, decodeTypeHint)
import Json.Decode as Decode
import Json.Encode as Encode


fetchTypeHints : String -> String -> String -> AppState -> ToMsg (List TypeHint) msg -> Cmd msg
fetchTypeHints packageId questionUuid q =
    let
        data =
            Encode.object
                [ ( "packageId", Encode.string packageId )
                , ( "questionUuid", Encode.string questionUuid )
                , ( "q", Encode.string q )
                ]
    in
    jwtFetch "/typehints" (Decode.list decodeTypeHint) data
