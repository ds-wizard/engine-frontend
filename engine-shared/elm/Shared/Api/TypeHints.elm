module Shared.Api.TypeHints exposing (fetchTypeHints)

import Json.Decode as Decode
import Json.Encode as Encode
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtFetch)
import Shared.Data.Event as Event exposing (Event)
import Shared.Data.TypeHint as TypeHint exposing (TypeHint)


fetchTypeHints : Maybe String -> List Event -> String -> String -> AbstractAppState a -> ToMsg (List TypeHint) msg -> Cmd msg
fetchTypeHints mbPackageId events questionUuid q =
    let
        data =
            Encode.object
                [ ( "packageId", mbPackageId |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
                , ( "events", Encode.list Event.encode events )
                , ( "questionUuid", Encode.string questionUuid )
                , ( "q", Encode.string q )
                ]
    in
    jwtFetch "/typehints" (Decode.list TypeHint.decoder) data
