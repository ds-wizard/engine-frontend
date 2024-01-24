module Shared.Api.TypeHints exposing (fetchTypeHints)

import Json.Decode as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtOrHttpFetch)
import Shared.Data.Event as Event exposing (Event)
import Shared.Data.TypeHint as TypeHint exposing (TypeHint)


fetchTypeHints : Maybe String -> List Event -> String -> String -> AbstractAppState a -> ToMsg (List TypeHint) msg -> Cmd msg
fetchTypeHints mbPackageId events questionUuid q =
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
    jwtOrHttpFetch "/typehints" (D.list TypeHint.decoder) data
