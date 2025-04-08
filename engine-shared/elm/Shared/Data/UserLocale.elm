module Shared.Data.UserLocale exposing
    ( UserLocale
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias UserLocale =
    { id : Maybe String }


decoder : Decoder UserLocale
decoder =
    D.succeed UserLocale
        |> D.required "id" (D.maybe D.string)


encode : UserLocale -> E.Value
encode userLocale =
    E.object
        [ ( "id", E.maybe E.string userLocale.id )
        ]
