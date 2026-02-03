module Wizard.Api.Models.UserLocale exposing
    ( UserLocale
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Uuid exposing (Uuid)


type alias UserLocale =
    { uuid : Maybe Uuid }


decoder : Decoder UserLocale
decoder =
    D.succeed UserLocale
        |> D.required "uuid" (D.maybe Uuid.decoder)


encode : UserLocale -> E.Value
encode userLocale =
    E.object
        [ ( "uuid", E.maybe Uuid.encode userLocale.uuid )
        ]
