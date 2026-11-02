module Registry.Api.Models.Translation exposing
    ( Translation
    , decoder
    )

{-| A translation the registry publishes, of the user interface or of the
mails: a language and the version of the strings it was made for. It has no
page of its own, only its PO file.
-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Translation =
    { code : String
    , name : String
    , version : String
    }


decoder : Decoder Translation
decoder =
    D.succeed Translation
        |> D.required "code" D.string
        |> D.required "name" D.string
        |> D.required "version" D.string
