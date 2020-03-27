module Wizard.Common.Config.AuthServiceConfig exposing
    ( AuthServiceConfig
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias AuthServiceConfig =
    { id : String
    , name : String
    , style : Maybe ButtonStyleConfig
    }


type alias ButtonStyleConfig =
    { background : Maybe String
    , color : Maybe String
    , icon : Maybe String
    }


decoder : Decoder AuthServiceConfig
decoder =
    D.succeed AuthServiceConfig
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.optional "style" (D.maybe buttonStyleConfigDecoder) Nothing


buttonStyleConfigDecoder : Decoder ButtonStyleConfig
buttonStyleConfigDecoder =
    D.succeed ButtonStyleConfig
        |> D.optional "background" (D.maybe D.string) Nothing
        |> D.optional "color" (D.maybe D.string) Nothing
        |> D.optional "icon" (D.maybe D.string) Nothing
