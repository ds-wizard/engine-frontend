module Json.Decode.Color exposing (hexColor)

import Color exposing (Color)
import Color.Convert as Convert
import Json.Decode as D exposing (Decoder)


hexColor : Decoder Color
hexColor =
    D.string
        |> D.andThen
            (\str ->
                case Convert.hexToColor str of
                    Ok color ->
                        D.succeed color

                    Err err ->
                        D.fail err
            )
