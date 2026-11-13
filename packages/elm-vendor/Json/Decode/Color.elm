module Json.Decode.Color exposing (lenientHexColor)

import Color exposing (Color)
import Color.Convert as Convert
import Json.Decode as D exposing (Decoder)


{-| An optional color that is never a reason to fail: `null`, or a string that
is not a hex color, is `Nothing`.
-}
lenientHexColor : Decoder (Maybe Color)
lenientHexColor =
    D.nullable D.string
        |> D.map (Maybe.andThen (Result.toMaybe << Convert.hexToColor))
