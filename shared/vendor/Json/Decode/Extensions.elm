module Json.Decode.Extensions exposing (listIgnoreInvalid, valueAsString)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


valueAsString : Decoder String
valueAsString =
    D.value
        |> D.map (E.encode 0)


listIgnoreInvalid : Decoder a -> Decoder (List a)
listIgnoreInvalid itemDecoder =
    D.list
        (D.oneOf
            [ D.map Just itemDecoder
            , D.succeed Nothing
            ]
        )
        |> D.map (List.filterMap identity)
