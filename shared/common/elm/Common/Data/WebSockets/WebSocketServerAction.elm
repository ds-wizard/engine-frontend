module Common.Data.WebSockets.WebSocketServerAction exposing
    ( WebSocketServerAction(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type WebSocketServerAction a
    = Success a
    | Error String


decoder : Decoder a -> Decoder (WebSocketServerAction a)
decoder messageDecoder =
    D.field "type" D.string
        |> D.andThen (decoderByType messageDecoder)


decoderByType : Decoder a -> String -> Decoder (WebSocketServerAction a)
decoderByType messageDecoder actionType =
    case actionType of
        "Success_ServerAction" ->
            D.succeed Success
                |> D.required "data" messageDecoder

        "Error_ServerAction" ->
            D.succeed Error
                |> D.requiredAt [ "data", "message", "code" ] D.string

        _ ->
            D.fail <| "Unknown WebSocketServerAction: " ++ actionType
