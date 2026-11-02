module Common.Api.Models.WebSockets.WebSocketServerAction exposing
    ( WebSocketServerAction(..)
    , decoder
    )

import Common.Api.ServerError as ServerError exposing (ServerError)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type WebSocketServerAction a
    = Success a
    | Error ServerError


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
                |> D.required "data" errorDecoder

        _ ->
            D.fail <| "Unknown WebSocketServerAction: " ++ actionType


{-| The error as the HTTP API sends it, or at least its code: an error that
cannot be read in full is still an error.
-}
errorDecoder : Decoder ServerError
errorDecoder =
    D.oneOf
        [ ServerError.decoder
        , D.at [ "message", "code" ] D.string
            |> D.map (\code -> ServerError.UserSimpleError { code = code, params = [] })
        ]
