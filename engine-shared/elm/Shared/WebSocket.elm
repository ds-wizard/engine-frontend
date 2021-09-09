port module Shared.WebSocket exposing
    ( RawMsg
    , WebSocket
    , WebSocketMsg(..)
    , close
    , init
    , listen
    , open
    , ping
    , receive
    , send
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.WebSockets.WebSocketServerAction as WebSocketServerAction exposing (WebSocketServerAction)


type alias WebSocket =
    { url : String }


type alias RawMsg =
    E.Value


type WebSocketMsg a
    = Open
    | Message a
    | Close
    | Ignore


type alias WebSocketRawMsg =
    { url : String
    , type_ : String
    , data : E.Value
    }


decodeWebSocketRawMsg : Decoder WebSocketRawMsg
decodeWebSocketRawMsg =
    D.succeed WebSocketRawMsg
        |> D.required "url" D.string
        |> D.required "type" D.string
        |> D.required "data" D.value


init : String -> WebSocket
init url =
    { url = url }


open : WebSocket -> Cmd msg
open { url } =
    wsOpen url


close : WebSocket -> Cmd msg
close { url } =
    wsClose url


receive : Decoder a -> RawMsg -> WebSocket -> WebSocketMsg (WebSocketServerAction a)
receive messageDecoder value ws =
    case D.decodeValue decodeWebSocketRawMsg value of
        Ok rawMsg ->
            if rawMsg.url == ws.url then
                case rawMsg.type_ of
                    "open" ->
                        Open

                    "message" ->
                        case D.decodeValue (WebSocketServerAction.decoder messageDecoder) rawMsg.data of
                            Ok action ->
                                Message action

                            Err _ ->
                                Ignore

                    "close" ->
                        Close

                    _ ->
                        Ignore

            else
                Ignore

        Err _ ->
            Ignore


send : WebSocket -> E.Value -> Cmd msg
send { url } value =
    wsSend ( url, value )


ping : WebSocket -> Cmd msg
ping { url } =
    wsPing url


listen : (E.Value -> msg) -> Sub msg
listen =
    wsMessage


port wsOpen : String -> Cmd msg


port wsClose : String -> Cmd msg


port wsSend : ( String, E.Value ) -> Cmd msg


port wsPing : String -> Cmd msg


port wsMessage : (E.Value -> msg) -> Sub msg
