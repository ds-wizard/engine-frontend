port module Common.Api.WebSocket exposing
    ( RawMsg
    , WebSocket
    , WebSocketMsg(..)
    , close
    , init
    , listen
    , open
    , ping
    , receive
    , schedulePing
    , send
    , url
    )

import Common.Api.Request as Request exposing (ServerInfo)
import Common.Data.WebSockets.WebSocketServerAction as WebSocketServerAction exposing (WebSocketServerAction)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Time


url : ServerInfo -> String -> String
url serverInfo originalUrl =
    String.replace "http" "ws" <| Request.authorizedUrl serverInfo originalUrl


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
init wsUrl =
    { url = wsUrl }


open : WebSocket -> Cmd msg
open ws =
    wsOpen ws.url


close : WebSocket -> Cmd msg
close ws =
    wsClose ws.url


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
send ws value =
    wsSend ( ws.url, value )


ping : WebSocket -> Cmd msg
ping ws =
    wsPing ws.url


listen : (E.Value -> msg) -> Sub msg
listen =
    wsMessage


schedulePing : msg -> Sub msg
schedulePing msg =
    Time.every (30 * 1000) (always msg)


port wsOpen : String -> Cmd msg


port wsClose : String -> Cmd msg


port wsSend : ( String, E.Value ) -> Cmd msg


port wsPing : String -> Cmd msg


port wsMessage : (E.Value -> msg) -> Sub msg
