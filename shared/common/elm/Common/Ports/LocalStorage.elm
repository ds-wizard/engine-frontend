port module Common.Ports.LocalStorage exposing
    ( Item
    , decodeItemValue
    , getAndRemoveItem
    , getItem
    , gotItem
    , gotItemRaw
    , itemDecoder
    , removeItem
    , setItem
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias Item a =
    { key : String
    , value : a
    }


itemDecoder : Decoder a -> Decoder (Item a)
itemDecoder valueDecoder =
    D.succeed Item
        |> D.required "key" D.string
        |> D.required "value" valueDecoder


decodeItemValue : Decoder a -> D.Value -> Result D.Error a
decodeItemValue valueDecoder =
    D.decodeValue (D.field "value" valueDecoder)


getItem : String -> Cmd msg
getItem =
    localStorageGetItem


getAndRemoveItem : String -> Cmd msg
getAndRemoveItem =
    localStorageGetAndRemoveItem


gotItem : Decoder a -> (Result D.Error (Item a) -> msg) -> Sub msg
gotItem valueDecoder toMsg =
    localStorageGotItem (toMsg << D.decodeValue (itemDecoder valueDecoder))


gotItemRaw : (E.Value -> msg) -> Sub msg
gotItemRaw =
    localStorageGotItem


setItem : String -> E.Value -> Cmd msg
setItem key value =
    localStorageSetItem
        (E.object
            [ ( "key", E.string key )
            , ( "value", value )
            ]
        )


removeItem : String -> Cmd msg
removeItem =
    localStorageRemoveItem


port localStorageGetItem : String -> Cmd msg


port localStorageSetItem : E.Value -> Cmd msg


port localStorageGotItem : (E.Value -> msg) -> Sub msg


port localStorageGetAndRemoveItem : String -> Cmd msg


port localStorageRemoveItem : String -> Cmd msg
