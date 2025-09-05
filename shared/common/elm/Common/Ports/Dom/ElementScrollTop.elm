module Common.Ports.Dom.ElementScrollTop exposing
    ( ElementScrollTop
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias ElementScrollTop =
    { selector : String
    , scrollTop : Int
    }


encode : ElementScrollTop -> E.Value
encode data =
    E.object
        [ ( "selector", E.string data.selector )
        , ( "scrollTop", E.int data.scrollTop )
        ]


decoder : Decoder ElementScrollTop
decoder =
    D.succeed ElementScrollTop
        |> D.required "selector" D.string
        |> D.required "scrollTop" D.int
