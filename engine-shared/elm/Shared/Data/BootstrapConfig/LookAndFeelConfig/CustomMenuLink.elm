module Shared.Data.BootstrapConfig.LookAndFeelConfig.CustomMenuLink exposing
    ( CustomMenuLink
    , decoder
    , encode
    , validation
    )

import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Form.FormError exposing (FormError)


type alias CustomMenuLink =
    { icon : String
    , title : String
    , url : String
    , newWindow : Bool
    }


decoder : Decoder CustomMenuLink
decoder =
    D.succeed CustomMenuLink
        |> D.required "icon" D.string
        |> D.required "title" D.string
        |> D.required "url" D.string
        |> D.required "newWindow" D.bool


encode : CustomMenuLink -> E.Value
encode link =
    E.object
        [ ( "icon", E.string link.icon )
        , ( "title", E.string link.title )
        , ( "url", E.string link.url )
        , ( "newWindow", E.bool link.newWindow )
        ]


validation : Validation FormError CustomMenuLink
validation =
    V.succeed CustomMenuLink
        |> V.andMap (V.field "icon" V.string)
        |> V.andMap (V.field "title" V.string)
        |> V.andMap (V.field "url" V.string)
        |> V.andMap (V.field "newWindow" V.bool)
