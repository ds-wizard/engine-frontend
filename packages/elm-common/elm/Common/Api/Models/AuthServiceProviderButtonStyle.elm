module Common.Api.Models.AuthServiceProviderButtonStyle exposing
    ( AuthServiceProviderButtonStyle
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias AuthServiceProviderButtonStyle =
    { background : Maybe String
    , color : Maybe String
    , icon : Maybe String
    }


decoder : Decoder AuthServiceProviderButtonStyle
decoder =
    D.succeed AuthServiceProviderButtonStyle
        |> D.required "background" (D.maybe D.string)
        |> D.required "color" (D.maybe D.string)
        |> D.required "icon" (D.maybe D.string)


encode : AuthServiceProviderButtonStyle -> E.Value
encode style =
    E.object
        [ ( "background", E.maybe E.string style.background )
        , ( "color", E.maybe E.string style.color )
        , ( "icon", E.maybe E.string style.icon )
        ]
