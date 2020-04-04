module WizardResearch.Common.Session exposing
    ( Session
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.UserInfo as UserInfo exposing (UserInfo)


type alias Session =
    { token : String
    , userInfo : UserInfo
    }


decoder : Decoder Session
decoder =
    D.succeed Session
        |> D.required "token" D.string
        |> D.required "userInfo" UserInfo.decoder


encode : Session -> E.Value
encode session =
    E.object
        [ ( "token", E.string session.token )
        , ( "userInfo", UserInfo.encode session.userInfo )
        ]
