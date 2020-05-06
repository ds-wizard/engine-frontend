module Wizard.Common.UserInfo exposing
    ( UserInfo
    , decoder
    , isAdmin
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Users.Common.Role as Role


type alias UserInfo =
    { uuid : String
    , email : String
    , firstName : String
    , lastName : String
    , role : String
    , imageUrl : Maybe String
    }


decoder : Decoder UserInfo
decoder =
    D.succeed UserInfo
        |> D.required "uuid" D.string
        |> D.required "email" D.string
        |> D.required "firstName" D.string
        |> D.required "lastName" D.string
        |> D.required "role" D.string
        |> D.required "imageUrl" (D.maybe D.string)


isAdmin : Maybe UserInfo -> Bool
isAdmin =
    Maybe.map (.role >> (==) Role.admin) >> Maybe.withDefault False
