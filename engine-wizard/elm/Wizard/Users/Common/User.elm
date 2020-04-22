module Wizard.Users.Common.User exposing
    ( User
    , compare
    , decoder
    , fullName
    , imageUrl
    , toUserInfo
    )

import Dict exposing (Dict)
import Gravatar
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D
import Wizard.Common.UserInfo exposing (UserInfo)


type alias User =
    { uuid : String
    , email : String
    , firstName : String
    , lastName : String
    , imageUrl : Maybe String
    , affiliation : Maybe String
    , role : String
    , active : Bool
    , sources : List String
    , submissionProps : List SubmissionProps
    }


type alias SubmissionProps =
    { id : String
    , name : String
    , values : Dict String String
    }


decoder : Decoder User
decoder =
    D.succeed User
        |> D.required "uuid" D.string
        |> D.required "email" D.string
        |> D.required "firstName" D.string
        |> D.required "lastName" D.string
        |> D.required "imageUrl" (D.maybe D.string)
        |> D.required "affiliation" (D.maybe D.string)
        |> D.required "role" D.string
        |> D.required "active" D.bool
        |> D.required "sources" (D.list D.string)
        |> D.optional "submissionProps" (D.list decodeSubmissionProps) []


decodeSubmissionProps : Decoder SubmissionProps
decodeSubmissionProps =
    D.succeed SubmissionProps
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "values" (D.dict D.string)


compare : User -> User -> Order
compare u1 u2 =
    case Basics.compare (String.toLower u1.lastName) (String.toLower u2.lastName) of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            Basics.compare (String.toLower u1.firstName) (String.toLower u2.firstName)


fullName : { a | firstName : String, lastName : String } -> String
fullName user =
    user.firstName ++ " " ++ user.lastName


toUserInfo : User -> UserInfo
toUserInfo user =
    { uuid = user.uuid
    , email = user.email
    , firstName = user.firstName
    , lastName = user.lastName
    , role = user.role
    , imageUrl = user.imageUrl
    }


imageUrl : { a | email : String, imageUrl : Maybe String } -> String
imageUrl user =
    let
        options =
            Gravatar.defaultOptions
                |> Gravatar.withDefault Gravatar.Identicon
    in
    Maybe.withDefault (Gravatar.url options user.email) user.imageUrl
