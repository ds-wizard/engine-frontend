module Shared.Data.User exposing
    ( SubmissionProps
    , User
    , compare
    , decoder
    , defaultGravatar
    , fullName
    , imageUrl
    , imageUrlOrGravatar
    , toUserInfo
    )

import Dict exposing (Dict)
import Gravatar
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.UserInfo exposing (UserInfo)
import Uuid exposing (Uuid)


type alias User =
    { uuid : Uuid
    , email : String
    , firstName : String
    , lastName : String
    , imageUrl : Maybe String
    , affiliation : Maybe String
    , role : String
    , permissions : List String
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
        |> D.required "uuid" Uuid.decoder
        |> D.required "email" D.string
        |> D.required "firstName" D.string
        |> D.required "lastName" D.string
        |> D.required "imageUrl" (D.maybe D.string)
        |> D.required "affiliation" (D.maybe D.string)
        |> D.required "role" D.string
        |> D.required "permissions" (D.list D.string)
        |> D.required "active" D.bool
        |> D.required "sources" (D.list D.string)
        |> D.optional "submissionProps" (D.list decodeSubmissionProps) []


decodeSubmissionProps : Decoder SubmissionProps
decodeSubmissionProps =
    D.succeed SubmissionProps
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "values" (D.dict D.string)


compare : { a | firstName : String, lastName : String } -> { a | firstName : String, lastName : String } -> Order
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
    , permissions = user.permissions
    , imageUrl = user.imageUrl
    }


imageUrl : { a | email : String, imageUrl : Maybe String } -> String
imageUrl user =
    let
        options =
            Gravatar.defaultOptions
                |> Gravatar.withDefault Gravatar.MysteryMan
    in
    Maybe.withDefault (Gravatar.url options user.email) user.imageUrl


imageUrlOrGravatar : { a | gravatarHash : String, imageUrl : Maybe String } -> String
imageUrlOrGravatar user =
    let
        options =
            Gravatar.defaultOptions
                |> Gravatar.withDefault Gravatar.MysteryMan
    in
    Maybe.withDefault (Gravatar.urlFromHash options user.gravatarHash) user.imageUrl


defaultGravatar : String
defaultGravatar =
    let
        options =
            Gravatar.defaultOptions
                |> Gravatar.withDefault Gravatar.MysteryMan
    in
    Gravatar.urlFromHash options ""
