module Common.Api.Models.User exposing
    ( User
    , compare
    , decoder
    , fullName
    )

import Common.Api.Models.RoleInfo as RoleInfo exposing (RoleInfo)
import Common.Api.Models.UserIdentity as UserIdentity exposing (UserIdentity)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)


type alias User =
    { uuid : Uuid
    , email : String
    , firstName : String
    , lastName : String
    , imageUrl : Maybe String
    , affiliation : Maybe String
    , role : RoleInfo
    , active : Bool
    , externalIdentities : List UserIdentity
    , createdAt : Maybe Time.Posix
    , updatedAt : Time.Posix
    , lastVisitedAt : Maybe Time.Posix
    , consentAcceptedAt : Maybe Time.Posix
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
        |> D.required "role" RoleInfo.decoder
        |> D.required "active" D.bool
        |> D.optional "externalIdentities" (D.list UserIdentity.decoder) []
        |> D.optional "createdAt" (D.maybe D.datetime) Nothing
        |> D.required "updatedAt" D.datetime
        |> D.optional "lastVisitedAt" (D.maybe D.datetime) Nothing
        |> D.optional "consentAcceptedAt" (D.maybe D.datetime) Nothing


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
