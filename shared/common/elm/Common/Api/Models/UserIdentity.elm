module Common.Api.Models.UserIdentity exposing
    ( UserIdentity
    , compare
    , decoder
    , visibleIdentifier
    )

import Common.Api.Models.AuthServiceProviderButtonStyle as AuthServiceProviderButtonStyle exposing (AuthServiceProviderButtonStyle)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)


type alias UserIdentity =
    { uuid : Uuid
    , externalId : String
    , externalLabel : Maybe String
    , providerUuid : Uuid
    , providerName : String
    , providerStyle : AuthServiceProviderButtonStyle
    , createdAt : Time.Posix
    }


decoder : Decoder UserIdentity
decoder =
    D.succeed UserIdentity
        |> D.required "uuid" Uuid.decoder
        |> D.required "externalId" D.string
        |> D.required "externalLabel" (D.maybe D.string)
        |> D.required "providerUuid" Uuid.decoder
        |> D.required "providerName" D.string
        |> D.required "providerStyle" AuthServiceProviderButtonStyle.decoder
        |> D.required "createdAt" D.datetime


visibleIdentifier : UserIdentity -> String
visibleIdentifier identity =
    Maybe.withDefault identity.externalId identity.externalLabel


compare : UserIdentity -> UserIdentity -> Order
compare i1 i2 =
    if i1.providerName == i2.providerName then
        Basics.compare (visibleIdentifier i1) (visibleIdentifier i2)

    else
        Basics.compare i1.providerName i2.providerName
