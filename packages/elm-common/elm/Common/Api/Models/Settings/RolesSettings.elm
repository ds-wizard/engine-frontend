module Common.Api.Models.Settings.RolesSettings exposing
    ( RolesSettings
    , decoder
    , encode
    , section
    )

{-| What is set about roles: the one a new person gets.
-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)


type alias RolesSettings =
    { defaultRoleUuid : Uuid }


section : { name : String, decoder : Decoder RolesSettings, encode : RolesSettings -> E.Value }
section =
    { name = "roles", decoder = decoder, encode = encode }


decoder : Decoder RolesSettings
decoder =
    D.succeed RolesSettings
        |> D.required "defaultRoleUuid" Uuid.decoder


encode : RolesSettings -> E.Value
encode settings =
    E.object
        [ ( "defaultRoleUuid", Uuid.encode settings.defaultRoleUuid ) ]
