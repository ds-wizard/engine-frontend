module Wizard.Settings.Common.EditableOrganizationConfig exposing
    ( EditableOrganizationConfig
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias EditableOrganizationConfig =
    { uuid : String
    , name : String
    , organizationId : String
    }


decoder : Decoder EditableOrganizationConfig
decoder =
    D.succeed EditableOrganizationConfig
        |> D.required "uuid" D.string
        |> D.required "name" D.string
        |> D.required "organizationId" D.string


encode : EditableOrganizationConfig -> E.Value
encode config =
    E.object
        [ ( "uuid", E.string config.uuid )
        , ( "name", E.string config.name )
        , ( "organizationId", E.string config.name )
        ]
