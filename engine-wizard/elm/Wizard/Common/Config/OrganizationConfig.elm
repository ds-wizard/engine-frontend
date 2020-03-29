module Wizard.Common.Config.OrganizationConfig exposing
    ( OrganizationConfig
    , decoder
    , default
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias OrganizationConfig =
    { name : String
    , organizationId : String
    , affiliations : List String
    }


default : OrganizationConfig
default =
    { name = ""
    , organizationId = ""
    , affiliations = []
    }



-- JSON


decoder : Decoder OrganizationConfig
decoder =
    D.succeed OrganizationConfig
        |> D.required "name" D.string
        |> D.required "organizationId" D.string
        |> D.required "affiliations" (D.list D.string)


encode : OrganizationConfig -> E.Value
encode config =
    E.object
        [ ( "name", E.string config.name )
        , ( "organizationId", E.string config.organizationId )
        , ( "affiliations", E.list E.string config.affiliations )
        ]
