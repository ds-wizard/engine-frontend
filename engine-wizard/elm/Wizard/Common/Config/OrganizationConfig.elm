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
    , description : String
    , affiliations : List String
    }


default : OrganizationConfig
default =
    { name = ""
    , organizationId = ""
    , description = ""
    , affiliations = []
    }



-- JSON


decoder : Decoder OrganizationConfig
decoder =
    D.succeed OrganizationConfig
        |> D.required "name" D.string
        |> D.required "organizationId" D.string
        |> D.required "description" D.string
        |> D.required "affiliations" (D.list D.string)


encode : OrganizationConfig -> E.Value
encode config =
    E.object
        [ ( "name", E.string config.name )
        , ( "organizationId", E.string config.organizationId )
        , ( "description", E.string config.description )
        , ( "affiliations", E.list E.string config.affiliations )
        ]
