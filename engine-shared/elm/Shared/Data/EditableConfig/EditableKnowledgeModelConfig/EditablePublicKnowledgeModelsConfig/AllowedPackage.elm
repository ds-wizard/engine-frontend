module Shared.Data.EditableConfig.EditableKnowledgeModelConfig.EditablePublicKnowledgeModelsConfig.AllowedPackage exposing
    ( AllowedPackage
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias AllowedPackage =
    { orgId : Maybe String
    , kmId : Maybe String
    , minVersion : Maybe String
    , maxVersion : Maybe String
    }


decoder : Decoder AllowedPackage
decoder =
    D.succeed AllowedPackage
        |> D.required "orgId" (D.maybe D.string)
        |> D.required "kmId" (D.maybe D.string)
        |> D.required "minVersion" (D.maybe D.string)
        |> D.required "maxVersion" (D.maybe D.string)


encode : AllowedPackage -> E.Value
encode allowedPackage =
    E.object
        [ ( "orgId", E.maybe E.string allowedPackage.orgId )
        , ( "kmId", E.maybe E.string allowedPackage.kmId )
        , ( "minVersion", E.maybe E.string allowedPackage.minVersion )
        , ( "maxVersion", E.maybe E.string allowedPackage.maxVersion )
        ]
