module Shared.Data.EditableConfig.EditableKnowledgeModelConfig.EditablePublicKnowledgeModelsConfig.AllowedPackage exposing
    ( AllowedPackage
    , decoder
    , encode
    , init
    , validation
    )

import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V


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


init : AllowedPackage -> Field
init allowedPackage =
    Field.group
        [ ( "orgId", Field.string (Maybe.withDefault "" allowedPackage.orgId) )
        , ( "kmId", Field.string (Maybe.withDefault "" allowedPackage.kmId) )
        , ( "minVersion", Field.string (Maybe.withDefault "" allowedPackage.minVersion) )
        , ( "maxVersion", Field.string (Maybe.withDefault "" allowedPackage.maxVersion) )
        ]


validation : Validation FormError AllowedPackage
validation =
    V.succeed AllowedPackage
        |> V.andMap (V.field "orgId" V.maybeString)
        |> V.andMap (V.field "kmId" V.maybeString)
        |> V.andMap (V.field "minVersion" V.maybeString)
        |> V.andMap (V.field "maxVersion" V.maybeString)
