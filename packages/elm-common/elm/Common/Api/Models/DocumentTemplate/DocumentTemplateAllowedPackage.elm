module Common.Api.Models.DocumentTemplate.DocumentTemplateAllowedPackage exposing
    ( DocumentTemplateAllowedPackage
    , decoder
    , encode
    , init
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias DocumentTemplateAllowedPackage =
    { id : Maybe String
    , minVersion : Maybe String
    , maxVersion : Maybe String
    }


decoder : Decoder DocumentTemplateAllowedPackage
decoder =
    D.succeed DocumentTemplateAllowedPackage
        |> D.optional "id" (D.maybe D.string) Nothing
        |> D.required "minVersion" (D.maybe D.string)
        |> D.required "maxVersion" (D.maybe D.string)


encode : DocumentTemplateAllowedPackage -> E.Value
encode allowedPackage =
    E.object
        [ ( "id", E.maybe E.string allowedPackage.id )
        , ( "minVersion", E.maybe E.string allowedPackage.minVersion )
        , ( "maxVersion", E.maybe E.string allowedPackage.maxVersion )
        ]


init : DocumentTemplateAllowedPackage -> Field
init allowedPackage =
    Field.group
        [ ( "id", Field.string (Maybe.withDefault "" allowedPackage.id) )
        , ( "minVersion", Field.string (Maybe.withDefault "" allowedPackage.minVersion) )
        , ( "maxVersion", Field.string (Maybe.withDefault "" allowedPackage.maxVersion) )
        ]


validation : Validation FormError DocumentTemplateAllowedPackage
validation =
    V.succeed DocumentTemplateAllowedPackage
        |> V.andMap (V.field "id" V.maybeString)
        |> V.andMap (V.field "minVersion" V.maybeString)
        |> V.andMap (V.field "maxVersion" V.maybeString)
