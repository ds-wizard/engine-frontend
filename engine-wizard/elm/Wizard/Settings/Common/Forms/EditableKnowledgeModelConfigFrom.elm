module Wizard.Settings.Common.Forms.EditableKnowledgeModelConfigFrom exposing
    ( EditableKnowledgeModelConfigForm
    , init
    , initEmpty
    , toEditableKnowledgeModelConfig
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Shared.Data.EditableConfig.EditableKnowledgeModelConfig exposing (EditableKnowledgeModelConfig)
import Shared.Data.EditableConfig.EditableKnowledgeModelConfig.EditablePublicKnowledgeModelsConfig.AllowedPackage exposing (AllowedPackage)
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V


type alias EditableKnowledgeModelConfigForm =
    { publicEnabled : Bool
    , publicPackages : List AllowedPackage
    }


initEmpty : Form FormError EditableKnowledgeModelConfigForm
initEmpty =
    Form.initial [] validation


init : EditableKnowledgeModelConfig -> Form FormError EditableKnowledgeModelConfigForm
init config =
    let
        packageToFields allowedPackage =
            Field.group
                [ ( "orgId", Field.string (Maybe.withDefault "" allowedPackage.orgId) )
                , ( "kmId", Field.string (Maybe.withDefault "" allowedPackage.kmId) )
                , ( "minVersion", Field.string (Maybe.withDefault "" allowedPackage.minVersion) )
                , ( "maxVersion", Field.string (Maybe.withDefault "" allowedPackage.maxVersion) )
                ]

        fields =
            [ ( "publicEnabled", Field.bool config.public.enabled )
            , ( "publicPackages", Field.list (List.map packageToFields config.public.packages) )
            ]
    in
    Form.initial fields validation


validation : Validation FormError EditableKnowledgeModelConfigForm
validation =
    let
        validatePackage =
            V.succeed AllowedPackage
                |> V.andMap (V.field "orgId" V.maybeString)
                |> V.andMap (V.field "kmId" V.maybeString)
                |> V.andMap (V.field "minVersion" V.maybeString)
                |> V.andMap (V.field "maxVersion" V.maybeString)
    in
    V.succeed EditableKnowledgeModelConfigForm
        |> V.andMap (V.field "publicEnabled" V.bool)
        |> V.andMap (V.field "publicPackages" (V.list validatePackage))


toEditableKnowledgeModelConfig : EditableKnowledgeModelConfigForm -> EditableKnowledgeModelConfig
toEditableKnowledgeModelConfig form =
    { public =
        { enabled = form.publicEnabled
        , packages = form.publicPackages
        }
    }
