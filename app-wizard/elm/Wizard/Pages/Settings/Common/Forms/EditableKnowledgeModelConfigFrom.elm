module Wizard.Pages.Settings.Common.Forms.EditableKnowledgeModelConfigFrom exposing
    ( EditableKnowledgeModelConfigForm
    , init
    , initEmpty
    , toEditableKnowledgeModelConfig
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Wizard.Api.Models.EditableConfig.EditableKnowledgeModelConfig exposing (EditableKnowledgeModelConfig)
import Wizard.Api.Models.EditableConfig.EditableKnowledgeModelConfig.EditablePublicKnowledgeModelsConfig.AllowedPackage as AllowedPackage exposing (AllowedPackage)


type alias EditableKnowledgeModelConfigForm =
    { publicEnabled : Bool
    , publicPackages : List AllowedPackage
    , integrationConfig : String
    }


initEmpty : Form FormError EditableKnowledgeModelConfigForm
initEmpty =
    Form.initial [] validation


init : EditableKnowledgeModelConfig -> Form FormError EditableKnowledgeModelConfigForm
init config =
    let
        fields =
            [ ( "publicEnabled", Field.bool config.public.enabled )
            , ( "publicPackages", Field.list (List.map AllowedPackage.init config.public.packages) )
            , ( "integrationConfig", Field.string config.integrationConfig )
            ]
    in
    Form.initial fields validation


validation : Validation FormError EditableKnowledgeModelConfigForm
validation =
    V.succeed EditableKnowledgeModelConfigForm
        |> V.andMap (V.field "publicEnabled" V.bool)
        |> V.andMap (V.field "publicPackages" (V.list AllowedPackage.validation))
        |> V.andMap (V.field "integrationConfig" V.optionalString)


toEditableKnowledgeModelConfig : EditableKnowledgeModelConfigForm -> EditableKnowledgeModelConfig
toEditableKnowledgeModelConfig form =
    { integrationConfig = form.integrationConfig
    , public =
        { enabled = form.publicEnabled
        , packages = form.publicPackages
        }
    }
