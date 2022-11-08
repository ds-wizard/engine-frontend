module Wizard.DocumentTemplateEditors.Editor.Components.TemplateEditor.DocumentTemplateForm exposing
    ( DocumentTemplateForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Data.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Shared.Data.DocumentTemplateDraft.DocumentTemplateFormatDraft as DocumentTemplateFormatDraft exposing (DocumentTemplateFormatDraft)
import Shared.Data.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Shared.Data.EditableConfig.EditableKnowledgeModelConfig.EditablePublicKnowledgeModelsConfig.AllowedPackage as AllowedPackage exposing (AllowedPackage)
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V
import String exposing (fromInt)
import Version


type alias DocumentTemplateForm =
    { name : String
    , description : String
    , license : String
    , readme : String
    , allowedPackages : List AllowedPackage
    , formats : List DocumentTemplateFormatDraft
    , templateId : String
    , versionMajor : Int
    , versionMinor : Int
    , versionPatch : Int
    }


initEmpty : Form FormError DocumentTemplateForm
initEmpty =
    Form.initial [] validation


init : DocumentTemplateDraftDetail -> Form FormError DocumentTemplateForm
init draft =
    let
        initialFields =
            [ ( "name", Field.string draft.name )
            , ( "description", Field.string draft.description )
            , ( "license", Field.string draft.license )
            , ( "readme", Field.string draft.readme )
            , ( "allowedPackages", Field.list (List.map AllowedPackage.init draft.allowedPackages) )
            , ( "formats", Field.list (List.map DocumentTemplateFormatDraft.field draft.formats) )
            , ( "templateId", Field.string draft.templateId )
            , ( "versionMajor", Field.string (String.fromInt (Version.getMajor draft.version)) )
            , ( "versionMinor", Field.string (String.fromInt (Version.getMinor draft.version)) )
            , ( "versionPatch", Field.string (String.fromInt (Version.getPatch draft.version)) )
            ]
    in
    Form.initial initialFields validation


validation : Validation FormError DocumentTemplateForm
validation =
    V.succeed DocumentTemplateForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "description" V.optionalString)
        |> V.andMap (V.field "license" V.optionalString)
        |> V.andMap (V.field "readme" V.optionalString)
        |> V.andMap (V.field "allowedPackages" (V.list AllowedPackage.validation))
        |> V.andMap (V.field "formats" (V.list DocumentTemplateFormatDraft.validation))
        |> V.andMap (V.field "templateId" V.string)
        |> V.andMap (V.field "versionMajor" V.versionNumber)
        |> V.andMap (V.field "versionMinor" V.versionNumber)
        |> V.andMap (V.field "versionPatch" V.versionNumber)


encode : DocumentTemplatePhase -> DocumentTemplateForm -> E.Value
encode phase form =
    let
        version =
            String.join "." <| List.map fromInt [ form.versionMajor, form.versionMinor, form.versionPatch ]
    in
    E.object
        [ ( "name", E.string form.name )
        , ( "description", E.string form.description )
        , ( "license", E.string form.license )
        , ( "readme", E.string form.readme )
        , ( "allowedPackages", E.list AllowedPackage.encode form.allowedPackages )
        , ( "formats", E.list DocumentTemplateFormatDraft.encode form.formats )
        , ( "templateId", E.string form.templateId )
        , ( "version", E.string version )
        , ( "phase", DocumentTemplatePhase.encode phase )
        ]
