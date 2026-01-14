module Wizard.Pages.DocumentTemplateEditors.Editor.Components.TemplateEditor.DocumentTemplateForm exposing
    ( DocumentTemplateForm
    , encode
    , fillFormat
    , fillStep
    , init
    , initEmpty
    , isFormatEmpty
    , isStepEmpty
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Dict
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Maybe.Extra as Maybe
import String exposing (fromInt)
import Version
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormatStep exposing (DocumentTemplateFormatStep)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Wizard.Api.Models.DocumentTemplateDraft.DocumentTemplateFormatDraft as DocumentTemplateFormatDraft exposing (DocumentTemplateFormatDraft)
import Wizard.Api.Models.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Wizard.Api.Models.EditableConfig.EditableKnowledgeModelConfig.EditablePublicKnowledgeModelsConfig.AllowedKnowledgeModelPackage as AllowedKnowledgeModelPackage exposing (AllowedKnowledgeModelPackage)
import Wizard.Data.AppState exposing (AppState)


type alias DocumentTemplateForm =
    { name : String
    , description : String
    , license : String
    , readme : String
    , allowedPackages : List AllowedKnowledgeModelPackage
    , formats : List DocumentTemplateFormatDraft
    , templateId : String
    , versionMajor : Int
    , versionMinor : Int
    , versionPatch : Int
    }


initEmpty : AppState -> Form FormError DocumentTemplateForm
initEmpty appState =
    Form.initial [] (validation appState)


init : AppState -> DocumentTemplateDraftDetail -> Form FormError DocumentTemplateForm
init appState draft =
    let
        initialFields =
            [ ( "name", Field.string draft.name )
            , ( "description", Field.string draft.description )
            , ( "license", Field.string draft.license )
            , ( "readme", Field.string draft.readme )
            , ( "allowedPackages", Field.list (List.map AllowedKnowledgeModelPackage.init draft.allowedPackages) )
            , ( "formats", Field.list (List.map DocumentTemplateFormatDraft.field draft.formats) )
            , ( "templateId", Field.string draft.templateId )
            , ( "versionMajor", Field.string (String.fromInt (Version.getMajor draft.version)) )
            , ( "versionMinor", Field.string (String.fromInt (Version.getMinor draft.version)) )
            , ( "versionPatch", Field.string (String.fromInt (Version.getPatch draft.version)) )
            ]
    in
    Form.initial initialFields (validation appState)


validation : AppState -> Validation FormError DocumentTemplateForm
validation appState =
    V.succeed DocumentTemplateForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "description" V.optionalString)
        |> V.andMap (V.field "license" V.optionalString)
        |> V.andMap (V.field "readme" V.optionalString)
        |> V.andMap (V.field "allowedPackages" (V.list AllowedKnowledgeModelPackage.validation))
        |> V.andMap (V.field "formats" (V.list DocumentTemplateFormatDraft.validation))
        |> V.andMap (V.field "templateId" (V.documentTemplateId appState))
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
        , ( "allowedPackages", E.list AllowedKnowledgeModelPackage.encode form.allowedPackages )
        , ( "formats", E.list DocumentTemplateFormatDraft.encode form.formats )
        , ( "templateId", E.string form.templateId )
        , ( "version", E.string version )
        , ( "phase", DocumentTemplatePhase.encode phase )
        ]


isFormatEmpty : Int -> Form FormError DocumentTemplateForm -> Bool
isFormatEmpty index form =
    let
        isFieldEmpty field =
            Maybe.isNothing <| (Form.getFieldAsString ("formats." ++ fromInt index ++ "." ++ field) form).value

        isStepsEmpty =
            List.isEmpty <| Form.getListIndexes ("formats." ++ fromInt index ++ ".steps") form
    in
    List.all identity
        [ isFieldEmpty "name"
        , isFieldEmpty "icon"
        , isStepsEmpty
        ]


fillFormat : AppState -> Int -> DocumentTemplateFormatDraft -> Form FormError DocumentTemplateForm -> Form FormError DocumentTemplateForm
fillFormat appState index format form =
    let
        toFormMsg field value =
            Form.Input ("formats." ++ fromInt index ++ "." ++ field) Form.Text (Field.String value)

        applyFormMsg formMsg =
            Form.update (validation appState) formMsg

        formatMsg =
            [ toFormMsg "name" format.name
            , toFormMsg "icon" format.icon
            ]

        appendStepsMsgs =
            List.repeat (List.length format.steps) (Form.Append ("formats." ++ fromInt index ++ ".steps"))

        form_ =
            List.foldl applyFormMsg form (formatMsg ++ appendStepsMsgs)
    in
    List.indexedMap (fillStep appState index) format.steps
        |> List.foldl (\a f -> a f) form_


isStepEmpty : Int -> Int -> Form FormError DocumentTemplateForm -> Bool
isStepEmpty formatIndex stepIndex form =
    let
        isFieldEmpty field =
            Maybe.isNothing <| (Form.getFieldAsString ("formats." ++ fromInt formatIndex ++ ".steps." ++ fromInt stepIndex ++ "." ++ field) form).value

        isOptionsEmpty =
            List.isEmpty <| Form.getListIndexes ("formats." ++ fromInt formatIndex ++ ".steps." ++ fromInt stepIndex ++ ".options") form
    in
    List.all identity
        [ isFieldEmpty "name"
        , isOptionsEmpty
        ]


fillStep : AppState -> Int -> Int -> DocumentTemplateFormatStep -> Form FormError DocumentTemplateForm -> Form FormError DocumentTemplateForm
fillStep appState formatIndex stepIndex step form =
    let
        toFormMsg field value =
            Form.Input ("formats." ++ fromInt formatIndex ++ ".steps." ++ fromInt stepIndex ++ "." ++ field) Form.Text (Field.String value)

        toOptionMsg i ( key, value ) =
            [ Form.Append ("formats." ++ fromInt formatIndex ++ ".steps." ++ fromInt stepIndex ++ ".options")
            , toFormMsg ("options." ++ fromInt i ++ ".key") key
            , toFormMsg ("options." ++ fromInt i ++ ".value") value
            ]

        applyFormMsg formMsg =
            Form.update (validation appState) formMsg

        formMsgs =
            [ toFormMsg "name" step.name
            ]

        optionsMsgs =
            List.concat <|
                List.indexedMap toOptionMsg (Dict.toList step.options)
    in
    List.foldl applyFormMsg form (formMsgs ++ optionsMsgs)
