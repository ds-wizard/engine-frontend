module Wizard.Pages.Projects.Common.QuestionnaireCreateForm exposing
    ( QuestionnaireCreateForm
    , ValidationMode(..)
    , encodeFromPackage
    , encodeFromTemplate
    , init
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Json.Encode.Extra as E
import Uuid exposing (Uuid)
import Wizard.Api.Models.Questionnaire.QuestionnaireSharing as QuestionnaireSharing
import Wizard.Api.Models.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility
import Wizard.Api.Models.QuestionnairePermission as QuestionnairePermission exposing (QuestionnairePermission)
import Wizard.Data.AppState exposing (AppState)


type alias QuestionnaireCreateForm =
    { name : String
    , visibilityEnabled : Bool
    , visibilityPermission : QuestionnairePermission
    , sharingEnabled : Bool
    , sharingPermission : QuestionnairePermission
    , knowledgeModelPackageId : String
    , templateId : String
    }


init : AppState -> ValidationMode -> Maybe Uuid -> Maybe String -> Form FormError QuestionnaireCreateForm
init appState validationMode selectedTemplateUuid selectedPackageId =
    let
        toField fieldName mbFieldValue =
            case mbFieldValue of
                Just fieldValue ->
                    [ ( fieldName, Field.string fieldValue ) ]

                _ ->
                    []

        ( visibilityEnabled, visibilityPermission ) =
            QuestionnaireVisibility.toFormValues appState.config.questionnaire.questionnaireVisibility.defaultValue

        ( sharingEnabled, sharingPermission ) =
            QuestionnaireSharing.toFormValues appState.config.questionnaire.questionnaireSharing.defaultValue

        initials =
            [ ( "visibilityEnabled", Field.bool visibilityEnabled )
            , ( "visibilityPermission", QuestionnairePermission.field visibilityPermission )
            , ( "sharingEnabled", Field.bool sharingEnabled )
            , ( "sharingPermission", QuestionnairePermission.field sharingPermission )
            ]
                ++ toField "templateId" (Maybe.map Uuid.toString selectedTemplateUuid)
                ++ toField "knowledgeModelPackageId" selectedPackageId
    in
    Form.initial initials (validation validationMode)


type ValidationMode
    = TemplateValidationMode
    | PackageValidationMode


validation : ValidationMode -> Validation FormError QuestionnaireCreateForm
validation validationMode =
    let
        validationBase =
            V.succeed QuestionnaireCreateForm
                |> V.andMap (V.field "name" V.string)
                |> V.andMap (V.field "visibilityEnabled" V.bool)
                |> V.andMap (V.field "visibilityPermission" QuestionnairePermission.validation)
                |> V.andMap (V.field "sharingEnabled" V.bool)
                |> V.andMap (V.field "sharingPermission" QuestionnairePermission.validation)
    in
    case validationMode of
        TemplateValidationMode ->
            validationBase
                |> V.andMap (V.succeed "")
                |> V.andMap (V.field "templateId" V.string)

        PackageValidationMode ->
            validationBase
                |> V.andMap (V.field "knowledgeModelPackageId" V.string)
                |> V.andMap (V.succeed "")


encodeFromPackage : List String -> QuestionnaireCreateForm -> E.Value
encodeFromPackage questionTagUuids form =
    E.object
        [ ( "name", E.string form.name )
        , ( "knowledgeModelPackageId", E.string form.knowledgeModelPackageId )
        , ( "visibility", QuestionnaireVisibility.encode (QuestionnaireVisibility.fromFormValues form.visibilityEnabled form.visibilityPermission form.sharingEnabled form.sharingPermission) )
        , ( "sharing", QuestionnaireSharing.encode (QuestionnaireSharing.fromFormValues form.sharingEnabled form.sharingPermission) )
        , ( "questionTagUuids", E.list E.string questionTagUuids )
        , ( "templateId", E.maybe E.string Nothing )
        ]


encodeFromTemplate : QuestionnaireCreateForm -> E.Value
encodeFromTemplate form =
    E.object
        [ ( "name", E.string form.name )
        , ( "questionnaireUuid", E.string form.templateId )
        ]
