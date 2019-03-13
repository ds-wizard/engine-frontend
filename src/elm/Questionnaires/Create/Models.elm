module Questionnaires.Create.Models exposing (Model, QuestionnaireCreateForm, encodeQuestionnaireCreateForm, initQuestionnaireCreateForm, initialModel, questionnaireCreateFormValidation)

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import Json.Encode as Encode exposing (..)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel)
import KnowledgeModels.Common.Models exposing (PackageDetail)


type alias Model =
    { packages : ActionResult (List PackageDetail)
    , savingQuestionnaire : ActionResult String
    , form : Form CustomFormError QuestionnaireCreateForm
    , selectedPackage : Maybe String
    , selectedTags : List String
    , lastFetchedPreview : Maybe String
    , knowledgeModelPreview : ActionResult KnowledgeModel
    }


initialModel : Maybe String -> Model
initialModel selectedPackage =
    { packages = Loading
    , savingQuestionnaire = Unset
    , form = initQuestionnaireCreateForm Nothing
    , selectedPackage = selectedPackage
    , selectedTags = []
    , lastFetchedPreview = Nothing
    , knowledgeModelPreview = Unset
    }


type alias QuestionnaireCreateForm =
    { name : String
    , packageId : String
    , private : Bool
    }


initQuestionnaireCreateForm : Maybe String -> Form CustomFormError QuestionnaireCreateForm
initQuestionnaireCreateForm selectedPackage =
    let
        initials =
            case selectedPackage of
                Just packageId ->
                    [ ( "packageId", Field.string packageId ) ]

                _ ->
                    []

        initialsWithPrivate =
            initials ++ [ ( "private", Field.bool True ) ]
    in
    Form.initial initialsWithPrivate questionnaireCreateFormValidation


questionnaireCreateFormValidation : Validation CustomFormError QuestionnaireCreateForm
questionnaireCreateFormValidation =
    Validate.map3 QuestionnaireCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "packageId" Validate.string)
        (Validate.field "private" Validate.bool)


encodeQuestionnaireCreateForm : List String -> QuestionnaireCreateForm -> Encode.Value
encodeQuestionnaireCreateForm tagUuids form =
    Encode.object
        [ ( "name", Encode.string form.name )
        , ( "packageId", Encode.string form.packageId )
        , ( "private", Encode.bool form.private )
        , ( "tagUuids", Encode.list Encode.string tagUuids )
        ]
