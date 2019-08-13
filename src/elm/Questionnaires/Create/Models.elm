module Questionnaires.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import KMEditor.Common.KnowledgeModel.KnowledgeModel exposing (KnowledgeModel)
import KnowledgeModels.Common.Package exposing (Package)
import Questionnaires.Common.QuestionnaireCreateForm as QuestionnaireCreateForm exposing (QuestionnaireCreateForm)


type alias Model =
    { packages : ActionResult (List Package)
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
    , form = QuestionnaireCreateForm.init selectedPackage
    , selectedPackage = selectedPackage
    , selectedTags = []
    , lastFetchedPreview = Nothing
    , knowledgeModelPreview = Unset
    }
