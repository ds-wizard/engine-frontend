module KMEditor.Editor.Preview.Models exposing
    ( Model
    , addTag
    , initialModel
    , removeTag
    , selectAllTags
    , selectNoneTags
    )

import Common.AppState exposing (AppState)
import Common.Questionnaire.Models
import KMEditor.Common.Events.Event exposing (Event)
import KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import KnowledgeModels.Common.Package as Package
import Questionnaires.Common.QuestionnaireAccessibility exposing (QuestionnaireAccessibility(..))


type alias Model =
    { questionnaireModel : Common.Questionnaire.Models.Model
    , knowledgeModel : KnowledgeModel
    , tags : List String
    , packageId : String
    }


initialModel : AppState -> KnowledgeModel -> List Metric -> List Event -> String -> Model
initialModel appState km metrics events packageId =
    { questionnaireModel = createQuestionnaireModel appState packageId km metrics events
    , knowledgeModel = km
    , tags = []
    , packageId = packageId
    }


addTag : AppState -> String -> Model -> Model
addTag appState uuid model =
    let
        tags =
            uuid :: model.tags
    in
    createFilteredQuestionnaireModel appState tags model


removeTag : AppState -> String -> Model -> Model
removeTag appState uuid model =
    let
        tags =
            List.filter (\t -> t /= uuid) model.tags
    in
    createFilteredQuestionnaireModel appState tags model


selectAllTags : AppState -> Model -> Model
selectAllTags appState model =
    let
        tags =
            List.map .uuid (KnowledgeModel.getTags model.knowledgeModel)
    in
    createFilteredQuestionnaireModel appState tags model


selectNoneTags : AppState -> Model -> Model
selectNoneTags appState model =
    createFilteredQuestionnaireModel appState [] model


createFilteredQuestionnaireModel : AppState -> List String -> Model -> Model
createFilteredQuestionnaireModel appState tags model =
    let
        questionnaireModel =
            createQuestionnaireModel
                appState
                model.packageId
                (KnowledgeModel.filterWithTags tags model.knowledgeModel)
                model.questionnaireModel.metrics
                model.questionnaireModel.events
    in
    { model
        | questionnaireModel = questionnaireModel
        , tags = tags
    }


createQuestionnaireModel : AppState -> String -> KnowledgeModel -> List Metric -> List Event -> Common.Questionnaire.Models.Model
createQuestionnaireModel appState packageId km =
    let
        package =
            Package.dummy
    in
    Common.Questionnaire.Models.initialModel
        appState
        { uuid = ""
        , name = ""
        , accessibility = PrivateQuestionnaire
        , ownerUuid = Nothing
        , package = { package | id = packageId }
        , knowledgeModel = km
        , replies = []
        , level = 0
        , selectedTagUuids = []
        , labels = []
        }
