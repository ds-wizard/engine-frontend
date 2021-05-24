module Wizard.KMEditor.Editor.Preview.Models exposing
    ( Model
    , addTag
    , initialModel
    , removeTag
    , selectAllTags
    , selectNoneTags
    )

import Shared.Data.Event exposing (Event)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.Package as Package
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire


type alias Model =
    { questionnaireModel : Questionnaire.Model
    , knowledgeModel : KnowledgeModel
    , tags : List String
    , packageId : String
    , metrics : List Metric
    , levels : List Level
    , events : List Event
    }


initialModel : AppState -> KnowledgeModel -> List Metric -> List Level -> List Event -> String -> Model
initialModel appState km metrics levels events packageId =
    let
        questionnaire =
            createQuestionnaireDetail packageId km
    in
    { questionnaireModel = Questionnaire.init appState questionnaire
    , knowledgeModel = km
    , tags = []
    , packageId = packageId
    , metrics = metrics
    , levels = levels
    , events = events
    }


addTag : String -> Model -> Model
addTag uuid model =
    let
        tags =
            uuid :: model.tags
    in
    createFilteredQuestionnaireModel tags model


removeTag : String -> Model -> Model
removeTag uuid model =
    let
        tags =
            List.filter (\t -> t /= uuid) model.tags
    in
    createFilteredQuestionnaireModel tags model


selectAllTags : Model -> Model
selectAllTags model =
    let
        tags =
            List.map .uuid (KnowledgeModel.getTags model.knowledgeModel)
    in
    createFilteredQuestionnaireModel tags model


selectNoneTags : Model -> Model
selectNoneTags model =
    createFilteredQuestionnaireModel [] model


createFilteredQuestionnaireModel : List String -> Model -> Model
createFilteredQuestionnaireModel tags model =
    let
        questionnaireDetail =
            createQuestionnaireDetail
                model.packageId
                (KnowledgeModel.filterWithTags tags model.knowledgeModel)

        questionnaireModel =
            model.questionnaireModel
    in
    { model
        | questionnaireModel = { questionnaireModel | questionnaire = questionnaireDetail }
        , tags = tags
    }


createQuestionnaireDetail : String -> KnowledgeModel -> QuestionnaireDetail
createQuestionnaireDetail packageId km =
    let
        package =
            Package.dummy
    in
    QuestionnaireDetail.createQuestionnaireDetail { package | id = packageId } km
