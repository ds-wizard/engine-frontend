module Wizard.KMEditor.Editor.Preview.Models exposing
    ( Model
    , addTag
    , initialModel
    , removeTag
    , selectAllTags
    , selectNoneTags
    )

import Dict
import Maybe.Extra as Maybe
import Shared.Data.Event exposing (Event)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.Package as Package
import Shared.Data.Questionnaire.QuestionnaireSharing exposing (QuestionnaireSharing(..))
import Shared.Data.Questionnaire.QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Uuid
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


initialModel : KnowledgeModel -> List Metric -> List Level -> List Event -> String -> Model
initialModel km metrics levels events packageId =
    let
        chapterUuid =
            Maybe.unwrap "" .uuid <|
                List.head (KnowledgeModel.getChapters km)

        questionnaire =
            createQuestionnaireDetail packageId km
    in
    { questionnaireModel = Questionnaire.setActiveChapterUuid chapterUuid <| Questionnaire.init questionnaire
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
    { uuid = Uuid.nil
    , name = ""
    , visibility = PrivateQuestionnaire
    , sharing = RestrictedQuestionnaire
    , ownerUuid = Nothing
    , package = { package | id = packageId }
    , knowledgeModel = km
    , replies = Dict.fromList []
    , level = 1
    , selectedTagUuids = []
    , templateId = Nothing
    , formatUuid = Nothing
    , format = Nothing
    , labels = Dict.fromList []
    }
