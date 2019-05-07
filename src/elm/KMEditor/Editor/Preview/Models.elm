module KMEditor.Editor.Preview.Models exposing
    ( Model
    , addTag
    , initialModel
    , removeTag
    , selectAllTags
    , selectNoneTags
    )

import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, Level, Metric, filterKnowledgModelWithTags)
import KMEditor.Common.Models.Events exposing (Event)


type alias Model =
    { questionnaireModel : Common.Questionnaire.Models.Model
    , knowledgeModel : KnowledgeModel
    , tags : List String
    , packageId : String
    }


initialModel : KnowledgeModel -> List Metric -> List Event -> String -> Model
initialModel km metrics events packageId =
    { questionnaireModel = createQuestionnaireModel packageId km metrics events
    , knowledgeModel = km
    , tags = []
    , packageId = packageId
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
            List.map .uuid model.knowledgeModel.tags
    in
    createFilteredQuestionnaireModel tags model


selectNoneTags : Model -> Model
selectNoneTags model =
    createFilteredQuestionnaireModel [] model


createFilteredQuestionnaireModel : List String -> Model -> Model
createFilteredQuestionnaireModel tags model =
    let
        questionnaireModel =
            createQuestionnaireModel
                model.packageId
                (filterKnowledgModelWithTags tags model.knowledgeModel)
                model.questionnaireModel.metrics
                model.questionnaireModel.events
    in
    { model
        | questionnaireModel = questionnaireModel
        , tags = tags
    }


createQuestionnaireModel : String -> KnowledgeModel -> List Metric -> List Event -> Common.Questionnaire.Models.Model
createQuestionnaireModel packageId km =
    Common.Questionnaire.Models.initialModel
        { uuid = ""
        , name = ""
        , private = True
        , package =
            { name = ""
            , id = packageId
            , organizationId = ""
            , kmId = ""
            , version = ""
            , description = ""
            , metamodelVersion = 0
            }
        , knowledgeModel = km
        , replies = []
        , level = 0
        }
