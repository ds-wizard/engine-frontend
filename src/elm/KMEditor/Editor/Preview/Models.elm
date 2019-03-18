module KMEditor.Editor.Preview.Models exposing
    ( Model
    , addTag
    , initialModel
    , removeTag
    , selectAllTags
    , selectNoneTags
    )

import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, Level, filterKnowledgModelWithTags)


type alias Model =
    { questionnaireModel : Common.Questionnaire.Models.Model
    , knowledgeModel : KnowledgeModel
    , tags : List String
    }


initialModel : KnowledgeModel -> Model
initialModel km =
    { questionnaireModel = createQuestionnaireModel km
    , knowledgeModel = km
    , tags = []
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
            createQuestionnaireModel <| filterKnowledgModelWithTags tags model.knowledgeModel
    in
    { model
        | questionnaireModel = questionnaireModel
        , tags = tags
    }


createQuestionnaireModel : KnowledgeModel -> Common.Questionnaire.Models.Model
createQuestionnaireModel km =
    Common.Questionnaire.Models.initialModel
        { uuid = ""
        , name = ""
        , private = True
        , package =
            { name = ""
            , id = ""
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
