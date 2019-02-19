module KMEditor.Preview.Models exposing
    ( Model
    , addTag
    , initialModel
    , removeTag
    , selectAllTags
    , selectNoneTags
    , setKnowledgeModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, Level, filterKnowledgModelWithTags)


type alias Model =
    { branchUuid : String
    , questionnaireModel : ActionResult Common.Questionnaire.Models.Model
    , levels : ActionResult (List Level)
    , knowledgeModel : Maybe KnowledgeModel
    , tags : List String
    }


initialModel : String -> Model
initialModel branchUuid =
    { branchUuid = branchUuid
    , questionnaireModel = Loading
    , levels = Loading
    , knowledgeModel = Nothing
    , tags = []
    }


setKnowledgeModel : KnowledgeModel -> Model -> Model
setKnowledgeModel km model =
    { model
        | questionnaireModel = createQuestionnaireModel km
        , knowledgeModel = Just km
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
            model.knowledgeModel
                |> Maybe.map .tags
                |> Maybe.withDefault []
                |> List.map .uuid
    in
    createFilteredQuestionnaireModel tags model


selectNoneTags : Model -> Model
selectNoneTags model =
    createFilteredQuestionnaireModel [] model


createFilteredQuestionnaireModel : List String -> Model -> Model
createFilteredQuestionnaireModel tags model =
    let
        questionnaireModel =
            model.knowledgeModel
                |> Maybe.map (filterKnowledgModelWithTags tags >> createQuestionnaireModel)
                |> Maybe.withDefault (Error "")
    in
    { model
        | questionnaireModel = questionnaireModel
        , tags = tags
    }


createQuestionnaireModel : KnowledgeModel -> ActionResult Common.Questionnaire.Models.Model
createQuestionnaireModel km =
    { uuid = ""
    , name = ""
    , package =
        { name = ""
        , id = ""
        , organizationId = ""
        , kmId = ""
        , version = ""
        , description = ""
        }
    , knowledgeModel = km
    , replies = []
    , level = 0
    }
        |> Common.Questionnaire.Models.initialModel
        |> Success
