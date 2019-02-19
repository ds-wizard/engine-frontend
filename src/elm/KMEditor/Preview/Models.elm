module KMEditor.Preview.Models exposing
    ( Model
    , initialModel
    , setKnowledgeModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, Level)


type alias Model =
    { branchUuid : String
    , questionnaireModel : ActionResult Common.Questionnaire.Models.Model
    , levels : ActionResult (List Level)
    }


initialModel : String -> Model
initialModel branchUuid =
    { branchUuid = branchUuid
    , questionnaireModel = Loading
    , levels = Loading
    }


setKnowledgeModel : KnowledgeModel -> Model -> Model
setKnowledgeModel km model =
    { model
        | questionnaireModel =
            createQuestionnaireModel km
                |> Common.Questionnaire.Models.initialModel
                |> Success
    }


createQuestionnaireModel : KnowledgeModel -> QuestionnaireDetail
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
