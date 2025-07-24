module Wizard.Api.Models.TypeHintRequest exposing
    ( TypeHintRequest
    , encode
    , fromBranchIntegration
    , fromBranchQuestion
    , fromQuestionnaire
    )

import Json.Encode as E
import Uuid exposing (Uuid)


type TypeHintRequest
    = BranchIntegration BranchIntegrationData
    | BranchQuestion BranchQuestionData
    | Questionnaire QuestionnaireData


fromBranchIntegration : Uuid -> Uuid -> TypeHintRequest
fromBranchIntegration branchUuid integrationUuid =
    BranchIntegration { branchUuid = branchUuid, integrationUuid = integrationUuid }


fromBranchQuestion : Uuid -> Uuid -> String -> TypeHintRequest
fromBranchQuestion branchUuid questionUuid q =
    BranchQuestion { branchUuid = branchUuid, questionUuid = questionUuid, q = q }


fromQuestionnaire : Uuid -> Uuid -> String -> TypeHintRequest
fromQuestionnaire questionnaireUuid questionUuid q =
    Questionnaire { questionnaireUuid = questionnaireUuid, questionUuid = questionUuid, q = q }


type alias BranchIntegrationData =
    { branchUuid : Uuid
    , integrationUuid : Uuid
    }


type alias BranchQuestionData =
    { branchUuid : Uuid
    , questionUuid : Uuid
    , q : String
    }


type alias QuestionnaireData =
    { questionnaireUuid : Uuid
    , questionUuid : Uuid
    , q : String
    }


encode : TypeHintRequest -> E.Value
encode typeHintRequest =
    case typeHintRequest of
        BranchIntegration data ->
            E.object
                [ ( "requestType", E.string "BranchIntegrationTypeHintRequest" )
                , ( "branchUuid", Uuid.encode data.branchUuid )
                , ( "integrationUuid", Uuid.encode data.integrationUuid )
                ]

        BranchQuestion data ->
            E.object
                [ ( "requestType", E.string "BranchQuestionTypeHintRequest" )
                , ( "branchUuid", Uuid.encode data.branchUuid )
                , ( "questionUuid", Uuid.encode data.questionUuid )
                , ( "q", E.string data.q )
                ]

        Questionnaire data ->
            E.object
                [ ( "requestType", E.string "QuestionnaireTypeHintRequest" )
                , ( "questionnaireUuid", Uuid.encode data.questionnaireUuid )
                , ( "questionUuid", Uuid.encode data.questionUuid )
                , ( "q", E.string data.q )
                ]
