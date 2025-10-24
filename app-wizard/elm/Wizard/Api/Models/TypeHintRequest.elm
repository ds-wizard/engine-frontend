module Wizard.Api.Models.TypeHintRequest exposing
    ( TypeHintRequest
    , encode
    , fromKmEditorIntegration
    , fromKmEditorQuestion
    , fromQuestionnaire
    )

import Json.Encode as E
import Uuid exposing (Uuid)


type TypeHintRequest
    = KnowledgeModelEditorIntegration KnowledgeModelEditorIntegrationData
    | KnowledgeModelEditorQuestion KnowledgeModelEditorQuestionData
    | Questionnaire QuestionnaireData


fromKmEditorIntegration : Uuid -> Uuid -> TypeHintRequest
fromKmEditorIntegration kmEditorUuid integrationUuid =
    KnowledgeModelEditorIntegration { knowledgeModelEditorUuid = kmEditorUuid, integrationUuid = integrationUuid }


fromKmEditorQuestion : Uuid -> Uuid -> String -> TypeHintRequest
fromKmEditorQuestion kmEditorUuid questionUuid q =
    KnowledgeModelEditorQuestion { knowledgeModelEditorUuid = kmEditorUuid, questionUuid = questionUuid, q = q }


fromQuestionnaire : Uuid -> Uuid -> String -> TypeHintRequest
fromQuestionnaire questionnaireUuid questionUuid q =
    Questionnaire { questionnaireUuid = questionnaireUuid, questionUuid = questionUuid, q = q }


type alias KnowledgeModelEditorIntegrationData =
    { knowledgeModelEditorUuid : Uuid
    , integrationUuid : Uuid
    }


type alias KnowledgeModelEditorQuestionData =
    { knowledgeModelEditorUuid : Uuid
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
        KnowledgeModelEditorIntegration data ->
            E.object
                [ ( "requestType", E.string "KnowledgeModelEditorIntegrationTypeHintRequest" )
                , ( "knowledgeModelEditorUuid", Uuid.encode data.knowledgeModelEditorUuid )
                , ( "integrationUuid", Uuid.encode data.integrationUuid )
                ]

        KnowledgeModelEditorQuestion data ->
            E.object
                [ ( "requestType", E.string "KnowledgeModelEditorQuestionTypeHintRequest" )
                , ( "knowledgeModelEditorUuid", Uuid.encode data.knowledgeModelEditorUuid )
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
