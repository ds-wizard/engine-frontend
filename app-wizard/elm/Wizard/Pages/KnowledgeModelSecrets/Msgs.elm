module Wizard.Pages.KnowledgeModelSecrets.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)
import Form
import Wizard.Api.Models.KnowledgeModelSecret exposing (KnowledgeModelSecret)


type Msg
    = GetKnowledgeModelSecretsCompleted (Result ApiError (List KnowledgeModelSecret))
    | SetCreateModalOpen Bool
    | CreateFormMsg Form.Msg
    | PostKnowledgeModelSecretCompleted (Result ApiError ())
    | SetEditSecret (Maybe KnowledgeModelSecret)
    | EditFormMsg Form.Msg
    | PutKnowledgeModelSecretCompleted (Result ApiError ())
    | SetDeleteSecret (Maybe KnowledgeModelSecret)
    | DeleteKnowledgeModelSecret
    | DeleteKnowledgeModelSecretCompleted (Result ApiError ())
