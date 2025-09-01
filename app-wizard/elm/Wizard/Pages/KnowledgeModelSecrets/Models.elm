module Wizard.Pages.KnowledgeModelSecrets.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult)
import Form exposing (Form)
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Api.Models.KnowledgeModelSecret exposing (KnowledgeModelSecret)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KnowledgeModelSecrets.Forms.KnowledgeModelSecretForm as KnowledgeModelSecretForm exposing (KnowledgeModelSecretForm)


type alias Model =
    { kmSecrets : ActionResult (List KnowledgeModelSecret)
    , createModalOpen : Bool
    , createSecretForm : Form FormError KnowledgeModelSecretForm
    , creatingSecret : ActionResult String
    , editSecret : Maybe KnowledgeModelSecret
    , editSecretForm : Form FormError KnowledgeModelSecretForm
    , editingSecret : ActionResult String
    , deleteSecret : Maybe KnowledgeModelSecret
    , deletingSecret : ActionResult String
    }


initialModel : AppState -> Model
initialModel appState =
    { kmSecrets = ActionResult.Loading
    , createModalOpen = False
    , createSecretForm = KnowledgeModelSecretForm.initEmpty appState
    , creatingSecret = ActionResult.Unset
    , editSecret = Nothing
    , editSecretForm = KnowledgeModelSecretForm.initEmpty appState
    , editingSecret = ActionResult.Unset
    , deleteSecret = Nothing
    , deletingSecret = ActionResult.Unset
    }
