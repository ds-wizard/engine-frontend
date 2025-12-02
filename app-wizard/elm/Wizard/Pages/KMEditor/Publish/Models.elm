module Wizard.Pages.KMEditor.Publish.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Wizard.Api.Models.KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Wizard.Pages.KMEditor.Common.KnowledgeModelEditorPublishForm as KnowledgeModelEditorPublishForm exposing (KnowledgeModelEditorPublishForm)


type alias Model =
    { kmEditor : ActionResult KnowledgeModelEditorDetail
    , publishingKnowledgeModelEditor : ActionResult String
    , form : Form FormError KnowledgeModelEditorPublishForm
    }


initialModel : Model
initialModel =
    { kmEditor = Loading
    , publishingKnowledgeModelEditor = Unset
    , form = KnowledgeModelEditorPublishForm.init
    }
