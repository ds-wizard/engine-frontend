module Wizard.Pages.Settings.KnowledgeModels.Models exposing
    ( Model
    , initialModel
    )

import Wizard.Pages.Settings.Common.Forms.EditableKnowledgeModelConfigFrom as EditableKnowledgeModelConfigForm exposing (EditableKnowledgeModelConfigForm)
import Wizard.Pages.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model EditableKnowledgeModelConfigForm


initialModel : Model
initialModel =
    GenericModel.initialModel EditableKnowledgeModelConfigForm.initEmpty
