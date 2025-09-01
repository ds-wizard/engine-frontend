module Wizard.Settings.Projects.Models exposing
    ( Model
    , initialModel
    )

import Wizard.Common.AppState exposing (AppState)
import Wizard.Settings.Common.Forms.EditableQuestionnairesConfigForm as EditableQuestionnairesConfigForm exposing (EditableQuestionnairesConfigForm)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model EditableQuestionnairesConfigForm


initialModel : AppState -> Model
initialModel appState =
    GenericModel.initialModel (EditableQuestionnairesConfigForm.initEmpty appState)
