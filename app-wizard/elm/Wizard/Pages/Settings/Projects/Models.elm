module Wizard.Pages.Settings.Projects.Models exposing
    ( Model
    , initialModel
    )

import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Common.Forms.EditableQuestionnairesConfigForm as EditableQuestionnairesConfigForm exposing (EditableQuestionnairesConfigForm)
import Wizard.Pages.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model EditableQuestionnairesConfigForm


initialModel : AppState -> Model
initialModel appState =
    GenericModel.initialModel (EditableQuestionnairesConfigForm.initEmpty appState)
