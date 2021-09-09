module Wizard.Settings.Projects.Models exposing
    ( Model
    , initialModel
    )

import Wizard.Settings.Common.Forms.EditableQuestionnairesConfigForm as EditableQuestionnairesConfigForm exposing (EditableQuestionnairesConfigForm)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model EditableQuestionnairesConfigForm


initialModel : Model
initialModel =
    GenericModel.initialModel EditableQuestionnairesConfigForm.initEmpty
