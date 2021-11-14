module Wizard.Settings.LookAndFeel.Models exposing
    ( Model
    , initialModel
    )

import Shared.Data.EditableConfig.EditableLookAndFeelConfig as EditableLookAndFeelConfig exposing (EditableLookAndFeelConfig)
import Wizard.Settings.Generic.Model as GenericModel
import Wizard.Settings.LookAndFeel.LogoUploadModal as LogoUploadModal


type alias Model =
    { genericModel : GenericModel.Model EditableLookAndFeelConfig
    , logoUploadModalModel : LogoUploadModal.Model
    }


initialModel : Model
initialModel =
    { genericModel = GenericModel.initialModel EditableLookAndFeelConfig.initEmptyForm
    , logoUploadModalModel = LogoUploadModal.initialModel
    }
