module Wizard.Settings.LookAndFeel.Models exposing
    ( Model
    , initialModel
    )

import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Wizard.Settings.Generic.Model as GenericModel
import Wizard.Settings.LookAndFeel.LogoUploadModal as LogoUploadModal


type alias Model =
    { genericModel : GenericModel.Model LookAndFeelConfig
    , logoUploadModalModel : LogoUploadModal.Model
    }


initialModel : Model
initialModel =
    { genericModel = GenericModel.initialModel LookAndFeelConfig.initEmptyForm
    , logoUploadModalModel = LogoUploadModal.initialModel
    }
