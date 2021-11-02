module Wizard.Settings.LookAndFeel.Msgs exposing (Msg(..))

import Wizard.Settings.Generic.Msgs as GenericMsg
import Wizard.Settings.LookAndFeel.LogoUploadModal as LogoUploadModal


type Msg
    = GenericMsg GenericMsg.Msg
    | LogoUploadModalMsg LogoUploadModal.Msg
