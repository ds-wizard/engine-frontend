module WizardResearch.Common.Provisioning.DefaultLocale exposing (..)

import Dict exposing (Dict)
import Shared.Common.Provisioning.DefaultLocale as SharedLocale


locale : Dict String String
locale =
    Dict.fromList
        (SharedLocale.locale
            ++ []
        )
