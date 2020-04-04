module WizardResearch.Common.Provisioning.DefaultIconSet exposing (..)

import Dict exposing (Dict)
import Shared.Common.Provisioning.DefaultIconSet as SharedIconSet


iconSet : Dict String String
iconSet =
    Dict.fromList
        (SharedIconSet.iconSet
            ++ []
        )
