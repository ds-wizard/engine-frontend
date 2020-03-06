module WizardResearch.Common.Provisioning.DefaultLocale exposing (..)

import Dict exposing (Dict)


locale : Dict String String
locale =
    Dict.fromList
        [ ( "WizardResearch.appName", "Research client" )
        ]
