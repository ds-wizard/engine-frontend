module Wizard.Common.Api.Configs exposing (..)

import Json.Encode as E
import Wizard.Common.Api exposing (ToMsg, jwtGet, jwtPut)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Settings.Common.EditableAffiliationConfig as EditableAffiliationConfig exposing (EditableAffiliationConfig)
import Wizard.Settings.Common.EditableClientConfig as EditableClientConfig exposing (EditableClientConfig)
import Wizard.Settings.Common.EditableFeaturesConfig as EditableFeaturesConfig exposing (EditableFeaturesConfig)
import Wizard.Settings.Common.EditableInfoConfig as EditableInfoConfig exposing (EditableInfoConfig)
import Wizard.Settings.Common.EditableOrganizationConfig as EditableOrganizationConfig exposing (EditableOrganizationConfig)


getAffiliationConfig : AppState -> ToMsg EditableAffiliationConfig msg -> Cmd msg
getAffiliationConfig =
    jwtGet "/configs/affiliation" EditableAffiliationConfig.decoder


putAffiliationConfig : E.Value -> AppState -> ToMsg () msg -> Cmd msg
putAffiliationConfig =
    jwtPut "/configs/affiliation"


getClientConfig : AppState -> ToMsg EditableClientConfig msg -> Cmd msg
getClientConfig =
    jwtGet "/configs/client" EditableClientConfig.decoder


putClientConfig : E.Value -> AppState -> ToMsg () msg -> Cmd msg
putClientConfig =
    jwtPut "/configs/client"


getFeaturesConfig : AppState -> ToMsg EditableFeaturesConfig msg -> Cmd msg
getFeaturesConfig =
    jwtGet "/configs/features" EditableFeaturesConfig.decoder


putFeaturesConfig : E.Value -> AppState -> ToMsg () msg -> Cmd msg
putFeaturesConfig =
    jwtPut "/configs/features"


getInfoConfig : AppState -> ToMsg EditableInfoConfig msg -> Cmd msg
getInfoConfig =
    jwtGet "/configs/info" EditableInfoConfig.decoder


putInfoConfig : E.Value -> AppState -> ToMsg () msg -> Cmd msg
putInfoConfig =
    jwtPut "/configs/info"


getOrganizationConfig : AppState -> ToMsg EditableOrganizationConfig msg -> Cmd msg
getOrganizationConfig =
    jwtGet "/configs/organization" EditableOrganizationConfig.decoder


putOrganizationConfig : E.Value -> AppState -> ToMsg () msg -> Cmd msg
putOrganizationConfig =
    jwtPut "/configs/organization"
