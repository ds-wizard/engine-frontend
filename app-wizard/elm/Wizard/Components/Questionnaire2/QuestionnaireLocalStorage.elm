module Wizard.Components.Questionnaire2.QuestionnaireLocalStorage exposing
    ( ApplyLocalStorageDataConfig
    , applyLocalStorageData
    , getItems
    , updateCollapsedPaths
    , updateNamedOnly
    , updateRightPanel
    , updateViewResolved
    , updateViewSettings
    )

import Common.Ports.LocalStorage as LocalStorage
import Json.Decode as D
import Json.Decode.Extra as D
import Json.Encode as E
import Set exposing (Set)
import Uuid exposing (Uuid)
import Wizard.Components.Questionnaire2.QuestionnaireRightPanel as QuestionnaireRightPanel exposing (QuestionnaireRightPanel)
import Wizard.Components.Questionnaire2.QuestionnaireViewSettings as QuestionnaireViewSettings exposing (QuestionnaireViewSettings)


localStorageViewSettingsKey : String
localStorageViewSettingsKey =
    "project-view-settings"


localStorageCollapsedPathsKey : Uuid -> String
localStorageCollapsedPathsKey uuid =
    "project-" ++ Uuid.toString uuid ++ "-collapsed-paths"


localStorageRightPanelKey : Uuid -> String
localStorageRightPanelKey uuid =
    "project-" ++ Uuid.toString uuid ++ "-right-panel"


localStorageViewResolvedKey : Uuid -> String
localStorageViewResolvedKey uuid =
    "project-" ++ Uuid.toString uuid ++ "-view-resolved"


localStorageNamedOnlyKey : Uuid -> String
localStorageNamedOnlyKey uuid =
    "project-" ++ Uuid.toString uuid ++ "-named-only"


getItems : Uuid -> Cmd msg
getItems projectUuid =
    Cmd.batch
        [ LocalStorage.getItem localStorageViewSettingsKey
        , LocalStorage.getItem (localStorageCollapsedPathsKey projectUuid)
        , LocalStorage.getItem (localStorageRightPanelKey projectUuid)
        , LocalStorage.getItem (localStorageViewResolvedKey projectUuid)
        , LocalStorage.getItem (localStorageNamedOnlyKey projectUuid)
        ]


updateCollapsedPaths : Uuid -> Set String -> Cmd msg
updateCollapsedPaths uuid collapsedItems =
    if Set.isEmpty collapsedItems then
        LocalStorage.removeItem (localStorageCollapsedPathsKey uuid)

    else
        LocalStorage.setItem (localStorageCollapsedPathsKey uuid) (E.set E.string collapsedItems)


updateViewResolved : Uuid -> Bool -> Cmd msg
updateViewResolved uuid viewResolved =
    LocalStorage.setItem (localStorageViewResolvedKey uuid) (E.bool viewResolved)


updateNamedOnly : Uuid -> Bool -> Cmd msg
updateNamedOnly uuid namedOnly =
    LocalStorage.setItem (localStorageNamedOnlyKey uuid) (E.bool namedOnly)


updateViewSettings : QuestionnaireViewSettings -> Cmd msg
updateViewSettings viewSettings =
    LocalStorage.setItem localStorageViewSettingsKey (QuestionnaireViewSettings.encode viewSettings)


updateRightPanel : Uuid -> QuestionnaireRightPanel -> Cmd msg
updateRightPanel uuid rightPanelState =
    LocalStorage.setItem (localStorageRightPanelKey uuid) (QuestionnaireRightPanel.encode rightPanelState)


type alias ApplyLocalStorageDataConfig msg =
    { updateViewSettings : QuestionnaireViewSettings -> Cmd msg
    , updateCollapsedItems : Set String -> Cmd msg
    , updateRightPanel : QuestionnaireRightPanel -> Cmd msg
    , updateViewResolved : Bool -> Cmd msg
    , updateNamedOnly : Bool -> Cmd msg
    }


applyLocalStorageData : Uuid -> E.Value -> ApplyLocalStorageDataConfig msg -> Cmd msg
applyLocalStorageData uuid json cfg =
    case D.decodeValue (D.field "key" D.string) json of
        Ok key ->
            if key == localStorageViewSettingsKey then
                case LocalStorage.decodeItemValue QuestionnaireViewSettings.decoder json of
                    Ok settings ->
                        cfg.updateViewSettings settings

                    Err _ ->
                        Cmd.none

            else if key == localStorageCollapsedPathsKey uuid then
                case LocalStorage.decodeItemValue (D.set D.string) json of
                    Ok collapsedItems ->
                        cfg.updateCollapsedItems collapsedItems

                    Err _ ->
                        Cmd.none

            else if key == localStorageRightPanelKey uuid then
                case LocalStorage.decodeItemValue QuestionnaireRightPanel.decoder json of
                    Ok rightPanelState ->
                        cfg.updateRightPanel rightPanelState

                    Err _ ->
                        Cmd.none

            else if key == localStorageViewResolvedKey uuid then
                case LocalStorage.decodeItemValue D.bool json of
                    Ok viewResolved ->
                        cfg.updateViewResolved viewResolved

                    Err _ ->
                        Cmd.none

            else if key == localStorageNamedOnlyKey uuid then
                case LocalStorage.decodeItemValue D.bool json of
                    Ok namedOnly ->
                        cfg.updateNamedOnly namedOnly

                    Err _ ->
                        Cmd.none

            else
                Cmd.none

        Err _ ->
            Cmd.none
