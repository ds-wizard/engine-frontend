module Wizard.Locales.Common.LocaleActionsDropdown exposing
    ( ActionsConfig
    , DropdownConfig
    , LocaleLike
    , actions
    , dropdown
    )

import Bootstrap.Dropdown as Dropdown
import Gettext exposing (gettext)
import Html exposing (Html)
import Shared.Components.FontAwesome exposing (faDelete, faDisable, faEnable, faExport, faLocaleDefault, faView)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Common.Feature as Feature
import Wizard.Routes as Routes


type alias LocaleLike a =
    { a
        | id : String
        , localeId : String
        , organizationId : String
        , defaultLocale : Bool
        , enabled : Bool
    }


type alias DropdownConfig msg =
    { dropdownState : Dropdown.State
    , toggleMsg : Dropdown.State -> msg
    }


type alias ActionsConfig a msg =
    { exportMsg : LocaleLike a -> msg
    , setDefaultMsg : LocaleLike a -> msg
    , setEnabledMsg : Bool -> LocaleLike a -> msg
    , deleteMsg : LocaleLike a -> msg
    , viewActionVisible : Bool
    }


actions : AppState -> ActionsConfig a msg -> LocaleLike a -> List (ListingDropdownItem msg)
actions appState cfg locale =
    let
        viewAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faView
                , label = gettext "View detail" appState.locale
                , msg = ListingActionLink (Routes.localesDetail locale.id)
                , dataCy = "view"
                }

        viewActionVisible =
            cfg.viewActionVisible && Feature.localeView appState

        exportAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faExport
                , label = gettext "Export" appState.locale
                , msg = ListingActionMsg (cfg.exportMsg locale)
                , dataCy = "export"
                }

        exportActionVisible =
            Feature.localeExport appState locale

        setDefaultAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faLocaleDefault
                , label = gettext "Set default" appState.locale
                , msg = ListingActionMsg (cfg.setDefaultMsg locale)
                , dataCy = "set-default"
                }

        setDefaultActionVisible =
            Feature.localeSetDefault appState locale

        changeEnabledAction =
            if locale.enabled then
                ListingDropdown.dropdownAction
                    { extraClass = Nothing
                    , icon = faDisable
                    , label = gettext "Disable" appState.locale
                    , msg = ListingActionMsg (cfg.setEnabledMsg False locale)
                    , dataCy = "disable"
                    }

            else
                ListingDropdown.dropdownAction
                    { extraClass = Nothing
                    , icon = faEnable
                    , label = gettext "Enable" appState.locale
                    , msg = ListingActionMsg (cfg.setEnabledMsg True locale)
                    , dataCy = "enable"
                    }

        changeEnabledActionVisible =
            Feature.localeChangeEnabled appState locale

        deleteAction =
            ListingDropdown.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faDelete
                , label = gettext "Delete" appState.locale
                , msg = ListingActionMsg (cfg.deleteMsg locale)
                , dataCy = "delete"
                }

        deleteActionVisible =
            Feature.localeDelete appState locale

        groups =
            [ [ ( viewAction, viewActionVisible )
              , ( exportAction, exportActionVisible )
              ]
            , [ ( setDefaultAction, setDefaultActionVisible )
              , ( changeEnabledAction, changeEnabledActionVisible )
              ]
            , [ ( deleteAction, deleteActionVisible )
              ]
            ]
    in
    ListingDropdown.itemsFromGroups groups


dropdown : AppState -> DropdownConfig msg -> ActionsConfig a msg -> LocaleLike a -> Html msg
dropdown appState dropdownConfig actionsConfig locale =
    ListingDropdown.dropdown
        { dropdownState = dropdownConfig.dropdownState
        , toggleMsg = dropdownConfig.toggleMsg
        , items = actions appState actionsConfig locale
        }
