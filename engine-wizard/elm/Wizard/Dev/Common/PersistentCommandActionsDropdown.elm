module Wizard.Dev.Common.PersistentCommandActionsDropdown exposing
    ( ActionsConfig
    , DropdownConfig
    , PersistentCommandLike
    , actions
    , dropdown
    )

import Bootstrap.Dropdown as Dropdown
import Html exposing (Html)
import Shared.Html exposing (fa, faSet)
import Uuid exposing (Uuid)
import Wizard.Api.Models.PersistentCommand.PersistentCommandState as PersistentCommandState exposing (PersistentCommandState)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Routes as Routes


type alias PersistentCommandLike a =
    { a
        | uuid : Uuid
        , state : PersistentCommandState
    }


type alias DropdownConfig msg =
    { dropdownState : Dropdown.State
    , toggleMsg : Dropdown.State -> msg
    }


type alias ActionsConfig a msg =
    { retryMsg : PersistentCommandLike a -> msg
    , setIgnoredMsg : PersistentCommandLike a -> msg
    , viewActionVisible : Bool
    }


actions : AppState -> ActionsConfig a msg -> PersistentCommandLike a -> List (ListingDropdownItem msg)
actions appState cfg command =
    let
        viewAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.open" appState
                , label = "Open"
                , msg = ListingActionLink (Routes.persistentCommandsDetail command.uuid)
                , dataCy = "view"
                }

        viewActionVisible =
            cfg.viewActionVisible

        retryAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "persistentCommand.retry" appState
                , label = "Rerun"
                , msg = ListingActionMsg (cfg.retryMsg command)
                , dataCy = "retry"
                }

        retryActionVisible =
            True

        setIgnoredAction =
            ListingDropdown.dropdownAction
                { extraClass = Just "text-danger"
                , icon = fa "fas fa-ban"
                , label = "Ignore"
                , msg = ListingActionMsg (cfg.setIgnoredMsg command)
                , dataCy = "set-ignored"
                }

        setIgnoredVisible =
            command.state /= PersistentCommandState.Ignore

        groups =
            [ [ ( viewAction, viewActionVisible )
              , ( retryAction, retryActionVisible )
              ]
            , [ ( setIgnoredAction, setIgnoredVisible )
              ]
            ]
    in
    ListingDropdown.itemsFromGroups groups


dropdown : AppState -> DropdownConfig msg -> ActionsConfig a msg -> PersistentCommandLike a -> Html msg
dropdown appState dropdownConfig actionsConfig command =
    ListingDropdown.dropdown appState
        { dropdownState = dropdownConfig.dropdownState
        , toggleMsg = dropdownConfig.toggleMsg
        , items = actions appState actionsConfig command
        }
