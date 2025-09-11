module Wizard.Pages.Dev.Common.PersistentCommandActionsDropdown exposing
    ( ActionsConfig
    , DropdownConfig
    , PersistentCommandLike
    , actions
    , dropdown
    )

import Bootstrap.Dropdown as Dropdown
import Common.Api.Models.PersistentCommand.PersistentCommandState as PersistentCommandState exposing (PersistentCommandState)
import Common.Components.FontAwesome exposing (fa, faOpen, faPersistentCommandRetry)
import Html exposing (Html)
import Uuid exposing (Uuid)
import Wizard.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
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


actions : ActionsConfig a msg -> PersistentCommandLike a -> List (ListingDropdownItem msg)
actions cfg command =
    let
        viewAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faOpen
                , label = "Open"
                , msg = ListingActionLink (Routes.persistentCommandsDetail command.uuid)
                , dataCy = "view"
                }

        viewActionVisible =
            cfg.viewActionVisible

        retryAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faPersistentCommandRetry
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


dropdown : DropdownConfig msg -> ActionsConfig a msg -> PersistentCommandLike a -> Html msg
dropdown dropdownConfig actionsConfig command =
    ListingDropdown.dropdown
        { dropdownState = dropdownConfig.dropdownState
        , toggleMsg = dropdownConfig.toggleMsg
        , items = actions actionsConfig command
        }
