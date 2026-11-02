module Common.Pages.Dev.Components.PersistentCommandActions exposing
    ( ActionsConfig
    , PersistentCommandLike
    , actions
    )

{-| The actions of a persistent command, for its row and its detail page.
-}

import Common.Api.Models.PersistentCommand.PersistentCommandState as PersistentCommandState exposing (PersistentCommandState)
import Common.Components.ItemActions as ItemActions
import Common.Routes as Routes
import Gettext exposing (gettext)
import Ui.Icon as Icon
import Uuid exposing (Uuid)


type alias PersistentCommandLike a =
    { a
        | uuid : Uuid
        , state : PersistentCommandState
    }


type alias ActionsConfig a msg =
    { retryMsg : PersistentCommandLike a -> msg
    , setIgnoredMsg : PersistentCommandLike a -> msg
    , viewActionVisible : Bool
    }


actions : { b | locale : Gettext.Locale, toUrl : Routes.Route -> String } -> ActionsConfig a msg -> PersistentCommandLike a -> List (ItemActions.Action msg)
actions appState cfg command =
    let
        viewAction =
            ItemActions.link
                { icon = Icon.regular "fa-folder-open"
                , label = gettext "Open" appState.locale
                , url = Routes.toUrl appState (Routes.persistentCommandsDetail command.uuid)
                }
                |> ItemActions.withTestId "listing-item_action_view"

        retryAction =
            ItemActions.action
                { icon = Icon.solid "fa-rotate"
                , label = gettext "Rerun" appState.locale
                , onSelect = cfg.retryMsg command
                }
                |> ItemActions.withTestId "listing-item_action_retry"

        setIgnoredAction =
            ItemActions.dangerAction
                { icon = Icon.solid "fa-ban"
                , label = gettext "Ignore" appState.locale
                , onSelect = cfg.setIgnoredMsg command
                }
                |> ItemActions.withTestId "listing-item_action_set-ignored"
    in
    ItemActions.groups
        [ [ ( viewAction, cfg.viewActionVisible )
          , ( retryAction, True )
          ]
        , [ ( setIgnoredAction, command.state /= PersistentCommandState.Ignore )
          ]
        ]
