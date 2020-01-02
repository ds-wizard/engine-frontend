module Wizard.Common.View.Listing exposing
    ( ListingActionConfig
    , ListingActionType(..)
    , ListingConfig
    , UpdatedConfig
    , view
    , viewItem
    )

import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Shared.Locale exposing (l)
import Time
import Time.Distance exposing (inWordsWithConfig)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (emptyNode)
import Wizard.Common.TimeDistance exposing (locale)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Common.View.Page as Page
import Wizard.Routes as Routes
import Wizard.Routing as Routing


type alias ListingConfig a msg =
    { title : a -> Html msg
    , description : a -> Html msg
    , actions : a -> List (ListingActionConfig msg)
    , textTitle : a -> String
    , emptyText : String
    , updated : Maybe (UpdatedConfig a)
    }


type alias UpdatedConfig a =
    { getTime : a -> Time.Posix
    , currentTime : Time.Posix
    }


type alias ListingActionConfig msg =
    { extraClass : Maybe String
    , icon : Maybe (Html msg)
    , label : String
    , msg : ListingActionType msg
    }


type ListingActionType msg
    = ListingActionMsg msg
    | ListingActionLink Routes.Route


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.View.Listing"


view : AppState -> ListingConfig a msg -> List a -> Html msg
view appState config data =
    if List.length data > 0 then
        div [ class "Listing list-group list-group-flush" ]
            (List.map (viewItem appState config) data)

    else
        viewEmpty appState config


viewEmpty : AppState -> ListingConfig a msg -> Html msg
viewEmpty appState config =
    Page.illustratedMessage
        { image = "no_data"
        , heading = l_ "empty.heading" appState
        , lines = [ config.emptyText ]
        }


viewItem : AppState -> ListingConfig a msg -> a -> Html msg
viewItem appState config item =
    div [ class "list-group-item" ]
        [ ItemIcon.view { text = config.textTitle item, image = Nothing }
        , div [ class "content" ]
            [ div [ class "title-row" ]
                [ span [ class "title" ] [ config.title item ]
                , viewUpdated appState config item
                ]
            , div [ class "extra" ]
                [ div [ class "description" ]
                    [ config.description item ]
                , div [ class "actions" ]
                    (List.map (viewAction appState) <| config.actions item)
                ]
            ]
        ]


viewUpdated : AppState -> ListingConfig a msg -> a -> Html msg
viewUpdated appState config item =
    case config.updated of
        Just updated ->
            span [ class "updated" ]
                [ text <| l_ "item.updated" appState ++ inWordsWithConfig { withAffix = True } (locale appState) (updated.getTime item) updated.currentTime ]

        Nothing ->
            emptyNode


viewAction : AppState -> ListingActionConfig msg -> Html msg
viewAction appState action =
    let
        icon =
            Maybe.withDefault emptyNode action.icon

        event =
            case action.msg of
                ListingActionLink route ->
                    href <| Routing.toUrl appState route

                ListingActionMsg msg ->
                    onClick msg
    in
    a [ class <| Maybe.withDefault "" action.extraClass, event ]
        [ icon, text action.label ]
