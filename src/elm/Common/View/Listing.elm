module Common.View.Listing exposing
    ( ListingActionConfig
    , ListingActionType(..)
    , ListingConfig
    , UpdatedConfig
    , view
    , viewItem
    )

import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode, fa)
import Common.Locale exposing (l)
import Common.TimeDistance exposing (locale)
import Common.View.ItemIcon as ItemIcon
import Common.View.Page as Page
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Routes
import Routing
import Time
import Time.Distance exposing (inWordsWithConfig)


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
    , icon : Maybe String
    , label : String
    , msg : ListingActionType msg
    }


type ListingActionType msg
    = ListingActionMsg msg
    | ListingActionLink Routes.Route


l_ : String -> AppState -> String
l_ =
    l "Common.View.Listing"


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
            action.icon
                |> Maybe.map fa
                |> Maybe.withDefault emptyNode

        event =
            case action.msg of
                ListingActionLink route ->
                    href <| Routing.toUrl appState route

                ListingActionMsg msg ->
                    onClick msg
    in
    a [ class <| Maybe.withDefault "" action.extraClass, event ]
        [ icon, text action.label ]
