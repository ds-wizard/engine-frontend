module Wizard.Common.Components.SimpleList exposing
    ( Config
    , UpdatedConfig
    , view
    )

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l)
import Shared.Undraw as Undraw
import Time
import Time.Distance exposing (inWordsWithConfig)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.TimeDistance exposing (locale)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Common.View.Page as Page


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Components.SimpleList"


type alias Config a msg =
    { title : a -> Html msg
    , description : a -> Html msg
    , textTitle : a -> String
    , emptyText : String
    , updated : Maybe (UpdatedConfig a)
    , iconView : Maybe (a -> Html msg)
    }


type alias UpdatedConfig a =
    { getTime : a -> Time.Posix
    , currentTime : Time.Posix
    }


view : AppState -> Config a msg -> List a -> Html msg
view appState config items =
    if List.length items > 0 then
        div [ class "Listing list-group list-group-flush" ]
            (List.map (viewItem appState config) items)

    else
        viewEmpty appState config


viewEmpty : AppState -> Config a msg -> Html msg
viewEmpty appState config =
    Page.illustratedMessage
        { image = Undraw.noData
        , heading = l_ "empty.heading" appState
        , lines = [ config.emptyText ]
        , cy = "listing-empty"
        }


viewItem : AppState -> Config a msg -> a -> Html msg
viewItem appState config item =
    let
        icon =
            config.iconView
                |> Maybe.andMap (Just item)
                |> Maybe.withDefault (ItemIcon.view { text = config.textTitle item, image = Nothing })
    in
    div [ class "list-group-item" ]
        [ icon
        , div [ class "content" ]
            [ div [ class "title-row" ]
                [ span [ class "title" ] [ config.title item ]
                ]
            , div [ class "extra" ]
                [ div [ class "description" ]
                    [ config.description item ]
                ]
            ]
        , div [ class "updated" ]
            [ viewUpdated appState config item ]
        ]


viewUpdated : AppState -> Config a msg -> a -> Html msg
viewUpdated appState config item =
    case config.updated of
        Just updated ->
            span []
                [ text <| l_ "item.updated" appState ++ inWordsWithConfig { withAffix = True } (locale appState) (updated.getTime item) updated.currentTime ]

        Nothing ->
            emptyNode
