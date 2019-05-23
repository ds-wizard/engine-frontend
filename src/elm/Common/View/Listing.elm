module Common.View.Listing exposing
    ( ListingActionConfig
    , ListingActionType(..)
    , ListingConfig
    , view
    , viewItem
    )

import Common.Html exposing (emptyNode, fa)
import Common.View.ItemIcon as ItemIcon
import Common.View.Page as Page
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Routing


type alias ListingConfig a msg =
    { title : a -> Html msg
    , description : a -> Html msg
    , actions : a -> List (ListingActionConfig msg)
    , textTitle : a -> String
    , emptyText : String
    }


type alias ListingActionConfig msg =
    { extraClass : Maybe String
    , icon : Maybe String
    , label : String
    , msg : ListingActionType msg
    }


type ListingActionType msg
    = ListingActionMsg msg
    | ListingActionLink Routing.Route


view : ListingConfig a msg -> List a -> Html msg
view config data =
    if List.length data > 0 then
        div [ class "Listing list-group list-group-flush" ]
            (List.map (viewItem config) data)

    else
        viewEmpty config


viewEmpty : ListingConfig a msg -> Html msg
viewEmpty config =
    Page.illustratedMessage
        { image = "no_data"
        , heading = "No data"
        , lines = [ config.emptyText ]
        }


viewItem : ListingConfig a msg -> a -> Html msg
viewItem config item =
    div [ class "list-group-item" ]
        [ ItemIcon.view { text = config.textTitle item, image = Nothing }
        , div [ class "content" ]
            [ div [ class "title" ]
                [ config.title item
                ]
            , div [ class "extra" ]
                [ div [ class "description" ]
                    [ config.description item ]
                , div [ class "actions" ]
                    (List.map viewAction <| config.actions item)
                ]
            ]
        ]


viewAction : ListingActionConfig msg -> Html msg
viewAction action =
    let
        icon =
            action.icon
                |> Maybe.map fa
                |> Maybe.withDefault emptyNode

        event =
            case action.msg of
                ListingActionLink route ->
                    href <| Routing.toUrl route

                ListingActionMsg msg ->
                    onClick msg
    in
    a [ class <| Maybe.withDefault "" action.extraClass, event ]
        [ icon, text action.label ]
