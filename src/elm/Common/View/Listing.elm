module Common.View.Listing exposing
    ( ListingActionConfig
    , ListingActionType(..)
    , ListingConfig
    , view
    , viewItem
    )

import Common.Html exposing (emptyNode, fa)
import Common.View.Page as Page
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href, style)
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
        div [ class "list-group list-group-flush list-group-listing" ]
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
        [ viewIcon config item
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


viewIcon : ListingConfig a msg -> a -> Html msg
viewIcon config item =
    let
        letter =
            config.textTitle item
                |> String.uncons
                |> Maybe.map (\( a, _ ) -> a)
                |> Maybe.withDefault 'A'

        hash =
            config.textTitle item
                |> String.toList
                |> List.map Char.toCode
                |> List.sum

        h =
            String.fromInt <| remainderBy 360 hash

        s =
            String.fromInt <| 25 + remainderBy 71 hash

        l =
            String.fromInt <| 85 + remainderBy 11 hash

        hsl =
            "hsl(" ++ h ++ "," ++ s ++ "%," ++ l ++ "%)"
    in
    div [ class "icon", style "background-color" hsl ]
        [ text <| String.fromChar letter
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
