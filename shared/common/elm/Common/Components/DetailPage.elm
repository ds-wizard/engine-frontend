module Common.Components.DetailPage exposing
    ( ContentConfig
    , HeaderWithNavConfig
    , HeaderWithNavItemConfig
    , container
    , content
    , contentBodyNarrow
    , contentFull
    , header
    , headerWithNav
    , sidePanelHtmlItemWithIcon
    , sidePanelItemWithIcon
    , sidePanelItemWithIconWithLink
    , sidePanelList
    )

import Common.Components.Badge as Badge
import Html exposing (Html, a, br, dd, div, dl, dt, li, span, strong, text, ul)
import Html.Attributes exposing (attribute, class, classList, href)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Maybe.Extra as Maybe


container : List (Html msg) -> Html msg
container =
    div [ class "detail" ]


header : Html msg -> List (Html msg) -> Html msg
header title actions =
    div [ class "detail-header" ]
        [ div [ class "detail-header-content" ]
            [ div [ class "detail-header-title", dataCy "detail-page-header-title" ] [ title ]
            , div [ class "detail-header-actions" ] actions
            ]
        ]


type alias HeaderWithNavConfig msg =
    { title : Html msg
    , actions : List (Html msg)
    , navItems : List (HeaderWithNavItemConfig msg)
    }


type alias HeaderWithNavItemConfig msg =
    { title : String
    , onClick : msg
    , count : Maybe Int
    , isActive : Bool
    , dataCy : String
    }


headerWithNav : HeaderWithNavConfig msg -> Html msg
headerWithNav cfg =
    let
        viewNavItem navItem =
            let
                countBadge count =
                    Badge.secondary
                        [ class "rounded-pill"
                        , dataCy ("nav-item_" ++ navItem.dataCy ++ "_count")
                        ]
                        [ text (String.fromInt count) ]
            in
            li [ class "nav-item" ]
                [ a
                    [ class "nav-link"
                    , classList [ ( "active", navItem.isActive ) ]
                    , onClick navItem.onClick
                    , dataCy ("nav-item_" ++ navItem.dataCy)
                    ]
                    [ span [ attribute "data-content" navItem.title ] [ text navItem.title ]
                    , Maybe.unwrap Html.nothing countBadge navItem.count
                    ]
                ]
    in
    div [ class "detail-header detail-header-with-nav" ]
        [ div [ class "detail-header-content" ]
            [ div [ class "detail-header-title", dataCy "detail-page-header-title" ] [ cfg.title ]
            , div [ class "detail-header-actions" ] cfg.actions
            ]
        , div [ class "detail-header-content" ]
            [ ul [ class "nav nav-underline-tabs" ]
                (List.map viewNavItem cfg.navItems)
            ]
        ]


type alias ContentConfig msg =
    { body : List (Html msg)
    , sidePanel : List (Html msg)
    }


content : ContentConfig msg -> Html msg
content cfg =
    let
        sidePanel =
            if List.isEmpty cfg.sidePanel then
                Html.nothing

            else
                div [ class "detail-content-side-panel" ] cfg.sidePanel
    in
    div [ class "detail-content", dataCy "detail-page_content" ]
        [ div [ class "detail-content-body" ] cfg.body
        , sidePanel
        ]


contentFull : List (Html msg) -> Html msg
contentFull body =
    div [ class "detail-content", dataCy "detail-page_content" ]
        [ div [ class "detail-content-body detail-content-body-full" ] body
        ]


contentBodyNarrow : List (Html msg) -> Html msg
contentBodyNarrow body =
    div [ class "detail-content-body-narrow" ]
        body


sidePanelList : Int -> Int -> List ( String, String, Html msg ) -> Html msg
sidePanelList colLabel colValue rows =
    let
        viewRow ( label, cy, value ) =
            [ dt [ class <| "col-" ++ String.fromInt colLabel ]
                [ text label ]
            , dd [ class <| "col-" ++ String.fromInt colValue, dataCy ("detail-page_metadata_" ++ cy) ]
                [ value ]
            ]
    in
    dl [ class "row" ] (List.concatMap viewRow rows)


sidePanelHtmlItemWithIcon : List (Html msg) -> Html msg -> Html msg
sidePanelHtmlItemWithIcon itemContent icon =
    div [ class "detail-content-side-panel-item-icon" ]
        [ icon
        , div [] itemContent
        ]


sidePanelItemWithIcon : String -> Html msg -> Html msg -> Html msg
sidePanelItemWithIcon title description icon =
    div [ class "detail-content-side-panel-item-icon" ]
        [ icon
        , div []
            [ strong [] [ text title ]
            , br [] []
            , description
            ]
        ]


sidePanelItemWithIconWithLink : String -> String -> Html msg -> Html msg -> Html msg
sidePanelItemWithIconWithLink url title description icon =
    div [ class "detail-content-side-panel-item-icon" ]
        [ icon
        , div []
            [ strong [] [ a [ href url ] [ text title ] ]
            , br [] []
            , description
            ]
        ]
