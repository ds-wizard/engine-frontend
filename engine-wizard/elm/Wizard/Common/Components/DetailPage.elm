module Wizard.Common.Components.DetailPage exposing
    ( container
    , content
    , contentInnerClass
    , contentInnerFullClass
    , header
    , sidePanel
    , sidePanelItemWithIcon
    , sidePanelItemWithIconWithLink
    , sidePanelList
    )

import Html exposing (Html, br, dd, div, dl, dt, strong, text)
import Html.Attributes exposing (class)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Routes exposing (Route)


container : List (Html msg) -> Html msg
container =
    div [ class "DetailPage" ]


header : Html msg -> List (Html msg) -> Html msg
header title actions =
    div [ class "top-header" ]
        [ div [ class "top-header-content" ]
            [ div [ class "top-header-title", dataCy "detail-page_header-title" ] [ title ]
            , div [ class "top-header-actions" ] actions
            ]
        ]


content : List (Html msg) -> Html msg
content =
    div [ class "DetailPage__Content", dataCy "detail-page_content" ]


contentInnerClass : Html.Attribute msg
contentInnerClass =
    class "DetailPage__Content__Inner"


contentInnerFullClass : Html.Attribute msg
contentInnerFullClass =
    class "DetailPage__Content__InnerFull"


sidePanel : List (Html msg) -> Html msg
sidePanel =
    div [ class "DetailPage__SidePanel" ]


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


sidePanelItemWithIcon : String -> Html msg -> Html msg -> Html msg
sidePanelItemWithIcon title description icon =
    div [ class "DetailPage__SidePanel__ItemIcon" ]
        [ icon
        , div [ class "content" ]
            [ strong [] [ text title ]
            , br [] []
            , description
            ]
        ]


sidePanelItemWithIconWithLink : Route -> String -> Html msg -> Html msg -> Html msg
sidePanelItemWithIconWithLink route title description icon =
    div [ class "DetailPage__SidePanel__ItemIcon" ]
        [ icon
        , div [ class "content" ]
            [ strong [] [ linkTo route [] [ text title ] ]
            , br [] []
            , description
            ]
        ]
