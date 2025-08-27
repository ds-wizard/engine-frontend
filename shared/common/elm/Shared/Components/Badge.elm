module Shared.Components.Badge exposing
    ( badge
    , danger
    , dark
    , info
    , infoClass
    , light
    , secondary
    , success
    , warning
    , warningClass
    )

import Html exposing (Html, span)
import Html.Attributes exposing (class)


secondary : List (Html.Attribute msg) -> List (Html msg) -> Html msg
secondary =
    render secondaryClass


secondaryClass : String
secondaryClass =
    badgeClass "bg-secondary"


success : List (Html.Attribute msg) -> List (Html msg) -> Html msg
success =
    render successClass


successClass : String
successClass =
    badgeClass "bg-success"


danger : List (Html.Attribute msg) -> List (Html msg) -> Html msg
danger =
    render dangerClass


dangerClass : String
dangerClass =
    badgeClass "bg-danger"


warning : List (Html.Attribute msg) -> List (Html msg) -> Html msg
warning =
    render warningClass


warningClass : String
warningClass =
    badgeClass "bg-warning text-dark"


info : List (Html.Attribute msg) -> List (Html msg) -> Html msg
info =
    render infoClass


infoClass : String
infoClass =
    badgeClass "bg-info"


light : List (Html.Attribute msg) -> List (Html msg) -> Html msg
light =
    render lightClass


lightClass : String
lightClass =
    badgeClass "bg-light text-dark"


dark : List (Html.Attribute msg) -> List (Html msg) -> Html msg
dark =
    render darkClass


darkClass : String
darkClass =
    badgeClass "bg-dark"


badge : List (Html.Attribute msg) -> List (Html msg) -> Html msg
badge attributes =
    span (class "badge" :: attributes)


render : String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
render badgeClasses attributes =
    span (class badgeClasses :: attributes)


badgeClass : String -> String
badgeClass specificClass =
    "badge " ++ specificClass
