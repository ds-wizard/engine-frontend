module Common.View.Forms exposing
    ( errorView
    , formActionOnly
    , formActions
    , formErrorResultView
    , formResultView
    , formSuccessResultView
    , infoView
    , statusView
    , successView
    )

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (..)
import Common.View.ActionButton as ActionButton
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs exposing (Msg)
import Routing exposing (Route)
import String


{-| Helper to show action buttons below the form.

  - Cancel button simply redirects to another route
  - Action button invokes specified message when clicked

-}
formActions : Route -> ( String, ActionResult a, Msg ) -> Html Msg
formActions cancelRoute actionButtonSettings =
    div [ class "form-actions" ]
        [ linkTo cancelRoute [ class "btn btn-secondary" ] [ text "Cancel" ]
        , ActionButton.button actionButtonSettings
        ]


{-| Similar to formActions, but it contains only the action button.
-}
formActionOnly : ( String, ActionResult a, msg ) -> Html msg
formActionOnly actionButtonSettings =
    div [ class "text-right" ]
        [ ActionButton.button actionButtonSettings ]


formResultView : ActionResult String -> Html msg
formResultView result =
    case result of
        Success msg ->
            successView msg

        Error msg ->
            errorView msg

        _ ->
            emptyNode


formSuccessResultView : ActionResult String -> Html msg
formSuccessResultView result =
    case result of
        Success msg ->
            successView msg

        _ ->
            emptyNode


formErrorResultView : ActionResult String -> Html msg
formErrorResultView result =
    case result of
        Error msg ->
            errorView msg

        _ ->
            emptyNode


errorView : String -> Html msg
errorView =
    statusView "alert-danger" "fa-exclamation-triangle"


successView : String -> Html msg
successView =
    statusView "alert-success" "fa-check"


infoView : String -> Html msg
infoView =
    statusView "alert-info" "fa-info-circle"


statusView : String -> String -> String -> Html msg
statusView className icon msg =
    if msg /= "" then
        div [ class ("alert " ++ className) ]
            [ i [ class ("fa " ++ icon) ] []
            , text msg
            ]

    else
        text ""
