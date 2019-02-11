module Common.View.Forms exposing
    ( actionButton
    , actionButtonView
    , errorView
    , formActionOnly
    , formActions
    , formErrorResultView
    , formResultView
    , formSuccessResultView
    , formText
    , formTextAfter
    , infoView
    , statusView
    , submitButton
    , successView
    )

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msgs exposing (Msg)
import Routing exposing (Route)
import String



-- Other form views


formText : String -> Html msg
formText str =
    p [ class "form-text text-muted" ] [ text str ]


formTextAfter : String -> Html msg
formTextAfter str =
    p [ class "form-text form-text-after text-muted" ] [ text str ]



-- Form Actions


{-| Helper to show action buttons below the form.

  - Cancel button simply redirects to another route
  - Action button invokes specified message when clicked

-}
formActions : Route -> ( String, ActionResult a, Msg ) -> Html Msg
formActions cancelRoute actionButtonSettings =
    div [ class "form-actions" ]
        [ linkTo cancelRoute [ class "btn btn-secondary" ] [ text "Cancel" ]
        , actionButton actionButtonSettings
        ]


{-| Similar to formActions, but it contains only the action button.
-}
formActionOnly : ( String, ActionResult a, msg ) -> Html msg
formActionOnly actionButtonSettings =
    div [ class "text-right" ]
        [ actionButton actionButtonSettings ]


{-| Action button invokes a message when clicked. It's state is defined by
the ActionResult. If the state is Loading action button is disabled and
a loader is shown instead of action name.
-}
actionButton : ( String, ActionResult a, msg ) -> Html msg
actionButton ( label, result, msg ) =
    actionButtonView [ onClick msg ] label result


submitButton : ( String, ActionResult a ) -> Html msg
submitButton ( label, result ) =
    actionButtonView [ type_ "submit" ] label result


actionButtonView : List (Attribute msg) -> String -> ActionResult a -> Html msg
actionButtonView attributes label result =
    let
        buttonContent =
            case result of
                Loading ->
                    i [ class "fa fa-spinner fa-spin" ] []

                _ ->
                    text label

        buttonAttributes =
            [ class "btn btn-primary btn-with-loader", disabled (result == Loading) ] ++ attributes
    in
    button buttonAttributes [ buttonContent ]



-- Status Views


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
