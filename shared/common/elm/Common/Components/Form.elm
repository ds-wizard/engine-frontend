module Common.Components.Form exposing
    ( SimpleFormConfig
    , simple
    )

import ActionResult exposing (ActionResult)
import Common.Components.ActionButton as ActionButton
import Common.Components.FormResult as FormResult
import Common.Utils.ShortcutUtils as Shortcut
import Form
import Gettext exposing (gettext)
import Html exposing (Html, button, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Shortcut


type alias SimpleFormConfig a msg =
    { formMsg : Form.Msg -> msg
    , formResult : ActionResult a
    , formView : Html msg
    , submitLabel : String
    , cancelMsg : Maybe msg
    , locale : Gettext.Locale
    , isMac : Bool
    }


simple : SimpleFormConfig a msg -> Html msg
simple cfg =
    let
        shortcuts =
            if not (ActionResult.isLoading cfg.formResult) then
                [ Shortcut.submitShortcut cfg.isMac (cfg.formMsg Form.Submit) ]

            else
                []
    in
    Shortcut.shortcutElement shortcuts
        []
        [ FormResult.errorOnlyView cfg.formResult
        , cfg.formView
        , formActions
            { formMsg = cfg.formMsg
            , formResult = cfg.formResult
            , submitLabel = cfg.submitLabel
            , cancelMsg = cfg.cancelMsg
            , locale = cfg.locale
            }
        ]


type alias FormActionsConfig a msg =
    { formMsg : Form.Msg -> msg
    , formResult : ActionResult a
    , submitLabel : String
    , cancelMsg : Maybe msg
    , locale : Gettext.Locale
    }


formActions : FormActionsConfig a msg -> Html msg
formActions cfg =
    let
        cancelButton =
            case cfg.cancelMsg of
                Just msg ->
                    button
                        [ class "btn btn-secondary btn-wide"
                        , onClick msg
                        , Html.Attributes.type_ "button"
                        ]
                        [ Html.text (gettext "Cancel" cfg.locale) ]

                Nothing ->
                    Html.nothing

        submitButton =
            ActionButton.button
                { label = cfg.submitLabel
                , result = cfg.formResult
                , msg = cfg.formMsg Form.Submit
                , dangerous = False
                }
    in
    div [ class "d-flex justify-content-between mt-4 pt-4 border-top" ]
        [ cancelButton
        , submitButton
        ]
