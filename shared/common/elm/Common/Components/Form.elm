module Common.Components.Form exposing
    ( AppStateLike
    , CustomFormConfig
    , DynamicFormConfig
    , FormActionsDynamicConfig
    , SimpleFormConfig
    , custom
    , formActionsDynamic
    , initDynamic
    , setClass
    , setFormChanged
    , setFormView
    , setWide
    , viewDynamic
    , viewSimple
    )

import ActionResult exposing (ActionResult)
import Common.Components.ActionButton as ActionButton
import Common.Components.FontAwesome exposing (faSpinner, faSuccess)
import Common.Components.FormResult as FormResult
import Common.Data.Navigator exposing (Navigator)
import Common.Utils.ShortcutUtils as Shortcut
import Form
import Gettext exposing (gettext)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Attributes.Extensions exposing (dataCy)
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


viewSimple : SimpleFormConfig a msg -> Html msg
viewSimple cfg =
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
            ActionButton.buttonWithAttrs
                { label = cfg.submitLabel
                , result = cfg.formResult
                , msg = cfg.formMsg Form.Submit
                , dangerous = False
                , attrs = [ dataCy "form_submit" ]
                }
    in
    div
        [ class "d-flex justify-content-between mt-4 pt-4 border-top"
        , dataCy "form-actions"
        ]
        [ cancelButton
        , submitButton
        ]


type alias CustomFormConfig a msg =
    { formMsg : Form.Msg -> msg
    , formResult : ActionResult a
    , isMac : Bool
    }


custom : SimpleFormConfig a msg -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
custom cfg =
    let
        shortcuts =
            if not (ActionResult.isLoading cfg.formResult) then
                [ Shortcut.submitShortcut cfg.isMac (cfg.formMsg Form.Submit) ]

            else
                []
    in
    Shortcut.shortcutElement shortcuts


type DynamicFormConfig a msg
    = DynamicFormConfig (DynamicFormConfigData a msg)


type alias DynamicFormConfigData a msg =
    { submitMsg : msg
    , formResult : ActionResult a
    , formView : Html msg
    , formChanged : Bool
    , wide : Bool
    , isMac : Bool
    , locale : Gettext.Locale
    , class : Maybe String
    }


type alias AppStateLike a =
    { a
        | navigator : Navigator
        , locale : Gettext.Locale
    }


initDynamic : AppStateLike b -> msg -> ActionResult a -> DynamicFormConfig a msg
initDynamic appState submitMsg actionResult =
    DynamicFormConfig
        { submitMsg = submitMsg
        , formResult = actionResult
        , formView = Html.nothing
        , formChanged = False
        , wide = False
        , isMac = appState.navigator.isMac
        , locale = appState.locale
        , class = Nothing
        }


setFormView : Html msg -> DynamicFormConfig a msg -> DynamicFormConfig a msg
setFormView view (DynamicFormConfig cfg) =
    DynamicFormConfig { cfg | formView = view }


setFormChanged : Bool -> DynamicFormConfig a msg -> DynamicFormConfig a msg
setFormChanged changed (DynamicFormConfig cfg) =
    DynamicFormConfig { cfg | formChanged = changed }


setWide : DynamicFormConfig a msg -> DynamicFormConfig a msg
setWide (DynamicFormConfig cfg) =
    DynamicFormConfig { cfg | wide = True }


setClass : String -> DynamicFormConfig a msg -> DynamicFormConfig a msg
setClass className (DynamicFormConfig cfg) =
    DynamicFormConfig { cfg | class = Just className }


viewDynamic : DynamicFormConfig a msg -> Html msg
viewDynamic (DynamicFormConfig cfg) =
    let
        shortcuts =
            if not (ActionResult.isLoading cfg.formResult) && cfg.formChanged then
                [ Shortcut.submitShortcut cfg.isMac cfg.submitMsg ]

            else
                []
    in
    Shortcut.shortcutElement shortcuts
        [ class (Maybe.withDefault "" cfg.class)
        , class "pb-6"
        ]
        [ FormResult.errorOnlyView cfg.formResult
        , cfg.formView
        , formActionsDynamic
            { submitMsg = cfg.submitMsg
            , actionResult = cfg.formResult
            , formChanged = cfg.formChanged
            , wide = cfg.wide
            , locale = cfg.locale
            }
        ]


type alias FormActionsDynamicConfig a msg =
    { submitMsg : msg
    , actionResult : ActionResult a
    , formChanged : Bool
    , wide : Bool
    , locale : Gettext.Locale
    }


formActionsDynamic : FormActionsDynamicConfig a msg -> Html msg
formActionsDynamic cfg =
    let
        isVisible =
            cfg.formChanged || ActionResult.isLoading cfg.actionResult || ActionResult.isSuccess cfg.actionResult

        isDisabled =
            ActionResult.isLoading cfg.actionResult || ActionResult.isSuccess cfg.actionResult

        content =
            case cfg.actionResult of
                ActionResult.Loading ->
                    faSpinner

                ActionResult.Success _ ->
                    faSuccess

                _ ->
                    text (gettext "Save" cfg.locale)

        saveButton =
            button
                [ class "btn btn-primary btn-wide"
                , disabled isDisabled
                , dataCy "form_submit"
                , onClick cfg.submitMsg
                ]
                [ content ]
    in
    div
        [ class "form-actions-dynamic"
        , classList
            [ ( "form-actions-dynamic-visible", isVisible )
            , ( "form-actions-dynamic-wide", cfg.wide )
            , ( "form-actions-dynamic-success", ActionResult.isSuccess cfg.actionResult )
            ]
        , dataCy "form-actions"
        ]
        [ p [] [ text (gettext "You have unsaved changes." cfg.locale) ]
        , saveButton
        ]
