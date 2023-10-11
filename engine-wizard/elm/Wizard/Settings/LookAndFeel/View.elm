module Wizard.Settings.LookAndFeel.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, a, div, form, hr, img, label, span, text)
import Html.Attributes exposing (attribute, class, placeholder, src)
import Html.Events exposing (onClick, onSubmit)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Shared.Data.EditableConfig.EditableLookAndFeelConfig exposing (EditableLookAndFeelConfig)
import Shared.Form as Form
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Markdown as Markdown
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (wideDetailClass)
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Settings.Common.FontAwesome as FontAwesome
import Wizard.Settings.Generic.Msgs as GenericMsgs exposing (Msg)
import Wizard.Settings.LookAndFeel.Models exposing (Model)


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewForm appState model) model.config


viewForm : AppState -> Model -> config -> Html Msg
viewForm appState model _ =
    let
        formActionsConfig =
            { text = Nothing
            , actionResult = model.savingConfig
            , formChanged = model.formRemoved || Form.containsChanges model.form
            , wide = True
            }
    in
    div [ wideDetailClass "LookAndFeel" ]
        [ Page.header (gettext "Look & Feel" appState.locale) []
        , form [ onSubmit (GenericMsgs.FormMsg Form.Submit) ]
            [ FormResult.errorOnlyView appState model.savingConfig
            , formView appState model.form
            , FormActions.viewDynamic formActionsConfig appState
            ]
        ]


formView : AppState -> Form FormError EditableLookAndFeelConfig -> Html Msg
formView appState form =
    let
        formWrap =
            Html.map GenericMsgs.FormMsg
    in
    div []
        [ div [ class "row" ]
            [ div [ class "col-8" ]
                [ formWrap <| FormGroup.inputAttrs [ placeholder LookAndFeelConfig.defaultAppTitle ] appState form "appTitle" (gettext "Application Title" appState.locale)
                , FormExtra.mdAfter (gettext "Full name of the DSW instance (displayed, for example, in the browser tab title or before login)." appState.locale)
                ]
            , div
                [ class "col-4" ]
                [ img [ class "settings-img", src "/wizard/img/settings/app-title.png" ] []
                ]
            ]
        , div [ class "row mt-5" ]
            [ div [ class "col-8" ]
                [ formWrap <| FormGroup.inputAttrs [ placeholder LookAndFeelConfig.defaultAppTitleShort ] appState form "appTitleShort" (gettext "Short Application Title" appState.locale)
                , FormExtra.mdAfter (gettext "Short name of the DSW instance (displayed, for example, on top of the navigation bar). Short title can be the same as the application title if it is short enough." appState.locale)
                ]
            , div [ class "col-4" ]
                [ img [ class "settings-img", src "/wizard/img/settings/app-title-short.png" ] []
                ]
            ]
        , hr [] []
        , div [ class "input-table mt-5" ]
            [ div [ class "row" ]
                [ div [ class "col-8" ]
                    [ label [] [ text (gettext "Custom Menu Links" appState.locale) ]
                    , Markdown.toHtml [ class "form-text text-muted" ]
                        (String.format
                            (gettext "Configure additional links in the menu. Choose any free icon from the [Font Awesome](%s), e.g. *fas fa-magic*. Check *New window* if you want to open the link in a new window." appState.locale)
                            [ FontAwesome.fontAwesomeLink ]
                        )
                    ]
                , div [ class "col-4" ]
                    [ img [ class "settings-img", src "/wizard/img/settings/custom-menu-links.png" ] [] ]
                ]
            , div [ class "row mt-3" ]
                [ div [ class "col" ]
                    [ customMenuLinksHeader appState form
                    , formWrap <| FormGroup.list appState (customMenuLinkItemView appState) form "customMenuLinks" "" (gettext "Add link" appState.locale)
                    ]
                ]
            ]
        ]


customMenuLinksHeader : AppState -> Form FormError EditableLookAndFeelConfig -> Html msg
customMenuLinksHeader appState form =
    let
        isEmpty =
            List.length (Form.getListIndexes "customMenuLinks" form) == 0
    in
    if isEmpty then
        emptyNode

    else
        div [ class "row input-table-header" ]
            [ div [ class "col-2" ]
                [ text (gettext "Icon" appState.locale) ]
            , div [ class "col-3" ]
                [ text (gettext "Title" appState.locale) ]
            , div [ class "col-4" ]
                [ text (gettext "URL" appState.locale) ]
            , div [ class "col-3" ]
                [ text (gettext "New window" appState.locale) ]
            ]


customMenuLinkItemView : AppState -> Form FormError EditableLookAndFeelConfig -> Int -> Html Form.Msg
customMenuLinkItemView appState form i =
    let
        iconField =
            Form.getFieldAsString ("customMenuLinks." ++ String.fromInt i ++ ".icon") form

        titleField =
            Form.getFieldAsString ("customMenuLinks." ++ String.fromInt i ++ ".title") form

        urlField =
            Form.getFieldAsString ("customMenuLinks." ++ String.fromInt i ++ ".url") form

        newWindowField =
            Form.getFieldAsBool ("customMenuLinks." ++ String.fromInt i ++ ".newWindow") form

        ( iconError, iconErrorClass ) =
            FormGroup.getErrors appState iconField (gettext "Icon" appState.locale)

        ( titleError, titleErrorClass ) =
            FormGroup.getErrors appState titleField (gettext "Title" appState.locale)

        ( urlError, urlErrorClass ) =
            FormGroup.getErrors appState urlField (gettext "URL" appState.locale)
    in
    div [ class "row" ]
        [ div [ class "col-2" ]
            [ Input.textInput iconField [ class <| "form-control " ++ iconErrorClass, attribute "data-cy" "input-icon" ] ]
        , div [ class "col-3" ]
            [ Input.textInput titleField [ class <| "form-control " ++ titleErrorClass, attribute "data-cy" "input-title" ] ]
        , div [ class "col-4" ]
            [ Input.textInput urlField [ class <| " form-control " ++ urlErrorClass, attribute "data-cy" "input-url" ] ]
        , div [ class "col-2" ]
            [ label [ class "checkbox-label form-check-label form-check-toggle" ]
                [ Input.checkboxInput newWindowField [ class "form-check-input", attribute "data-cy" "input-new-window" ]
                , span [] []
                ]
            ]
        , div [ class "col-1 text-end" ]
            [ a [ class "btn btn-link text-danger", onClick (Form.RemoveItem "customMenuLinks" i), attribute "data-cy" "button-remove" ]
                [ faSet "_global.delete" appState ]
            ]
        , iconError
        , titleError
        , urlError
        ]
