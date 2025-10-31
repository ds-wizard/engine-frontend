module Wizard.Pages.Settings.LookAndFeel.View exposing (view)

import Common.Components.FontAwesome exposing (faDelete)
import Common.Components.Form as Form
import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Components.Page as Page
import Common.Utils.Form as Form
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Markdown as Markdown
import Form exposing (Form)
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, a, div, hr, img, label, span, text)
import Html.Attributes exposing (attribute, class, placeholder, src)
import Html.Events exposing (onClick)
import Html.Extra as Html
import String.Format as String
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Wizard.Api.Models.EditableConfig.EditableLookAndFeelConfig exposing (EditableLookAndFeelConfig)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Settings.Common.FontAwesome as FontAwesome
import Wizard.Pages.Settings.Generic.Msgs as GenericMsgs exposing (Msg)
import Wizard.Pages.Settings.LookAndFeel.Models exposing (Model)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewForm appState model) model.config


viewForm : AppState -> Model -> config -> Html Msg
viewForm appState model _ =
    let
        headerTitle =
            if Admin.isEnabled appState.config.admin then
                gettext "Menu" appState.locale

            else
                gettext "Look & Feel" appState.locale

        form =
            Form.initDynamic appState (GenericMsgs.FormMsg Form.Submit) model.savingConfig
                |> Form.setFormView (formView appState model.form)
                |> Form.setFormChanged (model.formRemoved || Form.containsChanges model.form)
                |> Form.setWide
                |> Form.viewDynamic
    in
    div [ class "LookAndFeel" ]
        [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.settingsLookAndFeel) headerTitle
        , form
        ]


formView : AppState -> Form FormError EditableLookAndFeelConfig -> Html Msg
formView appState form =
    let
        formWrap =
            Html.map GenericMsgs.FormMsg

        appTitleSettings =
            if Admin.isEnabled appState.config.admin then
                []

            else
                let
                    appTitleGroup =
                        div [ class "row" ]
                            [ div [ class "col-8" ]
                                [ formWrap <| FormGroup.inputAttrs [ placeholder LookAndFeelConfig.defaultAppTitle ] appState.locale form "appTitle" (gettext "Application Title" appState.locale)
                                , FormExtra.mdAfter (gettext "Full name of the DSW instance (displayed, for example, in the browser tab title or before login)." appState.locale)
                                ]
                            , div
                                [ class "col-4" ]
                                [ img [ class "settings-img", src "/wizard/assets/settings/app-title.png" ] []
                                ]
                            ]

                    appTitleShortGroup =
                        div [ class "row mt-5" ]
                            [ div [ class "col-8" ]
                                [ formWrap <| FormGroup.inputAttrs [ placeholder LookAndFeelConfig.defaultAppTitleShort ] appState.locale form "appTitleShort" (gettext "Short Application Title" appState.locale)
                                , FormExtra.mdAfter (gettext "Short name of the DSW instance (displayed, for example, on top of the navigation bar). Short title can be the same as the application title if it is short enough." appState.locale)
                                ]
                            , div [ class "col-4" ]
                                [ img [ class "settings-img", src "/wizard/assets/settings/app-title-short.png" ] []
                                ]
                            ]
                in
                [ appTitleGroup
                , appTitleShortGroup
                , hr [] []
                ]
    in
    div []
        (appTitleSettings
            ++ [ div [ class "input-table mt-5" ]
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
                            [ img [ class "settings-img", src "/wizard/assets/settings/custom-menu-links.png" ] [] ]
                        ]
                    , div [ class "row mt-3" ]
                        [ div [ class "col" ]
                            [ customMenuLinksHeader appState form
                            , formWrap <| FormGroup.list appState.locale (customMenuLinkItemView appState) form "customMenuLinks" "" (gettext "Add link" appState.locale)
                            ]
                        ]
                    ]
               ]
        )


customMenuLinksHeader : AppState -> Form FormError EditableLookAndFeelConfig -> Html msg
customMenuLinksHeader appState form =
    let
        isEmpty =
            List.isEmpty (Form.getListIndexes "customMenuLinks" form)
    in
    if isEmpty then
        Html.nothing

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
            FormGroup.getErrors appState.locale iconField (gettext "Icon" appState.locale)

        ( titleError, titleErrorClass ) =
            FormGroup.getErrors appState.locale titleField (gettext "Title" appState.locale)

        ( urlError, urlErrorClass ) =
            FormGroup.getErrors appState.locale urlField (gettext "URL" appState.locale)
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
                [ faDelete ]
            ]
        , iconError
        , titleError
        , urlError
        ]
