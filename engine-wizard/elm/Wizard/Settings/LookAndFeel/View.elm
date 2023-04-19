module Wizard.Settings.LookAndFeel.View exposing (view)

import Form exposing (Form)
import Form.Field as Field
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, form, hr, img, label, span, text)
import Html.Attributes exposing (attribute, class, placeholder, src, style)
import Html.Events exposing (onClick, onSubmit)
import Maybe.Extra as Maybe
import Set
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Shared.Data.EditableConfig.EditableLookAndFeelConfig exposing (EditableLookAndFeelConfig)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet, faSetFw)
import Shared.Markdown as Markdown
import Shared.Undraw as Undraw
import String.Extra as String
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (wideDetailClass)
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Settings.Common.FontAwesome as FontAwesome
import Wizard.Settings.Generic.Msgs as GenericMsgs
import Wizard.Settings.LookAndFeel.LogoUploadModal as LogoUploadModal
import Wizard.Settings.LookAndFeel.Models exposing (Model)
import Wizard.Settings.LookAndFeel.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewForm appState model) model.genericModel.config


viewForm : AppState -> Model -> config -> Html Msg
viewForm appState model _ =
    let
        formActionsConfig =
            { text = Nothing
            , actionResult = model.genericModel.savingConfig
            , formChanged = model.genericModel.formRemoved || (not << Set.isEmpty) (Form.getChangedFields model.genericModel.form)
            , wide = True
            }
    in
    div [ wideDetailClass "LookAndFeel" ]
        [ Page.header (gettext "Look & Feel" appState.locale) []
        , form [ onSubmit (GenericMsg <| GenericMsgs.FormMsg Form.Submit) ]
            [ FormResult.errorOnlyView appState model.genericModel.savingConfig
            , formView appState model.genericModel.form
            , FormActions.viewDynamic formActionsConfig appState
            ]
        , Html.map LogoUploadModalMsg <| LogoUploadModal.view appState model.logoUploadModalModel
        ]


colorOptionsDarker : List String
colorOptionsDarker =
    [ "#16A085"
    , "#27AE60"
    , "#2980B9"
    , "#8E44AD"
    , "#C67d0A"
    , "#D35400"
    , "#C0392B"
    ]


colorOptionsLighter : List String
colorOptionsLighter =
    [ "#1ABC9C"
    , "#2ECC71"
    , "#3498DB"
    , "#9B59B6"
    , "#F1C40F"
    , "#E67E22"
    , "#E74C3C"
    ]


formView : AppState -> Form FormError EditableLookAndFeelConfig -> Html Msg
formView appState form =
    let
        formWrap =
            Html.map (GenericMsg << GenericMsgs.FormMsg)

        inputMsg field color =
            Form.Input field Form.Text (Field.String color)

        colorButtonView field color =
            div [ class "color", style "background" color, onClick (inputMsg field color) ] []

        colorPicker colorOptions field =
            div [ class "color-picker" ] (List.map (colorButtonView field) colorOptions)

        clientCustomizations =
            if appState.config.feature.clientCustomizationEnabled then
                let
                    appTitleValue =
                        (Form.getFieldAsString "appTitleShort" form).value
                            |> Maybe.andThen String.toMaybe
                            |> Maybe.withDefault LookAndFeelConfig.defaultAppTitleShort

                    logoPreview =
                        div []
                            [ div
                                [ class "LogoPreview" ]
                                [ span [ class "LogoPreview__Logo LogoPreview__Logo--Original" ] []
                                , text appTitleValue
                                ]
                            , div [ class "mt-2" ]
                                [ button
                                    [ class "btn btn-secondary"
                                    , onClick (LogoUploadModalMsg (LogoUploadModal.SetOpen True))
                                    ]
                                    [ text (gettext "Change" appState.locale) ]
                                ]
                            ]
                in
                [ div [ class "row mt-5" ]
                    [ div [ class "col-8" ]
                        [ FormGroup.plainGroup logoPreview (gettext "Logo" appState.locale)
                        , FormExtra.mdAfter (gettext "Logo is used next to the application name in the menu. It is recommended to use a square image." appState.locale)
                        ]
                    , div [ class "col-4" ]
                        [ img [ class "settings-img", src "/img/settings/logo.png" ] []
                        ]
                    ]
                , div [ class "row mt-5" ]
                    [ div [ class "col-6" ]
                        [ formWrap <| FormGroup.input appState form "stylePrimaryColor" (gettext "Primary Color" appState.locale)
                        , formWrap <| colorPicker colorOptionsDarker "stylePrimaryColor"
                        ]
                    , div [ class "col-6" ]
                        [ formWrap <| FormGroup.input appState form "styleIllustrationsColor" (gettext "Illustrations Color" appState.locale)
                        , formWrap <| colorPicker colorOptionsLighter "styleIllustrationsColor"
                        ]
                    ]
                , div [ class "row mt-5" ]
                    [ div [ class "col-12" ]
                        [ formWrap <| viewAppPreview appState form
                        ]
                    ]
                ]

            else
                []
    in
    div []
        ([ div [ class "row" ]
            [ div [ class "col-8" ]
                [ formWrap <| FormGroup.inputAttrs [ placeholder LookAndFeelConfig.defaultAppTitle ] appState form "appTitle" (gettext "Application Title" appState.locale)
                , FormExtra.mdAfter (gettext "Full name of the DSW instance (displayed, for example, in the browser tab title or before login)." appState.locale)
                ]
            , div
                [ class "col-4" ]
                [ img [ class "settings-img", src "/img/settings/app-title.png" ] []
                ]
            ]
         , div [ class "row mt-5" ]
            [ div [ class "col-8" ]
                [ formWrap <| FormGroup.inputAttrs [ placeholder LookAndFeelConfig.defaultAppTitleShort ] appState form "appTitleShort" (gettext "Short Application Title" appState.locale)
                , FormExtra.mdAfter (gettext "Short name of the DSW instance (displayed, for example, on top of the navigation bar). Short title can be the same as the application title if it is short enough." appState.locale)
                ]
            , div [ class "col-4" ]
                [ img [ class "settings-img", src "/img/settings/app-title-short.png" ] []
                ]
            ]
         ]
            ++ clientCustomizations
            ++ [ hr [] []
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
                            [ img [ class "settings-img", src "/img/settings/custom-menu-links.png" ] [] ]
                        ]
                    , div [ class "row mt-3" ]
                        [ div [ class "col" ]
                            [ customMenuLinksHeader appState form
                            , formWrap <| FormGroup.list appState (customMenuLinkItemView appState) form "customMenuLinks" "" (gettext "Add link" appState.locale)
                            ]
                        ]
                    ]
               ]
        )


viewAppPreview : AppState -> Form FormError EditableLookAndFeelConfig -> Html Form.Msg
viewAppPreview appState form =
    let
        toBackgroundColorStyle color =
            "background-color: " ++ color

        toBackgroundAndBorderColorStyle color =
            "background-color: " ++ color ++ "; border-color: " ++ color

        toColorStyle color =
            "color: " ++ color

        appTitleValue =
            (Form.getFieldAsString "appTitleShort" form).value
                |> Maybe.andThen String.toMaybe
                |> Maybe.withDefault LookAndFeelConfig.defaultAppTitleShort

        stylePrimaryColorValue =
            (Form.getFieldAsString "stylePrimaryColor" form).value
                |> Maybe.andThen String.toMaybe

        primaryColorStyle =
            stylePrimaryColorValue
                |> Maybe.unwrap "" toColorStyle
                |> attribute "style"

        primaryColorBackgroundStyle =
            stylePrimaryColorValue
                |> Maybe.unwrap "" toBackgroundColorStyle
                |> attribute "style"

        primaryColorBackgroundAndBorderStyle =
            stylePrimaryColorValue
                |> Maybe.unwrap "" toBackgroundAndBorderColorStyle
                |> attribute "style"

        illustrationsColorStyle =
            (Form.getFieldAsString "styleIllustrationsColor" form).value
                |> Maybe.andThen String.toMaybe
                |> Maybe.unwrap "" toColorStyle
                |> attribute "style"
    in
    div [ class "AppPreview" ]
        [ div [ class "AppPreview__Panel" ]
            [ a [ class "logo" ] [ span [ class "logo-full" ] [ text appTitleValue ] ]
            , div [ class "menu-button", primaryColorStyle ]
                [ div [ class "menu-button-color", primaryColorBackgroundStyle ] []
                , faSetFw "menu.projects" appState
                , text (gettext "Projects" appState.locale)
                ]
            ]
        , div [ class "AppPreview__Content" ]
            [ Undraw.teachingWithAttrs [ illustrationsColorStyle ]
            , button [ class "btn btn-primary btn-with-loader mt-4", primaryColorBackgroundAndBorderStyle ] [ text "Button" ]
            ]
        , div [ class "AppPreview__Overlay" ] []
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
