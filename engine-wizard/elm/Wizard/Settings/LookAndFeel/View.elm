module Wizard.Settings.LookAndFeel.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Html, a, div, hr, img, label)
import Html.Attributes exposing (attribute, class, placeholder, src)
import Html.Events exposing (onClick)
import Markdown
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy, wideDetailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Settings.Generic.Msgs as GenericMsgs
import Wizard.Settings.LookAndFeel.LogoUploadModal as LogoUploadModal
import Wizard.Settings.LookAndFeel.Models exposing (Model)
import Wizard.Settings.LookAndFeel.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.LookAndFeel.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.LookAndFeel.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewForm appState model) model.genericModel.config


viewForm : AppState -> Model -> config -> Html Msg
viewForm appState model _ =
    div [ wideDetailClass "LookAndFeel" ]
        [ Page.header (l_ "title" appState) []
        , div []
            [ FormResult.errorOnlyView appState model.genericModel.savingConfig
            , formView appState model.genericModel.form
            , div [ class "mt-5" ]
                [ ActionButton.buttonWithAttrs appState
                    (ActionButton.ButtonWithAttrsConfig (l_ "save" appState)
                        model.genericModel.savingConfig
                        (GenericMsg <| GenericMsgs.FormMsg Form.Submit)
                        False
                        [ dataCy "form_submit" ]
                    )
                ]
            ]
        , Html.map LogoUploadModalMsg <| LogoUploadModal.view appState model.logoUploadModalModel
        ]



--colorOptionsDarker : List String
--colorOptionsDarker =
--    [ "#16A085"
--    , "#27AE60"
--    , "#2980B9"
--    , "#8E44AD"
--    , "#F39C12"
--    , "#D35400"
--    , "#C0392B"
--    ]
--
--
--colorOptionsLighter : List String
--colorOptionsLighter =
--    [ "#1ABC9C"
--    , "#2ECC71"
--    , "#3498DB"
--    , "#9B59B6"
--    , "#F1C40F"
--    , "#E67E22"
--    , "#E74C3C"
--    ]


formView : AppState -> Form FormError LookAndFeelConfig -> Html Msg
formView appState form =
    let
        formWrap =
            Html.map (GenericMsg << GenericMsgs.FormMsg)

        --    inputMsg field color =
        --        Form.Input field Form.Text (Field.String color)
        --
        --    colorButtonView field color =
        --        div [ class "color", style "background" color, onClick (inputMsg field color) ] []
        --
        --    colorPicker colorOptions field =
        --        div [ class "color-picker" ] (List.map (colorButtonView field) colorOptions)
        --logoPreview =
        --    div []
        --        [ div
        --            [ class "LogoPreview" ]
        --            [ span [ class "LogoPreview__Logo LogoPreview__Logo--Original" ] []
        --            , text (LookAndFeelConfig.getAppTitleShort appState.config.lookAndFeel)
        --            ]
        --        , div [ class "mt-2" ]
        --            [ button
        --                [ class "btn btn-secondary"
        --                , onClick (LogoUploadModalMsg (LogoUploadModal.SetOpen True))
        --                ]
        --                [ text "Change" ]
        --            ]
        --        ]
    in
    div []
        [ div [ class "row" ]
            [ div [ class "col-8" ]
                [ formWrap <| FormGroup.inputAttrs [ placeholder LookAndFeelConfig.defaultAppTitle ] appState form "appTitle" (l_ "form.appTitle" appState)
                , FormExtra.mdAfter (l_ "form.appTitle.desc" appState)
                ]
            , div
                [ class "col-4" ]
                [ img [ class "settings-img", src "/img/settings/app-title.png" ] []
                ]
            ]
        , div [ class "row mt-5" ]
            [ div [ class "col-8" ]
                [ formWrap <| FormGroup.inputAttrs [ placeholder LookAndFeelConfig.defaultAppTitleShort ] appState form "appTitleShort" (l_ "form.appTitleShort" appState)
                , FormExtra.mdAfter (l_ "form.appTitleShort.desc" appState)
                ]
            , div [ class "col-4" ]
                [ img [ class "settings-img", src "/img/settings/app-title-short.png" ] []
                ]
            ]

        --, div [ class "row mt-5" ]
        --    [ div [ class "col-8" ]
        --        [ FormGroup.plainGroup logoPreview "Logo"
        --        , FormExtra.mdAfter "Logo is used next to the application name in the menu. It is recommended to use a square image."
        --        ]
        --    , div [ class "col-4" ]
        --        [ img [ class "settings-img", src "/img/settings/logo.png" ] []
        --        ]
        --    ]
        --, div [ class "row mt-5" ]
        --    [ div [ class "col-6" ]
        --        [ formWrap <| FormGroup.input appState form "stylePrimaryColor" "Primary Color"
        --        , colorPicker colorOptionsDarker "stylePrimaryColor"
        --        ]
        --    , div [ class "col-6" ]
        --        [ formWrap <| FormGroup.input appState form "styleIllustrationsColor" "Illustrations Color"
        --        , colorPicker colorOptionsLighter "styleIllustrationsColor"
        --        ]
        --    ]
        --, div [ class "row mt-5" ]
        --    [ div [ class "col-12" ]
        --        [ viewAppPreview form
        --        ]
        --    ]
        , hr [] []
        , div [ class "input-table mt-5" ]
            [ div [ class "row" ]
                [ div [ class "col-8" ]
                    [ label [] [ lx_ "form.customMenuLinks" appState ]
                    , Markdown.toHtml [ class "form-text text-muted" ] (l_ "form.customMenuLinks.desc" appState)
                    ]
                , div [ class "col-4" ]
                    [ img [ class "settings-img", src "/img/settings/custom-menu-links.png" ] [] ]
                ]
            , div [ class "row mt-3" ]
                [ div [ class "col" ]
                    [ customMenuLinksHeader appState form
                    , formWrap <| FormGroup.list appState (customMenuLinkItemView appState) form "customMenuLinks" ""
                    ]
                ]
            ]
        , hr [] []
        , div [ class "row mt-5" ]
            [ div [ class "col-12" ]
                [ label [] [ lx_ "form.loginInfo" appState ] ]
            , div [ class "col-8" ]
                [ formWrap <| FormGroup.markdownEditor appState form "loginInfo" ""
                , FormExtra.mdAfter (l_ "form.loginInfo.desc" appState)
                ]
            , div [ class "col-4" ]
                [ img [ class "settings-img", src "/img/settings/login-info-text.png" ] []
                ]
            ]
        ]



--viewAppPreview : Form FormError LookAndFeelConfig -> Html Form.Msg
--viewAppPreview form =
--    let
--        toBackgroundColorStyle color =
--            "background-color: " ++ color
--
--        toBackgroundAndBorderColorStyle color =
--            "background-color: " ++ color ++ "; border-color: " ++ color
--
--        toColorStyle color =
--            "color: " ++ color
--
--        appTitleValue =
--            (Form.getFieldAsString "appTitleShort" form).value
--                |> Maybe.andThen String.toMaybe
--                |> Maybe.withDefault LookAndFeelConfig.defaultAppTitleShort
--
--        stylePrimaryColorValue =
--            (Form.getFieldAsString "stylePrimaryColor" form).value
--                |> Maybe.andThen String.toMaybe
--
--        primaryColorBackgroundStyle =
--            stylePrimaryColorValue
--                |> Maybe.unwrap "" toBackgroundColorStyle
--                |> attribute "style"
--
--        primaryColorBackgroundAndBorderStyle =
--            stylePrimaryColorValue
--                |> Maybe.unwrap "" toBackgroundAndBorderColorStyle
--                |> attribute "style"
--
--        illustrationsColorStyle =
--            (Form.getFieldAsString "styleIllustrationsColor" form).value
--                |> Maybe.andThen String.toMaybe
--                |> Maybe.unwrap "" toColorStyle
--                |> attribute "style"
--    in
--    div [ class "AppPreview" ]
--        [ div [ class "AppPreview__Panel", primaryColorBackgroundStyle ]
--            [ a [ class "logo" ] [ span [ class "logo-full" ] [ text appTitleValue ] ]
--            ]
--        , div [ class "AppPreview__Content" ]
--            [ Undraw.teachingWithAttrs [ illustrationsColorStyle ]
--            , button [ class "btn btn-primary btn-with-loader mt-4", primaryColorBackgroundAndBorderStyle ] [ text "Button" ]
--            ]
--        , div [ class "AppPreview__Overlay" ] []
--        ]


customMenuLinksHeader : AppState -> Form FormError LookAndFeelConfig -> Html msg
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
                [ lx_ "form.customMenuLinks.icon" appState ]
            , div [ class "col-3" ]
                [ lx_ "form.customMenuLinks.title" appState ]
            , div [ class "col-4" ]
                [ lx_ "form.customMenuLinks.url" appState ]
            , div [ class "col-3" ]
                [ lx_ "form.customMenuLinks.newWindow" appState ]
            ]


customMenuLinkItemView : AppState -> Form FormError LookAndFeelConfig -> Int -> Html Form.Msg
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
            FormGroup.getErrors appState iconField "Icon"

        ( titleError, titleErrorClass ) =
            FormGroup.getErrors appState titleField "Title"

        ( urlError, urlErrorClass ) =
            FormGroup.getErrors appState urlField "URL"
    in
    div [ class "row" ]
        [ div [ class "col-2" ]
            [ Input.textInput iconField [ class <| "form-control " ++ iconErrorClass, attribute "data-cy" "input-icon" ] ]
        , div [ class "col-3" ]
            [ Input.textInput titleField [ class <| "form-control " ++ titleErrorClass, attribute "data-cy" "input-title" ] ]
        , div [ class "col-4" ]
            [ Input.textInput urlField [ class <| " form-control " ++ urlErrorClass, attribute "data-cy" "input-url" ] ]
        , div [ class "col-2" ]
            [ label [ class "checkbox-label" ] [ Input.checkboxInput newWindowField [ attribute "data-cy" "input-new-window" ] ]
            ]
        , div [ class "col-1 text-right" ]
            [ a [ class "btn btn-link text-danger", onClick (Form.RemoveItem "customMenuLinks" i), attribute "data-cy" "button-remove" ]
                [ faSet "_global.delete" appState ]
            ]
        , iconError
        , titleError
        , urlError
        ]
