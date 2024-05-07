module Wizard.Settings.Submission.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, label, p, strong, text)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onClick)
import List.Extra as List
import Shared.Data.DocumentTemplateSuggestion as DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Shared.Data.EditableConfig.EditableSubmissionConfig exposing (EditableSubmissionConfig)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Markdown as Markdown
import Shared.Utils exposing (getOrganizationAndItemId, httpMethodOptions)
import Uuid
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Page as Page
import Wizard.Settings.Generic.Msgs as GenericMsgs
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.Submission.Models exposing (Model)
import Wizard.Settings.Submission.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewConfig appState model) model.templates


viewConfig : AppState -> Model -> List DocumentTemplateSuggestion -> Html Msg
viewConfig appState model templates =
    GenericView.view (viewProps templates) appState model.genericModel


viewProps : List DocumentTemplateSuggestion -> GenericView.ViewProps EditableSubmissionConfig Msg
viewProps templates =
    { locTitle = gettext "Document Submission"
    , locSave = gettext "Save"
    , formView = formView templates
    , wrapMsg = GenericMsg << GenericMsgs.FormMsg
    }


formView : List DocumentTemplateSuggestion -> AppState -> Form FormError EditableSubmissionConfig -> Html Msg
formView templates appState form =
    let
        enabled =
            Maybe.withDefault False (Form.getFieldAsBool "enabled" form).value

        servicesInput =
            if enabled then
                FormGroup.list appState (serviceFormView appState templates) form "services" (gettext "Services" appState.locale) (gettext "Add service" appState.locale)

            else
                emptyNode
    in
    Html.map (GenericMsg << GenericMsgs.FormMsg) <|
        div []
            [ FormGroup.toggle form "enabled" (gettext "Enabled" appState.locale)
            , FormExtra.mdAfter (gettext "If enabled, you can configure external services for uploading documents from the instance." appState.locale)
            , servicesInput
            ]


serviceFormView : AppState -> List DocumentTemplateSuggestion -> Form FormError EditableSubmissionConfig -> Int -> Html Form.Msg
serviceFormView appState templates form i =
    let
        field name =
            "services." ++ String.fromInt i ++ "." ++ name

        idField =
            field "id"

        nameField =
            field "name"

        descriptionField =
            field "description"

        supportedFormatsField =
            field "supportedFormats"

        propsField =
            field "props"

        requestField =
            field "request"
    in
    div [ class "card bg-light mb-4" ]
        [ div [ class "card-body" ]
            [ div [ class "row" ]
                [ div [ class "col" ]
                    [ FormGroup.input appState form idField (gettext "ID" appState.locale) ]
                , div [ class "col text-end" ]
                    [ a [ class "btn btn-danger with-icon", onClick (Form.RemoveItem "services" i) ]
                        [ faSet "_global.delete" appState
                        , text (gettext "Remove" appState.locale)
                        ]
                    ]
                ]
            , FormGroup.input appState form nameField (gettext "Name" appState.locale)
            , FormGroup.markdownEditor appState form descriptionField (gettext "Description" appState.locale)
            , div [ class "input-table" ]
                [ label [] [ text (gettext "Supported Formats" appState.locale) ]
                , p [ class "text-muted" ] [ text (gettext "Select document templates and formats that can be submitted using this submission service." appState.locale) ]
                , FormGroup.list appState (supportedFormatFormView appState templates supportedFormatsField) form supportedFormatsField "" (gettext "Add format" appState.locale)
                ]
            , div [ class "input-table" ]
                [ label [] [ text (gettext "User Properties" appState.locale) ]
                , Markdown.toHtml [ class "text-muted text-justify" ] (gettext "You can create properties that can be set by each user in their profile. Then, you can use these properties in the request settings. For example, if you want each user to use their own authorization token, you can create a property called `Token` and use it in request headers as `${Token}`. Users will be able to set the token in their profile settings." appState.locale)
                , FormGroup.list appState (propFormView appState propsField) form propsField "" (gettext "Add property" appState.locale)
                ]
            , requestFormView appState form requestField
            ]
        ]


supportedFormatFormView : AppState -> List DocumentTemplateSuggestion -> String -> Form FormError EditableSubmissionConfig -> Int -> Html Form.Msg
supportedFormatFormView appState templates prefix form index =
    let
        field name =
            prefix ++ "." ++ String.fromInt index ++ "." ++ name

        templateField =
            Form.getFieldAsString (field "template") form

        templateIdField =
            Form.getFieldAsString (field "templateId") form

        formatUuidField =
            Form.getFieldAsString (field "formatUuid") form

        ( templateIdError, templateIdErrorClass ) =
            FormGroup.getErrors appState templateIdField (gettext "Document Template" appState.locale)

        ( formatUuidError, formatUuidErrorClass ) =
            FormGroup.getErrors appState formatUuidField (gettext "Format" appState.locale)

        defaultOption =
            ( "", "--" )

        templateOptions =
            DocumentTemplateSuggestion.createOptions templates

        templateToTemplateVersionOptions template =
            templates
                |> List.filter (.id >> getOrganizationAndItemId >> (==) template)
                |> List.sortWith (\a b -> Version.compare b.version a.version)
                |> List.map (\t -> ( t.id, Version.toString t.version ))
                |> (::) defaultOption

        templateVersionOptions =
            templateField.value
                |> Maybe.map templateToTemplateVersionOptions
                |> Maybe.withDefault []

        formatOptions =
            templateIdField.value
                |> Maybe.andThen (\uuid -> List.find (.id >> (==) uuid) templates)
                |> Maybe.map (.formats >> List.map (\f -> ( Uuid.toString f.uuid, f.name )) >> (::) defaultOption)
                |> Maybe.withDefault []
    in
    div [ class "input-group mb-2" ]
        [ Input.selectInput templateOptions templateField [ class "form-select", class templateIdErrorClass ]
        , Input.selectInput templateVersionOptions templateIdField [ class "form-select", class templateIdErrorClass ]
        , Input.selectInput formatOptions formatUuidField [ class "form-select", class formatUuidErrorClass ]
        , button [ class "btn btn-link text-danger", onClick (Form.RemoveItem prefix index), type_ "button" ]
            [ faSet "_global.delete" appState ]
        , templateIdError
        , formatUuidError
        ]


propFormView : AppState -> String -> Form FormError EditableSubmissionConfig -> Int -> Html Form.Msg
propFormView appState prefix form index =
    let
        field =
            Form.getFieldAsString (prefix ++ "." ++ String.fromInt index) form
    in
    div [ class "input-group mb-2" ]
        [ Input.textInput field [ class "form-control" ]
        , button [ class "btn btn-link text-danger", onClick (Form.RemoveItem prefix index), type_ "button" ]
            [ faSet "_global.delete" appState ]
        ]


requestFormView : AppState -> Form FormError EditableSubmissionConfig -> String -> Html Form.Msg
requestFormView appState form prefix =
    let
        field name =
            prefix ++ "." ++ name

        urlField =
            field "url"

        methodField =
            field "method"

        headersField =
            field "headers"

        multipartEnabledField =
            field "multipart.enabled"

        multipartEnabled =
            Maybe.withDefault False (Form.getFieldAsBool multipartEnabledField form).value

        multipartFileNameInput =
            if multipartEnabled then
                let
                    multipartFileName =
                        field "multipart.fileName"
                in
                div [ class "nested-group" ]
                    [ FormGroup.input appState form multipartFileName (gettext "Multipart File Name" appState.locale)
                    ]

            else
                emptyNode
    in
    div []
        [ strong [] [ text (gettext "Request" appState.locale) ]
        , div [ class "nested-group mt-2" ]
            [ FormGroup.select appState httpMethodOptions form methodField (gettext "Method" appState.locale)
            , FormGroup.input appState form urlField (gettext "URL" appState.locale)
            , FormGroup.list appState (headerFormView appState headersField) form headersField (gettext "Headers" appState.locale) (gettext "Add header" appState.locale)
            , FormGroup.toggle form multipartEnabledField (gettext "Multipart" appState.locale)
            , FormExtra.mdAfter (gettext "If enabled, file will be sent using multipart request. Otherwise, it will be directly in the request body." appState.locale)
            , multipartFileNameInput
            ]
        ]


headerFormView : AppState -> String -> Form FormError EditableSubmissionConfig -> Int -> Html Form.Msg
headerFormView appState prefix form index =
    let
        field name =
            prefix ++ "." ++ String.fromInt index ++ "." ++ name

        headerField =
            Form.getFieldAsString (field "key") form

        valueField =
            Form.getFieldAsString (field "value") form

        ( headerError, headerErrorClass ) =
            FormGroup.getErrors appState headerField (gettext "Header" appState.locale)

        ( valueError, valueErrorClass ) =
            FormGroup.getErrors appState valueField (gettext "Value" appState.locale)
    in
    div [ class "input-group mb-2" ]
        [ Input.textInput headerField [ placeholder (gettext "Header" appState.locale), class "form-control", class headerErrorClass ]
        , Input.textInput valueField [ placeholder (gettext "Value" appState.locale), class "form-control", class valueErrorClass ]
        , button [ class "btn btn-link text-danger", onClick (Form.RemoveItem prefix index), type_ "button" ]
            [ faSet "_global.delete" appState ]
        , headerError
        , valueError
        ]
