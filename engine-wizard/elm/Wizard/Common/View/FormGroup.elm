module Wizard.Common.View.FormGroup exposing
    ( codeView
    , formGroup
    , formGroupCustom
    , formatRadioGroup
    , getErrors
    , htmlRadioGroup
    , input
    , inputAttrs
    , inputWithTypehints
    , list
    , listWithCustomMsg
    , listWithHeader
    , markdownEditor
    , optionalWrapper
    , password
    , passwordWithStrength
    , plainGroup
    , resizableTextarea
    , richRadioGroup
    , select
    , selectWithDisabled
    , simpleDate
    , textView
    , textarea
    , textareaAttrs
    , toggle
    , viewList
    )

import Form exposing (Form, InputType(..), Msg(..))
import Form.Field as Field
import Form.Input as Input
import Html exposing (Html, a, button, code, div, label, li, option, p, span, text, ul)
import Html.Attributes exposing (autocomplete, checked, class, classList, disabled, for, id, name, rows, selected, type_, value)
import Html.Events exposing (on, onBlur, onCheck, onClick, onFocus, onMouseDown, targetValue)
import Json.Decode as Json
import Maybe.Extra as Maybe
import Shared.Data.Template.TemplateFormat exposing (TemplateFormat)
import Shared.Form exposing (errorToString)
import Shared.Form.FormError exposing (FormError(..))
import Shared.Html exposing (emptyNode, fa)
import Shared.Locale exposing (l, lx)
import Shared.Markdown as Markdown
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.PasswordBar as PasswordBar
import Wizard.Common.Html.Attribute exposing (dataCy, grammarlyAttributes)
import Wizard.Common.View.Flash as Flash


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.View.FormGroup"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.View.FormGroup"


optionalWrapper : AppState -> Html Form.Msg -> Html Form.Msg
optionalWrapper appState content =
    div [ class "form-group form-group-optional-wrapper" ]
        [ span [ class "optional-label" ] [ lx_ "optional" appState ]
        , content
        ]


{-| Helper for creating form group with text input field.
-}
input : AppState -> Form FormError o -> String -> String -> Html Form.Msg
input =
    formGroup Input.textInput []


inputAttrs : List (Html.Attribute Form.Msg) -> AppState -> Form FormError o -> String -> String -> Html.Html Form.Msg
inputAttrs =
    formGroup Input.textInput


inputWithTypehints : List String -> AppState -> Form FormError o -> String -> String -> Html Form.Msg
inputWithTypehints options appState form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, errorClass ) =
            getErrors appState field labelText

        typehintMessage =
            Form.Input fieldName Form.Text << Field.String

        contains a b =
            String.contains (String.toLower a) (String.toLower b)

        filteredOptions =
            case field.value of
                Just value ->
                    List.filter (contains value) options

                Nothing ->
                    options

        typehints =
            if field.hasFocus then
                ul [ class "typehints" ]
                    (List.map
                        (\option ->
                            li [ onMouseDown <| typehintMessage option, dataCy "form-group_typehints_item" ] [ text option ]
                        )
                        filteredOptions
                    )

            else
                emptyNode
    in
    div [ class "form-group" ]
        [ label [ for fieldName ] [ text labelText ]
        , Input.textInput field [ class <| "form-control " ++ errorClass, id fieldName, name fieldName, autocomplete False ]
        , typehints
        , error
        ]


{-| Helper for creating form group with password input field.
-}
password : AppState -> Form FormError o -> String -> String -> Html Form.Msg
password =
    formGroup Input.passwordInput []


passwordWithStrength : AppState -> Form FormError o -> String -> String -> Html Form.Msg
passwordWithStrength appState form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, errorClass ) =
            getErrors appState field labelText
    in
    div [ class "form-group" ]
        [ label [ for fieldName ] [ text labelText ]
        , Input.passwordInput field [ class ("form-control " ++ errorClass), id fieldName, name fieldName ]
        , PasswordBar.view (Maybe.withDefault "" field.value)
        , error
        ]


{-| Helper for creating form group with select field.
-}
select : AppState -> List ( String, String ) -> Form FormError o -> String -> String -> Html Form.Msg
select appState options =
    formGroup (Input.selectInput options) [] appState


selectWithDisabled : AppState -> List ( String, String, Bool ) -> Form FormError o -> String -> String -> Html Form.Msg
selectWithDisabled appState options =
    let
        input_ state attrs =
            let
                formAttrs =
                    [ on
                        "change"
                        (targetValue |> Json.map (Field.String >> Input state.path Select))
                    , onFocus (Focus state.path)
                    , onBlur (Blur state.path)
                    ]

                buildOption ( k, v, d ) =
                    option [ value k, selected (state.value == Just k), disabled d ] [ text v ]
            in
            Html.select (formAttrs ++ attrs) (List.map buildOption options)
    in
    formGroup input_ [] appState


richRadioGroup : AppState -> List ( String, String, String ) -> Form FormError o -> String -> String -> Html Form.Msg
richRadioGroup appState options =
    let
        radioInput state _ =
            let
                buildOption ( k, v, d ) =
                    div [ class "form-check", classList [ ( "form-check-selected", state.value == Just k ) ] ]
                        [ Html.input
                            [ value k
                            , checked (state.value == Just k)
                            , class "form-check-input"
                            , type_ "radio"
                            , id k
                            , onCheck (\_ -> Input state.path Form.Text <| Field.String k)
                            ]
                            []
                        , label [ class "form-check-label", for k ]
                            [ text v
                            , p [ class "form-text text-muted" ] [ Markdown.toHtml [] d ]
                            ]
                        ]
            in
            div [ class "form-radio-group" ] (List.map buildOption options)
    in
    formGroup radioInput [] appState


formatRadioGroup : AppState -> List TemplateFormat -> Form FormError o -> String -> String -> Html Form.Msg
formatRadioGroup appState options =
    let
        radioInput state _ =
            let
                buildOption : TemplateFormat -> Html Form.Msg
                buildOption format =
                    let
                        isDisabled =
                            appState.config.feature.pdfOnlyEnabled && not format.isPdf
                    in
                    label
                        [ class "export-link"
                        , classList
                            [ ( "export-link-selected", state.value == Just (Uuid.toString format.uuid) )
                            , ( "disabled", isDisabled )
                            ]
                        ]
                        [ Html.input
                            [ value (Uuid.toString format.uuid)
                            , checked (state.value == Just (Uuid.toString format.uuid))
                            , type_ "radio"
                            , name "format"
                            , onCheck (\_ -> Input state.path Form.Text <| Field.String <| Uuid.toString format.uuid)
                            , disabled isDisabled
                            ]
                            []
                        , fa format.icon
                        , text format.name
                        ]

                pdfOnlyInfo =
                    if appState.config.feature.pdfOnlyEnabled then
                        Flash.info appState (l_ "formatRadioGroup.pdfOnly" appState)

                    else
                        emptyNode
            in
            div []
                [ pdfOnlyInfo
                , div [ class "export-formats" ] (List.map buildOption options)
                ]
    in
    formGroup radioInput [] appState


htmlRadioGroup : AppState -> List ( String, Html Form.Msg ) -> Form FormError o -> String -> String -> Html Form.Msg
htmlRadioGroup appState options =
    let
        radioInput state _ =
            let
                buildOption ( k, html ) =
                    label
                        [ class "form-check"
                        , classList [ ( "form-check-selected", state.value == Just k ) ]
                        , dataCy ("form-group_html-radio-" ++ k)
                        ]
                        [ Html.input
                            [ value k
                            , checked (state.value == Just k)
                            , class "form-check-input"
                            , type_ "radio"
                            , id k
                            , onCheck (\_ -> Input state.path Form.Text <| Field.String k)
                            ]
                            []
                        , html
                        ]
            in
            div [ class "form-radio-group" ] (List.map buildOption options)
    in
    formGroup radioInput [] appState


{-| Helper for creating form group with textarea.
-}
textarea : AppState -> Form FormError o -> String -> String -> Html Form.Msg
textarea =
    formGroup Input.textArea grammarlyAttributes


textareaAttrs : List (Html.Attribute Form.Msg) -> AppState -> Form FormError o -> String -> String -> Html Form.Msg
textareaAttrs attrs =
    formGroup Input.textArea (attrs ++ grammarlyAttributes)


resizableTextarea : AppState -> Form FormError o -> String -> String -> Html Form.Msg
resizableTextarea appState form fieldName =
    let
        lines =
            (Form.getFieldAsString fieldName form).value
                |> Maybe.map (max 3 << List.length << String.split "\n")
                |> Maybe.withDefault 3

        attributes =
            [ rows lines, class "resizable-textarea" ] ++ grammarlyAttributes
    in
    formGroup Input.textArea attributes appState form fieldName


{-| Helper for creating form group with toggle
-}
toggle : Form FormError o -> String -> String -> Html Form.Msg
toggle form fieldName labelText =
    let
        field =
            Form.getFieldAsBool fieldName form
    in
    div [ class "form-check" ]
        [ label [ class "form-check-label form-check-toggle" ]
            [ Input.checkboxInput field [ class "form-check-input", name fieldName, id fieldName ]
            , span [] [ text labelText ]
            ]
        ]


list : AppState -> (Form FormError o -> Int -> Html Form.Msg) -> Form FormError o -> String -> String -> Html Form.Msg
list appState itemView form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, _ ) =
            getErrors appState field labelText
    in
    div [ class "form-group" ]
        [ label [] [ text labelText ]
        , div [] (List.map (itemView form) (Form.getListIndexes fieldName form))
        , div [ class "form-list-error" ] [ error ]
        , button
            [ class "btn btn-secondary"
            , onClick (Form.Append fieldName)
            , dataCy "form-group_list_add-button"
            ]
            [ lx_ "list.add" appState ]
        ]


listWithCustomMsg : AppState -> (Form.Msg -> msg) -> (Form FormError o -> Int -> Html msg) -> Form FormError o -> String -> String -> Html msg
listWithCustomMsg appState wrapMsg itemView form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, _ ) =
            getErrors appState field labelText
    in
    div [ class "form-group" ]
        [ label [] [ text labelText ]
        , div [] (List.map (itemView form) (Form.getListIndexes fieldName form))
        , div [ class "form-list-error" ] [ error ]
        , button
            [ class "btn btn-secondary"
            , onClick (wrapMsg <| Form.Append fieldName)
            , dataCy "form-group_list_add-button"
            ]
            [ lx_ "list.add" appState ]
        ]


listWithHeader : AppState -> Html Form.Msg -> (Form FormError o -> Int -> Html Form.Msg) -> Form FormError o -> String -> String -> Html Form.Msg
listWithHeader appState header itemView form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, _ ) =
            getErrors appState field labelText
    in
    div [ class "form-group" ]
        [ label [] [ text labelText ]
        , header
        , div [] (List.map (itemView form) (Form.getListIndexes fieldName form))
        , div [ class "form-list-error" ] [ error ]
        , button
            [ class "btn btn-secondary"
            , onClick (Form.Append fieldName)
            , dataCy "form-group_list_add-button"
            ]
            [ lx_ "list.add" appState ]
        ]


viewList : AppState -> (Form FormError o -> Int -> Html Form.Msg) -> Form FormError o -> String -> String -> Html Form.Msg
viewList appState itemView form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, _ ) =
            getErrors appState field labelText
    in
    div [ class "form-group" ]
        [ label [] [ text labelText ]
        , div [] (List.map (itemView form) (Form.getListIndexes fieldName form))
        , div [ class "form-list-error" ] [ error ]
        ]


markdownEditor : AppState -> Form FormError o -> String -> String -> Html Form.Msg
markdownEditor appState form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        previewActiveFieldName =
            fieldName ++ "-preview-active"

        ( error, errorClass ) =
            getErrors appState field labelText

        cardErrorClass =
            if String.isEmpty errorClass then
                ""

            else
                "border-danger"

        editorStateField =
            Form.getFieldAsBool previewActiveFieldName form

        previewActive =
            Maybe.withDefault False editorStateField.value

        valueString =
            Maybe.withDefault "" field.value

        content =
            if previewActive then
                Markdown.toHtml [] valueString

            else
                Input.textArea field
                    (grammarlyAttributes
                        ++ [ class <| "form-control " ++ errorClass
                           , id fieldName
                           , name fieldName
                           , rows <| List.length <| String.lines valueString
                           ]
                    )

        previewActiveMsg =
            Form.Input previewActiveFieldName Form.Checkbox << Field.Bool

        labelElement =
            if String.isEmpty labelText then
                emptyNode

            else
                label [ for fieldName ] [ text labelText ]
    in
    div [ class <| "form-group form-group-markdown " ++ errorClass ]
        [ labelElement
        , div [ class <| "card " ++ cardErrorClass ]
            [ div [ class "card-header" ]
                [ ul [ class "nav nav-tabs card-header-tabs" ]
                    [ li [ class "nav-item" ]
                        [ a
                            [ onClick <| previewActiveMsg False
                            , class "nav-link"
                            , classList [ ( "active", not previewActive ) ]
                            ]
                            [ lx_ "markdownEditor.editor" appState ]
                        ]
                    , li [ class "nav-item" ]
                        [ a
                            [ onClick <| previewActiveMsg True
                            , class "nav-link"
                            , classList [ ( "active", previewActive ) ]
                            ]
                            [ lx_ "markdownEditor.preview" appState ]
                        ]
                    ]
                ]
            , div [ class "card-body" ]
                [ content
                ]
            , div [ class "card-footer text-muted" ]
                [ lx_ "markdownEditor.markdownDescription" appState ]
            ]
        , error
        ]


simpleDate : AppState -> Form FormError o -> String -> String -> String -> String -> Html.Html Form.Msg
simpleDate appState form yearFieldName monthFieldName dayFieldName labelText =
    let
        yearField =
            Form.getFieldAsString yearFieldName form

        ( yearFieldError, yearFieldErrorClass ) =
            getErrors appState yearField labelText

        monthField =
            Form.getFieldAsString monthFieldName form

        ( monthFieldError, monthFieldErrorClass ) =
            getErrors appState monthField labelText

        dayField =
            Form.getFieldAsString dayFieldName form

        ( dayFieldError, dayFieldErrorClass ) =
            getErrors appState dayField labelText

        error =
            [ yearFieldError, monthFieldError, dayFieldError ]
                |> List.filter ((/=) emptyNode)
                |> List.head
                |> Maybe.withDefault emptyNode
    in
    div [ class "form-group form-group-simple-date" ]
        [ label [] [ text labelText ]
        , div [ class "date-inputs" ]
            [ Input.textInput dayField [ class <| "form-control " ++ dayFieldErrorClass, id dayFieldName, name dayFieldName ]
            , Input.textInput monthField [ class <| "form-control " ++ monthFieldErrorClass, id monthFieldName, name monthFieldName ]
            , Input.textInput yearField [ class <| "form-control " ++ yearFieldErrorClass, id yearFieldName, name yearFieldName ]
            ]
        , error
        ]


{-| Create Html for a form field using the given input field.
-}
formGroup : Input.Input FormError String -> List (Html.Attribute Form.Msg) -> AppState -> Form FormError o -> String -> String -> Html.Html Form.Msg
formGroup inputFn attrs appState form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, errorClass ) =
            getErrors appState field labelText
    in
    div [ class "form-group" ]
        [ label [ for fieldName ] [ text labelText ]
        , inputFn field (attrs ++ [ class <| "form-control " ++ errorClass, id fieldName, name fieldName ])
        , error
        ]


formGroupCustom : (Bool -> Html msg) -> AppState -> Form FormError o -> String -> String -> Html msg
formGroupCustom customInput appState form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, _ ) =
            getErrors appState field labelText
    in
    div [ class "form-group" ]
        [ label [ for fieldName ] [ text labelText ]
        , customInput (Maybe.isJust field.liveError)
        , error
        ]


{-| Helper for creating plain group with text value.
-}
textView : String -> String -> String -> Html.Html msg
textView name value =
    plainGroup <|
        p [ class "form-value", dataCy ("form-group_text_" ++ name) ] [ text value ]


{-| Helper for creating plain group with code block.
-}
codeView : String -> String -> Html.Html msg
codeView value =
    plainGroup <|
        code [ class "form-value" ] [ text value ]


{-| Plain group is same Html as formGroup but without any input fields. It only
shows label with read only Html value.
-}
plainGroup : Html.Html msg -> String -> Html.Html msg
plainGroup valueHtml labelText =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text labelText ]
        , valueHtml
        ]


{-| Get Html and form group error class for a given field. If the field
contains no errors, the returned Html and error class are empty.
-}
getErrors : AppState -> Form.FieldState FormError String -> String -> ( Html msg, String )
getErrors appState field labelText =
    case field.liveError of
        Just error ->
            ( p [ class "invalid-feedback" ] [ text (errorToString appState labelText error) ], "is-invalid" )

        Nothing ->
            ( emptyNode, "" )
