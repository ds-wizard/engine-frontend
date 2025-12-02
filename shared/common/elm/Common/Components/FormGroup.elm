module Common.Components.FormGroup exposing
    ( FormatOption
    , VersionFormGroupConfig
    , alertRadioGroup
    , codeView
    , date
    , fileSize
    , formGroupCustom
    , formatRadioGroup
    , getErrors
    , htmlOrMarkdownEditor
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
    , readOnlyInput
    , resizableTextarea
    , richRadioGroup
    , secret
    , select
    , textView
    , textarea
    , textareaAttrs
    , toggle
    , version
    , viewList
    )

import Common.Components.DatePicker as DatePicker
import Common.Components.FontAwesome exposing (fa, faAdd, faSecretHide, faSecretShow)
import Common.Components.FormExtra as FormExtra
import Common.Components.PasswordBar as PasswordBar
import Common.Utils.ByteUnits as ByteUnits
import Common.Utils.Form as Form
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Markdown as Markdown
import Common.Utils.MarkdownOrHtml as MarkdownOrHtml
import Form exposing (Form, Msg(..))
import Form.Field as Field
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, a, code, div, label, li, p, span, text, ul)
import Html.Attributes exposing (autocomplete, checked, class, classList, for, href, id, name, readonly, rows, target, type_, value, wrap)
import Html.Attributes.Extensions exposing (dataCy, dataTour)
import Html.Events exposing (onCheck, onClick, onMouseDown)
import Html.Extra as Html
import Maybe.Extra as Maybe
import String.Format as String
import Uuid exposing (Uuid)
import Version exposing (Version)


optionalWrapper : Gettext.Locale -> Html Form.Msg -> Html Form.Msg
optionalWrapper locale content =
    div [ class "form-group form-group-optional-wrapper" ]
        [ span [ class "optional-label" ] [ text (gettext "(optional)" locale) ]
        , content
        ]


input : Gettext.Locale -> Form FormError o -> String -> String -> Html Form.Msg
input =
    formGroup Input.textInput []


inputAttrs : List (Html.Attribute Form.Msg) -> Gettext.Locale -> Form FormError o -> String -> String -> Html.Html Form.Msg
inputAttrs =
    formGroup Input.textInput


inputWithTypehints : List String -> Gettext.Locale -> Form FormError o -> String -> String -> Html Form.Msg
inputWithTypehints options locale form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, errorClass ) =
            getErrors locale field labelText

        contains a b =
            String.contains (String.toLower a) (String.toLower b)

        typehints =
            if not (List.isEmpty options) && field.hasFocus then
                let
                    filteredOptions =
                        case field.value of
                            Just value ->
                                List.filter (contains value) options

                            Nothing ->
                                options
                in
                if List.isEmpty filteredOptions then
                    Html.nothing

                else
                    ul [ class "typehints" ]
                        (List.map
                            (\option ->
                                li
                                    [ onMouseDown (Form.Input fieldName Form.Text (Field.String option))
                                    , dataCy "form-group_typehints_item"
                                    ]
                                    [ text option ]
                            )
                            filteredOptions
                        )

            else
                Html.nothing
    in
    div [ class "form-group" ]
        [ label [ for fieldName ] [ text labelText ]
        , Input.textInput field [ class <| "form-control " ++ errorClass, id fieldName, name fieldName, autocomplete False ]
        , typehints
        , error
        ]


password : Gettext.Locale -> Form FormError o -> String -> String -> Html Form.Msg
password =
    formGroup Input.passwordInput []


passwordWithStrength : Gettext.Locale -> Form FormError o -> String -> String -> Html Form.Msg
passwordWithStrength locale form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, errorClass ) =
            getErrors locale field labelText
    in
    div [ class "form-group" ]
        [ label [ for fieldName ] [ text labelText ]
        , Input.passwordInput field [ class ("form-control " ++ errorClass), id fieldName, name fieldName ]
        , PasswordBar.view (Maybe.withDefault "" field.value)
        , error
        ]


secret : Gettext.Locale -> Form FormError o -> String -> String -> Html Form.Msg
secret locale form fieldName labelText =
    let
        visibleFieldName =
            fieldName ++ "-visible__"

        visibleStateField =
            Form.getFieldAsBool visibleFieldName form

        visible =
            Maybe.withDefault False visibleStateField.value

        visibleActiveMsg =
            Form.Input visibleFieldName Form.Checkbox << Field.Bool

        ( _, errorClass ) =
            getErrors locale (Form.getFieldAsString fieldName form) labelText

        inputFn field attributes =
            let
                inputField =
                    if visible then
                        Input.textInput field (attributes ++ [ class "form-control" ])

                    else
                        Input.passwordInput field (attributes ++ [ class "form-control" ])

                showHideIcon =
                    if visible then
                        a [ onClick (visibleActiveMsg False) ]
                            [ faSecretHide ]

                    else
                        a [ onClick (visibleActiveMsg True) ]
                            [ faSecretShow ]
            in
            div [ class ("input-secret " ++ errorClass) ]
                [ inputField
                , showHideIcon
                ]
    in
    formGroup inputFn [] locale form fieldName labelText


fileSize : Gettext.Locale -> Form FormError o -> String -> String -> Html Form.Msg
fileSize locale form fieldName labelText =
    let
        inputFn field attributes =
            let
                value =
                    (Form.getFieldAsString fieldName form).value
                        |> Maybe.andThen String.toInt
                        |> Maybe.withDefault 0
            in
            div [ class "input-group" ]
                [ Input.textInput field
                    (attributes
                        ++ [ class "form-control"
                           ]
                    )
                , span [ class "input-group-text" ]
                    [ text "≈ "
                    , text (ByteUnits.toReadable value)
                    ]
                ]
    in
    formGroup inputFn [] locale form fieldName labelText


select : Gettext.Locale -> List ( String, String ) -> Form FormError o -> String -> String -> Html Form.Msg
select locale options =
    formGroup (Input.selectInput options) [ class "form-select" ] locale


richRadioGroup : Gettext.Locale -> List ( String, String, String ) -> Form FormError o -> String -> String -> Html Form.Msg
richRadioGroup locale options =
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
    formGroup radioInput [] locale


type alias FormatOption a =
    { a
        | uuid : Uuid
        , name : String
        , icon : String
    }


formatRadioGroup : Gettext.Locale -> List (FormatOption a) -> Form FormError o -> String -> String -> Html Form.Msg
formatRadioGroup locale options form fieldName labelText =
    let
        radioInput state _ =
            let
                ( _, errorClass ) =
                    getErrors locale (Form.getFieldAsString fieldName form) labelText

                buildOption : FormatOption a -> Html Form.Msg
                buildOption format =
                    label
                        [ class "export-link"
                        , classList
                            [ ( "export-link-selected", state.value == Just (Uuid.toString format.uuid) )
                            ]
                        ]
                        [ Html.input
                            [ value (Uuid.toString format.uuid)
                            , checked (state.value == Just (Uuid.toString format.uuid))
                            , type_ "radio"
                            , name "format"
                            , onCheck (\_ -> Input state.path Form.Text <| Field.String <| Uuid.toString format.uuid)
                            ]
                            []
                        , fa format.icon
                        , text format.name
                        ]
            in
            div [ class errorClass ]
                [ div [ class "export-formats" ] (List.map buildOption options)
                ]
    in
    formGroup radioInput [] locale form fieldName labelText


htmlRadioGroup : Gettext.Locale -> List ( String, Html Form.Msg ) -> Form FormError o -> String -> String -> Html Form.Msg
htmlRadioGroup locale options =
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
    formGroup radioInput [] locale


alertRadioGroup : Gettext.Locale -> List ( String, String, String ) -> Form FormError o -> String -> String -> Html Form.Msg
alertRadioGroup locale options form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( _, errorClass ) =
            getErrors locale field labelText

        radioInput state _ =
            let
                buildOption ( k, v, c ) =
                    label [ class ("flex-grow-1 py-2 alert alert-" ++ c), classList [ ( "form-check-selected", state.value == Just k ) ] ]
                        [ Html.input
                            [ value k
                            , checked (state.value == Just k)
                            , class "form-check-input"
                            , type_ "radio"
                            , id k
                            , onCheck (\_ -> Input state.path Form.Text <| Field.String k)
                            ]
                            []
                        , span [ class "ms-2", for k ]
                            [ text v ]
                        ]
            in
            div [ class "form-radio-group", class errorClass ] (List.map buildOption options)
    in
    formGroup radioInput [] locale form fieldName labelText


{-| Helper for creating form group with textarea.
-}
textarea : Gettext.Locale -> Form FormError o -> String -> String -> Html Form.Msg
textarea =
    formGroup Input.textArea []


textareaAttrs : List (Html.Attribute Form.Msg) -> Gettext.Locale -> Form FormError o -> String -> String -> Html Form.Msg
textareaAttrs attrs =
    formGroup Input.textArea attrs


resizableTextarea : Gettext.Locale -> Form FormError o -> String -> String -> Html Form.Msg
resizableTextarea locale form fieldName =
    let
        lines =
            (Form.getFieldAsString fieldName form).value
                |> Maybe.map (max 3 << List.length << String.split "\n")
                |> Maybe.withDefault 3

        attributes =
            [ rows lines, wrap "off" ]
    in
    formGroup Input.textArea attributes locale form fieldName


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


list : Gettext.Locale -> (Form FormError o -> Int -> Html Form.Msg) -> Form FormError o -> String -> String -> String -> Html Form.Msg
list locale itemView form fieldName labelText addLabel =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, _ ) =
            getErrors locale field labelText

        listLabel =
            if String.isEmpty labelText then
                Html.nothing

            else
                label [] [ text labelText ]
    in
    div [ class "form-group" ]
        [ listLabel
        , div [] (List.map (itemView form) (Form.getListIndexes fieldName form))
        , div [ class "form-list-error" ] [ error ]
        , a
            [ class "with-icon"
            , onClick (Form.Append fieldName)
            , dataCy "form-group_list_add-button"
            ]
            [ faAdd
            , text addLabel
            ]
        ]


listWithCustomMsg : Gettext.Locale -> (Form.Msg -> msg) -> (Form FormError o -> Int -> Html msg) -> Form FormError o -> String -> String -> String -> Html msg
listWithCustomMsg locale wrapMsg itemView form fieldName labelText addLabel =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, _ ) =
            getErrors locale field labelText
    in
    div [ class "form-group" ]
        [ label [] [ text labelText ]
        , div [] (List.map (itemView form) (Form.getListIndexes fieldName form))
        , div [ class "form-list-error" ] [ error ]
        , a
            [ class "with-icon"
            , onClick (wrapMsg <| Form.Append fieldName)
            , dataCy "form-group_list_add-button"
            ]
            [ faAdd
            , text addLabel
            ]
        ]


listWithHeader : Gettext.Locale -> Html Form.Msg -> (Form FormError o -> Int -> Html Form.Msg) -> Form FormError o -> String -> String -> String -> Html Form.Msg
listWithHeader locale header itemView form fieldName labelText addLabel =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, _ ) =
            getErrors locale field labelText
    in
    div [ class "form-group" ]
        [ label [] [ text labelText ]
        , header
        , div [] (List.map (itemView form) (Form.getListIndexes fieldName form))
        , div [ class "form-list-error" ] [ error ]
        , a
            [ class "with-icon"
            , onClick (Form.Append fieldName)
            , dataCy "form-group_list_add-button"
            ]
            [ faAdd
            , text addLabel
            ]
        ]


viewList : Gettext.Locale -> (Form FormError o -> Int -> Html Form.Msg) -> Form FormError o -> String -> String -> Html Form.Msg
viewList locale itemView form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, _ ) =
            getErrors locale field labelText
    in
    div [ class "form-group" ]
        [ label [] [ text labelText ]
        , div [] (List.map (itemView form) (Form.getListIndexes fieldName form))
        , div [ class "form-list-error" ] [ error ]
        ]


markdownEditor : Gettext.Locale -> String -> Form FormError o -> String -> String -> Html Form.Msg
markdownEditor locale markdownGuideLink =
    markupEditor
        { toPreview = Markdown.toHtml []
        , hint = gettext "You can use %s and see the result in the preview tab." locale
        , extraClass = "form-group-markdown"
        , markdownGuideLink = markdownGuideLink
        }
        locale


htmlOrMarkdownEditor : Gettext.Locale -> String -> Form FormError o -> String -> String -> Html Form.Msg
htmlOrMarkdownEditor locale markdownGuideLink =
    markupEditor
        { toPreview = MarkdownOrHtml.toHtml []
        , hint = gettext "You can use HTML or %s and see the result in the preview tab." locale
        , extraClass = ""
        , markdownGuideLink = markdownGuideLink
        }
        locale


type alias MarkupEditorConfig =
    { toPreview : String -> Html Form.Msg
    , extraClass : String
    , hint : String
    , markdownGuideLink : String
    }


markupEditor : MarkupEditorConfig -> Gettext.Locale -> Form FormError o -> String -> String -> Html Form.Msg
markupEditor cfg locale form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        previewActiveFieldName =
            fieldName ++ "-preview-active__"

        ( error, errorClass ) =
            getErrors locale field labelText

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
                cfg.toPreview valueString

            else
                Input.textArea field
                    [ class <| "form-control " ++ errorClass
                    , id fieldName
                    , name fieldName
                    , rows <| List.length <| String.lines valueString
                    ]

        previewActiveMsg =
            Form.Input previewActiveFieldName Form.Checkbox << Field.Bool

        labelElement =
            if String.isEmpty labelText then
                Html.nothing

            else
                label [ for fieldName ] [ text labelText ]
    in
    div [ class <| "form-group form-group-markup-editor " ++ errorClass ++ " " ++ cfg.extraClass ]
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
                            [ text (gettext "Editor" locale) ]
                        ]
                    , li [ class "nav-item" ]
                        [ a
                            [ onClick <| previewActiveMsg True
                            , class "nav-link"
                            , classList [ ( "active", previewActive ) ]
                            ]
                            [ text (gettext "Preview" locale) ]
                        ]
                    ]
                ]
            , div [ class "card-body" ]
                [ content
                ]
            , div [ class "card-footer text-muted" ]
                (String.formatHtml cfg.hint
                    [ a
                        [ href cfg.markdownGuideLink
                        , target "_blank"
                        ]
                        [ text "Markdown" ]
                    ]
                )
            ]
        , error
        ]


date : Gettext.Locale -> Form FormError o -> String -> String -> Html.Html Form.Msg
date locale form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        dateValue =
            Maybe.withDefault "" field.value

        toMsg =
            Form.Input fieldName Form.Text << Field.String

        inputFn isInvalid =
            DatePicker.datePickerUtc
                [ DatePicker.invalid isInvalid
                , DatePicker.value dateValue
                , DatePicker.onChange toMsg
                ]
    in
    formGroupCustom inputFn locale form fieldName labelText


{-| Create Html for a form field using the given input field.
-}
formGroup : Input.Input FormError String -> List (Html.Attribute Form.Msg) -> Gettext.Locale -> Form FormError o -> String -> String -> Html.Html Form.Msg
formGroup inputFn attrs locale form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, errorClass ) =
            getErrors locale field labelText
    in
    div [ class "form-group", dataTour ("form-group_" ++ fieldName) ]
        [ label [ for fieldName ] [ text labelText ]
        , inputFn field (attrs ++ [ class <| "form-control " ++ errorClass, id fieldName, name fieldName ])
        , error
        ]


formGroupCustom : (Bool -> Html msg) -> Gettext.Locale -> Form FormError o -> String -> String -> Html msg
formGroupCustom customInput locale form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, _ ) =
            getErrors locale field labelText
    in
    div [ class "form-group", dataTour ("form-group_" ++ fieldName) ]
        [ label [ for fieldName ] [ text labelText ]
        , customInput (Maybe.isJust field.liveError)
        , error
        ]


{-| Helper for creating plain group with text value.
-}
textView : String -> String -> String -> Html msg
textView name value =
    plainGroup <|
        p [ class "form-value", dataCy ("form-group_text_" ++ name) ] [ text value ]


{-| Helper for creating plain group with code block.
-}
codeView : String -> String -> Html msg
codeView value =
    plainGroup <|
        code [ class "form-value" ] [ text value ]


{-| Plain group is same Html as formGroup but without any input fields. It only
shows label with read only Html value.
-}
plainGroup : Html.Html msg -> String -> Html msg
plainGroup valueHtml labelText =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text labelText ]
        , valueHtml
        ]


readOnlyInput : String -> String -> Html msg
readOnlyInput valueText labelText =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text labelText ]
        , Html.input [ type_ "text", value valueText, class "form-control", readonly True ] []
        ]


{-| Get Html and form group error class for a given field. If the field
contains no errors, the returned Html and error class are empty.
-}
getErrors : Gettext.Locale -> Form.FieldState FormError String -> String -> ( Html msg, String )
getErrors locale field labelText =
    case field.liveError of
        Just error ->
            ( p [ class "invalid-feedback" ] [ text (Form.errorToString locale labelText error) ], "is-invalid" )

        Nothing ->
            ( Html.nothing, "" )


type alias VersionFormGroupConfig msg =
    { label : String
    , majorField : String
    , minorField : String
    , patchField : String
    , currentVersion : Maybe Version
    , wrapFormMsg : Form.Msg -> msg
    , setVersionMsg : Maybe (Version -> msg)
    }


version : Gettext.Locale -> VersionFormGroupConfig msg -> Form FormError o -> Html msg
version locale cfg form =
    let
        majorField =
            Form.getFieldAsString cfg.majorField form

        minorField =
            Form.getFieldAsString cfg.minorField form

        patchField =
            Form.getFieldAsString cfg.patchField form

        errorClass =
            case ( majorField.liveError, minorField.liveError, patchField.liveError ) of
                ( Nothing, Nothing, Nothing ) ->
                    ""

                _ ->
                    " is-invalid"

        suggestions =
            case cfg.setVersionMsg of
                Just setVersionMsg ->
                    let
                        nextMajor =
                            cfg.currentVersion
                                |> Maybe.map Version.nextMajor
                                |> Maybe.withDefault (Version.create 1 0 0)

                        nextMinor =
                            cfg.currentVersion
                                |> Maybe.map Version.nextMinor
                                |> Maybe.withDefault (Version.create 0 1 0)

                        nextPatch =
                            cfg.currentVersion
                                |> Maybe.map Version.nextPatch
                                |> Maybe.withDefault (Version.create 0 0 1)
                    in
                    p [ class "form-text text-muted version-suggestions" ]
                        [ text (gettext "Suggestions: " locale)
                        , a [ onClick <| setVersionMsg nextMajor, class "color-primary" ] [ text <| Version.toString nextMajor ]
                        , a [ onClick <| setVersionMsg nextMinor, class "color-primary" ] [ text <| Version.toString nextMinor ]
                        , a [ onClick <| setVersionMsg nextPatch, class "color-primary" ] [ text <| Version.toString nextPatch ]
                        ]

                Nothing ->
                    Html.nothing
    in
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text cfg.label ]
        , div [ class "version-inputs" ]
            [ Html.map cfg.wrapFormMsg <| Input.baseInput "number" Field.String Form.Text majorField [ class <| "form-control" ++ errorClass, Html.Attributes.min "0", name "version-major", id "version-major" ]
            , text "."
            , Html.map cfg.wrapFormMsg <| Input.baseInput "number" Field.String Form.Text minorField [ class <| "form-control" ++ errorClass, Html.Attributes.min "0", name "version-minor", id "version-minor" ]
            , text "."
            , Html.map cfg.wrapFormMsg <| Input.baseInput "number" Field.String Form.Text patchField [ class <| "form-control" ++ errorClass, Html.Attributes.min "0", name "version-patch", id "version-patch" ]
            ]
        , suggestions
        , FormExtra.text <| gettext "The version number is in format X.Y.Z. Increasing number Z indicates only some fixes, number Y minor changes, and number X indicates a major change." locale
        ]
