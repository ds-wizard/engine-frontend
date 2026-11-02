module Common.Components.Questionnaire.Question exposing
    ( ItemOption
    , ItemSelectExtra
    , encodeItemSelectExtra
    , typeHintId
    , typeHintsId
    , viewFile
    , viewIntegrationApi
    , viewIntegrationPlugin
    , viewItemSelect
    , viewItemsCollapse
    , viewList
    , viewMessage
    , viewMultiChoice
    , viewOptions
    , viewValue
    )

{-| One question node of the questionnaire, by type.

**Every function here is the body of a lazy call.** `Content` calls each through
`Html.Lazy` with arguments that are primitives or references held in the model —
the locale, the node record from `Nodes.build`, the flags as one `Int`, the reply
and anything else as strings — and nothing in here reads the app state or the
knowledge model. That is what lets a keystroke in one question re-render that
question alone. Anything a question needs must arrive as one of those arguments
or be worked out from them in here; adding an argument that is built fresh on
every render would make every question re-render every time.

**Structure.** Each question is a `listitem` of the questionnaire's list
(`Nest.listItem`), at its depth, named by its title. Its inputs are a `group`
named by the same title, so a screen reader entering them hears which question
they answer. A radio or checkbox is labeled by its own option; a single input by
the title.

The markup, question type for question type, is the design system's:
choices are `.ui-choice` rows, notices are `Ui.Alert`, the actions are `Ui.Button`s
with their names.

-}

import ActionResult
import CharIdentifier
import Common.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Common.Api.Models.KnowledgeModel.Choice exposing (Choice)
import Common.Api.Models.KnowledgeModel.Expert exposing (Expert)
import Common.Api.Models.KnowledgeModel.Integration as Integration
import Common.Api.Models.KnowledgeModel.Metric exposing (Metric)
import Common.Api.Models.KnowledgeModel.Question as Question
import Common.Api.Models.KnowledgeModel.Question.QuestionValidation as QuestionValidation
import Common.Api.Models.KnowledgeModel.Question.QuestionValueType as QuestionValueType
import Common.Api.Models.ProjectDetail.Reply exposing (Reply)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue exposing (ReplyValue(..))
import Common.Api.Models.ProjectDetail.Reply.ReplyValue.IntegrationReplyType as IntegrationReplyType
import Common.Api.Models.ProjectFileSimple as ProjectFileSimple
import Common.Api.Models.TypeHint exposing (TypeHint)
import Common.Api.Models.User as User
import Common.Components.Markdown as Markdown
import Common.Components.Questionnaire.ContentMsg exposing (Msg(..), QuestionActions, TypeHints)
import Common.Components.Questionnaire.Flags as Flags exposing (Flags)
import Common.Components.Questionnaire.Nest as Nest
import Common.Components.Questionnaire.Nodes as Nodes exposing (IntegrationAnswering(..), IntegrationPluginData, QuestionExtraCrossReference, QuestionExtraData, QuestionExtraUrlReference, QuestionNodeData, QuestionSpecificNodeData(..))
import Common.Components.Questionnaire.Path as Path
import Common.Components.Questionnaire.ReplyString as ReplyString
import Common.Plugins.Plugin exposing (ProjectQuestionActionConnectorType(..))
import Common.Plugins.PluginElement as PluginElement
import Common.Utils.ByteUnits as ByteUnits
import Common.Utils.FileIcon as FileIcon
import Common.Utils.RegexPatterns as RegexPatterns
import Gettext exposing (gettext, ngettext)
import Html exposing (Html, a, div, input, label, li, option, select, span, strong, text, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, href, id, name, rel, selected, target, type_, value)
import Html.Events exposing (on, onBlur, onClick, onInput, onMouseEnter, preventDefaultOn, targetValue)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import List.Extra as List
import Maybe.Extra as Maybe
import Regex
import String.Format as String
import Time
import Ui.Alert as Alert
import Ui.Button as Button
import Ui.Field as Field
import Ui.Icon as Icon
import Ui.Tag as Tag
import Ui.TestId as TestId
import Ui.Text as Text
import Ui.Tooltip as Tooltip
import Ui.ValueScale as ValueScale



-- OPTIONS


viewOptions : Gettext.Locale -> QuestionActions -> QuestionNodeData -> Flags -> String -> String -> Int -> Html Msg
viewOptions locale actions node flags answeredAt replyString commentCount =
    let
        mbReply =
            ReplyString.toReply replyString

        { answers, followUpsCount, followUpsCollapsed, metrics } =
            case node.questionSpecificData of
                OptionsQuestionSpecificNodeData data ->
                    data

                _ ->
                    { answers = [], followUpsCount = 0, followUpsCollapsed = False, metrics = [] }

        mbSelectedAnswerUuid =
            Maybe.map (ReplyValue.getAnswerUuid << .value) mbReply

        mbAnswer =
            mbSelectedAnswerUuid
                |> Maybe.andThen (\selected -> List.find ((==) selected << .uuid) answers)

        answered =
            ReplyString.isAnswered mbReply
    in
    wrapper
        { locale = locale, actions = actions, node = node, flags = flags, commentCount = commentCount, answered = answered }
        [ div [ class "ui-choices" ]
            (List.indexedMap
                (viewAnswer
                    { locale = locale
                    , mbSelectedAnswerUuid = mbSelectedAnswerUuid
                    , metrics = metrics
                    , questionPath = node.questionPath
                    , flags = flags
                    }
                )
                answers
            )
        , viewClearReply { locale = locale, answered = answered, questionPath = node.questionPath, flags = flags }
        , viewAnsweredBy { locale = locale, mbReply = mbReply, answeredAt = answeredAt, flags = flags }
        , mbAnswer
            |> Maybe.andThen .advice
            |> Maybe.unwrap (text "") (\advice -> div [ class "c-questionnaire-advice" ] [ Markdown.view { headingOffset = 3 } advice ])
        , if followUpsCount > 0 then
            viewFollowUpsCollapse
                { locale = locale
                , answerPath = node.questionPath ++ "." ++ Maybe.withDefault "" mbSelectedAnswerUuid
                , count = followUpsCount
                , collapsed = followUpsCollapsed
                }

          else
            text ""
        ]


viewAnswer : { locale : Gettext.Locale, mbSelectedAnswerUuid : Maybe String, metrics : List Metric, questionPath : String, flags : Flags } -> Int -> Answer -> Html Msg
viewAnswer props order answer =
    let
        isSelected =
            props.mbSelectedAnswerUuid == Just answer.uuid

        fieldName =
            Path.toFieldId props.questionPath
    in
    label [ classList [ ( "ui-choice", True ), ( "is-selected", isSelected ) ] ]
        [ input
            [ type_ "radio"
            , class "ui-choice__input"
            , id (fieldName ++ "_" ++ answer.uuid)
            , name fieldName
            , checked isSelected
            , disabled (Flags.isReadOnly props.flags)
            , onClick (SetReply props.questionPath (ReplyValue.AnswerReply answer.uuid))
            ]
            []
        , let
            followUpsIndicator =
                if List.isEmpty answer.followUpUuids then
                    []

                else
                    -- The sentence twice: said with the label to a screen reader,
                    -- and shown on hover for the eye (the tooltip is hidden from
                    -- assistive technology, so it is not read out again).
                    let
                        sentence =
                            gettext "This option leads to some follow up questions." props.locale
                    in
                    [ Tooltip.view { label = sentence, placement = Tooltip.Below }
                        (span [ class "c-questionnaire-follow-ups-indicator" ]
                            [ Icon.toHtml (Icon.solid "fa-list-ul")
                            , Text.visuallyHiddenInline sentence
                            ]
                        )
                    ]

            choiceLabel =
                span [ class "ui-choice__label" ]
                    (text (CharIdentifier.fromInt order ++ ". " ++ answer.label) :: followUpsIndicator)
          in
          -- The body only when there is a second line under the label: most
          -- answers have none, and a chapter has hundreds of answers.
          case viewAnswerMetrics { answer = answer, metrics = props.metrics, showValue = Flags.showMetricValues props.flags } of
            Just metrics ->
                span [ class "ui-choice__body" ] [ choiceLabel, metrics ]

            Nothing ->
                choiceLabel
        ]


viewAnswerMetrics : { answer : Answer, metrics : List Metric, showValue : Bool } -> Maybe (Html msg)
viewAnswerMetrics props =
    let
        metricTitle metricUuid =
            List.find ((==) metricUuid << .uuid) props.metrics
                |> Maybe.map .title

        viewMeasure measure =
            metricTitle measure.metricUuid
                |> Maybe.map
                    (\title ->
                        span [ class "c-questionnaire-metric", ValueScale.tone measure.measure ]
                            (text title
                                :: (if props.showValue then
                                        -- The gap is what separates the two to
                                        -- the eye; the space is what separates
                                        -- them to a screen reader, and being
                                        -- whitespace alone it is no flex item
                                        -- and takes no room.
                                        [ text " "
                                        , span [ class "c-questionnaire-metric__value" ] [ text (String.fromInt (round (100 * measure.measure)) ++ "%") ]
                                        ]

                                    else
                                        []
                                   )
                            )
                    )
    in
    case List.filterMap viewMeasure props.answer.metricMeasures of
        [] ->
            Nothing

        measures ->
            Just (span [ class "c-questionnaire-metrics" ] measures)


{-| The follow-ups' own control, at the top of their section, as in the wizard.
It has their left line, so the Collapse above them reads as part of them;
folded, the line stays over a light ground saying what is inside — "Expand
**3** follow up questions", a sentence with its count.
-}
viewFollowUpsCollapse : { locale : Gettext.Locale, answerPath : String, count : Int, collapsed : Bool } -> Html Msg
viewFollowUpsCollapse props =
    let
        -- The same small button as Clear answer.
        control icon label msg testId =
            Button.toHtml
                (Button.ghost label msg
                    |> Button.withIcon icon
                    |> Button.small
                    |> Button.withTestId testId
                )
    in
    if props.collapsed then
        div [ class "c-questionnaire-follow-ups c-questionnaire-follow-ups--collapsed" ]
            (String.formatHtml (ngettext ( "%s %s follow up question", "%s %s follow up questions" ) props.count props.locale)
                [ control (Icon.solid "fa-chevron-down") (gettext "Expand" props.locale) (ExpandPaths [ props.answerPath ]) "questionnaire_followups_expand"
                , strong [] [ text (String.fromInt props.count) ]
                ]
            )

    else
        div [ class "c-questionnaire-follow-ups" ]
            [ control (Icon.solid "fa-chevron-up") (gettext "Collapse" props.locale) (CollapsePaths [ props.answerPath ]) "questionnaire_followups_collapse" ]



-- LIST


viewList : Gettext.Locale -> QuestionActions -> QuestionNodeData -> Flags -> String -> Int -> Html Msg
viewList locale actions node flags replyString commentCount =
    let
        mbReply =
            ReplyString.toReply replyString
    in
    wrapper
        { locale = locale, actions = actions, node = node, flags = flags, commentCount = commentCount, answered = ReplyString.isAnswered mbReply }
        [ viewItemsCollapse { locale = locale, mbReply = mbReply, questionPath = node.questionPath } ]


{-| "Expand all" and "Collapse all", from three items up.
-}
viewItemsCollapse : { locale : Gettext.Locale, mbReply : Maybe Reply, questionPath : String } -> Html Msg
viewItemsCollapse props =
    let
        itemPaths =
            Maybe.unwrap [] (ReplyValue.getItemUuids << .value) props.mbReply
                |> List.map (\itemUuid -> props.questionPath ++ "." ++ itemUuid)
    in
    if List.length itemPaths >= 3 then
        div [ class "c-questionnaire-items-collapse" ]
            [ Button.toHtml
                (Button.ghost (gettext "Expand all" props.locale) (ExpandPaths itemPaths)
                    |> Button.withIcon (Icon.solid "fa-angles-down")
                    |> Button.quiet
                    |> Button.small
                )
            , Button.toHtml
                (Button.ghost (gettext "Collapse all" props.locale) (CollapsePaths itemPaths)
                    |> Button.withIcon (Icon.solid "fa-angles-up")
                    |> Button.quiet
                    |> Button.small
                )
            ]

    else
        text ""



-- VALUE


viewValue : Gettext.Locale -> Time.Zone -> QuestionActions -> QuestionNodeData -> Flags -> String -> String -> Int -> Html Msg
viewValue locale timeZone actions node flags answeredAt replyString commentCount =
    let
        mbReply =
            ReplyString.toReply replyString

        readOnly =
            Flags.isReadOnly flags

        valueType =
            Question.getValueType node.question

        currentValue =
            Maybe.unwrap
                (if valueType == Just QuestionValueType.ColorQuestionValueType then
                    "#000000"

                 else
                    ""
                )
                (ReplyValue.getStringReply << .value)
                mbReply

        fieldId =
            Path.toFieldId node.questionPath

        titleId =
            Nest.titleId node.questionPath

        toMsg =
            SetReply node.questionPath << ReplyValue.StringReply

        plainInput inputType =
            input
                ([ type_ inputType
                 , class "ui-input"
                 , id fieldId
                 , name fieldId
                 , value currentValue
                 , attribute "aria-labelledby" titleId
                 ]
                    ++ (if readOnly then
                            [ disabled True ]

                        else
                            [ onInput toMsg ]
                       )
                )
                []

        -- The pickers and the editor are `Ui.Field`s, for the custom elements
        -- they are built on; their own label is the title again, hidden, since
        -- the title is already on the screen above them.
        field build =
            if readOnly then
                plainInput "text"

            else
                Field.toHtml (Field.withHiddenLabel (build { id = fieldId, label = Question.getTitle node.question, value = currentValue, onChange = toMsg }))

        warning pattern message =
            if not (String.isEmpty currentValue) && not (Regex.contains pattern currentValue) then
                Alert.toHtml (Alert.warning message)

            else
                text ""

        validations =
            case node.questionSpecificData of
                ValueQuestionSpecificNodeData data ->
                    data.validations

                _ ->
                    []

        validationWarning validation =
            case QuestionValidation.validate { locale = locale, timeZone = timeZone } validation currentValue of
                Ok _ ->
                    text ""

                Err error ->
                    Alert.toHtml (Alert.warning error)

        inputView =
            case valueType of
                Just QuestionValueType.NumberQuestionValueType ->
                    [ plainInput "number" ]

                Just QuestionValueType.DateQuestionValueType ->
                    [ field Field.date ]

                Just QuestionValueType.DateTimeQuestionValueType ->
                    [ field Field.dateTime ]

                Just QuestionValueType.TimeQuestionValueType ->
                    [ field Field.time ]

                Just QuestionValueType.EmailQuestionValueType ->
                    [ plainInput "email", warning RegexPatterns.email (gettext "This is not a valid email address." locale) ]

                Just QuestionValueType.UrlQuestionValueType ->
                    [ plainInput "text", warning RegexPatterns.url (gettext "This is not a valid URL." locale) ]

                Just QuestionValueType.TextQuestionValueType ->
                    if readOnly then
                        [ div [ class "c-questionnaire-read-only-text" ] [ Markdown.view { headingOffset = 3 } currentValue ] ]

                    else
                        [ field Field.markdown ]

                Just QuestionValueType.ColorQuestionValueType ->
                    [ if readOnly then
                        plainInput "text"

                      else
                        Field.toHtml (Field.withHiddenLabel (Field.color { id = fieldId, label = Question.getTitle node.question, value = currentValue, onInput = toMsg }))
                    , warning RegexPatterns.color (gettext "This is not a valid color." locale)
                    ]

                _ ->
                    [ plainInput "text" ]

        answered =
            ReplyString.isAnswered mbReply
    in
    wrapper
        { locale = locale, actions = actions, node = node, flags = flags, commentCount = commentCount, answered = answered }
        [ div [ class "c-questionnaire-value" ] (inputView ++ List.map validationWarning validations)
        , viewClearReply { locale = locale, answered = answered, questionPath = node.questionPath, flags = flags }
        , viewAnsweredBy { locale = locale, mbReply = mbReply, answeredAt = answeredAt, flags = flags }
        ]



-- MULTI-CHOICE


viewMultiChoice : Gettext.Locale -> QuestionActions -> QuestionNodeData -> Flags -> String -> String -> Int -> Html Msg
viewMultiChoice locale actions node flags answeredAt replyString commentCount =
    let
        mbReply =
            ReplyString.toReply replyString

        choices =
            case node.questionSpecificData of
                MultiChoiceQuestionSpecificNodeData data ->
                    data.choices

                _ ->
                    []

        selected =
            Maybe.unwrap [] (ReplyValue.getChoiceUuids << .value) mbReply

        answered =
            ReplyString.isAnswered mbReply
    in
    wrapper
        { locale = locale, actions = actions, node = node, flags = flags, commentCount = commentCount, answered = answered }
        [ div [ class "ui-choices" ] (List.indexedMap (viewChoice { questionPath = node.questionPath, selected = selected, flags = flags }) choices)
        , viewClearReply { locale = locale, answered = answered, questionPath = node.questionPath, flags = flags }
        , viewAnsweredBy { locale = locale, mbReply = mbReply, answeredAt = answeredAt, flags = flags }
        ]


viewChoice : { questionPath : String, selected : List String, flags : Flags } -> Int -> Choice -> Html Msg
viewChoice props order choice =
    let
        isSelected =
            List.member choice.uuid props.selected

        toggled =
            if isSelected then
                List.filter ((/=) choice.uuid) props.selected

            else
                choice.uuid :: props.selected

        fieldName =
            Path.toFieldId props.questionPath
    in
    label [ classList [ ( "ui-choice", True ), ( "is-selected", isSelected ) ] ]
        [ input
            [ type_ "checkbox"
            , class "ui-choice__input"
            , id (fieldName ++ "_" ++ choice.uuid)
            , name fieldName
            , checked isSelected
            , disabled (Flags.isReadOnly props.flags)
            , onClick (SetReply props.questionPath (ReplyValue.MultiChoiceReply toggled))
            ]
            []
        , span [ class "ui-choice__label" ] [ text (CharIdentifier.fromInt order ++ ". " ++ choice.label) ]
        ]



-- ITEM SELECT


{-| What an item select offers, worked out outside the lazy call and passed in as
a string (`encodeItemSelectExtra`), like the reply.
-}
type alias ItemSelectExtra =
    { itemOptions : List ItemOption
    , itemMissing : Bool
    , mbItemPath : Maybe String
    }


type alias ItemOption =
    { itemUuid : String
    , itemTitle : String
    }


encodeItemSelectExtra : ItemSelectExtra -> String
encodeItemSelectExtra extra =
    E.encode 0
        (E.object
            [ ( "itemOptions", E.list (\o -> E.object [ ( "itemUuid", E.string o.itemUuid ), ( "itemTitle", E.string o.itemTitle ) ]) extra.itemOptions )
            , ( "itemMissing", E.bool extra.itemMissing )
            , ( "itemPath", Maybe.unwrap E.null E.string extra.mbItemPath )
            ]
        )


itemSelectExtraDecoder : Decoder ItemSelectExtra
itemSelectExtraDecoder =
    D.succeed ItemSelectExtra
        |> D.required "itemOptions" (D.list (D.map2 ItemOption (D.field "itemUuid" D.string) (D.field "itemTitle" D.string)))
        |> D.required "itemMissing" D.bool
        |> D.required "itemPath" (D.nullable D.string)


viewItemSelect : Gettext.Locale -> QuestionActions -> QuestionNodeData -> Flags -> String -> String -> String -> Int -> Html Msg
viewItemSelect locale actions node flags extraString answeredAt replyString commentCount =
    let
        mbReply =
            ReplyString.toReply replyString

        { itemOptions, itemMissing, mbItemPath } =
            D.decodeString itemSelectExtraDecoder extraString
                |> Result.withDefault (ItemSelectExtra [] False Nothing)

        mbSelectedItem =
            Maybe.map (ReplyValue.getSelectedItemUuid << .value) mbReply

        fieldId =
            Path.toFieldId node.questionPath

        toOption item =
            option [ value item.itemUuid, selected (Just item.itemUuid == mbSelectedItem) ] [ text item.itemTitle ]

        options =
            if Maybe.isJust mbSelectedItem && not itemMissing then
                List.map toOption itemOptions

            else
                toOption { itemUuid = "", itemTitle = gettext "- select -" locale } :: List.map toOption itemOptions

        warning =
            case Question.getListQuestionUuid node.question of
                Just listQuestionUuid ->
                    if List.isEmpty itemOptions && not itemMissing then
                        Alert.toHtmlWithContent
                            [ text (gettext "There are no items to select from yet." locale)
                            , text " "
                            , textButton (gettext "Create them now." locale) (ScrollToQuestion listQuestionUuid)
                            ]
                            (Alert.warning "")

                    else
                        text ""

                Nothing ->
                    Alert.toHtml (Alert.warning (gettext "This question does not have any configured list options to select from." locale))

        answered =
            ReplyString.isAnswered mbReply
    in
    wrapper
        { locale = locale, actions = actions, node = node, flags = flags, commentCount = commentCount, answered = answered }
        [ div [ class "c-questionnaire-value" ]
            [ span [ class "ui-select" ]
                [ select
                    ([ class "ui-input", id fieldId, name fieldId, attribute "aria-labelledby" (Nest.titleId node.questionPath) ]
                        ++ (if Flags.isReadOnly flags then
                                [ disabled True ]

                            else
                                [ Html.Events.on "change" (D.map (SetReply node.questionPath << ItemSelectReply) Html.Events.targetValue) ]
                           )
                    )
                    options
                ]
            , case mbItemPath of
                Just itemPath ->
                    div [ class "c-questionnaire-item-link" ] [ textButton (gettext "Go to item" locale) (ScrollToPath itemPath) ]

                Nothing ->
                    text ""
            , warning
            , if itemMissing then
                Alert.toHtml (Alert.warning (gettext "The selected item was deleted." locale))

              else
                text ""
            ]
        , viewClearReply { locale = locale, answered = answered, questionPath = node.questionPath, flags = flags }
        , viewAnsweredBy { locale = locale, mbReply = mbReply, answeredAt = answeredAt, flags = flags }
        ]



-- FILE


viewFile : Gettext.Locale -> QuestionActions -> QuestionNodeData -> Flags -> String -> String -> String -> Int -> Html Msg
viewFile locale actions node flags answeredAt replyString fileString commentCount =
    let
        mbReply =
            ReplyString.toReply replyString

        readOnly =
            Flags.isReadOnly flags

        ( content, clearReplyVisible ) =
            case Maybe.andThen (ReplyValue.getFileUuid << .value) mbReply of
                Just _ ->
                    case Result.toMaybe (D.decodeString ProjectFileSimple.decoder fileString) of
                        Just file ->
                            ( div [ class "c-questionnaire-file" ]
                                [ span [ class "c-questionnaire-file__icon" ] [ Icon.toHtml (Icon.fromClass (FileIcon.getFileIcon file.fileName file.contentType)) ]
                                , textButton file.fileName (DownloadFile file.uuid)
                                , Text.muted ("(" ++ ByteUnits.toReadable file.fileSize ++ ")")
                                , if readOnly then
                                    text ""

                                  else
                                    Button.toHtml
                                        (Button.dangerIconOnly (Icon.solid "fa-trash")
                                            (gettext "Delete" locale)
                                            (DeleteFile file.uuid file.fileName node.questionPath)
                                            |> Button.withTooltip
                                            |> Button.withTestId "file-delete"
                                        )
                                ]
                            , False
                            )

                        Nothing ->
                            ( Alert.toHtml (Alert.warning (gettext "The file was deleted." locale)), True )

                Nothing ->
                    ( Button.toHtml
                        (Button.secondary (gettext "Upload file" locale)
                            (OpenFileUpload node.questionPath { fileTypes = Question.getFileTypes node.question, maxSize = Question.getMaxSize node.question })
                            |> Button.withIcon (Icon.solid "fa-upload")
                            |> Button.withDisabled readOnly
                            |> Button.withTestId "file-upload"
                        )
                    , False
                    )

        answered =
            ReplyString.isAnswered mbReply
    in
    wrapper
        { locale = locale, actions = actions, node = node, flags = flags, commentCount = commentCount, answered = answered }
        [ div [ class "c-questionnaire-value" ] [ content ]
        , if clearReplyVisible then
            viewClearReply { locale = locale, answered = answered, questionPath = node.questionPath, flags = flags }

          else
            text ""
        , viewAnsweredBy { locale = locale, mbReply = mbReply, answeredAt = answeredAt, flags = flags }
        ]



-- INTEGRATION


{-| An integration answered from hints: an API integration, or a plugin
integration the client has built in, which is the only plugin integration
drawn here. That one takes no custom reply, and an empty search lists
everything it has.
-}
viewIntegrationApi : Gettext.Locale -> QuestionActions -> QuestionNodeData -> Flags -> Maybe TypeHints -> String -> String -> Int -> Html Msg
viewIntegrationApi locale actions node flags mbTypeHints answeredAt replyString commentCount =
    let
        search =
            case node.questionSpecificData of
                IntegrationQuestionSpecificNodeData data ->
                    case data.integration of
                        Just (Integration.ApiIntegration api) ->
                            { allowCustomReply = api.allowCustomReply, allowEmptySearch = api.requestAllowEmptySearch }

                        Just (Integration.PluginIntegration _) ->
                            { allowCustomReply = False, allowEmptySearch = True }

                        Nothing ->
                            { allowCustomReply = False, allowEmptySearch = False }

                _ ->
                    { allowCustomReply = False, allowEmptySearch = False }

        allowCustomReply =
            search.allowCustomReply

        fieldId =
            Path.toFieldId node.questionPath

        mbReply =
            ReplyString.toReply replyString

        mbReplyValue =
            Maybe.map .value mbReply

        readOnly =
            Flags.isReadOnly flags

        hintsVisible =
            not readOnly && Maybe.unwrap False ((==) node.questionPath << .path) mbTypeHints

        -- The hints that can be moved through, and the one the cursor is on.
        ( hints, highlighted ) =
            case ( hintsVisible, mbTypeHints ) of
                ( True, Just typeHints ) ->
                    let
                        loaded =
                            ActionResult.withDefault [] typeHints.hints
                    in
                    ( loaded
                    , Maybe.andThen (\index -> Maybe.map (Tuple.pair index) (List.getAt index loaded)) typeHints.highlighted
                    )

                _ ->
                    ( [], Nothing )

        combobox =
            [ attribute "role" "combobox"
            , attribute "aria-autocomplete" "list"
            , attribute "aria-expanded" (boolToString hintsVisible)
            , attribute "aria-activedescendant" (Maybe.unwrap "" (typeHintId node.questionPath << Tuple.first) highlighted)
            ]
                ++ (if hintsVisible then
                        [ attribute "aria-controls" (typeHintsListId node.questionPath) ]

                    else
                        []
                   )

        events =
            if readOnly then
                [ disabled True ]

            else
                let
                    questionUuid =
                        Question.getUuid node.question

                    requestAllowEmptySearch =
                        search.allowEmptySearch

                    -- Searches for what is in the input, which is not saved
                    -- when the reply cannot be custom.
                    showHints =
                        ShowTypeHints node.questionPath requestAllowEmptySearch questionUuid
                in
                [ onInput (TypeHintInput node.questionPath allowCustomReply requestAllowEmptySearch questionUuid << IntegrationReply << IntegrationReplyType.PlainType)
                , onBlur HideTypeHints
                , on "focus" (D.map showHints targetValue)
                , onTypeHintKeyboard
                    { path = node.questionPath
                    , visible = hintsVisible
                    , count = List.length hints
                    , highlighted = highlighted
                    , show = D.map showHints targetValue
                    }
                ]

        searchInput attrs =
            input
                ([ type_ "text"
                 , class "ui-input"
                 , id fieldId
                 , name fieldId
                 , attribute "aria-labelledby" (Nest.titleId node.questionPath)
                 , attribute "autocomplete" "off"
                 ]
                    ++ attrs
                    ++ combobox
                    ++ events
                )
                []

        viewInput currentValue =
            if allowCustomReply then
                -- What is typed is the reply.
                div [ class "ui-field__affixed" ]
                    [ searchInput [ value currentValue ]
                    , if String.isEmpty currentValue then
                        text ""

                      else
                        span [ class "ui-field__affix ui-field__affix--icon" ]
                            [ Tooltip.view { label = gettext "Custom answer, not selected from the integration" locale, placement = Tooltip.End }
                                (Icon.toHtml (Icon.labeled (gettext "Custom answer, not selected from the integration" locale) (Icon.solid "fa-pen-to-square")))
                            ]
                    ]

            else
                -- What is typed is only the search: the input keeps it itself,
                -- and the reply is set once a hint is chosen.
                div [ class "ui-field__affixed" ]
                    [ span [ class "ui-field__affix ui-field__affix--icon", attribute "aria-hidden" "true" ]
                        [ Icon.toHtml (Icon.solid "fa-magnifying-glass") ]
                    , searchInput []
                    ]

        questionInput =
            case mbReplyValue of
                Just (IntegrationReply (IntegrationReplyType.IntegrationType selected _)) ->
                    div [ class "c-questionnaire-read-only-text" ] [ Markdown.view { headingOffset = 3 } selected ]

                Just (IntegrationReply (IntegrationReplyType.PlainType plain)) ->
                    viewInput plain

                _ ->
                    viewInput ""

        answered =
            ReplyString.isAnswered mbReply
    in
    wrapper
        { locale = locale, actions = actions, node = node, flags = flags, commentCount = commentCount, answered = answered }
        [ div [ class "c-questionnaire-value c-questionnaire-integration" ]
            [ questionInput
            , if hintsVisible then
                viewTypeHints locale node.questionPath (Maybe.map Tuple.first highlighted) mbTypeHints

              else
                text ""
            ]
        , viewClearReply { locale = locale, answered = answered, questionPath = node.questionPath, flags = flags }
        , viewAnsweredBy { locale = locale, mbReply = mbReply, answeredAt = answeredAt, flags = flags }
        ]


{-| The keys of the combobox (`Ui.Combobox`) on an integration's input: the arrows
move a cursor through the hints while focus stays in the input, Enter chooses the
one it is on, Escape closes them. Home and End are left to the text, which can be
a custom answer. Closed, Down opens them.
-}
onTypeHintKeyboard : { path : String, visible : Bool, count : Int, highlighted : Maybe ( Int, TypeHint ), show : Decoder Msg } -> Html.Attribute Msg
onTypeHintKeyboard config =
    let
        moveTo index =
            D.succeed ( HighlightTypeHint config.path (Just (modBy config.count index)), True )
    in
    preventDefaultOn "keydown"
        (D.field "key" D.string
            |> D.andThen
                (\key ->
                    case ( key, config.visible, config.highlighted ) of
                        ( "ArrowDown", False, _ ) ->
                            D.map (\msg -> ( msg, True )) config.show

                        ( "ArrowDown", True, _ ) ->
                            if config.count > 0 then
                                moveTo (Maybe.unwrap 0 ((+) 1 << Tuple.first) config.highlighted)

                            else
                                D.fail "no hints"

                        ( "ArrowUp", True, _ ) ->
                            if config.count > 0 then
                                moveTo (Maybe.unwrap (config.count - 1) (\( index, _ ) -> index - 1) config.highlighted)

                            else
                                D.fail "no hints"

                        ( "Escape", True, _ ) ->
                            D.succeed ( HideTypeHints, True )

                        ( "Enter", True, Just ( _, hint ) ) ->
                            D.succeed ( ChooseTypeHint config.path (typeHintReply hint), True )

                        _ ->
                            D.fail "not a hint key"
                )
        )


boolToString : Bool -> String
boolToString bool =
    if bool then
        "true"

    else
        "false"


typeHintReply : TypeHint -> ReplyValue
typeHintReply hint =
    IntegrationReply (IntegrationReplyType.IntegrationType hint.value hint.raw)


{-| The scrolling box of a question's hints.
-}
typeHintsId : String -> String
typeHintsId questionPath =
    Path.toFieldId questionPath ++ "-hints"


typeHintsListId : String -> String
typeHintsListId questionPath =
    Path.toFieldId questionPath ++ "-hints-list"


typeHintId : String -> Int -> String
typeHintId questionPath index =
    Path.toFieldId questionPath ++ "-hint-" ++ String.fromInt index


viewTypeHints : Gettext.Locale -> String -> Maybe Int -> Maybe TypeHints -> Html Msg
viewTypeHints locale questionPath highlighted mbTypeHints =
    case mbTypeHints of
        Just typeHints ->
            div [ class "c-questionnaire-type-hints", id (typeHintsId questionPath) ] [ viewTypeHintsResult locale questionPath highlighted typeHints ]

        Nothing ->
            text ""


viewTypeHintsResult : Gettext.Locale -> String -> Maybe Int -> TypeHints -> Html Msg
viewTypeHintsResult locale questionPath highlighted typeHints =
    case typeHints.hints of
        ActionResult.Success [] ->
            div [ class "c-questionnaire-type-hints__status", attribute "role" "status" ] [ text (gettext "There are no results for your search." locale) ]

        ActionResult.Success hints ->
            ul
                [ class "c-questionnaire-type-hints__list"
                , id (typeHintsListId questionPath)
                , attribute "role" "listbox"
                , attribute "aria-labelledby" (Nest.titleId questionPath)
                ]
                (List.indexedMap
                    (\index hint ->
                        let
                            isHighlighted =
                                highlighted == Just index
                        in
                        li
                            [ id (typeHintId questionPath index)
                            , attribute "role" "option"
                            , attribute "aria-selected" (boolToString isHighlighted)
                            , classList [ ( "c-questionnaire-type-hint", True ), ( "is-highlighted", isHighlighted ) ]

                            -- The press keeps focus in the input; the choice is
                            -- made on the click (WCAG 2.5.2).
                            , preventDefaultOn "mousedown" (D.succeed ( Noop, True ))
                            , onClick (ChooseTypeHint questionPath (typeHintReply hint))
                            , onMouseEnter (HighlightTypeHint questionPath (Just index))
                            ]
                            [ Markdown.view { headingOffset = 3 } (Maybe.withDefault hint.value hint.valueForSelection) ]
                    )
                    hints
                )

        ActionResult.Loading ->
            div [ class "c-questionnaire-type-hints__status", attribute "role" "status" ] [ Icon.toHtml Icon.spinner, text (gettext "Loading..." locale) ]

        ActionResult.Error error ->
            div [ class "c-questionnaire-type-hints__status", attribute "role" "alert" ] [ text error ]

        ActionResult.Unset ->
            text ""


{-| A plugin's element, with what the node carries for it and the project,
which can change while the node does not.
-}
viewIntegrationPlugin : Gettext.Locale -> QuestionActions -> QuestionNodeData -> Flags -> String -> String -> String -> Int -> Html Msg
viewIntegrationPlugin locale actions node flags answeredAt replyString project commentCount =
    let
        mbReply =
            ReplyString.toReply replyString

        answered =
            ReplyString.isAnswered mbReply

        content =
            case pluginOf node of
                Just pluginData ->
                    if answered && not pluginData.rendersReply then
                        div [ class "c-questionnaire-read-only-text" ]
                            [ Markdown.view { headingOffset = 3 } (Maybe.unwrap "" (ReplyValue.getStringReply << .value) mbReply) ]

                    else
                        PluginElement.element pluginData.pluginElement
                            [ PluginElement.settingValue pluginData.settings
                            , PluginElement.userSettingsValue pluginData.userSettings
                            , PluginElement.userValue pluginData.user
                            , PluginElement.projectValueString project
                            , PluginElement.pluginIntegrationSettingsValue pluginData.integrationSettings
                            , PluginElement.integrationReplyValue (Maybe.unwrap "" (E.encode 0 << ReplyValue.encode << .value) mbReply)
                            , PluginElement.onReplyValueChange (SetPluginReply node.questionPath)
                            ]

                _ ->
                    Alert.toHtml (Alert.danger (gettext "Plugin error." locale))
    in
    wrapper
        { locale = locale, actions = actions, node = node, flags = flags, commentCount = commentCount, answered = answered }
        [ div [ class "c-questionnaire-value" ] [ content ]
        , viewClearReply { locale = locale, answered = answered, questionPath = node.questionPath, flags = flags }
        , viewAnsweredBy { locale = locale, mbReply = mbReply, answeredAt = answeredAt, flags = flags }
        ]


pluginOf : QuestionNodeData -> Maybe IntegrationPluginData
pluginOf node =
    case node.questionSpecificData of
        IntegrationQuestionSpecificNodeData specific ->
            case specific.answering of
                AnsweredByPlugin pluginData ->
                    Just pluginData

                _ ->
                    Nothing

        _ ->
            Nothing


{-| A question that cannot be shown: its plugin is missing, or its integration is
not set up. Not lazy — it is rare, and says one sentence.
-}
viewMessage : Gettext.Locale -> QuestionActions -> QuestionNodeData -> Flags -> Bool -> Int -> Alert.Alert -> Html Msg
viewMessage locale actions node flags answered commentCount alert =
    wrapper
        { locale = locale, actions = actions, node = node, flags = flags, commentCount = commentCount, answered = answered }
        [ Alert.toHtml alert ]



-- WRAPPER


type alias WrapperProps =
    { locale : Gettext.Locale
    , actions : QuestionActions
    , node : QuestionNodeData
    , flags : Flags
    , commentCount : Int
    , answered : Bool
    }


type QuestionState
    = Answered
    | Desirable
    | Later


{-| The question around its inputs: its state, number and title, its actions, its
tags, text and references — and the list item it is.
-}
wrapper : WrapperProps -> List (Html Msg) -> Html Msg
wrapper props content =
    let
        titleId =
            Nest.titleId props.node.questionPath

        state =
            if props.answered then
                Answered

            else if props.node.isDesirable then
                Desirable

            else
                Later

        -- The title takes the state's color with the number, as in the wizard:
        -- green once answered, red while it is wanted now.
        ( stateIcon, stateLabel, stateModifier ) =
            case state of
                Answered ->
                    ( Icon.solid "fa-check", gettext "This question has been answered" props.locale, "answered" )

                Desirable ->
                    ( Icon.solid "fa-pen", gettext "This question should be answered now" props.locale, "desirable" )

                Later ->
                    ( Icon.regular "fa-hourglass", gettext "This question can be answered later" props.locale, "later" )

        tags =
            if Flags.showTags props.flags && not (List.isEmpty props.node.tags) then
                div [ class "c-questionnaire-question__tags" ]
                    [ Tag.list (List.map (\tag -> Tag.withColor tag.color (Tag.tag tag.name)) props.node.tags) ]

            else
                text ""

        questionText =
            case Question.getText props.node.question of
                Just markdown ->
                    div [ class "c-questionnaire-question__text" ] [ Markdown.view { headingOffset = 3 } markdown ]

                Nothing ->
                    text ""
    in
    Nest.listItem
        { level = Nodes.depth props.node.nestingType
        , labelledBy = titleId
        , attributes = []
        }
        props.node.nestingType
        [ div
            [ class "c-questionnaire-question"
            , classList [ ( "is-highlighted", Flags.isHighlighted props.flags ) ]
            , attribute "data-path" props.node.questionPath
            ]
            [ div [ class "c-questionnaire-question__header" ]
                [ div [ class "c-questionnaire-question__heading" ]
                    [ -- The state said off-screen with the number, and shown on hover
                      -- for the eye (the tooltip is hidden from assistive technology).
                      Tooltip.view { label = stateLabel, placement = Tooltip.Start }
                        (span [ class ("c-questionnaire-state c-questionnaire-state--" ++ stateModifier) ]
                            [ Icon.toHtml stateIcon
                            , text (String.join "." props.node.humanIdentifier)
                            , Text.visuallyHiddenInline (" " ++ stateLabel)
                            ]
                        )
                    , strong [ class ("c-questionnaire-question__title c-questionnaire-question__title--" ++ stateModifier), id titleId, TestId.attribute "questionnaire_question-title" ]
                        [ text (Question.getTitle props.node.question) ]
                    ]
                , viewActions props
                ]
            , tags
            , questionText
            , viewExtra props.locale props.flags props.node.questionExtraData
            , div
                [ class "c-questionnaire-question__inputs"
                , attribute "role" "group"
                , attribute "aria-labelledby" titleId
                ]
                content
            ]
        ]


viewActions : WrapperProps -> Html Msg
viewActions props =
    if Flags.showActions props.flags then
        let
            path =
                props.node.questionPath

            readOnly =
                Flags.isReadOnly props.flags

            -- The actions are quiet and small, one height, their tooltips opening
            -- below and towards the start, clear of the content's edge.
            action button =
                Button.toHtml (button |> Button.quiet |> Button.small)

            iconAction icon label msg =
                action (Button.iconOnly icon label msg |> Button.withTooltipAt Tooltip.End)

            -- A TODO is an icon to add it, and once there a badge saying TODO
            -- with a cross that takes it away.
            todo =
                if readOnly then
                    []

                else if Flags.hasTodo props.flags then
                    [ span [ class "c-questionnaire-todo" ]
                        [ text (gettext "TODO" props.locale)
                        , iconAction (Icon.solid "fa-xmark") (gettext "Remove TODO" props.locale) (RemoveTodo path)
                        ]
                    ]

                else
                    [ iconAction (Icon.solid "fa-plus") (gettext "Add TODO" props.locale) (AddTodo path) ]

            comments =
                if Flags.commentsEnabled props.flags then
                    let
                        msg =
                            if Flags.hasCommentsOpen props.flags then
                                CloseRightPanel

                            else
                                OpenComments path

                        button =
                            if props.commentCount > 0 then
                                Button.ghost (String.format (ngettext ( "1 comment", "%s comments" ) props.commentCount props.locale) [ String.fromInt props.commentCount ]) msg
                                    |> Button.withIcon (Icon.solid "fa-comment")
                                    |> Button.withPressed (Flags.hasCommentsOpen props.flags)

                            else
                                Button.iconOnly (Icon.regular "fa-comment") (gettext "Add comment" props.locale) msg
                                    |> Button.withTooltipAt Tooltip.End
                                    |> Button.withPressed (Flags.hasCommentsOpen props.flags)
                    in
                    [ action (Button.withTestId "questionnaire_question-action_comment" button) ]

                else
                    []

            plugins =
                if readOnly then
                    []

                else
                    List.map (viewPluginAction props) props.actions.plugins

            copyLabel =
                if Flags.hasCopiedLink props.flags then
                    gettext "Copied!" props.locale

                else
                    gettext "Copy link" props.locale
        in
        div [ class "c-questionnaire-question__actions" ]
            (todo ++ comments ++ plugins ++ [ iconAction (Icon.solid "fa-link") copyLabel (CopyLinkToQuestion path) ])

    else
        text ""


viewPluginAction : WrapperProps -> ( Common.Plugins.Plugin.Plugin, Common.Plugins.Plugin.ProjectQuestionActionConnector ) -> Html Msg
viewPluginAction props ( plugin, connector ) =
    let
        msg =
            case connector.type_ of
                ModalProjectQuestionAction ->
                    OpenPluginQuestionActionModal plugin.uuid connector.element props.node.question props.node.questionPath

                SidebarProjectQuestionAction ->
                    if props.actions.open == Just ( plugin.uuid, connector ) then
                        CloseRightPanel

                    else
                        OpenPluginQuestionActionRightPanel { plugin = plugin, connector = connector, question = props.node.question, questionPath = props.node.questionPath }
    in
    Button.toHtml
        (Button.iconOnly (Icon.fromClass connector.action.icon) (gettext connector.action.name props.locale) msg
            |> Button.quiet
            |> Button.small
            |> Button.withTooltipAt Tooltip.End
            |> (case connector.type_ of
                    SidebarProjectQuestionAction ->
                        Button.withPressed (props.actions.open == Just ( plugin.uuid, connector ))

                    ModalProjectQuestionAction ->
                        identity
               )
        )


viewExtra : Gettext.Locale -> Flags -> QuestionExtraData -> Html Msg
viewExtra locale flags extra =
    let
        requiredPhase =
            case ( Flags.showPhases flags, extra.requiredPhase ) of
                ( True, Just phase ) ->
                    [ extraGroup (Icon.regular "fa-square-check") (gettext "Desirable" locale) [ text phase.title ] ]

                _ ->
                    []

        items icon label viewItem list =
            if List.isEmpty list then
                []

            else
                [ extraGroup icon label (List.intersperse (text ", ") (List.map viewItem list)) ]

        resourceCollection collection =
            items (Icon.solid "fa-book") collection.title (\page -> externalLink page.title page.url) collection.resourcePages
    in
    case
        requiredPhase
            ++ List.concatMap resourceCollection extra.resourceCollections
            ++ items (Icon.solid "fa-up-right-from-square") (gettext "External links" locale) viewUrlReference extra.urlReferences
            ++ items (Icon.regular "fa-comment") (gettext "Related questions" locale) viewCrossReference extra.crossReferences
            ++ items (Icon.regular "fa-address-book") (gettext "Experts" locale) viewExpert extra.experts
    of
        [] ->
            text ""

        groups ->
            div [ class "c-questionnaire-question__extra", TestId.attribute "questionnaire_question-extra" ] groups


{-| One kind of extra on a line of its own — "Experts: …" — as in the wizard.
The label's colon is followed by a space outside the label's box, where a
trailing space inside it would be dropped.
-}
extraGroup : Icon.Icon -> String -> List (Html msg) -> Html msg
extraGroup icon label content =
    div [ class "c-questionnaire-extra" ]
        (span [ class "c-questionnaire-extra__label" ] [ Icon.toHtml icon, text (label ++ ":") ] :: text " " :: content)


externalLink : String -> String -> Html msg
externalLink label url =
    a [ class "ui-link", href url, target "_blank", rel "noopener noreferrer" ] [ text label ]


viewUrlReference : QuestionExtraUrlReference -> Html msg
viewUrlReference reference =
    externalLink
        (if String.isEmpty reference.label then
            reference.url

         else
            reference.label
        )
        reference.url


viewCrossReference : QuestionExtraCrossReference -> Html Msg
viewCrossReference reference =
    span []
        [ textButton reference.targetQuestionTitle (ScrollToQuestion reference.targetQuestionUuid)
        , if String.isEmpty reference.description then
            text ""

          else
            text (" (" ++ reference.description ++ ")")
        ]


viewExpert : Expert -> Html msg
viewExpert expert =
    let
        mail =
            a [ class "ui-link", href ("mailto:" ++ expert.email) ] [ text expert.email ]
    in
    span [] <|
        if String.isEmpty expert.name then
            [ mail ]

        else
            [ text (expert.name ++ " ("), mail, text ")" ]


viewClearReply : { locale : Gettext.Locale, answered : Bool, questionPath : String, flags : Flags } -> Html Msg
viewClearReply props =
    if Flags.isReadOnly props.flags || not props.answered then
        text ""

    else
        div [ class "c-questionnaire-clear" ]
            [ Button.toHtml
                (Button.ghost (gettext "Clear answer" props.locale) (ClearReply props.questionPath)
                    |> Button.withIcon (Icon.solid "fa-rotate-left")
                    |> Button.small
                )
            ]


viewAnsweredBy : { locale : Gettext.Locale, mbReply : Maybe Reply, answeredAt : String, flags : Flags } -> Html msg
viewAnsweredBy props =
    case ( props.mbReply, ReplyString.answeredAtFromString props.answeredAt, Flags.showAnsweredBy props.flags ) of
        ( Just reply, Just time, True ) ->
            let
                userName =
                    Maybe.unwrap (gettext "anonymous user" props.locale) User.fullName reply.createdBy
            in
            div [ class "c-questionnaire-answered-by", TestId.attribute "questionnaire_answered-by" ]
                (String.formatHtml (gettext "Answered %s by %s." props.locale)
                    [ Html.node "time" [ attribute "title" time.readable ] [ text time.distance ]
                    , text userName
                    ]
                )

        _ ->
            text ""


{-| An action in a sentence — "Go to item", a related question — that stays on
the page: a button that reads as a link.
-}
textButton : String -> msg -> Html msg
textButton label msg =
    Html.button [ type_ "button", class "c-questionnaire-text-button", onClick msg ] [ text label ]
