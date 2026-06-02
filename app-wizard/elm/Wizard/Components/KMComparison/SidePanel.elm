module Wizard.Components.KMComparison.SidePanel exposing (SidePanelProps, SidePanelState(..), viewSidePanel)

import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (faClose, fas)
import Common.Utils.ByteUnits as ByteUnits
import Dict
import Diff
import Flip exposing (flip)
import Gettext exposing (gettext)
import Html exposing (Html, a, del, div, h5, ins, li, span, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import List.Extra as List
import Maybe.Extra as Maybe
import String.Extra as String
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Choice exposing (Choice)
import Wizard.Api.Models.KnowledgeModel.Expert exposing (Expert)
import Wizard.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration)
import Wizard.Api.Models.KnowledgeModel.Metric exposing (Metric)
import Wizard.Api.Models.KnowledgeModel.Phase exposing (Phase)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Wizard.Api.Models.KnowledgeModel.Question.QuestionValueType as QuestionValueType
import Wizard.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference(..))
import Wizard.Api.Models.KnowledgeModel.Reference.CrossReferenceData as CrossReference
import Wizard.Api.Models.KnowledgeModel.Reference.ResourcePageReferenceData as ResourcePageReference
import Wizard.Api.Models.KnowledgeModel.ResourceCollection exposing (ResourceCollection)
import Wizard.Api.Models.KnowledgeModel.ResourcePage exposing (ResourcePage)
import Wizard.Api.Models.KnowledgeModel.Tag exposing (Tag)
import Wizard.Components.KMComparison.Differ as Differ


type SidePanelState
    = SidePanelChapter (Differ.DiffResult Chapter)
    | SidePanelQuestion (Differ.DiffResult Question)
    | SidePanelAnswer (Differ.DiffResult Answer)
    | SidePanelChoice (Differ.DiffResult Choice)
    | SidePanelReference (Differ.DiffResult Reference)
    | SidePanelExpert (Differ.DiffResult Expert)
    | SidePanelMetric (Differ.DiffResult Metric)
    | SidePanelPhase (Differ.DiffResult Phase)
    | SidePanelTag (Differ.DiffResult Tag)
    | SidePanelIntegration (Differ.DiffResult Integration)
    | SidePanelResourceCollection (Differ.DiffResult ResourceCollection)
    | SidePanelResourcePage (Differ.DiffResult ResourcePage)


type alias SidePanelProps msg =
    { locale : Gettext.Locale
    , leftKm : KnowledgeModel
    , rightKm : KnowledgeModel
    , closeMsg : msg
    }


type ContentType
    = AddedContentType
    | RemovedContentType
    | DefaultContentType


viewSidePanel : SidePanelProps msg -> SidePanelState -> Html msg
viewSidePanel props sidePanel =
    div [ class "kmComparison__sidePanel" ]
        (viewSidePanelContent props sidePanel)


viewSidePanelContent : SidePanelProps msg -> SidePanelState -> List (Html msg)
viewSidePanelContent props sidePanel =
    case sidePanel of
        SidePanelChapter chapterDiff ->
            sidePanelHeader props chapterDiff
                :: h5 [] [ text (gettext "Chapter" props.locale) ]
                :: sidePanelContent chapterDiff (chapterContentFields props) (chapterContentDiffFields props)

        SidePanelQuestion questionDiff ->
            sidePanelHeader props questionDiff
                :: h5 [] [ text (gettext "Question" props.locale) ]
                :: sidePanelContent questionDiff (questionContentFields props) (questionContentDiffFields props)

        SidePanelAnswer answerDiff ->
            sidePanelHeader props answerDiff
                :: h5 [] [ text (gettext "Answer" props.locale) ]
                :: sidePanelContent answerDiff (answerContentFields props) (answerContentDiffFields props)

        SidePanelChoice choiceDiff ->
            sidePanelHeader props choiceDiff
                :: h5 [] [ text (gettext "Choice" props.locale) ]
                :: sidePanelContent choiceDiff (choiceContentFields props) (choiceContentDiffFields props)

        SidePanelReference referenceDiff ->
            sidePanelHeader props referenceDiff
                :: h5 [] [ text (gettext "Reference" props.locale) ]
                :: sidePanelContent referenceDiff (referenceContentFields props) (referenceContentDiffFields props)

        SidePanelExpert expertDiff ->
            sidePanelHeader props expertDiff
                :: h5 [] [ text (gettext "Expert" props.locale) ]
                :: sidePanelContent expertDiff (expertContentFields props) (expertContentDiffFields props)

        SidePanelMetric metricDiff ->
            sidePanelHeader props metricDiff
                :: h5 [] [ text (gettext "Metric" props.locale) ]
                :: sidePanelContent metricDiff (metricContentFields props) (metricContentDiffFields props)

        SidePanelPhase phaseDiff ->
            sidePanelHeader props phaseDiff
                :: h5 [] [ text (gettext "Phase" props.locale) ]
                :: sidePanelContent phaseDiff (phaseContentFields props) (phaseContentDiffFields props)

        SidePanelTag tagDiff ->
            sidePanelHeader props tagDiff
                :: h5 [] [ text (gettext "Tag" props.locale) ]
                :: sidePanelContent tagDiff (tagContentFields props) (tagContentDiffFields props)

        SidePanelIntegration integrationDiff ->
            sidePanelHeader props integrationDiff
                :: h5 [] [ text (gettext "Integration" props.locale) ]
                :: sidePanelContent integrationDiff (integrationContentFields props) (integrationContentDiffFields props)

        SidePanelResourceCollection resourceCollectionDiff ->
            sidePanelHeader props resourceCollectionDiff
                :: h5 [] [ text (gettext "Resource Collection" props.locale) ]
                :: sidePanelContent resourceCollectionDiff (resourceCollectionContentFields props) (resourceCollectionContentDiffFields props)

        SidePanelResourcePage resourcePageDiff ->
            sidePanelHeader props resourcePageDiff
                :: h5 [] [ text (gettext "Resource Page" props.locale) ]
                :: sidePanelContent resourcePageDiff (resourcePageContentFields props) (resourcePageContentDiffFields props)


sidePanelHeader : SidePanelProps msg -> Differ.DiffResult a -> Html msg
sidePanelHeader props diff =
    div [ class "d-flex justify-content-between align-items-center mb-3" ]
        [ sidePanelBadge props.locale diff
        , a [ class "p-1", onClick props.closeMsg ] [ faClose ]
        ]


sidePanelBadge : Gettext.Locale -> Differ.DiffResult a -> Html msg
sidePanelBadge locale diff =
    case diff of
        Differ.Added _ ->
            Badge.success [] [ text (gettext "Added" locale) ]

        Differ.Removed _ ->
            Badge.danger [] [ text (gettext "Removed" locale) ]

        Differ.Changed _ _ ->
            Badge.warning [] [ text (gettext "Changed" locale) ]

        Differ.NoChange _ _ ->
            Badge.secondary [] [ text (gettext "No Change" locale) ]


sidePanelContent : Differ.DiffResult a -> (a -> ContentType -> List ( String, Html msg )) -> (a -> a -> List ( String, Html msg )) -> List (Html msg)
sidePanelContent result getContentFields getContentDiffFields =
    case result of
        Differ.Added item ->
            viewAdd (getContentFields item)

        Differ.Removed item ->
            viewRemoved (getContentFields item)

        Differ.NoChange _ item ->
            viewDefault (getContentFields item)

        Differ.Changed leftItem rightItem ->
            viewDefault (always (getContentDiffFields leftItem rightItem))


viewAdd : (ContentType -> List ( String, Html msg )) -> List (Html msg)
viewAdd getFields =
    getFields AddedContentType
        |> List.indexedMap (\i ( key, value ) -> viewRow i key value)


viewRemoved : (ContentType -> List ( String, Html msg )) -> List (Html msg)
viewRemoved getFields =
    getFields RemovedContentType
        |> List.indexedMap (\i ( key, value ) -> viewRow i key value)


viewDefault : (ContentType -> List ( String, Html msg )) -> List (Html msg)
viewDefault getFields =
    getFields DefaultContentType
        |> List.indexedMap (\i ( key, value ) -> viewRow i key value)


viewRow : Int -> String -> Html msg -> Html msg
viewRow index key value =
    div
        [ class "py-2 border-bottom"
        , classList [ ( "border-top", index == 0 ) ]
        ]
        [ div [ class "badge px-0 text-muted" ] [ text key ]
        , div [ class "text-break" ] [ value ]
        ]



-- Chapter


chapterContentFields : SidePanelProps msg -> Chapter -> ContentType -> List ( String, Html msg )
chapterContentFields props chapter contentType =
    [ ( gettext "Title" props.locale
      , viewContent contentType [ text chapter.title ]
      )
    , ( gettext "Text" props.locale
      , wrapMaybeValue chapter.text <|
            viewContent contentType [ text (Maybe.withDefault "" chapter.text) ]
      )
    ]


chapterContentDiffFields : SidePanelProps msg -> Chapter -> Chapter -> List ( String, Html msg )
chapterContentDiffFields props leftChapter rightChapter =
    [ ( gettext "Title" props.locale
      , renderTextDiff leftChapter.title rightChapter.title
      )
    , ( gettext "Text" props.locale
      , wrapMaybeValue (Maybe.or leftChapter.text rightChapter.text) <|
            renderMaybeTextDiff leftChapter.text rightChapter.text
      )
    ]



-- Question


questionContentFields : SidePanelProps msg -> Question -> ContentType -> List ( String, Html msg )
questionContentFields props question contentType =
    let
        km =
            case contentType of
                RemovedContentType ->
                    props.leftKm

                _ ->
                    props.rightKm

        -- Text
        questionText =
            Question.getText question

        -- Required Phase
        requiredPhase =
            case Question.getRequiredPhaseUuid question of
                Just phaseUuid ->
                    case KnowledgeModel.getPhase phaseUuid km of
                        Just phase ->
                            viewContent contentType [ text phase.title ]

                        Nothing ->
                            emptyValue

                Nothing ->
                    emptyValue

        -- Tags
        questionTags =
            KnowledgeModel.getQuestionTags (Question.getUuid question) km

        tags =
            if List.isEmpty questionTags then
                emptyValue

            else
                viewContentList contentType (List.map (\tag -> li [] [ text tag.name ]) questionTags)

        questionTypeFields =
            case question of
                ValueQuestion _ data ->
                    [ ( gettext "Value Type" props.locale
                      , viewContent contentType [ text (QuestionValueType.toReadableString props.locale data.valueType) ]
                      )
                    ]

                IntegrationQuestion _ data ->
                    case KnowledgeModel.getIntegration data.integrationUuid km of
                        Just integration ->
                            let
                                variables =
                                    if List.isEmpty (Integration.getVariables integration) then
                                        []

                                    else
                                        let
                                            viewVariable ( key, value ) =
                                                li [] [ text (key ++ ": " ++ value) ]
                                        in
                                        [ ( gettext "Variables" props.locale
                                          , viewContentList contentType (List.map viewVariable (Dict.toList data.variables))
                                          )
                                        ]
                            in
                            ( gettext "Integration" props.locale
                            , viewContent contentType [ text (Integration.getName integration) ]
                            )
                                :: variables

                        Nothing ->
                            [ ( gettext "Integration" props.locale
                              , emptyValue
                              )
                            ]

                ItemSelectQuestion _ data ->
                    let
                        viewListQuestion title =
                            viewContent contentType [ text title ]

                        listQuestion =
                            data.listQuestionUuid
                                |> Maybe.andThen (flip KnowledgeModel.getQuestion km)
                                |> Maybe.map Question.getTitle
                                |> Maybe.unwrap emptyValue viewListQuestion
                    in
                    [ ( gettext "List Question" props.locale
                      , listQuestion
                      )
                    ]

                FileQuestion _ data ->
                    [ ( gettext "File Types" props.locale
                      , wrapMaybeValue data.fileTypes <|
                            viewContent contentType [ text (Maybe.withDefault "" data.fileTypes) ]
                      )
                    , ( gettext "Max Size" props.locale
                      , wrapMaybeValue data.maxSize <|
                            viewContent contentType [ text (Maybe.unwrap "" ByteUnits.toReadable data.maxSize) ]
                      )
                    ]

                _ ->
                    []
    in
    [ ( gettext "Type" props.locale
      , viewContent contentType [ text (Question.getTypeString question) ]
      )
    , ( gettext "Title" props.locale
      , viewContent contentType [ text (Question.getTitle question) ]
      )
    , ( gettext "Text" props.locale
      , wrapMaybeValue questionText <|
            viewContent contentType [ text (Maybe.withDefault "" questionText) ]
      )
    , ( gettext "Required Phase" props.locale
      , requiredPhase
      )
    , ( gettext "Tags" props.locale
      , tags
      )
    ]
        ++ questionTypeFields


questionContentDiffFields : SidePanelProps msg -> Question -> Question -> List ( String, Html msg )
questionContentDiffFields props leftQuestion rightQuestion =
    let
        -- Text
        leftQuestionText =
            Question.getText leftQuestion

        rightQuestionText =
            Question.getText rightQuestion

        -- Required Phase
        leftRequiredPhase =
            Question.getRequiredPhaseUuid leftQuestion
                |> Maybe.andThen (flip KnowledgeModel.getPhase props.leftKm)

        rightRequiredPhase =
            Question.getRequiredPhaseUuid rightQuestion
                |> Maybe.andThen (flip KnowledgeModel.getPhase props.rightKm)

        requiredPhase =
            case ( leftRequiredPhase, rightRequiredPhase ) of
                ( Just leftPhase, Just rightPhase ) ->
                    viewContentDiffRow (leftPhase.uuid == rightPhase.uuid)
                        leftPhase.title
                        rightPhase.title

                _ ->
                    emptyValue

        -- Tags
        leftQuestionTags =
            KnowledgeModel.getQuestionTags (Question.getUuid leftQuestion) props.leftKm

        rightQuestionTags =
            KnowledgeModel.getQuestionTags (Question.getUuid rightQuestion) props.rightKm

        tagsDiff =
            Differ.createDiff .uuid (\tag1 tag2 -> tag1.name == tag2.name) leftQuestionTags rightQuestionTags
                |> List.map
                    (\diff ->
                        case diff of
                            Differ.Added tag ->
                                ins [] [ text tag.name ]

                            Differ.Removed tag ->
                                del [] [ text tag.name ]

                            Differ.NoChange tag _ ->
                                text tag.name

                            Differ.Changed tag1 tag2 ->
                                renderTextDiff tag1.name tag2.name
                    )

        tags =
            if List.isEmpty tagsDiff then
                emptyValue

            else
                viewContentList DefaultContentType <|
                    List.map (\tag -> li [] [ tag ]) tagsDiff

        questionTypeFields =
            case rightQuestion of
                ValueQuestion _ data ->
                    let
                        leftQuestionValueType =
                            Question.getValueType leftQuestion
                    in
                    [ ( gettext "Value Type" props.locale
                      , viewContentDiffRow (leftQuestionValueType == Just data.valueType)
                            (Maybe.unwrap "" (QuestionValueType.toReadableString props.locale) leftQuestionValueType)
                            (QuestionValueType.toReadableString props.locale data.valueType)
                      )
                    ]

                IntegrationQuestion _ data ->
                    let
                        leftIntegration =
                            Question.getIntegrationUuid leftQuestion
                                |> Maybe.andThen (flip KnowledgeModel.getIntegration props.leftKm)

                        rightIntegration =
                            KnowledgeModel.getIntegration data.integrationUuid props.rightKm

                        leftIntegrationName =
                            Maybe.map Integration.getName leftIntegration

                        rightIntegrationName =
                            Maybe.map Integration.getName rightIntegration

                        integrationValue =
                            if Maybe.isNothing leftIntegrationName && Maybe.isNothing rightIntegrationName then
                                emptyValue

                            else
                                viewContentDiffRow (leftIntegrationName == rightIntegrationName)
                                    (Maybe.unwrap "" identity leftIntegrationName)
                                    (Maybe.unwrap "" identity rightIntegrationName)

                        leftQuestionVariables =
                            Question.getVariables leftQuestion
                                |> Maybe.unwrap [] Dict.toList

                        rightQuestionVariables =
                            Question.getVariables rightQuestion
                                |> Maybe.unwrap [] Dict.toList

                        variablesDiff =
                            Differ.createDiff identity (\( k1, v1 ) ( k2, v2 ) -> k1 == k2 && v1 == v2) leftQuestionVariables rightQuestionVariables
                                |> List.map
                                    (\diff ->
                                        case diff of
                                            Differ.Added ( key, value ) ->
                                                ins [] [ text (key ++ ": " ++ value) ]

                                            Differ.Removed ( key, value ) ->
                                                del [] [ text (key ++ ": " ++ value) ]

                                            Differ.NoChange ( key, value ) _ ->
                                                text (key ++ ": " ++ value)

                                            Differ.Changed ( key1, value1 ) ( key2, value2 ) ->
                                                span []
                                                    [ renderTextDiff key1 key2
                                                    , text ": "
                                                    , renderTextDiff value1 value2
                                                    ]
                                    )

                        variables =
                            if List.isEmpty variablesDiff then
                                []

                            else
                                [ ( gettext "Variables" props.locale
                                  , viewContentList DefaultContentType variablesDiff
                                  )
                                ]
                    in
                    ( gettext "Integration" props.locale
                    , integrationValue
                    )
                        :: variables

                ItemSelectQuestion _ data ->
                    let
                        leftListQuestionTitle =
                            data.listQuestionUuid
                                |> Maybe.andThen (flip KnowledgeModel.getQuestion props.leftKm)
                                |> Maybe.map Question.getTitle

                        rightListQuestionTitle =
                            data.listQuestionUuid
                                |> Maybe.andThen (flip KnowledgeModel.getQuestion props.rightKm)
                                |> Maybe.map Question.getTitle

                        listQuestionValue =
                            if Maybe.isNothing leftListQuestionTitle && Maybe.isNothing rightListQuestionTitle then
                                emptyValue

                            else
                                viewContentDiffRow (leftListQuestionTitle == rightListQuestionTitle)
                                    (Maybe.withDefault "" leftListQuestionTitle)
                                    (Maybe.withDefault "" rightListQuestionTitle)
                    in
                    [ ( gettext "List Question" props.locale
                      , listQuestionValue
                      )
                    ]

                FileQuestion _ _ ->
                    let
                        leftFileTypes =
                            Question.getFileTypes leftQuestion

                        rightFileTypes =
                            Question.getFileTypes rightQuestion

                        fileTypesValue =
                            renderTextDiff
                                (Maybe.withDefault "" leftFileTypes)
                                (Maybe.withDefault "" rightFileTypes)

                        leftMaxSize =
                            Question.getMaxSize leftQuestion

                        rightMaxSize =
                            Question.getMaxSize rightQuestion

                        maxSizeValue =
                            viewContentDiffRow (leftMaxSize == rightMaxSize)
                                (Maybe.unwrap "" ByteUnits.toReadable leftMaxSize)
                                (Maybe.unwrap "" ByteUnits.toReadable rightMaxSize)
                    in
                    [ ( gettext "File Types" props.locale
                      , wrapMaybeValue (Maybe.or leftFileTypes rightFileTypes) fileTypesValue
                      )
                    , ( gettext "Max Size" props.locale
                      , wrapMaybeValue (Maybe.or leftMaxSize rightMaxSize) maxSizeValue
                      )
                    ]

                _ ->
                    []
    in
    [ ( gettext "Type" props.locale
      , renderTextDiff (Question.getTypeString leftQuestion) (Question.getTypeString rightQuestion)
      )
    , ( gettext "Title" props.locale
      , renderTextDiff (Question.getTitle leftQuestion) (Question.getTitle rightQuestion)
      )
    , ( gettext "Text" props.locale
      , wrapMaybeValue (Maybe.or leftQuestionText rightQuestionText) <|
            renderMaybeTextDiff leftQuestionText rightQuestionText
      )
    , ( gettext "Required Phase" props.locale
      , requiredPhase
      )
    , ( gettext "Tags" props.locale
      , tags
      )
    ]
        ++ questionTypeFields



-- Answer


answerContentFields : SidePanelProps msg -> Answer -> ContentType -> List ( String, Html msg )
answerContentFields props answer contentType =
    [ ( gettext "Label" props.locale
      , viewContent contentType [ text answer.label ]
      )
    , ( gettext "Advice" props.locale
      , wrapMaybeValue answer.advice <|
            viewContent contentType [ text (Maybe.withDefault "" answer.advice) ]
      )
    ]


answerContentDiffFields : SidePanelProps msg -> Answer -> Answer -> List ( String, Html msg )
answerContentDiffFields props leftAnswer rightAnswer =
    [ ( gettext "Label" props.locale
      , renderTextDiff leftAnswer.label rightAnswer.label
      )
    , ( gettext "Advice" props.locale
      , wrapMaybeValue (Maybe.or leftAnswer.advice rightAnswer.advice) <|
            renderMaybeTextDiff leftAnswer.advice rightAnswer.advice
      )
    ]



-- Choice


choiceContentFields : SidePanelProps msg -> Choice -> ContentType -> List ( String, Html msg )
choiceContentFields props choice contentType =
    [ ( gettext "Label" props.locale
      , viewContent contentType [ text choice.label ]
      )
    ]


choiceContentDiffFields : SidePanelProps msg -> Choice -> Choice -> List ( String, Html msg )
choiceContentDiffFields props leftChoice rightChoice =
    [ ( gettext "Label" props.locale
      , renderTextDiff leftChoice.label rightChoice.label
      )
    ]



-- Reference


referenceContentFields : SidePanelProps msg -> Reference -> ContentType -> List ( String, Html msg )
referenceContentFields props reference contentType =
    let
        km =
            case contentType of
                RemovedContentType ->
                    props.leftKm

                _ ->
                    props.rightKm

        referenceTypeFields =
            case reference of
                ResourcePageReference data ->
                    let
                        resourcePageLabel =
                            ResourcePageReference.toLabel (KnowledgeModel.getAllResourcePages km) data
                    in
                    [ ( gettext "Resource Page" props.locale
                      , wrapMaybeValue (String.toMaybe resourcePageLabel) <|
                            viewContent contentType [ text resourcePageLabel ]
                      )
                    ]

                URLReference data ->
                    [ ( gettext "URL" props.locale
                      , viewContent contentType [ text data.url ]
                      )
                    , ( gettext "Label" props.locale
                      , viewContent contentType [ text data.label ]
                      )
                    ]

                CrossReference data ->
                    let
                        questionTitle =
                            CrossReference.toLabel (KnowledgeModel.getAllQuestions km) data
                    in
                    [ ( gettext "Question" props.locale
                      , wrapMaybeValue (String.toMaybe questionTitle) <|
                            viewContent contentType [ text questionTitle ]
                      )
                    ]
    in
    ( gettext "Type" props.locale
    , viewContent contentType [ text (Reference.getTypeReadableString props.locale reference) ]
    )
        :: referenceTypeFields


referenceContentDiffFields : SidePanelProps msg -> Reference -> Reference -> List ( String, Html msg )
referenceContentDiffFields props leftReference rightReference =
    let
        referenceTypeFields =
            case rightReference of
                ResourcePageReference rightData ->
                    let
                        leftResourcePageLabel =
                            case leftReference of
                                ResourcePageReference leftData ->
                                    ResourcePageReference.toLabel (KnowledgeModel.getAllResourcePages props.leftKm) leftData

                                _ ->
                                    ""

                        rightResourcePageLabel =
                            ResourcePageReference.toLabel (KnowledgeModel.getAllResourcePages props.rightKm) rightData
                    in
                    [ ( gettext "Resource Page" props.locale
                      , renderTextDiff leftResourcePageLabel rightResourcePageLabel
                      )
                    ]

                URLReference rightData ->
                    let
                        leftUrl =
                            case leftReference of
                                URLReference leftData ->
                                    leftData.url

                                _ ->
                                    ""

                        rightUrl =
                            rightData.url

                        leftLabel =
                            case leftReference of
                                URLReference leftData ->
                                    leftData.label

                                _ ->
                                    ""

                        rightLabel =
                            rightData.label
                    in
                    [ ( gettext "URL" props.locale
                      , renderTextDiff leftUrl rightUrl
                      )
                    , ( gettext "Label" props.locale
                      , renderTextDiff leftLabel rightLabel
                      )
                    ]

                CrossReference rightData ->
                    let
                        leftQuestionTitle =
                            case leftReference of
                                CrossReference leftData ->
                                    CrossReference.toLabel (KnowledgeModel.getAllQuestions props.leftKm) leftData

                                _ ->
                                    ""

                        rightQuestionTitle =
                            CrossReference.toLabel (KnowledgeModel.getAllQuestions props.rightKm) rightData
                    in
                    [ ( gettext "Question" props.locale
                      , renderTextDiff leftQuestionTitle rightQuestionTitle
                      )
                    ]
    in
    ( gettext "Type" props.locale
    , renderTextDiff (Reference.getTypeString leftReference) (Reference.getTypeString rightReference)
    )
        :: referenceTypeFields



-- Expert


expertContentFields : SidePanelProps msg -> Expert -> ContentType -> List ( String, Html msg )
expertContentFields props expert contentType =
    [ ( gettext "Name" props.locale
      , viewContent contentType [ text expert.name ]
      )
    , ( gettext "Email" props.locale
      , viewContent contentType [ text expert.email ]
      )
    ]


expertContentDiffFields : SidePanelProps msg -> Expert -> Expert -> List ( String, Html msg )
expertContentDiffFields props leftExpert rightExpert =
    [ ( gettext "Name" props.locale
      , renderTextDiff leftExpert.name rightExpert.name
      )
    , ( gettext "Email" props.locale
      , renderTextDiff leftExpert.email rightExpert.email
      )
    ]



-- Metric


metricContentFields : SidePanelProps msg -> Metric -> ContentType -> List ( String, Html msg )
metricContentFields props metric contentType =
    [ ( gettext "Title" props.locale
      , viewContent contentType [ text metric.title ]
      )
    , ( gettext "Abbreviation" props.locale
      , wrapMaybeValue metric.abbreviation <|
            viewContent contentType [ text (Maybe.withDefault "" metric.abbreviation) ]
      )
    , ( gettext "Description" props.locale
      , wrapMaybeValue metric.description <|
            viewContent contentType [ text (Maybe.withDefault "" metric.description) ]
      )
    ]


metricContentDiffFields : SidePanelProps msg -> Metric -> Metric -> List ( String, Html msg )
metricContentDiffFields props leftMetric rightMetric =
    [ ( gettext "Title" props.locale
      , renderTextDiff leftMetric.title rightMetric.title
      )
    , ( gettext "Abbreviation" props.locale
      , wrapMaybeValue (Maybe.or leftMetric.abbreviation rightMetric.abbreviation) <|
            renderMaybeTextDiff leftMetric.abbreviation rightMetric.abbreviation
      )
    , ( gettext "Description" props.locale
      , wrapMaybeValue (Maybe.or leftMetric.description rightMetric.description) <|
            renderMaybeTextDiff leftMetric.description rightMetric.description
      )
    ]



-- Phase


phaseContentFields : SidePanelProps msg -> Phase -> ContentType -> List ( String, Html msg )
phaseContentFields props phase contentType =
    [ ( gettext "Title" props.locale
      , viewContent contentType [ text phase.title ]
      )
    , ( gettext "Description" props.locale
      , wrapMaybeValue phase.description <|
            viewContent contentType [ text (Maybe.withDefault "" phase.description) ]
      )
    ]


phaseContentDiffFields : SidePanelProps msg -> Phase -> Phase -> List ( String, Html msg )
phaseContentDiffFields props leftPhase rightPhase =
    [ ( gettext "Title" props.locale
      , renderTextDiff leftPhase.title rightPhase.title
      )
    , ( gettext "Description" props.locale
      , wrapMaybeValue (Maybe.or leftPhase.description rightPhase.description) <|
            renderMaybeTextDiff leftPhase.description rightPhase.description
      )
    ]



-- Tag


tagContentFields : SidePanelProps msg -> Tag -> ContentType -> List ( String, Html msg )
tagContentFields props tag contentType =
    [ ( gettext "Name" props.locale
      , viewContent contentType [ text tag.name ]
      )
    , ( gettext "Description" props.locale
      , wrapMaybeValue tag.description <|
            viewContent contentType [ text (Maybe.withDefault "" tag.description) ]
      )
    , ( gettext "Color" props.locale
      , viewContent contentType [ text tag.color ]
      )
    ]


tagContentDiffFields : SidePanelProps msg -> Tag -> Tag -> List ( String, Html msg )
tagContentDiffFields props leftTag rightTag =
    [ ( gettext "Name" props.locale
      , renderTextDiff leftTag.name rightTag.name
      )
    , ( gettext "Description" props.locale
      , wrapMaybeValue (Maybe.or leftTag.description rightTag.description) <|
            renderMaybeTextDiff leftTag.description rightTag.description
      )
    , ( gettext "Color" props.locale
      , renderTextDiff leftTag.color rightTag.color
      )
    ]



-- Integration


integrationContentFields : SidePanelProps msg -> Integration -> ContentType -> List ( String, Html msg )
integrationContentFields props integration contentType =
    [ ( gettext "Type" props.locale
      , viewContent contentType [ text (Integration.getTypeReadableString props.locale integration) ]
      )
    , ( gettext "Name" props.locale
      , viewContent contentType [ text (Integration.getName integration) ]
      )
    ]


integrationContentDiffFields : SidePanelProps msg -> Integration -> Integration -> List ( String, Html msg )
integrationContentDiffFields props leftIntegration rightIntegration =
    [ ( gettext "Type" props.locale
      , renderTextDiff (Integration.getTypeReadableString props.locale leftIntegration) (Integration.getTypeReadableString props.locale rightIntegration)
      )
    , ( gettext "Name" props.locale
      , renderTextDiff (Integration.getName leftIntegration) (Integration.getName rightIntegration)
      )
    ]



-- Resource Collection


resourceCollectionContentFields : SidePanelProps msg -> ResourceCollection -> ContentType -> List ( String, Html msg )
resourceCollectionContentFields props resourceCollection contentType =
    [ ( gettext "Title" props.locale
      , viewContent contentType [ text resourceCollection.title ]
      )
    ]


resourceCollectionContentDiffFields : SidePanelProps msg -> ResourceCollection -> ResourceCollection -> List ( String, Html msg )
resourceCollectionContentDiffFields props leftResourceCollection rightResourceCollection =
    [ ( gettext "Title" props.locale
      , renderTextDiff leftResourceCollection.title rightResourceCollection.title
      )
    ]



-- Resource Page


resourcePageContentFields : SidePanelProps msg -> ResourcePage -> ContentType -> List ( String, Html msg )
resourcePageContentFields props resourcePage contentType =
    [ ( gettext "Title" props.locale
      , viewContent contentType [ text resourcePage.title ]
      )
    , ( gettext "Content" props.locale
      , viewContent contentType [ text resourcePage.content ]
      )
    ]


resourcePageContentDiffFields : SidePanelProps msg -> ResourcePage -> ResourcePage -> List ( String, Html msg )
resourcePageContentDiffFields props leftResourcePage rightResourcePage =
    [ ( gettext "Title" props.locale
      , renderTextDiff leftResourcePage.title rightResourcePage.title
      )
    , ( gettext "Content" props.locale
      , renderTextDiff leftResourcePage.content rightResourcePage.content
      )
    ]



-- Field helpers


wrapMaybeValue : Maybe a -> Html msg -> Html msg
wrapMaybeValue value htmlValue =
    case value of
        Just _ ->
            htmlValue

        Nothing ->
            emptyValue


viewContent : ContentType -> List (Html msg) -> Html msg
viewContent contentType =
    span [ class (contentTypeClass contentType) ]


viewContentList : ContentType -> List (Html msg) -> Html msg
viewContentList contentType =
    ul [ class ("ps-3 m-0 " ++ contentTypeClass contentType) ]


viewContentDiffRow : Bool -> String -> String -> Html msg
viewContentDiffRow isSame leftValue rightValue =
    if isSame then
        text rightValue

    else
        div []
            [ div [ class "del" ] [ text leftValue ]
            , div [ class "ins " ] [ text rightValue ]
            ]


contentTypeClass : ContentType -> String
contentTypeClass contentType =
    case contentType of
        AddedContentType ->
            "ins"

        RemovedContentType ->
            "del"

        DefaultContentType ->
            ""


renderTextDiff : String -> String -> Html msg
renderTextDiff leftText rightText =
    let
        splitString str =
            let
                parts =
                    String.split " " str

                lastIndex =
                    List.length parts - 1
            in
            List.indexedMap
                (\i word ->
                    if i == lastIndex then
                        word

                    else
                        word ++ " "
                )
                parts
    in
    renderDiff <|
        Diff.diff
            (splitString leftText)
            (splitString rightText)


renderMaybeTextDiff : Maybe String -> Maybe String -> Html msg
renderMaybeTextDiff leftText rightText =
    renderTextDiff
        (Maybe.withDefault "" leftText)
        (Maybe.withDefault "" rightText)


renderDiff : List (Diff.Change String) -> Html msg
renderDiff =
    span [] << List.map renderChange << mergeDiffs


mergeDiffs : List (Diff.Change String) -> List (Diff.Change String)
mergeDiffs list =
    let
        dropLast =
            List.reverse >> List.drop 1 >> List.reverse

        fold item changes =
            case ( List.last changes, item ) of
                ( Just (Diff.Added s1), Diff.Added s2 ) ->
                    dropLast changes ++ [ Diff.Added (s1 ++ s2) ]

                ( Just (Diff.Removed s1), Diff.Removed s2 ) ->
                    dropLast changes ++ [ Diff.Removed (s1 ++ s2) ]

                ( Just (Diff.NoChange s1), Diff.NoChange s2 ) ->
                    dropLast changes ++ [ Diff.NoChange (s1 ++ s2) ]

                _ ->
                    changes ++ [ item ]
    in
    List.foldl fold [] list


renderChange : Diff.Change String -> Html msg
renderChange change =
    case change of
        Diff.Added s ->
            ins [] [ text s ]

        Diff.Removed s ->
            del [] [ text s ]

        Diff.NoChange s ->
            text s


emptyValue : Html msg
emptyValue =
    fas "fa-minus text-lighter"
