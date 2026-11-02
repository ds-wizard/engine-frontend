module Common.Components.KMComparison.Detail exposing
    ( Config
    , kind
    , view
    )

{-| One entity of the comparison, opened in the panel: what it is, what happened
to it, and every field it has — with what changed marked inside the value.

A field is written once, for a side that exists alone and for two sides to
diff alike, and takes the `DiffResult` itself, because the four cases
differ only in how the value is drawn:

  - added — the value, as an `<ins>`;
  - removed — the value, as a `<del>`;
  - unchanged — the value;
  - changed — the old struck through and the new inserted, word by word for
    text and whole for anything else.

`<ins>` and `<del>` are announced as an insertion and a deletion, so nothing has
to be written to say so, and they carry the success and danger subtle roles for
the eye. A value read from a knowledge model — a phase's
title, a question's tags, an integration's name — is read from the side the
entity is on, which `sides` is for.

-}

import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModel.Integration as Integration
import Common.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Common.Api.Models.KnowledgeModel.Question.QuestionValueType as QuestionValueType
import Common.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference(..))
import Common.Api.Models.KnowledgeModel.Reference.CrossReferenceData as CrossReferenceData
import Common.Api.Models.KnowledgeModel.Reference.ResourcePageReferenceData as ResourcePageReferenceData
import Common.Components.DiffText as DiffText
import Common.Components.KMComparison.Differ as Differ exposing (DiffResult)
import Common.Components.KMComparison.Rows as Rows exposing (EntityDiff(..))
import Common.Utils.ByteUnits as ByteUnits
import Dict
import Gettext exposing (gettext)
import Html exposing (Html, del, div, ins, li, span, text, ul)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Ui.Badge as Badge
import Ui.Copy as Copy
import Ui.DescriptionList as DescriptionList
import Ui.Icon as Icon


type alias Config msg =
    { locale : Gettext.Locale
    , leftKm : KnowledgeModel
    , rightKm : KnowledgeModel
    , copy : Copy.Model
    , toCopyMsg : Copy.Msg -> msg
    }


type alias Field msg =
    ( String, Html msg )


{-| What the entity is called, which is also what the panel is called.
-}
kind : Config msg -> EntityDiff -> String
kind config entity =
    case entity of
        ChapterDiff _ ->
            gettext "Chapter" config.locale

        QuestionDiff _ ->
            gettext "Question" config.locale

        AnswerDiff _ ->
            gettext "Answer" config.locale

        ChoiceDiff _ ->
            gettext "Choice" config.locale

        ReferenceDiff _ ->
            gettext "Reference" config.locale

        ExpertDiff _ ->
            gettext "Expert" config.locale

        MetricDiff _ ->
            gettext "Metric" config.locale

        PhaseDiff _ ->
            gettext "Phase" config.locale

        TagDiff _ ->
            gettext "Tag" config.locale

        IntegrationDiff _ ->
            gettext "Integration" config.locale

        ResourceCollectionDiff _ ->
            gettext "Resource Collection" config.locale

        ResourcePageDiff _ ->
            gettext "Resource Page" config.locale


view : Config msg -> EntityDiff -> List (Html msg)
view config entity =
    [ div [ class "c-km-comparison-detail" ]
        [ div [ class "c-km-comparison-detail__state" ] [ stateBadge config entity ]
        , DescriptionList.toHtml
            (List.map (\( term, value ) -> DescriptionList.withHtml { term = term, description = [ value ] })
                (( gettext "UUID" config.locale, uuidValue config entity ) :: fields config entity)
            )
        ]
    ]


stateBadge : Config msg -> EntityDiff -> Html msg
stateBadge config entity =
    case state entity of
        Rows.Added ->
            Badge.toHtml (Badge.success (gettext "Added" config.locale))

        Rows.Removed ->
            Badge.toHtml (Badge.danger (gettext "Removed" config.locale))

        Rows.Changed ->
            Badge.toHtml (Badge.warning (gettext "Changed" config.locale))

        _ ->
            Badge.toHtml (Badge.neutral (gettext "No change" config.locale))


uuidValue : Config msg -> EntityDiff -> Html msg
uuidValue config entity =
    let
        uuid =
            Rows.entityUuid entity
    in
    Copy.inline
        { key = "km-comparison-uuid"
        , label = gettext "Copy the UUID" config.locale
        , value = uuid
        , model = config.copy
        , toMsg = config.toCopyMsg
        }


state : EntityDiff -> Rows.State
state entity =
    -- Every row of the fold carries its state; the panel opens one entity, so
    -- it works its own out from the same diff rather than being told twice.
    case entity of
        ChapterDiff diff ->
            stateOf diff

        QuestionDiff diff ->
            stateOf diff

        AnswerDiff diff ->
            stateOf diff

        ChoiceDiff diff ->
            stateOf diff

        ReferenceDiff diff ->
            stateOf diff

        ExpertDiff diff ->
            stateOf diff

        MetricDiff diff ->
            stateOf diff

        PhaseDiff diff ->
            stateOf diff

        TagDiff diff ->
            stateOf diff

        IntegrationDiff diff ->
            stateOf diff

        ResourceCollectionDiff diff ->
            stateOf diff

        ResourcePageDiff diff ->
            stateOf diff


stateOf : DiffResult a -> Rows.State
stateOf diff =
    case diff of
        Differ.Added _ ->
            Rows.Added

        Differ.Removed _ ->
            Rows.Removed

        Differ.Changed _ _ ->
            Rows.Changed

        Differ.NoChange _ _ ->
            Rows.Unchanged



-- THE FIELDS OF EACH ENTITY


fields : Config msg -> EntityDiff -> List (Field msg)
fields config entity =
    case entity of
        ChapterDiff diff ->
            [ textField config (gettext "Title" config.locale) (always .title) diff
            , maybeTextField config (gettext "Text" config.locale) (always .text) diff
            ]

        QuestionDiff diff ->
            questionFields config diff

        AnswerDiff diff ->
            [ textField config (gettext "Label" config.locale) (always .label) diff
            , maybeTextField config (gettext "Advice" config.locale) (always .advice) diff
            ]

        ChoiceDiff diff ->
            [ textField config (gettext "Label" config.locale) (always .label) diff ]

        ReferenceDiff diff ->
            referenceFields config diff

        ExpertDiff diff ->
            [ textField config (gettext "Name" config.locale) (always .name) diff
            , textField config (gettext "Email" config.locale) (always .email) diff
            ]

        MetricDiff diff ->
            [ textField config (gettext "Title" config.locale) (always .title) diff
            , maybeTextField config (gettext "Abbreviation" config.locale) (always .abbreviation) diff
            , maybeTextField config (gettext "Description" config.locale) (always .description) diff
            ]

        PhaseDiff diff ->
            [ textField config (gettext "Title" config.locale) (always .title) diff
            , maybeTextField config (gettext "Description" config.locale) (always .description) diff
            ]

        TagDiff diff ->
            [ textField config (gettext "Name" config.locale) (always .name) diff
            , maybeTextField config (gettext "Description" config.locale) (always .description) diff
            , valueField config (gettext "Color" config.locale) (always .color) diff
            ]

        IntegrationDiff diff ->
            [ valueField config (gettext "Type" config.locale) (always Integration.getTypeString) diff
            , textField config (gettext "Name" config.locale) (always Integration.getName) diff
            ]

        ResourceCollectionDiff diff ->
            [ textField config (gettext "Title" config.locale) (always .title) diff ]

        ResourcePageDiff diff ->
            [ textField config (gettext "Title" config.locale) (always .title) diff
            , textField config (gettext "Content" config.locale) (always .content) diff
            ]


questionFields : Config msg -> DiffResult Question -> List (Field msg)
questionFields config diff =
    let
        requiredPhase km question =
            Question.getRequiredPhaseUuid question
                |> Maybe.andThen (\phaseUuid -> KnowledgeModel.getPhase phaseUuid km)
                |> Maybe.map .title

        tags km question =
            KnowledgeModel.getQuestionTags (Question.getUuid question) km
                |> List.map (\tag -> ( tag.uuid, tag.name ))
    in
    [ valueField config (gettext "Type" config.locale) (always Question.getTypeString) diff
    , textField config (gettext "Title" config.locale) (always Question.getTitle) diff
    , maybeTextField config (gettext "Text" config.locale) (always Question.getText) diff
    , maybeValueField config (gettext "Required Phase" config.locale) requiredPhase diff
    , listField config (gettext "Tags" config.locale) tags diff
    ]
        ++ questionTypeFields config diff


{-| The fields only some kinds of question have. Which kind it is comes from the
side that is there — the right one for everything but a removal.
-}
questionTypeFields : Config msg -> DiffResult Question -> List (Field msg)
questionTypeFields config diff =
    let
        integrationName km question =
            Question.getIntegrationUuid question
                |> Maybe.andThen (\uuid -> KnowledgeModel.getIntegration uuid km)
                |> Maybe.map Integration.getName

        variables _ question =
            case question of
                IntegrationQuestion _ data ->
                    List.map (\( key, value ) -> ( key, key ++ ": " ++ value )) (Dict.toList data.variables)

                _ ->
                    []

        listQuestion km question =
            Question.getListQuestionUuid question
                |> Maybe.andThen (\uuid -> KnowledgeModel.getQuestion uuid km)
                |> Maybe.map Question.getTitle
    in
    case present diff of
        ValueQuestion _ _ ->
            [ maybeValueField config
                (gettext "Value Type" config.locale)
                (\_ question -> Maybe.map (QuestionValueType.toReadableString config.locale) (Question.getValueType question))
                diff
            ]

        IntegrationQuestion _ _ ->
            [ maybeValueField config (gettext "Integration" config.locale) integrationName diff
            , listField config (gettext "Variables" config.locale) variables diff
            ]

        ItemSelectQuestion _ _ ->
            [ maybeValueField config (gettext "List Question" config.locale) listQuestion diff ]

        FileQuestion _ _ ->
            [ maybeTextField config (gettext "File Types" config.locale) (\_ question -> Question.getFileTypes question) diff
            , maybeValueField config
                (gettext "Max Size" config.locale)
                (\_ question -> Maybe.map ByteUnits.toReadable (Question.getMaxSize question))
                diff
            ]

        _ ->
            []


referenceFields : Config msg -> DiffResult Reference -> List (Field msg)
referenceFields config diff =
    let
        resourcePageLabel km reference =
            case reference of
                ResourcePageReference data ->
                    Just (ResourcePageReferenceData.toLabel (KnowledgeModel.getAllResourcePages km) data)

                _ ->
                    Nothing

        crossQuestionLabel km reference =
            case reference of
                CrossReference data ->
                    Just (CrossReferenceData.toLabel (KnowledgeModel.getAllQuestions km) data)

                _ ->
                    Nothing

        urlField get =
            \_ reference ->
                case reference of
                    URLReference data ->
                        Just (get data)

                    _ ->
                        Nothing

        typeField =
            valueField config (gettext "Type" config.locale) (always Reference.getTypeString) diff
    in
    typeField
        :: (case present diff of
                ResourcePageReference _ ->
                    [ maybeValueField config (gettext "Resource Page" config.locale) resourcePageLabel diff ]

                URLReference _ ->
                    [ maybeTextField config (gettext "URL" config.locale) (urlField .url) diff
                    , maybeTextField config (gettext "Label" config.locale) (urlField .label) diff
                    ]

                CrossReference _ ->
                    [ maybeValueField config (gettext "Question" config.locale) crossQuestionLabel diff ]
           )



-- READING A FIELD FROM THE SIDE IT IS ON


{-| The side that is there: the right one for everything but a removal, which is
only on the left.
-}
present : DiffResult a -> a
present diff =
    case diff of
        Differ.Added item ->
            item

        Differ.Removed item ->
            item

        Differ.Changed _ item ->
            item

        Differ.NoChange _ item ->
            item


{-| One value per side, each read from its own knowledge model.
-}
sides : Config msg2 -> (KnowledgeModel -> a -> b) -> DiffResult a -> DiffResult b
sides config read diff =
    case diff of
        Differ.Added item ->
            Differ.Added (read config.rightKm item)

        Differ.Removed item ->
            Differ.Removed (read config.leftKm item)

        Differ.Changed left right ->
            Differ.Changed (read config.leftKm left) (read config.rightKm right)

        Differ.NoChange left right ->
            Differ.NoChange (read config.leftKm left) (read config.rightKm right)



-- DRAWING A FIELD


{-| Text: what changed inside it is marked word by word.
-}
textField : Config msg2 -> String -> (KnowledgeModel -> a -> String) -> DiffResult a -> Field msg
textField config label read diff =
    ( label, textValue (sides config read diff) )


maybeTextField : Config msg2 -> String -> (KnowledgeModel -> a -> Maybe String) -> DiffResult a -> Field msg
maybeTextField config label read diff =
    ( label, maybeValue config textValue (sides config read diff) )


{-| A value that is not prose — a type, a color, a phase's title. It changes as
a whole or not at all, so there is nothing to diff inside it.
-}
valueField : Config msg2 -> String -> (KnowledgeModel -> a -> String) -> DiffResult a -> Field msg
valueField config label read diff =
    ( label, wholeValue (sides config read diff) )


maybeValueField : Config msg2 -> String -> (KnowledgeModel -> a -> Maybe String) -> DiffResult a -> Field msg
maybeValueField config label read diff =
    ( label, maybeValue config wholeValue (sides config read diff) )


{-| A set of things, each of which may have been added, removed or renamed.
Identity is the key, so a renamed item is one item that changed rather than one
gone and one arrived.
-}
listField : Config msg2 -> String -> (KnowledgeModel -> a -> List ( String, String )) -> DiffResult a -> Field msg
listField config label read diff =
    let
        items =
            case sides config read diff of
                Differ.Added list ->
                    List.map (\( _, name ) -> ins [] [ text name ]) list

                Differ.Removed list ->
                    List.map (\( _, name ) -> del [] [ text name ]) list

                Differ.NoChange _ list ->
                    List.map (\( _, name ) -> text name) list

                Differ.Changed left right ->
                    Differ.createDiff Tuple.first (\a b -> Tuple.second a == Tuple.second b) left right
                        |> List.map
                            (\itemDiff ->
                                case itemDiff of
                                    Differ.Added ( _, name ) ->
                                        ins [] [ text name ]

                                    Differ.Removed ( _, name ) ->
                                        del [] [ text name ]

                                    Differ.NoChange ( _, name ) _ ->
                                        text name

                                    Differ.Changed ( _, before ) ( _, after ) ->
                                        DiffText.words before after
                            )
    in
    ( label
    , if List.isEmpty items then
        emptyValue config

      else
        ul [ class "c-km-comparison-detail__list" ] (List.map (\item -> li [] [ item ]) items)
    )


maybeValue : Config msg2 -> (DiffResult String -> Html msg) -> DiffResult (Maybe String) -> Html msg
maybeValue config draw diff =
    let
        anything =
            case diff of
                Differ.Added value ->
                    Maybe.isJust value

                Differ.Removed value ->
                    Maybe.isJust value

                Differ.Changed left right ->
                    Maybe.isJust left || Maybe.isJust right

                Differ.NoChange left right ->
                    Maybe.isJust left || Maybe.isJust right
    in
    if anything then
        draw (mapDiff (Maybe.withDefault "") diff)

    else
        emptyValue config


mapDiff : (a -> b) -> DiffResult a -> DiffResult b
mapDiff f diff =
    case diff of
        Differ.Added item ->
            Differ.Added (f item)

        Differ.Removed item ->
            Differ.Removed (f item)

        Differ.Changed left right ->
            Differ.Changed (f left) (f right)

        Differ.NoChange left right ->
            Differ.NoChange (f left) (f right)


textValue : DiffResult String -> Html msg
textValue diff =
    case diff of
        Differ.Added value ->
            ins [] [ text value ]

        Differ.Removed value ->
            del [] [ text value ]

        Differ.NoChange _ value ->
            text value

        Differ.Changed before after ->
            if before == after then
                text after

            else
                DiffText.words before after


wholeValue : DiffResult String -> Html msg
wholeValue diff =
    case diff of
        Differ.Added value ->
            ins [] [ text value ]

        Differ.Removed value ->
            del [] [ text value ]

        Differ.NoChange _ value ->
            text value

        Differ.Changed before after ->
            if before == after then
                text after

            else
                span []
                    [ del [] [ text before ]
                    , ins [] [ text after ]
                    ]


{-| A field the entity does not fill in. A dash, with a name, rather than an
empty row that reads as a missing value.
-}
emptyValue : Config msg2 -> Html msg
emptyValue config =
    span [ class "c-km-comparison-detail__empty" ]
        [ Icon.toHtml (Icon.labeled (gettext "Not set" config.locale) (Icon.solid "fa-minus")) ]
