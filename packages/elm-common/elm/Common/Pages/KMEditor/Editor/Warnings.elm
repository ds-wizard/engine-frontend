module Common.Pages.KMEditor.Editor.Warnings exposing (Config, Warning, compute)

{-| Everything in the knowledge model that is not finished: an empty title, an
options question with no answers, an integration whose request URL has no
`{{ q }}` in it.

They are computed over the **filtered** knowledge model, so a deleted entity
raises nothing, and each one names the entity it belongs to so the panel can
take you there.

The messages are worded as translators already have them.

-}

import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Common.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Common.Api.Models.KnowledgeModel.Choice exposing (Choice)
import Common.Api.Models.KnowledgeModel.Expert exposing (Expert)
import Common.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration(..))
import Common.Api.Models.KnowledgeModel.Integration.ApiIntegrationData as ApiIntegrationData
import Common.Api.Models.KnowledgeModel.Metric exposing (Metric)
import Common.Api.Models.KnowledgeModel.Phase exposing (Phase)
import Common.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Common.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference(..))
import Common.Api.Models.KnowledgeModel.ResourceCollection exposing (ResourceCollection)
import Common.Api.Models.KnowledgeModel.ResourcePage exposing (ResourcePage)
import Common.Api.Models.KnowledgeModel.Tag exposing (Tag)
import Common.Api.Models.TypeHintTestResponse as TypeHintTestResponse
import Common.Data.BuiltInIntegration as BuiltInIntegration exposing (BuiltInIntegration)
import Common.Utils.JinjaUtils as JinjaUtils
import Common.Utils.RegexPatterns as RegexPatterns
import Dict
import Gettext exposing (gettext)
import List.Extra as List
import Maybe.Extra as Maybe
import Regex
import String.Format as String


type alias Warning =
    { editorUuid : String
    , message : String

    -- The field the warning is about, where it is about one — so following the
    -- warning can put the caret in what has to be changed rather than at the
    -- top of a form. A warning about the shape of an entity ("no answers for
    -- options question") is about no one field and names none.
    , field : Maybe String
    }


type alias Config =
    { locale : Gettext.Locale
    , builtIns : List BuiltInIntegration
    , secrets : List String
    , isDeleted : String -> Bool
    }


compute : Config -> KnowledgeModel -> List Warning
compute config km =
    List.concat
        [ List.concatMap (chapter config km) (KnowledgeModel.getChapters km)
        , List.concatMap (metric config) (KnowledgeModel.getMetrics km)
        , List.concatMap (phase config) (KnowledgeModel.getPhases km)
        , List.concatMap (tag config) (KnowledgeModel.getTags km)
        , List.concatMap (integration config) (KnowledgeModel.getIntegrations km)
        , List.concatMap (resourceCollection config km) (KnowledgeModel.getResourceCollections km)
        ]


warn : String -> String -> List Warning
warn editorUuid message =
    [ { editorUuid = editorUuid, message = message, field = Nothing } ]


{-| A warning about one field of the entity, named as the form names it.
-}
warnField : String -> String -> String -> List Warning
warnField field editorUuid message =
    [ { editorUuid = editorUuid, message = message, field = Just field } ]


whenEmpty : String -> String -> String -> String -> List Warning
whenEmpty field value editorUuid message =
    if String.isEmpty value then
        warnField field editorUuid message

    else
        []


chapter : Config -> KnowledgeModel -> Chapter -> List Warning
chapter config km value =
    whenEmpty "title" value.title value.uuid (gettext "Empty title for chapter" config.locale)
        ++ List.concatMap (question config km) (KnowledgeModel.getChapterQuestions value.uuid km)


question : Config -> KnowledgeModel -> Question -> List Warning
question config km value =
    let
        uuid =
            Question.getUuid value

        at field message =
            warnField field uuid message

        typeWarnings =
            case value of
                OptionsQuestion _ data ->
                    if List.isEmpty data.answerUuids then
                        at "answers-add" (gettext "No answers for options question" config.locale)

                    else
                        List.concatMap (answer config km) (KnowledgeModel.getQuestionAnswers uuid km)

                ListQuestion _ data ->
                    if List.isEmpty data.itemTemplateQuestionUuids then
                        at "itemQuestions-add" (gettext "No item questions for list question" config.locale)

                    else
                        List.concatMap (question config km) (KnowledgeModel.getQuestionItemTemplateQuestions uuid km)

                IntegrationQuestion _ data ->
                    -- Not chosen is "" in this editor and the nil UUID in
                    -- what the old one saved; a deleted one is not chosen either.
                    case Maybe.filter (not << config.isDeleted << Integration.getUuid) (KnowledgeModel.getIntegration data.integrationUuid km) of
                        Just questionIntegration ->
                            case List.filter (\name -> Maybe.isNothing (Dict.get name data.variables)) (Integration.getVariables questionIntegration) of
                                missing :: _ ->
                                    -- The first one that has not been filled
                                    -- in, which is where the work starts.
                                    at ("variable-" ++ missing) (gettext "Missing variables for integration question" config.locale)

                                [] ->
                                    []

                        Nothing ->
                            at "integration" (gettext "No integration selected for integration question" config.locale)

                MultiChoiceQuestion _ data ->
                    if List.isEmpty data.choiceUuids then
                        at "choices-add" (gettext "No choices for multi-choice question" config.locale)

                    else
                        List.concatMap (choice config) (KnowledgeModel.getQuestionChoices uuid km)

                ItemSelectQuestion _ data ->
                    if Maybe.unwrap True config.isDeleted data.listQuestionUuid then
                        at "listQuestion" (gettext "No list question selected for item select question" config.locale)

                    else
                        []

                _ ->
                    []
    in
    whenEmpty "title" (Question.getTitle value) uuid (gettext "Empty title for question" config.locale)
        ++ typeWarnings
        ++ List.concatMap (reference config) (KnowledgeModel.getQuestionReferences uuid km)
        ++ List.concatMap (expert config) (KnowledgeModel.getQuestionExperts uuid km)


answer : Config -> KnowledgeModel -> Answer -> List Warning
answer config km value =
    whenEmpty "label" value.label value.uuid (gettext "Empty label for answer" config.locale)
        ++ List.concatMap (question config km) (KnowledgeModel.getAnswerFollowupQuestions value.uuid km)


choice : Config -> Choice -> List Warning
choice config value =
    whenEmpty "label" value.label value.uuid (gettext "Empty label for choice" config.locale)


reference : Config -> Reference -> List Warning
reference config value =
    let
        uuid =
            Reference.getUuid value
    in
    case value of
        ResourcePageReference data ->
            if Maybe.isNothing data.resourcePageUuid then
                warnField "resourcePage" uuid (gettext "No resource page selected for resource page reference" config.locale)

            else
                []

        URLReference data ->
            if String.isEmpty data.url then
                warnField "url" uuid (gettext "Empty URL for URL reference" config.locale)

            else if not (Regex.contains RegexPatterns.url data.url) then
                warnField "url" uuid (gettext "Invalid URL for URL reference" config.locale)

            else
                []

        _ ->
            []


expert : Config -> Expert -> List Warning
expert config value =
    if String.isEmpty value.email then
        warnField "email" value.uuid (gettext "Empty email for expert" config.locale)

    else if not (Regex.contains RegexPatterns.email value.email) then
        warnField "email" value.uuid (gettext "Invalid email for expert" config.locale)

    else
        []


metric : Config -> Metric -> List Warning
metric config value =
    whenEmpty "title" value.title value.uuid (gettext "Empty title for metric" config.locale)


phase : Config -> Phase -> List Warning
phase config value =
    whenEmpty "title" value.title value.uuid (gettext "Empty title for phase" config.locale)


tag : Config -> Tag -> List Warning
tag config value =
    whenEmpty "name" value.name value.uuid (gettext "Empty name for tag" config.locale)


resourceCollection : Config -> KnowledgeModel -> ResourceCollection -> List Warning
resourceCollection config km value =
    whenEmpty "title" value.title value.uuid (gettext "Empty title for resource collection" config.locale)
        ++ List.concatMap (resourcePage config) (KnowledgeModel.getResourceCollectionResourcePages value.uuid km)


resourcePage : Config -> ResourcePage -> List Warning
resourcePage config value =
    whenEmpty "title" value.title value.uuid (gettext "Empty title for resource page" config.locale)



-- INTEGRATIONS
--
-- The longest of these by far: an API integration's request is a Jinja template
-- over the question's variables and the tenant's secrets, and most of what can
-- go wrong is a name that resolves to nothing.


integration : Config -> Integration -> List Warning
integration config value =
    let
        uuid =
            Integration.getUuid value

        error message =
            warn uuid message

        atRequestUrl message =
            warnField "requestUrl" uuid message

        at field message =
            warnField field uuid message

        nameWarning =
            whenEmpty "name" (Integration.getName value) uuid (gettext "Empty name for integration" config.locale)
    in
    case value of
        ApiIntegration data ->
            let
                variablesWarning =
                    case List.filter (\( _, name ) -> String.isEmpty name) (List.indexedMap Tuple.pair data.variables) of
                        ( index, _ ) :: _ ->
                            at ("variable-" ++ String.fromInt index)
                                (gettext "Empty variable name for integration" config.locale)

                        [] ->
                            []

                urlWarning =
                    if String.isEmpty data.requestUrl then
                        atRequestUrl (gettext "Empty request URL for integration" config.locale)

                    else
                        let
                            parsed =
                                JinjaUtils.parseJinja data.requestUrl

                            unknown =
                                ApiIntegrationData.getUnknownVariables parsed config.secrets data

                            missingQ =
                                if List.notMember "q" parsed.properties then
                                    atRequestUrl (gettext "Missing {{ q }} in request URL for integration" config.locale)

                                else
                                    []

                            unknownProperty =
                                if List.isEmpty unknown.properties then
                                    []

                                else
                                    atRequestUrl
                                        (String.format
                                            (gettext "Unknown Jinja property in request URL for integration: %s" config.locale)
                                            [ String.join ", " unknown.properties ]
                                        )
                        in
                        missingQ ++ unknownProperty

                unknownConfig =
                    ApiIntegrationData.getUnknownConfigReferences config.secrets data

                unknownVariableWarning =
                    if List.isEmpty unknownConfig.variables then
                        []

                    else
                        error
                            (String.format
                                (gettext "Unknown variable in request configuration for integration: %s" config.locale)
                                [ String.join ", " unknownConfig.variables ]
                            )

                secretWarning =
                    if List.isEmpty unknownConfig.secrets then
                        []

                    else
                        error
                            (String.format
                                (gettext "Unknown secret in request configuration for integration: %s" config.locale)
                                [ String.join ", " unknownConfig.secrets ]
                            )

                ( testDataLoaded, testDataWarning ) =
                    case data.testResponse of
                        Nothing ->
                            ( False, at "testQ" (gettext "No test data loaded for integration" config.locale) )

                        Just testData ->
                            case testData.response of
                                TypeHintTestResponse.SuccessTypeHintResponse response ->
                                    ( True
                                    , case Maybe.map (\field -> TypeHintTestResponse.checkListField field response) data.responseListField of
                                        Just TypeHintTestResponse.CheckListFieldResultNotFound ->
                                            at "responseListField" (gettext "Response list field not found in test data for integration" config.locale)

                                        Just TypeHintTestResponse.CheckListFieldResultNoList ->
                                            at "responseListField" (gettext "Response list field is not a list in test data for integration" config.locale)

                                        _ ->
                                            []
                                    )

                                _ ->
                                    ( False, at "testQ" (gettext "No test data success response for integration" config.locale) )

                itemTemplateWarning =
                    if testDataLoaded && String.isEmpty data.responseItemTemplate then
                        at "responseItemTemplate" (gettext "Empty response item template for integration" config.locale)

                    else
                        []
            in
            nameWarning
                ++ variablesWarning
                ++ urlWarning
                ++ unknownVariableWarning
                ++ secretWarning
                ++ testDataWarning
                ++ itemTemplateWarning

        PluginIntegration data ->
            case BuiltInIntegration.find config.builtIns data of
                Just builtIn ->
                    nameWarning
                        ++ Maybe.unwrap [] (at "plugin") (builtIn.warning data.pluginIntegrationSettings)

                Nothing ->
                    nameWarning
                        ++ (if String.isEmpty data.pluginUuid || String.isEmpty data.pluginIntegrationId then
                                at "plugin" (gettext "No plugin selected for plugin integration" config.locale)

                            else
                                []
                           )
