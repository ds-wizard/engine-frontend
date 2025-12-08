module Wizard.Components.Questionnaire.DefaultQuestionnaireRenderer exposing
    ( DefaultQuestionnaireRendererConfig
    , config
    , create
    , withKnowledgeModel
    , withResourcePageToRoute
    )

import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (faKmQuestion, faQuestionnaireDesirable, faQuestionnaireExperts, faQuestionnaireResourcePageReferences, faQuestionnaireUrlReferences)
import Common.Utils.Markdown as Markdown
import Dict
import Dict.Extra as Dict
import Flip exposing (flip)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, p, span, text)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import String.Extra as String
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Wizard.Api.Models.KnowledgeModel.Choice exposing (Choice)
import Wizard.Api.Models.KnowledgeModel.Expert exposing (Expert)
import Wizard.Api.Models.KnowledgeModel.Metric exposing (Metric)
import Wizard.Api.Models.KnowledgeModel.Phase exposing (Phase)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.KnowledgeModel.Reference exposing (Reference(..))
import Wizard.Api.Models.KnowledgeModel.Reference.CrossReferenceData exposing (CrossReferenceData)
import Wizard.Api.Models.KnowledgeModel.Reference.ResourcePageReferenceData exposing (ResourcePageReferenceData)
import Wizard.Api.Models.KnowledgeModel.Reference.URLReferenceData exposing (URLReferenceData)
import Wizard.Api.Models.ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.Questionnaire as Questionnaire exposing (QuestionnaireRenderer)
import Wizard.Components.Questionnaire.QuestionnaireViewSettings exposing (QuestionnaireViewSettings)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Routes


type DefaultQuestionnaireRendererConfig
    = DefaultQuestionnaireRendererConfig DefaultQuestionnaireRendererConfigData


type alias DefaultQuestionnaireRendererConfigData =
    { knowledgeModel : KnowledgeModel
    , questionnaire : ProjectQuestionnaire
    , resourcePageToRoute : String -> Wizard.Routes.Route
    }


config : ProjectQuestionnaire -> DefaultQuestionnaireRendererConfig
config questionnaire =
    DefaultQuestionnaireRendererConfig
        { knowledgeModel = questionnaire.knowledgeModel
        , questionnaire = questionnaire
        , resourcePageToRoute = defaultResourcePageToRoute questionnaire.knowledgeModelPackageId
        }


withKnowledgeModel : KnowledgeModel -> DefaultQuestionnaireRendererConfig -> DefaultQuestionnaireRendererConfig
withKnowledgeModel km (DefaultQuestionnaireRendererConfig cfg) =
    DefaultQuestionnaireRendererConfig { cfg | knowledgeModel = km }


withResourcePageToRoute : (String -> Wizard.Routes.Route) -> DefaultQuestionnaireRendererConfig -> DefaultQuestionnaireRendererConfig
withResourcePageToRoute f (DefaultQuestionnaireRendererConfig cfg) =
    DefaultQuestionnaireRendererConfig { cfg | resourcePageToRoute = f }


create : AppState -> DefaultQuestionnaireRendererConfig -> QuestionnaireRenderer
create appState (DefaultQuestionnaireRendererConfig cfg) =
    { renderQuestionLabel = renderQuestionLabel
    , renderQuestionDescription = renderQuestionDescription appState cfg
    , getQuestionExtraClass = always Nothing
    , renderAnswerLabel = renderAnswerLabel
    , renderAnswerBadges = renderAnswerBadges (KnowledgeModel.getMetrics cfg.knowledgeModel)
    , renderAnswerAdvice = renderAnswerAdvice
    , renderChoiceLabel = renderChoiceLabel
    }


defaultResourcePageToRoute : String -> String -> Wizard.Routes.Route
defaultResourcePageToRoute kmPackageId =
    Wizard.Routes.knowledgeModelsResourcePage kmPackageId


renderQuestionLabel : Question -> Html msg
renderQuestionLabel question =
    text <| Question.getTitle question


renderQuestionDescription : AppState -> DefaultQuestionnaireRendererConfigData -> QuestionnaireViewSettings -> Question -> Html Questionnaire.Msg
renderQuestionDescription appState cfg qvs question =
    let
        description =
            Question.getText question
                |> Maybe.map (\t -> p [ class "form-text text-muted" ] [ Markdown.toHtml [] t ])
                |> Maybe.withDefault (text "")

        extraData =
            viewExtraData appState cfg qvs <| createQuestionExtraData cfg.knowledgeModel question
    in
    div [ class "description" ]
        [ description
        , extraData
        ]


renderAnswerLabel : Answer -> Html msg
renderAnswerLabel answer =
    text answer.label


renderAnswerBadges : List Metric -> Bool -> Answer -> Html msg
renderAnswerBadges metrics viewValue answer =
    let
        getMetricName uuid =
            List.find ((==) uuid << .uuid) metrics
                |> Maybe.map .title
                |> Maybe.withDefault "Unknown"

        getBadgeClass value =
            (++) "bg-value-" <| String.fromInt <| (*) 10 <| round <| value * 10

        metricExists measure =
            List.find ((==) measure.metricUuid << .uuid) metrics /= Nothing

        metricValue metricMeasure =
            if viewValue then
                span [ class "ms-1" ] [ text (String.fromInt (round (100 * metricMeasure.measure)) ++ "%") ]

            else
                Html.nothing

        createBadge metricMeasure =
            Badge.badge
                [ class (getBadgeClass metricMeasure.measure) ]
                [ text <| getMetricName metricMeasure.metricUuid
                , metricValue metricMeasure
                ]
    in
    if List.isEmpty answer.metricMeasures then
        Html.nothing

    else
        div [ class "badges" ]
            (List.filter metricExists answer.metricMeasures
                |> List.map createBadge
            )


renderAnswerAdvice : Answer -> Html msg
renderAnswerAdvice answer =
    case answer.advice of
        Just advice ->
            Markdown.toHtml [ class "alert alert-info" ] advice

        _ ->
            Html.nothing


renderChoiceLabel : Choice -> Html msg
renderChoiceLabel choice =
    text choice.label



-- Question Extra data


type alias FormExtraData =
    { resourcePageReferences : List ResourcePageReferenceData
    , urlReferences : List URLReferenceData
    , crossReferences : List CrossReferenceData
    , experts : List Expert
    , requiredPhaseUuid : Maybe String
    }


createQuestionExtraData : KnowledgeModel -> Question -> FormExtraData
createQuestionExtraData km question =
    let
        foldReferences reference extraData =
            case reference of
                ResourcePageReference data ->
                    { extraData | resourcePageReferences = extraData.resourcePageReferences ++ [ data ] }

                URLReference data ->
                    { extraData | urlReferences = extraData.urlReferences ++ [ data ] }

                CrossReference data ->
                    { extraData | crossReferences = extraData.crossReferences ++ [ data ] }

        newExtraData =
            { resourcePageReferences = []
            , urlReferences = []
            , crossReferences = []
            , experts = KnowledgeModel.getQuestionExperts (Question.getUuid question) km
            , requiredPhaseUuid = Question.getRequiredPhaseUuid question
            }
    in
    KnowledgeModel.getQuestionReferences (Question.getUuid question) km
        |> List.foldl foldReferences newExtraData


viewExtraData : AppState -> DefaultQuestionnaireRendererConfigData -> QuestionnaireViewSettings -> FormExtraData -> Html Questionnaire.Msg
viewExtraData appState cfg qvs data =
    let
        isEmpty =
            List.isEmpty data.resourcePageReferences
                && List.isEmpty data.urlReferences
                && List.isEmpty data.crossReferences
                && List.isEmpty data.experts
                && Maybe.isNothing data.requiredPhaseUuid
    in
    if isEmpty then
        Html.nothing

    else
        let
            phases =
                KnowledgeModel.getPhases cfg.knowledgeModel
        in
        p [ class "extra-data" ]
            (viewRequiredLevel appState qvs phases data.requiredPhaseUuid
                :: viewResourcePageReferences cfg.knowledgeModel cfg.resourcePageToRoute data.resourcePageReferences
                ++ [ viewUrlReferences appState data.urlReferences
                   , viewCrossReferences appState cfg data.crossReferences
                   , viewExperts appState data.experts
                   ]
            )


viewRequiredLevel : AppState -> QuestionnaireViewSettings -> List Phase -> Maybe String -> Html msg
viewRequiredLevel appState qvs phases questionPhaseUuid =
    case ( qvs.phases, List.find (.uuid >> Just >> (==) questionPhaseUuid) phases ) of
        ( True, Just level ) ->
            span []
                [ span [ class "caption" ]
                    [ faQuestionnaireDesirable
                    , text (gettext "Desirable" appState.locale)
                    , text ": "
                    , span [] [ text level.title ]
                    ]
                ]

        _ ->
            Html.nothing


type alias ViewExtraItemsConfig a msg =
    { icon : Html msg
    , label : String
    , viewItem : a -> Html msg
    }


viewExtraItems : ViewExtraItemsConfig a msg -> List a -> Html msg
viewExtraItems cfg list =
    if List.isEmpty list then
        Html.nothing

    else
        let
            items =
                List.map cfg.viewItem list
                    |> List.intersperse (span [ class "separator" ] [ text ", " ])
        in
        span []
            (span [ class "caption" ] [ cfg.icon, text (cfg.label ++ ": ") ] :: items)


viewResourcePageReferences : KnowledgeModel -> (String -> Wizard.Routes.Route) -> List ResourcePageReferenceData -> List (Html msg)
viewResourcePageReferences km resourcePageToRoute resourcePageReferences =
    let
        resources =
            Dict.filterGroupBy
                (Maybe.andThen (flip KnowledgeModel.getResourceCollectionUuidByResourcePageUuid km) << .resourcePageUuid)
                resourcePageReferences

        viewResourceCollection ( resourceCollectionUuid, collectionResourcePageReferences ) =
            let
                resourceCollection =
                    KnowledgeModel.getResourceCollection resourceCollectionUuid km
            in
            case resourceCollection of
                Just rc ->
                    Just <|
                        ( rc.title
                        , viewExtraItems
                            { icon = faQuestionnaireResourcePageReferences
                            , label = rc.title
                            , viewItem = viewResourcePageReference km resourcePageToRoute
                            }
                            collectionResourcePageReferences
                        )

                Nothing ->
                    Nothing
    in
    Dict.toList resources
        |> List.filterMap viewResourceCollection
        |> List.sortBy Tuple.first
        |> List.map Tuple.second


viewResourcePageReference : KnowledgeModel -> (String -> Wizard.Routes.Route) -> ResourcePageReferenceData -> Html msg
viewResourcePageReference km resourcePageToRoute data =
    case Maybe.andThen (flip KnowledgeModel.getResourcePage km) data.resourcePageUuid of
        Just resourcePage ->
            linkTo (resourcePageToRoute resourcePage.uuid)
                [ target "_blank" ]
                [ text resourcePage.title ]

        Nothing ->
            Html.nothing


viewUrlReferences : AppState -> List URLReferenceData -> Html msg
viewUrlReferences appState =
    viewExtraItems
        { icon = faQuestionnaireUrlReferences
        , label = gettext "External links" appState.locale
        , viewItem = viewUrlReference
        }


viewUrlReference : URLReferenceData -> Html msg
viewUrlReference data =
    let
        urlLabel =
            String.withDefault data.url data.label
    in
    a [ href data.url, target "_blank" ]
        [ text urlLabel ]


viewCrossReferences : AppState -> DefaultQuestionnaireRendererConfigData -> List CrossReferenceData -> Html Questionnaire.Msg
viewCrossReferences appState cfg crossReferences =
    viewExtraItems
        { icon = faKmQuestion
        , label = gettext "Related questions" appState.locale
        , viewItem = viewCrossReference cfg.knowledgeModel
        }
        crossReferences


viewCrossReference : KnowledgeModel -> CrossReferenceData -> Html Questionnaire.Msg
viewCrossReference km data =
    case KnowledgeModel.getQuestion data.targetUuid km of
        Just question ->
            span []
                [ a [ onClick (Questionnaire.ScrollToQuestion data.targetUuid) ]
                    [ text (Question.getTitle question) ]
                , Html.viewIf (not (String.isEmpty data.description)) <|
                    text (" (" ++ data.description ++ ")")
                ]

        Nothing ->
            Html.nothing


viewExperts : AppState -> List Expert -> Html msg
viewExperts appState =
    viewExtraItems
        { icon = faQuestionnaireExperts
        , label = gettext "Experts" appState.locale
        , viewItem = viewExpert
        }


viewExpert : Expert -> Html msg
viewExpert expert =
    if String.isEmpty expert.name then
        span []
            [ a [ href <| "mailto:" ++ expert.email ] [ text expert.email ]
            ]

    else
        span []
            [ text expert.name
            , text " ("
            , a [ href <| "mailto:" ++ expert.email ] [ text expert.email ]
            , text ")"
            ]
