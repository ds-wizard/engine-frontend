module Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer exposing (create, defaultResourcePageToRoute)

import Dict
import Dict.Extra as Dict
import Gettext exposing (gettext)
import Html exposing (Html, a, div, p, span, text)
import Html.Attributes exposing (class, href, target)
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Components.Badge as Badge
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Choice exposing (Choice)
import Shared.Data.KnowledgeModel.Expert exposing (Expert)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question)
import Shared.Data.KnowledgeModel.Reference exposing (Reference(..))
import Shared.Data.KnowledgeModel.Reference.ResourcePageReferenceData exposing (ResourcePageReferenceData)
import Shared.Data.KnowledgeModel.Reference.URLReferenceData exposing (URLReferenceData)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Markdown as Markdown
import Shared.Utils exposing (flip)
import String.Extra as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire exposing (QuestionnaireRenderer)
import Wizard.Common.Components.Questionnaire.QuestionnaireViewSettings exposing (QuestionnaireViewSettings)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Routes


create : AppState -> KnowledgeModel -> (String -> Wizard.Routes.Route) -> QuestionnaireRenderer msg
create appState km resourcePageToRoute =
    { renderQuestionLabel = renderQuestionLabel
    , renderQuestionDescription = renderQuestionDescription appState km resourcePageToRoute
    , getQuestionExtraClass = always Nothing
    , renderAnswerLabel = renderAnswerLabel
    , renderAnswerBadges = renderAnswerBadges (KnowledgeModel.getMetrics km)
    , renderAnswerAdvice = renderAnswerAdvice
    , renderChoiceLabel = renderChoiceLabel
    }


defaultResourcePageToRoute : String -> String -> Wizard.Routes.Route
defaultResourcePageToRoute packageId =
    Wizard.Routes.knowledgeModelsResourcePage packageId


renderQuestionLabel : Question -> Html msg
renderQuestionLabel question =
    text <| Question.getTitle question


renderQuestionDescription : AppState -> KnowledgeModel -> (String -> Wizard.Routes.Route) -> QuestionnaireViewSettings -> Question -> Html msg
renderQuestionDescription appState km resourcePageToRoute qvs question =
    let
        description =
            Question.getText question
                |> Maybe.map (\t -> p [ class "form-text text-muted" ] [ Markdown.toHtml [] t ])
                |> Maybe.withDefault (text "")

        phases =
            KnowledgeModel.getPhases km

        extraData =
            viewExtraData appState qvs km resourcePageToRoute phases <| createQuestionExtraData km question
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
                emptyNode

        createBadge metricMeasure =
            Badge.badge
                [ class (getBadgeClass metricMeasure.measure) ]
                [ text <| getMetricName metricMeasure.metricUuid
                , metricValue metricMeasure
                ]
    in
    if List.isEmpty answer.metricMeasures then
        emptyNode

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
            emptyNode


renderChoiceLabel : Choice -> Html msg
renderChoiceLabel choice =
    text choice.label



-- Question Extra data


type alias FormExtraData =
    { resourcePageReferences : List ResourcePageReferenceData
    , urlReferences : List URLReferenceData
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

                _ ->
                    extraData

        newExtraData =
            { resourcePageReferences = []
            , urlReferences = []
            , experts = KnowledgeModel.getQuestionExperts (Question.getUuid question) km
            , requiredPhaseUuid = Question.getRequiredPhaseUuid question
            }
    in
    KnowledgeModel.getQuestionReferences (Question.getUuid question) km
        |> List.foldl foldReferences newExtraData


viewExtraData : AppState -> QuestionnaireViewSettings -> KnowledgeModel -> (String -> Wizard.Routes.Route) -> List Phase -> FormExtraData -> Html msg
viewExtraData appState qvs km resourcePageToRoute phases data =
    let
        isEmpty =
            List.isEmpty data.resourcePageReferences
                && List.isEmpty data.urlReferences
                && List.isEmpty data.experts
                && Maybe.isNothing data.requiredPhaseUuid
    in
    if isEmpty then
        emptyNode

    else
        p [ class "extra-data" ]
            (viewRequiredLevel appState qvs phases data.requiredPhaseUuid
                :: viewResourcePageReferences appState km resourcePageToRoute data.resourcePageReferences
                ++ [ viewUrlReferences appState data.urlReferences
                   , viewExperts appState data.experts
                   ]
            )


viewRequiredLevel : AppState -> QuestionnaireViewSettings -> List Phase -> Maybe String -> Html msg
viewRequiredLevel appState qvs phases questionPhaseUuid =
    case ( qvs.phases, List.find (.uuid >> Just >> (==) questionPhaseUuid) phases ) of
        ( True, Just level ) ->
            span []
                [ span [ class "caption" ]
                    [ faSet "questionnaire.desirable" appState
                    , text (gettext "Desirable" appState.locale)
                    , text ": "
                    , span [] [ text level.title ]
                    ]
                ]

        _ ->
            emptyNode


type alias ViewExtraItemsConfig a msg =
    { icon : Html msg
    , label : String
    , viewItem : a -> Html msg
    }


viewExtraItems : ViewExtraItemsConfig a msg -> List a -> Html msg
viewExtraItems cfg list =
    if List.length list == 0 then
        emptyNode

    else
        let
            items =
                List.map cfg.viewItem list
                    |> List.intersperse (span [ class "separator" ] [ text ", " ])
        in
        span []
            (span [ class "caption" ] [ cfg.icon, text (cfg.label ++ ": ") ] :: items)


viewResourcePageReferences : AppState -> KnowledgeModel -> (String -> Wizard.Routes.Route) -> List ResourcePageReferenceData -> List (Html msg)
viewResourcePageReferences appState km resourcePageToRoute resourcePageReferences =
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
                            { icon = faSet "questionnaire.resourcePageReferences" appState
                            , label = rc.title
                            , viewItem = viewResourcePageReference appState km resourcePageToRoute
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


viewResourcePageReference : AppState -> KnowledgeModel -> (String -> Wizard.Routes.Route) -> ResourcePageReferenceData -> Html msg
viewResourcePageReference appState km resourcePageToRoute data =
    case Maybe.andThen (flip KnowledgeModel.getResourcePage km) data.resourcePageUuid of
        Just resourcePage ->
            linkTo appState
                (resourcePageToRoute resourcePage.uuid)
                [ target "_blank" ]
                [ text resourcePage.title ]

        Nothing ->
            emptyNode


viewUrlReferences : AppState -> List URLReferenceData -> Html msg
viewUrlReferences appState =
    viewExtraItems
        { icon = faSet "questionnaire.urlReferences" appState
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


viewExperts : AppState -> List Expert -> Html msg
viewExperts appState =
    viewExtraItems
        { icon = faSet "questionnaire.experts" appState
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
