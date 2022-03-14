module Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer exposing (create)

import Html exposing (Html, a, div, p, span, text)
import Html.Attributes exposing (class, href, target)
import List.Extra as List
import Maybe.Extra as Maybe
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
import Shared.Locale exposing (l, lg, lx)
import Shared.Markdown as Markdown
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire exposing (QuestionnaireRenderer)
import Wizard.Common.Components.Questionnaire.QuestionnaireViewSettings exposing (QuestionnaireViewSettings)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer"


create : AppState -> KnowledgeModel -> QuestionnaireRenderer msg
create appState km =
    { renderQuestionLabel = renderQuestionLabel
    , renderQuestionDescription = renderQuestionDescription appState km
    , getQuestionExtraClass = always Nothing
    , renderAnswerLabel = renderAnswerLabel
    , renderAnswerBadges = renderAnswerBadges (KnowledgeModel.getMetrics km)
    , renderAnswerAdvice = renderAnswerAdvice
    , renderChoiceLabel = renderChoiceLabel
    }


renderQuestionLabel : Question -> Html msg
renderQuestionLabel question =
    text <| Question.getTitle question


renderQuestionDescription : AppState -> KnowledgeModel -> QuestionnaireViewSettings -> Question -> Html msg
renderQuestionDescription appState km qvs question =
    let
        description =
            Question.getText question
                |> Maybe.map (\t -> p [ class "form-text text-muted" ] [ Markdown.toHtml [] t ])
                |> Maybe.withDefault (text "")

        phases =
            KnowledgeModel.getPhases km

        extraData =
            viewExtraData appState qvs phases <| createQuestionExtraData km question
    in
    div []
        [ description
        , extraData
        ]


renderAnswerLabel : Answer -> Html msg
renderAnswerLabel answer =
    text answer.label


renderAnswerBadges : List Metric -> Answer -> Html msg
renderAnswerBadges metrics answer =
    let
        getMetricName uuid =
            List.find ((==) uuid << .uuid) metrics
                |> Maybe.map .title
                |> Maybe.withDefault "Unknown"

        getBadgeClass value =
            (++) "badge-value-" <| String.fromInt <| (*) 10 <| round <| value * 10

        metricExists measure =
            List.find ((==) measure.metricUuid << .uuid) metrics /= Nothing

        createBadge metricMeasure =
            span [ class <| "badge " ++ getBadgeClass metricMeasure.measure ]
                [ text <| getMetricName metricMeasure.metricUuid ]
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


viewExtraData : AppState -> QuestionnaireViewSettings -> List Phase -> FormExtraData -> Html msg
viewExtraData appState qvs phases data =
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
            [ viewRequiredLevel appState qvs phases data.requiredPhaseUuid
            , viewResourcePageReferences appState data.resourcePageReferences
            , viewUrlReferences appState data.urlReferences
            , viewExperts appState data.experts
            ]


viewRequiredLevel : AppState -> QuestionnaireViewSettings -> List Phase -> Maybe String -> Html msg
viewRequiredLevel appState qvs phases questionPhaseUuid =
    case ( qvs.phases, List.find (.uuid >> Just >> (==) questionPhaseUuid) phases ) of
        ( True, Just level ) ->
            span []
                [ span [ class "caption" ]
                    [ faSet "questionnaire.desirable" appState
                    , lx_ "desirable" appState
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


viewResourcePageReferences : AppState -> List ResourcePageReferenceData -> Html msg
viewResourcePageReferences appState =
    viewExtraItems
        { icon = faSet "questionnaire.resourcePageReferences" appState
        , label = "Data Stewardship for Open Science"
        , viewItem = viewResourcePageReference
        }


viewResourcePageReference : ResourcePageReferenceData -> Html msg
viewResourcePageReference data =
    a [ href <| "/book-references/" ++ data.shortUuid, target "_blank" ]
        [ text data.shortUuid ]


viewUrlReferences : AppState -> List URLReferenceData -> Html msg
viewUrlReferences appState =
    viewExtraItems
        { icon = faSet "questionnaire.urlReferences" appState
        , label = l_ "externalLinks" appState
        , viewItem = viewUrlReference
        }


viewUrlReference : URLReferenceData -> Html msg
viewUrlReference data =
    a [ href data.url, target "_blank" ]
        [ text data.label ]


viewExperts : AppState -> List Expert -> Html msg
viewExperts appState =
    viewExtraItems
        { icon = faSet "questionnaire.experts" appState
        , label = lg "experts" appState
        , viewItem = viewExpert
        }


viewExpert : Expert -> Html msg
viewExpert expert =
    span []
        [ text expert.name
        , text " ("
        , a [ href <| "mailto:" ++ expert.email ] [ text expert.email ]
        , text ")"
        ]
