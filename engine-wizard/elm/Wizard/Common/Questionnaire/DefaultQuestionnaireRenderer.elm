module Wizard.Common.Questionnaire.DefaultQuestionnaireRenderer exposing
    ( defaultQuestionnaireRenderer
    , renderOptionAdvice
    , renderOptionBadges
    , renderOptionLabel
    , renderQuestionDescription
    , renderQuestionLabel
    )

import Html exposing (..)
import Html.Attributes exposing (class, href, target)
import List.Extra as List
import Markdown
import Maybe.Extra as Maybe
import Shared.Error.ApiError exposing (ApiError)
import Shared.Locale exposing (l, lg, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.FormEngine.View exposing (FormRenderer)
import Wizard.Common.Html exposing (emptyNode, faSet)
import Wizard.Common.Questionnaire.Msgs exposing (CustomFormMessage)
import Wizard.KMEditor.Common.KnowledgeModel.Answer exposing (Answer)
import Wizard.KMEditor.Common.KnowledgeModel.Expert exposing (Expert)
import Wizard.KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.KMEditor.Common.KnowledgeModel.Level exposing (Level)
import Wizard.KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import Wizard.KMEditor.Common.KnowledgeModel.Question as Question exposing (Question)
import Wizard.KMEditor.Common.KnowledgeModel.Reference exposing (Reference(..))
import Wizard.KMEditor.Common.KnowledgeModel.Reference.ResourcePageReferenceData exposing (ResourcePageReferenceData)
import Wizard.KMEditor.Common.KnowledgeModel.Reference.URLReferenceData exposing (URLReferenceData)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Questionnaire.DefaultQuestionnaireRenderer"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Questionnaire.DefaultQuestionnaireRenderer"


defaultQuestionnaireRenderer : AppState -> KnowledgeModel -> List Level -> List Metric -> FormRenderer CustomFormMessage Question Answer ApiError
defaultQuestionnaireRenderer appState km levels metrics =
    { renderQuestionLabel = renderQuestionLabel
    , renderQuestionDescription = renderQuestionDescription appState levels km
    , renderOptionLabel = renderOptionLabel
    , renderOptionBadges = renderOptionBadges metrics
    , renderOptionAdvice = renderOptionAdvice
    }


renderQuestionLabel : Question -> Html msg
renderQuestionLabel question =
    text <| Question.getTitle question


renderQuestionDescription : AppState -> List Level -> KnowledgeModel -> Question -> Html msg
renderQuestionDescription appState levels km question =
    let
        description =
            Question.getText question
                |> Maybe.map (\t -> p [ class "form-text text-muted" ] [ Markdown.toHtml [] t ])
                |> Maybe.withDefault (text "")

        extraData =
            viewExtraData appState levels <| createQuestionExtraData km question
    in
    div []
        [ description
        , extraData
        ]


renderOptionLabel : Answer -> Html msg
renderOptionLabel answer =
    text answer.label


renderOptionBadges : List Metric -> Answer -> Html msg
renderOptionBadges metrics answer =
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


renderOptionAdvice : Answer -> Html msg
renderOptionAdvice answer =
    case answer.advice of
        Just advice ->
            div [ class "alert alert-info" ] [ Markdown.toHtml [] advice ]

        _ ->
            emptyNode



-- Question Extra data


type alias FormExtraData =
    { resourcePageReferences : List ResourcePageReferenceData
    , urlReferences : List URLReferenceData
    , experts : List Expert
    , requiredLevel : Maybe Int
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
            , requiredLevel = Question.getRequiredLevel question
            }
    in
    KnowledgeModel.getQuestionReferences (Question.getUuid question) km
        |> List.foldl foldReferences newExtraData


viewExtraData : AppState -> List Level -> FormExtraData -> Html msg
viewExtraData appState levels data =
    let
        isEmpty =
            List.isEmpty data.resourcePageReferences
                && List.isEmpty data.urlReferences
                && List.isEmpty data.experts
                && Maybe.isNothing data.requiredLevel
    in
    if isEmpty then
        emptyNode

    else
        p [ class "extra-data" ]
            [ viewRequiredLevel appState levels data.requiredLevel
            , viewResourcePageReferences appState data.resourcePageReferences
            , viewUrlReferences appState data.urlReferences
            , viewExperts appState data.experts
            ]


viewRequiredLevel : AppState -> List Level -> Maybe Int -> Html msg
viewRequiredLevel appState levels questionLevel =
    case List.find (.level >> (==) (questionLevel |> Maybe.withDefault 0)) levels of
        Just level ->
            span []
                [ span [ class "caption" ]
                    [ faSet "questionnaire.desirable" appState
                    , lx_ "desirable" appState
                    , span [] [ text level.title ]
                    ]
                ]

        Nothing ->
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
