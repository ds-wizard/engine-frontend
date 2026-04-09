module Wizard.Components.Questionnaire2.Components.PhaseSelection exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , update
    , viewPhaseModal
    , viewPhaseSelection
    )

import Common.Components.FontAwesome exposing (faClose)
import Common.Components.Modal as Modal
import Gettext exposing (gettext)
import Html exposing (Html, button, div, h5, label, small)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Html.Lazy as Lazy
import List.Extra as List
import Maybe.Extra as Maybe
import Svg exposing (text)
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModel.Phase exposing (Phase)
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Data.AppState exposing (AppState)


type alias Model =
    { phaseModalOpen : Bool }


init : Model
init =
    { phaseModalOpen = False }


type Msg
    = PhaseModalSetOpen Bool
    | PhaseModalSetPhaseUpdate (Maybe Uuid)


type alias UpdateConfig msg =
    { setPhaseCmd : Maybe Uuid -> Cmd msg
    }


update : UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update cfg msg model =
    case msg of
        PhaseModalSetOpen isOpen ->
            ( { model | phaseModalOpen = isOpen }
            , Cmd.none
            )

        PhaseModalSetPhaseUpdate mbPhaseUuid ->
            ( { model | phaseModalOpen = False }
            , cfg.setPhaseCmd mbPhaseUuid
            )


viewPhaseSelection : AppState -> ProjectQuestionnaire -> Bool -> Html Msg
viewPhaseSelection appState questionnaire readonly =
    Lazy.lazy5 viewPhaseSelectionLazy
        appState.locale
        questionnaire.knowledgeModel
        readonly
        questionnaire.phaseUuid
        (ProjectQuestionnaire.getCurrentPhaseIndex questionnaire)


viewPhaseSelectionLazy : Gettext.Locale -> KnowledgeModel -> Bool -> Maybe Uuid -> Int -> Html Msg
viewPhaseSelectionLazy locale knowledgeModel readonly mbPhaseUuid currentPhaseIndex =
    let
        phases =
            KnowledgeModel.getPhases knowledgeModel

        selectedPhaseTitle =
            List.find ((==) (Maybe.map Uuid.toString mbPhaseUuid) << Just << .uuid) phases
                |> Maybe.orElse (List.head phases)
                |> Maybe.unwrap "" .title

        phaseButtonOnClick =
            if readonly then
                []

            else
                [ onClick (PhaseModalSetOpen True) ]

        phaseButton =
            button
                ([ class "btn btn-input w-100"
                 , dataCy "phase-selection"
                 , disabled readonly
                 ]
                    ++ phaseButtonOnClick
                )
                [ text selectedPhaseTitle ]

        progress =
            toFloat currentPhaseIndex / toFloat (List.length phases - 1)

        phaseProgressPoint i _ =
            div
                [ class "questionnairePhaseProgress__point"
                , classList
                    [ ( "questionnairePhaseProgress__point--active", i <= currentPhaseIndex )
                    , ( "questionnairePhaseProgress__point--current", i == currentPhaseIndex )
                    ]
                ]
                []

        phaseProgressPoints =
            List.indexedMap phaseProgressPoint phases

        phaseProgress =
            div (class "questionnairePhaseProgress" :: phaseButtonOnClick)
                (div [ class "questionnairePhaseProgress__bar" ]
                    [ div
                        [ class "questionnairePhaseProgress__fill"
                        , style "width" (String.fromFloat (progress * 100) ++ "%")
                        ]
                        []
                    ]
                    :: phaseProgressPoints
                )
    in
    Html.viewIf (not (List.isEmpty phases)) <|
        div [ class "bg-light rounded p-3 m-3" ]
            [ label [] [ text (gettext "Current phase" locale) ]
            , phaseButton
            , phaseProgress
            ]


viewPhaseModal : AppState -> ProjectQuestionnaire -> Model -> Html Msg
viewPhaseModal appState questionnaire model =
    Lazy.lazy4 viewPhaseModalLazy
        appState.locale
        questionnaire.knowledgeModel
        (ProjectQuestionnaire.getCurrentPhaseIndex questionnaire)
        model.phaseModalOpen


viewPhaseModalLazy : Gettext.Locale -> KnowledgeModel -> Int -> Bool -> Html Msg
viewPhaseModalLazy locale knowledgeModel currentPhaseIndex phaseModalOpen =
    let
        phases =
            KnowledgeModel.getPhases knowledgeModel

        viewPhase : Int -> Phase -> Html Msg
        viewPhase index phase =
            let
                descriptionElement =
                    case phase.description of
                        Just description ->
                            small [ class "d-block text-secondary mt-1" ] [ text description ]

                        Nothing ->
                            Html.nothing

                clickAttribute =
                    if index == currentPhaseIndex then
                        []

                    else
                        [ onClick (PhaseModalSetPhaseUpdate (Uuid.fromString phase.uuid)) ]
            in
            div
                ([ class "questionnaireModalPhase"
                 , classList
                    [ ( "questionnaireModalPhase--done", index < currentPhaseIndex )
                    , ( "questionnaireModalPhase--active", index == currentPhaseIndex )
                    ]
                 , dataCy "phase-option"
                 ]
                    ++ clickAttribute
                )
                [ div [ class "fw-bold" ] [ text phase.title ]
                , descriptionElement
                ]
    in
    Modal.simpleWithAttrs [ class "modal-wide" ]
        { modalContent =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text (gettext "Select phase" locale) ]
                , button
                    [ class "close"
                    , onClick (PhaseModalSetOpen False)
                    ]
                    [ faClose ]
                ]
            , div [ class "modal-body" ]
                [ div [] (List.indexedMap viewPhase phases)
                ]
            ]
        , enterMsg = Nothing
        , escMsg = Just (PhaseModalSetOpen False)
        , visible = phaseModalOpen
        , dataCy = "phase-selection"
        }
