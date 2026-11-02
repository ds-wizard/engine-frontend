module Common.Components.Questionnaire.PhaseSelection exposing
    ( Msg(..)
    , viewBox
    , viewModal
    )

{-| The project's phase: a box at the top of the chapter navigation saying which
phase it is in and how far along, and the dialog that changes it.

Phases are the knowledge model's, so their titles and descriptions are in the
questionnaire's language, like its questions. Changing the phase is an event
(`SetPhase`) the root makes and sends; it changes which questions are desirable,
so the chapter is built again.

Both views are lazy over what they show and nothing else: the box is in the
navigation, which renders beside every keystroke.

-}

import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModel.Phase as Phase exposing (Phase)
import Common.Components.UiWords as UiWords
import Gettext exposing (gettext)
import Html exposing (Html, button, div, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, id, style, type_)
import Html.Events exposing (onClick)
import Html.Lazy as Lazy
import List.Extra as List
import String.Format as String
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Modal as Modal
import Ui.TestId as TestId
import Uuid exposing (Uuid)


type Msg
    = OpenModal
    | CloseModal
    | SetPhase (Maybe Uuid)


{-| The box, or nothing for a knowledge model without phases. `kmLocale` is the
questionnaire's language, `phaseIndex` where the current phase is in the list,
and `labelId` what names the box for a screen reader — it carries the
questionnaire's own prefix, because two questionnaires can be on the page at
once.
-}
viewBox : String -> Gettext.Locale -> Gettext.Locale -> KnowledgeModel -> Bool -> Maybe Uuid -> Int -> Html Msg
viewBox labelId locale kmLocale knowledgeModel readonly phaseUuid phaseIndex =
    Lazy.lazy7 viewBoxLazy labelId locale kmLocale knowledgeModel readonly phaseUuid phaseIndex


viewBoxLazy : String -> Gettext.Locale -> Gettext.Locale -> KnowledgeModel -> Bool -> Maybe Uuid -> Int -> Html Msg
viewBoxLazy labelId locale kmLocale knowledgeModel readonly phaseUuid phaseIndex =
    let
        phases =
            List.map (Phase.localize kmLocale) (KnowledgeModel.getPhases knowledgeModel)

        current =
            List.find (\phase -> Just phase.uuid == Maybe.map Uuid.toString phaseUuid) phases
                |> (\found ->
                        case found of
                            Just _ ->
                                found

                            Nothing ->
                                List.head phases
                   )
    in
    case current of
        Just phase ->
            div [ class "c-questionnaire-phase", attribute "role" "group", attribute "aria-labelledby" labelId ]
                [ span [ class "c-questionnaire-phase__label", id labelId ] [ text (gettext "Current phase" locale) ]
                , Button.toHtml
                    (Button.secondary phase.title OpenModal
                        |> Button.withAccessibleName (String.format (gettext "Change phase, current: %s" locale) [ phase.title ])
                        |> Button.withDisabled readonly
                        |> Button.withTestId "phase-selection"
                    )
                , viewProgress locale phaseIndex (List.length phases)
                ]

        Nothing ->
            text ""


{-| How far along the phases the project is: a bar with a point for each phase,
the ones reached filled and the current one ringed, as in the wizard. One
`progressbar` with its value in words ("2 of 4"); the bar and the points are its
drawing and say nothing of their own.
-}
viewProgress : Gettext.Locale -> Int -> Int -> Html Msg
viewProgress locale phaseIndex count =
    let
        fill =
            if count > 1 then
                toFloat phaseIndex / toFloat (count - 1) * 100

            else
                0

        point index =
            span
                [ classList
                    [ ( "c-questionnaire-phase-progress__point", True )
                    , ( "is-reached", index <= phaseIndex )
                    , ( "is-current", index == phaseIndex )
                    ]
                ]
                []
    in
    div
        [ class "c-questionnaire-phase-progress"
        , attribute "role" "progressbar"
        , attribute "aria-label" (gettext "Phase" locale)
        , attribute "aria-valuemin" "1"
        , attribute "aria-valuemax" (String.fromInt count)
        , attribute "aria-valuenow" (String.fromInt (phaseIndex + 1))
        , attribute "aria-valuetext" ((UiWords.progressBar { locale = locale }).value (toFloat (phaseIndex + 1)) (toFloat count))
        ]
        (span [ class "c-questionnaire-phase-progress__bar" ]
            [ span [ class "c-questionnaire-phase-progress__fill", style "inline-size" (String.fromFloat fill ++ "%") ] [] ]
            :: List.map point (List.range 0 (count - 1))
        )


viewModal : Gettext.Locale -> Gettext.Locale -> KnowledgeModel -> Int -> Bool -> Html Msg
viewModal locale kmLocale knowledgeModel phaseIndex open =
    Lazy.lazy5 viewModalLazy locale kmLocale knowledgeModel phaseIndex open


{-| The phases in order, as steps: the current one is `aria-current="step"` and
cannot be chosen again, the ones before it are marked done in words as well as
by the tick.
-}
viewModalLazy : Gettext.Locale -> Gettext.Locale -> KnowledgeModel -> Int -> Bool -> Html Msg
viewModalLazy locale kmLocale knowledgeModel phaseIndex open =
    let
        viewPhase index phase =
            let
                state =
                    if index < phaseIndex then
                        [ span [ class "c-questionnaire-phase-option__state" ]
                            [ Icon.toHtml (Icon.solid "fa-check")
                            , span [ class "ui-visually-hidden" ] [ text (gettext "Done" locale) ]
                            ]
                        ]

                    else
                        []

                currentAttrs =
                    if index == phaseIndex then
                        [ attribute "aria-current" "step" ]

                    else
                        [ onClick (SetPhase (Uuid.fromString phase.uuid)) ]
            in
            li
                [ classList
                    [ ( "c-questionnaire-phase-step", True )
                    , ( "is-done", index < phaseIndex )
                    , ( "is-current", index == phaseIndex )
                    ]
                ]
                [ button
                    ([ type_ "button"
                     , classList
                        [ ( "c-questionnaire-phase-option", True )
                        , ( "is-done", index < phaseIndex )
                        , ( "is-current", index == phaseIndex )
                        ]
                     , TestId.attribute "phase-option"
                     ]
                        ++ currentAttrs
                    )
                    (span [ class "c-questionnaire-phase-option__title" ] [ text phase.title ]
                        :: state
                        ++ (case phase.description of
                                Just description ->
                                    [ span [ class "c-questionnaire-phase-option__description" ] [ text description ] ]

                                Nothing ->
                                    []
                           )
                    )
                ]

        phases : List Phase
        phases =
            List.map (Phase.localize kmLocale) (KnowledgeModel.getPhases knowledgeModel)
    in
    Modal.plain
        { title = gettext "Select phase" locale
        , open = open
        , onClose = CloseModal
        , body = [ ul [ class "c-questionnaire-phase-options" ] (List.indexedMap viewPhase phases) ]
        }
        |> Modal.withCancelLabel (gettext "Close" locale)
        |> Modal.withTestId "phase-selection"
        |> Modal.toHtml
