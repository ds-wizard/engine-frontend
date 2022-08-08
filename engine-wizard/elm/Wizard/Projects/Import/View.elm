module Wizard.Projects.Import.View exposing (view)

import ActionResult
import Html exposing (Html, div, form, hr, li, p, strong, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireImporter exposing (QuestionnaireImporter)
import Shared.Html exposing (emptyNode)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire.Importer exposing (ImporterResult)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Projects.Import.Models exposing (Model)
import Wizard.Projects.Import.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    let
        actionResult =
            ActionResult.combine model.questionnaire model.questionnaireImporter
    in
    Page.actionResultView appState (viewContent appState model) actionResult


viewContent : AppState -> Model -> ( QuestionnaireDetail, QuestionnaireImporter ) -> Html Msg
viewContent appState model ( questionnaire, _ ) =
    let
        detail =
            case model.importResult of
                Just result ->
                    viewImportResult appState model result

                Nothing ->
                    Flash.info appState "Follow the instructions in the importer window."
    in
    div [ detailClass "" ]
        [ Page.header "Import answers" []
        , div [ class "" ]
            [ strong [] [ text questionnaire.name ]
            , p [] [ text (Maybe.withDefault "" questionnaire.description) ]
            ]
        , hr [] []
        , detail
        ]


viewImportResult : AppState -> Model -> ImporterResult -> Html Msg
viewImportResult appState model result =
    let
        importMessage =
            li []
                [ strong []
                    [ text (String.fromInt (List.length result.questionnaireEvents))
                    ]
                , text " questionnaire changes will be imported."
                ]

        errorMessages =
            if List.isEmpty result.errors then
                emptyNode

            else
                let
                    viewError errorText =
                        li [ class "text-danger" ] [ text errorText ]
                in
                li [ class "text-danger" ]
                    [ strong []
                        [ text (String.fromInt (List.length result.errors))
                        ]
                    , text " errors encountered:"
                    , ul [] (List.map viewError result.errors)
                    ]

        messages =
            ul [] [ importMessage, errorMessages ]
    in
    form [ onSubmit PutImportData ]
        [ FormResult.errorOnlyView appState model.importing
        , div []
            [ strong [] [ text "Import status" ]
            , messages
            ]
        , FormActions.viewSubmit appState
            (Routes.projectsDetailQuestionnaire model.uuid)
            (ActionButton.SubmitConfig "Import" model.importing)
        ]
