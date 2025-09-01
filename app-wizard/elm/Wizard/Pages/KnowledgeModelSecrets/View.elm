module Wizard.Pages.KnowledgeModelSecrets.View exposing (view)

import Form
import Gettext exposing (gettext)
import Html exposing (Html, button, code, div, em, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, colspan)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Shared.Components.FontAwesome exposing (faDelete, faEdit, fas)
import Shared.Components.FormExtra as FormExtra
import Shared.Components.FormGroup as FormGroup
import Shared.Components.Modal as Modal
import Shared.Components.Page as Page
import Shared.Components.Tooltip exposing (tooltip)
import Shared.Components.Undraw as Undraw
import Shared.Utils.TimeDistance as TimeDistance
import Shared.Utils.TimeUtils as TimeUtils
import String.Format as String
import Time.Distance exposing (inWordsWithConfig)
import Wizard.Api.Models.KnowledgeModelSecret exposing (KnowledgeModelSecret)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KnowledgeModelSecrets.Models exposing (Model)
import Wizard.Pages.KnowledgeModelSecrets.Msgs exposing (Msg(..))
import Wizard.Utils.HtmlAttributesUtils exposing (wideDetailClass)


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState model) model.kmSecrets


viewContent : AppState -> Model -> List KnowledgeModelSecret -> Html Msg
viewContent appState model list =
    let
        content =
            if List.isEmpty list then
                Page.illustratedMessage
                    { image = Undraw.noData
                    , heading = gettext "No secrets yet" appState.locale
                    , lines = [ gettext "Create a knowledge model secret to store sensitive information used for integrations in knowledge models." appState.locale ]
                    , cy = "km-secrets-empty"
                    }

            else
                viewSecrets appState list
    in
    div
        [ wideDetailClass "" ]
        [ Page.header (gettext "Knowledge Model Secrets" appState.locale)
            [ button
                [ class "btn btn-primary"
                , onClick (SetCreateModalOpen True)
                ]
                [ text (gettext "Create secret" appState.locale) ]
            ]
        , content
        , viewCreateSecretModal appState model
        , viewEditSecretModal appState model
        , viewDeleteSecretModal appState model
        ]


viewSecrets : AppState -> List KnowledgeModelSecret -> Html Msg
viewSecrets appState list =
    table [ class "table table-hover border rounded" ]
        [ thead [ class "table-light" ]
            [ tr []
                [ th [ colspan 2 ] [ text (gettext "Secret" appState.locale) ]
                , th [ class "text-nowrap" ] [ text (gettext "Last updated" appState.locale) ]
                , th [] []
                ]
            ]
        , tbody [] (List.map (viewSecret appState) (List.sortBy .name list))
        ]


viewSecret : AppState -> KnowledgeModelSecret -> Html Msg
viewSecret appState secret =
    let
        readableTime =
            TimeUtils.toReadableDateTime appState.timeZone secret.updatedAt
    in
    tr []
        [ td [ class "align-middle" ]
            [ fas "fa-lock text-secondary"
            ]
        , td [ class "align-middle", attribute "width" "99%" ]
            [ code [ class "text-break" ] [ text secret.name ]
            ]
        , td [ class "align-middle" ]
            [ em (class "text-secondary text-nowrap pe-2" :: tooltip readableTime)
                [ text (inWordsWithConfig { withAffix = True } (TimeDistance.locale appState.locale) secret.updatedAt appState.currentTime)
                ]
            ]
        , td [ class "align-middle text-end text-nowrap" ]
            [ button
                (class "btn btn-link"
                    :: onClick (SetEditSecret (Just secret))
                    :: tooltip (gettext "Edit" appState.locale)
                )
                [ faEdit ]
            , button
                (class "btn btn-link text-danger"
                    :: onClick (SetDeleteSecret (Just secret))
                    :: tooltip (gettext "Delete" appState.locale)
                )
                [ faDelete ]
            ]
        ]


viewCreateSecretModal : AppState -> Model -> Html Msg
viewCreateSecretModal appState model =
    let
        modalContent =
            [ Html.map CreateFormMsg <| FormGroup.input appState.locale model.createSecretForm "name" (gettext "Name" appState.locale)
            , FormExtra.mdAfter (gettext "Knowledge Model Secret Name can only contain alphanumeric characters, hyphens, underscores, and dots." appState.locale)
            , Html.map CreateFormMsg <| FormGroup.secret appState.locale model.createSecretForm "value" (gettext "Value" appState.locale)
            ]

        config =
            Modal.confirmConfig (gettext "Create Knowledge Model Secret" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible model.createModalOpen
                |> Modal.confirmConfigAction (gettext "Create" appState.locale) (CreateFormMsg Form.Submit)
                |> Modal.confirmConfigActionResult model.creatingSecret
                |> Modal.confirmConfigCancelMsg (SetCreateModalOpen False)
                |> Modal.confirmConfigDataCy "create-secret-modal"
    in
    Modal.confirm appState config


viewEditSecretModal : AppState -> Model -> Html Msg
viewEditSecretModal appState model =
    let
        modalContent =
            [ Html.map EditFormMsg <| FormGroup.input appState.locale model.editSecretForm "name" (gettext "Name" appState.locale)
            , FormExtra.mdAfter (gettext "Knowledge Model Secret Name can only contain alphanumeric characters, hyphens, underscores, and dots." appState.locale)
            , Html.map EditFormMsg <| FormGroup.secret appState.locale model.editSecretForm "value" (gettext "Value" appState.locale)
            ]

        config =
            Modal.confirmConfig (gettext "Edit Knowledge Model Secret" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible (Maybe.isJust model.editSecret)
                |> Modal.confirmConfigAction (gettext "Save" appState.locale) (EditFormMsg Form.Submit)
                |> Modal.confirmConfigActionResult model.editingSecret
                |> Modal.confirmConfigCancelMsg (SetEditSecret Nothing)
                |> Modal.confirmConfigDataCy "edit-secret-modal"
    in
    Modal.confirm appState config


viewDeleteSecretModal : AppState -> Model -> Html Msg
viewDeleteSecretModal appState model =
    let
        content =
            case model.deleteSecret of
                Just secret ->
                    String.formatHtml
                        (gettext "Are you sure you want to delete %s?" appState.locale)
                        [ code [] [ text secret.name ] ]
                        ++ [ p [ class "mt-3" ] [ text (gettext "This action may impact existing knowledge model integrations. Make sure you're confident about what you're doing before continuing." appState.locale) ] ]

                Nothing ->
                    []

        config =
            Modal.confirmConfig (gettext "Delete Knowledge Model Secret" appState.locale)
                |> Modal.confirmConfigContent content
                |> Modal.confirmConfigVisible (Maybe.isJust model.deleteSecret)
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) DeleteKnowledgeModelSecret
                |> Modal.confirmConfigActionResult model.deletingSecret
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigCancelMsg (SetDeleteSecret Nothing)
                |> Modal.confirmConfigDataCy "delete-secret-modal"
    in
    Modal.confirm appState config
