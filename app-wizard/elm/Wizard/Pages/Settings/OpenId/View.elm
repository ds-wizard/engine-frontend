module Wizard.Pages.Settings.OpenId.View exposing (view)

import Common.Api.Models.OpenIdClient exposing (OpenIdClient)
import Common.Components.Flash as Flash
import Common.Components.FontAwesome exposing (faDelete)
import Common.Components.Modal as Modal
import Common.Components.Page as Page
import Gettext exposing (gettext)
import Html exposing (Html, a, div, p, strong, text)
import Html.Attributes exposing (class, href)
import Html.Events.Extra exposing (onClickPreventDefaultAndStopPropagation)
import String.Format as String
import Wizard.Components.ExternalLoginButton as ExternalLoginButton
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.OpenId.Models exposing (Model)
import Wizard.Pages.Settings.OpenId.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewOpenIdClients appState model) model.openIdClients


viewOpenIdClients : AppState -> Model -> List OpenIdClient -> Html Msg
viewOpenIdClients appState model openIDs =
    let
        content =
            if List.isEmpty openIDs then
                Flash.info (gettext "There are no OpenID configurations." appState.locale)

            else
                div [ class "card-list" ] (List.map viewOpenIdClient (List.sortBy .name openIDs))
    in
    div []
        [ Page.header (gettext "OpenID" appState.locale)
            [ a
                [ class "btn btn-primary btn-wide"
                , href (Routing.toUrl Routes.settingsOpenIdCreate)
                ]
                [ text (gettext "Create" appState.locale) ]
            ]
        , content
        , viewDeleteModal appState model
        ]


viewOpenIdClient : OpenIdClient -> Html Msg
viewOpenIdClient openId =
    a
        [ class "card bg-light mb-2"
        , href (Routing.toUrl (Routes.settingsOpenIdDetail openId.uuid))
        ]
        [ div [ class "card-body py-2 d-flex align-items-center" ]
            [ ExternalLoginButton.renderIcon openId.style.icon openId.style.color openId.style.background
            , text openId.name
            , a
                [ class "ms-auto link-danger"
                , onClickPreventDefaultAndStopPropagation (ShowHideDeleteOpenIdClient (Just openId))
                ]
                [ faDelete ]
            ]
        ]


viewDeleteModal : AppState -> Model -> Html Msg
viewDeleteModal appState model =
    let
        ( visible, content ) =
            case model.openIdClientToBeDeleted of
                Just openId ->
                    ( True
                    , [ p []
                            (String.formatHtml
                                (gettext "Are you sure you want to permanently delete %s?" appState.locale)
                                [ strong [] [ text openId.name ] ]
                            )
                      ]
                    )

                Nothing ->
                    ( False, [] )

        cfg =
            Modal.confirmConfig (gettext "Delete OpenID" appState.locale)
                |> Modal.confirmConfigContent content
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigActionResult model.deletingOpenIdClient
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) DeleteOpenIdClient
                |> Modal.confirmConfigCancelMsg (ShowHideDeleteOpenIdClient Nothing)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "openid-delete"
    in
    Modal.confirm appState cfg
