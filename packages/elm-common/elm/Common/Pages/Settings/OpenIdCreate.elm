module Common.Pages.Settings.OpenIdCreate exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.OpenIdClient exposing (OpenIdClient)
import Common.Api.Models.OpenIdClientDetail exposing (OpenIdClientDetail)
import Common.Api.Models.Prefab exposing (Prefab)
import Common.Api.OpenIdClients as OpenIdClientsApi
import Common.Api.Prefabs as PrefabsApi
import Common.Components.ExternalLoginButton as ExternalLoginButton
import Common.Components.Form as CommonForm
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.OpenIdClientForm as OpenIdClientForm exposing (OpenIdClientForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.OpenIdClientFields as OpenIdClientFields
import Common.Pages.Settings.Components.Section as Section
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Form as UiForm
import Ui.Layout as Layout
import Uuid


type alias Model =
    { form : Form FormError OpenIdClientForm
    , savingForm : ActionResult ()
    , openIdPrefabs : ActionResult (List OpenIdClientDetail)
    }


type Msg
    = GetOpenIdPrefabsComplete (Result ApiError (List (Prefab OpenIdClientDetail)))
    | PostOpenIdComplete (Result ApiError OpenIdClient)
    | Cancel
    | FormMsg Form.Msg
    | FillOpenIDServiceConfig OpenIdClientDetail
    | NoOp


initialModel : AppState -> Model
initialModel appState =
    { form = OpenIdClientForm.initEmpty appState
    , savingForm = Unset
    , openIdPrefabs = Loading
    }


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( initialModel appState
    , PrefabsApi.getOpenIDPrefabs appState GetOpenIdPrefabsComplete
    )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetOpenIdPrefabsComplete result ->
            case result of
                Ok openIdPrefabs ->
                    ( { model | openIdPrefabs = Success (List.map .content openIdPrefabs) }, Cmd.none )

                Err error ->
                    ( { model | openIdPrefabs = ApiError.toActionResult appState (gettext "Unable to get OpenID prefabs." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        PostOpenIdComplete result ->
            case result of
                Ok openId ->
                    ( { model | savingForm = Success () }
                    , Routes.navigate appState (Routes.settingsOpenIdDetail openId.uuid)
                    )

                Err error ->
                    ( { model | savingForm = ApiError.toActionResult appState (gettext "OpenID config could not be saved." appState.locale) error }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , CommonForm.focusErrorSummary
                        ]
                    )

        Cancel ->
            ( model, Window.historyBack (Routes.toUrl appState Routes.settingsOpenId) )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    ( { model | savingForm = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        OpenIdClientsApi.postOpenIdClient appState (OpenIdClientForm.toOpenIdClientDetail Uuid.nil form) PostOpenIdComplete
                    )

                _ ->
                    ( { model | form = Form.update (OpenIdClientForm.validation appState) formMsg model.form }
                    , OpenIdClientFields.focusProviderTab (cfg.wrapMsg NoOp) formMsg
                    )

        FillOpenIDServiceConfig openIdServiceConfig ->
            ( { model | form = OpenIdClientForm.fillFromDetail appState openIdServiceConfig model.form }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    CommonForm.submitShortcut appState { result = model.savingForm, onSubmit = FormMsg Form.Submit }


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState model =
    { body =
        [ Layout.stack
            [ Section.header (gettext "Create OpenID Config" appState.locale)
                [ Section.guideLink appState GuideLinks.settingsOpenId ]
            , Section.actionResult appState (viewForm appState model) model.openIdPrefabs
            ]
        ]
    , footer = Nothing
    }


viewForm : AppState -> Model -> List OpenIdClientDetail -> Html Msg
viewForm appState model openIdPrefabs =
    let
        prefabs =
            if not (List.isEmpty openIdPrefabs) && OpenIdClientForm.isEmpty model.form then
                [ UiForm.section
                    { id = "openid-quick-setup"
                    , legend = gettext "Quick setup" appState.locale
                    , content =
                        [ Layout.inline
                            (openIdPrefabs
                                |> List.sortBy .name
                                |> List.map (\prefab -> ExternalLoginButton.view { service = prefab, onClick = FillOpenIDServiceConfig prefab })
                            )
                        ]
                    }
                ]

            else
                []
    in
    CommonForm.createView
        { onSubmit = FormMsg Form.Submit
        , content =
            CommonForm.errorSummary model.savingForm
                :: prefabs
                ++ OpenIdClientFields.view appState { form = model.form, toMsg = FormMsg, afterName = [] }
                ++ [ CommonForm.actions appState
                        { submitLabel = gettext "Create" appState.locale
                        , result = model.savingForm
                        , onCancel = Just Cancel
                        }
                   ]
        }
