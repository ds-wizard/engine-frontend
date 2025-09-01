module Wizard.Pages.Users.Edit.Components.ApiKeys exposing (Model, Msg, UpdateConfig, fetchData, initialModel, update, view)

import ActionResult exposing (ActionResult)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, form, h3, hr, strong, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onSubmit)
import Maybe.Extra as Maybe
import Shared.Components.ActionButton as ActionButton
import Shared.Components.ActionResultBlock as ActionResultBlock
import Shared.Components.Flash as Flash
import Shared.Components.FontAwesome exposing (faDelete)
import Shared.Components.FormExtra as FormExtra
import Shared.Components.FormGroup as FormGroup
import Shared.Components.FormResult as FormResult
import Shared.Components.Modal as Modal
import Shared.Components.Page as Page
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Data.UuidOrCurrent exposing (UuidOrCurrent)
import Shared.Utils.Form.FormError exposing (FormError)
import Shared.Utils.Markdown as Markdown
import Shared.Utils.RequestHelpers as RequestHelpers
import Shared.Utils.Setters exposing (setApiKey, setApiKeys)
import Shared.Utils.TimeUtils as TimeUtils
import String.Format as String
import Wizard.Api.ApiKeys as ApiKeysApi
import Wizard.Api.Models.ApiKey as ApiKey exposing (ApiKey)
import Wizard.Components.CopyableCodeBlock as CopyableCodeBlock
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Users.Common.ApiKeyCreateForm as ApiKeyCreateForm exposing (ApiKeyCreateForm)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


type alias Model =
    { uuidOrCurrent : UuidOrCurrent
    , apiKeys : ActionResult (List ApiKey)
    , apiKey : ActionResult String
    , form : Form FormError ApiKeyCreateForm
    , apiKeyCodeBlockModel : CopyableCodeBlock.Model
    , apiKeyToDelete : Maybe ApiKey
    , deletingApiKey : ActionResult String
    }


initialModel : UuidOrCurrent -> Model
initialModel uuidOrCurrent =
    { uuidOrCurrent = uuidOrCurrent
    , apiKeys = ActionResult.Loading
    , apiKey = ActionResult.Unset
    , form = ApiKeyCreateForm.init
    , apiKeyCodeBlockModel = CopyableCodeBlock.initialModel
    , apiKeyToDelete = Nothing
    , deletingApiKey = ActionResult.Unset
    }


type Msg
    = GetApiKeysComplete (Result ApiError (List ApiKey))
    | FetchApiKeyComplete (Result ApiError String)
    | FormMsg Form.Msg
    | CopyableCodeBlockMsg CopyableCodeBlock.Msg
    | NewApiKeyDone
    | SetApiKeyToDelete (Maybe ApiKey)
    | DeleteApiKey
    | DeleteApiKeyComplete (Result ApiError ())


fetchData : AppState -> Cmd Msg
fetchData appState =
    ApiKeysApi.getApiKeys appState GetApiKeysComplete


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetApiKeysComplete result ->
            RequestHelpers.applyResult
                { setResult = setApiKeys
                , defaultError = gettext "Unable to get API keys." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    let
                        body =
                            ApiKeyCreateForm.encode form

                        cmd =
                            Cmd.map cfg.wrapMsg <|
                                ApiKeysApi.fetchApiKey appState body FetchApiKeyComplete
                    in
                    ( { model | apiKey = ActionResult.Loading }, cmd )

                _ ->
                    ( { model | form = Form.update ApiKeyCreateForm.validation formMsg model.form }
                    , Cmd.none
                    )

        FetchApiKeyComplete result ->
            RequestHelpers.applyResult
                { setResult = setApiKey
                , defaultError = gettext "Unable to create API key." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        CopyableCodeBlockMsg copyableCodeBlockMsg ->
            let
                ( apiKeyCodeBlockModel, apiKeyCodeBlockCmd ) =
                    CopyableCodeBlock.update copyableCodeBlockMsg model.apiKeyCodeBlockModel
            in
            ( { model | apiKeyCodeBlockModel = apiKeyCodeBlockModel }
            , Cmd.map (cfg.wrapMsg << CopyableCodeBlockMsg) apiKeyCodeBlockCmd
            )

        NewApiKeyDone ->
            ( { model
                | apiKey = ActionResult.Unset
                , form = ApiKeyCreateForm.init
                , apiKeys = ActionResult.Loading
              }
            , Cmd.map cfg.wrapMsg <| ApiKeysApi.getApiKeys appState GetApiKeysComplete
            )

        SetApiKeyToDelete mbApiKey ->
            ( { model | apiKeyToDelete = mbApiKey }
            , Cmd.none
            )

        DeleteApiKey ->
            case model.apiKeyToDelete of
                Just apiKey ->
                    ( { model | deletingApiKey = ActionResult.Loading }
                    , Cmd.map cfg.wrapMsg (ApiKeysApi.deleteApiKey appState apiKey.uuid DeleteApiKeyComplete)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteApiKeyComplete result ->
            case result of
                Ok _ ->
                    ( { model
                        | apiKeyToDelete = Nothing
                        , apiKeys = ActionResult.Loading
                        , deletingApiKey = ActionResult.Unset
                      }
                    , Cmd.map cfg.wrapMsg (fetchData appState)
                    )

                Err error ->
                    ( { model
                        | deletingApiKey = ApiError.toActionResult appState (gettext "API key could not be deleted." appState.locale) error
                      }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )


view : AppState -> Model -> Html Msg
view appState model =
    div []
        [ div [ class "row" ]
            [ div [ class "col-8" ]
                [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.profileApiKeys) (gettext "API Keys" appState.locale)
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-8" ]
                [ viewApiKeyForm appState model
                , hr [ class "my-4" ] []
                , viewApiKeys appState model
                ]
            , div [ class "col-4" ]
                [ div [ class "col-border-left" ]
                    [ Markdown.toHtml []
                        (String.format
                            (gettext "You can generate an API key for every application you use that needs access to the [API](%s)." appState.locale)
                            [ appState.apiUrl ++ "/swagger-ui/" ]
                        )
                    ]
                ]
            ]
        , viewApiKeyDeleteModal appState model
        ]


viewApiKeyForm : AppState -> Model -> Html Msg
viewApiKeyForm appState model =
    case model.apiKey of
        ActionResult.Success apiKey ->
            div []
                [ FormGroup.plainGroup
                    (Html.map CopyableCodeBlockMsg <| CopyableCodeBlock.view appState model.apiKeyCodeBlockModel apiKey)
                    (gettext "Your new API key" appState.locale)
                , FormExtra.textAfter (gettext "Make sure to save it, you will not be able to access it again." appState.locale)
                , button
                    [ class "btn btn-primary btn-wide"
                    , onClick NewApiKeyDone
                    ]
                    [ text (gettext "Done" appState.locale) ]
                ]

        _ ->
            form [ onSubmit (FormMsg Form.Submit) ]
                [ FormResult.errorOnlyView model.apiKey
                , Html.map FormMsg <| FormGroup.input appState.locale model.form "name" (gettext "API Key Name" appState.locale)
                , FormExtra.textAfter (gettext "Give the API key a name to identify it, such as the name of the application using it or the purpose of the key." appState.locale)
                , Html.map FormMsg <| FormGroup.date appState.locale model.form "expiresAt" (gettext "Expiration" appState.locale)
                , FormExtra.textAfter (gettext "The date when the API key will no longer be valid." appState.locale)
                , ActionButton.submit { label = gettext "Create" appState.locale, result = model.apiKey }
                ]


viewApiKeys : AppState -> Model -> Html Msg
viewApiKeys appState model =
    ActionResultBlock.view
        { viewContent = viewApiKeysTable appState
        , actionResult = model.apiKeys
        , locale = appState.locale
        }


viewApiKeysTable : AppState -> List ApiKey -> Html Msg
viewApiKeysTable appState apiKeys =
    let
        viewTime time =
            TimeUtils.toReadableDate appState.timeZone time

        viewApiKeyRow : ApiKey -> Html Msg
        viewApiKeyRow apiKey =
            tr []
                [ td [] [ text apiKey.name ]
                , td [ class "text-nowrap" ] [ text (viewTime apiKey.createdAt) ]
                , td [ class "text-nowrap" ] [ text (viewTime apiKey.expiresAt) ]
                , td [ class "text-center px-2" ]
                    [ a [ class "text-danger", onClick (SetApiKeyToDelete (Just apiKey)) ]
                        [ faDelete ]
                    ]
                ]

        activeKeys =
            List.filter (ApiKey.isActive appState.currentTime) apiKeys

        content =
            if List.isEmpty activeKeys then
                Flash.info (gettext "You have no active API keys." appState.locale)

            else
                table [ class "table table-hover" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text (gettext "API Key Name" appState.locale) ]
                            , th [] [ text (gettext "Created" appState.locale) ]
                            , th [] [ text (gettext "Expiration" appState.locale) ]
                            , th [] []
                            ]
                        ]
                    , tbody [] (List.map viewApiKeyRow (List.sortBy (String.toLower << .name) activeKeys))
                    ]
    in
    div []
        [ h3 []
            [ text
                (String.format (gettext "Active API Keys (%s)" appState.locale)
                    [ String.fromInt (List.length activeKeys) ]
                )
            ]
        , content
        ]


viewApiKeyDeleteModal : AppState -> Model -> Html Msg
viewApiKeyDeleteModal appState model =
    let
        modalContent =
            String.formatHtml (gettext "Are you sure you want to delete %s?" appState.locale)
                [ strong [] [ text (Maybe.unwrap "" .name model.apiKeyToDelete) ] ]

        cfg =
            Modal.confirmConfig (gettext "Delete API Key" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible (Maybe.isJust model.apiKeyToDelete)
                |> Modal.confirmConfigActionResult model.deletingApiKey
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) DeleteApiKey
                |> Modal.confirmConfigCancelMsg (SetApiKeyToDelete Nothing)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "api-keys_delete"
    in
    Modal.confirm appState cfg
