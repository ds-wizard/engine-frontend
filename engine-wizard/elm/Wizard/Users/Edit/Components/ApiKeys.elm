module Wizard.Users.Edit.Components.ApiKeys exposing (Model, Msg, UpdateConfig, fetchData, initialModel, update, view)

import ActionResult exposing (ActionResult)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, form, h3, hr, strong, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onSubmit)
import Maybe.Extra as Maybe
import Shared.Api.ApiKeys as ApiKeysApi
import Shared.Common.TimeUtils as TimeUtils
import Shared.Common.UuidOrCurrent exposing (UuidOrCurrent)
import Shared.Data.ApiKey as ApiKey exposing (ApiKey)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (faSet)
import Shared.Markdown as Markdown
import Shared.Setters exposing (setApiKey, setApiKeys)
import String.Format as String
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.CopyableCodeBlock as CopyableCodeBlock
import Wizard.Common.Html.Attribute exposing (wideDetailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.ActionResultBlock as ActionResultBlock
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Users.Common.ApiKeyCreateForm as ApiKeyCreateForm exposing (ApiKeyCreateForm)


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
            applyResult appState
                { setResult = setApiKeys
                , defaultError = gettext "Unable to get API keys." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                }

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    let
                        body =
                            ApiKeyCreateForm.encode form

                        cmd =
                            Cmd.map cfg.wrapMsg <|
                                ApiKeysApi.fetchApiKey body appState FetchApiKeyComplete
                    in
                    ( { model | apiKey = ActionResult.Loading }, cmd )

                _ ->
                    ( { model | form = Form.update ApiKeyCreateForm.validation formMsg model.form }
                    , Cmd.none
                    )

        FetchApiKeyComplete result ->
            applyResult appState
                { setResult = setApiKey
                , defaultError = gettext "Unable to create API key." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
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
                    , Cmd.map cfg.wrapMsg (ApiKeysApi.deleteApiKey apiKey.uuid appState DeleteApiKeyComplete)
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
                    , getResultCmd cfg.logoutMsg result
                    )


view : AppState -> Model -> Html Msg
view appState model =
    div [ wideDetailClass "" ]
        [ Page.header (gettext "API Keys" appState.locale) []
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
                            (gettext "You can generate an API key for every application you use that needs access to [DSW API](%s)." appState.locale)
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
                    [ class "btn btn-primary btn-with-loader"
                    , onClick NewApiKeyDone
                    ]
                    [ text (gettext "Done" appState.locale) ]
                ]

        _ ->
            form [ onSubmit (FormMsg Form.Submit) ]
                [ FormResult.errorOnlyView appState model.apiKey
                , Html.map FormMsg <| FormGroup.input appState model.form "name" (gettext "API Key Name" appState.locale)
                , FormExtra.textAfter (gettext "Give the API key a name to identify it, such as the name of the application using it or the purpose of the key." appState.locale)
                , Html.map FormMsg <| FormGroup.date appState model.form "expiresAt" (gettext "Expiration" appState.locale)
                , FormExtra.textAfter (gettext "The date when the API key will no longer be valid." appState.locale)
                , ActionButton.submit appState { label = gettext "Create" appState.locale, result = model.apiKey }
                ]


viewApiKeys : AppState -> Model -> Html Msg
viewApiKeys appState model =
    ActionResultBlock.view appState (viewApiKeysTable appState) model.apiKeys


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
                        [ faSet "_global.delete" appState ]
                    ]
                ]

        activeKeys =
            List.filter (ApiKey.isActive appState.currentTime) apiKeys

        content =
            if List.isEmpty activeKeys then
                Flash.info appState (gettext "You have no active API keys." appState.locale)

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
    Modal.confirm appState
        { modalTitle = gettext "Delete API Key" appState.locale
        , modalContent =
            String.formatHtml (gettext "Are you sure you want to delete %s?" appState.locale)
                [ strong [] [ text (Maybe.unwrap "" .name model.apiKeyToDelete) ] ]
        , visible = Maybe.isJust model.apiKeyToDelete
        , actionResult = model.deletingApiKey
        , actionName = gettext "Delete" appState.locale
        , actionMsg = DeleteApiKey
        , cancelMsg = Just (SetApiKeyToDelete Nothing)
        , dangerous = True
        , dataCy = "api-keys_delete"
        }
