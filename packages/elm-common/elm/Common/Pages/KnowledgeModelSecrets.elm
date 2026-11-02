module Common.Pages.KnowledgeModelSecrets exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

{-| Knowledge Model Secrets: the feature router, and the one page
of the feature.

The secrets are a table; creating one and setting a new value of one is a form
in a modal, and deleting one is confirmed. The value is a `secret` field, hidden
until revealed. The server never sends a value back, so a secret's value is
never shown again: it can only be replaced.

Two differences from the wizard, both about where the reader is left:

  - **The page has its heading while the list loads**, so focus after the route
    change lands on it and a failed load is an alert under it.
  - **After a create or an edit the list is fetched again in place** instead of
    being replaced by the loading state, so the button the modal returns focus
    to is still there. After a delete the row that had the button is gone, so
    focus goes to the page heading.

This feature has no nested route type and its top-level route has no arguments,
so the functions that other routers give a route take none here.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelSecrets as KnowledgeModelSecretsApi
import Common.Api.Models.KnowledgeModelSecret exposing (KnowledgeModelSecret)
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.Form as C2Form
import Common.Components.Form.Field as FormField
import Common.Components.Page as Page
import Common.Components.Timestamp as Timestamp
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.KnowledgeModelSecretForm as KnowledgeModelSecretForm exposing (KnowledgeModelSecretForm)
import Common.Data.RowPolicy exposing (RowPolicy)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Random
import String.Format as String
import Ui.Alert as Alert
import Ui.AppShell as AppShell
import Ui.Button as Button exposing (Button)
import Ui.EmptyState as EmptyState
import Ui.Icon as Icon
import Ui.Illustration as Illustration
import Ui.Layout as Layout
import Ui.Modal as Modal
import Ui.Page.Content as Content
import Ui.Table as Table
import Ui.Text as Text
import Uuid



-- MODEL


type alias Model =
    { kmSecrets : ActionResult (List KnowledgeModelSecret)
    , createModalOpen : Bool
    , createSecretForm : Form FormError KnowledgeModelSecretForm
    , creatingSecret : ActionResult String
    , editSecret : Maybe KnowledgeModelSecret
    , editSecretForm : Form FormError KnowledgeModelSecretForm
    , editingSecret : ActionResult String
    , deleteSecret : Maybe KnowledgeModelSecret
    , deletingSecret : ActionResult String
    }


initialModel : AppState -> Model
initialModel appState =
    { kmSecrets = ActionResult.Loading
    , createModalOpen = False
    , createSecretForm = KnowledgeModelSecretForm.initEmpty appState
    , creatingSecret = ActionResult.Unset
    , editSecret = Nothing
    , editSecretForm = KnowledgeModelSecretForm.initEmpty appState
    , editingSecret = ActionResult.Unset
    , deleteSecret = Nothing
    , deletingSecret = ActionResult.Unset
    }


init : AppState -> Model -> ( Model, Cmd Msg )
init =
    \appState _ ->
        ( initialModel appState, fetchData appState )


fetchData : AppState -> Cmd Msg
fetchData appState =
    KnowledgeModelSecretsApi.getKnowledgeModelSecrets appState GetKnowledgeModelSecretsCompleted



-- UPDATE


type Msg
    = GetKnowledgeModelSecretsCompleted (Result ApiError (List KnowledgeModelSecret))
    | SetCreateModalOpen Bool
    | CreateFormMsg Form.Msg
    | PostKnowledgeModelSecretCompleted (Result ApiError ())
    | SetEditSecret (Maybe KnowledgeModelSecret)
    | EditFormMsg Form.Msg
    | PutKnowledgeModelSecretCompleted (Result ApiError ())
    | SetDeleteSecret (Maybe KnowledgeModelSecret)
    | DeleteKnowledgeModelSecret
    | DeleteKnowledgeModelSecretCompleted (Result ApiError ())
    | Noop


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    let
        ( newModel, cmd ) =
            updateModel cfg appState msg model
    in
    ( appState.seed, newModel, cmd )


updateModel : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
updateModel cfg appState msg model =
    case msg of
        GetKnowledgeModelSecretsCompleted result ->
            RequestHelpers.applyResult
                { setResult = \v m -> { m | kmSecrets = v }
                , defaultError = gettext "Unable to load knowledge model secrets." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        SetCreateModalOpen open ->
            ( { model
                | createModalOpen = open
                , createSecretForm = KnowledgeModelSecretForm.initEmpty appState
                , creatingSecret = ActionResult.Unset
              }
            , Cmd.none
            )

        CreateFormMsg formMsg ->
            case ( formMsg, Form.getOutput model.createSecretForm ) of
                ( Form.Submit, Just createForm ) ->
                    if ActionResult.isLoading model.creatingSecret then
                        ( model, Cmd.none )

                    else
                        ( { model | creatingSecret = ActionResult.Loading }
                        , KnowledgeModelSecretsApi.postKnowledgeModelSecret appState createForm (cfg.wrapMsg << PostKnowledgeModelSecretCompleted)
                        )

                _ ->
                    ( { model | createSecretForm = Form.update (KnowledgeModelSecretForm.validation appState) formMsg model.createSecretForm }
                    , Cmd.none
                    )

        PostKnowledgeModelSecretCompleted result ->
            case result of
                Ok _ ->
                    ( { model | createModalOpen = False, creatingSecret = ActionResult.Unset }
                    , Cmd.map cfg.wrapMsg (fetchData appState)
                    )

                Err error ->
                    ( { model | creatingSecret = ApiError.toActionResult appState (gettext "Unable to create knowledge model secret." appState.locale) error }
                    , Cmd.batch [ RequestHelpers.getResultCmd cfg.logoutMsg result, C2Form.focusErrorSummary ]
                    )

        SetEditSecret mbSecret ->
            let
                editSecretForm =
                    case mbSecret of
                        Just secret ->
                            KnowledgeModelSecretForm.init appState secret

                        Nothing ->
                            model.editSecretForm
            in
            ( { model
                | editSecret = mbSecret
                , editSecretForm = editSecretForm
                , editingSecret = ActionResult.Unset
              }
            , Cmd.none
            )

        EditFormMsg formMsg ->
            case ( formMsg, Form.getOutput model.editSecretForm, model.editSecret ) of
                ( Form.Submit, Just editForm, Just secret ) ->
                    if ActionResult.isLoading model.editingSecret then
                        ( model, Cmd.none )

                    else
                        ( { model | editingSecret = ActionResult.Loading }
                        , KnowledgeModelSecretsApi.putKnowledgeModelSecret appState secret.uuid editForm (cfg.wrapMsg << PutKnowledgeModelSecretCompleted)
                        )

                _ ->
                    ( { model | editSecretForm = Form.update (KnowledgeModelSecretForm.validation appState) formMsg model.editSecretForm }
                    , Cmd.none
                    )

        PutKnowledgeModelSecretCompleted result ->
            case result of
                Ok _ ->
                    ( { model | editSecret = Nothing, editingSecret = ActionResult.Unset }
                    , Cmd.map cfg.wrapMsg (fetchData appState)
                    )

                Err error ->
                    ( { model | editingSecret = ApiError.toActionResult appState (gettext "Unable to update knowledge model secret." appState.locale) error }
                    , Cmd.batch [ RequestHelpers.getResultCmd cfg.logoutMsg result, C2Form.focusErrorSummary ]
                    )

        SetDeleteSecret secret ->
            ( { model | deleteSecret = secret, deletingSecret = ActionResult.Unset }
            , Cmd.none
            )

        DeleteKnowledgeModelSecret ->
            case model.deleteSecret of
                Just secret ->
                    ( { model | deletingSecret = ActionResult.Loading }
                    , KnowledgeModelSecretsApi.deleteKnowledgeModelSecret appState secret.uuid (cfg.wrapMsg << DeleteKnowledgeModelSecretCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteKnowledgeModelSecretCompleted result ->
            case result of
                Ok _ ->
                    ( { model
                        | deleteSecret = Nothing
                        , deletingSecret = ActionResult.Unset
                        , kmSecrets = ActionResult.Loading
                      }
                    , Cmd.batch
                        [ Cmd.map cfg.wrapMsg (fetchData appState)
                        , AppShell.focusOnRouteChange (cfg.wrapMsg Noop)
                        ]
                    )

                Err error ->
                    ( { model | deletingSecret = ApiError.toActionResult appState (gettext "Unable to delete knowledge model secret." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        Noop ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    if model.createModalOpen then
        C2Form.submitShortcut appState { result = model.creatingSecret, onSubmit = CreateFormMsg Form.Submit }

    else if Maybe.isJust model.editSecret then
        C2Form.submitShortcut appState { result = model.editingSecret, onSubmit = EditFormMsg Form.Submit }

    else
        Sub.none



-- VIEW


view : RowPolicy -> AppState -> Model -> Html Msg
view policy appState model =
    let
        body =
            case model.kmSecrets of
                Success secrets ->
                    if List.isEmpty secrets then
                        EmptyState.emptyState
                            { title = gettext "No secrets yet" appState.locale
                            , description = gettext "Create a knowledge model secret to store sensitive information used for integrations in knowledge models." appState.locale
                            }
                            |> EmptyState.withIllustration (Illustration.toHtml Illustration.empty)
                            |> EmptyState.toHtml

                    else
                        viewSecrets policy appState secrets

                Error message ->
                    Alert.toHtml (Alert.danger message)

                _ ->
                    Page.loading appState
    in
    Content.content
        { title = gettext "Knowledge Model Secrets" appState.locale
        , body =
            [ body
            , viewCreateSecretModal appState model
            , viewEditSecretModal appState model
            , viewDeleteSecretModal appState model
            ]
        }
        |> Content.withActions
            [ Button.primary (gettext "Create secret" appState.locale) (SetCreateModalOpen True)
                |> Button.withTestId "km-secrets_create"
            ]
        |> Content.list
        |> Content.toHtml


viewSecrets : RowPolicy -> AppState -> List KnowledgeModelSecret -> Html Msg
viewSecrets policy appState secrets =
    Table.table
        [ Table.column (gettext "Secret" appState.locale) viewSecretName
        , Table.column (gettext "Last updated" appState.locale) (Timestamp.view appState << .updatedAt)
        , Table.actions (gettext "Actions" appState.locale) (viewSecretActions policy appState)
        ]
        (List.sortBy .name secrets)
        |> Table.withRowTestId (\row -> "km-secrets_row_" ++ Uuid.toString row.uuid)
        |> Table.toHtml


viewSecretName : KnowledgeModelSecret -> Html Msg
viewSecretName secret =
    Layout.inline
        [ Icon.toHtml (Icon.solid "fa-lock")
        , Text.code secret.name
        ]


viewSecretActions : RowPolicy -> AppState -> KnowledgeModelSecret -> List (Button Msg)
viewSecretActions policy appState secret =
    -- Inside a workspace, the organization's secrets are used and not
    -- changed: they are kept in the organization's library.
    if policy.manage secret.workspaceUuid then
        [ Button.iconOnly (Icon.solid "fa-key") (gettext "Set new value" appState.locale) (SetEditSecret (Just secret))
            |> Button.withAccessibleName (String.format (gettext "Set new value of %s" appState.locale) [ secret.name ])
            |> Button.withTestId "km-secrets_set-value"
        , Button.dangerIconOnly (Icon.solid "fa-trash") (gettext "Delete" appState.locale) (SetDeleteSecret (Just secret))
            |> Button.withAccessibleName (String.format (gettext "Delete %s" appState.locale) [ secret.name ])
            |> Button.withTestId "km-secrets_delete"
        ]

    else
        []


{-| The create and the set-value dialog are both on the page, so each form's
ids get their own prefix.
-}
nameField : AppState -> String -> Form FormError KnowledgeModelSecretForm -> Html Form.Msg
nameField appState idPrefix form =
    FormField.input appState form "name" (gettext "Name" appState.locale)
        |> FormField.withIdPrefix idPrefix
        |> FormField.withHint (gettext "Knowledge Model Secret Name can only contain alphanumeric characters, hyphens, underscores, and dots." appState.locale)
        |> FormField.withRequired True
        |> FormField.withAutocomplete "off"
        |> FormField.toHtml


valueField : AppState -> String -> Form FormError KnowledgeModelSecretForm -> Html Form.Msg
valueField appState idPrefix form =
    FormField.secret appState form "value" (gettext "Value" appState.locale)
        |> FormField.withIdPrefix idPrefix
        |> FormField.withRequired True
        |> FormField.toHtml


viewSecretFormModal :
    AppState
    ->
        { title : String
        , open : Bool
        , form : Form FormError KnowledgeModelSecretForm
        , toMsg : Form.Msg -> Msg
        , fields : List (Html Form.Msg)
        , result : ActionResult String
        , submitLabel : String
        , onClose : Msg
        , testId : String
        }
    -> Html Msg
viewSecretFormModal appState config =
    Modal.plain
        { title = config.title
        , open = config.open
        , onClose = config.onClose
        , body =
            C2Form.errorSummary config.result
                :: List.map (Html.map config.toMsg) config.fields
        }
        |> Modal.withForm { onSubmit = config.toMsg Form.Submit, submitLabel = config.submitLabel }
        |> Modal.withLoading (ActionResult.isLoading config.result)
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withTestId config.testId
        |> Modal.toHtml


viewCreateSecretModal : AppState -> Model -> Html Msg
viewCreateSecretModal appState model =
    viewSecretFormModal appState
        { title = gettext "Create Knowledge Model Secret" appState.locale
        , open = model.createModalOpen
        , form = model.createSecretForm
        , toMsg = CreateFormMsg
        , fields =
            [ nameField appState "create-secret-modal-" model.createSecretForm
            , valueField appState "create-secret-modal-" model.createSecretForm
            ]
        , result = model.creatingSecret
        , submitLabel = gettext "Create" appState.locale
        , onClose = SetCreateModalOpen False
        , testId = "create-secret-modal"
        }


{-| The name is kept in the form, from the secret, since the server takes it with
the value; only the value is asked for.
-}
viewEditSecretModal : AppState -> Model -> Html Msg
viewEditSecretModal appState model =
    let
        name =
            Maybe.unwrap "" .name model.editSecret
    in
    viewSecretFormModal appState
        { title = gettext "Set New Value" appState.locale
        , open = Maybe.isJust model.editSecret
        , form = model.editSecretForm
        , toMsg = EditFormMsg
        , fields =
            [ Text.body (String.format (gettext "The value of %s cannot be shown. The new value replaces it, and the knowledge models that use the secret use the new one." appState.locale) [ name ])
            , valueField appState "edit-secret-modal-" model.editSecretForm
            ]
        , result = model.editingSecret
        , submitLabel = gettext "Set value" appState.locale
        , onClose = SetEditSecret Nothing
        , testId = "edit-secret-modal"
        }


viewDeleteSecretModal : AppState -> Model -> Html Msg
viewDeleteSecretModal appState model =
    let
        name =
            Maybe.unwrap "" .name model.deleteSecret
    in
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete Knowledge Model Secret" appState.locale
        , open = Maybe.isJust model.deleteSecret
        , question = String.format (gettext "Are you sure you want to delete %s?" appState.locale) [ name ]
        , entity = Text.body (gettext "This action may impact existing knowledge model integrations. Make sure you're confident about what you're doing before continuing." appState.locale)
        , onConfirm = DeleteKnowledgeModelSecret
        , onClose = SetDeleteSecret Nothing
        , result = model.deletingSecret
        , testId = Just "delete-secret-modal"
        }


isGuarded : AppState -> Model -> Maybe String
isGuarded =
    \_ _ ->
        Nothing
