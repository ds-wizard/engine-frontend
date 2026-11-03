module Registry.Pages.ApiKeys exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

{-| The keys a wizard reads the registry with: named, without an end,
revocable. A new key's value is shown once, right after it is made, with a
button that copies it, and never again.
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.Copyable as Copyable
import Common.Components.Form as Form
import Common.Components.Page as Page
import Common.Utils.TimeUtils as TimeUtils
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Registry.Api as Api
import Registry.Api.Models.User exposing (ApiKey)
import Registry.Components.Fields as Fields
import Registry.Data.AppState exposing (AppState)
import String.Format as String
import Ui.Alert as Alert
import Ui.Button as Button
import Ui.Copy as Copy
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Page.Content as Content
import Ui.Table as Table
import Ui.Text as Text
import Uuid


type alias Model =
    { keys : ActionResult (List ApiKey)
    , name : String
    , submitted : Bool
    , creating : ActionResult String
    , copy : Copy.Model
    , toDelete : Maybe ApiKey
    , deleting : ActionResult ()
    }


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( { keys = Loading
      , name = ""
      , submitted = False
      , creating = Unset
      , copy = Copy.init
      , toDelete = Nothing
      , deleting = Unset
      }
    , Api.getApiKeys appState GetKeysCompleted
    )


type Msg
    = GetKeysCompleted (Result ApiError (List ApiKey))
    | SetName String
    | Create
    | CreateCompleted (Result ApiError String)
    | CopyMsg Copy.Msg
    | AskDelete (Maybe ApiKey)
    | Delete
    | DeleteCompleted (Result ApiError ())


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        GetKeysCompleted (Ok keys) ->
            ( { model | keys = Success keys }, Cmd.none )

        GetKeysCompleted (Err error) ->
            ( { model | keys = ApiError.toActionResult appState (gettext "Unable to get API keys." appState.locale) error }, Cmd.none )

        SetName name ->
            ( { model | name = name }, Cmd.none )

        Create ->
            if ActionResult.isLoading model.creating then
                ( model, Cmd.none )

            else if Maybe.isNothing (Fields.requiredError appState model.name) then
                ( { model | submitted = True, creating = Loading }
                , Api.postApiKey appState (String.trim model.name) CreateCompleted
                )

            else
                ( { model | submitted = True }, Cmd.none )

        CreateCompleted (Ok token) ->
            ( { model | creating = Success token, name = "", submitted = False }
            , Api.getApiKeys appState GetKeysCompleted
            )

        CreateCompleted (Err error) ->
            ( { model | creating = ApiError.toActionResult appState (gettext "API key could not be created." appState.locale) error }
            , Form.focusErrorSummary
            )

        CopyMsg copyMsg ->
            let
                ( copy, cmd ) =
                    Copy.update copyMsg model.copy
            in
            ( { model | copy = copy }, Cmd.map CopyMsg cmd )

        AskDelete mbKey ->
            ( { model | toDelete = mbKey, deleting = Unset }, Cmd.none )

        Delete ->
            case model.toDelete of
                Just key ->
                    ( { model | deleting = Loading }, Api.deleteApiKey appState key.uuid DeleteCompleted )

                Nothing ->
                    ( model, Cmd.none )

        DeleteCompleted (Ok _) ->
            ( { model | toDelete = Nothing, deleting = Unset }, Api.getApiKeys appState GetKeysCompleted )

        DeleteCompleted (Err error) ->
            ( { model | deleting = ApiError.toActionResult appState (gettext "API key could not be deleted." appState.locale) error }, Cmd.none )


view : AppState -> Model -> Html Msg
view appState model =
    let
        created =
            case model.creating of
                Success token ->
                    [ Alert.toHtml (Alert.success (gettext "The API key has been created. Copy it now: it will not be shown again." appState.locale))
                    , Layout.inline
                        [ Text.code token
                        , Copyable.button appState
                            { key = "api-key"
                            , label = gettext "Copy the API key" appState.locale
                            , value = token
                            , model = model.copy
                            , toMsg = CopyMsg
                            }
                        ]
                    ]

                _ ->
                    []

        createForm =
            Form.view
                { onSubmit = Create
                , content =
                    [ Text.title (gettext "New API key" appState.locale)
                    , Form.errorSummary model.creating
                    , Fields.field
                        { id = "api-key-name"
                        , label = gettext "Name" appState.locale
                        , kind = Fields.Text "off"
                        , value = model.name
                        , onInput = SetName
                        , error =
                            if model.submitted then
                                Fields.requiredError appState model.name

                            else
                                Nothing
                        }
                    , Form.actions appState { submitLabel = gettext "Create" appState.locale, result = model.creating, onCancel = Nothing }
                    ]
                }

        keysTable keys =
            if List.isEmpty keys then
                Text.body (gettext "You have no API keys yet." appState.locale)

            else
                Table.table
                    [ Table.column (gettext "Name" appState.locale) (Text.body << .name)
                    , Table.column (gettext "Created" appState.locale) (Text.body << TimeUtils.toReadableDateTime appState.timeZone << .createdAt)
                    , Table.actions (gettext "Actions" appState.locale)
                        (\key ->
                            [ Button.dangerIconOnly (Icon.solid "fa-trash") (gettext "Delete" appState.locale) (AskDelete (Just key))
                                |> Button.withAccessibleName (String.format (gettext "Delete %s" appState.locale) [ key.name ])
                            ]
                        )
                    ]
                    keys
                    |> Table.withRowTestId (\row -> "api-keys_row_" ++ Uuid.toString row.uuid)
                    |> Table.withCaption (gettext "API keys" appState.locale)
                    |> Table.toHtml

        deleteModal =
            ConfirmModal.deleteWithTestId appState
                { title = gettext "Delete API key" appState.locale
                , open = Maybe.isJust model.toDelete
                , question = gettext "Are you sure you want to delete this API key? Wizards that use it will no longer reach the registry." appState.locale
                , entity = Text.body (Maybe.unwrap "" .name model.toDelete)
                , onConfirm = Delete
                , onClose = AskDelete Nothing
                , result = model.deleting
                , testId = Just "api-keys_delete"
                }
    in
    Content.content
        { title = gettext "API Keys" appState.locale
        , body =
            [ Layout.stack
                (Text.body (gettext "A wizard reads the registry with an API key: paste it in the wizard's registry settings." appState.locale)
                    :: created
                    ++ [ Page.actionResultWithinPage appState keysTable model.keys
                       , createForm
                       , deleteModal
                       ]
                )
            ]
        }
        |> Content.toHtml
