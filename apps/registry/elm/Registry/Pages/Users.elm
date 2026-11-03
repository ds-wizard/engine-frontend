module Registry.Pages.Users exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

{-| The registry's accounts, for an administrator: who they are, and
switching one off or deleting it. One's own account is not offered either.
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.Page as Page
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Registry.Api as Api
import Registry.Api.Models.User as User exposing (User)
import Registry.Data.AppState exposing (AppState)
import String.Format as String
import Ui.Alert as Alert
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Page.Content as Content
import Ui.Table as Table
import Ui.Text as Text
import Uuid


type alias Model =
    { users : ActionResult (List User)
    , changing : ActionResult ()
    , toDelete : Maybe User
    , deleting : ActionResult ()
    }


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( { users = Loading, changing = Unset, toDelete = Nothing, deleting = Unset }
    , Api.getUsers appState GetUsersCompleted
    )


type Msg
    = GetUsersCompleted (Result ApiError (List User))
    | SetActive User Bool
    | SetActiveCompleted (Result ApiError ())
    | AskDelete (Maybe User)
    | Delete
    | DeleteCompleted (Result ApiError ())


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        GetUsersCompleted (Ok users) ->
            ( { model | users = Success (List.sortBy (String.toLower << .email) users) }, Cmd.none )

        GetUsersCompleted (Err error) ->
            ( { model | users = ApiError.toActionResult appState (gettext "Unable to get users." appState.locale) error }, Cmd.none )

        SetActive user active ->
            ( { model | changing = Loading }, Api.putUserState appState user.uuid active SetActiveCompleted )

        SetActiveCompleted (Ok _) ->
            ( { model | changing = Success () }, Api.getUsers appState GetUsersCompleted )

        SetActiveCompleted (Err error) ->
            ( { model | changing = ApiError.toActionResult appState (gettext "User could not be saved." appState.locale) error }, Cmd.none )

        AskDelete mbUser ->
            ( { model | toDelete = mbUser, deleting = Unset }, Cmd.none )

        Delete ->
            case model.toDelete of
                Just user ->
                    ( { model | deleting = Loading }, Api.deleteUser appState user.uuid DeleteCompleted )

                Nothing ->
                    ( model, Cmd.none )

        DeleteCompleted (Ok _) ->
            ( { model | toDelete = Nothing, deleting = Unset }, Api.getUsers appState GetUsersCompleted )

        DeleteCompleted (Err error) ->
            ( { model | deleting = ApiError.toActionResult appState (gettext "User could not be deleted." appState.locale) error }, Cmd.none )


view : AppState -> Model -> Html Msg
view appState model =
    let
        isMe user =
            Maybe.map .uuid appState.user == Just user.uuid

        stateBadge user =
            if user.active then
                Badge.toHtml (Badge.success (gettext "active" appState.locale))

            else
                Badge.toHtml (Badge.neutral (gettext "inactive" appState.locale))

        roleText user =
            if User.isAdmin user then
                gettext "Admin" appState.locale

            else
                gettext "User" appState.locale

        rowActions user =
            if isMe user then
                []

            else
                [ if user.active then
                    Button.iconOnly (Icon.solid "fa-user-slash") (gettext "Deactivate" appState.locale) (SetActive user False)
                        |> Button.withAccessibleName (String.format (gettext "Deactivate %s" appState.locale) [ user.email ])

                  else
                    Button.iconOnly (Icon.solid "fa-user-check") (gettext "Activate" appState.locale) (SetActive user True)
                        |> Button.withAccessibleName (String.format (gettext "Activate %s" appState.locale) [ user.email ])
                , Button.dangerIconOnly (Icon.solid "fa-trash") (gettext "Delete" appState.locale) (AskDelete (Just user))
                    |> Button.withAccessibleName (String.format (gettext "Delete %s" appState.locale) [ user.email ])
                ]

        usersTable users =
            Table.table
                [ Table.column (gettext "Name" appState.locale) (Text.body << User.fullName)
                , Table.column (gettext "Email" appState.locale) (Text.body << .email)
                , Table.column (gettext "Role" appState.locale) (Text.body << roleText)
                , Table.column (gettext "State" appState.locale) stateBadge
                , Table.actions (gettext "Actions" appState.locale) rowActions
                ]
                users
                |> Table.withRowTestId (\row -> "users_row_" ++ Uuid.toString row.uuid)
                |> Table.withCaption (gettext "Users" appState.locale)
                |> Table.toHtml

        changeError =
            case model.changing of
                Error message ->
                    Alert.toHtml (Alert.danger message)

                _ ->
                    Html.text ""

        deleteModal =
            ConfirmModal.deleteWithTestId appState
                { title = gettext "Delete user" appState.locale
                , open = Maybe.isJust model.toDelete
                , question = gettext "Are you sure you want to permanently delete the following user?" appState.locale
                , entity = Text.body (Maybe.unwrap "" .email model.toDelete)
                , onConfirm = Delete
                , onClose = AskDelete Nothing
                , result = model.deleting
                , testId = Just "users-delete"
                }
    in
    Content.content
        { title = gettext "Users" appState.locale
        , body =
            [ Layout.stack
                [ changeError
                , Page.actionResultWithinPage appState usersTable model.users
                , deleteModal
                ]
            ]
        }
        |> Content.full
        |> Content.toHtml
