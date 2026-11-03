module Common.Pages.Settings.Roles exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Browser.Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.Role as Role exposing (Role)
import Common.Api.Models.Settings exposing (Settings)
import Common.Api.Models.Settings.RolesSettings as RolesSettings exposing (RolesSettings)
import Common.Api.Roles as RolesApi
import Common.Api.Settings as SettingsApi
import Common.Components.ApiErrorAlert as ApiErrorAlert
import Common.Components.ConfirmModal as ConfirmModal
import Common.Data.AppState exposing (AppState)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Section as Section
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String
import Task
import Ui.AppShell as AppShell
import Ui.Badge as Badge
import Ui.Button as Button exposing (Button)
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Table as Table
import Uuid exposing (Uuid)


type alias Model =
    { roles : ActionResult (List Role)
    , settings : ActionResult (Settings RolesSettings)
    , savingDefault : ActionResult ()
    , roleToBeDeleted : Maybe Role
    , deletingRole : ActionResult String
    }


type Msg
    = GetRolesCompleted (Result ApiError (Pagination Role))
    | GetSettingsCompleted (Result ApiError (Settings RolesSettings))
    | SetDefaultRole Uuid
    | SetDefaultRoleCompleted Uuid (Result ApiError ())
    | ShowHideDeleteRole (Maybe Role)
    | DeleteRole
    | DeleteRoleCompleted (Result ApiError ())
    | FocusDone


initialModel : Model
initialModel =
    { roles = Loading
    , settings = Loading
    , savingDefault = Unset
    , roleToBeDeleted = Nothing
    , deletingRole = Unset
    }


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( initialModel
    , Cmd.batch
        [ RolesApi.getRoles appState GetRolesCompleted
        , SettingsApi.get appState RolesSettings.section GetSettingsCompleted
        ]
    )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetRolesCompleted result ->
            RequestHelpers.applyResult
                { setResult = \r m -> { m | roles = ActionResult.map .items r }
                , defaultError = gettext "Unable to get roles." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        GetSettingsCompleted result ->
            RequestHelpers.applyResult
                { setResult = \r m -> { m | settings = r }
                , defaultError = gettext "Unable to get roles." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        SetDefaultRole roleUuid ->
            case model.settings of
                Success settings ->
                    ( { model | savingDefault = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        SettingsApi.put appState
                            RolesSettings.section
                            { settings | value = { defaultRoleUuid = roleUuid } }
                            (SetDefaultRoleCompleted roleUuid)
                    )

                _ ->
                    ( model, Cmd.none )

        SetDefaultRoleCompleted roleUuid result ->
            case result of
                Ok _ ->
                    -- The button that was pressed is gone with the role
                    -- becoming the default, so focus goes to the heading.
                    ( { model
                        | savingDefault = Unset
                        , settings = ActionResult.map (\settings -> { settings | value = { defaultRoleUuid = roleUuid } }) model.settings
                      }
                    , Cmd.map cfg.wrapMsg (Task.attempt (always FocusDone) (Browser.Dom.focus AppShell.mainHeadingId))
                    )

                Err error ->
                    ( { model | savingDefault = ApiError.toActionResult appState (gettext "Default role could not be changed." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        ShowHideDeleteRole mbRole ->
            ( { model | roleToBeDeleted = mbRole, deletingRole = Unset }, Cmd.none )

        DeleteRole ->
            case model.roleToBeDeleted of
                Just role ->
                    ( { model | deletingRole = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        RolesApi.deleteRole appState role.uuid DeleteRoleCompleted
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteRoleCompleted result ->
            case result of
                Ok _ ->
                    -- The row and its delete button are gone, so focus goes to
                    -- the section's heading rather than to nothing.
                    ( { model | roleToBeDeleted = Nothing, deletingRole = Unset, roles = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        Cmd.batch
                            [ RolesApi.getRoles appState GetRolesCompleted
                            , Task.attempt (always FocusDone) (Browser.Dom.focus AppShell.mainHeadingId)
                            ]
                    )

                Err error ->
                    ( { model | deletingRole = ApiError.toActionResult appState (gettext "Role could not be deleted." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        FocusDone ->
            ( model, Cmd.none )


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState model =
    { body =
        [ Layout.stack
            [ Section.header (gettext "Roles" appState.locale)
                [ Button.linkPrimary (gettext "Create" appState.locale) (Routes.toUrl appState Routes.settingsRoleCreate) ]
            , ApiErrorAlert.view appState model.savingDefault
            , Section.actionResult appState (viewRoles appState) (ActionResult.combine model.roles model.settings)
            , viewDeleteModal appState model
            ]
        ]
    , footer = Nothing
    }


viewRoles : AppState -> ( List Role, Settings RolesSettings ) -> Html Msg
viewRoles appState ( roles, settings ) =
    let
        defaultRoleUuid =
            settings.value.defaultRoleUuid
    in
    Table.table
        [ Table.column (gettext "Name" appState.locale) (viewName appState defaultRoleUuid)
        , Table.column (gettext "Users" appState.locale) (Html.text << String.fromInt << .usersCount)
        , Table.actions (gettext "Actions" appState.locale) (\role -> viewSetDefault appState defaultRoleUuid role ++ viewDelete appState defaultRoleUuid role)
        ]
        (List.sortBy (Role.localizedName appState.locale) roles)
        |> Table.withRowTestId (\row -> "roles_row_" ++ Uuid.toString row.uuid)
        |> Table.toHtml


viewName : AppState -> Uuid -> Role -> Html Msg
viewName appState defaultRoleUuid role =
    let
        link =
            Link.link
                { label = Role.localizedName appState.locale role
                , url = Routes.toUrl appState (Routes.settingsRoleDetail role.uuid)
                }
                |> Link.toHtml
    in
    if role.uuid == defaultRoleUuid then
        Layout.inline [ link, Badge.toHtml (Badge.neutral (gettext "Default" appState.locale)) ]

    else
        link


{-| The role new people get is one of the list: any other role can be made it.
-}
viewSetDefault : AppState -> Uuid -> Role -> List (Button Msg)
viewSetDefault appState defaultRoleUuid role =
    if role.uuid == defaultRoleUuid then
        []

    else
        [ Button.ghost (gettext "Set as default" appState.locale) (SetDefaultRole role.uuid)
            |> Button.withAccessibleName (String.format (gettext "Set %s as the default role" appState.locale) [ Role.localizedName appState.locale role ])
            |> Button.withTestId "roles_set-default"
        ]


{-| Every role has a Delete button, so the column reads the same down every row.
A role that cannot be deleted has it unavailable, with the reason on hover and
on focus (`Button.withDisabledReason`): the button stays focusable and is
described by the reason, so a keyboard and a screen reader reach it too.
-}
viewDelete : AppState -> Uuid -> Role -> List (Button Msg)
viewDelete appState defaultRoleUuid role =
    let
        button =
            Button.dangerIconOnly (Icon.solid "fa-trash") (gettext "Delete" appState.locale) (ShowHideDeleteRole (Just role))
                |> Button.withAccessibleName (String.format (gettext "Delete %s" appState.locale) [ Role.localizedName appState.locale role ])
                |> Button.withTestId "roles_delete"
    in
    case deleteBlockedReason appState defaultRoleUuid role of
        Just reason ->
            [ Button.withDisabledReason { id = "role-delete-reason-" ++ Uuid.toString role.uuid, reason = reason } button ]

        Nothing ->
            [ button ]


deleteBlockedReason : AppState -> Uuid -> Role -> Maybe String
deleteBlockedReason appState defaultRoleUuid role =
    if role.isAdmin then
        Just (gettext "Admin role cannot be deleted" appState.locale)

    else if role.uuid == defaultRoleUuid then
        Just (gettext "Default role cannot be deleted" appState.locale)

    else if role.usersCount > 0 then
        Just (gettext "Role cannot be deleted because it is assigned to users" appState.locale)

    else
        Nothing


viewDeleteModal : AppState -> Model -> Html Msg
viewDeleteModal appState model =
    let
        roleName =
            Maybe.map (Role.localizedName appState.locale) model.roleToBeDeleted
                |> Maybe.withDefault ""
    in
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete role" appState.locale
        , open = Maybe.isJust model.roleToBeDeleted
        , question = String.format (gettext "Are you sure you want to permanently delete %s?" appState.locale) [ roleName ]
        , entity = Html.text ""
        , onConfirm = DeleteRole
        , onClose = ShowHideDeleteRole Nothing
        , result = model.deletingRole
        , testId = Just "roles-delete"
        }
