module Wizard.Pages.Settings.Roles.Update exposing
    ( UpdateConfig
    , fetchData
    , update
    )

import ActionResult
import Common.Api.ApiError as ApiError
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Wizard.Api.Roles as RolesApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Roles.Models exposing (Model)
import Wizard.Pages.Settings.Roles.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    RolesApi.getRoles appState GetRolesCompleted


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
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

        ShowHideDeleteRole mbRole ->
            ( { model
                | roleToBeDeleted = mbRole
                , deletingRole = ActionResult.Unset
              }
            , Cmd.none
            )

        DeleteRole ->
            case model.roleToBeDeleted of
                Just role ->
                    ( { model | deletingRole = ActionResult.Loading }
                    , Cmd.map cfg.wrapMsg <|
                        RolesApi.deleteRole appState role.uuid DeleteRoleCompleted
                    )

                _ ->
                    ( model, Cmd.none )

        DeleteRoleCompleted result ->
            case result of
                Ok _ ->
                    ( { model
                        | roleToBeDeleted = Nothing
                        , roles = ActionResult.Loading
                      }
                    , RolesApi.getRoles appState (cfg.wrapMsg << GetRolesCompleted)
                    )

                Err error ->
                    ( { model | deletingRole = ApiError.toActionResult appState (gettext "Role could not be deleted." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )
