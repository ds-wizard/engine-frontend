module Wizard.Pages.Projects.Detail.Components.ShareModal exposing
    ( Model
    , Msg
    , ShareData
    , UpdateConfig
    , init
    , openMsg
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Components.ActionButton as ActionButton
import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (faQuestionnaireCopyLinkCopiedFw, faQuestionnaireCopyLinkFw, faRemove, faRemoveFw, fas)
import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Components.FormResult as FormResult
import Common.Components.GuideLink as GuideLink
import Common.Components.Tooltip exposing (tooltipLeft)
import Common.Components.TypeHintInput as TypeHintInput
import Common.Ports.Copy as Copy
import Common.Utils.CmdUtils exposing (withNoCmd)
import Common.Utils.Driver as Driver exposing (TourConfig)
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field as Field
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, h5, hr, span, strong, text)
import Html.Attributes exposing (class, classList, disabled, title)
import Html.Attributes.Extensions exposing (dataCy, dataTour, selectDataTour)
import Html.Events exposing (onClick, onMouseOut)
import Html.Extra as Html
import List.Extra as List
import Random exposing (Seed)
import Shortcut
import String.Format as String
import Task.Extra as Task
import Tuple.Extensions as Tuple
import Uuid exposing (Uuid)
import Uuid.Extra as Uuid
import Wizard.Api.Models.BootstrapConfig.AdminConfig as Admin
import Wizard.Api.Models.Member as Member
import Wizard.Api.Models.Permission exposing (Permission)
import Wizard.Api.Models.Project.ProjectSharing as ProjectSharing exposing (ProjectSharing)
import Wizard.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility)
import Wizard.Api.Models.ProjectCommon exposing (ProjectCommon)
import Wizard.Api.Models.ProjectPermission as ProjectPermission
import Wizard.Api.Models.User as User
import Wizard.Api.Models.UserGroupSuggestion exposing (UserGroupSuggestion)
import Wizard.Api.Projects as ProjectsApi
import Wizard.Api.UserGroups as UserGroupsApi
import Wizard.Api.Users as UsersApi
import Wizard.Components.MemberIcon as MemberIcon
import Wizard.Components.TypeHintInput.TypeHintInputItem as TypeHintInput
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Projects.Common.ProjectShareForm as ProjectShareForm exposing (ProjectShareForm)
import Wizard.Pages.Projects.Common.ProjectShareFormMemberPermType as QuestionnaireEditFormMemberPerms
import Wizard.Pages.Projects.Common.ProjectShareFormMemberType as ProjectShareFormMemberType exposing (ProjectShareFormMemberType(..))
import Wizard.Pages.Projects.Detail.ProjectDetailRoute as ProjectDetailRoute
import Wizard.Pages.Projects.Routes as ProjectsRoutes
import Wizard.Routes as Routes
import Wizard.Routing as Routing
import Wizard.Utils.Driver as Driver
import Wizard.Utils.TourId as TourId
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks



-- MODEL


type alias Model =
    { visible : Bool
    , savingSharing : ActionResult String
    , projectShareForm : Form FormError ProjectShareForm
    , projectUuid : Uuid
    , userTypeHintInputModel : TypeHintInput.Model UserSuggestion
    , userGroupTypeHintInputModel : TypeHintInput.Model UserGroupSuggestion
    , users : List UserSuggestion
    , userGroups : List UserGroupSuggestion
    , copiedLink : Bool
    }


init : Model
init =
    { visible = False
    , savingSharing = Unset
    , projectShareForm = ProjectShareForm.initEmpty
    , projectUuid = Uuid.nil
    , userTypeHintInputModel = TypeHintInput.init "memberId"
    , userGroupTypeHintInputModel = TypeHintInput.init "userGroupUuid"
    , users = []
    , userGroups = []
    , copiedLink = False
    }


setProject : ProjectCommon -> Model -> Model
setProject project model =
    { model
        | projectShareForm = ProjectShareForm.init project
        , projectUuid = project.uuid
        , users = List.filterMap (.member >> Member.toUserSuggestion) project.permissions
        , userGroups = List.filterMap (.member >> Member.toUserGroupSuggestion) project.permissions
    }



-- UPDATE


type Msg
    = Open ProjectCommon
    | Close
    | Save
    | UserTypeHintInputMsg (TypeHintInput.Msg UserSuggestion)
    | UserGroupTypeHintInputMsg (TypeHintInput.Msg UserGroupSuggestion)
    | AddUser UserSuggestion
    | AddUserGroup UserGroupSuggestion
    | FormMsg Form.Msg
    | PutQuestionnaireShareComplete ShareData (Result ApiError ())
    | CopyLink String
    | ClearCopiedLink


openMsg : ProjectCommon -> Msg
openMsg =
    Open


tour : AppState -> TourConfig
tour appState =
    Driver.fromAppState TourId.projectsDetailShareModal appState
        |> Driver.addModalDelay
        |> Driver.addStep
            { element = selectDataTour "project-detail_share-modal_users"
            , popover =
                { title = gettext "Users" appState.locale
                , description = gettext "Invite specific people to your project. They need to have an existing account." appState.locale
                }
            }
        |> Driver.addStep
            { element = selectDataTour "project-detail_share-modal_permissions"
            , popover =
                { title = gettext "Permissions" appState.locale
                , description = gettext "Choose to share your project with all logged-in users or anyone with the link." appState.locale
                }
            }


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , projectUuid : Uuid
    , onSaveMsg : ShareData -> msg
    , onCloseMsg : msg
    }


type alias ShareData =
    { permissions : List Permission
    , sharing : ProjectSharing
    , visibility : ProjectVisibility
    }


update : UpdateConfig msg -> Msg -> AppState -> Model -> ( Seed, Model, Cmd msg )
update cfg msg appState model =
    case msg of
        Open project ->
            ( appState.seed
            , setProject project { model | visible = True }
            , Driver.init (tour appState)
            )

        Close ->
            closeModal cfg model
                |> Tuple.prepend appState.seed

        Save ->
            let
                newModel =
                    { model | projectShareForm = Form.update ProjectShareForm.validation Form.Submit model.projectShareForm }
            in
            saveSharing appState cfg newModel
                |> Tuple.prepend appState.seed

        UserTypeHintInputMsg typeHintInputMsg ->
            handleUserTypeHintInputMsg cfg typeHintInputMsg appState model
                |> Tuple.prepend appState.seed

        UserGroupTypeHintInputMsg typeHintInputMsg ->
            handleUserGroupTypeHintInputMsg cfg typeHintInputMsg appState model
                |> Tuple.prepend appState.seed

        AddUser user ->
            handleAddUser appState model user

        AddUserGroup userGroup ->
            handleAddUserGroup appState model userGroup

        FormMsg formMsg ->
            handleFormMsg formMsg model
                |> Tuple.prepend appState.seed

        PutQuestionnaireShareComplete shareData result ->
            handlePutQuestionnaireComplete appState cfg model shareData result
                |> Tuple.prepend appState.seed

        CopyLink link ->
            ( { model | copiedLink = True }, Copy.copyToClipboard link )
                |> Tuple.prepend appState.seed

        ClearCopiedLink ->
            { model | copiedLink = False }
                |> withNoCmd
                |> Tuple.prepend appState.seed


handleUserTypeHintInputMsg : UpdateConfig msg -> TypeHintInput.Msg UserSuggestion -> AppState -> Model -> ( Model, Cmd msg )
handleUserTypeHintInputMsg cfg typeHintInputMsg appState model =
    let
        projectMemberUuids =
            ProjectShareForm.getMemberUuids model.projectShareForm

        filterResults userSuggestion =
            not <| List.member (Uuid.toString userSuggestion.uuid) projectMemberUuids

        typeHintInputCfg =
            { wrapMsg = cfg.wrapMsg << UserTypeHintInputMsg
            , getTypeHints = UsersApi.getUsersSuggestions appState
            , getError = gettext "Unable to get users." appState.locale
            , setReply = cfg.wrapMsg << AddUser
            , clearReply = Nothing
            , filterResults = Just filterResults
            }

        ( userTypeHintInputModel, cmd ) =
            TypeHintInput.update typeHintInputCfg typeHintInputMsg model.userTypeHintInputModel
    in
    ( { model | userTypeHintInputModel = userTypeHintInputModel }, cmd )


handleUserGroupTypeHintInputMsg : UpdateConfig msg -> TypeHintInput.Msg UserGroupSuggestion -> AppState -> Model -> ( Model, Cmd msg )
handleUserGroupTypeHintInputMsg cfg typeHintInputMsg appState model =
    let
        projectMemberUuids =
            ProjectShareForm.getMemberUuids model.projectShareForm

        filterResults userGroupSuggestion =
            not <| List.member (Uuid.toString userGroupSuggestion.uuid) projectMemberUuids

        typeHintInputCfg =
            { wrapMsg = cfg.wrapMsg << UserGroupTypeHintInputMsg
            , getTypeHints = UserGroupsApi.getUserGroupsSuggestions appState
            , getError = gettext "Unable to get user groups." appState.locale
            , setReply = cfg.wrapMsg << AddUserGroup
            , clearReply = Nothing
            , filterResults = Just filterResults
            }

        ( userGroupTypeHintInputModel, cmd ) =
            TypeHintInput.update typeHintInputCfg typeHintInputMsg model.userGroupTypeHintInputModel
    in
    ( { model | userGroupTypeHintInputModel = userGroupTypeHintInputModel }, cmd )


handleAddUser : AppState -> Model -> UserSuggestion -> ( Seed, Model, Cmd msg )
handleAddUser appState model user =
    let
        userTypeHintInputModel =
            TypeHintInput.clear model.userTypeHintInputModel

        permissionsLength =
            List.length <| Form.getListIndexes "permissions" model.projectShareForm

        formUpdate =
            Form.update ProjectShareForm.validation

        createInputMessage field value =
            Form.Input field Form.Text (Field.String value)

        ( newUuid, newSeed ) =
            Uuid.step appState.seed

        msgs =
            [ Form.Append "permissions"
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".uuid") (Uuid.toString newUuid)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".memberUuid") (Uuid.toString user.uuid)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".memberType") (ProjectShareFormMemberType.toString UserProjectPermType)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".perms") (QuestionnaireEditFormMemberPerms.toString QuestionnaireEditFormMemberPerms.Viewer)
            ]

        newForm =
            List.foldl formUpdate model.projectShareForm msgs

        newModel =
            { model
                | userTypeHintInputModel = userTypeHintInputModel
                , projectShareForm = newForm
                , users = List.uniqueBy (Uuid.toString << .uuid) (user :: model.users)
            }
    in
    newModel
        |> withNoCmd
        |> Tuple.prepend newSeed


handleAddUserGroup : AppState -> Model -> UserGroupSuggestion -> ( Seed, Model, Cmd msg )
handleAddUserGroup appState model userGroup =
    let
        userGroupTypeHintInputModel =
            TypeHintInput.clear model.userGroupTypeHintInputModel

        permissionsLength =
            List.length <| Form.getListIndexes "permissions" model.projectShareForm

        formUpdate =
            Form.update ProjectShareForm.validation

        createInputMessage field value =
            Form.Input field Form.Text (Field.String value)

        ( newUuid, newSeed ) =
            Uuid.step appState.seed

        msgs =
            [ Form.Append "permissions"
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".uuid") (Uuid.toString newUuid)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".memberUuid") (Uuid.toString userGroup.uuid)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".memberType") (ProjectShareFormMemberType.toString UserGroupProjectPermType)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".perms") (QuestionnaireEditFormMemberPerms.toString QuestionnaireEditFormMemberPerms.Viewer)
            ]

        newForm =
            List.foldl formUpdate model.projectShareForm msgs

        newModel =
            { model
                | userGroupTypeHintInputModel = userGroupTypeHintInputModel
                , projectShareForm = newForm
                , userGroups = List.uniqueBy (Uuid.toString << .uuid) (userGroup :: model.userGroups)
            }
    in
    newModel
        |> withNoCmd
        |> Tuple.prepend newSeed


handleFormMsg : Form.Msg -> Model -> ( Model, Cmd msg )
handleFormMsg formMsg model =
    { model | projectShareForm = Form.update ProjectShareForm.validation formMsg model.projectShareForm }
        |> withNoCmd


handlePutQuestionnaireComplete : AppState -> UpdateConfig msg -> Model -> ShareData -> Result ApiError () -> ( Model, Cmd msg )
handlePutQuestionnaireComplete appState cfg model shareData result =
    case result of
        Ok _ ->
            let
                ( newModel, closeCmd ) =
                    closeModal cfg model
            in
            ( newModel, Cmd.batch [ Task.dispatch (cfg.onSaveMsg shareData), closeCmd ] )

        Err error ->
            { model | savingSharing = ApiError.toActionResult appState (gettext "Questionnaire could not be saved." appState.locale) error }
                |> withNoCmd


saveSharing : AppState -> UpdateConfig msg -> Model -> ( Model, Cmd msg )
saveSharing appState cfg model =
    case Form.getOutput model.projectShareForm of
        Just form ->
            let
                body =
                    ProjectShareForm.encode form
            in
            ( { model | savingSharing = Loading }
            , ProjectsApi.putShare appState cfg.projectUuid body (cfg.wrapMsg << PutQuestionnaireShareComplete (toShareData model form))
            )

        Nothing ->
            ( model, Cmd.none )


toShareData : Model -> ProjectShareForm -> ShareData
toShareData model form =
    let
        toMember formPermission =
            case formPermission.memberType of
                UserProjectPermType ->
                    Maybe.map Member.userMember <|
                        List.find (.uuid >> (==) formPermission.memberUuid) model.users

                UserGroupProjectPermType ->
                    Maybe.map Member.userGroupMember <|
                        List.find (.uuid >> (==) formPermission.memberUuid) model.userGroups

        toPermission formPermission =
            Maybe.map
                (\member ->
                    { member = member
                    , perms = QuestionnaireEditFormMemberPerms.toPerms formPermission.perms
                    }
                )
                (toMember formPermission)
    in
    { permissions = List.filterMap toPermission form.permissions
    , sharing = ProjectSharing.fromFormValues form.sharingEnabled form.sharingPermission
    , visibility = ProjectVisibility.fromFormValues form.visibilityEnabled form.visibilityPermission form.sharingEnabled form.sharingPermission
    }


closeModal : UpdateConfig msg -> Model -> ( Model, Cmd msg )
closeModal cfg model =
    ( { model | visible = False, savingSharing = Unset, copiedLink = False }
    , Task.dispatch cfg.onCloseMsg
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        userTypeHintInputSub =
            Sub.map UserTypeHintInputMsg <|
                TypeHintInput.subscriptions model.userTypeHintInputModel

        userGroupTypeHintInputSub =
            Sub.map UserGroupTypeHintInputMsg <|
                TypeHintInput.subscriptions model.userGroupTypeHintInputModel
    in
    Sub.batch [ userTypeHintInputSub, userGroupTypeHintInputSub ]



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    let
        modalContent =
            [ FormResult.view model.savingSharing
            , Html.viewIf (Admin.isEnabled appState.config.admin) <| userGroupsView appState model
            , usersView appState model
            , formView appState model
            ]

        shortcuts =
            if not model.visible || ActionResult.isLoading model.savingSharing then
                []

            else
                [ Shortcut.simpleShortcut Shortcut.Enter Save
                , Shortcut.simpleShortcut Shortcut.Escape Close
                ]
    in
    Shortcut.shortcutElement shortcuts
        [ class "modal modal-cover", classList [ ( "visible", model.visible ) ] ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content", dataCy "modal_project-share" ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ] [ text (gettext "Share Project" appState.locale) ]
                    , GuideLink.guideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.projectsSharing)
                    ]
                , div [ class "modal-body" ] modalContent
                , div [ class "modal-footer" ]
                    [ ActionButton.buttonWithAttrs
                        { label = gettext "Save" appState.locale
                        , result = model.savingSharing
                        , msg = Save
                        , dangerous = False
                        , attrs = [ dataCy "modal_action-button" ]
                        }
                    , button
                        [ onClick Close
                        , disabled (ActionResult.isLoading model.savingSharing)
                        , class "btn btn-secondary"
                        , dataCy "modal_cancel-button"
                        ]
                        [ text (gettext "Cancel" appState.locale) ]
                    ]
                ]
            ]
        ]


copyLinkButton : AppState -> Model -> Html Msg
copyLinkButton appState model =
    let
        publicLink =
            appState.clientUrl ++ String.replace "/wizard" "" (Routing.toUrl (Routes.ProjectsRoute (ProjectsRoutes.DetailRoute model.projectUuid (ProjectDetailRoute.Questionnaire Nothing Nothing))))

        ( copyLinkIcon, copyLinkLabel ) =
            if model.copiedLink then
                ( faQuestionnaireCopyLinkCopiedFw, gettext "Copied" appState.locale )

            else
                ( faQuestionnaireCopyLinkFw, gettext "Copy link" appState.locale )
    in
    div []
        [ button
            [ class "btn btn-link btn-sm with-icon px-0"
            , onClick (CopyLink publicLink)
            , onMouseOut ClearCopiedLink
            ]
            [ copyLinkIcon
            , text copyLinkLabel
            ]
        ]


userGroupsView : AppState -> Model -> Html Msg
userGroupsView appState model =
    let
        userGroupTypeHintInputCfg =
            { viewItem = TypeHintInput.userGroupSuggestion appState
            , wrapMsg = UserGroupTypeHintInputMsg
            , nothingSelectedItem = span [ class "text-muted" ] [ text <| gettext "Add user group" appState.locale ]
            , clearEnabled = False
            , locale = appState.locale
            }

        userGroupTypeHintInput =
            TypeHintInput.view userGroupTypeHintInputCfg model.userGroupTypeHintInputModel False
    in
    div [ class "ShareModal__Users" ]
        [ div []
            [ strong [] [ text (gettext "User Groups" appState.locale) ]
            , userGroupTypeHintInput
            ]
        , Html.map FormMsg <| FormGroup.viewList appState.locale (userGroupView appState model.userGroups) model.projectShareForm "permissions" ""
        , hr [] []
        ]


userGroupView : AppState -> List UserGroupSuggestion -> Form FormError ProjectShareForm -> Int -> Html Form.Msg
userGroupView appState userGroups form i =
    let
        memberUuid =
            (Form.getFieldAsString ("permissions." ++ String.fromInt i ++ ".memberUuid") form).value

        mbUserGroup =
            List.find (.uuid >> Uuid.toString >> Just >> (==) memberUuid) userGroups
    in
    case mbUserGroup of
        Just userGroup ->
            let
                roleOptions =
                    QuestionnaireEditFormMemberPerms.formOptions appState

                roleSelect =
                    FormExtra.inlineSelect roleOptions form ("permissions." ++ String.fromInt i ++ ".perms") False

                privateBadge =
                    if userGroup.private then
                        Badge.dark [ class "ms-2" ] [ text (gettext "private" appState.locale) ]

                    else
                        Html.nothing
            in
            userRow
                [ div []
                    [ MemberIcon.viewCustom { text = userGroup.name, image = Nothing }
                    , span [ class "user-row-name" ] [ text userGroup.name ]
                    , privateBadge
                    ]
                , div []
                    [ roleSelect
                    , a
                        [ class "text-danger"
                        , onClick (Form.RemoveItem "permissions" i)
                        , title (gettext "Remove" appState.locale)
                        ]
                        [ faRemove ]
                    ]
                ]

        Nothing ->
            Html.nothing


usersView : AppState -> Model -> Html Msg
usersView appState model =
    let
        userTypeHintInputCfg =
            { viewItem = TypeHintInput.memberSuggestion
            , wrapMsg = UserTypeHintInputMsg
            , nothingSelectedItem = span [ class "text-muted" ] [ text <| gettext "Add users" appState.locale ]
            , clearEnabled = False
            , locale = appState.locale
            }

        userTypeHintInput =
            TypeHintInput.view userTypeHintInputCfg model.userTypeHintInputModel False

        separator =
            if appState.config.project.projectVisibility.enabled || appState.config.project.projectSharing.enabled then
                hr [] []

            else
                Html.nothing
    in
    div [ class "ShareModal__Users", dataTour "project-detail_share-modal_users" ]
        [ div [ class "mt-2" ]
            [ strong [] [ text (gettext "Users" appState.locale) ]
            , userTypeHintInput
            ]
        , Html.map FormMsg <| FormGroup.viewList appState.locale (userView appState model.users) model.projectShareForm "permissions" ""
        , separator
        ]


userView : AppState -> List UserSuggestion -> Form FormError ProjectShareForm -> Int -> Html Form.Msg
userView appState users form i =
    let
        memberUuid =
            (Form.getFieldAsString ("permissions." ++ String.fromInt i ++ ".memberUuid") form).value

        mbUser =
            List.find (.uuid >> Uuid.toString >> Just >> (==) memberUuid) users
    in
    case mbUser of
        Just user ->
            let
                isLastOwner =
                    let
                        owners =
                            List.filter
                                (\index ->
                                    (Form.getFieldAsString ("permissions." ++ String.fromInt index ++ ".perms") form).value
                                        == Just (QuestionnaireEditFormMemberPerms.toString QuestionnaireEditFormMemberPerms.Owner)
                                )
                                (Form.getListIndexes "permissions" form)
                    in
                    List.length owners == 1 && List.head owners == Just i

                roleOptions =
                    QuestionnaireEditFormMemberPerms.formOptions appState

                roleSelectWrapper =
                    if isLastOwner then
                        span (tooltipLeft (gettext "Last owner cannot be removed." appState.locale)) << List.singleton

                    else
                        identity

                roleSelect =
                    roleSelectWrapper <| FormExtra.inlineSelect roleOptions form ("permissions." ++ String.fromInt i ++ ".perms") isLastOwner

                removeButton =
                    if isLastOwner then
                        fas "fa-fw"

                    else
                        a
                            [ class "text-danger"
                            , onClick (Form.RemoveItem "permissions" i)
                            , title (gettext "Remove" appState.locale)
                            ]
                            [ faRemoveFw ]

                affiliationBadge =
                    case user.affiliation of
                        Just affiliation ->
                            Badge.badge [ class "d-block px-0 fw-normal text-wrap text-start text-secondary ms-0 me-2" ] [ text affiliation ]

                        Nothing ->
                            Html.nothing
            in
            userRow
                [ div [ class "align-items-start" ]
                    [ MemberIcon.viewCustom { text = User.fullName user, image = Just (User.imageUrlOrGravatar user) }
                    , div []
                        [ span [ class "user-row-name" ] [ text <| User.fullName user ]
                        , affiliationBadge
                        ]
                    ]
                , div []
                    [ roleSelect
                    , removeButton
                    ]
                ]

        Nothing ->
            Html.nothing


userRow : List (Html msg) -> Html msg
userRow =
    div [ class "user-row bg-light rounded" ]


formView : AppState -> Model -> Html Msg
formView appState model =
    let
        form =
            model.projectShareForm

        sharingEnabled =
            Maybe.withDefault False (Form.getFieldAsBool "sharingEnabled" form).value

        visibilityInputs =
            if appState.config.project.projectVisibility.enabled then
                let
                    visibilitySelect =
                        let
                            mbFilterPerm =
                                if sharingEnabled then
                                    (Form.getFieldAsString "sharingPermission" form).value

                                else
                                    Nothing
                        in
                        FormExtra.inlineSelect (ProjectPermission.formOptions appState mbFilterPerm) form "visibilityPermission" False

                    visibilityEnabled =
                        Maybe.withDefault False (Form.getFieldAsBool "visibilityEnabled" form).value

                    visibilityPermissionInput =
                        div
                            [ class "form-group form-group-toggle-extra"
                            , classList [ ( "visible", visibilityEnabled ) ]
                            ]
                            (String.formatHtml
                                (gettext "Other logged-in users can %s the project." appState.locale)
                                [ visibilitySelect ]
                            )

                    visibilityEnabledInput =
                        FormGroup.toggle form "visibilityEnabled" (gettext "Visible by all other logged-in users" appState.locale)
                in
                [ Html.map FormMsg visibilityEnabledInput
                , Html.map FormMsg visibilityPermissionInput
                ]

            else
                []

        sharingInputs =
            if appState.config.project.projectSharing.enabled then
                let
                    sharingSelect =
                        FormExtra.inlineSelect (ProjectPermission.formOptions appState Nothing) form "sharingPermission" False

                    sharingPermissionInput =
                        div
                            [ class "form-group form-group-toggle-extra ShareModal__PublicLink"
                            , classList [ ( "visible", sharingEnabled ) ]
                            ]
                            (List.map (Html.map FormMsg)
                                (String.formatHtml
                                    (gettext "Anyone with the link can %s the project." appState.locale)
                                    [ sharingSelect ]
                                )
                                ++ [ copyLinkButton appState model ]
                            )

                    sharingEnabledInput =
                        FormGroup.toggle form "sharingEnabled" (gettext "Public link" appState.locale)
                in
                [ Html.map FormMsg sharingEnabledInput
                , sharingPermissionInput
                ]

            else
                []
    in
    div [ dataTour "project-detail_share-modal_permissions" ]
        (visibilityInputs ++ sharingInputs)
