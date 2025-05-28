module Wizard.Projects.Detail.Components.ShareModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , openMsg
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Form.Field as Field
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, h5, hr, span, strong, text)
import Html.Attributes exposing (class, classList, title)
import Html.Events exposing (onClick, onMouseOut)
import Html.Extra as Html
import List.Extra as List
import Random exposing (Seed)
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Api.UserGroups as UserGroupsApi
import Shared.Api.Users as UsersApi
import Shared.Components.Badge as Badge
import Shared.Copy as Copy
import Shared.Data.BootstrapConfig.Admin as Admin
import Shared.Data.Member as Member
import Shared.Data.Permission exposing (Permission)
import Shared.Data.QuestionnaireCommon exposing (QuestionnaireCommon)
import Shared.Data.QuestionnairePermission as QuestionnairePermission
import Shared.Data.User as User
import Shared.Data.UserGroupSuggestion exposing (UserGroupSuggestion)
import Shared.Data.UserSuggestion exposing (UserSuggestion)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Utils exposing (getUuid, withNoCmd, withSeed)
import Shortcut
import String.Format as String
import Time
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintInput
import Wizard.Common.Driver as Driver exposing (TourConfig)
import Wizard.Common.GuideLinks as GuideLinks
import Wizard.Common.Html exposing (guideLink)
import Wizard.Common.Html.Attribute exposing (dataCy, dataTour, selectDataTour, tooltip)
import Wizard.Common.TourId as TourId
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.MemberIcon as MemberIcon
import Wizard.Projects.Common.QuestionnaireShareForm as QuestionnaireShareForm exposing (QuestionnaireShareForm)
import Wizard.Projects.Common.QuestionnaireShareFormMemberPermType as QuestionnaireEditFormMemberPerms
import Wizard.Projects.Common.QuestionnaireShareFormMemberType as QuestionnaireShareFormMemberType exposing (QuestionnaireShareFormMemberType(..))
import Wizard.Projects.Detail.ProjectDetailRoute as ProjectDetailRoute
import Wizard.Projects.Routes as ProjectsRoutes
import Wizard.Routes as Routes
import Wizard.Routing as Routing



-- MODEL


type alias Model =
    { visible : Bool
    , savingSharing : ActionResult String
    , lastSavingSharing : Time.Posix
    , questionnaireEditForm : Form FormError QuestionnaireShareForm
    , questionnaireUuid : Uuid
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
    , lastSavingSharing = Time.millisToPosix 0
    , questionnaireEditForm = QuestionnaireShareForm.initEmpty
    , questionnaireUuid = Uuid.nil
    , userTypeHintInputModel = TypeHintInput.init "memberId"
    , userGroupTypeHintInputModel = TypeHintInput.init "userGroupUuid"
    , users = []
    , userGroups = []
    , copiedLink = False
    }


setQuestionnaire : QuestionnaireCommon -> Model -> Model
setQuestionnaire questionnaire model =
    { model
        | questionnaireEditForm = QuestionnaireShareForm.init questionnaire
        , questionnaireUuid = questionnaire.uuid
        , users = List.filterMap (.member >> Member.toUserSuggestion) questionnaire.permissions
        , userGroups = List.filterMap (.member >> Member.toUserGroupSuggestion) questionnaire.permissions
    }



-- UPDATE


type Msg
    = Open QuestionnaireCommon
    | Close
    | UserTypeHintInputMsg (TypeHintInput.Msg UserSuggestion)
    | UserGroupTypeHintInputMsg (TypeHintInput.Msg UserGroupSuggestion)
    | AddUser UserSuggestion
    | AddUserGroup UserGroupSuggestion
    | FormMsg Form.Msg
    | PutQuestionnaireShareComplete Time.Posix (Result ApiError ())
    | CopyLink String
    | ClearCopiedLink


openMsg : QuestionnaireCommon -> Msg
openMsg =
    Open


tour : AppState -> TourConfig
tour appState =
    Driver.tourConfig TourId.projectsDetailShareModal appState
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
    , questionnaireUuid : Uuid
    , permissions : List Permission
    }


update : UpdateConfig msg -> Msg -> AppState -> Model -> ( Seed, Model, Cmd msg )
update cfg msg appState model =
    case msg of
        Open questionnaire ->
            ( appState.seed
            , setQuestionnaire questionnaire { model | visible = True }
            , Driver.init (tour appState)
            )

        Close ->
            { model | visible = False }
                |> withNoCmd
                |> withSeed appState.seed

        UserTypeHintInputMsg typeHintInputMsg ->
            handleUserTypeHintInputMsg cfg typeHintInputMsg appState model
                |> withSeed appState.seed

        UserGroupTypeHintInputMsg typeHintInputMsg ->
            handleUserGroupTypeHintInputMsg cfg typeHintInputMsg appState model
                |> withSeed appState.seed

        AddUser user ->
            handleAddUser appState cfg model user

        AddUserGroup userGroup ->
            handleAddUserGroup appState cfg model userGroup

        FormMsg formMsg ->
            handleFormMsg cfg formMsg appState model
                |> withSeed appState.seed

        PutQuestionnaireShareComplete time result ->
            handlePutQuestionnaireComplete appState model time result
                |> withNoCmd
                |> withSeed appState.seed

        CopyLink link ->
            ( { model | copiedLink = True }, Copy.copyToClipboard link )
                |> withSeed appState.seed

        ClearCopiedLink ->
            { model | copiedLink = False }
                |> withNoCmd
                |> withSeed appState.seed


handleUserTypeHintInputMsg : UpdateConfig msg -> TypeHintInput.Msg UserSuggestion -> AppState -> Model -> ( Model, Cmd msg )
handleUserTypeHintInputMsg cfg typeHintInputMsg appState model =
    let
        projectMemberUuids =
            QuestionnaireShareForm.getMemberUuids model.questionnaireEditForm

        filterResults userSuggestion =
            not <| List.member (Uuid.toString userSuggestion.uuid) projectMemberUuids

        typeHintInputCfg =
            { wrapMsg = cfg.wrapMsg << UserTypeHintInputMsg
            , getTypeHints = UsersApi.getUsersSuggestions
            , getError = gettext "Unable to get users." appState.locale
            , setReply = cfg.wrapMsg << AddUser
            , clearReply = Nothing
            , filterResults = Just filterResults
            }

        ( userTypeHintInputModel, cmd ) =
            TypeHintInput.update typeHintInputCfg typeHintInputMsg appState model.userTypeHintInputModel
    in
    ( { model | userTypeHintInputModel = userTypeHintInputModel }, cmd )


handleUserGroupTypeHintInputMsg : UpdateConfig msg -> TypeHintInput.Msg UserGroupSuggestion -> AppState -> Model -> ( Model, Cmd msg )
handleUserGroupTypeHintInputMsg cfg typeHintInputMsg appState model =
    let
        projectMemberUuids =
            QuestionnaireShareForm.getMemberUuids model.questionnaireEditForm

        filterResults userGroupSuggestion =
            not <| List.member (Uuid.toString userGroupSuggestion.uuid) projectMemberUuids

        typeHintInputCfg =
            { wrapMsg = cfg.wrapMsg << UserGroupTypeHintInputMsg
            , getTypeHints = UserGroupsApi.getUserGroupsSuggestions
            , getError = gettext "Unable to get user groups." appState.locale
            , setReply = cfg.wrapMsg << AddUserGroup
            , clearReply = Nothing
            , filterResults = Just filterResults
            }

        ( userGroupTypeHintInputModel, cmd ) =
            TypeHintInput.update typeHintInputCfg typeHintInputMsg appState model.userGroupTypeHintInputModel
    in
    ( { model | userGroupTypeHintInputModel = userGroupTypeHintInputModel }, cmd )


handleAddUser : AppState -> UpdateConfig msg -> Model -> UserSuggestion -> ( Seed, Model, Cmd msg )
handleAddUser appState cfg model user =
    let
        userTypeHintInputModel =
            TypeHintInput.clear model.userTypeHintInputModel

        permissionsLength =
            List.length <| Form.getListIndexes "permissions" model.questionnaireEditForm

        formUpdate =
            Form.update QuestionnaireShareForm.validation

        createInputMessage field value =
            Form.Input field Form.Text (Field.String value)

        ( newUuid, newSeed ) =
            getUuid appState.seed

        msgs =
            [ Form.Append "permissions"
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".uuid") (Uuid.toString newUuid)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".memberUuid") (Uuid.toString user.uuid)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".memberType") (QuestionnaireShareFormMemberType.toString UserQuestionnairePermType)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".perms") (QuestionnaireEditFormMemberPerms.toString QuestionnaireEditFormMemberPerms.Viewer)
            ]

        newForm =
            List.foldl formUpdate model.questionnaireEditForm msgs

        newModel =
            { model
                | userTypeHintInputModel = userTypeHintInputModel
                , questionnaireEditForm = newForm
                , users = List.uniqueBy (Uuid.toString << .uuid) (user :: model.users)
            }
    in
    newModel
        |> saveSharing appState cfg
        |> withSeed newSeed


handleAddUserGroup : AppState -> UpdateConfig msg -> Model -> UserGroupSuggestion -> ( Seed, Model, Cmd msg )
handleAddUserGroup appState cfg model userGroup =
    let
        userGroupTypeHintInputModel =
            TypeHintInput.clear model.userGroupTypeHintInputModel

        permissionsLength =
            List.length <| Form.getListIndexes "permissions" model.questionnaireEditForm

        formUpdate =
            Form.update QuestionnaireShareForm.validation

        createInputMessage field value =
            Form.Input field Form.Text (Field.String value)

        ( newUuid, newSeed ) =
            getUuid appState.seed

        msgs =
            [ Form.Append "permissions"
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".uuid") (Uuid.toString newUuid)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".memberUuid") (Uuid.toString userGroup.uuid)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".memberType") (QuestionnaireShareFormMemberType.toString UserGroupQuestionnairePermType)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".perms") (QuestionnaireEditFormMemberPerms.toString QuestionnaireEditFormMemberPerms.Viewer)
            ]

        newForm =
            List.foldl formUpdate model.questionnaireEditForm msgs

        newModel =
            { model
                | userGroupTypeHintInputModel = userGroupTypeHintInputModel
                , questionnaireEditForm = newForm
                , userGroups = List.uniqueBy (Uuid.toString << .uuid) (userGroup :: model.userGroups)
            }
    in
    newModel
        |> saveSharing appState cfg
        |> withSeed newSeed


handleFormMsg : UpdateConfig msg -> Form.Msg -> AppState -> Model -> ( Model, Cmd msg )
handleFormMsg cfg formMsg appState model =
    let
        newModel =
            { model | questionnaireEditForm = Form.update QuestionnaireShareForm.validation formMsg model.questionnaireEditForm }

        shouldSave =
            case formMsg of
                Form.Input _ _ _ ->
                    True

                Form.RemoveItem _ _ ->
                    True

                _ ->
                    False
    in
    if shouldSave then
        saveSharing appState cfg newModel

    else
        ( newModel, Cmd.none )


handlePutQuestionnaireComplete : AppState -> Model -> Time.Posix -> Result ApiError () -> Model
handlePutQuestionnaireComplete appState model time result =
    case result of
        Ok _ ->
            if model.lastSavingSharing == time then
                { model | savingSharing = Unset }

            else
                model

        Err error ->
            { model | savingSharing = ApiError.toActionResult appState (gettext "Questionnaire could not be saved." appState.locale) error }


saveSharing : AppState -> UpdateConfig msg -> Model -> ( Model, Cmd msg )
saveSharing appState cfg model =
    case Form.getOutput model.questionnaireEditForm of
        Just form ->
            let
                body =
                    QuestionnaireShareForm.encode form
            in
            ( { model
                | savingSharing = Loading
                , lastSavingSharing = appState.currentTime
              }
            , QuestionnairesApi.putQuestionnaireShare cfg.questionnaireUuid body appState (cfg.wrapMsg << PutQuestionnaireShareComplete appState.currentTime)
            )

        Nothing ->
            ( model, Cmd.none )



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
            [ FormResult.view appState model.savingSharing
            , Html.viewIf (Admin.isEnabled appState.config.admin) <| userGroupsView appState model
            , usersView appState model
            , formView appState model.questionnaireEditForm
            ]

        shortcuts =
            if not model.visible || ActionResult.isLoading model.savingSharing then
                []

            else
                [ Shortcut.simpleShortcut Shortcut.Enter Close
                , Shortcut.simpleShortcut Shortcut.Escape Close
                ]
    in
    Shortcut.shortcutElement shortcuts
        [ class "modal modal-cover", classList [ ( "visible", model.visible ) ] ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content", dataCy "modal_project-share" ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ] [ text (gettext "Share Project" appState.locale) ]
                    , guideLink appState GuideLinks.projectsSharing
                    ]
                , div [ class "modal-body" ] modalContent
                , div [ class "modal-footer" ]
                    [ ActionButton.buttonWithAttrs appState
                        { label = gettext "Done" appState.locale
                        , result = model.savingSharing
                        , msg = Close
                        , dangerous = False
                        , attrs = [ dataCy "modal_action-button" ]
                        }
                    , copyLinkButton appState model
                    ]
                ]
            ]
        ]


copyLinkButton : AppState -> Model -> Html Msg
copyLinkButton appState model =
    let
        copyLinkTooltip =
            if model.copiedLink then
                tooltip (gettext "Copied!" appState.locale)

            else
                []

        publicLink =
            appState.clientUrl ++ String.replace "/wizard" "" (Routing.toUrl appState (Routes.ProjectsRoute (ProjectsRoutes.DetailRoute model.questionnaireUuid (ProjectDetailRoute.Questionnaire Nothing Nothing))))

        copyLinkIcon =
            if model.copiedLink then
                faSet "questionnaire.copyLinkCopied" appState

            else
                faSet "questionnaire.copyLink" appState
    in
    button
        (class "btn btn-outline-primary with-icon"
            :: onClick (CopyLink publicLink)
            :: onMouseOut ClearCopiedLink
            :: copyLinkTooltip
        )
        [ copyLinkIcon
        , text (gettext "Copy link" appState.locale)
        ]


userGroupsView : AppState -> Model -> Html Msg
userGroupsView appState model =
    let
        userGroupTypeHintInputCfg =
            { viewItem = TypeHintInput.userGroupSuggestion appState
            , wrapMsg = UserGroupTypeHintInputMsg
            , nothingSelectedItem = span [ class "text-muted" ] [ text <| gettext "Add user group" appState.locale ]
            , clearEnabled = False
            }

        userGroupTypeHintInput =
            TypeHintInput.view appState userGroupTypeHintInputCfg model.userGroupTypeHintInputModel False
    in
    div [ class "ShareModal__Users" ]
        [ div []
            [ strong [] [ text (gettext "User Groups" appState.locale) ]
            , userGroupTypeHintInput
            ]
        , Html.map FormMsg <| FormGroup.viewList appState (userGroupView appState model.userGroups) model.questionnaireEditForm "permissions" ""
        , hr [] []
        ]


userGroupView : AppState -> List UserGroupSuggestion -> Form FormError QuestionnaireShareForm -> Int -> Html Form.Msg
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
                    FormExtra.inlineSelect roleOptions form ("permissions." ++ String.fromInt i ++ ".perms")

                privateBadge =
                    if userGroup.private then
                        Badge.dark [ class "ms-2" ] [ text (gettext "private" appState.locale) ]

                    else
                        emptyNode
            in
            div [ class "user-row" ]
                [ div []
                    [ MemberIcon.viewCustom { text = userGroup.name, image = Nothing }
                    , text userGroup.name
                    , privateBadge
                    ]
                , div []
                    [ roleSelect
                    , a
                        [ class "text-danger"
                        , onClick (Form.RemoveItem "permissions" i)
                        , title (gettext "Remove" appState.locale)
                        ]
                        [ faSet "_global.remove" appState ]
                    ]
                ]

        Nothing ->
            emptyNode


usersView : AppState -> Model -> Html Msg
usersView appState model =
    let
        userTypeHintInputCfg =
            { viewItem = TypeHintInput.memberSuggestion
            , wrapMsg = UserTypeHintInputMsg
            , nothingSelectedItem = span [ class "text-muted" ] [ text <| gettext "Add users" appState.locale ]
            , clearEnabled = False
            }

        userTypeHintInput =
            TypeHintInput.view appState userTypeHintInputCfg model.userTypeHintInputModel False

        separator =
            if appState.config.questionnaire.questionnaireVisibility.enabled || appState.config.questionnaire.questionnaireSharing.enabled then
                hr [] []

            else
                emptyNode
    in
    div [ class "ShareModal__Users", dataTour "project-detail_share-modal_users" ]
        [ div [ class "mt-2" ]
            [ strong [] [ text (gettext "Users" appState.locale) ]
            , userTypeHintInput
            ]
        , Html.map FormMsg <| FormGroup.viewList appState (userView appState model.users) model.questionnaireEditForm "permissions" ""
        , separator
        ]


userView : AppState -> List UserSuggestion -> Form FormError QuestionnaireShareForm -> Int -> Html Form.Msg
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
                roleOptions =
                    QuestionnaireEditFormMemberPerms.formOptions appState

                roleSelect =
                    FormExtra.inlineSelect roleOptions form ("permissions." ++ String.fromInt i ++ ".perms")
            in
            div [ class "user-row" ]
                [ div []
                    [ MemberIcon.viewCustom { text = User.fullName user, image = Just (User.imageUrlOrGravatar user) }
                    , text <| User.fullName user
                    ]
                , div []
                    [ roleSelect
                    , a
                        [ class "text-danger"
                        , onClick (Form.RemoveItem "permissions" i)
                        , title (gettext "Remove" appState.locale)
                        ]
                        [ faSet "_global.remove" appState ]
                    ]
                ]

        Nothing ->
            emptyNode


formView : AppState -> Form FormError QuestionnaireShareForm -> Html Msg
formView appState form =
    let
        visibilityInputs =
            if appState.config.questionnaire.questionnaireVisibility.enabled then
                let
                    visibilitySelect =
                        let
                            sharingPermission =
                                (Form.getFieldAsString "sharingPermission" form).value
                        in
                        FormExtra.inlineSelect (QuestionnairePermission.formOptions appState sharingPermission) form "visibilityPermission"

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
            if appState.config.questionnaire.questionnaireSharing.enabled then
                let
                    sharingEnabled =
                        Maybe.withDefault False (Form.getFieldAsBool "sharingEnabled" form).value

                    sharingSelect =
                        FormExtra.inlineSelect (QuestionnairePermission.formOptions appState Nothing) form "sharingPermission"

                    sharingPermissionInput =
                        div
                            [ class "form-group form-group-toggle-extra"
                            , classList [ ( "visible", sharingEnabled ) ]
                            ]
                            (String.formatHtml
                                (gettext "Anyone with the link can %s the project." appState.locale)
                                [ sharingSelect ]
                            )

                    sharingEnabledInput =
                        FormGroup.toggle form "sharingEnabled" (gettext "Public link" appState.locale)
                in
                [ Html.map FormMsg sharingEnabledInput
                , Html.map FormMsg sharingPermissionInput
                ]

            else
                []
    in
    div [ dataTour "project-detail_share-modal_permissions" ]
        (visibilityInputs ++ sharingInputs)
