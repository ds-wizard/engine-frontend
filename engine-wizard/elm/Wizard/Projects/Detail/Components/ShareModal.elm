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
import Html exposing (Html, a, button, div, hr, input, span, strong, text)
import Html.Attributes exposing (class, classList, id, readonly, title, value)
import Html.Events exposing (onClick)
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
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnairePermission as QuestionnairePermission
import Shared.Data.User as User
import Shared.Data.UserGroupSuggestion exposing (UserGroupSuggestion)
import Shared.Data.UserSuggestion exposing (UserSuggestion)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Utils exposing (getUuid)
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintInput
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.MemberIcon as MemberIcon
import Wizard.Common.View.Modal as Modal
import Wizard.Ports as Ports
import Wizard.Projects.Common.QuestionnaireEditForm as QuestionnaireEditForm exposing (QuestionnaireEditForm)
import Wizard.Projects.Common.QuestionnaireEditFormMemberType as QuestionnaireEditFormMemberType exposing (QuestionnaireEditFormMemberType(..))
import Wizard.Projects.Common.QuestionnaireEditFormQuestionnairePermType as QuestionnaireEditFormMemberPerms
import Wizard.Projects.Detail.ProjectDetailRoute as ProjectDetailRoute
import Wizard.Projects.Routes as Routes
import Wizard.Routes as Routes
import Wizard.Routing as Routing



-- MODEL


type alias Model =
    { visible : Bool
    , savingSharing : ActionResult String
    , questionnaireEditForm : Form FormError QuestionnaireEditForm
    , questionnaireUuid : Uuid
    , userTypeHintInputModel : TypeHintInput.Model UserSuggestion
    , userGroupTypeHintInputModel : TypeHintInput.Model UserGroupSuggestion
    , users : List UserSuggestion
    , userGroups : List UserGroupSuggestion
    }


init : AppState -> Model
init appState =
    { visible = False
    , savingSharing = Unset
    , questionnaireEditForm = QuestionnaireEditForm.initEmpty appState
    , questionnaireUuid = Uuid.nil
    , userTypeHintInputModel = TypeHintInput.init "memberId"
    , userGroupTypeHintInputModel = TypeHintInput.init "userGroupUuid"
    , users = []
    , userGroups = []
    }


setQuestionnaire : AppState -> QuestionnaireDetail -> Model -> Model
setQuestionnaire appState questionnaire model =
    { model
        | questionnaireEditForm = QuestionnaireEditForm.init appState questionnaire
        , questionnaireUuid = questionnaire.uuid
        , users = List.filterMap (.member >> Member.toUserSuggestion) questionnaire.permissions
        , userGroups = List.filterMap (.member >> Member.toUserGroupSuggestion) questionnaire.permissions
    }



-- UPDATE


type Msg
    = Open QuestionnaireDetail
    | Close
    | UserTypeHintInputMsg (TypeHintInput.Msg UserSuggestion)
    | UserGroupTypeHintInputMsg (TypeHintInput.Msg UserGroupSuggestion)
    | AddUser UserSuggestion
    | AddUserGroup UserGroupSuggestion
    | FormMsg Form.Msg
    | PutQuestionnaireComplete (Result ApiError ())
    | CopyPublicLink String


openMsg : QuestionnaireDetail -> Msg
openMsg =
    Open


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , questionnaireUuid : Uuid
    , permissions : List Permission
    }


update : UpdateConfig msg -> Msg -> AppState -> Model -> ( Seed, Model, Cmd msg )
update cfg msg appState model =
    let
        withSeed ( m, c ) =
            ( appState.seed, m, c )
    in
    case msg of
        Open questionnaire ->
            withSeed ( setQuestionnaire appState questionnaire { model | visible = True }, Cmd.none )

        Close ->
            withSeed ( { model | visible = False }, Cmd.none )

        UserTypeHintInputMsg typeHintInputMsg ->
            withSeed <| handleUserTypeHintInputMsg cfg typeHintInputMsg appState model

        UserGroupTypeHintInputMsg typeHintInputMsg ->
            withSeed <| handleUserGroupTypeHintInputMsg cfg typeHintInputMsg appState model

        AddUser user ->
            handleAddUser appState model user

        AddUserGroup userGroup ->
            handleAddUserGroup appState model userGroup

        FormMsg formMsg ->
            withSeed <| handleFormMsg cfg formMsg appState model

        PutQuestionnaireComplete result ->
            withSeed <| handlePutQuestionnaireComplete appState model result

        CopyPublicLink publicLink ->
            withSeed ( model, Copy.copyToClipboard publicLink )


handleUserTypeHintInputMsg : UpdateConfig msg -> TypeHintInput.Msg UserSuggestion -> AppState -> Model -> ( Model, Cmd msg )
handleUserTypeHintInputMsg cfg typeHintInputMsg appState model =
    let
        projectMemberUuids =
            QuestionnaireEditForm.getMemberUuids model.questionnaireEditForm

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
            QuestionnaireEditForm.getMemberUuids model.questionnaireEditForm

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


handleAddUser : AppState -> Model -> UserSuggestion -> ( Seed, Model, Cmd msg )
handleAddUser appState model user =
    let
        userTypeHintInputModel =
            TypeHintInput.clear model.userTypeHintInputModel

        permissionsLength =
            List.length <| Form.getListIndexes "permissions" model.questionnaireEditForm

        formUpdate =
            Form.update (QuestionnaireEditForm.validation appState)

        createInputMessage field value =
            Form.Input field Form.Text (Field.String value)

        ( newUuid, newSeed ) =
            getUuid appState.seed

        msgs =
            [ Form.Append "permissions"
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".uuid") (Uuid.toString newUuid)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".memberUuid") (Uuid.toString user.uuid)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".memberType") (QuestionnaireEditFormMemberType.toString UserQuestionnairePermType)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".perms") (QuestionnaireEditFormMemberPerms.toString QuestionnaireEditFormMemberPerms.Viewer)
            ]

        newForm =
            List.foldl formUpdate model.questionnaireEditForm msgs
    in
    ( newSeed
    , { model
        | userTypeHintInputModel = userTypeHintInputModel
        , questionnaireEditForm = newForm
        , users = List.uniqueBy (Uuid.toString << .uuid) (user :: model.users)
      }
    , Cmd.none
    )


handleAddUserGroup : AppState -> Model -> UserGroupSuggestion -> ( Seed, Model, Cmd msg )
handleAddUserGroup appState model userGroup =
    let
        userGroupTypeHintInputModel =
            TypeHintInput.clear model.userGroupTypeHintInputModel

        permissionsLength =
            List.length <| Form.getListIndexes "permissions" model.questionnaireEditForm

        formUpdate =
            Form.update (QuestionnaireEditForm.validation appState)

        createInputMessage field value =
            Form.Input field Form.Text (Field.String value)

        ( newUuid, newSeed ) =
            getUuid appState.seed

        msgs =
            [ Form.Append "permissions"
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".uuid") (Uuid.toString newUuid)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".memberUuid") (Uuid.toString userGroup.uuid)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".memberType") (QuestionnaireEditFormMemberType.toString UserGroupQuestionnairePermType)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".perms") (QuestionnaireEditFormMemberPerms.toString QuestionnaireEditFormMemberPerms.Viewer)
            ]

        newForm =
            List.foldl formUpdate model.questionnaireEditForm msgs
    in
    ( newSeed
    , { model
        | userGroupTypeHintInputModel = userGroupTypeHintInputModel
        , questionnaireEditForm = newForm
        , userGroups = List.uniqueBy (Uuid.toString << .uuid) (userGroup :: model.userGroups)
      }
    , Cmd.none
    )


handleFormMsg : UpdateConfig msg -> Form.Msg -> AppState -> Model -> ( Model, Cmd msg )
handleFormMsg cfg formMsg appState model =
    case ( formMsg, Form.getOutput model.questionnaireEditForm ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    QuestionnaireEditForm.encode form

                cmd =
                    Cmd.map cfg.wrapMsg <|
                        QuestionnairesApi.putQuestionnaire cfg.questionnaireUuid body appState PutQuestionnaireComplete
            in
            ( { model | savingSharing = Loading }
            , cmd
            )

        _ ->
            ( { model | questionnaireEditForm = Form.update (QuestionnaireEditForm.validation appState) formMsg model.questionnaireEditForm }
            , Cmd.none
            )


handlePutQuestionnaireComplete : AppState -> Model -> Result ApiError () -> ( Model, Cmd msg )
handlePutQuestionnaireComplete appState model result =
    case result of
        Ok _ ->
            ( { model | visible = False, savingSharing = Unset }, Ports.refresh () )

        Err error ->
            ( { model | savingSharing = ApiError.toActionResult appState (gettext "Questionnaire could not be saved." appState.locale) error }
            , Cmd.none
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
            [ Html.viewIf (Admin.isEnabled appState.config.admin) <| userGroupsView appState model
            , usersView appState model
            , formView appState model.questionnaireUuid model.questionnaireEditForm
            ]

        modalConfig =
            { modalTitle = gettext "Share Project" appState.locale
            , modalContent = modalContent
            , visible = model.visible
            , actionResult = model.savingSharing
            , actionName = gettext "Save" appState.locale
            , actionMsg = FormMsg Form.Submit
            , cancelMsg = Just Close
            , dangerous = False
            , dataCy = "project-share"
            }
    in
    Modal.confirm appState modalConfig


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


userGroupView : AppState -> List UserGroupSuggestion -> Form FormError QuestionnaireEditForm -> Int -> Html Form.Msg
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
    div [ class "ShareModal__Users" ]
        [ div [ class "mt-2" ]
            [ strong [] [ text (gettext "Users" appState.locale) ]
            , userTypeHintInput
            ]
        , Html.map FormMsg <| FormGroup.viewList appState (userView appState model.users) model.questionnaireEditForm "permissions" ""
        , separator
        ]


userView : AppState -> List UserSuggestion -> Form FormError QuestionnaireEditForm -> Int -> Html Form.Msg
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


formView : AppState -> Uuid -> Form FormError QuestionnaireEditForm -> Html Msg
formView appState questionnaireUuid form =
    let
        visibilityInputs =
            if appState.config.questionnaire.questionnaireVisibility.enabled then
                let
                    visibilitySelect =
                        if (Form.getFieldAsString "sharingPermission" form).value == Just "edit" then
                            strong [] [ text (gettext "edit" appState.locale) ]

                        else
                            FormExtra.inlineSelect (QuestionnairePermission.formOptions appState) form "visibilityPermission"

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
                    publicLink =
                        appState.clientUrl ++ String.replace "/wizard" "" (Routing.toUrl appState (Routes.ProjectsRoute (Routes.DetailRoute questionnaireUuid (ProjectDetailRoute.Questionnaire Nothing))))

                    sharingEnabled =
                        Maybe.withDefault False (Form.getFieldAsBool "sharingEnabled" form).value

                    publicLinkView =
                        div
                            [ class "form-group form-group-toggle-extra"
                            , classList [ ( "visible", sharingEnabled ) ]
                            ]
                            [ div [ class "d-flex" ]
                                [ input [ readonly True, class "form-control", id "public-link", value publicLink ] []
                                , button [ class "btn btn-link text-nowrap", onClick (CopyPublicLink publicLink) ] [ text (gettext "Copy link" appState.locale) ]
                                ]
                            ]

                    sharingSelect =
                        FormExtra.inlineSelect (QuestionnairePermission.formOptions appState) form "sharingPermission"

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
                , publicLinkView
                ]

            else
                []
    in
    div []
        (visibilityInputs ++ sharingInputs)
