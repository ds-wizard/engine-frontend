module Common.Pages.Projects.Components.ShareModal exposing
    ( Model
    , Msg
    , ShareData
    , UpdateConfig
    , initialModel
    , open
    , subscriptions
    , update
    , view
    )

{-| "Share Project": who the project belongs to, who else may open it, and what
each of them may do with it.

It lives beside the other project components rather than under `Detail/`,
because everything in `Dsw/Pages/Projects/Detail/` is a tab of the project — a
route with a body — and this is a dialog the header opens.

The dialog is a form (`Ui.Modal.withForm`), so Cmd/Ctrl+Enter anywhere in it
saves and the submit is busy while the request is out. What it saves is one request:
`ProjectsApi.putShare` takes the visibility, the public link and the whole list
of members at once, and the page is handed the same three back as `ShareData`
so the header can say how the project is shared without loading it again.

**Two lists, one field.** Users and user groups are one `permissions` list in
the form, shown as two lists on the screen — the member's uuid says which of
them a row belongs to, exactly as the wizard did. Each list is added to by a
`Common.Components.TypeHint` over the suggestion endpoints, filtered by the
uuids the form already holds so nobody is offered twice. In a workspace a
project is shared only with its members — a share with anybody else could not
be added — so the users come from the members of the workspace the reader is
in (`appState.workspaceUuid`).

**The last owner cannot be removed.** A project with nobody who may administer
it cannot be shared, renamed or deleted again, so the row that is the only
owner keeps its permission. Its permission and its Remove button are both
unavailable with the reason as their tooltip (`Field.withDisabledReason`,
`Button.withDisabledReason`) — still focusable, so the reason reaches a keyboard
and a screen reader and not only a pointer.

**No selects inside sentences.** "Other logged-in users can [view] the
project." is not a `select` inside a sentence, which the design system has no
room for: a select is a labeled field. The words are the label instead,
and the toggle above it still decides whether the field is there at all.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom as BrowserDom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Member as Member
import Common.Api.Models.Permission exposing (Permission)
import Common.Api.Models.Project.ProjectSharing as ProjectSharing exposing (ProjectSharing)
import Common.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility)
import Common.Api.Models.ProjectCommon exposing (ProjectCommon)
import Common.Api.Models.ProjectPermission as ProjectPermission
import Common.Api.Models.User as User
import Common.Api.Models.UserGroupSuggestion exposing (UserGroupSuggestion)
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Api.Projects as ProjectsApi
import Common.Api.UserGroups as UserGroupsApi
import Common.Api.Users as UsersApi
import Common.Api.Workspaces as WorkspacesApi
import Common.Components.Copyable as Copyable
import Common.Components.Form as Form
import Common.Components.Form.Field as FormField
import Common.Components.GuideLink as GuideLink
import Common.Components.TypeHint as TypeHint
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Forms.ProjectShareForm as ProjectShareForm exposing (ProjectShareForm)
import Common.Data.Forms.ProjectShareFormMemberPermType as MemberPerms
import Common.Data.Forms.ProjectShareFormMemberType as MemberType exposing (ProjectShareFormMemberType(..))
import Common.Data.GuideLinks as GuideLinks
import Common.Routes as Routes
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form as ElmForm exposing (Form)
import Form.Field
import Gettext exposing (gettext)
import Html exposing (Html)
import List.Extra as List
import String.Format as String
import Task
import Task.Extra as Task
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.Copy as Copy
import Ui.Form as UiForm
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon
import Ui.Layout as Layout
import Ui.ListItem as ListItem
import Ui.Modal as Modal
import Ui.Separator as Separator
import Ui.Text as Text
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { visible : Bool
    , savingSharing : ActionResult String
    , projectShareForm : Form FormError ProjectShareForm
    , projectUuid : Uuid
    , userTypeHint : TypeHint.Model UserSuggestion
    , userGroupTypeHint : TypeHint.Model UserGroupSuggestion
    , users : List UserSuggestion
    , userGroups : List UserGroupSuggestion
    , copy : Copy.Model
    }


initialModel : Model
initialModel =
    { visible = False
    , savingSharing = Unset
    , projectShareForm = ProjectShareForm.initEmpty
    , projectUuid = Uuid.nil
    , userTypeHint = TypeHint.init
    , userGroupTypeHint = TypeHint.init
    , users = []
    , userGroups = []
    , copy = Copy.init
    }


{-| The dialog is filled from the project every time it is opened, so it never
shows what the project looked like the last time somebody shared it.

The members it knows about start as the ones the project already has: a row
shows a name and a picture, and the form field holds only a uuid.

-}
setProject : ProjectCommon -> Model -> Model
setProject project model =
    { model
        | projectShareForm = ProjectShareForm.init project
        , projectUuid = project.uuid
        , users = List.filterMap (.member >> Member.toUserSuggestion) project.permissions
        , userGroups = List.filterMap (.member >> Member.toUserGroupSuggestion) project.permissions
        , savingSharing = Unset
    }



-- UPDATE


type Msg
    = Open ProjectCommon
    | Close
    | FormMsg ElmForm.Msg
    | UserTypeHintMsg (TypeHint.Msg UserSuggestion)
    | UserGroupTypeHintMsg (TypeHint.Msg UserGroupSuggestion)
    | AddUser UserSuggestion
    | AddUserGroup UserGroupSuggestion
    | PutShareCompleted ShareData (Result ApiError ())
    | CopyMsg Copy.Msg
    | NoOp


{-| Opened by the project's page, with the project to share.
-}
open : ProjectCommon -> Msg
open =
    Open


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    , projectUuid : Uuid
    , onSaveMsg : ShareData -> msg
    }


{-| What the page needs to redraw its header: the same three things
`ProjectCommon.updateWithShareData` takes.
-}
type alias ShareData =
    { permissions : List Permission
    , sharing : ProjectSharing
    , visibility : ProjectVisibility
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        Open project ->
            ( setProject project { model | visible = True }, Cmd.none )

        Close ->
            -- The dialog gives focus back to the button that opened it, so
            -- there is nothing to move here.
            ( { model | visible = False, savingSharing = Unset }, Cmd.none )

        FormMsg formMsg ->
            handleFormMsg cfg appState formMsg model

        UserTypeHintMsg typeHintMsg ->
            let
                ( userTypeHint, cmd ) =
                    TypeHint.update appState
                        { wrapMsg = cfg.wrapMsg << UserTypeHintMsg
                        , getTypeHints =
                            case appState.workspaceUuid of
                                Just workspaceUuid ->
                                    WorkspacesApi.getMemberSuggestions appState workspaceUuid

                                Nothing ->
                                    UsersApi.getUsersSuggestions appState
                        , getError = gettext "Unable to get users." appState.locale
                        , setReply = cfg.wrapMsg << AddUser
                        , clearReply = Nothing
                        , filterResults = Just (notAlreadyAMember model << .uuid)
                        , logoutMsg = cfg.logoutMsg
                        , id = usersFieldId
                        , toKey = Uuid.toString << .uuid
                        }
                        typeHintMsg
                        model.userTypeHint
            in
            ( { model | userTypeHint = userTypeHint }, cmd )

        UserGroupTypeHintMsg typeHintMsg ->
            let
                ( userGroupTypeHint, cmd ) =
                    TypeHint.update appState
                        { wrapMsg = cfg.wrapMsg << UserGroupTypeHintMsg
                        , getTypeHints = UserGroupsApi.getUserGroupsSuggestions appState
                        , getError = gettext "Unable to get user groups." appState.locale
                        , setReply = cfg.wrapMsg << AddUserGroup
                        , clearReply = Nothing
                        , filterResults = Just (notAlreadyAMember model << .uuid)
                        , logoutMsg = cfg.logoutMsg
                        , id = userGroupsFieldId
                        , toKey = Uuid.toString << .uuid
                        }
                        typeHintMsg
                        model.userGroupTypeHint
            in
            ( { model | userGroupTypeHint = userGroupTypeHint }, cmd )

        AddUser user ->
            -- The picker is emptied again: it is the way to add somebody, not
            -- a field holding a value, and the person just added is now a row.
            ( { model
                | userTypeHint = TypeHint.setSelected Nothing model.userTypeHint
                , projectShareForm = appendMember UserProjectPermType user.uuid model.projectShareForm
                , users = List.uniqueBy (Uuid.toString << .uuid) (user :: model.users)
              }
            , Cmd.none
            )

        AddUserGroup userGroup ->
            ( { model
                | userGroupTypeHint = TypeHint.setSelected Nothing model.userGroupTypeHint
                , projectShareForm = appendMember UserGroupProjectPermType userGroup.uuid model.projectShareForm
                , userGroups = List.uniqueBy (Uuid.toString << .uuid) (userGroup :: model.userGroups)
              }
            , Cmd.none
            )

        PutShareCompleted shareData result ->
            case result of
                Ok _ ->
                    ( { model | visible = False, savingSharing = Unset }
                    , Task.dispatch (cfg.onSaveMsg shareData)
                    )

                Err error ->
                    ( { model | savingSharing = ApiError.toActionResult appState (gettext "Questionnaire could not be saved." appState.locale) error }
                    , Cmd.batch
                        [ Form.focusErrorSummary
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        ]
                    )

        CopyMsg copyMsg ->
            let
                ( copy, cmd ) =
                    Copy.update copyMsg model.copy
            in
            ( { model | copy = copy }, Cmd.map (cfg.wrapMsg << CopyMsg) cmd )

        NoOp ->
            ( model, Cmd.none )


handleFormMsg : UpdateConfig msg -> AppState -> ElmForm.Msg -> Model -> ( Model, Cmd msg )
handleFormMsg cfg appState formMsg model =
    case ( formMsg, ElmForm.getOutput model.projectShareForm ) of
        ( ElmForm.Submit, Just form ) ->
            ( { model | savingSharing = Loading }
            , ProjectsApi.putShare appState
                cfg.projectUuid
                (ProjectShareForm.encode form)
                (cfg.wrapMsg << PutShareCompleted (toShareData model form))
            )

        _ ->
            let
                projectShareForm =
                    ElmForm.update ProjectShareForm.validation formMsg model.projectShareForm
            in
            ( { model | projectShareForm = projectShareForm }
            , case formMsg of
                ElmForm.RemoveItem removedField index ->
                    if removedField == permissionsField then
                        -- The button that was pressed went with its row, so
                        -- focus moves to the row that took its place.
                        Cmd.map cfg.wrapMsg (focusAfterRemove index projectShareForm)

                    else
                        Cmd.none

                _ ->
                    Cmd.none
            )


{-| A new row for somebody who has just been picked, as a viewer — the least a
member can be given, which is what the wizard added them as too.
-}
appendMember : ProjectShareFormMemberType -> Uuid -> Form FormError ProjectShareForm -> Form FormError ProjectShareForm
appendMember memberType memberUuid form =
    let
        index =
            List.length (ElmForm.getListIndexes permissionsField form)

        setString field value =
            ElmForm.Input (permissionPath index field) ElmForm.Text (Form.Field.String value)
    in
    List.foldl (ElmForm.update ProjectShareForm.validation)
        form
        [ ElmForm.Append permissionsField
        , setString "memberUuid" (Uuid.toString memberUuid)
        , setString "memberType" (MemberType.toString memberType)
        , setString "perms" (MemberPerms.toString MemberPerms.Viewer)
        ]


{-| The permission of the row that is now where the removed one was, or of the
one before it when the last row went. Nothing to focus when no rows are left:
the dialog holds focus, and Tab from there reaches its first control.
-}
focusAfterRemove : Int -> Form FormError ProjectShareForm -> Cmd Msg
focusAfterRemove removed form =
    let
        remaining =
            List.length (ElmForm.getListIndexes permissionsField form)
    in
    if remaining == 0 then
        Cmd.none

    else
        Task.attempt (always NoOp)
            (BrowserDom.focus (FormField.fieldId (permissionPath (min removed (remaining - 1)) "perms")))


notAlreadyAMember : Model -> Uuid -> Bool
notAlreadyAMember model uuid =
    List.notMember (Uuid.toString uuid) (ProjectShareForm.getMemberUuids model.projectShareForm)


toShareData : Model -> ProjectShareForm -> ShareData
toShareData model form =
    let
        toMember formPermission =
            case formPermission.memberType of
                UserProjectPermType ->
                    Maybe.map Member.userMember
                        (List.find (.uuid >> (==) formPermission.memberUuid) model.users)

                UserGroupProjectPermType ->
                    Maybe.map Member.userGroupMember
                        (List.find (.uuid >> (==) formPermission.memberUuid) model.userGroups)

        toPermission formPermission =
            Maybe.map
                (\member -> { member = member, perms = MemberPerms.toPerms formPermission.perms })
                (toMember formPermission)
    in
    { permissions = List.filterMap toPermission form.permissions
    , sharing = ProjectSharing.fromFormValues form.sharingEnabled form.sharingPermission
    , visibility = ProjectVisibility.fromFormValues form.visibilityEnabled form.visibilityPermission form.sharingEnabled form.sharingPermission
    }



-- FIELDS


permissionsField : String
permissionsField =
    "permissions"


permissionPath : Int -> String -> String
permissionPath index field =
    permissionsField ++ "." ++ String.fromInt index ++ "." ++ field


usersFieldId : String
usersFieldId =
    "projectShareUsers"


userGroupsFieldId : String
userGroupsFieldId =
    "projectShareUserGroups"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map UserTypeHintMsg (TypeHint.subscriptions model.userTypeHint)
        , Sub.map UserGroupTypeHintMsg (TypeHint.subscriptions model.userGroupTypeHint)
        ]



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Modal.plain
        { title = gettext "Share Project" appState.locale
        , open = model.visible
        , onClose = Close
        , body = body appState model
        }
        |> Modal.withForm
            { onSubmit = FormMsg ElmForm.Submit
            , submitLabel = gettext "Save" appState.locale
            }
        |> Modal.withLoading (ActionResult.isLoading model.savingSharing)
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withHeaderAction (GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.projectsSharing)
        -- Opened to see who has the project as much as to change it, and the
        -- first field is a picker that drops its suggestions over that list on
        -- focus.
        |> Modal.withFocusOnTitle
        |> Modal.withTestId "project-share"
        |> Modal.toHtml


{-| The people first, then what everybody else may do. The guide is the
header's, at the end of the title's line, as it is on a page.
-}
body : AppState -> Model -> List (Html Msg)
body appState model =
    let
        sections =
            List.filterNot List.isEmpty
                [ userGroupsSection appState model
                , usersSection appState model
                , permissionsSection appState model
                ]
    in
    [ Form.errorSummary model.savingSharing
    , Layout.stack
        (List.intersperse Separator.line (List.map Layout.stack sections))
    ]



-- VIEW - MEMBERS


{-| The groups a project is shared with, and the picker to add one.
-}
userGroupsSection : AppState -> Model -> List (Html Msg)
userGroupsSection appState model =
    TypeHint.view appState
        { id = userGroupsFieldId
        , label = gettext "User Groups" appState.locale
        , placeholder = gettext "Add user group" appState.locale
        , required = False
        , error = Nothing
        , hint = Nothing
        , clearable = False
        , viewItem =
            \userGroup ->
                { label = userGroup.name
                , code = Nothing
                , badges = []
                , description = userGroup.description
                , icon = Nothing
                }
        , toKey = Uuid.toString << .uuid
        , wrapMsg = UserGroupTypeHintMsg
        }
        model.userGroupTypeHint
        :: memberList (userGroupRow appState model) model


usersSection : AppState -> Model -> List (Html Msg)
usersSection appState model =
    TypeHint.view appState
        { id = usersFieldId
        , label = gettext "Users" appState.locale
        , placeholder = gettext "Add users" appState.locale
        , required = False
        , error = Nothing
        , hint = Nothing
        , clearable = False
        , viewItem =
            \user ->
                { label = User.fullName user
                , code = Nothing
                , badges = []
                , description = user.affiliation
                , icon =
                    Just
                        (UserAvatar.user
                            { firstName = user.firstName
                            , lastName = user.lastName
                            , imageUrl = user.imageUrl
                            , gravatarHash = Just user.gravatarHash
                            }
                        )
                }
        , toKey = Uuid.toString << .uuid
        , wrapMsg = UserTypeHintMsg
        }
        model.userTypeHint
        :: memberList (userRow appState model) model


{-| The rows one kind of member has. Both kinds are indexes into the same form
field, so each row decides for itself whether it is one of its kind; an empty
list renders nothing rather than an empty list element.

Tiles, each member on a ground of its own: rules between the rows would look
like the rules between the dialog's sections, and the list would not read as
one thing.

-}
memberList : (Int -> Maybe (ListItem.ListItem Msg)) -> Model -> List (Html Msg)
memberList toRow model =
    case List.filterMap toRow (ElmForm.getListIndexes permissionsField model.projectShareForm) of
        [] ->
            []

        rows ->
            [ ListItem.tiles rows ]


userRow : AppState -> Model -> Int -> Maybe (ListItem.ListItem Msg)
userRow appState model index =
    let
        toRow user =
            let
                name =
                    User.fullName user

                lastOwner =
                    isLastOwner index model.projectShareForm
            in
            ListItem.item (Html.text name)
                |> ListItem.withIcon
                    (ItemIcon.toHtml
                        (UserAvatar.user
                            { firstName = user.firstName
                            , lastName = user.lastName
                            , imageUrl = user.imageUrl
                            , gravatarHash = Just user.gravatarHash
                            }
                        )
                    )
                |> ListItem.withFragments (List.filterMap (Maybe.map Text.muted) [ user.affiliation ])
                |> ListItem.withActions (memberActions appState model index name lastOwner)
                |> ListItem.withTestId ("share-members_item_" ++ Uuid.toString user.uuid)
    in
    Maybe.map toRow (memberAt index model.projectShareForm model.users)


userGroupRow : AppState -> Model -> Int -> Maybe (ListItem.ListItem Msg)
userGroupRow appState model index =
    let
        toRow userGroup =
            let
                fragments =
                    if userGroup.private then
                        [ Badge.toHtml (Badge.neutral (gettext "private" appState.locale)) ]

                    else
                        []
            in
            ListItem.item (Html.text userGroup.name)
                |> ListItem.withIcon (ItemIcon.toHtml (UserAvatar.group userGroup.name))
                |> ListItem.withFragments fragments
                |> ListItem.withActions (memberActions appState model index userGroup.name False)
                |> ListItem.withTestId ("share-user-groups_item_" ++ Uuid.toString userGroup.uuid)
    in
    Maybe.map toRow (memberAt index model.projectShareForm model.userGroups)


{-| The member a row stands for, when the uuid in the form is one of the ones
this list knows about. A row whose uuid belongs to the other kind of member
returns nothing, which is how one form field becomes two lists.
-}
memberAt : Int -> Form FormError ProjectShareForm -> List { a | uuid : Uuid } -> Maybe { a | uuid : Uuid }
memberAt index form members =
    let
        memberUuid =
            (ElmForm.getFieldAsString (permissionPath index "memberUuid") form).value
    in
    List.find (.uuid >> Uuid.toString >> Just >> (==) memberUuid) members


{-| What this member may do, and the way to take them off the project.

The permission is a field with a hidden label: the row already shows whose
permission it is, and a visible "Permissions" on every row would be the same
word down the list. The name is in the label all the same, so a screen reader
reads which member the select belongs to.

-}
memberActions : AppState -> Model -> Int -> String -> Bool -> Html Msg
memberActions appState model index name lastOwner =
    let
        permissionSelect =
            FormField.select appState
                model.projectShareForm
                (permissionPath index "perms")
                (String.format (gettext "Permissions for %s" appState.locale) [ name ])
                (MemberPerms.formOptions appState)
                |> FormField.withHiddenLabel
                |> (if lastOwner then
                        FormField.withDisabledReason
                            { id = FormField.fieldId (permissionPath index "perms-reason")
                            , reason = gettext "Last owner cannot be changed." appState.locale
                            }

                    else
                        identity
                   )
                |> FormField.toHtml
                |> Html.map FormMsg

        removeButton =
            Button.dangerIconOnly (Icon.solid "fa-xmark")
                (gettext "Remove" appState.locale)
                (FormMsg (ElmForm.RemoveItem permissionsField index))
                |> Button.withAccessibleName (String.format (gettext "Remove %s" appState.locale) [ name ])
                |> Button.withTooltip

        -- The last owner's Remove is there and unavailable, with the reason
        -- as its tooltip, rather than gone: a missing button says nothing
        -- about why, and a disabled one cannot take the focus the reason is
        -- read on.
        removeControl =
            if lastOwner then
                Button.withDisabledReason
                    { id = FormField.fieldId (permissionPath index "remove-reason")
                    , reason = gettext "Last owner cannot be removed." appState.locale
                    }
                    removeButton

            else
                removeButton
    in
    Layout.inline [ permissionSelect, Button.toHtml removeControl ]


{-| The one row that may not be changed: this row is an owner and the only one.
-}
isLastOwner : Int -> Form FormError ProjectShareForm -> Bool
isLastOwner index form =
    let
        owners =
            List.filter
                (\other ->
                    (ElmForm.getFieldAsString (permissionPath other "perms") form).value
                        == Just (MemberPerms.toString MemberPerms.Owner)
                )
                (ElmForm.getListIndexes permissionsField form)
    in
    owners == [ index ]



-- VIEW - VISIBILITY AND THE PUBLIC LINK


permissionsSection : AppState -> Model -> List (Html Msg)
permissionsSection appState model =
    visibilityFields appState model ++ sharingFields appState model


{-| What every other signed-in user may do without being invited.

Its choices stop at the one the public link already gives: a project anyone
with the link may edit cannot also be one signed-in users may only view.

-}
visibilityFields : AppState -> Model -> List (Html Msg)
visibilityFields appState model =
    if appState.config.project.projectVisibility.enabled then
        let
            visibilityToggle =
                toggle appState model "visibilityEnabled" (gettext "Visible by all other logged-in users" appState.locale)
        in
        if isEnabled "visibilityEnabled" model then
            let
                filterPermission =
                    if isEnabled "sharingEnabled" model then
                        (ElmForm.getFieldAsString "sharingPermission" model.projectShareForm).value

                    else
                        Nothing
            in
            [ visibilityToggle
            , UiForm.nested
                [ select appState model "visibilityPermission" (gettext "What other logged-in users can do" appState.locale) (ProjectPermission.formOptions appState filterPermission) ]
            ]

        else
            [ visibilityToggle ]

    else
        []


{-| The link anyone may open, and under it a "Copy
link", which turns to "Copied!" once pressed — the link itself is there to be
passed on, not read, so it is not written out.
-}
sharingFields : AppState -> Model -> List (Html Msg)
sharingFields appState model =
    if appState.config.project.projectSharing.enabled then
        let
            sharingToggle =
                toggle appState model "sharingEnabled" (gettext "Public link" appState.locale)
        in
        if isEnabled "sharingEnabled" model then
            [ sharingToggle
            , UiForm.nested
                [ select appState model "sharingPermission" (gettext "What anyone with the link can do" appState.locale) (ProjectPermission.formOptions appState Nothing)
                , Copyable.link appState
                    { key = "project-public-link"
                    , label = gettext "Copy link" appState.locale
                    , value = publicLink appState model
                    , model = model.copy
                    , toMsg = CopyMsg
                    }
                ]
            ]

        else
            [ sharingToggle ]

    else
        []


publicLink : AppState -> Model -> String
publicLink appState model =
    AppState.getClientUrlRoot appState ++ Routes.toUrl appState (Routes.projectsDetail model.projectUuid)


isEnabled : String -> Model -> Bool
isEnabled path model =
    Maybe.withDefault False (ElmForm.getFieldAsBool path model.projectShareForm).value


toggle : AppState -> Model -> String -> String -> Html Msg
toggle appState model path label =
    FormField.toggle appState model.projectShareForm path label
        |> FormField.toHtml
        |> Html.map FormMsg


select : AppState -> Model -> String -> String -> List ( String, String ) -> Html Msg
select appState model path label options =
    FormField.select appState model.projectShareForm path label options
        |> FormField.toHtml
        |> Html.map FormMsg
