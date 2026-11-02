module Common.Pages.UserGroups.Detail exposing
    ( Model
    , Msg
    , entityName
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

{-| One user group, with two tabs: its details, and its members.

Owners of the group and those who manage users change it; everybody else sees
it read-only.

The details are a settings form, saved through the unsaved-changes bar in the
page's footer; leaving with something unsaved asks first.

The members are a list, and every change to it is saved at once, as on a
workspace's members: a row's button shows what the member is in the group and
opens onto the other membership, the chosen one checked, and removing them,
which asks first. "Add member" is a dialog to choose the user and their
membership. The user is picked from the members of the group's workspace
(being in a group requires being in its workspace), those not in the group
yet.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Pagination as Pagination
import Common.Api.Models.UserGroupDetail exposing (UserGroupDetail)
import Common.Api.Models.UserGroupMember exposing (UserGroupMember)
import Common.Api.Models.UserGroupMembershipType as UserGroupMembershipType exposing (UserGroupMembershipType(..))
import Common.Api.Models.WorkspaceMember exposing (WorkspaceMember)
import Common.Api.UserGroups as UserGroupsApi
import Common.Api.Workspaces as WorkspacesApi
import Common.Components.ApiErrorAlert as ApiErrorAlert
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.DetailPage as DetailPage
import Common.Components.EntityCard as EntityCard
import Common.Components.Form as FormLayout
import Common.Components.Form.Field as Field
import Common.Components.GuideLink as GuideLink
import Common.Components.ItemActions as ItemActions
import Common.Components.TypeHint as TypeHint
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.Forms.UserGroupForm as UserGroupForm exposing (UserGroupForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Data.UuidOrCurrent as UuidOrCurrent
import Common.Pages.UserGroups.Routes as UserGroupsRoutes exposing (DetailTab(..))
import Common.Routes as Routes
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Dict exposing (Dict)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import String.Format as String
import Task
import Ui.Alert as Alert
import Ui.AppShell as AppShell
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.EmptyState as EmptyState
import Ui.Field as UiField
import Ui.Icon as Icon
import Ui.Illustration as Illustration
import Ui.ItemIcon as ItemIcon
import Ui.Link as Link
import Ui.ListItem as ListItem
import Ui.Menu as Menu
import Ui.Modal as Modal
import Ui.Page.Content as Content
import Ui.Text as Text
import Uuid exposing (Uuid)


type alias Model =
    { uuid : Uuid
    , userGroup : ActionResult UserGroupDetail
    , form : Form FormError UserGroupForm
    , savingUserGroup : ActionResult String

    -- The members: each row's menu, a change of membership on its way, and the
    -- member asked about before being removed.
    , memberMenus : Dict String Menu.Model
    , changingMember : ActionResult String
    , memberToBeRemoved : Maybe UserGroupMember
    , removingMember : ActionResult String

    -- Adding a member: whether the dialog is open, the user being chosen, the
    -- membership chosen for them, and the request.
    , addOpen : Bool
    , userTypeHint : TypeHint.Model WorkspaceMember
    , addMembershipType : UserGroupMembershipType
    , addUserMissing : Bool
    , adding : ActionResult String
    }


type Msg
    = GetUserGroupComplete (Result ApiError UserGroupDetail)
    | FormMsg Form.Msg
    | PutUserGroupComplete UserGroupForm (Result ApiError ())
    | DiscardInfo
    | MemberMenuMsg Uuid Menu.Msg
    | SetMembershipType UserGroupMember UserGroupMembershipType
    | SetMembershipTypeComplete Uuid (List UserGroupMember) (Result ApiError ())
    | ShowHideRemove (Maybe UserGroupMember)
    | Remove
    | RemoveComplete (List UserGroupMember) (Result ApiError ())
    | SetAddOpen Bool
    | UserTypeHintMsg (TypeHint.Msg WorkspaceMember)
    | UserChosen
    | ClearAddUser
    | SetAddMembershipType String
    | Add
    | AddComplete (List UserGroupMember) (Result ApiError ())
    | NoOp


initialModel : Uuid -> Model
initialModel uuid =
    { uuid = uuid
    , userGroup = Loading
    , form = UserGroupForm.initEmpty
    , savingUserGroup = Unset
    , memberMenus = Dict.empty
    , changingMember = Unset
    , memberToBeRemoved = Nothing
    , removingMember = Unset
    , addOpen = False
    , userTypeHint = TypeHint.init
    , addMembershipType = Member
    , addUserMissing = False
    , adding = Unset
    }


init : AppState -> Uuid -> ( Model, Cmd Msg )
init appState uuid =
    ( initialModel uuid
    , UserGroupsApi.getUserGroup appState uuid GetUserGroupComplete
    )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    let
        focus elementId =
            Task.attempt (always (cfg.wrapMsg NoOp)) (Dom.focus elementId)

        -- Every change to the members is the whole list, sent at once.
        putMembers newMembers toMsg =
            UserGroupsApi.putUserGroupMemberships appState model.uuid newMembers (cfg.wrapMsg << toMsg newMembers)

        setMembers newMembers =
            ActionResult.map (\userGroup -> { userGroup | users = newMembers }) model.userGroup
    in
    case msg of
        GetUserGroupComplete result ->
            case result of
                Ok userGroup ->
                    ( { model
                        | userGroup = Success userGroup
                        , form = UserGroupForm.init userGroup
                      }
                    , UpdateConfig.entityWorkspace cfg userGroup.workspaceUuid
                    )

                Err error ->
                    if ApiError.isNotFound error then
                        ( model
                        , UpdateConfig.notFound cfg
                            { message = gettext "The user group you are looking for was not found." appState.locale
                            , back = Nothing
                            }
                        )

                    else
                        ( { model | userGroup = ApiError.toActionResult appState (gettext "Unable to get the user group." appState.locale) error }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    ( { model | savingUserGroup = Loading }
                    , UserGroupsApi.putUserGroup appState model.uuid (UserGroupForm.encode form) (cfg.wrapMsg << PutUserGroupComplete form)
                    )

                _ ->
                    ( { model | form = Form.update UserGroupForm.validation formMsg model.form, savingUserGroup = Unset }
                    , Cmd.none
                    )

        PutUserGroupComplete form result ->
            case result of
                Ok _ ->
                    let
                        setFields userGroup =
                            { userGroup | name = form.name, description = form.description, private = form.private }
                    in
                    ( { model
                        | userGroup = ActionResult.map setFields model.userGroup
                        , form = UserGroupForm.init form
                        , savingUserGroup = Success (gettext "User group was successfully updated." appState.locale)
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | savingUserGroup = ApiError.toActionResult appState (gettext "User group could not be saved." appState.locale) error
                        , form = FormUtils.setFormErrors appState error model.form
                      }
                    , Cmd.batch [ RequestHelpers.getResultCmd cfg.logoutMsg result, FormLayout.focusErrorSummary ]
                    )

        DiscardInfo ->
            -- The bar's buttons go with the bar: focus goes to the heading.
            ( { model
                | form = ActionResult.unwrap model.form UserGroupForm.init model.userGroup
                , savingUserGroup = Unset
              }
            , Task.attempt (always (cfg.wrapMsg NoOp)) (Dom.focus AppShell.mainHeadingId)
            )

        MemberMenuMsg uuid menuMsg ->
            let
                ( menu, cmd ) =
                    Menu.update (memberMenuId uuid) memberMenuCount menuMsg (memberMenu uuid model)
            in
            ( { model | memberMenus = Dict.insert (Uuid.toString uuid) menu model.memberMenus }
            , Cmd.map (cfg.wrapMsg << MemberMenuMsg uuid) cmd
            )

        SetMembershipType member membershipType ->
            -- Closed, with focus back on the row's button, which shows the
            -- membership once it is saved.
            let
                ( menu, menuCmd ) =
                    Menu.update (memberMenuId member.uuid) memberMenuCount Menu.close (memberMenu member.uuid model)

                closed =
                    { model | memberMenus = Dict.insert (Uuid.toString member.uuid) menu model.memberMenus }

                setType current =
                    if current.uuid == member.uuid then
                        { current | membershipType = membershipType }

                    else
                        current
            in
            if member.membershipType == membershipType then
                ( closed, Cmd.map (cfg.wrapMsg << MemberMenuMsg member.uuid) menuCmd )

            else
                ( { closed | changingMember = Loading }
                , Cmd.batch
                    [ Cmd.map (cfg.wrapMsg << MemberMenuMsg member.uuid) menuCmd
                    , putMembers (List.map setType (currentMembers model)) (SetMembershipTypeComplete member.uuid)
                    ]
                )

        SetMembershipTypeComplete uuid newMembers result ->
            case result of
                Ok _ ->
                    ( { model | userGroup = setMembers newMembers, changingMember = Unset }
                    , focus (Menu.triggerId (memberMenuId uuid))
                    )

                Err error ->
                    ( { model | changingMember = ApiError.toActionResult appState (gettext "The membership could not be changed." appState.locale) error }
                    , Cmd.batch [ RequestHelpers.getResultCmd cfg.logoutMsg result, ApiErrorAlert.focus (cfg.wrapMsg NoOp) ]
                    )

        ShowHideRemove mbMember ->
            ( { model
                | memberToBeRemoved = mbMember
                , removingMember = Unset
                , memberMenus = Dict.empty
              }
            , case ( mbMember, model.memberToBeRemoved ) of
                ( Nothing, Just member ) ->
                    focus (Menu.triggerId (memberMenuId member.uuid))

                _ ->
                    Cmd.none
            )

        Remove ->
            case model.memberToBeRemoved of
                Just member ->
                    ( { model | removingMember = Loading }
                    , putMembers (List.filter ((/=) member.uuid << .uuid) (currentMembers model)) RemoveComplete
                    )

                Nothing ->
                    ( model, Cmd.none )

        RemoveComplete newMembers result ->
            case result of
                Ok _ ->
                    -- The row and its button are gone: focus goes to the
                    -- button that adds members, the one control that stays.
                    ( { model | userGroup = setMembers newMembers, memberToBeRemoved = Nothing, removingMember = Unset }
                    , focus addMemberButtonId
                    )

                Err error ->
                    ( { model | removingMember = ApiError.toActionResult appState (gettext "The member could not be removed." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        SetAddOpen open ->
            -- Opened afresh each time, as an ordinary member.
            ( { model
                | addOpen = open
                , userTypeHint = TypeHint.init
                , addMembershipType = Member
                , addUserMissing = False
                , adding = Unset
              }
            , if open then
                Cmd.none

              else
                focus addMemberButtonId
            )

        UserTypeHintMsg typeHintMsg ->
            let
                ( userTypeHint, cmd ) =
                    TypeHint.update appState
                        { wrapMsg = cfg.wrapMsg << UserTypeHintMsg
                        , getTypeHints =
                            \qs toMsg ->
                                case ActionResult.toMaybe model.userGroup |> Maybe.andThen .workspaceUuid of
                                    Just workspaceUuid ->
                                        WorkspacesApi.getMembers appState workspaceUuid qs toMsg

                                    -- A server that does not say where the
                                    -- group is offers nobody.
                                    Nothing ->
                                        Task.perform toMsg (Task.succeed (Ok Pagination.empty))
                        , getError = gettext "Unable to get users." appState.locale
                        , setReply = always (cfg.wrapMsg UserChosen)
                        , clearReply = Just (cfg.wrapMsg ClearAddUser)
                        , filterResults = Just (\user -> List.notMember user.uuid (List.map .uuid (currentMembers model)))
                        , logoutMsg = cfg.logoutMsg
                        , id = addMemberFieldId
                        , toKey = Uuid.toString << .uuid
                        }
                        typeHintMsg
                        model.userTypeHint
            in
            ( { model | userTypeHint = userTypeHint }, cmd )

        UserChosen ->
            ( { model | addUserMissing = False }, Cmd.none )

        ClearAddUser ->
            ( { model | userTypeHint = TypeHint.setSelected Nothing model.userTypeHint }, Cmd.none )

        SetAddMembershipType value ->
            ( { model | addMembershipType = Maybe.withDefault model.addMembershipType (UserGroupMembershipType.fromString value) }
            , Cmd.none
            )

        Add ->
            case TypeHint.selected model.userTypeHint of
                Just user ->
                    let
                        member =
                            { uuid = user.uuid
                            , firstName = user.firstName
                            , lastName = user.lastName
                            , gravatarHash = user.gravatarHash
                            , imageUrl = user.imageUrl
                            , membershipType = model.addMembershipType
                            , affiliation = Nothing
                            }
                    in
                    ( { model | adding = Loading }
                    , putMembers
                        (List.sortWith compareMembers (member :: currentMembers model))
                        AddComplete
                    )

                Nothing ->
                    ( { model | addUserMissing = True }, focus addMemberFieldId )

        AddComplete newMembers result ->
            case result of
                Ok _ ->
                    ( { model | userGroup = setMembers newMembers, addOpen = False, adding = Unset }
                    , focus addMemberButtonId
                    )

                Err error ->
                    ( { model | adding = ApiError.toActionResult appState (gettext "The member could not be added." appState.locale) error }
                    , Cmd.batch [ RequestHelpers.getResultCmd cfg.logoutMsg result, ApiErrorAlert.focus (cfg.wrapMsg NoOp) ]
                    )

        NoOp ->
            ( model, Cmd.none )


currentMembers : Model -> List UserGroupMember
currentMembers model =
    ActionResult.unwrap [] .users model.userGroup


compareMembers : UserGroupMember -> UserGroupMember -> Order
compareMembers a b =
    case compare (String.toLower a.lastName) (String.toLower b.lastName) of
        EQ ->
            compare (String.toLower a.firstName) (String.toLower b.firstName)

        order ->
            order


addMemberFieldId : String
addMemberFieldId =
    "user-group-add-member"


addMemberButtonId : String
addMemberButtonId =
    "user-group-add-member-button"


memberMenuId : Uuid -> String
memberMenuId uuid =
    "user-group-member-" ++ Uuid.toString uuid


memberMenu : Uuid -> Model -> Menu.Model
memberMenu uuid model =
    Maybe.withDefault Menu.init (Dict.get (Uuid.toString uuid) model.memberMenus)


{-| Owner, Member, and Remove.
-}
memberMenuCount : Int
memberMenuCount =
    3


infoChanged : Model -> Bool
infoChanged model =
    ActionResult.isSuccess model.userGroup && FormUtils.containsChanges model.form


subscriptions : AppState -> DetailTab -> Model -> Sub Msg
subscriptions appState tab model =
    case tab of
        Info ->
            if infoChanged model then
                FormLayout.submitShortcut appState { result = model.savingUserGroup, onSubmit = FormMsg Form.Submit }

            else
                Sub.none

        Members ->
            Sub.batch
                (Sub.map UserTypeHintMsg (TypeHint.subscriptions model.userTypeHint)
                    :: (if model.addOpen then
                            FormLayout.submitShortcut appState { result = model.adding, onSubmit = Add }

                        else
                            Sub.none
                       )
                    :: List.filterMap
                        (\( key, menu ) ->
                            Maybe.map (\uuid -> Sub.map (MemberMenuMsg uuid) (Menu.subscriptions (memberMenuId uuid) menu)) (Uuid.fromString key)
                        )
                        (Dict.toList model.memberMenus)
                )


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    if infoChanged model then
        Just (gettext "There are unsaved changes." appState.locale)

    else
        Nothing



-- VIEW


view : AppState -> DetailTab -> Model -> Html Msg
view appState tab model =
    DetailPage.view appState (detailPage appState tab model) model.userGroup


detailPage : AppState -> DetailTab -> Model -> UserGroupDetail -> DetailPage.DetailPage Msg
detailPage appState tab model userGroup =
    let
        editable =
            Feature.userGroupEdit appState userGroup

        tabUrl detailTab =
            Routes.toUrl appState (Routes.UserGroupsRoute (UserGroupsRoutes.Detail model.uuid detailTab))

        tabItem detailTab label testId =
            { label = label
            , url = tabUrl detailTab
            , active = tab == detailTab
            , badge = Nothing
            , testId = Just testId
            , icon = Nothing
            }

        -- Each tab carries its own actions — the guide, and for the members
        -- adding one — so the header has none.
        content =
            case tab of
                Info ->
                    viewInfo appState model editable

                Members ->
                    viewMembers appState model editable userGroup

        badges =
            if userGroup.private then
                [ Badge.neutral (gettext "private" appState.locale) ]

            else
                []
    in
    DetailPage.detailPage { title = userGroup.name, content = content }
        |> DetailPage.withTitleIcon (ItemIcon.toHtml (UserAvatar.group userGroup.name))
        |> DetailPage.withBadges badges
        |> DetailPage.withTabsWithTestIds
            { label = gettext "User group" appState.locale
            , items =
                [ tabItem Info (gettext "Info" appState.locale) "user-group_nav_info"
                , tabItem Members (gettext "Members" appState.locale) "user-group_nav_members"
                ]
            }
        |> (if editable && tab == Info then
                DetailPage.withFooter
                    (FormLayout.unsavedBar appState
                        { changed = infoChanged model
                        , result = model.savingUserGroup
                        , onSave = FormMsg Form.Submit
                        , onDiscard = Just DiscardInfo
                        }
                    )

            else
                identity
           )


guideLink : AppState -> Button.Button msg
guideLink appState =
    GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.userGroupsDetail


viewInfo : AppState -> Model -> Bool -> List (Html Msg)
viewInfo appState model editable =
    let
        body =
            if editable then
                [ FormLayout.view
                    { onSubmit = FormMsg Form.Submit
                    , content =
                        FormLayout.errorSummary model.savingUserGroup
                            :: List.map (Html.map FormMsg << Field.toHtml)
                                [ Field.input appState model.form "name" (gettext "Name" appState.locale)
                                    |> Field.withRequired True
                                , Field.input appState model.form "description" (gettext "Description" appState.locale)
                                , Field.toggle appState model.form "private" (gettext "Private" appState.locale)
                                    |> Field.withHint (gettext "If enabled, this group is only visible to other members of the group and instance admins." appState.locale)
                                ]
                    }
                ]

            else
                Alert.toHtml (Alert.info (gettext "Only user group owners can edit details." appState.locale))
                    :: List.map (Html.map FormMsg << Field.toHtml)
                        [ Field.readOnly model.form "name" (gettext "Name" appState.locale)
                        , Field.readOnly model.form "description" (gettext "Description" appState.locale)
                        ]
    in
    [ Content.content
        { title = gettext "Info" appState.locale
        , body = body
        }
        |> Content.form
        |> Content.withActions [ guideLink appState ]
        |> Content.withinPage
        |> Content.toHtml
    ]


viewMembers : AppState -> Model -> Bool -> UserGroupDetail -> List (Html Msg)
viewMembers appState model editable userGroup =
    let
        addButton =
            Button.primary (gettext "Add member" appState.locale) (SetAddOpen True)
                |> Button.withIcon (Icon.solid "fa-user-plus")
                |> Button.withId addMemberButtonId
                |> Button.withTestId "user-group-members_add-button"

        actions =
            if editable then
                [ guideLink appState, addButton ]

            else
                [ guideLink appState ]

        list =
            if List.isEmpty userGroup.users then
                EmptyState.emptyState
                    { title = gettext "No members" appState.locale
                    , description = gettext "Nobody is a member of this user group yet." appState.locale
                    }
                    |> EmptyState.withIllustration (Illustration.toHtml Illustration.empty)
                    |> (if editable then
                            EmptyState.withAction addButton

                        else
                            identity
                       )
                    |> EmptyState.toHtml

            else
                ListItem.list (List.map (memberRow appState model editable) userGroup.users)
    in
    [ Content.content
        { title = gettext "Members" appState.locale
        , body = [ ApiErrorAlert.view appState model.changingMember, list ]
        }
        |> Content.list
        |> Content.withActions actions
        |> Content.withinPage
        |> Content.toHtml
    , addModal appState model
    , removeModal appState model
    ]


memberRow : AppState -> Model -> Bool -> UserGroupMember -> ListItem.ListItem Msg
memberRow appState model editable member =
    let
        name =
            member.firstName ++ " " ++ member.lastName

        title =
            if Feature.usersManage appState then
                Link.toHtml (Link.link { label = name, url = Routes.toUrl appState (Routes.usersEdit (UuidOrCurrent.uuid member.uuid)) })

            else
                Html.text name

        membership =
            membershipLabel appState member.membershipType

        withMembership row =
            if editable then
                ListItem.withActions
                    (ItemActions.labeledMenu
                        { id = memberMenuId member.uuid
                        , label = String.format (gettext "%s, membership of %s in this user group" appState.locale) [ membership, name ]
                        , content = [ Html.text membership ]
                        , actions =
                            ItemActions.groups
                                [ List.map
                                    (\membershipType ->
                                        ( ItemActions.choice
                                            { label = membershipLabel appState membershipType
                                            , chosen = membershipType == member.membershipType
                                            , onSelect = SetMembershipType member membershipType
                                            }
                                            |> ItemActions.withTestId (membershipTestId membershipType)
                                        , True
                                        )
                                    )
                                    membershipTypes
                                , [ ( ItemActions.dangerAction
                                        { icon = Icon.solid "fa-user-minus"
                                        , label = gettext "Remove" appState.locale
                                        , onSelect = ShowHideRemove (Just member)
                                        }
                                        |> ItemActions.withTestId "listing-item_action_remove"
                                    , True
                                    )
                                  ]
                                ]
                        , model = memberMenu member.uuid model
                        , toMsg = MemberMenuMsg member.uuid
                        }
                    )
                    row

            else
                ListItem.withFragments [ Text.muted membership ] row
    in
    ListItem.item title
        |> ListItem.withIcon (ItemIcon.toHtml (avatar member))
        |> ListItem.withDescription (Maybe.withDefault "" member.affiliation)
        |> ListItem.withTestId ("user-group-members_item_" ++ Uuid.toString member.uuid)
        |> withMembership


{-| Adding a member: the user, chosen from the members of the group's workspace
who are not in it yet, and what they are in it, an ordinary member to begin with.
-}
addModal : AppState -> Model -> Html Msg
addModal appState model =
    Modal.plain
        { title = gettext "Add member" appState.locale
        , open = model.addOpen
        , onClose = SetAddOpen False
        , body =
            [ ApiErrorAlert.view appState model.adding
            , TypeHint.view appState
                { id = addMemberFieldId
                , label = gettext "User" appState.locale
                , placeholder = gettext "Select a user" appState.locale
                , required = True
                , error =
                    if model.addUserMissing then
                        Just (gettext "Choose the user to add." appState.locale)

                    else
                        Nothing
                , hint = Nothing
                , clearable = True
                , viewItem =
                    \user ->
                        { label = user.firstName ++ " " ++ user.lastName
                        , code = Nothing
                        , badges = []
                        , description = Just user.email
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
            , UiField.select
                { id = "user-group-add-member-membership"
                , label = gettext "Membership" appState.locale
                , value = UserGroupMembershipType.toString model.addMembershipType
                , options =
                    List.map
                        (\membershipType -> ( UserGroupMembershipType.toString membershipType, membershipLabel appState membershipType ))
                        membershipTypes
                , onInput = SetAddMembershipType
                }
                |> UiField.toHtml
            ]
        }
        |> Modal.withForm { onSubmit = Add, submitLabel = gettext "Add" appState.locale }
        |> Modal.withLoading (ActionResult.isLoading model.adding)
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withTestId "user-group-member-add"
        |> Modal.toHtml


removeModal : AppState -> Model -> Html Msg
removeModal appState model =
    let
        -- The same element whether or not a member is chosen: the dialog must
        -- not gain or lose children while it is on the page.
        entity =
            EntityCard.slot
                (Maybe.map
                    (\member ->
                        { icon = avatar member
                        , name = member.firstName ++ " " ++ member.lastName
                        , badges = [ Badge.neutral (membershipLabel appState member.membershipType) ]
                        , code = Nothing
                        , detail = member.affiliation
                        }
                    )
                    model.memberToBeRemoved
                )
    in
    ConfirmModal.dangerConfirm appState
        { title = gettext "Remove member" appState.locale
        , open = Maybe.isJust model.memberToBeRemoved
        , body =
            [ Text.body (gettext "Are you sure you want to remove this member from the user group? They lose the access the group gives them to projects." appState.locale)
            , entity
            ]
        , confirmLabel = gettext "Remove" appState.locale
        , onConfirm = Remove
        , onClose = ShowHideRemove Nothing
        , result = model.removingMember
        , testId = Just "user-group-member-remove"
        }


avatar : UserGroupMember -> ItemIcon.ItemIcon
avatar member =
    UserAvatar.user
        { firstName = member.firstName
        , lastName = member.lastName
        , imageUrl = member.imageUrl
        , gravatarHash = Just member.gravatarHash
        }


membershipTypes : List UserGroupMembershipType
membershipTypes =
    [ Owner, Member ]


membershipLabel : AppState -> UserGroupMembershipType -> String
membershipLabel appState membershipType =
    case membershipType of
        Owner ->
            gettext "Owner" appState.locale

        Member ->
            gettext "Member" appState.locale


membershipTestId : UserGroupMembershipType -> String
membershipTestId membershipType =
    case membershipType of
        Owner ->
            "listing-item_action_membership-owner"

        Member ->
            "listing-item_action_membership-member"


{-| What the page is about, by name, for the window's title, once it has
arrived.
-}
entityName : Model -> Maybe String
entityName model =
    Maybe.map .name (ActionResult.toMaybe model.userGroup)
