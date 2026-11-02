module Common.Pages.Tenants.Detail exposing
    ( Extension
    , Model
    , Msg
    , Server
    , entityName
    , gotTenant
    , gotUsage
    , init
    , initialModel
    , subscriptions
    , update
    , updateWith
    , view
    , viewWith
    )

{-| One tenant: its usage and limits, its URLs and admins, and editing it.

The tenant and its usage are two requests, loaded side by side: a
usage that cannot be had says so where the usage would be, and the tenant is
shown all the same.

A client can show the same page with more on it. None of that is here: it
keeps its own state beside this page's and hands in what to
draw, as data: `viewWith` takes the extra content (`Extension`), and
`gotTenant` and `gotUsage` let it load the tenant and the usage itself, together
with its own fields of the same responses, and give this page its part.
`updateWith` takes how its server differs (`Server`): how the tenant and the
usage are loaded again after a save, and the fields it takes beside the form's
when the tenant is changed.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.TenantDetail exposing (TenantDetail)
import Common.Api.Models.TenantState as TenantState exposing (TenantState)
import Common.Api.Models.Usage exposing (Usage)
import Common.Api.Models.User as User exposing (User)
import Common.Api.Request exposing (ToMsg)
import Common.Api.Tenants as TenantsApi
import Common.Components.DetailPage as DetailPage
import Common.Components.Form as FormLayout
import Common.Components.Form.Field as Field
import Common.Components.Page as Page
import Common.Components.UsageTable as UsageTable
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.TenantEditForm as TenantEditForm exposing (TenantEditForm)
import Common.Data.Forms.TenantLimitsForm as TenantLimitsForm exposing (TenantLimitsForm)
import Common.Data.NotFound as NotFound
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Data.UuidOrCurrent as UuidOrCurrent
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.TimeUtils as TimeUtils
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Json.Encode as E
import Maybe.Extra as Maybe
import Ui.Alert as Alert
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.DescriptionList as DescriptionList
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.ListItem as ListItem
import Ui.Modal as Modal
import Ui.Resizer as Resizer
import Ui.Text as Text
import Uuid exposing (Uuid)


type alias Model =
    { uuid : Uuid
    , tenant : ActionResult TenantDetail
    , usage : ActionResult Usage
    , editForm : Maybe (Form FormError TenantEditForm)
    , limitsForm : Maybe (Form FormError TenantLimitsForm)
    , savingTenant : ActionResult String
    , resizer : Resizer.Model
    }


type Msg
    = GetTenantComplete (Result ApiError TenantDetail)
    | GetUsageComplete (Result ApiError Usage)
    | EditModalOpen
    | EditModalClose
    | EditModalFormMsg Form.Msg
    | PutTenantComplete (Result ApiError ())
    | EditLimitsModalOpen
    | EditLimitsModalClose
    | EditLimitsModalFormMsg Form.Msg
    | PutTenantLimitsComplete (Result ApiError ())
    | ResizerMsg Resizer.Msg


initialModel : AppState -> Uuid -> Model
initialModel appState uuid =
    { uuid = uuid
    , tenant = Loading
    , usage = Loading
    , editForm = Nothing
    , limitsForm = Nothing
    , savingTenant = Unset
    , resizer = Resizer.init (DetailPage.resizerConfig appState)
    }


init : AppState -> Uuid -> ( Model, Cmd Msg )
init appState uuid =
    ( initialModel appState uuid
    , Cmd.batch
        [ TenantsApi.getTenant appState uuid GetTenantComplete
        , TenantsApi.getTenantUsage appState (UuidOrCurrent.uuid uuid) GetUsageComplete
        ]
    )


{-| How a server differs from the one this page expects: how the tenant and
its usage are loaded, and the fields it takes beside the form's when the tenant
is changed, as they are now.
-}
type alias Server =
    { getTenant : ToMsg TenantDetail Msg -> Cmd Msg
    , getUsage : ToMsg Usage Msg -> Cmd Msg
    , editFields : List ( String, E.Value )
    }


defaultServer : AppState -> Uuid -> Server
defaultServer appState uuid =
    { getTenant = TenantsApi.getTenant appState uuid
    , getUsage = TenantsApi.getTenantUsage appState (UuidOrCurrent.uuid uuid)
    , editFields = []
    }


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    updateWith (defaultServer appState model.uuid) cfg appState msg model


updateWith : Server -> UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
updateWith server cfg appState msg model =
    case msg of
        GetTenantComplete result ->
            case result of
                Ok tenant ->
                    ( { model | tenant = Success tenant }, Cmd.none )

                Err error ->
                    if ApiError.isNotFound error then
                        ( model
                        , UpdateConfig.notFound cfg (NotFound.tenant appState)
                        )

                    else
                        ( { model | tenant = ApiError.toActionResult appState (gettext "Unable to get tenant." appState.locale) error }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

        GetUsageComplete result ->
            case result of
                Ok usage ->
                    ( { model | usage = Success usage }, Cmd.none )

                Err error ->
                    ( { model | usage = ApiError.toActionResult appState (gettext "Unable to get usage." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        EditModalOpen ->
            ( { model
                | editForm = ActionResult.unwrap Nothing (Just << TenantEditForm.init) model.tenant
                , savingTenant = Unset
              }
            , Cmd.none
            )

        EditModalClose ->
            ( { model | editForm = Nothing }, Cmd.none )

        EditModalFormMsg formMsg ->
            case model.editForm of
                Just form ->
                    case ( formMsg, Form.getOutput form ) of
                        ( Form.Submit, Just tenantEditForm ) ->
                            ( { model | savingTenant = Loading }
                            , TenantsApi.putTenant appState model.uuid (TenantEditForm.encodeWith server.editFields tenantEditForm) (cfg.wrapMsg << PutTenantComplete)
                            )

                        _ ->
                            ( { model | editForm = Just (Form.update TenantEditForm.validation formMsg form) }
                            , Cmd.none
                            )

                Nothing ->
                    ( model, Cmd.none )

        PutTenantComplete result ->
            case result of
                Ok _ ->
                    reloadTenant server cfg { model | editForm = Nothing }

                Err error ->
                    ( { model
                        | savingTenant = ApiError.toActionResult appState (gettext "Tenant could not be saved." appState.locale) error
                        , editForm = Maybe.map (FormUtils.setFormErrors appState error) model.editForm
                      }
                    , Cmd.batch [ RequestHelpers.getResultCmd cfg.logoutMsg result, FormLayout.focusErrorSummary ]
                    )

        EditLimitsModalOpen ->
            ( { model
                | limitsForm = ActionResult.unwrap Nothing (Just << TenantLimitsForm.init) model.usage
                , savingTenant = Unset
              }
            , Cmd.none
            )

        EditLimitsModalClose ->
            ( { model | limitsForm = Nothing }, Cmd.none )

        EditLimitsModalFormMsg formMsg ->
            case model.limitsForm of
                Just form ->
                    case ( formMsg, Form.getOutput form ) of
                        ( Form.Submit, Just tenantLimitsForm ) ->
                            ( { model | savingTenant = Loading }
                            , TenantsApi.putTenantLimits appState model.uuid (TenantLimitsForm.encode tenantLimitsForm) (cfg.wrapMsg << PutTenantLimitsComplete)
                            )

                        _ ->
                            ( { model | limitsForm = Just (Form.update TenantLimitsForm.validation formMsg form) }
                            , Cmd.none
                            )

                Nothing ->
                    ( model, Cmd.none )

        PutTenantLimitsComplete result ->
            case result of
                Ok _ ->
                    -- The limits are the usage's maxima: the usage is what changed.
                    ( { model | limitsForm = Nothing, savingTenant = Unset }
                    , Cmd.map cfg.wrapMsg (server.getUsage GetUsageComplete)
                    )

                Err error ->
                    ( { model
                        | savingTenant = ApiError.toActionResult appState (gettext "Tenant limits could not be saved." appState.locale) error
                        , limitsForm = Maybe.map (FormUtils.setFormErrors appState error) model.limitsForm
                      }
                    , Cmd.batch [ RequestHelpers.getResultCmd cfg.logoutMsg result, FormLayout.focusErrorSummary ]
                    )

        ResizerMsg resizerMsg ->
            ( { model | resizer = Resizer.update resizerMsg model.resizer }, Cmd.none )


{-| The tenant, as loaded by whoever wraps this page — the message this page
sends itself when it loads the tenant.
-}
gotTenant : Result ApiError TenantDetail -> Msg
gotTenant =
    GetTenantComplete


{-| The tenant's usage, as loaded by whoever wraps this page.
-}
gotUsage : Result ApiError Usage -> Msg
gotUsage =
    GetUsageComplete


{-| After a save the wizard navigated to the same detail route, which fetched
the tenant again. Here the modal closes (returning focus to the button that
opened it) and the tenant is fetched in place, without the page going through
its loading state.
-}
reloadTenant : Server -> UpdateConfig Msg msg -> Model -> ( Model, Cmd msg )
reloadTenant server cfg model =
    ( { model | savingTenant = Unset }
    , Cmd.map cfg.wrapMsg (server.getTenant GetTenantComplete)
    )


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    let
        formShortcut =
            case ( model.editForm, model.limitsForm ) of
                ( Just _, _ ) ->
                    FormLayout.submitShortcut appState { result = model.savingTenant, onSubmit = EditModalFormMsg Form.Submit }

                ( _, Just _ ) ->
                    FormLayout.submitShortcut appState { result = model.savingTenant, onSubmit = EditLimitsModalFormMsg Form.Submit }

                _ ->
                    Sub.none
    in
    Sub.batch
        [ Sub.map ResizerMsg (Resizer.subscriptions model.resizer)
        , formShortcut
        ]



-- VIEW


view : AppState -> Model -> Html Msg
view appState =
    viewWith appState { usageRows = [], content = [], editLimits = True } identity


{-| What a wrapping page adds: `usageRows` at the end of the usage table (the
usage of its own modules), `content` after the table, and whether the tenant's
limits are edited here.
-}
type alias Extension msg =
    { usageRows : List UsageTable.Row
    , content : List (Html msg)
    , editLimits : Bool
    }


{-| The page, inside another one's messages, with its additions.
-}
viewWith : AppState -> Extension msg -> (Msg -> msg) -> Model -> Html msg
viewWith appState extension toMsg model =
    DetailPage.view appState (detailPage appState extension toMsg model) model.tenant


detailPage : AppState -> Extension msg -> (Msg -> msg) -> Model -> TenantDetail -> DetailPage.DetailPage msg
detailPage appState extension toMsg model tenant =
    let
        editLimits =
            if extension.editLimits then
                [ Button.secondary (gettext "Edit limits" appState.locale) (toMsg EditLimitsModalOpen)
                    |> Button.withIcon (Icon.solid "fa-sliders")
                    |> Button.withTestId "tenant-detail_edit-limits"
                    |> (if ActionResult.isSuccess model.usage then
                            identity

                        else
                            -- The form starts from the limits the usage says.
                            Button.withDisabledReason
                                { id = "tenant-detail_edit-limits-reason"
                                , reason = gettext "The limits can be edited once the usage has loaded." appState.locale
                                }
                       )
                ]

            else
                []

        actions =
            (Button.secondary (gettext "Edit" appState.locale) (toMsg EditModalOpen)
                |> Button.withIcon (Icon.solid "fa-pen-to-square")
                |> Button.withTestId "tenant-detail_edit"
            )
                :: editLimits

        modals =
            List.map (Html.map toMsg)
                (viewEditModal appState model
                    :: (if extension.editLimits then
                            [ viewEditLimitsModal appState model ]

                        else
                            []
                       )
                )
    in
    DetailPage.detailPage
        { title = tenant.name
        , content =
            [ Layout.stack
                ([ Text.title (gettext "Usage" appState.locale)
                 , case model.usage of
                    Success usage ->
                        UsageTable.view appState
                            { showSoftLimits = True
                            , rows = usageRows appState usage ++ extension.usageRows
                            }

                    -- Said in its part of the page: the tenant is shown all the same.
                    Error message ->
                        Alert.toHtml (Alert.danger message)

                    _ ->
                        Page.actionResultWithinPage appState (always (Html.text "")) model.usage
                 ]
                    ++ extension.content
                    ++ modals
                )
            ]
        }
        |> DetailPage.withTitleIcon (ItemIcon.toHtml (UserAvatar.tenant { name = tenant.name, logoUrl = tenant.logoUrl }))
        |> DetailPage.withBadges (enabledBadge appState tenant.enabled :: stateBadges appState tenant.state)
        |> DetailPage.withActions actions
        |> DetailPage.withSidePanel
            { resizer = model.resizer
            , toMsg = toMsg << ResizerMsg
            , sections =
                [ sidePanelInfo appState tenant
                , sidePanelUrls appState tenant
                , sidePanelAdmins appState tenant
                ]
            }


usageRows : AppState -> Usage -> List UsageTable.Row
usageRows appState usage =
    [ UsageTable.count "users" (gettext "Users" appState.locale) usage.users
    , UsageTable.count "active-users" (gettext "Active Users" appState.locale) usage.activeUsers
    , UsageTable.count "knowledge-model-editors" (gettext "Knowledge Model Editors" appState.locale) usage.knowledgeModelEditors
    , UsageTable.count "knowledge-models" (gettext "Knowledge Models" appState.locale) usage.knowledgeModels
    , UsageTable.count "document-template-editors" (gettext "Document Template Editors" appState.locale) usage.documentTemplateDrafts
    , UsageTable.count "document-templates" (gettext "Document Templates" appState.locale) usage.documentTemplates
    , UsageTable.count "projects" (gettext "Projects" appState.locale) usage.projects
    , UsageTable.count "documents" (gettext "Documents" appState.locale) usage.documents
    , UsageTable.bytes "storage" (gettext "Storage" appState.locale) usage.storage
    ]


enabledBadge : AppState -> Bool -> Badge.Badge msg
enabledBadge appState enabled =
    if enabled then
        Badge.success (gettext "Enabled" appState.locale)

    else
        Badge.danger (gettext "Disabled" appState.locale)


{-| Only a state that is not the normal one is worth a badge beside the name;
the side panel always says which state it is.
-}
stateBadges : AppState -> TenantState -> List (Badge.Badge msg)
stateBadges appState state =
    if state == TenantState.ReadyForUse then
        []

    else
        [ Badge.warning (stateLabel appState state) ]


stateLabel : AppState -> TenantState -> String
stateLabel appState state =
    case state of
        TenantState.NotSeeded ->
            gettext "Not seeded" appState.locale

        TenantState.PendingHousekeeping ->
            gettext "Pending housekeeping" appState.locale

        TenantState.HousekeepingInProgress ->
            gettext "Housekeeping in progress" appState.locale

        TenantState.ReadyForUse ->
            gettext "Ready for use" appState.locale


sidePanelInfo : AppState -> TenantDetail -> DetailPage.Section msg
sidePanelInfo appState tenant =
    DetailPage.section (gettext "Info" appState.locale)
        [ DescriptionList.entry { term = gettext "Tenant ID" appState.locale, description = tenant.tenantId }
        , DescriptionList.withHtml
            { term = gettext "Enabled" appState.locale
            , description = [ Badge.toHtml (enabledBadge appState tenant.enabled) ]
            }
        , DescriptionList.entry { term = gettext "State" appState.locale, description = stateLabel appState tenant.state }
        , DescriptionList.entry { term = gettext "Created at" appState.locale, description = TimeUtils.toReadableDateTime appState.timeZone tenant.createdAt }
        , DescriptionList.entry { term = gettext "Updated at" appState.locale, description = TimeUtils.toReadableDateTime appState.timeZone tenant.updatedAt }
        ]


sidePanelUrls : AppState -> TenantDetail -> DetailPage.Section msg
sidePanelUrls appState tenant =
    let
        externalLink url =
            Link.link { label = url, url = url }
                |> Link.external
                |> Link.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                |> Link.toHtml
    in
    DetailPage.section (gettext "URLs" appState.locale)
        [ DescriptionList.withHtml { term = gettext "Client URL" appState.locale, description = [ externalLink tenant.clientUrl ] }
        , DescriptionList.withHtml { term = gettext "Server URL" appState.locale, description = [ externalLink tenant.serverUrl ] }
        ]


sidePanelAdmins : AppState -> TenantDetail -> DetailPage.Section msg
sidePanelAdmins appState tenant =
    let
        viewUser : User -> ListItem.ListItem msg
        viewUser user =
            let
                inactiveBadge =
                    if user.active then
                        []

                    else
                        [ Badge.toHtml (Badge.danger (gettext "inactive" appState.locale)) ]
            in
            ListItem.item (Layout.inline (Html.text (User.fullName user) :: inactiveBadge))
                |> ListItem.withIcon
                    (UserAvatar.user
                        { firstName = user.firstName
                        , lastName = user.lastName
                        , imageUrl = user.imageUrl
                        , gravatarHash = UserAvatar.emailHash user.email
                        }
                        |> ItemIcon.toHtml
                    )
                |> ListItem.withFragments [ Html.text user.email ]
                |> ListItem.withTestId ("tenant-admins_item_" ++ Uuid.toString user.uuid)

        users =
            List.sortWith User.compare tenant.users
    in
    DetailPage.sectionHtml (gettext "Admins" appState.locale)
        (if List.isEmpty users then
            []

         else
            [ ListItem.list (List.map viewUser users) ]
        )


{-| Both modals share the request state; only the open one shows it, so the
error summary's id is on one element at a time.
-}
resultWhen : Bool -> ActionResult String -> ActionResult String
resultWhen open result =
    if open then
        result

    else
        Unset


{-| The modal's children stay the same for its whole life (`Ui.Modal`): the
error slot and the fields are always there, over an empty form while closed.
-}
viewEditModal : AppState -> Model -> Html Msg
viewEditModal appState model =
    let
        form =
            Maybe.withDefault (Form.initial [] TenantEditForm.validation) model.editForm

        field toField path label =
            toField appState form path label
                |> Field.withRequired True
                |> Field.toHtml
                |> Html.map EditModalFormMsg
    in
    Modal.plain
        { title = gettext "Edit tenant" appState.locale
        , open = Maybe.isJust model.editForm
        , onClose = EditModalClose
        , body =
            [ div [] [ FormLayout.errorSummary (resultWhen (Maybe.isJust model.editForm) model.savingTenant) ]
            , field Field.input "tenantId" (gettext "Tenant ID" appState.locale)
            , field Field.input "name" (gettext "Name" appState.locale)
            ]
        }
        |> Modal.withForm { onSubmit = EditModalFormMsg Form.Submit, submitLabel = gettext "Save" appState.locale }
        |> Modal.withLoading (ActionResult.isLoading model.savingTenant)
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withTestId "tenant-edit"
        |> Modal.toHtml


viewEditLimitsModal : AppState -> Model -> Html Msg
viewEditLimitsModal appState model =
    let
        form =
            Maybe.withDefault (Form.initial [] TenantLimitsForm.validation) model.limitsForm

        field toField path label =
            toField appState form path label
                |> Field.withRequired True
                |> Field.toHtml
                |> Html.map EditLimitsModalFormMsg
    in
    Modal.plain
        { title = gettext "Edit limits" appState.locale
        , open = Maybe.isJust model.limitsForm
        , onClose = EditLimitsModalClose
        , body =
            [ div [] [ FormLayout.errorSummary (resultWhen (Maybe.isJust model.limitsForm) model.savingTenant) ]
            , field Field.number "users" (gettext "Users" appState.locale)
            , field Field.number "activeUsers" (gettext "Active Users" appState.locale)
            , field Field.number "knowledgeModelEditors" (gettext "Knowledge Model Editors" appState.locale)
            , field Field.number "knowledgeModels" (gettext "Knowledge Models" appState.locale)
            , field Field.number "documentTemplateDrafts" (gettext "Document Template Editors" appState.locale)
            , field Field.number "documentTemplates" (gettext "Document Templates" appState.locale)
            , field Field.number "questionnaires" (gettext "Projects" appState.locale)
            , field Field.number "documents" (gettext "Documents" appState.locale)
            , field Field.fileSize "storage" (gettext "Storage" appState.locale)
            ]
        }
        |> Modal.withForm { onSubmit = EditLimitsModalFormMsg Form.Submit, submitLabel = gettext "Save" appState.locale }
        |> Modal.withLoading (ActionResult.isLoading model.savingTenant)
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withTestId "tenant-edit-limits"
        |> Modal.toHtml


{-| What the page is about, by name, for the window's title, once it has
arrived.
-}
entityName : Model -> Maybe String
entityName model =
    Maybe.map .name (ActionResult.toMaybe model.tenant)
