module Common.Pages.Dev.PersistentCommandsDetail exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.PersistentCommand as PersistentCommand
import Common.Api.Models.PersistentCommand.PersistentCommandState as PersistentCommandState
import Common.Api.Models.PersistentCommandDetail exposing (PersistentCommandDetail)
import Common.Api.PersistentCommands as PersistentCommandsApi
import Common.Components.DetailPage as DetailPage
import Common.Components.ItemActions as ItemActions
import Common.Components.PersistentCommand as PersistentCommandBadge
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.NotFound as NotFound
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.Dev.Components.PersistentCommandActions as PersistentCommandActions
import Common.Routes as Routes
import Common.Utils.JsonUtils as JsonUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.TimeUtils as TimeUtils
import Gettext exposing (gettext)
import Html exposing (Html)
import List.Extra as List
import String.Format as String
import Ui.Alert as Alert
import Ui.CodeBlock as CodeBlock
import Ui.DescriptionList as DescriptionList
import Ui.ItemIcon as ItemIcon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.ListItem as ListItem
import Ui.Menu as Menu
import Ui.Resizer as Resizer
import Ui.Text as Text
import Uuid exposing (Uuid)


type alias Model =
    { uuid : Uuid
    , persistentCommand : ActionResult PersistentCommandDetail
    , updating : ActionResult String
    , actionsMenu : Menu.Model
    , resizer : Resizer.Model
    }


type Msg
    = GetPersistentCommandComplete (Result ApiError PersistentCommandDetail)
    | ActionsMenuMsg Menu.Msg
    | RerunCommand
    | RerunCommandComplete (Result ApiError ())
    | SetIgnored
    | SetIgnoredComplete (Result ApiError ())
    | ResizerMsg Resizer.Msg


initialModel : AppState -> Uuid -> Model
initialModel appState uuid =
    { uuid = uuid
    , persistentCommand = Loading
    , updating = Unset
    , actionsMenu = Menu.init
    , resizer = Resizer.init (DetailPage.resizerConfig appState)
    }


init : AppState -> Uuid -> ( Model, Cmd Msg )
init appState uuid =
    ( initialModel appState uuid
    , PersistentCommandsApi.getPersistentCommand appState uuid GetPersistentCommandComplete
    )


actionsMenuId : String
actionsMenuId =
    "persistent-command-detail-actions"


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetPersistentCommandComplete result ->
            case result of
                Ok persistentCommand ->
                    ( { model | persistentCommand = Success persistentCommand }, Cmd.none )

                Err error ->
                    if ApiError.isNotFound error then
                        ( model
                        , UpdateConfig.notFound cfg (NotFound.persistentCommand appState)
                        )

                    else
                        ( { model | persistentCommand = ApiError.toActionResult appState (gettext "Unable to get persistent command." appState.locale) error }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

        ActionsMenuMsg menuMsg ->
            let
                count =
                    ActionResult.unwrap 0 (ItemActions.focusableCount << detailActions appState) model.persistentCommand

                ( actionsMenu, menuCmd ) =
                    Menu.update actionsMenuId count menuMsg model.actionsMenu
            in
            ( { model | actionsMenu = actionsMenu }
            , Cmd.map (cfg.wrapMsg << ActionsMenuMsg) menuCmd
            )

        RerunCommand ->
            ( { model | updating = Loading }
            , PersistentCommandsApi.retry appState model.uuid (cfg.wrapMsg << RerunCommandComplete)
            )

        RerunCommandComplete result ->
            case result of
                Ok _ ->
                    ( { model | updating = Success (gettext "Persistent command has been scheduled for the rerun." appState.locale) }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | updating = ApiError.toActionResult appState (gettext "Unable to rerun persistent command." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        SetIgnored ->
            ( { model | updating = Loading }
            , PersistentCommandsApi.updateState appState model.uuid PersistentCommandState.Ignore (cfg.wrapMsg << SetIgnoredComplete)
            )

        SetIgnoredComplete result ->
            case model.persistentCommand of
                Success persistentCommand ->
                    case result of
                        Ok _ ->
                            ( { model
                                | persistentCommand = Success { persistentCommand | state = PersistentCommandState.Ignore }
                                , updating = Success (gettext "Persistent command is now being ignored." appState.locale)
                              }
                            , Cmd.none
                            )

                        Err error ->
                            ( { model | updating = ApiError.toActionResult appState (gettext "Unable to set persistent command ignored." appState.locale) error }
                            , RequestHelpers.getResultCmd cfg.logoutMsg result
                            )

                _ ->
                    ( model, Cmd.none )

        ResizerMsg resizerMsg ->
            ( { model | resizer = Resizer.update resizerMsg model.resizer }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ActionsMenuMsg (Menu.subscriptions actionsMenuId model.actionsMenu)
        , Sub.map ResizerMsg (Resizer.subscriptions model.resizer)
        ]



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    DetailPage.view appState (detailPage appState model) model.persistentCommand


detailActions : AppState -> PersistentCommandDetail -> List (ItemActions.Action Msg)
detailActions appState =
    PersistentCommandActions.actions appState
        { retryMsg = always RerunCommand
        , setIgnoredMsg = always SetIgnored
        , viewActionVisible = False
        }


detailPage : AppState -> Model -> PersistentCommandDetail -> DetailPage.DetailPage Msg
detailPage appState model persistentCommand =
    let
        name =
            PersistentCommand.visibleName persistentCommand

        updatingResult =
            case model.updating of
                Success message ->
                    Alert.toHtml (Alert.success message)

                Error message ->
                    Alert.toHtml (Alert.danger message)

                _ ->
                    Html.text ""

        body =
            Result.withDefault persistentCommand.body (JsonUtils.prettyString 4 persistentCommand.body)

        lastError =
            case persistentCommand.lastErrorMessage of
                Just lastErrorMessage ->
                    [ Text.title (gettext "Last Error Message" appState.locale)
                    , CodeBlock.toHtml
                        { label = gettext "Last Error Message" appState.locale
                        , language = ""
                        , value = lastErrorMessage
                        }
                    ]

                Nothing ->
                    []
    in
    DetailPage.detailPage
        { title = name
        , content =
            [ Layout.stack
                (updatingResult
                    :: lastError
                    ++ [ Text.title (gettext "Body" appState.locale)
                       , CodeBlock.fill
                            { label = gettext "Body" appState.locale
                            , language = ""
                            , value = body
                            }
                       ]
                )
            ]
        }
        |> DetailPage.withFillContent
        |> DetailPage.withBadges [ PersistentCommandBadge.badge appState persistentCommand.state ]
        |> DetailPage.withMoreActions
            { id = actionsMenuId
            , label = String.format (gettext "Actions for %s" appState.locale) [ name ]
            , actions = detailActions appState persistentCommand
            , model = model.actionsMenu
            , toMsg = ActionsMenuMsg
            }
        |> DetailPage.withSidePanel
            { resizer = model.resizer
            , toMsg = ResizerMsg
            , sections = sidePanelSections appState persistentCommand
            }


sidePanelSections : AppState -> PersistentCommandDetail -> List (DetailPage.Section Msg)
sidePanelSections appState persistentCommand =
    let
        tenantUrl =
            persistentCommand.tenant.clientUrl
                |> String.split "://"
                |> List.getAt 1
                |> Maybe.withDefault ""

        tenantItem =
            ListItem.item
                (Link.toHtml
                    (Link.link
                        { label = persistentCommand.tenant.name
                        , url = Routes.toUrl appState (Routes.tenantsDetail persistentCommand.tenant.uuid)
                        }
                    )
                )
                |> ListItem.withIcon
                    (ItemIcon.toHtml
                        (UserAvatar.tenant { name = persistentCommand.tenant.name, logoUrl = persistentCommand.tenant.logoUrl })
                    )
                |> ListItem.withFragments [ Text.muted tenantUrl ]
                |> ListItem.withTestId ("persistent-command-tenant_item_" ++ Uuid.toString persistentCommand.tenant.uuid)

        lastTraceUuidSection =
            case persistentCommand.lastTraceUuid of
                Just lastTraceUuid ->
                    DetailPage.sectionHtml (gettext "Last Trace UUID" appState.locale)
                        [ Text.code (Uuid.toString lastTraceUuid) ]

                Nothing ->
                    DetailPage.sectionHtml (gettext "Last Trace UUID" appState.locale) []

        createdBySection =
            DetailPage.sectionHtml (gettext "Created by" appState.locale)
                (case persistentCommand.createdBy of
                    Just createdBy ->
                        [ ListItem.list
                            [ ListItem.item (Html.text (createdBy.firstName ++ " " ++ createdBy.lastName))
                                |> ListItem.withIcon
                                    (UserAvatar.user
                                        { firstName = createdBy.firstName
                                        , lastName = createdBy.lastName
                                        , imageUrl = createdBy.imageUrl
                                        , gravatarHash = Just createdBy.gravatarHash
                                        }
                                        |> ItemIcon.toHtml
                                    )
                                |> ListItem.withTestId ("persistent-command-created-by_item_" ++ Uuid.toString createdBy.uuid)
                            ]
                        ]

                    Nothing ->
                        []
                )
    in
    [ DetailPage.section (gettext "Persistent Command" appState.locale)
        [ DescriptionList.entry
            { term = gettext "Attempts" appState.locale
            , description = String.fromInt persistentCommand.attempts ++ "/" ++ String.fromInt persistentCommand.maxAttempts
            }
        , DescriptionList.entry
            { term = gettext "Created at" appState.locale
            , description = TimeUtils.toReadableDateTime appState.timeZone persistentCommand.createdAt
            }
        , DescriptionList.entry
            { term = gettext "Updated at" appState.locale
            , description = TimeUtils.toReadableDateTime appState.timeZone persistentCommand.updatedAt
            }
        ]
    , DetailPage.sectionHtml (gettext "Tenant" appState.locale) [ ListItem.list [ tenantItem ] ]
    , lastTraceUuidSection
    , createdBySection
    ]
