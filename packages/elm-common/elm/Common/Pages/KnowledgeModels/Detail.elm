module Common.Pages.KnowledgeModels.Detail exposing
    ( Model
    , Msg
    , entityName
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| The detail of one knowledge model package.

The readme and the locales policy are tabs, and the tabs are routes. Switching
between them is a link to the other route, which the application recognizes as
the same detail (`Common.Routes.isSameListingRoute`) and does not re-initialize,
so the package is loaded once.

The page is reachable without logging in. Every action is gated by a `Feature`
predicate, so an anonymous visitor sees only those meant for them.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom as BrowserDom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.Models.BootstrapConfig.RegistryConfig as RegistryConfig
import Common.Api.Models.KnowledgeModelLocale exposing (KnowledgeModelLocale)
import Common.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage
import Common.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Common.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Common.Api.Models.VersionUuid as VersionUuid
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.DetailPage as DetailPage
import Common.Components.ItemActions as ItemActions
import Common.Components.LibraryDeleteModal as DeleteModal
import Common.Components.Markdown as Markdown
import Common.Components.RowNotice as RowNotice
import Common.Data.AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.NotFound as NotFound
import Common.Data.RowPolicy exposing (RowPolicy)
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.KnowledgeModels.Components.ImportLocaleModal as ImportLocaleModal
import Common.Pages.KnowledgeModels.Components.KnowledgeModelActions as KnowledgeModelActions
import Common.Pages.KnowledgeModels.Detail.Routes as KnowledgeModelDetailRoute exposing (KnowledgeModelDetailRoute)
import Common.Ports.Dom as Dom
import Common.Routes as Routes
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setKnowledgeModelPackage)
import Common.Utils.TimeUtils as TimeUtils
import File.Download as Download
import Gettext exposing (gettext)
import Html exposing (Html, li, text, ul)
import Html.Attributes exposing (id)
import Maybe.Extra as Maybe
import String.Format as String
import Task
import Ui.Alert as Alert
import Ui.AppShell as AppShell
import Ui.Badge as Badge
import Ui.Button as Button exposing (Button)
import Ui.DescriptionList as DescriptionList
import Ui.EmptyState as EmptyState
import Ui.Icon as Icon
import Ui.Illustration as Illustration
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Menu as Menu
import Ui.Resizer as Resizer
import Ui.Table as Table
import Ui.Text as Text
import Uuid exposing (Uuid)
import Version


type alias Model =
    { knowledgeModelPackage : ActionResult KnowledgeModelPackageDetail
    , actionsMenu : Menu.Model
    , resizer : Resizer.Model
    , deleteModalModel : DeleteModal.Model
    , importLocaleModal : ImportLocaleModal.Model
    , localeToDelete : Maybe KnowledgeModelLocale
    , deletingLocale : ActionResult String
    , showAllVersions : Bool
    }


initialModel : AppState -> Model
initialModel appState =
    { knowledgeModelPackage = Loading
    , actionsMenu = Menu.init
    , resizer = Resizer.init (DetailPage.resizerConfig appState)
    , deleteModalModel = DeleteModal.initialModel False
    , importLocaleModal = ImportLocaleModal.init
    , localeToDelete = Nothing
    , deletingLocale = Unset
    , showAllVersions = False
    }


init : AppState -> Uuid -> ( Model, Cmd Msg )
init appState kmPackageUuid =
    ( initialModel appState
    , KnowledgeModelPackagesApi.getKnowledgeModelPackage appState kmPackageUuid GetKnowledgeModelPackageCompleted
    )


type Msg
    = GetKnowledgeModelPackageCompleted (Result ApiError KnowledgeModelPackageDetail)
    | ActionsMenuMsg Int Menu.Msg
    | ResizerMsg Resizer.Msg
    | OpenImportLocaleModal
    | ImportLocaleModalMsg ImportLocaleModal.Msg
    | ShowDeleteLocale KnowledgeModelLocale
    | HideDeleteLocale
    | DeleteLocale
    | DeleteLocaleCompleted (Result ApiError ())
    | DownloadLocale KnowledgeModelLocale
    | DownloadLocaleCompleted KnowledgeModelLocale (Result ApiError String)
    | DeleteModalMsg DeleteModal.Msg
    | OpenDeleteModal KnowledgeModelPackageDetail
    | UpdatePhase KnowledgeModelPackagePhase
    | UpdatePhaseCompleted KnowledgeModelPackagePhase (Result ApiError ())
    | UpdatePublic Bool
    | UpdatePublicCompleted Bool (Result ApiError ())
    | ExportKnowledgeModelPackage KnowledgeModelPackageDetail
    | ExportKnowledgeModelPackagePot KnowledgeModelPackageDetail
    | ExportCompleted KnowledgeModelActions.ExportMsg
    | ShowAllVersions
    | Noop


actionsMenuId : String
actionsMenuId =
    "km-detail-actions"


otherVersionsId : String
otherVersionsId =
    "km-detail-other-versions"


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetKnowledgeModelPackageCompleted result ->
            RequestHelpers.applyResultOrNotFound
                { setResult = setKnowledgeModelPackage
                , defaultError = gettext "Unable to get the Knowledge Model." appState.locale
                , notFoundCmd = UpdateConfig.notFound cfg (NotFound.knowledgeModel appState)
                , notAllowedCmd = UpdateConfig.notAllowed cfg
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }
                |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, inItsWorkspace cfg result ])

        ActionsMenuMsg count menuMsg ->
            let
                ( actionsMenu, menuCmd ) =
                    Menu.update actionsMenuId count menuMsg model.actionsMenu
            in
            ( { model | actionsMenu = actionsMenu }, Cmd.map (cfg.wrapMsg << ActionsMenuMsg count) menuCmd )

        ResizerMsg resizerMsg ->
            ( { model | resizer = Resizer.update resizerMsg model.resizer }, Cmd.none )

        OpenImportLocaleModal ->
            case model.knowledgeModelPackage of
                Success kmPackage ->
                    ( { model | importLocaleModal = ImportLocaleModal.open kmPackage.uuid }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ImportLocaleModalMsg importLocaleModalMsg ->
            let
                ( importLocaleModal, importLocaleModalCmd, mbImportedLocale ) =
                    ImportLocaleModal.update appState importLocaleModalMsg model.importLocaleModal

                knowledgeModelPackage =
                    case mbImportedLocale of
                        Just locale ->
                            ActionResult.map (\kmPackage -> { kmPackage | locales = kmPackage.locales ++ [ locale ] }) model.knowledgeModelPackage

                        Nothing ->
                            model.knowledgeModelPackage
            in
            ( { model | importLocaleModal = importLocaleModal, knowledgeModelPackage = knowledgeModelPackage }
            , Cmd.map (cfg.wrapMsg << ImportLocaleModalMsg) importLocaleModalCmd
            )

        ShowDeleteLocale locale ->
            ( { model | localeToDelete = Just locale, deletingLocale = Unset }, Cmd.none )

        HideDeleteLocale ->
            ( { model | localeToDelete = Nothing }, Cmd.none )

        DeleteLocale ->
            case ( model.knowledgeModelPackage, model.localeToDelete ) of
                ( Success kmPackage, Just locale ) ->
                    ( { model | deletingLocale = Loading }
                    , KnowledgeModelPackagesApi.deleteLocale appState kmPackage.uuid locale.uuid (cfg.wrapMsg << DeleteLocaleCompleted)
                    )

                _ ->
                    ( model, Cmd.none )

        DeleteLocaleCompleted result ->
            case result of
                Ok _ ->
                    let
                        knowledgeModelPackage =
                            case model.localeToDelete of
                                Just locale ->
                                    ActionResult.map (\kmPackage -> { kmPackage | locales = List.filter (\l -> l.uuid /= locale.uuid) kmPackage.locales }) model.knowledgeModelPackage

                                Nothing ->
                                    model.knowledgeModelPackage
                    in
                    -- The row and its delete button are gone; focus goes to the
                    -- page heading rather than to the document.
                    ( { model | localeToDelete = Nothing, deletingLocale = Success "", knowledgeModelPackage = knowledgeModelPackage }
                    , AppShell.focusOnRouteChange (cfg.wrapMsg Noop)
                    )

                Err error ->
                    ( { model | deletingLocale = ApiError.toActionResult appState (gettext "Deleting the locale failed." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        DownloadLocale locale ->
            case model.knowledgeModelPackage of
                Success kmPackage ->
                    ( model
                    , KnowledgeModelPackagesApi.getLocaleContent appState kmPackage.uuid locale.uuid (cfg.wrapMsg << DownloadLocaleCompleted locale)
                    )

                _ ->
                    ( model, Cmd.none )

        DownloadLocaleCompleted locale result ->
            case result of
                Ok content ->
                    ( model, Download.string (locale.code ++ ".po") "text/x-gettext-translation" content )

                Err _ ->
                    ( model, RequestHelpers.getResultCmd cfg.logoutMsg result )

        DeleteModalMsg deleteModalMsg ->
            let
                deleteModalConfig =
                    { afterDeleteCmd = Routes.navigate appState Routes.knowledgeModelsIndex
                    , afterCloseCmd = focusActionsTrigger cfg
                    , wrapMsg = cfg.wrapMsg << DeleteModalMsg
                    }

                ( deleteModalModel, deleteModalCmd ) =
                    DeleteModal.update appState deleteModalConfig deleteModalMsg model.deleteModalModel
            in
            ( { model | deleteModalModel = deleteModalModel }
            , deleteModalCmd
            )

        OpenDeleteModal kmPackage ->
            -- The dialog takes focus as it opens, so the menu closes without
            -- taking it back; closing the dialog puts it on the menu's trigger.
            let
                ( actionsMenu, menuCmd ) =
                    Menu.update actionsMenuId 0 Menu.closeWithoutFocus model.actionsMenu

                ( newModel, deleteCmd ) =
                    update cfg appState (DeleteModalMsg (DeleteModal.open (DeleteModal.target DeleteModal.KnowledgeModel kmPackage))) { model | actionsMenu = actionsMenu }
            in
            ( newModel, Cmd.batch [ Cmd.map (cfg.wrapMsg << ActionsMenuMsg 0) menuCmd, deleteCmd ] )

        UpdatePhase phase ->
            case model.knowledgeModelPackage of
                Success kmPackage ->
                    closeActionsMenu cfg
                        ( model
                        , KnowledgeModelPackagesApi.putKnowledgeModelPackage appState { kmPackage | phase = phase } (cfg.wrapMsg << UpdatePhaseCompleted phase)
                        )

                _ ->
                    ( model, Cmd.none )

        UpdatePhaseCompleted phase result ->
            case model.knowledgeModelPackage of
                Success kmPackage ->
                    RequestHelpers.applyResultTransform
                        { setResult = setKnowledgeModelPackage
                        , defaultError = gettext "Unable to update the knowledge model." appState.locale
                        , model = model
                        , result = result
                        , logoutMsg = cfg.logoutMsg
                        , transform = always { kmPackage | phase = phase }
                        , locale = appState.locale
                        }

                _ ->
                    ( model, Cmd.none )

        UpdatePublic isPublic ->
            case model.knowledgeModelPackage of
                Success kmPackage ->
                    closeActionsMenu cfg
                        ( model
                        , KnowledgeModelPackagesApi.putKnowledgeModelPackage appState { kmPackage | public = isPublic } (cfg.wrapMsg << UpdatePublicCompleted isPublic)
                        )

                _ ->
                    ( model, Cmd.none )

        UpdatePublicCompleted isPublic result ->
            case model.knowledgeModelPackage of
                Success kmPackage ->
                    RequestHelpers.applyResultTransform
                        { setResult = setKnowledgeModelPackage
                        , defaultError = gettext "Unable to update the knowledge model." appState.locale
                        , model = model
                        , result = result
                        , logoutMsg = cfg.logoutMsg
                        , transform = always { kmPackage | public = isPublic }
                        , locale = appState.locale
                        }

                _ ->
                    ( model, Cmd.none )

        ExportKnowledgeModelPackage kmPackage ->
            closeActionsMenu cfg
                ( model, KnowledgeModelActions.export appState kmPackage.uuid (cfg.wrapMsg << ExportCompleted) )

        ExportKnowledgeModelPackagePot kmPackage ->
            closeActionsMenu cfg
                ( model, KnowledgeModelActions.exportPot appState kmPackage.uuid (cfg.wrapMsg << ExportCompleted) )

        ExportCompleted result ->
            ( model, KnowledgeModelActions.exportUpdate result )

        ShowAllVersions ->
            -- The button goes away; focus moves on to the first version it
            -- revealed, which is where the reader was heading.
            ( { model | showAllVersions = True }
            , Dom.focus ("#" ++ otherVersionsId ++ " li:nth-child(" ++ String.fromInt (maxVersions + 1) ++ ") a")
            )

        Noop ->
            ( model, Cmd.none )


{-| Back to the actions menu once a dialog it opened is dismissed. A task rather
than the port, so it runs after the dialog has closed: until then everything
behind the dialog is inert and cannot take focus.
-}
focusActionsTrigger : UpdateConfig Msg msg -> Cmd msg
focusActionsTrigger cfg =
    Task.attempt (always (cfg.wrapMsg Noop)) (BrowserDom.focus (Menu.triggerId actionsMenuId))


{-| A command chosen from the actions menu leaves the menu open under it unless
it is closed; closing returns focus to the trigger.
-}
closeActionsMenu : UpdateConfig Msg msg -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
closeActionsMenu cfg ( model, cmd ) =
    let
        ( actionsMenu, menuCmd ) =
            Menu.update actionsMenuId 0 Menu.close model.actionsMenu
    in
    ( { model | actionsMenu = actionsMenu }
    , Cmd.batch [ cmd, Cmd.map (cfg.wrapMsg << ActionsMenuMsg 0) menuCmd ]
    )


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    Sub.batch
        [ Sub.map (ActionsMenuMsg 0) (Menu.subscriptions actionsMenuId model.actionsMenu)
        , Sub.map ResizerMsg (Resizer.subscriptions model.resizer)
        , Sub.map ImportLocaleModalMsg (ImportLocaleModal.subscriptions appState model.importLocaleModal)
        ]



-- VIEW


view : RowPolicy -> AppState -> KnowledgeModelDetailRoute -> Model -> Html Msg
view policy appState detailRoute model =
    DetailPage.view appState (detailPage policy appState detailRoute model) model.knowledgeModelPackage


detailPage : RowPolicy -> AppState -> KnowledgeModelDetailRoute -> Model -> KnowledgeModelPackageDetail -> DetailPage.DetailPage Msg
detailPage policy appState detailRoute model kmPackage =
    let
        badges =
            Maybe.values
                [ badgeIf kmPackage.nonEditable (Badge.neutral (gettext "non-editable" appState.locale))
                , badgeIf (kmPackage.phase == KnowledgeModelPackagePhase.Deprecated) (Badge.danger (gettext "deprecated" appState.locale))
                , badgeIf kmPackage.public (Badge.info (gettext "public" appState.locale))
                ]

        actions =
            KnowledgeModelActions.actions policy
                appState
                { exportMsg = always (ExportKnowledgeModelPackage kmPackage)
                , exportPotMsg = always (ExportKnowledgeModelPackagePot kmPackage)
                , updatePhaseMsg = \_ phase -> UpdatePhase phase
                , updatePublicMsg = \_ isPublic -> UpdatePublic isPublic
                , deleteMsg = always (OpenDeleteModal kmPackage)
                , viewActionVisible = False
                }
                kmPackage

        content =
            case detailRoute of
                KnowledgeModelDetailRoute.Readme ->
                    readme appState kmPackage

                KnowledgeModelDetailRoute.Locales ->
                    locales policy appState kmPackage
    in
    DetailPage.detailPage
        { title = kmPackage.name
        , content =
            RowNotice.view (policy.notice kmPackage.workspaceUuid (Routes.toPath (Routes.knowledgeModelsDetail kmPackage.uuid)))
                :: content
                ++ [ Html.map DeleteModalMsg <| DeleteModal.view policy appState model.deleteModalModel
                   , Html.map ImportLocaleModalMsg <| ImportLocaleModal.view appState model.importLocaleModal
                   , deleteLocaleModal appState model
                   ]
        }
        |> DetailPage.withBadges badges
        |> DetailPage.withMoreActions
            { id = actionsMenuId
            , label = String.format (gettext "Actions for %s" appState.locale) [ kmPackage.name ]
            , actions = actions
            , model = model.actionsMenu
            , toMsg = ActionsMenuMsg (ItemActions.focusableCount actions)
            }
        |> DetailPage.withTabsWithTestIds
            { label = gettext "Knowledge Model" appState.locale
            , items =
                [ { label = gettext "Readme" appState.locale
                  , url = Routes.toUrl appState (Routes.knowledgeModelsDetail kmPackage.uuid)
                  , active = detailRoute == KnowledgeModelDetailRoute.Readme
                  , badge = Nothing
                  , testId = Just "km-detail_nav_readme"
                  , icon = Nothing
                  }
                , { label = gettext "Locales" appState.locale
                  , url = Routes.toUrl appState (Routes.knowledgeModelsDetailLocales kmPackage.uuid)
                  , active = detailRoute == KnowledgeModelDetailRoute.Locales
                  , badge = Just (String.fromInt (List.length kmPackage.locales))
                  , testId = Just "km-detail_nav_locales"
                  , icon = Nothing
                  }
                ]
            }
        |> DetailPage.withSidePanel
            { resizer = model.resizer
            , toMsg = ResizerMsg
            , sections =
                [ sidePanelKmInfo appState kmPackage
                , sidePanelRegistryLink appState kmPackage
                , sidePanelOtherVersions appState model kmPackage
                ]
            }


badgeIf : Bool -> Badge.Badge msg -> Maybe (Badge.Badge msg)
badgeIf condition badge =
    if condition then
        Just badge

    else
        Nothing


readme : AppState -> KnowledgeModelPackageDetail -> List (Html Msg)
readme appState kmPackage =
    let
        containsNewerVersions =
            List.any (Version.greaterThan kmPackage.version << .version) kmPackage.versions

        nonEditableInfo =
            if kmPackage.nonEditable then
                Alert.toHtml (Alert.info (gettext "This is a non-editable knowledge model, i.e., it cannot be edited, forked, or exported." appState.locale))

            else
                text ""

        warning =
            if containsNewerVersions then
                Alert.toHtml (Alert.warning (gettext "This is not the latest available version of this knowledge model." appState.locale))

            else
                newVersionInRegistryWarning appState kmPackage
    in
    [ Layout.stack
        [ nonEditableInfo
        , warning
        , Markdown.view { headingOffset = 1 } kmPackage.readme
        ]
    ]


newVersionInRegistryWarning : AppState -> KnowledgeModelPackageDetail -> Html msg
newVersionInRegistryWarning appState kmPackage =
    case ( kmPackage.remoteLatestVersion, KnowledgeModelPackage.isOutdated kmPackage, RegistryConfig.isEnabled appState.config.registry ) of
        ( Just remoteLatestVersion, True, True ) ->
            let
                message =
                    String.format (gettext "There is a newer version (%s) available." appState.locale)
                        [ Version.toString remoteLatestVersion ]

                importLink =
                    if Feature.knowledgeModelsImport appState then
                        let
                            latestPackageId =
                                kmPackage.id ++ ":" ++ Version.toString remoteLatestVersion
                        in
                        [ Button.link (gettext "Import" appState.locale) (Routes.toUrl appState (Routes.knowledgeModelsImport (Just latestPackageId)))
                            |> Button.withIcon (Icon.solid "fa-cloud-arrow-down")
                            |> Button.small
                            |> Button.toHtml
                        ]

                    else
                        []
            in
            Alert.toHtmlWithContent
                [ Layout.inlineBetween (text message :: importLink) ]
                (Alert.warning message)

        _ ->
            text ""


locales : RowPolicy -> AppState -> KnowledgeModelPackageDetail -> List (Html Msg)
locales policy appState kmPackage =
    let
        importButton =
            if Feature.knowledgeModelsImportLocale appState && policy.manage kmPackage.workspaceUuid then
                Layout.inlineEnd
                    [ Button.primary (gettext "Import" appState.locale) OpenImportLocaleModal
                        |> Button.withIcon (Icon.solid "fa-upload")
                        |> Button.withTestId "km-detail_import-locale"
                        |> Button.toHtml
                    ]

            else
                text ""

        localesBody =
            if List.isEmpty kmPackage.locales then
                EmptyState.emptyState
                    { title = gettext "No locales policy" appState.locale
                    , description =
                        gettext "There are no locales policy for this knowledge model yet." appState.locale
                            ++ " "
                            ++ gettext "Export the .pot file to create one." appState.locale
                    }
                    |> EmptyState.withIllustration (Illustration.toHtml Illustration.empty)
                    |> EmptyState.toHtml

            else
                Table.table
                    [ Table.column (gettext "Name" appState.locale) (text << .name)
                    , Table.column (gettext "Code" appState.locale) (Text.code << .code)
                    , Table.actions (gettext "Actions" appState.locale) (viewLocaleActions policy appState kmPackage.workspaceUuid)
                    ]
                    kmPackage.locales
                    |> Table.withRowTestId (\row -> "km-locales_row_" ++ Uuid.toString row.uuid)
                    |> Table.toHtml
    in
    [ Layout.stack [ importButton, localesBody ] ]


viewLocaleActions : RowPolicy -> AppState -> Maybe Uuid -> KnowledgeModelLocale -> List (Button Msg)
viewLocaleActions policy appState rowWorkspaceUuid locale =
    let
        downloadButton =
            if Feature.knowledgeModelsExportLocale appState then
                [ Button.iconOnly (Icon.solid "fa-download") (gettext "Download" appState.locale) (DownloadLocale locale)
                    |> Button.withAccessibleName (String.format (gettext "Download %s" appState.locale) [ locale.name ])
                    |> Button.withTestId "km-detail_locale-download"
                ]

            else
                []

        deleteButton =
            if Feature.knowledgeModelsDeleteLocale appState && policy.manage rowWorkspaceUuid then
                [ Button.dangerIconOnly (Icon.solid "fa-trash") (gettext "Delete" appState.locale) (ShowDeleteLocale locale)
                    |> Button.withAccessibleName (String.format (gettext "Delete %s" appState.locale) [ locale.name ])
                    |> Button.withTestId "km-detail_locale-delete"
                ]

            else
                []
    in
    downloadButton ++ deleteButton


deleteLocaleModal : AppState -> Model -> Html Msg
deleteLocaleModal appState model =
    let
        name =
            Maybe.unwrap "" .name model.localeToDelete
    in
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete locale" appState.locale
        , open = Maybe.isJust model.localeToDelete
        , question = String.format (gettext "Are you sure you want to delete the locale %s?" appState.locale) [ name ]
        , entity = Text.code (Maybe.unwrap "" .code model.localeToDelete)
        , onConfirm = DeleteLocale
        , onClose = HideDeleteLocale
        , result = model.deletingLocale
        , testId = Just "km-locale-delete"
        }



-- SIDE PANEL


sidePanelKmInfo : AppState -> KnowledgeModelPackageDetail -> DetailPage.Section Msg
sidePanelKmInfo appState kmPackage =
    let
        forkOfInfo =
            case kmPackage.forkOfPackageId of
                Just forkOfPackageId ->
                    [ DescriptionList.entry
                        { term = gettext "Fork of" appState.locale
                        , description = forkOfPackageId ++ Maybe.unwrap "" ((++) ":" << Version.toString) kmPackage.forkOfPackageVersion
                        }
                    ]

                Nothing ->
                    []
    in
    DetailPage.section (gettext "Knowledge Model" appState.locale)
        ([ DescriptionList.withHtml
            { term = gettext "ID" appState.locale
            , description =
                [ Text.code (KnowledgeModelUtils.getPackageIdWithoutVersion kmPackage) ]
            }
         , DescriptionList.entry { term = gettext "Version" appState.locale, description = Version.toString kmPackage.version }
         , DescriptionList.entry { term = gettext "Metamodel" appState.locale, description = String.fromInt kmPackage.metamodelVersion }
         , DescriptionList.entry { term = gettext "License" appState.locale, description = kmPackage.license }
         , DescriptionList.entry { term = gettext "Language" appState.locale, description = kmPackage.language }
         , DescriptionList.entry { term = gettext "Created at" appState.locale, description = TimeUtils.toReadableDateTime appState.timeZone kmPackage.createdAt }
         ]
            ++ forkOfInfo
        )


sidePanelRegistryLink : AppState -> KnowledgeModelPackageDetail -> DetailPage.Section msg
sidePanelRegistryLink appState kmPackage =
    DetailPage.sectionHtml (gettext "Registry Link" appState.locale) <|
        case kmPackage.registryLink of
            Just registryLink ->
                [ Link.link { label = gettext "View in registry" appState.locale, url = registryLink }
                    |> Link.external
                    |> Link.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                    |> Link.toHtml
                ]

            Nothing ->
                []


maxVersions : Int
maxVersions =
    10


sidePanelOtherVersions : AppState -> Model -> KnowledgeModelPackageDetail -> DetailPage.Section Msg
sidePanelOtherVersions appState model kmPackage =
    let
        versionLink version =
            li []
                [ Link.link
                    { label = Version.toString version.version
                    , url = Routes.toUrl appState (Routes.knowledgeModelsDetail version.uuid)
                    }
                    |> Link.toHtml
                ]

        takeFirstVersions =
            if model.showAllVersions then
                identity

            else
                List.take maxVersions

        versionLinks =
            kmPackage.versions
                |> List.filter ((/=) kmPackage.version << .version)
                |> List.sortWith VersionUuid.compare
                |> List.reverse
                |> takeFirstVersions
                |> List.map versionLink
    in
    DetailPage.sectionHtml (gettext "Other versions" appState.locale) <|
        if List.isEmpty versionLinks then
            []

        else
            let
                showAllButton =
                    if model.showAllVersions || List.length kmPackage.versions <= maxVersions then
                        text ""

                    else
                        Button.ghost (gettext "Show all" appState.locale) ShowAllVersions
                            |> Button.withIcon (Icon.solid "fa-angle-down")
                            |> Button.small
                            |> Button.toHtml
            in
            [ ul [ id otherVersionsId ] versionLinks
            , showAllButton
            ]


{-| The shell hears which workspace the thing shown is in
(`UpdateConfig.entityWorkspace`).
-}
inItsWorkspace : UpdateConfig Msg msg -> Result ApiError { a | workspaceUuid : Maybe Uuid } -> Cmd msg
inItsWorkspace cfg result =
    case result of
        Ok entity ->
            UpdateConfig.entityWorkspace cfg entity.workspaceUuid

        Err _ ->
            Cmd.none


{-| What the page is about, by name, for the window's title, once it has
arrived.
-}
entityName : Model -> Maybe String
entityName model =
    Maybe.map .name (ActionResult.toMaybe model.knowledgeModelPackage)
