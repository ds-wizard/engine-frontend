module Common.Pages.DocumentTemplates.Detail exposing
    ( Model
    , Msg
    , entityName
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| One document template, with its readme and its locales policy as tabs.

The tabs are routes (`Routes.isSameListingRoute` keeps the page model when
switching between them), so the active tab comes from the route and not from
the model.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.DocumentTemplates as DocumentTemplatesApi
import Common.Api.Models.BootstrapConfig.RegistryConfig as RegistryConfig
import Common.Api.Models.DocumentTemplate as DocumentTemplate
import Common.Api.Models.DocumentTemplate.DocumentTemplatePackage as DocumentTemplatePackage
import Common.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Common.Api.Models.DocumentTemplate.DocumentTemplateState as DocumentTemplateState
import Common.Api.Models.DocumentTemplateDetail as DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Common.Api.Models.DocumentTemplateLocale exposing (DocumentTemplateLocale)
import Common.Api.Models.UrlResponse exposing (UrlResponse)
import Common.Api.Models.VersionUuid as VersionUuid
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.DetailPage as DetailPage
import Common.Components.ItemActions as ItemActions
import Common.Components.LibraryDeleteModal as DeleteModal
import Common.Components.Markdown as Markdown
import Common.Components.RowNotice as RowNotice
import Common.Data.AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.GuideLinks as GuideLinks
import Common.Data.NotFound as NotFound
import Common.Data.RowPolicy exposing (RowPolicy)
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.DocumentTemplates.Components.DocumentTemplateActions as DocumentTemplateActions
import Common.Pages.DocumentTemplates.Components.ImportLocaleModal as ImportLocaleModal
import Common.Pages.DocumentTemplates.Detail.Routes as DocumentTemplateDetailRoute exposing (DocumentTemplateDetailRoute)
import Common.Routes as Routes
import Common.Utils.DocumentTemplateUtils as DocumentTemplateUtils
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setTemplate)
import Common.Utils.TimeUtils as TimeUtils
import File.Download as Download
import Gettext exposing (gettext)
import Html exposing (Html, li, strong, text, ul)
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



-- MODEL


type alias Model =
    { template : ActionResult DocumentTemplateDetail
    , deleteModalModel : DeleteModal.Model
    , showAllKms : Bool
    , importLocaleModal : ImportLocaleModal.Model
    , localeToDelete : Maybe DocumentTemplateLocale
    , deletingLocale : ActionResult String
    , actionsMenu : Menu.Model
    , resizer : Resizer.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { template = Loading
    , deleteModalModel = DeleteModal.initialModel False
    , showAllKms = False
    , importLocaleModal = ImportLocaleModal.init
    , localeToDelete = Nothing
    , deletingLocale = Unset
    , actionsMenu = Menu.init
    , resizer = Resizer.init (DetailPage.resizerConfig appState)
    }


init : AppState -> Uuid -> ( Model, Cmd Msg )
init appState templateUuid =
    ( initialModel appState
    , DocumentTemplatesApi.getTemplate appState templateUuid GetTemplateCompleted
    )



-- UPDATE


type Msg
    = GetTemplateCompleted (Result ApiError DocumentTemplateDetail)
    | DeleteModalMsg DeleteModal.Msg
    | OpenDeleteModal DocumentTemplateDetail
    | UpdatePhase DocumentTemplatePhase
    | UpdatePhaseCompleted (Result ApiError DocumentTemplateDetail)
    | ExportTemplate DocumentTemplateDetail
    | ExportTemplatePot DocumentTemplateDetail
    | DownloadUrlCompleted (Result ApiError UrlResponse)
    | OpenImportLocaleModal
    | ImportLocaleModalMsg ImportLocaleModal.Msg
    | ShowDeleteLocale DocumentTemplateLocale
    | HideDeleteLocale
    | DeleteLocale
    | DeleteLocaleCompleted (Result ApiError ())
    | DownloadLocale DocumentTemplateLocale
    | DownloadLocaleCompleted DocumentTemplateLocale (Result ApiError String)
    | ShowAllKms
    | ActionsMenuMsg Int Menu.Msg
    | ResizerMsg Resizer.Msg
    | NoOp


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetTemplateCompleted result ->
            RequestHelpers.applyResultOrNotFound
                { setResult = setTemplate
                , defaultError = gettext "Unable to get the document template." appState.locale
                , notFoundCmd = UpdateConfig.notFound cfg (NotFound.documentTemplate appState)
                , notAllowedCmd = UpdateConfig.notAllowed cfg
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }
                |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, inItsWorkspace cfg result ])

        DeleteModalMsg deleteModalMsg ->
            let
                deleteModalConfig =
                    { afterDeleteCmd = Routes.navigate appState Routes.documentTemplatesIndex
                    , afterCloseCmd = Task.attempt (always (cfg.wrapMsg NoOp)) (Dom.focus (Menu.triggerId actionsMenuId))
                    , wrapMsg = cfg.wrapMsg << DeleteModalMsg
                    }

                ( deleteModalModel, deleteModalCmd ) =
                    DeleteModal.update appState deleteModalConfig deleteModalMsg model.deleteModalModel
            in
            ( { model | deleteModalModel = deleteModalModel }, deleteModalCmd )

        OpenDeleteModal template ->
            -- The dialog takes focus as it opens, so the menu closes without
            -- taking it back; closing the dialog puts it on the menu's trigger.
            let
                ( actionsMenu, menuCmd ) =
                    Menu.update actionsMenuId 0 Menu.closeWithoutFocus model.actionsMenu

                ( newModel, deleteCmd ) =
                    update cfg appState (DeleteModalMsg (DeleteModal.open (DeleteModal.target DeleteModal.DocumentTemplate template))) { model | actionsMenu = actionsMenu }
            in
            ( newModel, Cmd.batch [ Cmd.map (cfg.wrapMsg << ActionsMenuMsg 0) menuCmd, deleteCmd ] )

        UpdatePhase phase ->
            case model.template of
                Success documentTemplate ->
                    closeActionsMenu cfg
                        ( model
                        , DocumentTemplatesApi.putTemplate appState { documentTemplate | phase = phase } (cfg.wrapMsg << UpdatePhaseCompleted)
                        )

                _ ->
                    ( model, Cmd.none )

        UpdatePhaseCompleted result ->
            RequestHelpers.applyResult
                { setResult = setTemplate
                , defaultError = gettext "Unable to update the document template." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        ExportTemplate template ->
            closeActionsMenu cfg
                ( model
                , DocumentTemplateActions.fetchDownloadUrl appState
                    (DocumentTemplatesApi.exportTemplateUrl template.uuid)
                    (cfg.wrapMsg << DownloadUrlCompleted)
                )

        ExportTemplatePot template ->
            closeActionsMenu cfg
                ( model
                , DocumentTemplateActions.fetchDownloadUrl appState
                    (DocumentTemplatesApi.exportTemplatePotUrl template.uuid)
                    (cfg.wrapMsg << DownloadUrlCompleted)
                )

        DownloadUrlCompleted result ->
            ( model, DocumentTemplateActions.downloadCompleted cfg.logoutMsg result )

        OpenImportLocaleModal ->
            case model.template of
                Success template ->
                    ( { model | importLocaleModal = ImportLocaleModal.open template.uuid }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ImportLocaleModalMsg importLocaleModalMsg ->
            let
                ( importLocaleModal, importLocaleModalCmd, mbImportedLocale ) =
                    ImportLocaleModal.update
                        { wrapMsg = cfg.wrapMsg << ImportLocaleModalMsg
                        , logoutMsg = cfg.logoutMsg
                        }
                        appState
                        importLocaleModalMsg
                        model.importLocaleModal

                template =
                    case mbImportedLocale of
                        Just locale ->
                            ActionResult.map (\t -> { t | locales = t.locales ++ [ locale ] }) model.template

                        Nothing ->
                            model.template
            in
            ( { model | importLocaleModal = importLocaleModal, template = template }
            , importLocaleModalCmd
            )

        ShowDeleteLocale locale ->
            ( { model | localeToDelete = Just locale, deletingLocale = Unset }, Cmd.none )

        HideDeleteLocale ->
            ( { model | localeToDelete = Nothing }, Cmd.none )

        DeleteLocale ->
            case ( model.template, model.localeToDelete ) of
                ( Success template, Just locale ) ->
                    ( { model | deletingLocale = Loading }
                    , DocumentTemplatesApi.deleteLocale appState template.uuid locale.uuid (cfg.wrapMsg << DeleteLocaleCompleted)
                    )

                _ ->
                    ( model, Cmd.none )

        DeleteLocaleCompleted result ->
            case result of
                Ok _ ->
                    let
                        template =
                            case model.localeToDelete of
                                Just locale ->
                                    ActionResult.map (\t -> { t | locales = List.filter (\l -> l.uuid /= locale.uuid) t.locales }) model.template

                                Nothing ->
                                    model.template
                    in
                    -- The row whose delete button had focus is gone.
                    ( { model | localeToDelete = Nothing, deletingLocale = Success "", template = template }
                    , AppShell.focusOnRouteChange (cfg.wrapMsg NoOp)
                    )

                Err error ->
                    ( { model | deletingLocale = ApiError.toActionResult appState (gettext "Deleting the locale failed." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        DownloadLocale locale ->
            case model.template of
                Success template ->
                    ( model
                    , DocumentTemplatesApi.getLocaleContent appState template.uuid locale.uuid (cfg.wrapMsg << DownloadLocaleCompleted locale)
                    )

                _ ->
                    ( model, Cmd.none )

        DownloadLocaleCompleted locale result ->
            case result of
                Ok content ->
                    ( model, Download.string (locale.code ++ ".po") "text/x-gettext-translation" content )

                Err _ ->
                    ( model, RequestHelpers.getResultCmd cfg.logoutMsg result )

        ShowAllKms ->
            ( { model | showAllKms = True }, Cmd.none )

        ActionsMenuMsg count menuMsg ->
            let
                ( actionsMenu, menuCmd ) =
                    Menu.update actionsMenuId count menuMsg model.actionsMenu
            in
            ( { model | actionsMenu = actionsMenu }, Cmd.map (cfg.wrapMsg << ActionsMenuMsg count) menuCmd )

        ResizerMsg resizerMsg ->
            ( { model | resizer = Resizer.update resizerMsg model.resizer }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


actionsMenuId : String
actionsMenuId =
    "document-template-actions"


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



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    Sub.batch
        [ Sub.map (ActionsMenuMsg 0) (Menu.subscriptions actionsMenuId model.actionsMenu)
        , Sub.map ResizerMsg (Resizer.subscriptions model.resizer)
        , Sub.map ImportLocaleModalMsg (ImportLocaleModal.subscriptions appState model.importLocaleModal)
        ]



-- VIEW


view : RowPolicy -> AppState -> DocumentTemplateDetailRoute -> Model -> Html Msg
view policy appState detailRoute model =
    DetailPage.view appState (viewDocumentTemplate policy appState detailRoute model) model.template


viewDocumentTemplate : RowPolicy -> AppState -> DocumentTemplateDetailRoute -> Model -> DocumentTemplateDetail -> DetailPage.DetailPage Msg
viewDocumentTemplate policy appState detailRoute model template =
    let
        badges =
            Maybe.values
                [ if template.nonEditable then
                    Just (Badge.neutral (gettext "non-editable" appState.locale))

                  else
                    Nothing
                , if template.phase == DocumentTemplatePhase.Deprecated then
                    Just (Badge.danger (gettext "deprecated" appState.locale))

                  else
                    Nothing
                ]

        actions =
            DocumentTemplateActions.actions policy
                appState
                { exportMsg = always (ExportTemplate template)
                , exportPotMsg = always (ExportTemplatePot template)
                , updatePhaseMsg = \_ phase -> UpdatePhase phase
                , deleteMsg = always (OpenDeleteModal template)
                , viewActionVisible = False
                }
                template

        content =
            case detailRoute of
                DocumentTemplateDetailRoute.Readme ->
                    readme appState template

                DocumentTemplateDetailRoute.Locales ->
                    locales policy appState template

        modals =
            [ Html.map DeleteModalMsg <| DeleteModal.view policy appState model.deleteModalModel
            , Html.map ImportLocaleModalMsg <| ImportLocaleModal.view appState model.importLocaleModal
            , deleteLocaleModal appState model
            ]
    in
    DetailPage.detailPage
        { title = template.name
        , content = RowNotice.view (policy.notice template.workspaceUuid (Routes.toPath (Routes.documentTemplatesDetail template.uuid))) :: content ++ modals
        }
        |> DetailPage.withBadges badges
        |> DetailPage.withMoreActions
            { id = actionsMenuId
            , label = String.format (gettext "Actions for %s" appState.locale) [ template.name ]
            , actions = actions
            , model = model.actionsMenu
            , toMsg = ActionsMenuMsg (ItemActions.focusableCount actions)
            }
        |> DetailPage.withTabsWithTestIds
            { label = gettext "Document Template" appState.locale
            , items =
                [ { label = gettext "Readme" appState.locale
                  , url = Routes.toUrl appState (Routes.documentTemplatesDetail template.uuid)
                  , active = detailRoute == DocumentTemplateDetailRoute.Readme
                  , badge = Nothing
                  , testId = Just "document-template-detail_nav_readme"
                  , icon = Nothing
                  }
                , { label = gettext "Locales" appState.locale
                  , url = Routes.toUrl appState (Routes.documentTemplatesDetailLocales template.uuid)
                  , active = detailRoute == DocumentTemplateDetailRoute.Locales
                  , badge = Just (String.fromInt (List.length template.locales))
                  , testId = Just "document-template-detail_nav_locales"
                  , icon = Nothing
                  }
                ]
            }
        |> DetailPage.withSidePanel
            { resizer = model.resizer
            , toMsg = ResizerMsg
            , sections = sidePanel appState model template
            }



-- README


readme : AppState -> DocumentTemplateDetail -> List (Html Msg)
readme appState template =
    let
        nonEditableInfo =
            if template.nonEditable then
                Alert.toHtml (Alert.info (gettext "This is a non-editable document template, i.e., it cannot be edited, or exported." appState.locale))

            else
                text ""

        warning =
            if not (DocumentTemplateDetail.isLatestVersion template) then
                Alert.toHtml (Alert.warning (gettext "This is not the latest available version of this document template." appState.locale))

            else
                newVersionInRegistryWarning appState template
    in
    [ Layout.stack
        [ nonEditableInfo
        , warning
        , unsupportedMetamodelVersionWarning appState template
        , Markdown.view { headingOffset = 1 } template.readme
        ]
    ]


importLatestLink : AppState -> DocumentTemplateDetail -> Version.Version -> List (Html msg)
importLatestLink appState template remoteLatestVersion =
    if Feature.documentTemplatesImport appState then
        let
            latestPackageId =
                template.id ++ ":" ++ Version.toString remoteLatestVersion
        in
        [ text " "
        , Link.link
            { label = gettext "Import" appState.locale
            , url = Routes.toUrl appState (Routes.documentTemplatesImport (Just latestPackageId))
            }
            |> Link.withIcon (Icon.solid "fa-cloud-arrow-down")
            |> Link.toHtml
        ]

    else
        []


newerVersionText : AppState -> Version.Version -> List (Html msg)
newerVersionText appState remoteLatestVersion =
    String.formatHtml
        (gettext "There is a newer version (%s) available." appState.locale)
        [ strong [] [ text (Version.toString remoteLatestVersion) ] ]


newVersionInRegistryWarning : AppState -> DocumentTemplateDetail -> Html msg
newVersionInRegistryWarning appState template =
    if template.state /= DocumentTemplateState.UnsupportedMetamodelVersion then
        case ( template.remoteLatestVersion, DocumentTemplate.isOutdated template, RegistryConfig.isEnabled appState.config.registry ) of
            ( Just remoteLatestVersion, True, True ) ->
                let
                    importLink =
                        if Version.greaterThan template.version remoteLatestVersion then
                            importLatestLink appState template remoteLatestVersion

                        else
                            []
                in
                Alert.warning ""
                    |> Alert.toHtmlWithContent (newerVersionText appState remoteLatestVersion ++ importLink)

            _ ->
                text ""

    else
        text ""


unsupportedMetamodelVersionWarning : AppState -> DocumentTemplateDetail -> Html msg
unsupportedMetamodelVersionWarning appState template =
    if template.state == DocumentTemplateState.UnsupportedMetamodelVersion then
        let
            link =
                case ( DocumentTemplateDetail.isLatestVersion template, template.remoteLatestVersion, RegistryConfig.isEnabled appState.config.registry ) of
                    ( True, Just remoteLatestVersion, True ) ->
                        if Version.greaterThan template.version remoteLatestVersion then
                            text " "
                                :: newerVersionText appState remoteLatestVersion
                                ++ importLatestLink appState template remoteLatestVersion

                        else
                            []

                    ( True, _, _ ) ->
                        [ text " "
                        , Link.link
                            { label = gettext "Learn more in guide" appState.locale
                            , url = GuideLinks.documentTemplatesUnsupportedMetamodel appState.guideLinks
                            }
                            |> Link.external
                            |> Link.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                            |> Link.toHtml
                        ]

                    _ ->
                        []
        in
        Alert.danger ""
            |> Alert.toHtmlWithContent
                (text (gettext "This document template is not supported in the current version." appState.locale) :: link)

    else
        text ""



-- LOCALES


locales : RowPolicy -> AppState -> DocumentTemplateDetail -> List (Html Msg)
locales policy appState template =
    let
        importButton =
            if Feature.documentTemplatesImportLocale appState && policy.manage template.workspaceUuid then
                Layout.inlineEnd
                    [ Button.primary (gettext "Import" appState.locale) OpenImportLocaleModal
                        |> Button.withIcon (Icon.solid "fa-upload")
                        |> Button.withTestId "document-template-detail_import-locale"
                        |> Button.toHtml
                    ]

            else
                text ""

        localesBody =
            if List.isEmpty template.locales then
                let
                    secondLine =
                        if template.potFileReady then
                            gettext "Export the .pot file to create one." appState.locale

                        else
                            gettext "This document template is not prepared for translations." appState.locale
                in
                EmptyState.emptyState
                    { title = gettext "No locales policy" appState.locale
                    , description = gettext "There are no locales policy for this document template yet." appState.locale ++ " " ++ secondLine
                    }
                    |> EmptyState.withIllustration (Illustration.toHtml Illustration.empty)
                    |> EmptyState.withHeadingLevel 2
                    |> EmptyState.toHtml

            else
                Table.table
                    [ Table.column (gettext "Name" appState.locale) (text << .name)
                    , Table.column (gettext "Code" appState.locale) (text << .code)
                    , Table.actions (gettext "Actions" appState.locale) (viewLocaleActions policy appState template.workspaceUuid)
                    ]
                    template.locales
                    |> Table.withRowTestId (\row -> "dt-locales_row_" ++ Uuid.toString row.uuid)
                    |> Table.toHtml
    in
    [ Layout.stack [ importButton, localesBody ] ]


viewLocaleActions : RowPolicy -> AppState -> Maybe Uuid -> DocumentTemplateLocale -> List (Button Msg)
viewLocaleActions policy appState rowWorkspaceUuid locale =
    let
        downloadButton =
            if Feature.documentTemplatesExportLocale appState then
                [ Button.iconOnly (Icon.solid "fa-download") (gettext "Download" appState.locale) (DownloadLocale locale)
                    |> Button.withAccessibleName (String.format (gettext "Download %s" appState.locale) [ locale.name ])
                    |> Button.withTestId "document-template-detail_locale-download"
                ]

            else
                []

        deleteButton =
            if Feature.documentTemplatesDeleteLocale appState && policy.manage rowWorkspaceUuid then
                [ Button.dangerIconOnly (Icon.solid "fa-trash") (gettext "Delete" appState.locale) (ShowDeleteLocale locale)
                    |> Button.withAccessibleName (String.format (gettext "Delete %s" appState.locale) [ locale.name ])
                    |> Button.withTestId "document-template-detail_locale-delete"
                ]

            else
                []
    in
    downloadButton ++ deleteButton


deleteLocaleModal : AppState -> Model -> Html Msg
deleteLocaleModal appState model =
    let
        ( visible, name, code ) =
            case model.localeToDelete of
                Just locale ->
                    ( True, locale.name, locale.code )

                Nothing ->
                    ( False, "", "" )
    in
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete locale" appState.locale
        , open = visible
        , question = String.format (gettext "Are you sure you want to delete the locale %s?" appState.locale) [ name ]
        , entity = Text.code code
        , onConfirm = DeleteLocale
        , onClose = HideDeleteLocale
        , result = model.deletingLocale
        , testId = Just "document-template-locale-delete"
        }



-- SIDE PANEL


sidePanel : AppState -> Model -> DocumentTemplateDetail -> List (DetailPage.Section Msg)
sidePanel appState model template =
    [ sidePanelTemplateInfo appState template
    , sidePanelRegistryLink appState template
    , sidePanelFormats appState template
    , sidePanelOtherVersions appState template
    , sidePanelUsableWith appState model template
    ]


sidePanelTemplateInfo : AppState -> DocumentTemplateDetail -> DetailPage.Section Msg
sidePanelTemplateInfo appState template =
    DetailPage.section (gettext "Document Template" appState.locale)
        [ DescriptionList.withHtml
            { term = gettext "ID" appState.locale
            , description =
                [ Text.code (DocumentTemplateUtils.getIdWithoutVersion template) ]
            }
        , DescriptionList.entry { term = gettext "Version" appState.locale, description = Version.toString template.version }
        , DescriptionList.entry { term = gettext "Metamodel" appState.locale, description = Version.toStringMinor template.metamodelVersion }
        , DescriptionList.entry { term = gettext "License" appState.locale, description = template.license }
        , DescriptionList.entry { term = gettext "Language" appState.locale, description = template.language }
        , DescriptionList.entry { term = gettext "Created at" appState.locale, description = TimeUtils.toReadableDateTime appState.timeZone template.createdAt }
        ]


sidePanelRegistryLink : AppState -> DocumentTemplateDetail -> DetailPage.Section msg
sidePanelRegistryLink appState template =
    DetailPage.sectionHtml (gettext "Registry Link" appState.locale)
        (case template.registryLink of
            Just registryLink ->
                [ Link.link { label = gettext "View in registry" appState.locale, url = registryLink }
                    |> Link.withIcon (Icon.solid "fa-up-right-from-square")
                    |> Link.external
                    |> Link.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                    |> Link.toHtml
                ]

            Nothing ->
                []
        )


sidePanelFormats : AppState -> DocumentTemplateDetail -> DetailPage.Section msg
sidePanelFormats appState template =
    let
        -- The format's icon is a whole Font Awesome class ("far fa-file-pdf").
        formatView format =
            Layout.inline
                [ Icon.toHtml (Icon.fromClass format.icon)
                , text format.name
                ]
    in
    DetailPage.sectionHtml (gettext "Formats" appState.locale)
        (case template.formats of
            [] ->
                []

            formats ->
                [ Layout.stack (List.map formatView (List.sortBy .name formats)) ]
        )


sidePanelOtherVersions : AppState -> DocumentTemplateDetail -> DetailPage.Section msg
sidePanelOtherVersions appState template =
    let
        versionLink version =
            li []
                [ Link.link
                    { label = Version.toString version.version
                    , url = Routes.toUrl appState (Routes.documentTemplatesDetail version.uuid)
                    }
                    |> Link.toHtml
                ]

        versionLinks =
            template.versions
                |> List.filter ((/=) template.version << .version)
                |> List.sortWith VersionUuid.compare
                |> List.reverse
                |> List.map versionLink
    in
    DetailPage.sectionHtml (gettext "Other versions" appState.locale)
        (if List.isEmpty versionLinks then
            []

         else
            -- A list of like things reads as a list: bullets, which the side
            -- panel gives a class-less `ul` on its own, rather than links
            -- stacked one under another.
            [ ul [] versionLinks ]
        )


sidePanelUsableWith : AppState -> Model -> DocumentTemplateDetail -> DetailPage.Section Msg
sidePanelUsableWith appState model template =
    let
        packageLink kmPackage =
            li []
                [ Link.link
                    { label = KnowledgeModelUtils.getPackageId kmPackage
                    , url = Routes.toUrl appState (Routes.knowledgeModelsDetail kmPackage.uuid)
                    }
                    |> Link.withTestId "template_km-link"
                    |> Link.toHtml
                ]

        takeFirstPackages =
            if model.showAllKms then
                identity

            else
                List.take 10

        kmPackageLinks =
            template.usableKnowledgeModels
                |> List.sortWith DocumentTemplatePackage.compareById
                |> takeFirstPackages
                |> List.map packageLink
    in
    DetailPage.sectionHtml (gettext "Usable with" appState.locale)
        (if List.isEmpty kmPackageLinks then
            []

         else
            let
                showAllButton =
                    if model.showAllKms || List.length template.usableKnowledgeModels <= 10 then
                        []

                    else
                        [ Button.ghost (gettext "Show all" appState.locale) ShowAllKms
                            |> Button.withIcon (Icon.solid "fa-angle-down")
                            |> Button.small
                            |> Button.toHtml
                        ]
            in
            -- The same list as the versions above it, and the button that
            -- lengthens it after the list rather than in it.
            ul [] kmPackageLinks :: showAllButton
        )


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
    Maybe.map .name (ActionResult.toMaybe model.template)
