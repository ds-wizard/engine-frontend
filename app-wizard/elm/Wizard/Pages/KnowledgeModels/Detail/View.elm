module Wizard.Pages.KnowledgeModels.Detail.View exposing (view)

import Common.Components.Badge as Badge
import Common.Components.DetailPage as DetailPage
import Common.Components.FontAwesome exposing (faDelete, faDetailShowAll, faInfo, faKmDetailRegistryLink, faKmImportFromRegistry, faLocaleImport, faWarning)
import Common.Components.Modal as Modal
import Common.Components.Page as Page
import Common.Components.Tooltip exposing (tooltipLeft)
import Common.Components.Undraw as Undraw
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Common.Utils.Markdown as Markdown
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, li, p, span, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, href, target)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import String.Format as String
import Version
import Wizard.Api.Models.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Wizard.Api.Models.KnowledgeModelLocale exposing (KnowledgeModelLocale)
import Wizard.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage
import Wizard.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnowledgeModelPackagePhase
import Wizard.Api.Models.KnowledgeModelPackageDetail as KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Api.Models.OrganizationInfo exposing (OrganizationInfo)
import Wizard.Api.Models.VersionUuid as VersionUuid
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.ItemIcon as ItemIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KnowledgeModels.Common.DeleteModal as DeleteModal
import Wizard.Pages.KnowledgeModels.Common.KnowledgeModelActionsDropdown as KnowledgeModelActionsDropdown
import Wizard.Pages.KnowledgeModels.Detail.ImportLocaleModal as ImportLocaleModal
import Wizard.Pages.KnowledgeModels.Detail.KnowledgeModelDetailRoute as KnowledgeModelDetailRoute exposing (KnowledgeModelDetailRoute)
import Wizard.Pages.KnowledgeModels.Detail.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Detail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Feature


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewPackage appState model) model.knowledgeModelPackage


viewPackage : AppState -> Model -> KnowledgeModelPackageDetail -> Html Msg
viewPackage appState model kmPackage =
    DetailPage.container
        [ header appState model kmPackage
        , DetailPage.content
            { body = content model.detailRoute appState kmPackage
            , sidePanel = sidePanel appState model kmPackage
            }
        , Html.map DeleteModalMsg <| DeleteModal.view appState model.deleteModalModel
        , Html.map ImportLocaleModalMsg <| ImportLocaleModal.view appState model.importLocaleModal
        , deleteLocaleModal appState model
        ]


content : KnowledgeModelDetailRoute -> AppState -> KnowledgeModelPackageDetail -> List (Html Msg)
content route appState kmPackage =
    case route of
        KnowledgeModelDetailRoute.Readme ->
            readme appState kmPackage

        KnowledgeModelDetailRoute.Locales ->
            locales appState kmPackage


header : AppState -> Model -> KnowledgeModelPackageDetail -> Html Msg
header appState model kmPackage =
    let
        deprecatedBadge =
            Html.viewIf (kmPackage.phase == KnowledgeModelPackagePhase.Deprecated) <|
                Badge.danger [] [ text (gettext "deprecated" appState.locale) ]

        nonEditableBadge =
            Html.viewIf kmPackage.nonEditable <|
                Badge.dark [] [ text (gettext "non-editable" appState.locale) ]

        publicBadge =
            Html.viewIf kmPackage.public <|
                Badge.info [] [ text (gettext "public" appState.locale) ]

        dropdownActions =
            KnowledgeModelActionsDropdown.dropdown appState
                { dropdownState = model.dropdownState
                , toggleMsg = DropdownMsg
                }
                { exportMsg = ExportKnowledgeModelPackage
                , exportPotMsg = ExportKnowledgeModelPackagePot
                , updatePhaseMsg = \_ phase -> UpdatePhase phase
                , updatePublicMsg = \_ isPublic -> UpdatePublic isPublic
                , deleteMsg = always (DeleteModalMsg (DeleteModal.open (KnowledgeModelPackageDetail.toPackage kmPackage)))
                , viewActionVisible = False
                }
                kmPackage
    in
    DetailPage.headerWithNav
        { title = span [] [ text kmPackage.name, nonEditableBadge, deprecatedBadge, publicBadge ]
        , actions = [ dropdownActions ]
        , navItems =
            [ { title = gettext "Readme" appState.locale
              , onClick = OpenDetailRoute KnowledgeModelDetailRoute.Readme
              , count = Nothing
              , isActive = model.detailRoute == KnowledgeModelDetailRoute.Readme
              , dataCy = "km-detail_nav_readme"
              }
            , { title = gettext "Locales" appState.locale
              , onClick = OpenDetailRoute KnowledgeModelDetailRoute.Locales
              , count = Just (List.length kmPackage.locales)
              , isActive = model.detailRoute == KnowledgeModelDetailRoute.Locales
              , dataCy = "km-detail_nav_locales"
              }
            ]
        }


readme : AppState -> KnowledgeModelPackageDetail -> List (Html msg)
readme appState kmPackage =
    let
        containsNewerVersions =
            List.any (Version.greaterThan kmPackage.version << .version) kmPackage.versions

        nonEditableInfo =
            if kmPackage.nonEditable then
                div [ class "alert alert-info" ]
                    [ faInfo
                    , text (gettext "This is a non-editable knowledge model, i.e., it cannot be edited, forked, or exported." appState.locale)
                    ]

            else
                Html.nothing

        warning =
            if containsNewerVersions then
                div [ class "alert alert-warning" ]
                    [ text (gettext "This is not the latest available version of this knowledge model." appState.locale) ]

            else
                newVersionInRegistryWarning appState kmPackage
    in
    [ nonEditableInfo
    , warning
    , DetailPage.contentBodyNarrow [ Markdown.toHtml [] kmPackage.readme ]
    ]


locales : AppState -> KnowledgeModelPackageDetail -> List (Html Msg)
locales appState kmPackage =
    let
        importButton =
            if Feature.knowledgeModelsImportLocale appState then
                div [ class "d-flex justify-content-end mb-3" ]
                    [ button
                        [ class "btn btn-primary with-icon"
                        , onClick OpenImportLocaleModal
                        , dataCy "km-detail_import-locale"
                        ]
                        [ faLocaleImport
                        , text (gettext "Import" appState.locale)
                        ]
                    ]

            else
                Html.nothing

        localesBody =
            if List.isEmpty kmPackage.locales then
                Page.illustratedMessage
                    { illustration = Undraw.noData
                    , heading = gettext "No locales" appState.locale
                    , lines =
                        [ gettext "There are no locales for this knowledge model yet." appState.locale
                        , gettext "Export the .pot file to create one." appState.locale
                        ]
                    , cy = "km-locales-empty"
                    }

            else
                table [ class "table table-hover" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text (gettext "Name" appState.locale) ]
                            , th [] [ text (gettext "Code" appState.locale) ]
                            , th [] []
                            ]
                        ]
                    , tbody [] (List.map (viewLocale appState) kmPackage.locales)
                    ]
    in
    [ DetailPage.contentBodyNarrow [ importButton, localesBody ] ]


viewLocale : AppState -> KnowledgeModelLocale -> Html Msg
viewLocale appState locale =
    let
        deleteButton =
            if Feature.knowledgeModelsDeleteLocale appState then
                a
                    (class "text-danger"
                        :: onClick (ShowDeleteLocale locale)
                        :: dataCy "km-detail_locale-delete"
                        :: tooltipLeft (gettext "Delete" appState.locale)
                    )
                    [ faDelete ]

            else
                Html.nothing
    in
    tr []
        [ td [] [ text locale.name ]
        , td [] [ text locale.code ]
        , td [ class "text-end" ] [ deleteButton ]
        ]


deleteLocaleModal : AppState -> Model -> Html Msg
deleteLocaleModal appState model =
    let
        ( visible, name ) =
            case model.localeToDelete of
                Just locale ->
                    ( True, locale.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                (String.formatHtml
                    (gettext "Are you sure you want to delete the locale %s?" appState.locale)
                    [ strong [] [ text name ] ]
                )
            ]

        modalConfig =
            Modal.confirmConfig (gettext "Delete locale" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigActionResult model.deletingLocale
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) DeleteLocale
                |> Modal.confirmConfigCancelMsg HideDeleteLocale
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "km-locale-delete"
    in
    Modal.confirm appState modalConfig


newVersionInRegistryWarning : AppState -> KnowledgeModelPackageDetail -> Html msg
newVersionInRegistryWarning appState kmPackage =
    case ( kmPackage.remoteLatestVersion, KnowledgeModelPackage.isOutdated kmPackage, appState.config.registry ) of
        ( Just remoteLatestVersion, True, RegistryEnabled _ ) ->
            let
                importLink =
                    if Feature.knowledgeModelsImport appState then
                        let
                            latestPackageId =
                                kmPackage.organizationId ++ ":" ++ kmPackage.kmId ++ ":" ++ Version.toString remoteLatestVersion
                        in
                        [ linkTo (Routes.knowledgeModelsImport (Just latestPackageId))
                            [ class "btn btn-primary btn-sm with-icon ms-2" ]
                            [ faKmImportFromRegistry
                            , text (gettext "Import" appState.locale)
                            ]
                        ]

                    else
                        []
            in
            div [ class "alert alert-warning" ]
                (faWarning
                    :: String.formatHtml (gettext "There is a newer version (%s) available." appState.locale)
                        [ strong [] [ text (Version.toString remoteLatestVersion) ] ]
                    ++ importLink
                )

        _ ->
            Html.nothing


sidePanel : AppState -> Model -> KnowledgeModelPackageDetail -> List (Html Msg)
sidePanel appState model kmPackage =
    let
        sections =
            [ sidePanelKmInfo appState kmPackage
            , sidePanelOrganizationInfo appState kmPackage
            , sidePanelRegistryLink appState kmPackage
            , sidePanelOtherVersions appState model kmPackage
            ]
    in
    [ DetailPage.sidePanelList 12 12 <| List.filterMap identity sections ]


sidePanelKmInfo : AppState -> KnowledgeModelPackageDetail -> Maybe ( String, String, Html msg )
sidePanelKmInfo appState kmPackage =
    let
        kmInfoList =
            [ ( gettext "ID" appState.locale, "id", text (KnowledgeModelUtils.getPackageId kmPackage) )
            , ( gettext "Version" appState.locale, "version", text <| Version.toString kmPackage.version )
            , ( gettext "Metamodel" appState.locale, "metamodel", text <| String.fromInt kmPackage.metamodelVersion )
            , ( gettext "License" appState.locale, "license", text kmPackage.license )
            , ( gettext "Language" appState.locale, "language", text kmPackage.language )
            ]

        forkOfInfo =
            case kmPackage.forkOfPackageId of
                Just forkOfPackageId ->
                    [ ( gettext "Fork of" appState.locale
                      , "fork-of"
                      , span [] [ text forkOfPackageId ]
                      )
                    ]

                Nothing ->
                    []
    in
    Just ( gettext "Knowledge Model" appState.locale, "knowledge-model-package", DetailPage.sidePanelList 4 8 <| kmInfoList ++ forkOfInfo )


sidePanelOtherVersions : AppState -> Model -> KnowledgeModelPackageDetail -> Maybe ( String, String, Html Msg )
sidePanelOtherVersions appState model kmPackage =
    let
        versionLink version =
            li []
                [ linkTo (Routes.knowledgeModelsDetail version.uuid)
                    []
                    [ text <| Version.toString version.version ]
                ]

        takeFirstVersions =
            if model.showAllVersions then
                identity

            else
                List.take 10

        versionLinks =
            kmPackage.versions
                |> List.filter ((/=) kmPackage.version << .version)
                |> List.sortWith VersionUuid.compare
                |> List.reverse
                |> takeFirstVersions
                |> List.map versionLink
    in
    if List.isEmpty versionLinks then
        Nothing

    else
        let
            showAllLink =
                if model.showAllVersions || List.length kmPackage.versions <= 10 then
                    Html.nothing

                else
                    li [ class "show-all-link" ]
                        [ a [ onClick ShowAllVersions ]
                            [ text (gettext "Show all" appState.locale)
                            , faDetailShowAll
                            ]
                        ]
        in
        Just ( gettext "Other versions" appState.locale, "other-versions", ul [] (versionLinks ++ [ showAllLink ]) )


sidePanelOrganizationInfo : AppState -> KnowledgeModelPackageDetail -> Maybe ( String, String, Html msg )
sidePanelOrganizationInfo appState kmPackage =
    let
        toOrganizationInfo organization =
            ( gettext "Published by" appState.locale, "published-by", viewOrganization organization )
    in
    Maybe.map toOrganizationInfo kmPackage.organization


sidePanelRegistryLink : AppState -> KnowledgeModelPackageDetail -> Maybe ( String, String, Html msg )
sidePanelRegistryLink appState kmPackage =
    let
        toRegistryLinkInfo registryLink =
            ( gettext "Registry Link" appState.locale
            , "registry-link"
            , ul [ class "fa-ul" ]
                [ li []
                    [ a [ href registryLink, target "_blank" ]
                        [ span [ class "fa-li" ] [ faKmDetailRegistryLink ]
                        , span [ class "fa-li-content" ] [ text (gettext "View in registry" appState.locale) ]
                        ]
                    ]
                ]
            )
    in
    Maybe.map toRegistryLinkInfo kmPackage.registryLink


viewOrganization : OrganizationInfo -> Html msg
viewOrganization organization =
    DetailPage.sidePanelItemWithIcon organization.name
        (text organization.organizationId)
        (ItemIcon.view { text = organization.name, image = organization.logo })
