module Wizard.KnowledgeModels.Detail.View exposing (view)

import Bootstrap.Dropdown as Dropdown
import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, p, span, strong, text, ul)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Shared.Components.Badge as Badge
import Shared.Data.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Shared.Data.OrganizationInfo exposing (OrganizationInfo)
import Shared.Data.Package.PackagePhase as PackagePhase
import Shared.Data.Package.PackageState as PackageState
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Markdown as Markdown
import Shared.Utils exposing (listFilterJust)
import String.Format as String
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.DetailPage as DetailPage
import Wizard.Common.Components.ListingDropdown as ListingDropdown
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.KnowledgeModels.Detail.Models exposing (Model)
import Wizard.KnowledgeModels.Detail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewPackage appState model) model.package


viewPackage : AppState -> Model -> PackageDetail -> Html Msg
viewPackage appState model package =
    DetailPage.container
        [ header appState model package
        , readme appState package
        , sidePanel appState package
        , deleteVersionModal appState model package
        ]


header : AppState -> Model -> PackageDetail -> Html Msg
header appState model package =
    let
        deprecatedBadge =
            if package.phase == PackagePhase.Deprecated then
                Badge.danger [] [ text (gettext "deprecated" appState.locale) ]

            else
                emptyNode

        previewAction =
            ListingDropdown.linkAnchorItem appState
                { route = Routes.knowledgeModelsPreview package.id Nothing
                , icon = faSet "kmDetail.preview" appState
                , label = gettext "Preview" appState.locale
                , dataCy = "km-detail_preview-link"
                }

        previewActionVisible =
            Feature.knowledgeModelsPreview appState

        createEditorAction =
            ListingDropdown.linkAnchorItem appState
                { route = Routes.kmEditorCreate (Just package.id) (Just True)
                , icon = faSet "kmDetail.createKMEditor" appState
                , label = gettext "Create KM editor" appState.locale
                , dataCy = "km-detail_create-editor-link"
                }

        createEditorActionVisible =
            Feature.knowledgeModelEditorsCreate appState

        forkAction =
            ListingDropdown.linkAnchorItem appState
                { route = Routes.kmEditorCreate (Just package.id) Nothing
                , icon = faSet "kmDetail.fork" appState
                , label = gettext "Fork KM" appState.locale
                , dataCy = "km-detail_fork-link"
                }

        forkActionVisible =
            Feature.knowledgeModelEditorsCreate appState

        createProjectAction =
            ListingDropdown.linkAnchorItem appState
                { route = Routes.projectsCreateCustom (Just package.id)
                , icon = faSet "kmDetail.createQuestionnaire" appState
                , label = gettext "Create project" appState.locale
                , dataCy = "km-detail_create-project-link"
                }

        createProjectActionVisible =
            Feature.projectsCreateCustom appState

        setDeprecatedAction =
            ListingDropdown.msgAnchorItem
                { msg = UpdatePhase PackagePhase.Deprecated
                , icon = faSet "documentTemplate.setDeprecated" appState
                , label = gettext "Set deprecated" appState.locale
                , dataCy = "km-detail_set-deprecated"
                }

        setDeprecatedActionVisible =
            package.phase == PackagePhase.Released

        restoreAction =
            ListingDropdown.msgAnchorItem
                { msg = UpdatePhase PackagePhase.Released
                , icon = faSet "documentTemplate.restore" appState
                , label = gettext "Restore" appState.locale
                , dataCy = "km-detail_restore"
                }

        restoreActionVisible =
            package.phase == PackagePhase.Deprecated

        exportAction =
            ListingDropdown.msgAnchorItem
                { msg = ExportPackage package
                , icon = faSet "_global.export" appState
                , label = gettext "Export" appState.locale
                , dataCy = "km-detail_export-link"
                }

        exportActionVisible =
            Feature.knowledgeModelsExport appState

        deleteAction =
            Dropdown.anchorItem
                [ onClick <| ShowDeleteDialog True
                , class "text-danger"
                , dataCy "km-detail_delete-link"
                ]
                [ faSet "_global.delete" appState
                , text (gettext "Delete" appState.locale)
                ]

        deleteActionVisible =
            Feature.knowledgeModelsDelete appState

        groups =
            [ [ ( previewAction, previewActionVisible ) ]
            , [ ( createEditorAction, createEditorActionVisible )
              , ( forkAction, forkActionVisible )
              ]
            , [ ( createProjectAction, createProjectActionVisible ) ]
            , [ ( exportAction, exportActionVisible ) ]
            , [ ( setDeprecatedAction, setDeprecatedActionVisible )
              , ( restoreAction, restoreActionVisible )
              , ( deleteAction, deleteActionVisible )
              ]
            ]

        dropdownActions =
            ListingDropdown.dropdown appState
                { dropdownState = model.dropdownState
                , toggleMsg = DropdownMsg
                , items = ListingDropdown.itemsFromGroups Dropdown.divider groups
                }
    in
    DetailPage.header (span [] [ text package.name, deprecatedBadge ]) [ dropdownActions ]


readme : AppState -> PackageDetail -> Html msg
readme appState package =
    let
        containsNewerVersions =
            (List.length <| List.filter (Version.greaterThan package.version) package.versions) > 0

        warning =
            if containsNewerVersions then
                div [ class "alert alert-warning" ]
                    [ text (gettext "This is not the latest available version of this Knowledge Model." appState.locale) ]

            else
                newVersionInRegistryWarning appState package
    in
    DetailPage.content
        [ warning
        , Markdown.toHtml [ DetailPage.contentInnerClass ] package.readme
        ]


newVersionInRegistryWarning : AppState -> PackageDetail -> Html msg
newVersionInRegistryWarning appState package =
    case ( package.remoteLatestVersion, PackageState.isOutdated package.state, appState.config.registry ) of
        ( Just remoteLatestVersion, True, RegistryEnabled _ ) ->
            let
                importLink =
                    if Feature.knowledgeModelsImport appState then
                        let
                            latestPackageId =
                                package.organizationId ++ ":" ++ package.kmId ++ ":" ++ Version.toString remoteLatestVersion
                        in
                        [ linkTo appState
                            (Routes.knowledgeModelsImport (Just latestPackageId))
                            [ class "btn btn-primary btn-sm with-icon ms-2" ]
                            [ faSet "kmImport.fromRegistry" appState
                            , text (gettext "Import" appState.locale)
                            ]
                        ]

                    else
                        []
            in
            div [ class "alert alert-warning" ]
                (faSet "_global.warning" appState
                    :: String.formatHtml (gettext "There is a newer version (%s) available." appState.locale)
                        [ strong [] [ text (Version.toString remoteLatestVersion) ] ]
                    ++ importLink
                )

        _ ->
            emptyNode


sidePanel : AppState -> PackageDetail -> Html msg
sidePanel appState package =
    let
        sections =
            [ sidePanelKmInfo appState package
            , sidePanelOtherVersions appState package
            , sidePanelOrganizationInfo appState package
            , sidePanelRegistryLink appState package
            ]
    in
    DetailPage.sidePanel
        [ DetailPage.sidePanelList 12 12 <| listFilterJust sections ]


sidePanelKmInfo : AppState -> PackageDetail -> Maybe ( String, String, Html msg )
sidePanelKmInfo appState package =
    let
        kmInfoList =
            [ ( gettext "ID" appState.locale, "id", text package.id )
            , ( gettext "Version" appState.locale, "version", text <| Version.toString package.version )
            , ( gettext "Metamodel" appState.locale, "metamodel", text <| String.fromInt package.metamodelVersion )
            , ( gettext "License" appState.locale, "license", text package.license )
            ]

        parentInfo =
            case package.forkOfPackageId of
                Just parentPackageId ->
                    [ ( gettext "Fork of" appState.locale
                      , "fork-of"
                      , linkTo appState (Routes.knowledgeModelsDetail parentPackageId) [] [ text parentPackageId ]
                      )
                    ]

                Nothing ->
                    []
    in
    Just ( gettext "Knowledge Model" appState.locale, "package", DetailPage.sidePanelList 4 8 <| kmInfoList ++ parentInfo )


sidePanelOtherVersions : AppState -> PackageDetail -> Maybe ( String, String, Html msg )
sidePanelOtherVersions appState package =
    let
        versionLink version =
            li []
                [ linkTo appState
                    (Routes.knowledgeModelsDetail <| package.organizationId ++ ":" ++ package.kmId ++ ":" ++ Version.toString version)
                    []
                    [ text <| Version.toString version ]
                ]

        versionLinks =
            package.versions
                |> List.filter ((/=) package.version)
                |> List.sortWith Version.compare
                |> List.reverse
                |> List.map versionLink
    in
    if List.length versionLinks > 0 then
        Just ( gettext "Other versions" appState.locale, "other-versions", ul [] versionLinks )

    else
        Nothing


sidePanelOrganizationInfo : AppState -> PackageDetail -> Maybe ( String, String, Html msg )
sidePanelOrganizationInfo appState package =
    let
        toOrganizationInfo organization =
            ( gettext "Published by" appState.locale, "published-by", viewOrganization organization )
    in
    Maybe.map toOrganizationInfo package.organization


sidePanelRegistryLink : AppState -> PackageDetail -> Maybe ( String, String, Html msg )
sidePanelRegistryLink appState package =
    let
        toRegistryLinkInfo registryLink =
            ( gettext "Registry Link" appState.locale
            , "registry-link"
            , a [ href registryLink, target "_blank", class "with-icon" ]
                [ faSet "kmDetail.registryLink" appState
                , text (gettext "View in registry" appState.locale)
                ]
            )
    in
    Maybe.map toRegistryLinkInfo package.registryLink


viewOrganization : OrganizationInfo -> Html msg
viewOrganization organization =
    DetailPage.sidePanelItemWithIcon organization.name
        (text organization.organizationId)
        (ItemIcon.view { text = organization.name, image = organization.logo })


deleteVersionModal : AppState -> Model -> PackageDetail -> Html Msg
deleteVersionModal appState model package =
    let
        modalContent =
            [ p []
                (String.formatHtml
                    (gettext "Are you sure you want to permanently delete %s?" appState.locale)
                    [ strong [] [ text package.id ] ]
                )
            ]

        modalConfig =
            { modalTitle = gettext "Delete version" appState.locale
            , modalContent = modalContent
            , visible = model.showDeleteDialog
            , actionResult = model.deletingVersion
            , actionName = gettext "Delete" appState.locale
            , actionMsg = DeleteVersion
            , cancelMsg = Just <| ShowDeleteDialog False
            , dangerous = True
            , dataCy = "km-delete-version"
            }
    in
    Modal.confirm appState modalConfig
