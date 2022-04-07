module Wizard.KnowledgeModels.Detail.View exposing (view)

import Html exposing (Html, a, div, li, p, strong, text, ul)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Shared.Api.Packages as PackagesApi
import Shared.Data.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Shared.Data.OrganizationInfo exposing (OrganizationInfo)
import Shared.Data.Package.PackageState as PackageState
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lg, lgx, lh, lx)
import Shared.Markdown as Markdown
import Shared.Utils exposing (listFilterJust, listInsertIf)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.DetailPage as DetailPage
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.KnowledgeModels.Detail.Models exposing (Model)
import Wizard.KnowledgeModels.Detail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.KnowledgeModels.Detail.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.KnowledgeModels.Detail.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KnowledgeModels.Detail.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewPackage appState model) model.package


viewPackage : AppState -> Model -> PackageDetail -> Html Msg
viewPackage appState model package =
    DetailPage.container
        [ header appState package
        , readme appState package
        , sidePanel appState package
        , deleteVersionModal appState model package
        ]


header : AppState -> PackageDetail -> Html Msg
header appState package =
    let
        previewAction =
            linkTo appState
                (Routes.knowledgeModelsPreview package.id Nothing)
                [ class "link-with-icon"
                , dataCy "km-detail_preview-link"
                ]
                [ faSet "kmDetail.preview" appState
                , lgx "km.action.preview" appState
                ]

        previewActionVisible =
            Feature.knowledgeModelsPreview appState

        createEditorAction =
            linkTo appState
                (Routes.kmEditorCreate (Just package.id) (Just True))
                [ class "link-with-icon"
                , dataCy "km-detail_create-editor-link"
                ]
                [ faSet "kmDetail.createKMEditor" appState
                , lgx "km.action.kmEditor" appState
                ]

        createEditorActionVisible =
            Feature.knowledgeModelEditorsCreate appState

        forkAction =
            linkTo appState
                (Routes.kmEditorCreate (Just package.id) Nothing)
                [ class "link-with-icon"
                , dataCy "km-detail_fork-link"
                ]
                [ faSet "kmDetail.fork" appState
                , lgx "km.action.fork" appState
                ]

        forkActionVisible =
            Feature.knowledgeModelEditorsCreate appState

        createProjectAction =
            linkTo appState
                (Routes.projectsCreateCustom (Just package.id))
                [ class "link-with-icon"
                , dataCy "km-detail_create-project-link"
                ]
                [ faSet "kmDetail.createQuestionnaire" appState
                , lgx "km.action.project" appState
                ]

        createProjectActionVisible =
            Feature.projectsCreateCustom appState

        exportAction =
            a
                [ class "link-with-icon"
                , href <| PackagesApi.exportPackageUrl package.id appState
                , target "_blank"
                , dataCy "km-detail_export-link"
                ]
                [ faSet "_global.export" appState
                , lgx "km.action.export" appState
                ]

        exportActionVisible =
            Feature.knowledgeModelsExport appState

        deleteAction =
            a
                [ onClick <| ShowDeleteDialog True
                , class "text-danger link-with-icon"
                , dataCy "km-detail_delete-link"
                ]
                [ faSet "_global.delete" appState
                , lgx "km.action.delete" appState
                ]

        deleteActionVisible =
            Feature.knowledgeModelsDelete appState

        actions =
            []
                |> listInsertIf previewAction previewActionVisible
                |> listInsertIf createEditorAction createEditorActionVisible
                |> listInsertIf forkAction forkActionVisible
                |> listInsertIf createProjectAction createProjectActionVisible
                |> listInsertIf exportAction exportActionVisible
                |> listInsertIf deleteAction deleteActionVisible
    in
    DetailPage.header (text package.name) actions


readme : AppState -> PackageDetail -> Html msg
readme appState package =
    let
        containsNewerVersions =
            (List.length <| List.filter (Version.greaterThan package.version) package.versions) > 0

        warning =
            if containsNewerVersions then
                div [ class "alert alert-warning" ]
                    [ lx_ "readme.versionWarning" appState ]

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
                latestPackageId =
                    package.organizationId ++ ":" ++ package.kmId ++ ":" ++ Version.toString remoteLatestVersion
            in
            div [ class "alert alert-warning" ]
                (faSet "_global.warning" appState
                    :: lh_ "registryVersion.warning"
                        [ text (Version.toString remoteLatestVersion)
                        , linkTo appState
                            (Routes.knowledgeModelsImport (Just latestPackageId))
                            []
                            [ lx_ "registryVersion.warning.import" appState ]
                        ]
                        appState
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
            [ ( lg "package.id" appState, "id", text package.id )
            , ( lg "package.version" appState, "version", text <| Version.toString package.version )
            , ( lg "package.metamodel" appState, "metamodel", text <| String.fromInt package.metamodelVersion )
            , ( lg "package.license" appState, "license", text package.license )
            ]

        parentInfo =
            case package.forkOfPackageId of
                Just parentPackageId ->
                    [ ( lg "package.forkOf" appState
                      , "fork-of"
                      , text parentPackageId
                      )
                    ]

                Nothing ->
                    []
    in
    Just ( lg "package" appState, "package", DetailPage.sidePanelList 4 8 <| kmInfoList ++ parentInfo )


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
        Just ( lg "package.otherVersions" appState, "other-versions", ul [] versionLinks )

    else
        Nothing


sidePanelOrganizationInfo : AppState -> PackageDetail -> Maybe ( String, String, Html msg )
sidePanelOrganizationInfo appState package =
    let
        toOrganizationInfo organization =
            ( lg "package.publishedBy" appState, "published-by", viewOrganization organization )
    in
    Maybe.map toOrganizationInfo package.organization


sidePanelRegistryLink : AppState -> PackageDetail -> Maybe ( String, String, Html msg )
sidePanelRegistryLink appState package =
    let
        toRegistryLinkInfo registryLink =
            ( lg "package.registryLink" appState
            , "registry-link"
            , a [ href registryLink, class "link-with-icon", target "_blank" ]
                [ faSet "kmDetail.registryLink" appState
                , text package.id
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
                (lh_ "deleteModal.message" [ strong [] [ text package.id ] ] appState)
            ]

        modalConfig =
            { modalTitle = l_ "deleteModal.title" appState
            , modalContent = modalContent
            , visible = model.showDeleteDialog
            , actionResult = model.deletingVersion
            , actionName = l_ "deleteModal.action" appState
            , actionMsg = DeleteVersion
            , cancelMsg = Just <| ShowDeleteDialog False
            , dangerous = True
            , dataCy = "km-delete-version"
            }
    in
    Modal.confirm appState modalConfig
