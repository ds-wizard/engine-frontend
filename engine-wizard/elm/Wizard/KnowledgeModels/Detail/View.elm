module Wizard.KnowledgeModels.Detail.View exposing (view)

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
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.KnowledgeModels.Common.KnowledgeModelActionsDropdown as KnowledgeModelActionsDropdown
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
        , sidePanel appState model package
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

        nonEditableBadge =
            if package.nonEditable then
                Badge.dark [] [ text (gettext "non-editable" appState.locale) ]

            else
                emptyNode

        dropdownActions =
            KnowledgeModelActionsDropdown.dropdown appState
                { dropdownState = model.dropdownState
                , toggleMsg = DropdownMsg
                }
                { exportMsg = ExportPackage
                , updatePhaseMsg = \_ phase -> UpdatePhase phase
                , deleteMsg = always (ShowDeleteDialog True)
                , viewActionVisible = False
                }
                package
    in
    DetailPage.header (span [] [ text package.name, nonEditableBadge, deprecatedBadge ]) [ dropdownActions ]


readme : AppState -> PackageDetail -> Html msg
readme appState package =
    let
        containsNewerVersions =
            (List.length <| List.filter (Version.greaterThan package.version) package.versions) > 0

        nonEditableInfo =
            if package.nonEditable then
                div [ class "alert alert-info" ]
                    [ faSet "_global.info" appState
                    , text (gettext "This is a non-editable knowledge model, i.e., it cannot be edited, forked, or exported." appState.locale)
                    ]

            else
                emptyNode

        warning =
            if containsNewerVersions then
                div [ class "alert alert-warning" ]
                    [ text (gettext "This is not the latest available version of this Knowledge Model." appState.locale) ]

            else
                newVersionInRegistryWarning appState package
    in
    DetailPage.content
        [ nonEditableInfo
        , warning
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


sidePanel : AppState -> Model -> PackageDetail -> Html Msg
sidePanel appState model package =
    let
        sections =
            [ sidePanelKmInfo appState package
            , sidePanelOrganizationInfo appState package
            , sidePanelRegistryLink appState package
            , sidePanelOtherVersions appState model package
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


sidePanelOtherVersions : AppState -> Model -> PackageDetail -> Maybe ( String, String, Html Msg )
sidePanelOtherVersions appState model package =
    let
        versionLink version =
            li []
                [ linkTo appState
                    (Routes.knowledgeModelsDetail <| package.organizationId ++ ":" ++ package.kmId ++ ":" ++ Version.toString version)
                    []
                    [ text <| Version.toString version ]
                ]

        takeFirstVersions =
            if model.showAllVersions then
                identity

            else
                List.take 10

        versionLinks =
            package.versions
                |> List.filter ((/=) package.version)
                |> List.sortWith Version.compare
                |> List.reverse
                |> takeFirstVersions
                |> List.map versionLink
    in
    if List.length versionLinks > 0 then
        let
            showAllLink =
                if model.showAllVersions || List.length package.versions <= 10 then
                    emptyNode

                else
                    li [ class "show-all-link" ]
                        [ a [ onClick ShowAllVersions ]
                            [ text (gettext "Show all" appState.locale)
                            , faSet "detail.showAll" appState
                            ]
                        ]
        in
        Just ( gettext "Other versions" appState.locale, "other-versions", ul [] (versionLinks ++ [ showAllLink ]) )

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
            , ul [ class "fa-ul" ]
                [ li []
                    [ a [ href registryLink, target "_blank" ]
                        [ span [ class "fa-li" ] [ faSet "kmDetail.registryLink" appState ]
                        , span [ class "fa-li-content" ] [ text (gettext "View in registry" appState.locale) ]
                        ]
                    ]
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
