module Wizard.Pages.KnowledgeModels.Detail.View exposing (view)

import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (faDetailShowAll, faInfo, faKmDetailRegistryLink, faKmImportFromRegistry, faWarning)
import Common.Components.Modal as Modal
import Common.Components.Page as Page
import Common.Utils.Markdown as Markdown
import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, p, span, strong, text, ul)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Html.Extra as Html
import String.Format as String
import Version
import Wizard.Api.Models.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Wizard.Api.Models.OrganizationInfo exposing (OrganizationInfo)
import Wizard.Api.Models.Package as Package
import Wizard.Api.Models.Package.PackagePhase as PackagePhase
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)
import Wizard.Components.DetailPage as DetailPage
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.ItemIcon as ItemIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KnowledgeModels.Common.KnowledgeModelActionsDropdown as KnowledgeModelActionsDropdown
import Wizard.Pages.KnowledgeModels.Detail.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Detail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Feature


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
                Html.nothing

        nonEditableBadge =
            if package.nonEditable then
                Badge.dark [] [ text (gettext "non-editable" appState.locale) ]

            else
                Html.nothing

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
                newVersionInRegistryWarning appState package
    in
    DetailPage.content
        [ nonEditableInfo
        , warning
        , Markdown.toHtml [ DetailPage.contentInnerClass ] package.readme
        ]


newVersionInRegistryWarning : AppState -> PackageDetail -> Html msg
newVersionInRegistryWarning appState package =
    case ( package.remoteLatestVersion, Package.isOutdated package, appState.config.registry ) of
        ( Just remoteLatestVersion, True, RegistryEnabled _ ) ->
            let
                importLink =
                    if Feature.knowledgeModelsImport appState then
                        let
                            latestPackageId =
                                package.organizationId ++ ":" ++ package.kmId ++ ":" ++ Version.toString remoteLatestVersion
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
        [ DetailPage.sidePanelList 12 12 <| List.filterMap identity sections ]


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
                      , linkTo (Routes.knowledgeModelsDetail parentPackageId) [] [ text parentPackageId ]
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
                [ linkTo (Routes.knowledgeModelsDetail <| package.organizationId ++ ":" ++ package.kmId ++ ":" ++ Version.toString version)
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
                        [ span [ class "fa-li" ] [ faKmDetailRegistryLink ]
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
            Modal.confirmConfig (gettext "Delete version" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible model.showDeleteDialog
                |> Modal.confirmConfigActionResult model.deletingVersion
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) DeleteVersion
                |> Modal.confirmConfigCancelMsg (ShowDeleteDialog False)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "km-delete-version"
    in
    Modal.confirm appState modalConfig
