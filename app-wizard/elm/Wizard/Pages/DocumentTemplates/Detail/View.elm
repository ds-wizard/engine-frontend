module Wizard.Pages.DocumentTemplates.Detail.View exposing (view)

import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (fa, faDetailShowAll, faInfo, faKmDetailRegistryLink, faKmImportFromRegistry, faWarning)
import Common.Components.Modal as Modal
import Common.Components.Page as Page
import Common.Utils.Markdown as Markdown
import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, p, span, strong, text, ul)
import Html.Attributes exposing (class, href, target)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import String.Format as String
import Version
import Wizard.Api.Models.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Wizard.Api.Models.DocumentTemplate as DocumentTemplate
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePackage as DocumentTemplatePackage
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateState as DocumentTemplateState
import Wizard.Api.Models.DocumentTemplateDetail as DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Wizard.Api.Models.OrganizationInfo exposing (OrganizationInfo)
import Wizard.Components.DetailPage as DetailPage
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.ItemIcon as ItemIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.DocumentTemplates.Common.DocumentTemplateActionsDropdown as DocumentTemplateActionsDropdown
import Wizard.Pages.DocumentTemplates.Detail.Models exposing (Model)
import Wizard.Pages.DocumentTemplates.Detail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Feature
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewDocumentTemplate appState model) model.template


viewDocumentTemplate : AppState -> Model -> DocumentTemplateDetail -> Html Msg
viewDocumentTemplate appState model template =
    DetailPage.container
        [ header appState model template
        , readme appState template
        , sidePanel appState model template
        , deleteVersionModal appState model template
        ]


header : AppState -> Model -> DocumentTemplateDetail -> Html Msg
header appState model template =
    let
        deprecatedBadge =
            if template.phase == DocumentTemplatePhase.Deprecated then
                Badge.danger [] [ text (gettext "deprecated" appState.locale) ]

            else
                Html.nothing

        nonEditableBadge =
            if template.nonEditable then
                Badge.dark [] [ text (gettext "non-editable" appState.locale) ]

            else
                Html.nothing

        dropdownActions =
            DocumentTemplateActionsDropdown.dropdown appState
                { dropdownState = model.dropdownState
                , toggleMsg = DropdownMsg
                }
                { exportMsg = ExportTemplate
                , updatePhaseMsg = \_ phase -> UpdatePhase phase
                , deleteMsg = always (ShowDeleteDialog True)
                , viewActionVisible = False
                }
                template
    in
    DetailPage.header (span [] [ text template.name, nonEditableBadge, deprecatedBadge ]) [ dropdownActions ]


readme : AppState -> DocumentTemplateDetail -> Html msg
readme appState template =
    let
        containsNewerVersions =
            not <| DocumentTemplateDetail.isLatestVersion template

        nonEditableInfo =
            if template.nonEditable then
                div [ class "alert alert-info" ]
                    [ faInfo
                    , text (gettext "This is a non-editable document template, i.e., it cannot be edited, or exported." appState.locale)
                    ]

            else
                Html.nothing

        warning =
            if containsNewerVersions then
                div [ class "alert alert-warning" ]
                    [ faWarning
                    , text (gettext "This is not the latest available version of this document template." appState.locale)
                    ]

            else
                newVersionInRegistryWarning appState template
    in
    DetailPage.content
        [ nonEditableInfo
        , warning
        , unsupportedMetamodelVersionWarning appState template
        , Markdown.toHtml [ DetailPage.contentInnerClass ] template.readme
        ]


newVersionInRegistryWarning : AppState -> DocumentTemplateDetail -> Html msg
newVersionInRegistryWarning appState template =
    if template.state /= DocumentTemplateState.UnsupportedMetamodelVersion then
        case ( template.remoteLatestVersion, DocumentTemplate.isOutdated template, appState.config.registry ) of
            ( Just remoteLatestVersion, True, RegistryEnabled _ ) ->
                let
                    importLink =
                        if Feature.documentTemplatesImport appState && Version.greaterThan template.version remoteLatestVersion then
                            let
                                latestPackageId =
                                    template.organizationId ++ ":" ++ template.templateId ++ ":" ++ Version.toString remoteLatestVersion
                            in
                            [ linkTo (Routes.documentTemplatesImport (Just latestPackageId))
                                [ class "btn btn-primary btn-sm with-icon ms-2" ]
                                [ faKmImportFromRegistry, text (gettext "Import" appState.locale) ]
                            ]

                        else
                            []
                in
                div [ class "alert alert-warning" ]
                    (faWarning
                        :: String.formatHtml
                            (gettext "There is a newer version (%s) available." appState.locale)
                            [ strong [] [ text (Version.toString remoteLatestVersion) ] ]
                        ++ importLink
                    )

            _ ->
                Html.nothing

    else
        Html.nothing


unsupportedMetamodelVersionWarning : AppState -> DocumentTemplateDetail -> Html msg
unsupportedMetamodelVersionWarning appState template =
    if template.state == DocumentTemplateState.UnsupportedMetamodelVersion then
        let
            link =
                case ( DocumentTemplateDetail.isLatestVersion template, template.remoteLatestVersion, appState.config.registry ) of
                    ( True, Just remoteLatestVersion, RegistryEnabled _ ) ->
                        if Version.greaterThan template.version remoteLatestVersion then
                            let
                                importLink =
                                    if Feature.documentTemplatesImport appState then
                                        let
                                            latestPackageId =
                                                template.organizationId ++ ":" ++ template.templateId ++ ":" ++ Version.toString remoteLatestVersion
                                        in
                                        [ linkTo (Routes.documentTemplatesImport (Just latestPackageId))
                                            [ class "btn btn-primary btn-sm with-icon ms-2" ]
                                            [ faKmImportFromRegistry, text (gettext "Import" appState.locale) ]
                                        ]

                                    else
                                        []
                            in
                            text " "
                                :: String.formatHtml
                                    (gettext "There is a newer version (%s) available." appState.locale)
                                    [ strong [] [ text (Version.toString remoteLatestVersion) ] ]
                                ++ importLink

                        else
                            []

                    ( True, _, _ ) ->
                        [ a
                            [ href (WizardGuideLinks.documentTemplatesUnsupportedMetamodel appState.guideLinks)
                            , target "_blank"
                            , class "ms-1"
                            ]
                            [ text (gettext "Learn more in guide" appState.locale) ]
                        ]

                    _ ->
                        []
        in
        div [ class "alert alert-danger" ]
            ([ faWarning
             , text (gettext "This document template is not supported in the current version." appState.locale)
             ]
                ++ link
            )

    else
        Html.nothing


sidePanel : AppState -> Model -> DocumentTemplateDetail -> Html Msg
sidePanel appState model template =
    let
        sections =
            [ sidePanelKmInfo appState template
            , sidePanelOrganizationInfo appState template
            , sidePanelRegistryLink appState template
            , sidePanelFormats appState template
            , sidePanelOtherVersions appState template
            , sidePanelUsableWith appState model template
            ]
    in
    DetailPage.sidePanel
        [ DetailPage.sidePanelList 12 12 <| List.filterMap identity sections ]


sidePanelKmInfo : AppState -> DocumentTemplateDetail -> Maybe ( String, String, Html msg )
sidePanelKmInfo appState template =
    let
        templateInfoList =
            [ ( gettext "ID" appState.locale, "id", text template.id )
            , ( gettext "Version" appState.locale, "version", text <| Version.toString template.version )
            , ( gettext "Metamodel" appState.locale, "metamodel", text <| Version.toStringMinor template.metamodelVersion )
            , ( gettext "License" appState.locale, "license", text template.license )
            ]
    in
    Just ( gettext "Document Template" appState.locale, "template", DetailPage.sidePanelList 4 8 templateInfoList )


sidePanelFormats : AppState -> DocumentTemplateDetail -> Maybe ( String, String, Html msg )
sidePanelFormats appState template =
    let
        formatView format =
            li []
                [ span [ class "fa-li" ] [ fa format.icon ]
                , span [ class "fa-li-content" ] [ text format.name ]
                ]

        formats =
            template.formats
                |> List.sortBy .name
                |> List.map formatView
    in
    if List.length formats > 0 then
        Just ( gettext "Formats" appState.locale, "formats", ul [ class "fa-ul" ] formats )

    else
        Nothing


sidePanelOtherVersions : AppState -> DocumentTemplateDetail -> Maybe ( String, String, Html msg )
sidePanelOtherVersions appState template =
    let
        versionLink version =
            li []
                [ linkTo (Routes.documentTemplatesDetail <| template.organizationId ++ ":" ++ template.templateId ++ ":" ++ Version.toString version)
                    []
                    [ text <| Version.toString version ]
                ]

        versionLinks =
            template.versions
                |> List.filter ((/=) template.version)
                |> List.sortWith Version.compare
                |> List.reverse
                |> List.map versionLink
    in
    if List.length versionLinks > 0 then
        Just ( gettext "Other versions" appState.locale, "other-versions", ul [] versionLinks )

    else
        Nothing


sidePanelOrganizationInfo : AppState -> DocumentTemplateDetail -> Maybe ( String, String, Html msg )
sidePanelOrganizationInfo appState template =
    Maybe.map (\organization -> ( gettext "Published by" appState.locale, "published-by", viewOrganization organization )) template.organization


sidePanelRegistryLink : AppState -> DocumentTemplateDetail -> Maybe ( String, String, Html msg )
sidePanelRegistryLink appState template =
    let
        toRegistryLink registryLink =
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
    Maybe.map toRegistryLink template.registryLink


sidePanelUsableWith : AppState -> Model -> DocumentTemplateDetail -> Maybe ( String, String, Html Msg )
sidePanelUsableWith appState model template =
    let
        packageLink package =
            li []
                [ linkTo (Routes.knowledgeModelsDetail package.id)
                    [ dataCy "template_km-link" ]
                    [ text package.id ]
                ]

        takeFirstPackages =
            if model.showAllKms then
                identity

            else
                List.take 10

        packageLinks =
            template.usablePackages
                |> List.sortWith DocumentTemplatePackage.compareById
                |> takeFirstPackages
                |> List.map packageLink
    in
    if List.length packageLinks > 0 then
        let
            showAllLink =
                if model.showAllKms || List.length template.usablePackages <= 10 then
                    Html.nothing

                else
                    li [ class "show-all-link" ]
                        [ a [ onClick ShowAllKms ]
                            [ text (gettext "Show all" appState.locale)
                            , faDetailShowAll
                            ]
                        ]
        in
        Just ( gettext "Usable with" appState.locale, "usable-with", ul [] (packageLinks ++ [ showAllLink ]) )

    else
        Nothing


viewOrganization : OrganizationInfo -> Html msg
viewOrganization organization =
    DetailPage.sidePanelItemWithIcon organization.name
        (text organization.organizationId)
        (ItemIcon.view { text = organization.name, image = organization.logo })


deleteVersionModal : AppState -> Model -> { a | id : String } -> Html Msg
deleteVersionModal appState model template =
    let
        modalContent =
            [ p []
                (String.formatHtml
                    (gettext "Are you sure you want to permanently delete %s?" appState.locale)
                    [ strong [] [ text template.id ] ]
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
                |> Modal.confirmConfigDataCy "template-delete"
    in
    Modal.confirm appState modalConfig
