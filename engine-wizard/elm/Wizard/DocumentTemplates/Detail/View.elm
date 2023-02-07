module Wizard.DocumentTemplates.Detail.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, p, span, strong, text, ul)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Shared.Components.Badge as Badge
import Shared.Data.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Shared.Data.DocumentTemplate.DocumentTemplatePackage as DocumentTemplatePackage
import Shared.Data.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase
import Shared.Data.DocumentTemplate.DocumentTemplateState as DocumentTemplateState
import Shared.Data.DocumentTemplateDetail as DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Shared.Data.OrganizationInfo exposing (OrganizationInfo)
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Markdown as Markdown
import Shared.Utils exposing (listFilterJust, listInsertIf)
import String.Format as String
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.DetailPage as DetailPage
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.DocumentTemplates.Detail.Models exposing (Model)
import Wizard.DocumentTemplates.Detail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewPackage appState model) model.template


viewPackage : AppState -> Model -> DocumentTemplateDetail -> Html Msg
viewPackage appState model template =
    DetailPage.container
        [ header appState template
        , readme appState template
        , sidePanel appState template
        , deleteVersionModal appState model template
        ]


header : AppState -> DocumentTemplateDetail -> Html Msg
header appState template =
    let
        createEditorAction =
            linkTo appState
                (Routes.documentTemplateEditorCreate (Just template.id) (Just True))
                [ dataCy "dt-detail_create-editor-link" ]
                [ faSet "_global.edit" appState
                , text (gettext "Create editor" appState.locale)
                ]

        createEditorActionVisible =
            Feature.templatesView appState

        setDeprecatedAction =
            a [ onClick (UpdatePhase DocumentTemplatePhase.Deprecated) ]
                [ faSet "documentTemplate.setDeprecated" appState
                , text (gettext "Set deprecated" appState.locale)
                ]

        setDeprecatedActionVisible =
            template.phase == DocumentTemplatePhase.Released

        restoreAction =
            a [ onClick (UpdatePhase DocumentTemplatePhase.Released) ]
                [ faSet "documentTemplate.restore" appState
                , text (gettext "Restore" appState.locale)
                ]

        restoreActionVisible =
            template.phase == DocumentTemplatePhase.Deprecated

        exportAction =
            a [ onClick (ExportTemplate template) ]
                [ faSet "_global.export" appState
                , text (gettext "Export" appState.locale)
                ]

        exportActionVisible =
            Feature.templatesExport appState

        deleteAction =
            a [ onClick <| ShowDeleteDialog True, class "text-danger with-icon" ]
                [ faSet "_global.delete" appState
                , text (gettext "Delete" appState.locale)
                ]

        deleteActionVisible =
            Feature.templatesDelete appState

        actions =
            []
                |> listInsertIf createEditorAction createEditorActionVisible
                |> listInsertIf setDeprecatedAction setDeprecatedActionVisible
                |> listInsertIf restoreAction restoreActionVisible
                |> listInsertIf exportAction exportActionVisible
                |> listInsertIf deleteAction deleteActionVisible

        deprecatedBadge =
            if template.phase == DocumentTemplatePhase.Deprecated then
                Badge.danger [] [ text "deprecated" ]

            else
                emptyNode
    in
    DetailPage.header (span [] [ text template.name, deprecatedBadge ]) actions


readme : AppState -> DocumentTemplateDetail -> Html msg
readme appState template =
    let
        containsNewerVersions =
            not <| DocumentTemplateDetail.isLatestVersion template

        warning =
            if containsNewerVersions then
                div [ class "alert alert-warning" ]
                    [ faSet "_global.warning" appState
                    , text (gettext "This is not the latest available version of this document template." appState.locale)
                    ]

            else
                newVersionInRegistryWarning appState template
    in
    DetailPage.content
        [ warning
        , unsupportedMetamodelVersionWarning appState template
        , Markdown.toHtml [ DetailPage.contentInnerClass ] template.readme
        ]


newVersionInRegistryWarning : AppState -> DocumentTemplateDetail -> Html msg
newVersionInRegistryWarning appState template =
    case ( template.remoteLatestVersion, template.state == DocumentTemplateState.Outdated, appState.config.registry ) of
        ( Just remoteLatestVersion, True, RegistryEnabled _ ) ->
            let
                importLink =
                    if Feature.templatesImport appState && Version.greaterThan template.version remoteLatestVersion then
                        let
                            latestPackageId =
                                template.organizationId ++ ":" ++ template.templateId ++ ":" ++ Version.toString remoteLatestVersion
                        in
                        [ linkTo appState
                            (Routes.documentTemplatesImport (Just latestPackageId))
                            [ class "btn btn-primary btn-sm with-icon ms-2" ]
                            [ faSet "kmImport.fromRegistry" appState, text (gettext "Import" appState.locale) ]
                        ]

                    else
                        []
            in
            div [ class "alert alert-warning" ]
                (faSet "_global.warning" appState
                    :: String.formatHtml
                        (gettext "There is a newer version (%s) available." appState.locale)
                        [ strong [] [ text (Version.toString remoteLatestVersion) ] ]
                    ++ importLink
                )

        _ ->
            emptyNode


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
                                    if Feature.templatesImport appState then
                                        let
                                            latestPackageId =
                                                template.organizationId ++ ":" ++ template.templateId ++ ":" ++ Version.toString remoteLatestVersion
                                        in
                                        [ linkTo appState
                                            (Routes.documentTemplatesImport (Just latestPackageId))
                                            [ class "btn btn-primary btn-sm with-icon ms-2" ]
                                            [ faSet "kmImport.fromRegistry" appState, text (gettext "Import" appState.locale) ]
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

                    _ ->
                        []
        in
        div [ class "alert alert-danger" ]
            ([ faSet "_global.warning" appState
             , text (gettext "This document template is not supported in this version of DSW." appState.locale)
             ]
                ++ link
            )

    else
        emptyNode


sidePanel : AppState -> DocumentTemplateDetail -> Html msg
sidePanel appState template =
    let
        sections =
            [ sidePanelKmInfo appState template
            , sidePanelFormats appState template
            , sidePanelOtherVersions appState template
            , sidePanelOrganizationInfo appState template
            , sidePanelRegistryLink appState template
            , sidePanelUsableWith appState template
            ]
    in
    DetailPage.sidePanel
        [ DetailPage.sidePanelList 12 12 <| listFilterJust sections ]


sidePanelKmInfo : AppState -> DocumentTemplateDetail -> Maybe ( String, String, Html msg )
sidePanelKmInfo appState template =
    let
        templateInfoList =
            [ ( gettext "ID" appState.locale, "id", text template.id )
            , ( gettext "Version" appState.locale, "version", text <| Version.toString template.version )
            , ( gettext "Metamodel" appState.locale, "metamodel", text <| String.fromInt template.metamodelVersion )
            , ( gettext "License" appState.locale, "license", text template.license )
            ]
    in
    Just ( gettext "Document Template" appState.locale, "template", DetailPage.sidePanelList 4 8 templateInfoList )


sidePanelFormats : AppState -> DocumentTemplateDetail -> Maybe ( String, String, Html msg )
sidePanelFormats appState template =
    let
        formatView format =
            li [] [ fa format.icon, text format.name ]

        formats =
            template.formats
                |> List.sortBy .name
                |> List.map formatView
    in
    if List.length formats > 0 then
        Just ( gettext "Formats" appState.locale, "formats", ul [ class "format-list" ] formats )

    else
        Nothing


sidePanelOtherVersions : AppState -> DocumentTemplateDetail -> Maybe ( String, String, Html msg )
sidePanelOtherVersions appState template =
    let
        versionLink version =
            li []
                [ linkTo appState
                    (Routes.documentTemplatesDetail <| template.organizationId ++ ":" ++ template.templateId ++ ":" ++ Version.toString version)
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
            ( gettext "Registry link" appState.locale
            , "registry-link"
            , a [ href registryLink, target "_blank", class "with-icon" ]
                [ faSet "kmDetail.registryLink" appState
                , text (gettext "View in registry" appState.locale)
                ]
            )
    in
    Maybe.map toRegistryLink template.registryLink


sidePanelUsableWith : AppState -> DocumentTemplateDetail -> Maybe ( String, String, Html msg )
sidePanelUsableWith appState template =
    let
        packageLink package =
            li []
                [ linkTo appState
                    (Routes.knowledgeModelsDetail package.id)
                    [ dataCy "template_km-link" ]
                    [ text package.id ]
                ]

        packageLinks =
            template.usablePackages
                |> List.sortWith DocumentTemplatePackage.compareById
                |> List.map packageLink
    in
    if List.length packageLinks > 0 then
        Just ( gettext "Usable with" appState.locale, "usable-with", ul [] packageLinks )

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
            { modalTitle = gettext "Delete version" appState.locale
            , modalContent = modalContent
            , visible = model.showDeleteDialog
            , actionResult = model.deletingVersion
            , actionName = gettext "Delete" appState.locale
            , actionMsg = DeleteVersion
            , cancelMsg = Just <| ShowDeleteDialog False
            , dangerous = True
            , dataCy = "template-delete"
            }
    in
    Modal.confirm appState modalConfig
