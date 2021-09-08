module Wizard.Templates.Detail.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Markdown
import Shared.Api.Templates as TemplatesApi
import Shared.Auth.Permission as Perm
import Shared.Data.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Shared.Data.OrganizationInfo exposing (OrganizationInfo)
import Shared.Data.Template.TemplatePackage as TemplatePackage
import Shared.Data.Template.TemplateState as TemplateState
import Shared.Data.TemplateDetail as TemplateDetail exposing (TemplateDetail)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lg, lh, lx)
import Shared.Utils exposing (listFilterJust, listInsertIf)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.KnowledgeModels.Routes
import Wizard.Routes as Routes
import Wizard.Templates.Detail.Models exposing (..)
import Wizard.Templates.Detail.Msgs exposing (..)
import Wizard.Templates.Routes exposing (Route(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Templates.Detail.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Templates.Detail.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Templates.Detail.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewPackage appState model) model.template


viewPackage : AppState -> Model -> TemplateDetail -> Html Msg
viewPackage appState model template =
    div [ class "KnowledgeModels__Detail" ]
        [ header appState template
        , readme appState template
        , sidePanel appState template
        , deleteVersionModal appState model template
        ]


header : AppState -> TemplateDetail -> Html Msg
header appState template =
    let
        exportAction =
            a [ class "link-with-icon", href <| TemplatesApi.exportTemplateUrl template.id appState, target "_blank" ]
                [ faSet "_global.export" appState
                , lx_ "header.export" appState
                ]

        exportActionVisible =
            Feature.templatesExport appState

        deleteAction =
            a [ onClick <| ShowDeleteDialog True, class "text-danger link-with-icon" ]
                [ faSet "_global.delete" appState
                , lx_ "header.delete" appState
                ]

        deleteActionVisible =
            Feature.templatesDelete appState

        actions =
            []
                |> listInsertIf exportAction exportActionVisible
                |> listInsertIf deleteAction deleteActionVisible
    in
    div [ class "top-header" ]
        [ div [ class "top-header-content" ]
            [ div [ class "top-header-title", dataCy "template_header-title" ] [ text template.name ]
            , div [ class "top-header-actions" ] actions
            ]
        ]


readme : AppState -> TemplateDetail -> Html msg
readme appState template =
    let
        containsNewerVersions =
            not <| TemplateDetail.isLatestVersion template

        warning =
            if containsNewerVersions then
                div [ class "alert alert-warning" ]
                    [ faSet "_global.warning" appState
                    , lx_ "readme.versionWarning" appState
                    ]

            else
                newVersionInRegistryWarning appState template
    in
    div [ class "KnowledgeModels__Detail__Readme" ]
        [ warning
        , unsupportedMetamodelVersionWarning appState template
        , Markdown.toHtml [ class "readme" ] template.readme
        ]


newVersionInRegistryWarning : AppState -> TemplateDetail -> Html msg
newVersionInRegistryWarning appState template =
    case ( template.remoteLatestVersion, template.state == TemplateState.Outdated, appState.config.registry ) of
        ( Just remoteLatestVersion, True, RegistryEnabled _ ) ->
            let
                latestPackageId =
                    template.organizationId ++ ":" ++ template.templateId ++ ":" ++ Version.toString remoteLatestVersion
            in
            div [ class "alert alert-warning" ]
                ([ faSet "_global.warning" appState ]
                    ++ lh_ "registryVersion.warning"
                        [ text (Version.toString remoteLatestVersion)
                        , linkTo appState
                            (Routes.TemplatesRoute <| ImportRoute <| Just <| latestPackageId)
                            []
                            [ lx_ "registryVersion.warning.import" appState ]
                        ]
                        appState
                )

        _ ->
            emptyNode


unsupportedMetamodelVersionWarning : AppState -> TemplateDetail -> Html msg
unsupportedMetamodelVersionWarning appState template =
    if template.state == TemplateState.UnsupportedMetamodelVersion then
        let
            link =
                case ( TemplateDetail.isLatestVersion template, template.remoteLatestVersion, appState.config.registry ) of
                    ( True, Just remoteLatestVersion, RegistryEnabled _ ) ->
                        let
                            latestPackageId =
                                template.organizationId ++ ":" ++ template.templateId ++ ":" ++ Version.toString remoteLatestVersion
                        in
                        text " "
                            :: lh_ "registryVersion.warning"
                                [ text (Version.toString remoteLatestVersion)
                                , linkTo appState
                                    (Routes.TemplatesRoute <| ImportRoute <| Just <| latestPackageId)
                                    []
                                    [ lx_ "registryVersion.warning.import" appState ]
                                ]
                                appState

                    _ ->
                        []
        in
        div [ class "alert alert-danger" ]
            ([ faSet "_global.warning" appState
             , lx_ "readme.unsupportedMetamodelVersion" appState
             ]
                ++ link
            )

    else
        emptyNode


sidePanel : AppState -> TemplateDetail -> Html msg
sidePanel appState template =
    let
        sections =
            [ sidePanelKmInfo appState template
            , sidePanelOtherVersions appState template
            , sidePanelOrganizationInfo appState template
            , sidePanelRegistryLink appState template
            , sidePanelUsableWith appState template
            ]
    in
    div [ class "KnowledgeModels__Detail__SidePanel" ]
        [ list 12 12 <| listFilterJust sections ]


sidePanelKmInfo : AppState -> TemplateDetail -> Maybe ( String, Html msg )
sidePanelKmInfo appState template =
    let
        templateInfoList =
            [ ( lg "template.id" appState, text template.id )
            , ( lg "template.version" appState, text <| Version.toString template.version )
            , ( lg "template.metamodel" appState, text <| String.fromInt template.metamodelVersion )
            , ( lg "template.license" appState, text template.license )
            ]
    in
    Just ( lg "template" appState, list 4 8 <| templateInfoList )


sidePanelOtherVersions : AppState -> TemplateDetail -> Maybe ( String, Html msg )
sidePanelOtherVersions appState template =
    let
        versionLink version =
            li []
                [ linkTo appState
                    (Routes.TemplatesRoute <| DetailRoute <| template.organizationId ++ ":" ++ template.templateId ++ ":" ++ Version.toString version)
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
        Just ( lg "template.otherVersions" appState, ul [] versionLinks )

    else
        Nothing


sidePanelOrganizationInfo : AppState -> TemplateDetail -> Maybe ( String, Html msg )
sidePanelOrganizationInfo appState template =
    case template.organization of
        Just organization ->
            Just ( lg "template.publishedBy" appState, viewOrganization organization )

        Nothing ->
            Nothing


sidePanelRegistryLink : AppState -> TemplateDetail -> Maybe ( String, Html msg )
sidePanelRegistryLink appState template =
    case template.registryLink of
        Just registryLink ->
            Just
                ( lg "template.registryLink" appState
                , a [ href registryLink, class "link-with-icon", target "_blank" ]
                    [ faSet "kmDetail.registryLink" appState
                    , text template.id
                    ]
                )

        Nothing ->
            Nothing


sidePanelUsableWith : AppState -> TemplateDetail -> Maybe ( String, Html msg )
sidePanelUsableWith appState template =
    let
        packageLink package =
            li []
                [ linkTo appState
                    (Routes.KnowledgeModelsRoute <| Wizard.KnowledgeModels.Routes.DetailRoute <| package.id)
                    [ dataCy "template_km-link" ]
                    [ text package.id ]
                ]

        packageLinks =
            template.usablePackages
                |> List.sortWith TemplatePackage.compareById
                |> List.map packageLink
    in
    if List.length packageLinks > 0 then
        Just ( lg "template.usableWith" appState, ul [] packageLinks )

    else
        Nothing


list : Int -> Int -> List ( String, Html msg ) -> Html msg
list colLabel colValue rows =
    let
        viewRow ( label, value ) =
            [ dt [ class <| "col-" ++ String.fromInt colLabel ]
                [ text label ]
            , dd [ class <| "col-" ++ String.fromInt colValue ]
                [ value ]
            ]
    in
    dl [ class "row" ] (List.concatMap viewRow rows)


viewOrganization : OrganizationInfo -> Html msg
viewOrganization organization =
    div [ class "organization" ]
        [ ItemIcon.view { text = organization.name, image = organization.logo }
        , div [ class "content" ]
            [ strong [] [ text organization.name ]
            , br [] []
            , text organization.organizationId
            ]
        ]


deleteVersionModal : AppState -> Model -> { a | id : String } -> Html Msg
deleteVersionModal appState model template =
    let
        modalContent =
            [ p []
                (lh_ "deleteModal.message" [ strong [] [ text template.id ] ] appState)
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
            , dataCy = "template-delete"
            }
    in
    Modal.confirm appState modalConfig
