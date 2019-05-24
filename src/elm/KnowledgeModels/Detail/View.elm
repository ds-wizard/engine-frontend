module KnowledgeModels.Detail.View exposing (view)

import Auth.Permission as Perm exposing (hasPerm)
import Common.Api.Packages as PackagesApi
import Common.AppState exposing (AppState)
import Common.Config exposing (Registry(..))
import Common.Html exposing (emptyNode, fa, linkTo)
import Common.View.ItemIcon as ItemIcon
import Common.View.Modal as Modal
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import KMEditor.Routing
import KnowledgeModels.Common.OrganizationInfo exposing (OrganizationInfo)
import KnowledgeModels.Common.PackageDetail exposing (PackageDetail)
import KnowledgeModels.Common.PackageState as PackageState
import KnowledgeModels.Common.Version as Version
import KnowledgeModels.Detail.Models exposing (..)
import KnowledgeModels.Detail.Msgs exposing (..)
import KnowledgeModels.Routing exposing (Route(..))
import Markdown
import Questionnaires.Routing
import Routing
import Utils exposing (listFilterJust, listInsertIf)


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView (viewPackage appState model) model.package


viewPackage : AppState -> Model -> PackageDetail -> Html Msg
viewPackage appState model package =
    div [ class "KnowledgeModels__Detail" ]
        [ header appState package
        , readme appState package
        , sidePanel package
        , deleteVersionModal model package
        ]


header : AppState -> PackageDetail -> Html Msg
header appState package =
    let
        exportAction =
            a [ class "link-with-icon", href <| PackagesApi.exportPackageUrl package.id appState, target "_blank" ]
                [ fa "download", text "Export" ]

        forkAction =
            linkTo (Routing.KMEditor <| KMEditor.Routing.CreateRoute <| Just package.id)
                [ class "link-with-icon" ]
                [ fa "code-fork"
                , text "Fork knowledge model"
                ]

        questionnaireAction =
            linkTo (Routing.Questionnaires <| Questionnaires.Routing.Create <| Just package.id)
                [ class "link-with-icon" ]
                [ fa "list-alt"
                , text "Create Questionnaire"
                ]

        deleteAction =
            a [ onClick <| ShowDeleteDialog True, class "text-danger link-with-icon" ]
                [ fa "trash-o"
                , text "Delete"
                ]

        actions =
            []
                |> listInsertIf exportAction (hasPerm appState.jwt Perm.packageManagementWrite)
                |> listInsertIf forkAction (hasPerm appState.jwt Perm.knowledgeModel)
                |> listInsertIf questionnaireAction (hasPerm appState.jwt Perm.questionnaire)
                |> listInsertIf deleteAction (hasPerm appState.jwt Perm.packageManagementWrite)
    in
    div [ class "KnowledgeModels__Detail__Header" ]
        [ div [ class "header-content" ]
            [ div [ class "name" ] [ text package.name ]
            , div [ class "actions" ] actions
            ]
        ]


readme : AppState -> PackageDetail -> Html msg
readme appState package =
    let
        outdated =
            case ( package.remoteLatestVersion, PackageState.isOutdated package.state, appState.config.registry ) of
                ( Just remoteLatestVersion, True, RegistryEnabled _ ) ->
                    let
                        latestPackageId =
                            package.organizationId ++ ":" ++ package.kmId ++ ":" ++ Version.toString remoteLatestVersion
                    in
                    div [ class "alert alert-warning" ]
                        [ fa "exclamation-triangle"
                        , text <| "There is a newer version (" ++ Version.toString remoteLatestVersion ++ ") available in the registry, you can "
                        , linkTo (Routing.KnowledgeModels <| KnowledgeModels.Routing.Import <| Just <| latestPackageId)
                            []
                            [ text "import" ]
                        , text " it."
                        ]

                _ ->
                    emptyNode
    in
    div [ class "KnowledgeModels__Detail__Readme" ]
        [ outdated
        , Markdown.toHtml [ class "readme" ] package.readme
        ]


sidePanel : PackageDetail -> Html msg
sidePanel package =
    let
        sections =
            [ sidePanelKmInfo package
            , sidePanelOtherVersions package
            , sidePanelOrganizationInfo package
            , sidePanelRegistryLink package
            ]
    in
    div [ class "KnowledgeModels__Detail__SidePanel" ]
        [ list 12 12 <| listFilterJust sections ]


sidePanelKmInfo : PackageDetail -> Maybe ( String, Html msg )
sidePanelKmInfo package =
    let
        kmInfoList =
            [ ( "ID:", text package.id )
            , ( "Version:", text <| Version.toString package.version )
            , ( "Metadmodel:", text <| String.fromInt package.metamodelVersion )
            ]

        parentInfo =
            case package.parentPackageId of
                Just parentPackageId ->
                    [ ( "Parent KM:"
                      , linkTo (Routing.KnowledgeModels <| Detail parentPackageId)
                            []
                            [ text parentPackageId ]
                      )
                    ]

                Nothing ->
                    []
    in
    Just ( "Knowledge Model", list 4 8 <| kmInfoList ++ parentInfo )


sidePanelOtherVersions : PackageDetail -> Maybe ( String, Html msg )
sidePanelOtherVersions package =
    let
        versionLink version =
            li []
                [ linkTo (Routing.KnowledgeModels <| Detail <| package.organizationId ++ ":" ++ package.kmId ++ ":" ++ Version.toString version)
                    []
                    [ text <| Version.toString version ]
                ]

        versionLinks =
            package.versions
                |> List.filter ((/=) package.version)
                |> List.sortWith Version.compare
                |> List.map versionLink
    in
    if List.length versionLinks > 0 then
        Just ( "Other Versions", ul [] versionLinks )

    else
        Nothing


sidePanelOrganizationInfo : PackageDetail -> Maybe ( String, Html msg )
sidePanelOrganizationInfo package =
    case package.organization of
        Just organization ->
            Just ( "Published by", viewOrganization organization )

        Nothing ->
            Nothing


sidePanelRegistryLink : PackageDetail -> Maybe ( String, Html msg )
sidePanelRegistryLink package =
    case package.registryLink of
        Just registryLink ->
            Just
                ( "Registry Link"
                , a [ href registryLink, class "link-with-icon", target "_blank" ]
                    [ fa "external-link"
                    , text package.id
                    ]
                )

        Nothing ->
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


deleteVersionModal : Model -> PackageDetail -> Html Msg
deleteVersionModal model package =
    let
        modalContent =
            [ p []
                [ text "Are you sure you want to permanently delete "
                , strong [] [ text package.id ]
                , text "?"
                ]
            ]

        modalConfig =
            { modalTitle = "Delete version"
            , modalContent = modalContent
            , visible = model.showDeleteDialog
            , actionResult = model.deletingVersion
            , actionName = "Delete"
            , actionMsg = DeleteVersion
            , cancelMsg = Just <| ShowDeleteDialog False
            , dangerous = True
            }
    in
    Modal.confirm modalConfig
