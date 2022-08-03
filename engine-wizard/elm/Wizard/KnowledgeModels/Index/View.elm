module Wizard.KnowledgeModels.Index.View exposing (view)

import Html exposing (Html, code, div, img, p, span, strong, text)
import Html.Attributes exposing (class, src, title)
import Shared.Components.Badge as Badge
import Shared.Data.Package exposing (Package)
import Shared.Data.Package.PackageState as PackageState
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lg, lh, lx)
import Shared.Utils exposing (listInsertIf)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ListingActionType(..), ListingDropdownItem, ViewConfig)
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass, tooltip)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.KnowledgeModels.Index.Models exposing (Model)
import Wizard.KnowledgeModels.Index.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.KnowledgeModels.Index.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.KnowledgeModels.Index.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KnowledgeModels.Index.View"


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "KnowledgeModels__Index" ]
        [ Page.header (l_ "header.title" appState) []
        , FormResult.successOnlyView appState model.deletingPackage
        , Listing.view appState (listingConfig appState) model.packages
        , deleteModal appState model
        ]


importButton : AppState -> Html Msg
importButton appState =
    if Feature.knowledgeModelsImport appState then
        linkTo appState
            (Routes.knowledgeModelsImport Nothing)
            [ class "btn btn-primary" ]
            [ faSet "kms.upload" appState
            , lx_ "header.import" appState
            ]

    else
        emptyNode


listingConfig : AppState -> ViewConfig Package Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , itemAdditionalData = always Nothing
    , dropdownItems = listingActions appState
    , textTitle = .name
    , emptyText = l_ "listing.empty" appState
    , updated =
        Just
            { getTime = .createdAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Just (l_ "listing.searchPlaceholderText" appState)
    , sortOptions =
        [ ( "name", lg "package.name" appState )
        , ( "createdAt", lg "package.createdAt" appState )
        ]
    , filters = []
    , toRoute = \_ -> Routes.KnowledgeModelsRoute << IndexRoute
    , toolbarExtra = Just (importButton appState)
    }


listingTitle : AppState -> Package -> Html Msg
listingTitle appState package =
    span []
        [ linkTo appState (Routes.knowledgeModelsDetail package.id) [] [ text package.name ]
        , Badge.light
            (tooltip <| lg "package.latestVersion" appState)
            [ text <| Version.toString package.version ]
        , listingTitleOutdatedBadge appState package
        ]


listingTitleOutdatedBadge : AppState -> Package -> Html Msg
listingTitleOutdatedBadge appState package =
    if PackageState.isOutdated package.state then
        let
            packageId =
                Maybe.map ((++) (package.organizationId ++ ":" ++ package.kmId ++ ":")) package.remoteLatestVersion
        in
        linkTo appState
            (Routes.knowledgeModelsImport packageId)
            [ class Badge.warningClass ]
            [ lx_ "badge.outdated" appState ]

    else
        emptyNode


listingDescription : AppState -> Package -> Html Msg
listingDescription appState package =
    let
        organizationFragment =
            case package.organization of
                Just organization ->
                    let
                        logo =
                            case organization.logo of
                                Just organizationLogo ->
                                    img [ class "organization-image", src organizationLogo ] []

                                Nothing ->
                                    emptyNode
                    in
                    span [ class "fragment", title <| lg "package.publishedBy" appState ]
                        [ logo
                        , text organization.name
                        ]

                Nothing ->
                    emptyNode
    in
    span []
        [ code [ class "fragment" ] [ text package.id ]
        , organizationFragment
        , span [ class "fragment" ] [ text package.description ]
        ]


listingActions : AppState -> Package -> List (ListingDropdownItem Msg)
listingActions appState package =
    let
        viewAction =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.view" appState
                , label = lg "km.action.view" appState
                , msg = ListingActionLink (Routes.knowledgeModelsDetail package.id)
                , dataCy = "view"
                }

        viewActionVisible =
            Feature.knowledgeModelsView appState

        exportAction =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.export" appState
                , label = lg "km.action.export" appState
                , msg = ListingActionMsg (ExportPackage package)
                , dataCy = "export"
                }

        exportActionVisible =
            Feature.knowledgeModelsExport appState

        createKMEditor =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmDetail.createKMEditor" appState
                , label = lg "km.action.kmEditor" appState
                , msg = ListingActionLink (Routes.kmEditorCreate (Just package.id) (Just True))
                , dataCy = "create-km-editor"
                }

        createKMEditorVisible =
            Feature.knowledgeModelEditorsCreate appState

        forkAction =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmDetail.fork" appState
                , label = lg "km.action.fork" appState
                , msg = ListingActionLink (Routes.kmEditorCreate (Just package.id) Nothing)
                , dataCy = "fork"
                }

        forkActionVisible =
            Feature.knowledgeModelEditorsCreate appState

        questionnaireAction =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmDetail.createQuestionnaire" appState
                , label = lg "km.action.project" appState
                , msg = ListingActionLink (Routes.projectsCreateCustom <| Just package.id)
                , dataCy = "create-project"
                }

        questionnaireActionVisible =
            Feature.projectsCreateCustom appState

        deleteAction =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = lg "km.action.delete" appState
                , msg = ListingActionMsg <| ShowHideDeletePackage <| Just package
                , dataCy = "delete"
                }

        deleteActionVisible =
            Feature.knowledgeModelsDelete appState
    in
    []
        |> listInsertIf viewAction viewActionVisible
        |> listInsertIf exportAction exportActionVisible
        |> listInsertIf Listing.dropdownSeparator (createKMEditorVisible || forkActionVisible || questionnaireActionVisible)
        |> listInsertIf createKMEditor createKMEditorVisible
        |> listInsertIf forkAction forkActionVisible
        |> listInsertIf questionnaireAction questionnaireActionVisible
        |> listInsertIf Listing.dropdownSeparator deleteActionVisible
        |> listInsertIf deleteAction deleteActionVisible


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    let
        ( visible, version ) =
            case model.packageToBeDeleted of
                Just package ->
                    ( True, package.organizationId ++ ":" ++ package.kmId )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                (lh_ "deleteModal.message" [ strong [] [ text version ] ] appState)
            ]

        modalConfig =
            { modalTitle = l_ "deleteModal.title" appState
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingPackage
            , actionName = l_ "deleteModal.action" appState
            , actionMsg = DeletePackage
            , cancelMsg = Just <| ShowHideDeletePackage Nothing
            , dangerous = True
            , dataCy = "km-delete"
            }
    in
    Modal.confirm appState modalConfig
