module Wizard.KnowledgeModels.Index.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, code, div, img, p, span, strong, text)
import Html.Attributes exposing (class, src, title)
import Shared.Components.Badge as Badge
import Shared.Data.Package exposing (Package)
import Shared.Data.Package.PackageState as PackageState
import Shared.Html exposing (emptyNode, faSet)
import Shared.Utils exposing (listInsertIf)
import String.Format as String
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


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "" ]
        [ Page.header (gettext "Knowledge Models" appState.locale) []
        , FormResult.successOnlyView appState model.deletingPackage
        , Listing.view appState (listingConfig appState) model.packages
        , deleteModal appState model
        ]


importButton : AppState -> Html Msg
importButton appState =
    if Feature.knowledgeModelsImport appState then
        linkTo appState
            (Routes.knowledgeModelsImport Nothing)
            [ class "btn btn-primary with-icon" ]
            [ faSet "kms.upload" appState
            , text (gettext "Import" appState.locale)
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
    , emptyText = gettext "Click \"Import\" button to import a new Knowledge Model." appState.locale
    , updated =
        Just
            { getTime = .createdAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Just (gettext "Search KMs..." appState.locale)
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "createdAt", gettext "Created" appState.locale )
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
            (tooltip <| gettext "Latest version" appState.locale)
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
            [ text (gettext "update available" appState.locale) ]

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
                    span [ class "fragment", title <| gettext "Published by" appState.locale ]
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
                , label = gettext "View detail" appState.locale
                , msg = ListingActionLink (Routes.knowledgeModelsDetail package.id)
                , dataCy = "view"
                }

        viewActionVisible =
            Feature.knowledgeModelsView appState

        exportAction =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.export" appState
                , label = gettext "Export" appState.locale
                , msg = ListingActionMsg (ExportPackage package)
                , dataCy = "export"
                }

        exportActionVisible =
            Feature.knowledgeModelsExport appState

        createKMEditor =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmDetail.createKMEditor" appState
                , label = gettext "Create KM editor" appState.locale
                , msg = ListingActionLink (Routes.kmEditorCreate (Just package.id) (Just True))
                , dataCy = "create-km-editor"
                }

        createKMEditorVisible =
            Feature.knowledgeModelEditorsCreate appState

        forkAction =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmDetail.fork" appState
                , label = gettext "Fork KM" appState.locale
                , msg = ListingActionLink (Routes.kmEditorCreate (Just package.id) Nothing)
                , dataCy = "fork"
                }

        forkActionVisible =
            Feature.knowledgeModelEditorsCreate appState

        questionnaireAction =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmDetail.createQuestionnaire" appState
                , label = gettext "Create project" appState.locale
                , msg = ListingActionLink (Routes.projectsCreateCustom <| Just package.id)
                , dataCy = "create-project"
                }

        questionnaireActionVisible =
            Feature.projectsCreateCustom appState

        deleteAction =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = gettext "Delete" appState.locale
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
                (String.formatHtml
                    (gettext "Are you sure you want to permanently delete %s and all its versions?" appState.locale)
                    [ strong [] [ text version ] ]
                )
            ]

        modalConfig =
            { modalTitle = gettext "Delete package" appState.locale
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingPackage
            , actionName = gettext "Delete" appState.locale
            , actionMsg = DeletePackage
            , cancelMsg = Just <| ShowHideDeletePackage Nothing
            , dangerous = True
            , dataCy = "km-delete"
            }
    in
    Modal.confirm appState modalConfig
