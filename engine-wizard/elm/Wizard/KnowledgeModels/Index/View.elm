module Wizard.KnowledgeModels.Index.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, code, div, img, p, span, strong, text)
import Html.Attributes exposing (class, src, title)
import Html.Extra as Html
import Shared.Components.Badge as Badge
import Shared.Components.FontAwesome exposing (faKmsUpload)
import String.Format as String
import Version
import Wizard.Api.Models.Package as Package exposing (Package)
import Wizard.Api.Models.Package.PackagePhase as PackagePhase
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass, tooltip)
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.KnowledgeModels.Common.KnowledgeModelActionsDropdown as KnowledgeModelActionsDropdown
import Wizard.KnowledgeModels.Index.Models exposing (Model)
import Wizard.KnowledgeModels.Index.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Routes exposing (Route(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "" ]
        [ Page.header (gettext "Knowledge Models" appState.locale) []
        , Listing.view appState (listingConfig appState) model.packages
        , deleteModal appState model
        ]


importButton : AppState -> Html Msg
importButton appState =
    if Feature.knowledgeModelsImport appState then
        linkTo (Routes.knowledgeModelsImport Nothing)
            [ class "btn btn-primary with-icon" ]
            [ faKmsUpload
            , text (gettext "Import" appState.locale)
            ]

    else
        Html.nothing


listingConfig : AppState -> ViewConfig Package Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , itemAdditionalData = always Nothing
    , dropdownItems =
        KnowledgeModelActionsDropdown.actions appState
            { exportMsg = ExportPackage
            , updatePhaseMsg = UpdatePhase
            , deleteMsg = ShowHideDeletePackage << Just
            , viewActionVisible = True
            }
    , textTitle = .name
    , emptyText = gettext "Click \"Import\" button to import a new knowledge model." appState.locale
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
        [ linkTo (Routes.knowledgeModelsDetail package.id) [] [ text package.name ]
        , Badge.light
            (tooltip <| gettext "Latest version" appState.locale)
            [ text <| Version.toString package.version ]
        , listingTitleNonEditableBadge appState package
        , listingTitleDeprecatedBadge appState package
        , listingTitleOutdatedBadge appState package
        ]


listingTitleOutdatedBadge : AppState -> Package -> Html Msg
listingTitleOutdatedBadge appState package =
    if Package.isOutdated package then
        let
            packageId =
                Maybe.map ((++) (package.organizationId ++ ":" ++ package.kmId ++ ":") << Version.toString) package.remoteLatestVersion
        in
        linkTo (Routes.knowledgeModelsImport packageId)
            [ class Badge.warningClass ]
            [ text (gettext "update available" appState.locale) ]

    else
        Html.nothing


listingTitleDeprecatedBadge : AppState -> Package -> Html Msg
listingTitleDeprecatedBadge appState package =
    if package.phase == PackagePhase.Deprecated then
        Badge.danger [] [ text (gettext "deprecated" appState.locale) ]

    else
        Html.nothing


listingTitleNonEditableBadge : AppState -> Package -> Html Msg
listingTitleNonEditableBadge appState package =
    if package.nonEditable then
        Badge.dark [] [ text (gettext "non-editable" appState.locale) ]

    else
        Html.nothing


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
                                    Html.nothing
                    in
                    span [ class "fragment", title <| gettext "Published by" appState.locale ]
                        [ logo
                        , text organization.name
                        ]

                Nothing ->
                    Html.nothing
    in
    span []
        [ code [ class "fragment" ] [ text package.id ]
        , organizationFragment
        , span [ class "fragment" ] [ text package.description ]
        ]


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
            Modal.confirmConfig (gettext "Delete package" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigActionResult model.deletingPackage
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) DeletePackage
                |> Modal.confirmConfigCancelMsg (ShowHideDeletePackage Nothing)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "km-delete"
    in
    Modal.confirm appState modalConfig
