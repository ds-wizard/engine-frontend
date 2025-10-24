module Wizard.Pages.KnowledgeModels.Index.View exposing (view)

import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (faKmsUpload)
import Common.Components.Modal as Modal
import Common.Components.Page as Page
import Common.Components.Tooltip exposing (tooltip)
import Gettext exposing (gettext)
import Html exposing (Html, code, div, img, p, span, strong, text)
import Html.Attributes exposing (class, src, title)
import Html.Extra as Html
import String.Format as String
import Version
import Wizard.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnowledgeModelPackagePhase
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KnowledgeModels.Common.KnowledgeModelActionsDropdown as KnowledgeModelActionsDropdown
import Wizard.Pages.KnowledgeModels.Index.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Index.Msgs exposing (Msg(..))
import Wizard.Pages.KnowledgeModels.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Feature
import Wizard.Utils.HtmlAttributesUtils exposing (listClass)


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


listingConfig : AppState -> ViewConfig KnowledgeModelPackage Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , itemAdditionalData = always Nothing
    , dropdownItems =
        KnowledgeModelActionsDropdown.actions appState
            { exportMsg = ExportKnowledgeModelPackage
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


listingTitle : AppState -> KnowledgeModelPackage -> Html Msg
listingTitle appState kmPackage =
    span []
        [ linkTo (Routes.knowledgeModelsDetail kmPackage.id) [] [ text kmPackage.name ]
        , Badge.light
            (tooltip <| gettext "Latest version" appState.locale)
            [ text <| Version.toString kmPackage.version ]
        , listingTitleNonEditableBadge appState kmPackage
        , listingTitleDeprecatedBadge appState kmPackage
        , listingTitleOutdatedBadge appState kmPackage
        ]


listingTitleOutdatedBadge : AppState -> KnowledgeModelPackage -> Html Msg
listingTitleOutdatedBadge appState kmPackage =
    if KnowledgeModelPackage.isOutdated kmPackage then
        let
            kmPackageId =
                Maybe.map ((++) (kmPackage.organizationId ++ ":" ++ kmPackage.kmId ++ ":") << Version.toString) kmPackage.remoteLatestVersion
        in
        linkTo (Routes.knowledgeModelsImport kmPackageId)
            [ class Badge.warningClass ]
            [ text (gettext "update available" appState.locale) ]

    else
        Html.nothing


listingTitleDeprecatedBadge : AppState -> KnowledgeModelPackage -> Html Msg
listingTitleDeprecatedBadge appState kmPackage =
    if kmPackage.phase == KnowledgeModelPackagePhase.Deprecated then
        Badge.danger [] [ text (gettext "deprecated" appState.locale) ]

    else
        Html.nothing


listingTitleNonEditableBadge : AppState -> KnowledgeModelPackage -> Html Msg
listingTitleNonEditableBadge appState kmPackage =
    if kmPackage.nonEditable then
        Badge.dark [] [ text (gettext "non-editable" appState.locale) ]

    else
        Html.nothing


listingDescription : AppState -> KnowledgeModelPackage -> Html Msg
listingDescription appState kmPackage =
    let
        organizationFragment =
            case kmPackage.organization of
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
        [ code [ class "fragment" ] [ text kmPackage.id ]
        , organizationFragment
        , span [ class "fragment" ] [ text kmPackage.description ]
        ]


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    let
        ( visible, version ) =
            case model.kmPackageToBeDeleted of
                Just kmPackage ->
                    ( True, kmPackage.organizationId ++ ":" ++ kmPackage.kmId )

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
            Modal.confirmConfig (gettext "Delete knowledge model" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigActionResult model.deletingKmPackage
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) DeleteKnowledgeModelPackage
                |> Modal.confirmConfigCancelMsg (ShowHideDeletePackage Nothing)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "km-delete"
    in
    Modal.confirm appState modalConfig
