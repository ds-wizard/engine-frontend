module Wizard.Pages.DocumentTemplates.Index.View exposing (view)

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
import Wizard.Api.Models.DocumentTemplate as DocumentTemplate exposing (DocumentTemplate)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateState as DocumentTemplateState
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.DocumentTemplates.Common.DocumentTemplateActionsDropdown as DocumentTemplateActionsDropdown
import Wizard.Pages.DocumentTemplates.Index.Models exposing (Model)
import Wizard.Pages.DocumentTemplates.Index.Msgs exposing (Msg(..))
import Wizard.Pages.DocumentTemplates.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Feature
import Wizard.Utils.HtmlAttributesUtils exposing (listClass)


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "" ]
        [ Page.header (gettext "Document Templates" appState.locale) []
        , Listing.view appState (listingConfig appState) model.documentTemplates
        , deleteModal appState model
        ]


importButton : AppState -> Html Msg
importButton appState =
    if Feature.documentTemplatesImport appState then
        linkTo (Routes.documentTemplatesImport Nothing)
            [ class "btn btn-primary with-icon" ]
            [ faKmsUpload
            , text (gettext "Import" appState.locale)
            ]

    else
        Html.nothing


listingConfig : AppState -> ViewConfig DocumentTemplate Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , itemAdditionalData = always Nothing
    , dropdownItems =
        DocumentTemplateActionsDropdown.actions appState
            { exportMsg = ExportDocumentTemplate
            , updatePhaseMsg = UpdatePhase
            , deleteMsg = ShowHideDeleteDocumentTemplate << Just
            , viewActionVisible = True
            }
    , textTitle = .name
    , emptyText = gettext "Click \"Import\" button to import a new document template." appState.locale
    , updated =
        Just
            { getTime = .createdAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Just (gettext "Search..." appState.locale)
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "createdAt", gettext "Created" appState.locale )
        ]
    , filters = []
    , toRoute = \_ -> Routes.DocumentTemplatesRoute << IndexRoute
    , toolbarExtra = Just (importButton appState)
    }


listingTitle : AppState -> DocumentTemplate -> Html Msg
listingTitle appState documentTemplate =
    span []
        [ linkTo (Routes.documentTemplatesDetail documentTemplate.id) [] [ text documentTemplate.name ]
        , Badge.light
            (tooltip (gettext "Latest version" appState.locale))
            [ text <| Version.toString documentTemplate.version ]
        , listingTitleNonEditableBadge appState documentTemplate
        , listingTitleUnsupportedBadge appState documentTemplate
        , listingTitleDeprecatedBadge appState documentTemplate
        , listingTitleOutdatedBadge appState documentTemplate
        ]


listingTitleOutdatedBadge : AppState -> DocumentTemplate -> Html Msg
listingTitleOutdatedBadge appState documentTemplate =
    if DocumentTemplate.isOutdated documentTemplate then
        let
            documentTemplateId =
                Maybe.map
                    ((++) (documentTemplate.organizationId ++ ":" ++ documentTemplate.templateId ++ ":") << Version.toString)
                    documentTemplate.remoteLatestVersion
        in
        linkTo (Routes.documentTemplatesImport documentTemplateId)
            [ class Badge.warningClass ]
            [ text (gettext "update available" appState.locale) ]

    else
        Html.nothing


listingTitleUnsupportedBadge : AppState -> DocumentTemplate -> Html Msg
listingTitleUnsupportedBadge appState documentTemplate =
    if documentTemplate.state == DocumentTemplateState.UnsupportedMetamodelVersion then
        Badge.danger [] [ text (gettext "unsupported metamodel" appState.locale) ]

    else
        Html.nothing


listingTitleDeprecatedBadge : AppState -> DocumentTemplate -> Html Msg
listingTitleDeprecatedBadge appState documentTemplate =
    if documentTemplate.phase == DocumentTemplatePhase.Deprecated then
        Badge.danger [] [ text (gettext "deprecated" appState.locale) ]

    else
        Html.nothing


listingTitleNonEditableBadge : AppState -> DocumentTemplate -> Html Msg
listingTitleNonEditableBadge appState documentTemplate =
    if documentTemplate.nonEditable then
        Badge.dark [] [ text (gettext "non-editable" appState.locale) ]

    else
        Html.nothing


listingDescription : AppState -> DocumentTemplate -> Html Msg
listingDescription appState documentTemplate =
    let
        organizationFragment =
            case documentTemplate.organization of
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
        [ code [ class "fragment" ] [ text documentTemplate.id ]
        , organizationFragment
        , span [ class "fragment" ] [ text documentTemplate.description ]
        ]


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    let
        ( visible, documentTemplateName ) =
            case model.documentTemplateToBeDeleted of
                Just documentTemplate ->
                    ( True, documentTemplate.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                (String.formatHtml
                    (gettext "Are you sure you want to permanently delete %s and all its versions?" appState.locale)
                    [ strong [] [ text documentTemplateName ] ]
                )
            ]

        modalConfig =
            Modal.confirmConfig (gettext "Delete document template" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigActionResult model.deletingDocumentTemplate
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) DeleteDocumentTemplate
                |> Modal.confirmConfigCancelMsg (ShowHideDeleteDocumentTemplate Nothing)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "templates-delete"
    in
    Modal.confirm appState modalConfig
