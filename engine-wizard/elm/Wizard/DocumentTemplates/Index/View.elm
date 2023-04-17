module Wizard.DocumentTemplates.Index.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, code, div, img, p, span, strong, text)
import Html.Attributes exposing (class, src, title)
import Shared.Components.Badge as Badge
import Shared.Data.DocumentTemplate exposing (DocumentTemplate)
import Shared.Data.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase
import Shared.Data.DocumentTemplate.DocumentTemplateState as DocumentTemplateState
import Shared.Html exposing (emptyNode, faSet)
import String.Format as String
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass, tooltip)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.DocumentTemplates.Common.DocumentTemplateActionsDropdown as DocumentTemplateActionsDropdown
import Wizard.DocumentTemplates.Index.Models exposing (Model)
import Wizard.DocumentTemplates.Index.Msgs exposing (Msg(..))
import Wizard.DocumentTemplates.Routes exposing (Route(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "" ]
        [ Page.header (gettext "Document Templates" appState.locale) []
        , FormResult.view appState model.deletingDocumentTemplate
        , Listing.view appState (listingConfig appState) model.documentTemplates
        , deleteModal appState model
        ]


importButton : AppState -> Html Msg
importButton appState =
    if Feature.documentTemplatesImport appState then
        linkTo appState
            (Routes.documentTemplatesImport Nothing)
            [ class "btn btn-primary with-icon" ]
            [ faSet "kms.upload" appState
            , text (gettext "Import" appState.locale)
            ]

    else
        emptyNode


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
        [ linkTo appState (Routes.documentTemplatesDetail documentTemplate.id) [] [ text documentTemplate.name ]
        , Badge.light
            (tooltip (gettext "Latest version" appState.locale))
            [ text <| Version.toString documentTemplate.version ]
        , listingTitleOutdatedBadge appState documentTemplate
        , listingTitleUnsupportedBadge appState documentTemplate
        , listingTitleDeprecatedBadge appState documentTemplate
        ]


listingTitleOutdatedBadge : AppState -> DocumentTemplate -> Html Msg
listingTitleOutdatedBadge appState documentTemplate =
    if documentTemplate.state == DocumentTemplateState.Outdated then
        let
            documentTemplateId =
                Maybe.map ((++) (documentTemplate.organizationId ++ ":" ++ documentTemplate.templateId ++ ":")) documentTemplate.remoteLatestVersion
        in
        linkTo appState
            (Routes.documentTemplatesImport documentTemplateId)
            [ class Badge.warningClass ]
            [ text (gettext "update available" appState.locale) ]

    else
        emptyNode


listingTitleUnsupportedBadge : AppState -> DocumentTemplate -> Html Msg
listingTitleUnsupportedBadge appState documentTemplate =
    if documentTemplate.state == DocumentTemplateState.UnsupportedMetamodelVersion then
        Badge.danger [] [ text (gettext "unsupported metamodel" appState.locale) ]

    else
        emptyNode


listingTitleDeprecatedBadge : AppState -> DocumentTemplate -> Html Msg
listingTitleDeprecatedBadge appState documentTemplate =
    if documentTemplate.phase == DocumentTemplatePhase.Deprecated then
        Badge.danger [] [ text (gettext "deprecated" appState.locale) ]

    else
        emptyNode


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
            { modalTitle = gettext "Delete document template" appState.locale
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingDocumentTemplate
            , actionName = gettext "Delete" appState.locale
            , actionMsg = DeleteDocumentTemplate
            , cancelMsg = Just <| ShowHideDeleteDocumentTemplate Nothing
            , dangerous = True
            , dataCy = "templates-delete"
            }
    in
    Modal.confirm appState modalConfig
