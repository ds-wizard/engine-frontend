module Wizard.DocumentTemplateEditors.Index.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, code, div, p, span, strong, text)
import Html.Attributes exposing (class)
import Shared.Components.Badge as Badge
import Shared.Data.DocumentTemplateDraft exposing (DocumentTemplateDraft)
import Shared.Html exposing (faSet)
import Shared.Utils exposing (listInsertIf)
import String.Format as String
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ListingActionType(..), ListingDropdownItem, ViewConfig)
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, listClass, tooltip)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.DocumentTemplateEditors.Index.Models exposing (Model)
import Wizard.DocumentTemplateEditors.Index.Msgs exposing (Msg(..))
import Wizard.DocumentTemplateEditors.Routes exposing (Route(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "" ]
        [ Page.header (gettext "Document Template Editors" appState.locale) []
        , FormResult.successOnlyView appState model.deletingDocumentTemplateDraft
        , Listing.view appState (listingConfig appState) model.documentTemplateDrafts
        , deleteModal appState model
        ]


listingConfig : AppState -> ViewConfig DocumentTemplateDraft Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription
    , itemAdditionalData = always Nothing
    , dropdownItems = listingActions appState
    , textTitle = .name
    , emptyText = gettext "Click \"Import\" button to import a new document template." appState.locale
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Just (gettext "Search..." appState.locale)
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "createdAt", gettext "Created" appState.locale )
        , ( "updatedAt", gettext "Updated" appState.locale )
        ]
    , filters = []
    , toRoute = \_ -> Routes.DocumentTemplateEditorsRoute << IndexRoute
    , toolbarExtra = Just (createButton appState)
    }


listingTitle : AppState -> DocumentTemplateDraft -> Html Msg
listingTitle appState template =
    span []
        [ linkTo appState (Routes.documentTemplateEditorDetail template.id) [] [ text template.name ]
        , Badge.light
            (tooltip (gettext "Latest version" appState.locale))
            [ text <| Version.toString template.version ]
        ]


listingDescription : DocumentTemplateDraft -> Html Msg
listingDescription template =
    span []
        [ code [ class "fragment" ] [ text template.id ]
        , span [ class "fragment" ] [ text template.description ]
        ]


listingActions : AppState -> DocumentTemplateDraft -> List (ListingDropdownItem Msg)
listingActions appState template =
    let
        viewAction =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.edit" appState
                , label = gettext "Open editor" appState.locale
                , msg = ListingActionLink (Routes.documentTemplateEditorDetail template.id)
                , dataCy = "view"
                }

        viewActionVisible =
            Feature.templatesView appState

        deleteAction =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = gettext "Delete" appState.locale
                , msg = ListingActionMsg <| ShowHideDeleteDocumentTemplateDraft <| Just template
                , dataCy = "delete"
                }

        deleteActionVisible =
            Feature.templatesDelete appState
    in
    []
        |> listInsertIf viewAction viewActionVisible
        |> listInsertIf Listing.dropdownSeparator deleteActionVisible
        |> listInsertIf deleteAction deleteActionVisible


createButton : AppState -> Html Msg
createButton appState =
    linkTo appState
        (Routes.documentTemplateEditorCreate Nothing Nothing)
        [ class "btn btn-primary"
        , dataCy "document-template-editors_create-button"
        ]
        [ text (gettext "Create" appState.locale) ]


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    let
        ( visible, templateName ) =
            case model.documentTemplateDraftToBeDeleted of
                Just template ->
                    ( True, template.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                (String.formatHtml
                    (gettext "Are you sure you want to permanently delete %s?" appState.locale)
                    [ strong [] [ text templateName ] ]
                )
            ]

        modalConfig =
            { modalTitle = gettext "Delete document template editor" appState.locale
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingDocumentTemplateDraft
            , actionName = gettext "Delete" appState.locale
            , actionMsg = DeleteDocumentTemplateDraft
            , cancelMsg = Just <| ShowHideDeleteDocumentTemplateDraft Nothing
            , dangerous = True
            , dataCy = "templates-delete"
            }
    in
    Modal.confirm appState modalConfig
