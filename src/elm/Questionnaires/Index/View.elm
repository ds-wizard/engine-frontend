module Questionnaires.Index.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode, linkTo)
import Common.Html.Attribute exposing (listClass)
import Common.View.FormResult as FormResult
import Common.View.Listing as Listing exposing (ListingActionConfig, ListingActionType(..), ListingConfig)
import Common.View.Modal as Modal
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (..)
import KnowledgeModels.Common.Version as Version
import KnowledgeModels.Routing
import Msgs
import Questionnaires.Common.Models exposing (Questionnaire, isEditable)
import Questionnaires.Common.View exposing (accessibilityBadge)
import Questionnaires.Index.ExportModal.View as ExportModal
import Questionnaires.Index.Models exposing (Model)
import Questionnaires.Index.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Routing
import Utils exposing (listInsertIf)


view : (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
view wrapMsg appState model =
    Page.actionResultView (viewQuestionnaires wrapMsg appState model) model.questionnaires


viewQuestionnaires : (Msg -> Msgs.Msg) -> AppState -> Model -> List Questionnaire -> Html Msgs.Msg
viewQuestionnaires wrapMsg appState model questionnaires =
    div [ listClass "Questionnaires__Index" ]
        [ Page.header "Questionnaires" indexActions
        , FormResult.successOnlyView model.deletingQuestionnaire
        , Listing.view (listingConfig wrapMsg appState) <| List.sortBy .name questionnaires
        , ExportModal.view (wrapMsg << ExportModalMsg) appState model.exportModalModel
        , deleteModal wrapMsg model
        ]


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo (Routing.Questionnaires <| Create Nothing) [ class "btn btn-primary" ] [ text "Create" ] ]


listingConfig : (Msg -> Msgs.Msg) -> AppState -> ListingConfig Questionnaire Msgs.Msg
listingConfig wrapMsg appState =
    { title = listingTitle appState
    , description = listingDescription
    , actions = listingActions wrapMsg appState
    , textTitle = .name
    , emptyText = "Click \"Create\" button to add a new Questionnaire."
    }


listingTitle : AppState -> Questionnaire -> Html Msgs.Msg
listingTitle appState questionnaire =
    span []
        [ linkTo (detailRoute questionnaire) [] [ text questionnaire.name ]
        , ownerIcon appState questionnaire
        , accessibilityBadge appState questionnaire.accessibility
        ]


ownerIcon : AppState -> Questionnaire -> Html Msgs.Msg
ownerIcon appState questionnaire =
    if appState.config.questionnaireAccessibilityEnabled && questionnaire.ownerUuid == Maybe.map .uuid appState.session.user then
        i [ class "fa fa-user", title "Questionnaire created by you" ] []

    else
        emptyNode


listingDescription : Questionnaire -> Html Msgs.Msg
listingDescription questionnaire =
    let
        kmRoute =
            Routing.KnowledgeModels <|
                KnowledgeModels.Routing.Detail questionnaire.package.id
    in
    linkTo kmRoute
        [ title "Knowledge Model" ]
        [ text questionnaire.package.name
        , text ", "
        , text <| Version.toString questionnaire.package.version
        , text " ("
        , code [] [ text questionnaire.package.id ]
        , text ")"
        ]


listingActions : (Msg -> Msgs.Msg) -> AppState -> Questionnaire -> List (ListingActionConfig Msgs.Msg)
listingActions wrapMsg appState questionnaire =
    let
        fillQuestionnaire =
            { extraClass = Just "font-weight-bold"
            , icon = Nothing
            , label = "Fill questionnaire"
            , msg = ListingActionLink (detailRoute questionnaire)
            }

        viewQuestionnaire =
            { extraClass = Just "font-weight-bold"
            , icon = Nothing
            , label = "View questionnaire"
            , msg = ListingActionLink (detailRoute questionnaire)
            }

        export_ =
            { extraClass = Nothing
            , icon = Just "download"
            , label = "Export"
            , msg = ListingActionMsg (wrapMsg <| ShowExportQuestionnaire questionnaire)
            }

        edit =
            { extraClass = Nothing
            , icon = Just "edit"
            , label = "Edit"
            , msg = ListingActionLink (Routing.Questionnaires <| Edit <| questionnaire.uuid)
            }

        delete =
            { extraClass = Just "text-danger"
            , icon = Just "trash-o"
            , label = "Delete"
            , msg = ListingActionMsg (wrapMsg <| ShowHideDeleteQuestionnaire <| Just questionnaire)
            }

        editable =
            isEditable appState questionnaire
    in
    []
        |> listInsertIf fillQuestionnaire editable
        |> listInsertIf viewQuestionnaire (not editable)
        |> listInsertIf export_ True
        |> listInsertIf edit editable
        |> listInsertIf delete editable


detailRoute : Questionnaire -> Routing.Route
detailRoute =
    Routing.Questionnaires << Detail << .uuid


deleteModal : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
deleteModal wrapMsg model =
    let
        ( visible, name ) =
            case model.questionnaireToBeDeleted of
                Just questionnaire ->
                    ( True, questionnaire.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                [ text "Are you sure you want to permanently delete "
                , strong [] [ text name ]
                , text "?"
                ]
            ]

        modalConfig =
            { modalTitle = "Delete questionnaire"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingQuestionnaire
            , actionName = "Delete"
            , actionMsg = wrapMsg DeleteQuestionnaire
            , cancelMsg = Just <| wrapMsg <| ShowHideDeleteQuestionnaire Nothing
            , dangerous = True
            }
    in
    Modal.confirm modalConfig
