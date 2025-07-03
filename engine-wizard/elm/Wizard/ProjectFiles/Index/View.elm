module Wizard.ProjectFiles.Index.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, a, div, p, span, strong, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Shared.Common.ByteUnits as ByteUnits
import Shared.Components.FontAwesome exposing (fa, faDelete, faDownload)
import String.Format as String
import Wizard.Api.Models.QuestionnaireFile exposing (QuestionnaireFile)
import Wizard.Api.Models.User as User
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Common.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Common.FileIcon as FileIcon
import Wizard.Common.GuideLinks as GuideLinks
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Common.View.UserIcon as UserIcon
import Wizard.ProjectFiles.Index.Models exposing (Model)
import Wizard.ProjectFiles.Index.Msgs exposing (Msg(..))
import Wizard.ProjectFiles.Routes exposing (Route(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "ProjectFiles__Index" ]
        [ Page.headerWithGuideLink appState (gettext "Project Files" appState.locale) GuideLinks.projectsFiles
        , Listing.view appState (listingConfig appState) model.questionnaireFiles
        , deleteModal appState model
        ]


listingConfig : AppState -> ViewConfig QuestionnaireFile Msg
listingConfig appState =
    { title = listingTitle
    , description = listingDescription appState
    , itemAdditionalData = always Nothing
    , dropdownItems = listingActions appState
    , textTitle = .fileName
    , emptyText = gettext "There are no project files." appState.locale
    , updated =
        Just
            { getTime = .createdAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Just iconView
    , searchPlaceholderText = Just (gettext "Search files..." appState.locale)
    , sortOptions =
        [ ( "createdAt", gettext "Created" appState.locale )
        , ( "fileName", gettext "File name" appState.locale )
        , ( "fileSize", gettext "File size" appState.locale )
        ]
    , filters = []
    , toRoute = \_ -> Routes.ProjectFilesRoute << IndexRoute
    , toolbarExtra = Nothing
    }


iconView : QuestionnaireFile -> Html msg
iconView questionnaireFile =
    let
        icon =
            FileIcon.getFileIcon questionnaireFile.fileName questionnaireFile.contentType
    in
    ItemIcon.iconFa
        { icon = fa icon
        , extraClass = Nothing
        }


listingTitle : QuestionnaireFile -> Html Msg
listingTitle questionnaireFile =
    span []
        [ a [ onClick (DownloadFile questionnaireFile) ] [ text questionnaireFile.fileName ]
        ]


listingDescription : AppState -> QuestionnaireFile -> Html Msg
listingDescription appState questionnaireFile =
    let
        userFragment =
            span [ class "fragment" ] <|
                case questionnaireFile.createdBy of
                    Just user ->
                        [ UserIcon.viewSmall user
                        , text (User.fullName user)
                        ]

                    Nothing ->
                        [ text (gettext "Anonymous user" appState.locale) ]
    in
    span []
        [ span [ class "fragment" ] [ text (ByteUnits.toReadable questionnaireFile.fileSize) ]
        , span [ class "fragment" ]
            [ linkTo (Routes.projectsDetail questionnaireFile.questionnaire.uuid)
                []
                [ text questionnaireFile.questionnaire.name ]
            ]
        , userFragment
        ]


listingActions : AppState -> QuestionnaireFile -> List (ListingDropdownItem Msg)
listingActions appState questionnaireFile =
    let
        downloadFile =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faDownload
                , label = gettext "Download" appState.locale
                , msg = ListingActionMsg (DownloadFile questionnaireFile)
                , dataCy = "download"
                }

        deleteFile =
            ListingDropdown.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faDelete
                , label = gettext "Delete" appState.locale
                , msg = ListingActionMsg (ShowHideDeleteFile (Just questionnaireFile))
                , dataCy = "delete"
                }

        groups =
            [ [ ( downloadFile, True ) ]
            , [ ( deleteFile, True ) ]
            ]
    in
    ListingDropdown.itemsFromGroups groups


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    let
        ( visible, fileName ) =
            case model.questionnaireFileToBeDeleted of
                Just file ->
                    ( True, file.fileName )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                (String.formatHtml
                    (gettext "Are you sure you want to permanently delete %s?" appState.locale)
                    [ strong [] [ text fileName ] ]
                )
            ]

        modalConfig =
            Modal.confirmConfig (gettext "Delete file" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigActionResult model.deletingQuestionnaireFile
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) DeleteFileConfirm
                |> Modal.confirmConfigCancelMsg (ShowHideDeleteFile Nothing)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "file-delete"
    in
    Modal.confirm appState modalConfig
