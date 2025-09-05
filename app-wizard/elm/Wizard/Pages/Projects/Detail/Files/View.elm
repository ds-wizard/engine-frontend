module Wizard.Pages.Projects.Detail.Files.View exposing (view)

import Common.Components.FontAwesome exposing (fa, faDelete, faDownload)
import Common.Components.Modal as Modal
import Common.Utils.ByteUnits as ByteUnits
import Common.Utils.FileIcon as FileIcon
import Gettext exposing (gettext)
import Html exposing (Html, a, div, p, span, strong, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import String.Format as String
import Wizard.Api.Models.QuestionnaireCommon exposing (QuestionnaireCommon)
import Wizard.Api.Models.QuestionnaireFile exposing (QuestionnaireFile)
import Wizard.Api.Models.User as User
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.ItemIcon as ItemIcon
import Wizard.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Components.UserIcon as UserIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.ProjectFiles.Routes exposing (Route(..))
import Wizard.Pages.Projects.Detail.Files.Models exposing (Model)
import Wizard.Pages.Projects.Detail.Files.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Utils.QuestionnaireUtils as QuestionnaireUtils


view : AppState -> QuestionnaireCommon -> Model -> Html Msg
view appState questionnaire model =
    div [ class "Projects__Detail__Content" ]
        [ div [ class "container my-4" ]
            [ Listing.view appState (listingConfig appState questionnaire) model.questionnaireFiles
            , deleteModal appState model
            ]
        ]


listingConfig : AppState -> QuestionnaireCommon -> ViewConfig QuestionnaireFile Msg
listingConfig appState questionnaire =
    { title = listingTitle
    , description = listingDescription appState
    , itemAdditionalData = always Nothing
    , dropdownItems = listingActions appState questionnaire
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


listingActions : AppState -> QuestionnaireCommon -> QuestionnaireFile -> List (ListingDropdownItem Msg)
listingActions appState questionnaire questionnaireFile =
    let
        downloadFile =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faDownload
                , label = gettext "Download" appState.locale
                , msg = ListingActionMsg (DownloadFile questionnaireFile)
                , dataCy = "download"
                }

        deleteFileVisible =
            QuestionnaireUtils.isEditor appState questionnaire

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
            , [ ( deleteFile, deleteFileVisible ) ]
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
