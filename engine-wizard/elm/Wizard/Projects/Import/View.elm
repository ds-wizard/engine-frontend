module Wizard.Projects.Import.View exposing (view)

import ActionResult
import Gettext exposing (gettext)
import Html exposing (Html, a, div, em, h5, li, span, strong, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Data.KnowledgeModel as KnowledgeModel
import Shared.Data.KnowledgeModel.Question as Question exposing (Question)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetReplyData exposing (SetReplyData)
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue as ReplyValue
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue.IntegrationReplyType as IntegrationReplyType
import Shared.Data.QuestionnaireImporter exposing (QuestionnaireImporter)
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Undraw as Undraw
import Shared.Utils exposing (flip)
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.DetailNavigation as DetailNavigation
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.Components.Questionnaire.Importer exposing (ImporterResult)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.Page as Page
import Wizard.Projects.Import.Models exposing (Model, SidePanel(..))
import Wizard.Projects.Import.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState model) <|
        ActionResult.combine3 model.questionnaire model.questionnaireModel model.questionnaireImporter


viewContent : AppState -> Model -> ( QuestionnaireDetail, Questionnaire.Model, QuestionnaireImporter ) -> Html Msg
viewContent appState model ( questionnaire, questionnaireModel, _ ) =
    Maybe.unwrap
        (viewContentBeforeImport appState)
        (viewContentImportResult appState model questionnaire questionnaireModel)
        model.importResult


viewContentBeforeImport : AppState -> Html msg
viewContentBeforeImport appState =
    Page.illustratedMessage
        { image = Undraw.addInformation
        , heading = gettext "Import" appState.locale
        , lines = [ gettext "Follow the instructions in the importer window." appState.locale ]
        , cy = "import"
        }


viewContentImportResult : AppState -> Model -> QuestionnaireDetail -> Questionnaire.Model -> ImporterResult -> Html Msg
viewContentImportResult appState model questionnaire questionnaireModel importResult =
    div [ class "Projects__Import col-full flex-column" ]
        [ viewNavigation appState model questionnaire importResult
        , div [ class "Projects__Import__Content" ]
            [ viewQuestionnairePreview appState model questionnaire questionnaireModel importResult ]
        ]


viewNavigation : AppState -> Model -> QuestionnaireDetail -> ImporterResult -> Html Msg
viewNavigation appState model questionnaire importResult =
    let
        importTitle =
            div [ class "title" ]
                [ text (String.format (gettext "Importing to %s" appState.locale) [ questionnaire.name ]) ]

        importStatus =
            div []
                [ strong [] [ text (gettext "Import status:" appState.locale) ]
                , changesLink
                , errorsLink
                ]

        changesLink =
            a [ onClick (ChangeSidePanel ChangesSidePanel), class "ms-3" ]
                (String.formatHtml
                    (gettext "%s questionnaire changes will be imported" appState.locale)
                    [ strong []
                        [ text (String.fromInt (List.length importResult.questionnaireEvents))
                        ]
                    ]
                )

        errorsLink =
            if List.isEmpty importResult.errors then
                emptyNode

            else
                a [ onClick (ChangeSidePanel ErrorsSidePanel), class "ms-3 text-danger" ]
                    (String.formatHtml
                        (gettext "%s errors encountered" appState.locale)
                        [ strong []
                            [ text (String.fromInt (List.length importResult.errors))
                            ]
                        ]
                    )

        cancelButton =
            linkTo appState
                (Routes.projectsDetailQuestionnaire model.uuid)
                [ class "btn btn-secondary btn-with-loader me-2" ]
                [ text (gettext "Cancel" appState.locale) ]

        importButton =
            ActionButton.button appState
                { label = gettext "Import" appState.locale
                , result = model.importing
                , msg = PutImportData
                , dangerous = False
                }
    in
    DetailNavigation.container
        [ DetailNavigation.row
            [ DetailNavigation.section
                [ importTitle ]
            , DetailNavigation.section
                [ cancelButton
                , importButton
                ]
            ]
        , DetailNavigation.row
            [ DetailNavigation.section
                [ importStatus
                ]
            ]
        ]


viewQuestionnairePreview : AppState -> Model -> QuestionnaireDetail -> Questionnaire.Model -> ImporterResult -> Html Msg
viewQuestionnairePreview appState model questionnaire questionnaireModel importResult =
    div [ class "Projects__Import__Content__Questionnaire" ]
        [ viewImportResults appState model questionnaire importResult
        , Questionnaire.view appState
            { features =
                { feedbackEnabled = False
                , todosEnabled = False
                , commentsEnabled = False
                , readonly = True
                , toolbarEnabled = False
                }
            , renderer = DefaultQuestionnaireRenderer.create appState questionnaire.knowledgeModel
            , wrapMsg = QuestionnaireMsg
            , previewQuestionnaireEventMsg = Nothing
            , revertQuestionnaireMsg = Nothing
            }
            { events = [] }
            questionnaireModel
        ]


viewImportResults : AppState -> Model -> QuestionnaireDetail -> ImporterResult -> Html Msg
viewImportResults appState model questionnaire importResult =
    let
        heading =
            case model.sidePanel of
                ChangesSidePanel ->
                    gettext "Questionnaire Changes" appState.locale

                ErrorsSidePanel ->
                    gettext "Errors" appState.locale

        viewResult =
            case model.sidePanel of
                ChangesSidePanel ->
                    if List.isEmpty importResult.questionnaireEvents then
                        Flash.warning appState (gettext "No changes to be imported" appState.locale)

                    else
                        div [] (List.map (viewEvent appState questionnaire) importResult.questionnaireEvents)

                ErrorsSidePanel ->
                    let
                        viewError error =
                            div [ class "alert alert-danger" ] [ text error ]
                    in
                    div [] (List.map viewError importResult.errors)
    in
    div [ class "Projects__Import__Content__Questionnaire__Results" ]
        [ h5 [] [ text heading ]
        , viewResult
        ]


viewEvent : AppState -> QuestionnaireDetail -> QuestionnaireEvent -> Html Msg
viewEvent appState questionnaire event =
    let
        mbQuestion =
            Maybe.unwrap Nothing
                (flip KnowledgeModel.getQuestion questionnaire.knowledgeModel)
                (QuestionnaireEvent.getQuestionUuid event)
    in
    case ( event, mbQuestion ) of
        ( QuestionnaireEvent.SetReply data, Just question ) ->
            viewReply appState questionnaire question data

        _ ->
            emptyNode


viewReply : AppState -> QuestionnaireDetail -> Question -> SetReplyData -> Html Msg
viewReply appState questionnaire question data =
    let
        eventView replies =
            div [ class "EventDetail" ]
                [ em []
                    [ a [ onClick (QuestionnaireMsg (Questionnaire.ScrollToPath path)) ]
                        [ text (Question.getTitle question) ]
                    ]
                , ul [ class "fa-ul" ]
                    (List.map replyView replies)
                ]

        path =
            case data.value of
                ReplyValue.ItemListReply items ->
                    case List.last items of
                        Just item ->
                            data.path ++ "." ++ item

                        Nothing ->
                            data.path

                _ ->
                    data.path

        replyView ( icon, replyText ) =
            li []
                [ span [ class "fa-li" ] [ icon ]
                , span [ class "fa-li-content" ] [ text replyText ]
                ]
    in
    case data.value of
        ReplyValue.StringReply reply ->
            eventView [ ( fa "far fa-edit", reply ) ]

        ReplyValue.AnswerReply answerUuid ->
            eventView
                [ ( faSet "km.answer" appState
                  , Maybe.unwrap "" .label (KnowledgeModel.getAnswer answerUuid questionnaire.knowledgeModel)
                  )
                ]

        ReplyValue.MultiChoiceReply choiceUuids ->
            let
                choices =
                    KnowledgeModel.getQuestionChoices (Question.getUuid question) questionnaire.knowledgeModel
                        |> List.filter (.uuid >> flip List.member choiceUuids)
                        |> List.map (\choice -> ( faSet "km.choice" appState, choice.label ))
            in
            eventView choices

        ReplyValue.ItemListReply _ ->
            eventView [ ( fa "fas fa-plus", gettext "Added item" appState.locale ) ]

        ReplyValue.IntegrationReply replyType ->
            case replyType of
                IntegrationReplyType.PlainType reply ->
                    eventView [ ( fa "far fa-edit", reply ) ]

                IntegrationReplyType.IntegrationType _ reply ->
                    eventView [ ( fa "fas fa-link", reply ) ]
