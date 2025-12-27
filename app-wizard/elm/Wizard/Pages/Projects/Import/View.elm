module Wizard.Pages.Projects.Import.View exposing (view)

import ActionResult
import Common.Components.ActionButton as ActionButton
import Common.Components.Flash as Flash
import Common.Components.FontAwesome exposing (fa, faKmAnswer, faKmChoice)
import Common.Components.Page as Page
import Common.Utils.Markdown as Markdown
import Flip exposing (flip)
import Gettext exposing (gettext, ngettext)
import Html exposing (Html, a, div, h5, li, span, strong, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import String.Format as String
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.ProjectDetail.ProjectEvent as QuestionnaireEvent exposing (ProjectEvent)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.SetReplyData exposing (SetReplyData)
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue.IntegrationReplyType as IntegrationReplyType
import Wizard.Api.Models.ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Components.DetailNavigation as DetailNavigation
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.PluginView as PluginView
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Components.Questionnaire.Importer exposing (ImporterResult)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Projects.Import.Models exposing (Model, SidePanel(..))
import Wizard.Pages.Projects.Import.Msgs exposing (Msg(..))
import Wizard.Plugins.Plugin as Plugin
import Wizard.Plugins.PluginElement as PluginElement
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState model) <|
        ActionResult.combine3 model.project model.questionnaireModel model.knowledgeModelString


viewContent : AppState -> Model -> ( ProjectQuestionnaire, Questionnaire.Model, String ) -> Html Msg
viewContent appState model ( project, questionnaireModel, kmString ) =
    Maybe.unwrap
        (viewContentBeforeImport appState model project kmString)
        (viewContentImportResult appState model project questionnaireModel)
        model.importResult


viewContentBeforeImport : AppState -> Model -> ProjectQuestionnaire -> String -> Html Msg
viewContentBeforeImport appState model project kmString =
    let
        mbPluginData =
            AppState.getPluginsByConnector appState .projectImporters
                |> Plugin.filterByKmPatterns project.knowledgeModelPackageId
                |> List.find (\( _, connector ) -> connector.url == model.importerUrl)
    in
    case mbPluginData of
        Just ( plugin, connector ) ->
            PluginView.view appState
                plugin.uuid
                connector.element
                [ PluginElement.knowledgeModelValue kmString
                , PluginElement.onImport GotImporterData
                ]

        Nothing ->
            Page.error appState (gettext "Importer not found" appState.locale)


viewContentImportResult : AppState -> Model -> ProjectQuestionnaire -> Questionnaire.Model -> ImporterResult -> Html Msg
viewContentImportResult appState model questionnaire questionnaireModel importResult =
    div [ class "Projects__Import col-full flex-column" ]
        [ viewNavigation appState model questionnaire importResult
        , div [ class "Projects__Import__Content" ]
            [ viewQuestionnairePreview appState model questionnaire questionnaireModel importResult ]
        ]


viewNavigation : AppState -> Model -> ProjectQuestionnaire -> ImporterResult -> Html Msg
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
            let
                changesElement =
                    if List.isEmpty importResult.errors then
                        span [ class "ms-3" ]

                    else
                        a [ onClick (ChangeSidePanel ChangesSidePanel), class "ms-3" ]

                changesCount =
                    List.length importResult.questionnaireEvents
            in
            changesElement
                (String.formatHtml
                    (ngettext ( "%s questionnaire change will be imported", "%s questionnaire changes will be imported" ) changesCount appState.locale)
                    [ strong []
                        [ text (String.fromInt changesCount)
                        ]
                    ]
                )

        errorsLink =
            if List.isEmpty importResult.errors then
                Html.nothing

            else
                let
                    errorCount =
                        List.length importResult.errors
                in
                a [ onClick (ChangeSidePanel ErrorsSidePanel), class "ms-3 text-danger" ]
                    (String.formatHtml
                        (ngettext ( "%s error encountered", "%s errors encountered" ) errorCount appState.locale)
                        [ strong []
                            [ text (String.fromInt errorCount)
                            ]
                        ]
                    )

        cancelButton =
            linkTo (Routes.projectsDetail model.uuid)
                [ class "btn btn-secondary btn-wide me-2" ]
                [ text (gettext "Cancel" appState.locale) ]

        importButton =
            ActionButton.button
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


viewQuestionnairePreview : AppState -> Model -> ProjectQuestionnaire -> Questionnaire.Model -> ImporterResult -> Html Msg
viewQuestionnairePreview appState model questionnaire questionnaireModel importResult =
    div [ class "Projects__Import__Content__Questionnaire" ]
        [ viewImportResults appState model questionnaire importResult
        , Questionnaire.view appState
            { features =
                { feedbackEnabled = False
                , todosEnabled = False
                , commentsEnabled = False
                , pluginsEnabled = False
                , readonly = True
                , toolbarEnabled = False
                , questionLinksEnabled = False
                }
            , renderer =
                DefaultQuestionnaireRenderer.create appState
                    (DefaultQuestionnaireRenderer.config questionnaire)
            , wrapMsg = QuestionnaireMsg
            , previewQuestionnaireEventMsg = Nothing
            , revertQuestionnaireMsg = Nothing
            , isKmEditor = False
            , projectCommon = Nothing
            }
            { events = []
            , kmEditorUuid = Nothing
            }
            questionnaireModel
        ]


viewImportResults : AppState -> Model -> ProjectQuestionnaire -> ImporterResult -> Html Msg
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
                        Flash.warning (gettext "No changes to be imported" appState.locale)

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


viewEvent : AppState -> ProjectQuestionnaire -> ProjectEvent -> Html Msg
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
            Html.nothing


viewReply : AppState -> ProjectQuestionnaire -> Question -> SetReplyData -> Html Msg
viewReply appState questionnaire question data =
    let
        eventView replies =
            div [ class "EventDetail" ]
                [ a [ class "question-link", onClick (QuestionnaireMsg (Questionnaire.ScrollToPath path)) ]
                    [ text (Question.getTitle question) ]
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
                , span [ class "fa-li-content" ] [ replyText ]
                ]
    in
    case data.value of
        ReplyValue.StringReply reply ->
            eventView [ ( fa "far fa-edit", text reply ) ]

        ReplyValue.AnswerReply answerUuid ->
            eventView
                [ ( faKmAnswer
                  , text (Maybe.unwrap "" .label (KnowledgeModel.getAnswer answerUuid questionnaire.knowledgeModel))
                  )
                ]

        ReplyValue.MultiChoiceReply choiceUuids ->
            let
                choices =
                    KnowledgeModel.getQuestionChoices (Question.getUuid question) questionnaire.knowledgeModel
                        |> List.filter (.uuid >> flip List.member choiceUuids)
                        |> List.map (\choice -> ( faKmChoice, text choice.label ))
            in
            eventView choices

        ReplyValue.ItemListReply _ ->
            eventView [ ( fa "fas fa-plus", text (gettext "Added item" appState.locale) ) ]

        ReplyValue.IntegrationReply replyType ->
            case replyType of
                IntegrationReplyType.PlainType reply ->
                    eventView [ ( fa "far fa-edit", text reply ) ]

                IntegrationReplyType.IntegrationType reply _ ->
                    eventView [ ( fa "fas fa-link", text (Markdown.toString reply) ) ]

                IntegrationReplyType.IntegrationLegacyType _ reply ->
                    eventView [ ( fa "fas fa-link", text (Markdown.toString reply) ) ]

        ReplyValue.ItemSelectReply _ ->
            eventView [ ( fa "fas fa-plus", text (gettext "Added item" appState.locale) ) ]

        ReplyValue.FileReply _ ->
            eventView [ ( fa "fas fa-plus", text (gettext "Added file" appState.locale) ) ]
