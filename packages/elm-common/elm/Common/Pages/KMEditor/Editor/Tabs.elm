module Common.Pages.KMEditor.Editor.Tabs exposing
    ( phases
    , questionTags
    , settings
    , settingsChanged
    , settingsFooter
    )

{-| The tabs beside the knowledge model itself.

**Question Tags and Phases** are the same matrix with a different column
(`Common.Pages.KMEditor.Editor.Matrix`). Ticking a cell is an
`EditQuestionEvent` carrying one field: the tags a question has, or the phase
it becomes required in.

**Settings** is an ordinary form — name, description, id, version, language,
license, readme — saved with a `PUT`, with the parent knowledge model beside it
and a danger zone at the end.

-}

import ActionResult exposing (ActionResult)
import Common.Api.Models.Event.EditQuestionEventData exposing (EditQuestionEventData)
import Common.Api.Models.Event.EventContent exposing (EventContent(..))
import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Common.Components.Form as CommonForm
import Common.Components.Form.Field as FormField
import Common.Components.GuideLink as GuideLink
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Forms.KnowledgeModelEditorEditForm exposing (KnowledgeModelEditorEditForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Pages.KMEditor.Editor.EditorContext as EditorContext exposing (EditorContext)
import Common.Pages.KMEditor.Editor.Matrix as Matrix
import Common.Pages.KMEditor.Editor.QuestionEvent as QuestionEvent
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import LanguageCodes
import List.Extra as List
import Set
import Ui.Button as Button
import Ui.Card as Card
import Ui.Layout as Layout
import Ui.Page.Content as Content
import Ui.Separator as Separator
import Ui.Text as Text
import Version exposing (Version)


type alias EventMsg msg =
    Bool -> Maybe String -> String -> Maybe String -> EventContent -> msg



-- THE TWO MATRICES


questionTags : { a | locale : Gettext.Locale } -> (String -> String) -> EventMsg msg -> EditorContext -> Html msg
questionTags appState questionUrl eventMsg editorContext =
    Matrix.view
        { locale = appState.locale
        , id = "km-editor-question-tags"
        , testId = "km-editor_question-tags"
        , label = gettext "Question tags" appState.locale
        , columns =
            KnowledgeModel.getTags editorContext.kmEditor.knowledgeModel
                |> keptColumns editorContext
                |> List.map (\tag -> { uuid = tag.uuid, label = tag.name, color = Just tag.color })
        , noColumns = gettext "There are no question tags, create them first." appState.locale
        , isChecked = \question tagUuid -> List.member tagUuid (Question.getTagUuids question)
        , onToggle =
            \question tagUuid ->
                send eventMsg
                    editorContext
                    question
                    (QuestionEvent.questionData question QuestionEvent.tagSetters (toggle tagUuid (Question.getTagUuids question)))
        , questionUrl = questionUrl
        }
        editorContext


phases : { a | locale : Gettext.Locale } -> (String -> String) -> EventMsg msg -> EditorContext -> Html msg
phases appState questionUrl eventMsg editorContext =
    Matrix.view
        { locale = appState.locale
        , id = "km-editor-phases"
        , testId = "km-editor_phases"
        , label = gettext "Phases" appState.locale
        , columns =
            KnowledgeModel.getPhases editorContext.kmEditor.knowledgeModel
                |> keptColumns editorContext
                |> List.map (\phase -> { uuid = phase.uuid, label = phase.title, color = Nothing })
        , noColumns = gettext "There are no phases, create them first." appState.locale
        , isChecked = \question phaseUuid -> Question.getRequiredPhaseUuid question == Just phaseUuid
        , onToggle =
            \question phaseUuid ->
                -- A question is required in one phase, so ticking another moves
                -- it and ticking the one it is in clears it.
                send eventMsg
                    editorContext
                    question
                    (QuestionEvent.questionData question
                        QuestionEvent.requiredPhaseSetters
                        (if Question.getRequiredPhaseUuid question == Just phaseUuid then
                            Nothing

                         else
                            Just phaseUuid
                        )
                    )
        , questionUrl = questionUrl
        }
        editorContext


keptColumns : EditorContext -> List { a | uuid : String } -> List { a | uuid : String }
keptColumns editorContext =
    List.filterNot (\entity -> EditorContext.isDeleted entity.uuid editorContext)


{-| Ticking a cell is a structural change rather than typing, so it goes at
once.
-}
send : EventMsg msg -> EditorContext -> Question -> EditQuestionEventData -> msg
send eventMsg editorContext question data =
    let
        uuid =
            Question.getUuid question
    in
    eventMsg False Nothing (EditorContext.getParentUuid uuid editorContext) (Just uuid) (EditQuestionEvent data)


toggle : String -> List String -> List String
toggle value list =
    if List.member value list then
        List.filter ((/=) value) list

    else
        list ++ [ value ]



-- SETTINGS


settings :
    AppState
    ->
        { form : Form FormError KnowledgeModelEditorEditForm
        , saving : ActionResult String
        , deleting : ActionResult String
        , previousVersion : Maybe Version
        , formMsg : Form.Msg -> msg
        , onVersion : FormField.VersionSuggestion -> msg
        , onDelete : msg
        , onSubmit : msg
        }
    -> Html msg
settings appState cfg =
    Content.content
        { title = gettext "Settings" appState.locale
        , body =
            [ settingsForm appState cfg
            , Separator.line
            , dangerZone appState cfg
            ]
        }
        |> Content.form
        |> Content.withinPage
        |> Content.withActions [ GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.kmEditorSettings ]
        |> Content.toHtml


settingsForm :
    AppState
    ->
        { a
            | form : Form FormError KnowledgeModelEditorEditForm
            , saving : ActionResult String
            , previousVersion : Maybe Version
            , formMsg : Form.Msg -> msg
            , onVersion : FormField.VersionSuggestion -> msg
            , onSubmit : msg
        }
    -> Html msg
settingsForm appState cfg =
    let
        field name label =
            Html.map cfg.formMsg (FormField.toHtml (FormField.input appState cfg.form name label))
    in
    CommonForm.view
        { onSubmit = cfg.onSubmit
        , content =
            [ CommonForm.errorSummary cfg.saving
            , field "name" (gettext "Name" appState.locale)
            , field "description" (gettext "Description" appState.locale)
            , field "id" (gettext "Knowledge Model ID" appState.locale)

            -- The version the knowledge model goes out under, which is fixed
            -- once it is published and cannot be used again — so it is chosen
            -- here, before the publish dialog shows it back.
            , FormField.versionWithSuggestions appState
                cfg.form
                { majorField = "versionMajor"
                , minorField = "versionMinor"
                , patchField = "versionPatch"
                , label = gettext "Version" appState.locale
                , hint = Just (FormField.versionHint appState)
                , suggestions = FormField.versionSuggestions cfg.previousVersion
                , onSuggestion = cfg.onVersion
                , toMsg = cfg.formMsg
                }

            -- The language the knowledge model is written in, as a code. The
            -- names are what anybody knows it by, so the list says both.
            , Html.map cfg.formMsg
                (FormField.toHtml
                    (FormField.select appState cfg.form "language" (gettext "Language" appState.locale) LanguageCodes.options)
                )
            , field "license" (gettext "License" appState.locale)
            , Html.map cfg.formMsg (FormField.toHtml (FormField.markdown appState cfg.form "readme" (gettext "Readme" appState.locale)))
            ]
        }


{-| Deleting the editor is not deleting the knowledge model: what goes is
everything done since it was published. The wording says so, because "Delete
editor" beside a knowledge model reads like the other thing.
-}
dangerZone : AppState -> { a | deleting : ActionResult String, onDelete : msg } -> Html msg
dangerZone appState cfg =
    Card.viewWithTitleLevel 3
        (gettext "Danger Zone" appState.locale)
        [ Layout.stack
            [ Text.body (gettext "Deleting the editor throws away every change made in it since the knowledge model was published. The published knowledge model itself is not touched." appState.locale)
            , Button.toHtml
                (Button.dangerSecondary (gettext "Delete editor" appState.locale) cfg.onDelete
                    |> Button.withLoading (ActionResult.isLoading cfg.deleting)
                    |> Button.withTestId "km-editor_settings_delete"
                )
            ]
        ]


{-| The bar the settings are saved from, for the page's footer: it is there
while the form differs from what was loaded, as the project's settings are.
-}
settingsFooter :
    AppState
    -> { a | form : Form FormError KnowledgeModelEditorEditForm, saving : ActionResult String, onSubmit : msg, onDiscard : msg }
    -> Html msg
settingsFooter appState cfg =
    CommonForm.unsavedBar appState
        { changed = settingsChanged cfg.form
        , result = cfg.saving
        , onSave = cfg.onSubmit
        , onDiscard = Just cfg.onDiscard
        }


{-| Whether anything in the settings form has been touched, which is what says
there is something to save and what the page guards leaving on.
-}
settingsChanged : Form FormError KnowledgeModelEditorEditForm -> Bool
settingsChanged =
    not << Set.isEmpty << Form.getChangedFields
