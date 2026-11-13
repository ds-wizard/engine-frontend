module Common.Pages.Projects.Detail.Settings exposing
    ( Model
    , Msg
    , footer
    , initialModel
    , isGuarded
    , setName
    , setProject
    , subscriptions
    , update
    , view
    )

{-| The settings tab of one project: what the project is called, how documents
are generated from it by default, which knowledge model it runs on, and the way
to delete it.

It is a region of the project's page rather than a page, so its title is the
section's `h2` (`Ui.Page.Content.withinPage`) and the sections under it are
`h3`s.

**The form is saved through the unsaved-changes bar**, as every settings screen
in the application is: editing what is already there has no natural moment to
press Save, so the bar appears once the form differs from what was loaded, in
the page's footer (`footer`, which the frame passes to
`Common.Components.DetailPage.withFooter` on this route only). It stays while
the save is out, Discard puts the form back and focus on the page's heading, and
a save that went through leaves the form clean, so the bar goes. Cmd/Ctrl+Enter
saves as well.

**Saving reloads the page**, as the wizard does: the name in the header, the
tabs the new settings enable and the questionnaire behind them all come from
requests this tab does not own.

**The knowledge model is information, not a field.** It is chosen when the
project is created and changed only by migrating it, so it is drawn as the row a
chosen item is in a combobox (`Common.Components.ItemSummary`, the same row the
template field shows) inside a `Ui.Card`: no control border, no chevron, no
hover and no tab stop. The way to the knowledge model's page is a small link
under the card, as the way to the selected template's is under its field; both
open in a new tab, so the settings being edited stay where they are.

**The project's tags are `Ui.TagInput` over the form's list field.** The field's
last entry is what is being typed and the ones before it are the tags, which is
how `Common.Data.Forms.ProjectSettingsForm` is built and what the wizard did; only
the entry itself is different. What is typed asks the server for tags starting
that way, half a second after the typing stops, and they are offered as the
field's suggestions.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.DocumentTemplates as DocumentTemplatesApi
import Common.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase
import Common.Api.Models.DocumentTemplate.DocumentTemplateState as DocumentTemplateState
import Common.Api.Models.DocumentTemplateSuggestion as DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Common.Api.Models.KnowledgeModel.Tag exposing (Tag)
import Common.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnowledgeModelPackagePhase
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.Project.DocumentTemplateProjectState as DocumentTemplateProjectState
import Common.Api.Models.Project.KnowledgeModelProjectState as KnowledgeModelProjectState
import Common.Api.Models.ProjectSettings exposing (ProjectSettings)
import Common.Api.Projects as ProjectsApi
import Common.Components.Form as Form
import Common.Components.Form.Field as FormField
import Common.Components.GuideLink as GuideLink
import Common.Components.ItemSummary as ItemSummary
import Common.Components.Page as Page
import Common.Components.RowBadge as RowBadge
import Common.Components.TypeHint as TypeHint
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.Forms.ProjectSettingsForm as ProjectSettingsForm exposing (ProjectSettingsForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Projects.Components.DeleteProjectModal as DeleteProjectModal
import Common.Pages.Projects.Components.ProjectDescriptor as ProjectDescriptor
import Common.Routes as Routes
import Common.Utils.DocumentTemplateUtils as DocumentTemplateUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Debouncer exposing (Debouncer)
import Form as ElmForm exposing (Form)
import Form.Field
import Gettext exposing (gettext)
import Html exposing (Html, div, p)
import Html.Attributes exposing (class)
import List.Extra as List
import Maybe.Extra as Maybe
import Set
import String.Format as String
import Task
import Task.Extra as Task
import Ui.Alert as Alert
import Ui.AppShell as AppShell
import Ui.Button as Button
import Ui.Card as Card
import Ui.Field as Field
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Page.Content as Content
import Ui.Separator as Separator
import Ui.Tag as Tag
import Ui.TagInput as TagInput
import Ui.Text as Text
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { settings : ActionResult ProjectSettings
    , form : Form FormError ProjectSettingsForm
    , documentTemplateTypeHint : TypeHint.Model DocumentTemplateSuggestion
    , savingProject : ActionResult ()
    , savedSettings : Maybe ProjectSettings
    , projectTagsSuggestions : ActionResult (Pagination String)
    , projectTagsDebouncer : Debouncer Msg
    , deleteModal : DeleteProjectModal.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { settings = Loading
    , form = ProjectSettingsForm.initEmpty appState
    , documentTemplateTypeHint = TypeHint.init
    , savingProject = Unset
    , savedSettings = Nothing
    , projectTagsSuggestions = Unset
    , projectTagsDebouncer = Debouncer.toDebouncer (Debouncer.debounce 500)
    , deleteModal = DeleteProjectModal.initialModel
    }


{-| Fill the form from the project's settings, once they have arrived. The
request is the frame's, which makes it for this route the way the wizard did.
-}
setProject : AppState -> ProjectSettings -> Model -> Model
setProject appState settings model =
    { model
        | settings = Success settings
        , form = ProjectSettingsForm.init appState settings
        , documentTemplateTypeHint = TypeHint.setSelected settings.documentTemplate model.documentTemplateTypeHint
    }


{-| The project was renamed elsewhere — from the page's title, or by somebody
else. The form follows, unless its own name has been typed over and not saved:
that is the user's, and saving it is how they would say which name they want.
Anything else typed and not saved stays as it was.
-}
setName : AppState -> String -> Model -> Model
setName appState name model =
    case model.settings of
        Success settings ->
            -- The socket says the project changed for any change to it, its
            -- sharing as well, so the same name is nothing to follow.
            if name == settings.name then
                model

            else if not (isDirty model) then
                setProject appState { settings | name = name } model

            else if (ElmForm.getFieldAsString "name" model.form).value == Just settings.name then
                -- The name field still says what was loaded: nothing was typed
                -- there, so it follows while the rest stays as it was typed.
                { model
                    | settings = Success { settings | name = name }
                    , form =
                        ElmForm.update (ProjectSettingsForm.validation appState)
                            (ElmForm.Input "name" ElmForm.Text (Form.Field.String name))
                            model.form
                }

            else
                { model | settings = Success { settings | name = name } }

        _ ->
            model



-- UPDATE


type Msg
    = FormMsg ElmForm.Msg
    | PutSettingsCompleted (Result ApiError ())
    | SetDocumentTemplate (Maybe DocumentTemplateSuggestion)
    | DocumentTemplateTypeHintMsg (TypeHint.Msg DocumentTemplateSuggestion)
    | ProjectTagInput String
    | ProjectTagAdd String
    | ProjectTagRemove String
    | ProjectTagsSearch String
    | ProjectTagsSearchCompleted (Result ApiError (Pagination String))
    | DebouncerMsg (Debouncer.Msg Msg)
    | DeleteModalMsg DeleteProjectModal.Msg
    | Discard
    | NoOp


update : UpdateConfig Msg msg -> AppState -> Uuid -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState projectUuid msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, ElmForm.getOutput model.form ) of
                ( ElmForm.Submit, Just form ) ->
                    ( { model
                        | savingProject = Loading
                        , savedSettings = ActionResult.toMaybe (ActionResult.map (withSaved model form) model.settings)
                      }
                    , ProjectsApi.putSettings appState
                        projectUuid
                        (ProjectSettingsForm.encode form)
                        (cfg.wrapMsg << PutSettingsCompleted)
                    )

                _ ->
                    ( { model | form = ElmForm.update (ProjectSettingsForm.validation appState) formMsg model.form }
                    , Cmd.none
                    )

        PutSettingsCompleted result ->
            case result of
                Ok _ ->
                    let
                        -- What was saved is now what is loaded, so the form is
                        -- clean and the unsaved bar goes, rather than offering
                        -- to save what is already saved until the page reloads.
                        savedModel =
                            case model.savedSettings of
                                Just saved ->
                                    setProject appState saved model

                                Nothing ->
                                    model
                    in
                    -- The project's name is in the header, the template decides
                    -- what the documents tab offers and the knowledge model
                    -- decides the questionnaire: all of it is loaded again.
                    ( { savedModel | savingProject = Success (), savedSettings = Nothing }, Form.reloadWhenSaved )

                Err error ->
                    ( { model | savingProject = ApiError.toActionResult appState (gettext "Questionnaire could not be saved." appState.locale) error }
                    , Cmd.batch
                        [ Form.focusErrorSummary
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        ]
                    )

        SetDocumentTemplate mbDocumentTemplate ->
            ( { model | form = setDocumentTemplate appState mbDocumentTemplate model.form }, Cmd.none )

        DocumentTemplateTypeHintMsg typeHintMsg ->
            let
                knowledgeModelPackageUuid =
                    ActionResult.unwrap Uuid.nil (.uuid << .knowledgeModelPackage) model.settings

                ( documentTemplateTypeHint, cmd ) =
                    TypeHint.update appState
                        { wrapMsg = cfg.wrapMsg << DocumentTemplateTypeHintMsg
                        , getTypeHints = DocumentTemplatesApi.getTemplatesFor appState knowledgeModelPackageUuid
                        , getError = gettext "Unable to get document templates." appState.locale
                        , setReply = cfg.wrapMsg << SetDocumentTemplate << Just
                        , clearReply = Just (cfg.wrapMsg (SetDocumentTemplate Nothing))
                        , filterResults = Nothing
                        , logoutMsg = cfg.logoutMsg
                        , id = documentTemplateFieldId
                        , toKey = Uuid.toString << .uuid
                        }
                        typeHintMsg
                        model.documentTemplateTypeHint
            in
            ( { model | documentTemplateTypeHint = documentTemplateTypeHint }, cmd )

        ProjectTagInput value ->
            ( { model | form = setProjectTagInput appState value model.form }
            , Task.dispatch (cfg.wrapMsg (DebouncerMsg (Debouncer.provideInput (ProjectTagsSearch value))))
            )

        ProjectTagAdd value ->
            if String.isEmpty value then
                ( model, Cmd.none )

            else
                ( { model
                    | form =
                        model.form
                            |> setProjectTagInput appState value
                            |> ElmForm.update (ProjectSettingsForm.validation appState) (ElmForm.Append projectTagsField)
                  }
                , Cmd.none
                )

        ProjectTagRemove value ->
            let
                form =
                    case List.find (\( _, tag ) -> tag == value) (projectTags model.form) of
                        Just ( index, _ ) ->
                            ElmForm.update (ProjectSettingsForm.validation appState)
                                (ElmForm.RemoveItem projectTagsField index)
                                model.form

                        Nothing ->
                            model.form
            in
            -- The remove button that had focus goes with its tag, so focus
            -- moves to the field the tags belong to.
            ( { model | form = form }
            , TagInput.focusInput (cfg.wrapMsg NoOp) projectTagsFieldId
            )

        ProjectTagsSearch value ->
            let
                queryString =
                    PaginationQueryString.fromQ value
                        |> PaginationQueryString.withSize (Just 10)
            in
            ( model
            , ProjectsApi.getProjectTagsSuggestions appState
                queryString
                (List.map Tuple.second (projectTags model.form))
                (cfg.wrapMsg << ProjectTagsSearchCompleted)
            )

        ProjectTagsSearchCompleted result ->
            case result of
                Ok data ->
                    ( { model | projectTagsSuggestions = Success data }, Cmd.none )

                Err error ->
                    ( { model | projectTagsSuggestions = ApiError.toActionResult appState (gettext "Unable to get project tags." appState.locale) error }
                    , Cmd.none
                    )

        DebouncerMsg debouncerMsg ->
            Debouncer.update (update cfg appState projectUuid)
                { mapMsg = cfg.wrapMsg << DebouncerMsg
                , getDebouncer = .projectTagsDebouncer
                , setDebouncer = \debouncer m -> { m | projectTagsDebouncer = debouncer }
                }
                debouncerMsg
                model

        DeleteModalMsg deleteModalMsg ->
            let
                ( deleteModal, cmd ) =
                    DeleteProjectModal.update
                        { wrapMsg = cfg.wrapMsg << DeleteModalMsg
                        , logoutMsg = cfg.logoutMsg
                        , deleteCompleteCmd = Routes.navigate appState (Routes.projectsIndex appState)
                        , closeCmd = always Cmd.none
                        }
                        appState
                        deleteModalMsg
                        model.deleteModal
            in
            ( { model | deleteModal = deleteModal }, cmd )

        Discard ->
            -- The bar's buttons go with the bar, so focus would be left on
            -- nothing: it goes to the page's heading, as it does in settings.
            ( case model.settings of
                Success settings ->
                    setProject appState settings { model | savingProject = Unset }

                _ ->
                    model
            , Browser.Dom.focus AppShell.mainHeadingId
                |> Task.attempt (always (cfg.wrapMsg NoOp))
            )

        NoOp ->
            ( model, Cmd.none )


{-| The settings as they are once the form has been saved: the ones that were
loaded, with the form's values in their place, sent the way `encode` sends them.
What the server says of the template — deprecated, outdated — is about the one
that was loaded, so it goes when the template is another one.
-}
withSaved : Model -> ProjectSettingsForm -> ProjectSettings -> ProjectSettings
withSaved model form settings =
    let
        documentTemplate =
            TypeHint.selected model.documentTemplateTypeHint

        sameTemplate =
            Maybe.map .uuid documentTemplate == Maybe.map .uuid settings.documentTemplate

        keepIfSameTemplate value =
            if sameTemplate then
                value

            else
                Nothing

        withTemplate value =
            Maybe.andThen (always value) form.documentTemplateUuid
    in
    { settings
        | name = form.name
        , description = form.description
        , projectTags =
            form.projectTags
                |> List.filterNot String.isEmpty
                |> List.sortBy String.toUpper
        , isTemplate = form.isTemplate
        , documentTemplate = documentTemplate
        , documentTemplatePhase = keepIfSameTemplate settings.documentTemplatePhase
        , documentTemplateSupportState = keepIfSameTemplate settings.documentTemplateSupportState
        , documentTemplateState = keepIfSameTemplate settings.documentTemplateState
        , formatUuid = Maybe.andThen Uuid.fromString (withTemplate form.formatUuid)
        , documentTemplateLanguage = withTemplate form.documentTemplateLanguage
        , language =
            if String.isEmpty form.language then
                Nothing

            else
                Just form.language
    }


{-| Choosing a template also sets the language it is written in and takes away
the format chosen for the previous one, which the new template may not have.
-}
setDocumentTemplate : AppState -> Maybe DocumentTemplateSuggestion -> Form FormError ProjectSettingsForm -> Form FormError ProjectSettingsForm
setDocumentTemplate appState mbDocumentTemplate form =
    let
        setField field value =
            ElmForm.update (ProjectSettingsForm.validation appState)
                (ElmForm.Input field ElmForm.Select (Form.Field.String value))
    in
    form
        |> setField "documentTemplateUuid" (Maybe.unwrap "" (Uuid.toString << .uuid) mbDocumentTemplate)
        |> setField "formatUuid" ""
        |> setField "documentTemplateLanguage" (Maybe.unwrap "" .language mbDocumentTemplate)


setProjectTagInput : AppState -> String -> Form FormError ProjectSettingsForm -> Form FormError ProjectSettingsForm
setProjectTagInput appState value form =
    ElmForm.update (ProjectSettingsForm.validation appState)
        (ElmForm.Input (projectTagInputField form) ElmForm.Text (Form.Field.String value))
        form


documentTemplateFieldId : String
documentTemplateFieldId =
    FormField.fieldId "documentTemplateUuid"


projectTagsField : String
projectTagsField =
    "projectTags"


projectTagsFieldId : String
projectTagsFieldId =
    FormField.fieldId projectTagsField


{-| The tags the project has, with the index each one is at. The list's last
entry is what is being typed and is not one of them.
-}
projectTags : Form FormError ProjectSettingsForm -> List ( Int, String )
projectTags form =
    ElmForm.getListIndexes projectTagsField form
        |> List.unconsLast
        |> Maybe.unwrap [] Tuple.second
        |> List.map (\index -> ( index, projectTagValue index form ))
        |> List.filterNot (String.isEmpty << Tuple.second)


{-| The list entry that holds what is being typed: the last one.
-}
projectTagInputField : Form FormError ProjectSettingsForm -> String
projectTagInputField form =
    ElmForm.getListIndexes projectTagsField form
        |> List.last
        |> Maybe.withDefault 0
        |> projectTagField


projectTagField : Int -> String
projectTagField index =
    projectTagsField ++ "." ++ String.fromInt index


projectTagValue : Int -> Form FormError ProjectSettingsForm -> String
projectTagValue index form =
    Maybe.withDefault "" (ElmForm.getFieldAsString (projectTagField index) form).value



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    Sub.batch
        [ Form.submitShortcut appState { result = model.savingProject, onSubmit = FormMsg ElmForm.Submit }
        , Sub.map DocumentTemplateTypeHintMsg (TypeHint.subscriptions model.documentTemplateTypeHint)
        ]



-- GUARD


{-| Leaving the tab with something typed and not saved asks first, with what the
unsaved-changes bar says.

Adding or removing a tag does not go through a field, so the form does not count
it as a change; the number of tags is compared instead. What is half-typed in
the tag field is not a change either — it is not a tag until it is added.

-}
isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    if isDirty model then
        Just (gettext "There are unsaved changes." appState.locale)

    else
        Nothing


isDirty : Model -> Bool
isDirty model =
    let
        tagsChanged =
            ActionResult.unwrap 0 (List.length << .projectTags) model.settings
                /= List.length (projectTags model.form)

        fieldsChanged =
            ElmForm.getChangedFields model.form
                |> Set.remove (projectTagInputField model.form)
                |> Set.isEmpty
                |> not
    in
    tagsChanged || fieldsChanged



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResult appState (viewSettings appState model) model.settings


viewSettings : AppState -> Model -> ProjectSettings -> Html Msg
viewSettings appState model settings =
    Content.content
        { title = gettext "Settings" appState.locale
        , body =
            [ formView appState model settings
            , Separator.line
            , knowledgeModelView appState settings
            , Separator.line
            , dangerZone appState settings
            , Html.map DeleteModalMsg (DeleteProjectModal.view appState model.deleteModal)
            ]
        }
        |> Content.form
        |> Content.withActions [ GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.projectsSettings ]
        |> Content.withinPage
        |> Content.toHtml



-- VIEW - FORM


formView : AppState -> Model -> ProjectSettings -> Html Msg
formView appState model settings =
    Form.view
        { onSubmit = FormMsg ElmForm.Submit
        , content =
            [ Form.errorSummary model.savingProject
            , Html.map FormMsg
                (FormField.input appState model.form "name" (gettext "Name" appState.locale)
                    |> FormField.withRequired True
                    |> FormField.toHtml
                )
            , Html.map FormMsg
                (FormField.input appState model.form "description" (gettext "Description" appState.locale)
                    |> FormField.toHtml
                )
            ]
                ++ projectTagsInput appState model
                ++ languageField appState model settings
                ++ isTemplateField appState model
                ++ [ Separator.line ]
                ++ documentTemplateFields appState model settings
        }


{-| The unsaved-changes bar, for the page's footer once the settings are loaded.
It is there while the form differs from what was loaded — by the same rule as
the navigation guard — and while a save is out.
-}
footer : AppState -> Model -> Maybe (Html Msg)
footer appState model =
    if ActionResult.isSuccess model.settings then
        Just
            (Form.unsavedBar appState
                { changed = isDirty model
                , result = model.savingProject
                , onSave = FormMsg ElmForm.Submit
                , onDiscard = Just Discard
                }
            )

    else
        Nothing


{-| The project's own tags, when the instance uses them. The suggestions are the
tags other projects already carry, so the same word is not spelled two ways.
-}
projectTagsInput : AppState -> Model -> List (Html Msg)
projectTagsInput appState model =
    if Feature.projectTagging appState then
        let
            label =
                gettext "Project Tags" appState.locale

            inputField =
                ElmForm.getFieldAsString (projectTagInputField model.form) model.form
        in
        [ TagInput.tagInput
            { id = projectTagsFieldId
            , label = label
            , values = List.map Tuple.second (projectTags model.form)
            , input = Maybe.withDefault "" inputField.value
            , onInput = ProjectTagInput
            , onAdd = ProjectTagAdd
            , onRemove = ProjectTagRemove
            , addLabel = gettext "Add" appState.locale
            , removeLabel = \tag -> String.format (gettext "Remove tag %s" appState.locale) [ tag ]
            }
            |> TagInput.withError (projectTagsError appState label model.form)
            |> TagInput.withSuggestions (ActionResult.unwrap [] .items model.projectTagsSuggestions)
            |> TagInput.toHtml
        ]

    else
        []


{-| What is wrong with the tags: with what is being typed, or — because a tag is
added by a press and not by passing validation — with one that was added anyway.
A tag that carries an error has no control of its own to say so beside, so the
field says it for all of them.
-}
projectTagsError : AppState -> String -> Form FormError ProjectSettingsForm -> Maybe String
projectTagsError appState label form =
    (projectTagInputField form :: List.map (projectTagField << Tuple.first) (projectTags form))
        |> List.filterMap (\field -> FormField.errorText appState label (ElmForm.getFieldAsString field form))
        |> List.head


{-| Which of the knowledge model's languages the project is answered in, when
it has more than the one it was written in.
-}
languageField : AppState -> Model -> ProjectSettings -> List (Html Msg)
languageField appState model settings =
    if List.isEmpty settings.availableLocales then
        []

    else
        let
            localeOptions =
                ( settings.knowledgeModelPackage.language, settings.knowledgeModelPackage.language )
                    :: List.map (\locale -> ( locale.code, locale.code ))
                        (List.sortBy .code settings.availableLocales)
        in
        [ Html.map FormMsg
            (FormField.select appState model.form "language" (gettext "Language" appState.locale) localeOptions
                |> FormField.toHtml
            )
        ]


{-| What a document made from this project starts with: the template, the format
it is rendered in and the language it is written in.
-}
documentTemplateFields : AppState -> Model -> ProjectSettings -> List (Html Msg)
documentTemplateFields appState model settings =
    let
        label =
            gettext "Default document template" appState.locale

        field =
            TypeHint.view appState
                { id = documentTemplateFieldId
                , label = label
                , placeholder = gettext "Select document template" appState.locale
                , required = False
                , error = FormField.errorText appState label (ElmForm.getFieldAsString "documentTemplateUuid" model.form)
                , hint = Nothing
                , clearable = True
                , viewItem =
                    \documentTemplate ->
                        { label = documentTemplate.name
                        , code = Just (DocumentTemplateUtils.getId documentTemplate)
                        , badges = RowBadge.owner appState documentTemplate.workspaceUuid
                        , description = Just documentTemplate.description
                        , icon = Nothing
                        }
                , toKey = Uuid.toString << .uuid
                , wrapMsg = DocumentTemplateTypeHintMsg
                }
                model.documentTemplateTypeHint
    in
    documentTemplateNotice appState model settings
        ++ documentTemplateWithLink appState model field
        :: documentTemplateFormatField appState model
        ++ documentTemplateLanguageField appState model


{-| The template field, with the way to the selected template's own page under
it, for a look at it without leaving the form.
-}
documentTemplateWithLink : AppState -> Model -> Html Msg -> Html Msg
documentTemplateWithLink appState model field =
    case TypeHint.selected model.documentTemplateTypeHint of
        Just documentTemplate ->
            withDetailLink appState
                (gettext "View document template" appState.locale)
                (Routes.toUrl appState (Routes.documentTemplatesDetail documentTemplate.uuid))
                field

        Nothing ->
            field


{-| What is shown, with a small link close under it opening that thing's own
page in a new tab, so the settings being edited stay where they are. The
template field and the knowledge model's card take it alike, so the link is the
same distance under both.
-}
withDetailLink : AppState -> String -> String -> Html Msg -> Html Msg
withDetailLink appState label url content =
    div [ class "c-project-settings-linked" ]
        [ content
        , p [ class "c-project-settings-linked__link" ]
            [ Link.link { label = label, url = url }
                |> Link.external
                |> Link.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                |> Link.toHtml
            ]
        ]


{-| What is wrong with the template the project is saved with — it is gone from
the metamodel, it is deprecated, or there is a newer version of it. Only while
that is still the one selected: a template about to be replaced is not a problem
to report.
-}
documentTemplateNotice : AppState -> Model -> ProjectSettings -> List (Html Msg)
documentTemplateNotice appState model settings =
    let
        selectedUuid =
            Maybe.map .uuid (TypeHint.selected model.documentTemplateTypeHint)

        savedUuid =
            Maybe.map .uuid settings.documentTemplate
    in
    if selectedUuid /= savedUuid then
        []

    else
        case ( settings.documentTemplateSupportState, settings.documentTemplatePhase, settings.documentTemplateState ) of
            ( Just DocumentTemplateState.UnsupportedMetamodelVersion, _, _ ) ->
                [ Alert.toHtml (Alert.danger (gettext "The used version of the document template is no longer supported. Select a newer version or another supported template." appState.locale)) ]

            ( _, Just DocumentTemplatePhase.Deprecated, _ ) ->
                [ Alert.toHtml (Alert.warning (gettext "This document template is now deprecated." appState.locale)) ]

            ( _, _, Just DocumentTemplateProjectState.Outdated ) ->
                [ Alert.toHtml (Alert.warning (gettext "A newer version of this document template is available." appState.locale)) ]

            _ ->
                []


documentTemplateFormatField : AppState -> Model -> List (Html Msg)
documentTemplateFormatField appState model =
    case TypeHint.selected model.documentTemplateTypeHint of
        Just documentTemplate ->
            let
                formatChoice format =
                    Field.choice { value = Uuid.toString format.uuid, label = format.name }
                        |> Field.withChoiceIcon (Icon.fromClass format.icon)
            in
            [ Html.map FormMsg
                (FormField.radioGroup appState
                    model.form
                    "formatUuid"
                    (gettext "Default document format" appState.locale)
                    (List.map formatChoice documentTemplate.formats)
                    |> FormField.toHtml
                )
            ]

        Nothing ->
            []


documentTemplateLanguageField : AppState -> Model -> List (Html Msg)
documentTemplateLanguageField appState model =
    case TypeHint.selected model.documentTemplateTypeHint of
        Just documentTemplate ->
            if List.isEmpty documentTemplate.locales then
                []

            else
                [ Html.map FormMsg
                    (FormField.select appState
                        model.form
                        "documentTemplateLanguage"
                        (gettext "Default document language" appState.locale)
                        (DocumentTemplateSuggestion.languageOptions documentTemplate)
                        |> FormField.toHtml
                    )
                ]

        Nothing ->
            []


{-| Whether other people may start their projects from this one, for whoever is
allowed to make project templates.
-}
isTemplateField : AppState -> Model -> List (Html Msg)
isTemplateField appState model =
    if Feature.projectTemplatesCreate appState then
        [ Separator.line
        , Html.map FormMsg
            (FormField.toggle appState model.form "isTemplate" (gettext "Project Template" appState.locale)
                |> FormField.withHintMarkdown (gettext "Other users can use project templates so they don't have to start their new projects from scratch. Project templates follow the same sharing policy as projects, so make sure to share them with users who should use them." appState.locale)
                |> FormField.toHtml
            )
        ]

    else
        []



-- VIEW - KNOWLEDGE MODEL


{-| Which knowledge model the project asks its questions from, which of its
questions are used, and the way to move the project to another one.

The knowledge model is the row a chosen item is in a combobox, in a card: it
reads as what the project runs on, not as a field waiting for an answer. Its
page is a link under the card. The card, the question tags and the migration
are set apart as fields are, so the migration does not read as part of the tags.

-}
knowledgeModelView : AppState -> ProjectSettings -> Html Msg
knowledgeModelView appState settings =
    let
        package =
            settings.knowledgeModelPackage
    in
    Layout.stack
        (Text.subtitle (gettext "Knowledge Model" appState.locale)
            :: knowledgeModelNotice appState settings
            ++ [ Layout.stackLoose
                    (withDetailLink appState
                        (gettext "View knowledge model" appState.locale)
                        (Routes.toUrl appState (Routes.knowledgeModelsDetail package.uuid))
                        (Card.view
                            [ ItemSummary.view
                                { label = package.name
                                , code = Just (KnowledgeModelUtils.getPackageId package)
                                , badges = RowBadge.owner appState package.workspaceUuid
                                , description =
                                    if String.isEmpty package.description then
                                        Nothing

                                    else
                                        Just package.description
                                , icon = Nothing
                                }
                            ]
                        )
                        :: questionTagsField appState settings
                        ++ [ Layout.stack
                                [ Button.toHtml
                                    (Button.link (gettext "Migrate project" appState.locale)
                                        (Routes.toUrl appState (Routes.projectsCreateMigration settings.uuid))
                                    )
                                , Text.muted (gettext "Project migration lets you move your project to a newer or different version of the knowledge model, and update which questions are included by changing question tags." appState.locale)
                                ]
                           ]
                    )
               ]
        )


knowledgeModelNotice : AppState -> ProjectSettings -> List (Html Msg)
knowledgeModelNotice appState settings =
    if settings.knowledgeModelPackage.phase == KnowledgeModelPackagePhase.Deprecated then
        [ Alert.toHtml (Alert.warning (gettext "This knowledge model is now deprecated." appState.locale)) ]

    else if settings.knowledgeModelState == KnowledgeModelProjectState.Outdated then
        [ Alert.toHtml (Alert.warning (gettext "A newer version of this knowledge model is available." appState.locale)) ]

    else
        []


{-| The tags the project's questions are filtered by, when it uses only the
questions carrying them. Which tags those are is settled when the project is
created or migrated, so they are a read-only field: here to read and not to
change. A project using every question has nothing to say here, so the field is
left out.
-}
questionTagsField : AppState -> ProjectSettings -> List (Html Msg)
questionTagsField appState settings =
    if List.isEmpty settings.selectedQuestionTagUuids then
        []

    else
        [ Field.readOnlyHtml
            { id = "project-settings-question-tags"
            , label = gettext "Filtered by question tags" appState.locale
            , value = [ Tag.list (List.map questionTag settings.knowledgeModelTags) ]
            }
            |> Field.toHtml
        ]


questionTag : Tag -> Tag.Tag msg
questionTag tag =
    Tag.withColor tag.color (Tag.tag tag.name)



-- VIEW - DANGER ZONE


dangerZone : AppState -> ProjectSettings -> Html Msg
dangerZone appState settings =
    Card.viewWithTitleLevel 3
        (gettext "Danger Zone" appState.locale)
        [ Layout.stack
            [ Text.body (gettext "Deleted projects cannot be recovered." appState.locale)
            , Button.toHtml
                (Button.dangerSecondary (gettext "Delete this project" appState.locale)
                    (DeleteModalMsg (DeleteProjectModal.open (ProjectDescriptor.fromProjectSettings settings)))
                )
            ]
        ]
