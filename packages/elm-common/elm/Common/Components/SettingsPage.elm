module Common.Components.SettingsPage exposing
    ( Model
    , Msg
    , UpdateConfig
    , ViewConfig
    , discard
    , formMsg
    , getConfigCompleted
    , init
    , initialModel
    , isDirty
    , isUnsavedBarVisible
    , layout
    , layoutWithTestIds
    , moveItemDown
    , moveItemUp
    , setOverrideAllowed
    , subscriptions
    , update
    , view
    )

{-| A settings page: a menu of sections beside the one being edited, and the
state machine of a section that edits one configuration object.

A section is one
`Common.Api.Settings.Section`: it is read, edited and written on its own.


# Layout

`layout` is `Ui.Page.SplitNav`. Its `title` names the menu ("Settings"), not the
page: the body begins with `Ui.Text.pageTitle` for the section, which is the one
`h1` and where focus lands after choosing a section. `view` renders that heading
for you; a section drawn without `view` has to render its own. On a narrow page
the menu becomes a disclosure named "Settings: Organization" — the template's,
and nothing here has to know.


# The configuration form

The section is loaded, the form is built from its value, and Save sends the
section back with the form merged in, then reloads the application so everything
reading the configuration sees the new one.


# Defaults and values of one's own

A section says whether it may be overridden and whether what was read is an
override (`Common.Api.Models.Settings`), and the page is drawn from that alone:

  - where the defaults are kept, a switch says whether they may be overridden;
  - where they are used, a note says so, and saving makes the value one's own;
  - where they have been replaced, a note says so and offers to go back to them;
  - where they may not be replaced, the form is shown and cannot be changed.

A section that says neither is a plain form.

The presentation is what changed, and each change is an accessibility decision:

  - **Save is never disabled on validity.** A disabled Save says nothing about
    what is wrong and strands a keyboard user on a dead control.
  - **The unsaved bar is the save affordance.** Editing something that exists
    has no natural moment to press Save, so `Ui.Form.unsavedBar` appears as soon
    as the form differs from what was loaded, in the page's footer slot, and
    announces itself politely. It stays while saving, with Save showing progress
    and Discard unavailable: abandoning a request in flight leaves the page and
    the server disagreeing about what happened.
  - **Discard puts focus on the page heading.** The button that had focus goes
    away with the bar, and focus on nothing is focus at the top of the document.
  - **A failed save is summarized and focused.** `Common.Components.Form.errorSummary`
    above the form, announced assertively, with focus moved to it — the person
    pressed Save at the bottom of the screen, and the reason is at the top.
  - **An invalid save focuses the first invalid field**, as every submit does
    (`Common.Components.Form`), whose error is already showing beside it; there
    is no second summary repeating it.
  - **The fields are in a real `form`**, so Enter in a text field saves, as the
    bar's Save does.
  - **Cmd/Ctrl+Enter saves**, through `subscriptions`, only while there is
    something to save and nothing is being saved already.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Context exposing (Context)
import Common.Api.Models.Settings as Settings exposing (Settings)
import Common.Api.Settings as SettingsApi exposing (Section)
import Common.Components.ApiErrorAlert as ApiErrorAlert
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.Form as CommonForm
import Common.Data.Context exposing (WithLocale, WithNavigator)
import Common.Ports.Window as Window
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Form.Validate exposing (Validation)
import Gettext exposing (gettext)
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import Task
import Ui.Alert as Alert
import Ui.AppShell as AppShell
import Ui.AsyncBlock
import Ui.Button as Button exposing (Button)
import Ui.Field as UiField
import Ui.Form as UiForm
import Ui.Layout as Layout
import Ui.Page.SplitNav as SplitNav
import Ui.Separator as Separator
import Ui.Text as Text



-- LAYOUT


{-| The settings shape: the menu of sections, the body, and a footer slot below
the scrolling region for `Ui.Form.unsavedBar`.

Groups with no items are dropped, so a group whose every section is hidden by
permissions does not leave a heading over nothing.

-}
layout :
    { title : String
    , groups : List ( Maybe String, List { label : String, url : String, active : Bool } )
    , body : List (Html msg)
    , footer : Maybe (Html msg)
    }
    -> Html msg
layout config =
    layoutWithTestIds
        { title = config.title
        , groups =
            List.map
                (Tuple.mapSecond (List.map (\entry -> { label = entry.label, url = entry.url, active = entry.active, testId = Nothing })))
                config.groups
        , body = config.body
        , footer = config.footer
        }


{-| `layout`, where a section in the menu can carry a `data-cy` for the
end-to-end tests (`Ui.Page.SplitNav.toHtmlWithTestIds`).
-}
layoutWithTestIds :
    { title : String
    , groups : List ( Maybe String, List SplitNav.ItemWithTestId )
    , body : List (Html msg)
    , footer : Maybe (Html msg)
    }
    -> Html msg
layoutWithTestIds config =
    SplitNav.toHtmlWithTestIds
        { title = config.title
        , groups = List.filterNot (List.isEmpty << Tuple.second) config.groups
        , body = config.body
        , footer = config.footer
        }



-- MODEL


{-| `formRemoved` is kept because removing an item from a form list does not
mark any field as changed, so without it removing the last custom menu link would
not count as a change.

`overrideAllowed` is the switch as it stands on the page; what was loaded is in
`config`.

-}
type alias Model value form =
    { config : ActionResult (Settings value)
    , overrideAllowed : Maybe Bool
    , savingConfig : ActionResult ()
    , resetting : ActionResult ()
    , resetOpen : Bool
    , form : Form FormError form
    , formRemoved : Bool
    }


{-| The model before anything is loaded. The form is an empty one built with the
section's validation, `Form.initial [] validation`; it is replaced as soon as the
section arrives.
-}
initialModel : Form FormError form -> Model value form
initialModel form =
    { config = Loading
    , overrideAllowed = Nothing
    , savingConfig = Unset
    , resetting = Unset
    , resetOpen = False
    , form = form
    , formRemoved = False
    }


{-| A fresh model and the request that loads the section.
-}
init : Context ctx -> Section value -> Form FormError form -> ( Model value form, Cmd (Msg value) )
init appState section form =
    ( initialModel form
    , SettingsApi.get appState section GetConfigCompleted
    )


{-| Whether the page differs from the section it was built from. What a
navigation guard asks before leaving the page.
-}
isDirty : Model value form -> Bool
isDirty model =
    model.formRemoved
        || FormUtils.containsChanges model.form
        || ActionResult.unwrap False (\config -> config.overrideAllowed /= model.overrideAllowed) model.config


{-| The bar is up while there are changes and while they are being saved, so it
does not vanish from under the Save button that was just pressed. After a
successful save it goes, showing the check on its Save, and then the application
reloads.
-}
isUnsavedBarVisible : Model value form -> Bool
isUnsavedBarVisible model =
    (isDirty model && not (ActionResult.isSuccess model.savingConfig)) || ActionResult.isLoading model.savingConfig


isLocked : Model value form -> Bool
isLocked model =
    ActionResult.unwrap False (\config -> Settings.state config == Settings.Locked) model.config



-- MSG


type Msg value
    = GetConfigCompleted (Result ApiError (Settings value))
    | PutConfigCompleted (Result ApiError ())
    | FormMsg Form.Msg
    | FormMoveItemUp String (List String) Int
    | FormMoveItemDown String (List String) Int
    | Discard
    | FocusDone
    | SetOverrideAllowed Bool
    | SetResetOpen Bool
    | Reset
    | ResetCompleted (Result ApiError ())


{-| Put the form back to the loaded section. The unsaved bar sends it; it is
exposed for a section that offers the same thing somewhere else.
-}
discard : Msg value
discard =
    Discard


{-| The load's response.
-}
getConfigCompleted : Result ApiError (Settings value) -> Msg value
getConfigCompleted =
    GetConfigCompleted


{-| Whether the defaults may be overridden: the switch under the form.
-}
setOverrideAllowed : Bool -> Msg value
setOverrideAllowed =
    SetOverrideAllowed


{-| For a form view: `Html.map SettingsPage.formMsg (fields form)`.
-}
formMsg : Form.Msg -> Msg value
formMsg =
    FormMsg


{-| Move an item of a form list one place up: the list's name, the names of the
fields each item has, and the item's index. `enbala/elm-form` has no move, so the
values are swapped field by field (`Common.Utils.Form.moveListItem`).
-}
moveItemUp : String -> List String -> Int -> Msg value
moveItemUp =
    FormMoveItemUp


moveItemDown : String -> List String -> Int -> Msg value
moveItemDown =
    FormMoveItemDown



-- UPDATE


{-| What a section provides.

  - `section` is where it is read from and written to.
  - `initForm` builds the form from the loaded value.
  - `formToValue` merges the form's output into the loaded value.

-}
type alias UpdateConfig value form msg =
    { wrapMsg : Msg value -> msg
    , logoutMsg : msg
    , section : Section value
    , initForm : value -> Form FormError form
    , formToValue : form -> value -> value
    , formValidation : Validation FormError form
    }


update : Context ctx -> UpdateConfig value form msg -> Msg value -> Model value form -> ( Model value form, Cmd msg )
update appState cfg msg model =
    case msg of
        GetConfigCompleted result ->
            case result of
                Ok config ->
                    ( { model
                        | form = cfg.initForm config.value
                        , config = Success config
                        , overrideAllowed = config.overrideAllowed
                        , formRemoved = False
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | config = ApiError.toActionResult appState (gettext "Unable to load settings." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        PutConfigCompleted result ->
            case result of
                Ok _ ->
                    ( { model | savingConfig = Success () }
                    , CommonForm.reloadWhenSaved
                    )

                Err error ->
                    ( { model | savingConfig = ApiError.toActionResult appState (gettext "Settings could not be saved." appState.locale) error }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , CommonForm.focusErrorSummary
                        ]
                    )

        FormMsg formMsgValue ->
            handleForm appState cfg formMsgValue model

        FormMoveItemUp listName fields index ->
            ( moveFormListItem cfg listName fields index (index - 1) model, Cmd.none )

        FormMoveItemDown listName fields index ->
            ( moveFormListItem cfg listName fields index (index + 1) model, Cmd.none )

        Discard ->
            case model.config of
                Success config ->
                    ( { model
                        | form = cfg.initForm config.value
                        , overrideAllowed = config.overrideAllowed
                        , formRemoved = False
                        , savingConfig = Unset
                      }
                    , Cmd.map cfg.wrapMsg (focusHeading FocusDone)
                    )

                _ ->
                    ( model, Cmd.none )

        FocusDone ->
            ( model, Cmd.none )

        SetOverrideAllowed allowed ->
            ( { model | overrideAllowed = Maybe.map (always allowed) model.overrideAllowed }, Cmd.none )

        SetResetOpen open ->
            ( { model | resetOpen = open, resetting = Unset }, Cmd.none )

        Reset ->
            ( { model | resetting = Loading }
            , Cmd.map cfg.wrapMsg (SettingsApi.delete appState cfg.section ResetCompleted)
            )

        ResetCompleted result ->
            case result of
                Ok _ ->
                    ( { model | resetting = Success () }
                    , Window.refresh ()
                    )

                Err error ->
                    ( { model | resetting = ApiError.toActionResult appState (gettext "Settings could not be reset." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )


handleForm : Context ctx -> UpdateConfig value form msg -> Form.Msg -> Model value form -> ( Model value form, Cmd msg )
handleForm appState cfg formMsgValue model =
    case ( formMsgValue, Form.getOutput model.form, model.config ) of
        ( Form.Submit, Just form, Success config ) ->
            if isLocked model then
                ( model, Cmd.none )

            else
                ( { model | savingConfig = Loading }
                , Cmd.map cfg.wrapMsg <|
                    SettingsApi.put appState
                        cfg.section
                        { config
                            | value = cfg.formToValue form config.value
                            , overrideAllowed = model.overrideAllowed
                        }
                        PutConfigCompleted
                )

        _ ->
            let
                formRemoved =
                    case formMsgValue of
                        Form.RemoveItem _ _ ->
                            True

                        _ ->
                            model.formRemoved
            in
            ( { model
                | form = Form.update cfg.formValidation formMsgValue model.form
                , formRemoved = formRemoved
              }
            , Cmd.none
            )


moveFormListItem : UpdateConfig value form msg -> String -> List String -> Int -> Int -> Model value form -> Model value form
moveFormListItem cfg listName fields fromIndex toIndex model =
    let
        count =
            List.length (Form.getListIndexes listName model.form)
    in
    if toIndex >= 0 && toIndex < count then
        { model | form = FormUtils.moveListItem cfg.formValidation listName fields fromIndex toIndex model.form }

    else
        model


focusHeading : msg -> Cmd msg
focusHeading msg =
    Browser.Dom.focus AppShell.mainHeadingId
        |> Task.attempt (always msg)



-- SUBSCRIPTIONS


{-| Cmd/Ctrl+Enter saves, while there are changes and no save is in flight.
-}
subscriptions : WithNavigator a -> Model value form -> Sub (Msg value)
subscriptions appState model =
    if isDirty model && ActionResult.isSuccess model.config && not model.resetOpen then
        CommonForm.submitShortcut appState { result = model.savingConfig, onSubmit = FormMsg Form.Submit }

    else
        Sub.none



-- VIEW


{-| What a section provides to `view`.

  - `title` is the section's name, rendered as the page's `h1`.
  - `description` is a sentence under it, when the section needs one.
  - `guideLink` is `Common.Components.GuideLink.button`, beside the title.
  - `formView` renders the fields with `Common.Components.Form`; map its
    `Form.Msg`s with `formMsg`, or use `moveItemUp`/`moveItemDown` directly.

-}
type alias ViewConfig value form msg =
    { title : String
    , description : Maybe String
    , guideLink : Maybe (Button msg)
    , formView : Form FormError form -> Html (Msg value)
    , wrapMsg : Msg value -> msg
    }


{-| The body and the footer for `layout`.

The heading is rendered whatever the state of the request, so a section that is
loading or failed to load still has the `h1` focus is moved to on arrival.

-}
view : WithLocale a -> ViewConfig value form msg -> Model value form -> { body : List (Html msg), footer : Maybe (Html msg) }
view appState cfg model =
    let
        header =
            Layout.stack
                (Maybe.values
                    [ Just
                        (case cfg.guideLink of
                            Just guideLink ->
                                Layout.inlineBetween [ Text.pageTitle cfg.title, Button.toHtml guideLink ]

                            Nothing ->
                                Text.pageTitle cfg.title
                        )
                    , Maybe.map Text.body cfg.description
                    ]
                )
    in
    case model.config of
        Success config ->
            let
                state =
                    Settings.state config

                form =
                    CommonForm.view
                        { onSubmit = FormMsg Form.Submit
                        , content = [ cfg.formView model.form ]
                        }

                formOrReadOnly =
                    if state == Settings.Locked then
                        UiForm.readOnly [ form ]

                    else
                        form
            in
            { body =
                [ Layout.stack
                    (header
                        :: CommonForm.errorSummary model.savingConfig
                        :: List.map (Html.map cfg.wrapMsg)
                            (overrideNote appState state
                                ++ [ withOverrideSwitch appState model formOrReadOnly
                                   , resetModal appState model
                                   ]
                            )
                    )
                ]
            , footer =
                if state == Settings.Locked then
                    Nothing

                else
                    Just
                        (CommonForm.unsavedBar appState
                            { changed = isUnsavedBarVisible model
                            , result = model.savingConfig
                            , onSave = cfg.wrapMsg (FormMsg Form.Submit)
                            , onDiscard = Just (cfg.wrapMsg Discard)
                            }
                        )
            }

        Error _ ->
            { body = [ Layout.stack [ header, ApiErrorAlert.view appState model.config ] ]
            , footer = Nothing
            }

        _ ->
            { body = [ Layout.stack [ header, loadingIndicator appState ] ]
            , footer = Nothing
            }


{-| Whose value this is, above the form, for a section that can be overridden
where it is being read.
-}
overrideNote : WithLocale a -> Settings.State -> List (Html (Msg value))
overrideNote appState state =
    case state of
        Settings.Inherited ->
            [ Alert.toHtml (Alert.info (gettext "These are the organization's settings. Saving a change here replaces all of them for this workspace." appState.locale)) ]

        Settings.Overridden ->
            [ Alert.toHtmlWithContent
                [ Layout.inlineBetween
                    [ Text.body (gettext "This workspace has its own settings here, in place of the organization's." appState.locale)
                    , Button.secondary (gettext "Reset to organization default" appState.locale) (SetResetOpen True)
                        |> Button.withTestId "settings-reset"
                        |> Button.toHtml
                    ]
                ]
                (Alert.info "")
            ]

        Settings.Locked ->
            [ Alert.toHtml (Alert.info (gettext "These settings are managed by the organization and cannot be changed here." appState.locale)) ]

        _ ->
            []


{-| The form, and under it, where the defaults are kept, whether they may be
overridden. A line sets the switch apart from the form — it is about the
workspaces, not one of the settings — at the room fields have between them.
They are one column of their own, so the page's spacing above the form is the
same whether the switch is there or not.
-}
withOverrideSwitch : WithLocale a -> Model value form -> Html (Msg value) -> Html (Msg value)
withOverrideSwitch appState model form =
    case ( ActionResult.unwrap Settings.Plain Settings.state model.config, model.overrideAllowed ) of
        ( Settings.Defaults _, Just allowed ) ->
            Layout.stack
                [ form
                , Separator.line
                , UiField.toggle
                    { id = "settings-override-allowed"
                    , label = gettext "Workspaces can override these settings" appState.locale
                    , checked = allowed
                    , onCheck = SetOverrideAllowed
                    }
                    |> UiField.withHint (gettext "If enabled, each workspace can replace these settings with its own. Switching it off removes the settings workspaces have saved." appState.locale)
                    |> UiField.toHtml
                ]

        _ ->
            form


resetModal : WithLocale a -> Model value form -> Html (Msg value)
resetModal appState model =
    ConfirmModal.dangerConfirm appState
        { title = gettext "Reset to organization default" appState.locale
        , open = model.resetOpen
        , body =
            [ Text.body (gettext "The settings this workspace has saved here will be removed, and the organization's will apply again." appState.locale) ]
        , confirmLabel = gettext "Reset" appState.locale
        , onConfirm = Reset
        , onClose = SetResetOpen False
        , result = model.resetting
        , testId = Just "settings-reset-modal"
        }


loadingIndicator : WithLocale a -> Html msg
loadingIndicator appState =
    Ui.AsyncBlock.view
        { loading = True
        , error = Nothing
        , loadingLabel = gettext "Loading..." appState.locale
        , content = []
        }
