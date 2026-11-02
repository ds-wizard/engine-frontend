module Common.Pages.Users.Edit.Language exposing
    ( Model
    , Msg
    , fetchData
    , footer
    , initialModel
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Locales as LocalesApi
import Common.Api.Models.LocaleSuggestion exposing (LocaleSuggestion)
import Common.Api.Models.UserLocale exposing (UserLocale)
import Common.Api.Users as UsersApi
import Common.Components.Form as CommonForm
import Common.Data.AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Users.Components.Section as Section
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Field as Field
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { userLocale : ActionResult UserLocale
    , locales : ActionResult (List LocaleSuggestion)
    , selectedLocale : Maybe Uuid
    , savingLocale : ActionResult ()
    }


initialModel : Model
initialModel =
    { userLocale = Loading
    , locales = Loading
    , selectedLocale = Nothing
    , savingLocale = Unset
    }


fetchData : AppState -> Cmd Msg
fetchData appState =
    Cmd.batch
        [ UsersApi.getCurrentUserLocale appState GetUserLocaleCompleted
        , LocalesApi.getLocalesSuggestions appState GetLocalesCompleted
        ]



-- UPDATE


type Msg
    = GetUserLocaleCompleted (Result ApiError UserLocale)
    | GetLocalesCompleted (Result ApiError (List LocaleSuggestion))
    | SelectLocale String
    | SaveLocale
    | SaveLocaleCompleted (Result ApiError ())
    | Discard
    | NoOp


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetUserLocaleCompleted result ->
            case result of
                Ok userLocale ->
                    ( { model | userLocale = Success userLocale, selectedLocale = userLocale.uuid }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | userLocale = ApiError.toActionResult appState (gettext "Unable to get user language." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        GetLocalesCompleted result ->
            case result of
                Ok locales ->
                    ( { model | locales = Success locales }, Cmd.none )

                Err error ->
                    ( { model | locales = ApiError.toActionResult appState (gettext "Unable to get languages." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        SelectLocale value ->
            -- The empty value is no language of one's own: the
            -- organization's default, or built-in English without one.
            ( { model | selectedLocale = Uuid.fromString value }, Cmd.none )

        Discard ->
            -- The bar's buttons go with the bar, so focus would be left on
            -- nothing: it goes to the page's heading, as it does in settings.
            ( { model
                | selectedLocale = ActionResult.unwrap model.selectedLocale .uuid model.userLocale
                , savingLocale = Unset
              }
            , Cmd.map cfg.wrapMsg (Section.focusHeading NoOp)
            )

        NoOp ->
            ( model, Cmd.none )

        SaveLocale ->
            if ActionResult.isLoading model.savingLocale then
                ( model, Cmd.none )

            else
                ( { model | savingLocale = Loading }
                , UsersApi.putCurrentUserLocale appState { uuid = model.selectedLocale } (cfg.wrapMsg << SaveLocaleCompleted)
                )

        SaveLocaleCompleted result ->
            case result of
                Ok _ ->
                    ( { model | savingLocale = Success () }
                    , CommonForm.reloadWhenSaved
                    )

                Err error ->
                    ( { model | savingLocale = ApiError.toActionResult appState (gettext "Unable to save user language." appState.locale) error }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , CommonForm.focusErrorSummary
                        ]
                    )



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    if ActionResult.isSuccess (ActionResult.combine model.userLocale model.locales) then
        CommonForm.submitShortcut appState { result = model.savingLocale, onSubmit = SaveLocale }

    else
        Sub.none



-- VIEW


view : AppState -> Model -> List (Html Msg)
view appState model =
    Section.view appState
        { title = gettext "Language" appState.locale
        , guideLink = Just GuideLinks.profileLanguage
        , content =
            Section.actionResult appState
                (viewLanguageSelection appState model)
                (ActionResult.combine model.userLocale model.locales)
        }


viewLanguageSelection : AppState -> Model -> ( UserLocale, List LocaleSuggestion ) -> List (Html Msg)
viewLanguageSelection appState model ( _, locales ) =
    let
        localeChoice locale =
            Field.choice { value = Uuid.toString locale.uuid, label = locale.name }
                |> Field.withChoiceDescription locale.code
                |> (if locale.defaultLocale then
                        Field.withChoiceBadge (gettext "default" appState.locale)

                    else
                        identity
                   )

        -- No language of one's own follows the organization's default, which is
        -- built-in English while no translation is the default. An enabled
        -- `en` translation stands in for built-in English.
        noChoice =
            case List.filter .defaultLocale locales of
                defaultLocale :: _ ->
                    [ Field.choice
                        { value = ""
                        , label = String.format (gettext "Default of the organization (%s)" appState.locale) [ defaultLocale.name ]
                        }
                    ]

                [] ->
                    if List.any ((==) "en" << .code) locales then
                        [ Field.choice { value = "", label = gettext "Default of the organization" appState.locale } ]

                    else
                        [ Field.choice { value = "", label = gettext "English (built-in)" appState.locale }
                            |> Field.withChoiceDescription "en"
                        ]
    in
    [ CommonForm.view
        { onSubmit = SaveLocale
        , content =
            [ CommonForm.errorSummary model.savingLocale
            , Field.radioGroup
                { id = "user-language"
                , label = gettext "Language" appState.locale
                , value = Maybe.unwrap "" Uuid.toString model.selectedLocale
                , options = noChoice ++ List.map localeChoice locales
                , onChange = SelectLocale
                }
                |> Field.withHiddenLabel
                |> Field.toHtml
            ]
        }
    ]


{-| The unsaved-changes bar, in the settings template's footer slot: the save
the rest of the profile has, which appears once another language is chosen. After
a successful save it goes with the check on its Save, and the application reloads.
-}
footer : AppState -> Model -> Maybe (Html Msg)
footer appState model =
    if ActionResult.isSuccess model.userLocale then
        Just
            (CommonForm.unsavedBar appState
                { changed =
                    ActionResult.unwrap False (\userLocale -> userLocale.uuid /= model.selectedLocale) model.userLocale
                        && not (ActionResult.isSuccess model.savingLocale)
                , result = model.savingLocale
                , onSave = SaveLocale
                , onDiscard = Just Discard
                }
            )

    else
        Nothing
