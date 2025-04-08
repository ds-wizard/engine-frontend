module Wizard.Users.Edit.Components.Language exposing
    ( Model
    , Msg
    , UpdateConfig
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html, div, form, input, label, text)
import Html.Attributes exposing (checked, class, classList, name, type_)
import Html.Events exposing (onClick, onSubmit)
import Html.Extra as Html
import Shared.Api.Locales as LocalesApi
import Shared.Api.Users as UsersApi
import Shared.Components.Badge as Badge
import Shared.Data.LocaleSuggestion exposing (LocaleSuggestion)
import Shared.Data.UserLocale exposing (UserLocale)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.GuideLinks as GuideLinks
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Ports as Ports


type alias Model =
    { userLocale : ActionResult UserLocale
    , locales : ActionResult (List LocaleSuggestion)
    , selectedLocale : Maybe String
    , savingLocale : ActionResult ()
    }


initialModel : Model
initialModel =
    { userLocale = ActionResult.Loading
    , locales = ActionResult.Loading
    , selectedLocale = Nothing
    , savingLocale = ActionResult.Unset
    }


type Msg
    = GetUserLocaleCompleted (Result ApiError UserLocale)
    | GetLocalesCompleted (Result ApiError (List LocaleSuggestion))
    | SelectLocale String
    | SaveLocale
    | SaveLocaleCompleted (Result ApiError ())


fetchData : AppState -> Cmd Msg
fetchData appState =
    Cmd.batch
        [ UsersApi.getCurrentUserLocale appState GetUserLocaleCompleted
        , LocalesApi.getLocalesSuggestions appState GetLocalesCompleted
        ]


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetUserLocaleCompleted result ->
            case result of
                Ok userLocale ->
                    ( { model
                        | userLocale = ActionResult.Success userLocale
                        , selectedLocale = userLocale.id
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | userLocale = ApiError.toActionResult appState (gettext "Unable to get user language." appState.locale) error }
                    , getResultCmd cfg.logoutMsg result
                    )

        GetLocalesCompleted result ->
            case result of
                Ok locales ->
                    ( { model | locales = ActionResult.Success locales }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | locales = ApiError.toActionResult appState (gettext "Unable to get languages." appState.locale) error }
                    , getResultCmd cfg.logoutMsg result
                    )

        SelectLocale localeId ->
            ( { model | selectedLocale = Just localeId }, Cmd.none )

        SaveLocale ->
            let
                locale =
                    { id = model.selectedLocale }
            in
            ( { model | savingLocale = ActionResult.Loading }
            , UsersApi.putCurrentUserLocale appState locale (cfg.wrapMsg << SaveLocaleCompleted)
            )

        SaveLocaleCompleted result ->
            case result of
                Ok _ ->
                    ( { model | savingLocale = ActionResult.Success () }
                    , Ports.refresh ()
                    )

                Err error ->
                    ( { model | savingLocale = ApiError.toActionResult appState (gettext "Unable to save user language." appState.locale) error }
                    , getResultCmd cfg.logoutMsg result
                    )


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewLanguageSelection appState model) (ActionResult.combine model.userLocale model.locales)


viewLanguageSelection : AppState -> Model -> ( UserLocale, List LocaleSuggestion ) -> Html Msg
viewLanguageSelection appState model ( _, locales ) =
    div []
        [ Page.headerWithGuideLink appState (gettext "Language" appState.locale) GuideLinks.profileLanguage
        , div [ class "row" ]
            [ form [ class "col-8", onSubmit SaveLocale ]
                [ FormResult.errorOnlyView appState model.savingLocale
                , languageFormView appState model locales
                , div [ class "mt-5" ]
                    [ ActionButton.submit appState (ActionButton.SubmitConfig (gettext "Save" appState.locale) model.savingLocale) ]
                ]
            ]
        ]


languageFormView : AppState -> Model -> List LocaleSuggestion -> Html Msg
languageFormView appState model locales =
    let
        viewLocale locale =
            let
                isSelected =
                    Just locale.id == model.selectedLocale

                defaultBadge =
                    if locale.defaultLocale then
                        Badge.info [ class "ms-2" ] [ text (gettext "default" appState.locale) ]

                    else
                        Html.nothing
            in
            label [ classList [ ( "selected", Just locale.id == model.selectedLocale ) ] ]
                [ input
                    [ type_ "radio"
                    , name "language"
                    , checked isSelected
                    , onClick (SelectLocale locale.id)
                    ]
                    []
                , div []
                    [ text locale.name
                    , defaultBadge
                    , div [ class "description" ] [ text locale.description ]
                    ]
                ]
    in
    div [ class "language-selection" ] (List.map viewLocale locales)
