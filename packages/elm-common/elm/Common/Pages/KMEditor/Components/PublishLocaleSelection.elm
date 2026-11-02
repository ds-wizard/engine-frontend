module Common.Pages.KMEditor.Components.PublishLocaleSelection exposing
    ( Model
    , Msg
    , fetchLocales
    , initialModel
    , selectedLocaleUuids
    , update
    , view
    )

{-| Which of the editor's locales are copied to the published version.

The locales are a `Ui.Field.checkboxGroup` — a `fieldset` named "Copy locales" — all checked
once they load, and nothing is shown when the editor has none.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Common.Api.Models.KnowledgeModelLocale exposing (KnowledgeModelLocale)
import Common.Data.AppState exposing (AppState)
import Gettext exposing (gettext)
import Html exposing (Html, text)
import Set exposing (Set)
import Ui.Field as Field
import Uuid exposing (Uuid)


type alias Model =
    { locales : ActionResult (List KnowledgeModelLocale)
    , selectedUuids : Set String
    }


initialModel : Model
initialModel =
    { locales = Loading
    , selectedUuids = Set.empty
    }


{-| The value for the request's `localeUuids` field. `Nothing` when there are no
locales available (the field should be omitted), otherwise the selected UUIDs.
-}
selectedLocaleUuids : Model -> Maybe (List Uuid)
selectedLocaleUuids model =
    case model.locales of
        Success locales ->
            if List.isEmpty locales then
                Nothing

            else
                locales
                    |> List.filter (\locale -> Set.member (Uuid.toString locale.uuid) model.selectedUuids)
                    |> List.map .uuid
                    |> Just

        _ ->
            Nothing


type Msg
    = GetLocalesCompleted (Result ApiError (List KnowledgeModelLocale))
    | ToggleLocale String


fetchLocales : AppState -> Uuid -> Cmd Msg
fetchLocales appState kmEditorUuid =
    KnowledgeModelEditorsApi.getKnowledgeModelEditorLocales appState kmEditorUuid GetLocalesCompleted


update : AppState -> Msg -> Model -> Model
update appState msg model =
    case msg of
        GetLocalesCompleted result ->
            case result of
                Ok locales ->
                    { model
                        | locales = Success locales
                        , selectedUuids = Set.fromList (List.map (Uuid.toString << .uuid) locales)
                    }

                Err error ->
                    { model | locales = ApiError.toActionResult appState (gettext "Unable to get locales." appState.locale) error }

        ToggleLocale uuidString ->
            { model
                | selectedUuids =
                    if Set.member uuidString model.selectedUuids then
                        Set.remove uuidString model.selectedUuids

                    else
                        Set.insert uuidString model.selectedUuids
            }


view : AppState -> Model -> Html Msg
view appState model =
    case model.locales of
        Success ((_ :: _) as locales) ->
            Field.checkboxGroup
                { id = "km-publish-locales"
                , label = gettext "Copy locales" appState.locale
                , values = Set.toList model.selectedUuids
                , options = List.map localeChoice locales
                , onToggle = ToggleLocale
                }
                |> Field.withHint (gettext "Select the locales to copy to the new version." appState.locale)
                |> Field.toHtml

        _ ->
            text ""


localeChoice : KnowledgeModelLocale -> Field.Choice msg
localeChoice locale =
    Field.choice
        { value = Uuid.toString locale.uuid
        , label = locale.name ++ " (" ++ locale.code ++ ")"
        }
