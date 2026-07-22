module Wizard.Pages.KMEditor.Common.PublishLocaleSelection exposing
    ( Model
    , Msg
    , fetchLocales
    , initialModel
    , ready
    , selectedLocaleUuids
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Gettext exposing (gettext)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (checked, class, type_)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onCheck)
import Html.Extra as Html
import Set exposing (Set)
import Uuid exposing (Uuid)
import Wizard.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Wizard.Api.Models.KnowledgeModelLocale exposing (KnowledgeModelLocale)
import Wizard.Data.AppState exposing (AppState)



-- MODEL


type alias Model =
    { locales : ActionResult (List KnowledgeModelLocale)
    , selectedUuids : Set String
    }


initialModel : Model
initialModel =
    { locales = Loading
    , selectedUuids = Set.empty
    }


{-| Whether the locales have finished loading (so it is safe to publish).
-}
ready : Model -> Bool
ready model =
    not (ActionResult.isLoading model.locales)


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



-- UPDATE


type Msg
    = GetLocalesCompleted (Result ApiError (List KnowledgeModelLocale))
    | ToggleLocale String Bool


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

        ToggleLocale uuidString isChecked ->
            { model
                | selectedUuids =
                    if isChecked then
                        Set.insert uuidString model.selectedUuids

                    else
                        Set.remove uuidString model.selectedUuids
            }



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    case model.locales of
        Success locales ->
            if List.isEmpty locales then
                Html.nothing

            else
                div [ class "card bg-light mb-2" ]
                    [ div [ class "card-body" ]
                        [ div [ dataCy "km-publish_locales" ]
                            [ label [ class "mb-0" ] [ text (gettext "Copy locales" appState.locale) ]
                            , div [ class "form-text text-muted mb-2" ]
                                [ text (gettext "Select the locales to copy to the new version." appState.locale) ]
                            , div [ class "ps-4" ] (List.map (viewLocaleCheckbox model) locales)
                            ]
                        ]
                    ]

        _ ->
            Html.nothing


viewLocaleCheckbox : Model -> KnowledgeModelLocale -> Html Msg
viewLocaleCheckbox model locale =
    let
        uuidString =
            Uuid.toString locale.uuid
    in
    div [ class "form-check mb-0" ]
        [ label [ class "form-check-label" ]
            [ input
                [ type_ "checkbox"
                , class "form-check-input"
                , checked (Set.member uuidString model.selectedUuids)
                , onCheck (ToggleLocale uuidString)
                ]
                []
            , text (locale.name ++ " (" ++ locale.code ++ ")")
            ]
        ]
