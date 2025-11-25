module Wizard.Pages.KMEditor.Editor.Components.KMEditor.UrlChecker exposing
    ( Model
    , Msg
    , ViewConfig
    , countBrokenReferences
    , getResultByUrl
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.ActionButton as ActionButton
import Common.Components.Flash as Flash
import Common.Components.FontAwesome exposing (faWarning)
import Common.Components.FormResult as FormResult
import Gettext exposing (gettext, ngettext)
import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (class)
import Html.Extra as Html
import List.Extra as List
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel.Reference.URLReferenceData as URLReferenceData exposing (URLReferenceData)
import Wizard.Api.Models.UrlCheckResponse as UrlCheckResponse exposing (UrlCheckResponse)
import Wizard.Api.Models.UrlCheckResponse.UrlResult as UrlResult exposing (UrlResult)
import Wizard.Api.UrlCheck as UrlCheckApi
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Routes as Routes


type alias Model =
    { urlCheckResponse : ActionResult UrlCheckResponse
    }


initialModel : Model
initialModel =
    { urlCheckResponse = ActionResult.Unset
    }


getResultByUrl : String -> Model -> Maybe UrlResult
getResultByUrl url model =
    ActionResult.toMaybe model.urlCheckResponse
        |> Maybe.andThen (UrlCheckResponse.getResultByUrl url)


countBrokenReferences : List URLReferenceData -> Model -> Int
countBrokenReferences urlReferences model =
    let
        isBroken reference =
            case getResultByUrl reference.url model of
                Just urlResult ->
                    not urlResult.ok

                Nothing ->
                    False
    in
    List.length <| List.filter isBroken urlReferences


type Msg
    = CheckUrls (List String)
    | CheckUrlsCompleted (Result ApiError UrlCheckResponse)


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        CheckUrls urls ->
            ( { model | urlCheckResponse = ActionResult.Loading }
            , UrlCheckApi.postUrlCheck appState
                { urls = urls }
                CheckUrlsCompleted
            )

        CheckUrlsCompleted result ->
            case result of
                Ok urlCheckResponse ->
                    ( { model | urlCheckResponse = ActionResult.Success urlCheckResponse }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | urlCheckResponse = ApiError.toActionResult appState (gettext "Failed to check URLs." appState.locale) error }
                    , Cmd.none
                    )


type alias ViewConfig =
    { references : List URLReferenceData
    , kmEditorUuid : Uuid
    }


view : AppState -> ViewConfig -> Model -> Html Msg
view appState cfg model =
    let
        content =
            if List.isEmpty cfg.references then
                viewNoReferences appState

            else
                viewChecker appState cfg model
    in
    div [ class "editor-right-panel" ] content


viewNoReferences : AppState -> List (Html msg)
viewNoReferences appState =
    [ Flash.info (gettext "There are no URL references in the Knowledge Model to check." appState.locale) ]


viewChecker : AppState -> ViewConfig -> Model -> List (Html Msg)
viewChecker appState cfg model =
    let
        urls =
            cfg.references
                |> List.map .url
                |> List.unique

        referenceCount =
            List.length cfg.references

        results =
            case model.urlCheckResponse of
                ActionResult.Success urlCheckResponse ->
                    viewBrokenReferences appState cfg urlCheckResponse

                _ ->
                    Html.nothing
    in
    [ div [ class "mb-4" ]
        [ p [ class "text-muted small mb-2" ]
            [ text (String.format (ngettext ( "There is %s URL reference to check.", "There are %s URL references to check." ) referenceCount appState.locale) [ String.fromInt referenceCount ])
            ]
        , ActionButton.button
            { label = gettext "Check URLs" appState.locale
            , result = model.urlCheckResponse
            , msg = CheckUrls urls
            , dangerous = False
            }
        ]
    , FormResult.errorOnlyView model.urlCheckResponse
    , results
    ]


viewBrokenReferences : AppState -> ViewConfig -> UrlCheckResponse -> Html msg
viewBrokenReferences appState cfg urlCheckResponse =
    let
        brokenReferences =
            List.filterMap (viewBrokenReference appState cfg urlCheckResponse) cfg.references
    in
    if List.isEmpty brokenReferences then
        Flash.success (gettext "All URL references are valid." appState.locale)

    else
        let
            brokenCount =
                List.length brokenReferences

            message =
                String.format (ngettext ( "Found 1 broken reference.", "Found %s broken references." ) brokenCount appState.locale) [ String.fromInt brokenCount ]
        in
        div []
            [ Flash.warning message
            , div [ class "list-group list-group-flush" ] brokenReferences
            ]


viewBrokenReference : AppState -> ViewConfig -> UrlCheckResponse -> URLReferenceData -> Maybe (Html msg)
viewBrokenReference appState cfg urlCheckResponse referenceData =
    case UrlCheckResponse.getResultByUrl referenceData.url urlCheckResponse of
        Just urlResult ->
            if urlResult.ok then
                Nothing

            else
                let
                    warningText =
                        UrlResult.toReadableErrorString appState.locale urlResult
                            |> Maybe.withDefault (gettext "Unknown error." appState.locale)
                in
                Just <|
                    div [ class "list-group-item px-0" ]
                        [ linkTo (Routes.kmEditorEditor cfg.kmEditorUuid (Just (Uuid.fromUuidString referenceData.uuid)))
                            [ class "fw-bold" ]
                            [ text (URLReferenceData.toLabel referenceData) ]
                        , p [ class "mb-0 text-muted" ]
                            [ span [ class "me-1 text-danger" ] [ faWarning ]
                            , text warningText
                            ]
                        ]

        Nothing ->
            Nothing
