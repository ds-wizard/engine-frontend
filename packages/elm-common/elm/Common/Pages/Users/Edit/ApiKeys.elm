module Common.Pages.Users.Edit.ApiKeys exposing
    ( Model
    , Msg
    , fetchData
    , initialModel
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.ApiKeys as ApiKeysApi
import Common.Api.Models.ApiKey as ApiKey exposing (ApiKey)
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.Form as CommonForm
import Common.Components.Form.Field as Field
import Common.Components.Markdown as Markdown
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.ApiKeyCreateForm as ApiKeyCreateForm exposing (ApiKeyCreateForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Data.UuidOrCurrent exposing (UuidOrCurrent)
import Common.Pages.Users.Components.Section as Section
import Common.Utils.Form.FormError as FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setApiKey, setApiKeys)
import Common.Utils.TimeUtils as TimeUtils
import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Gettext exposing (gettext)
import Html exposing (Html)
import Iso8601
import List.Extra as List
import Maybe.Extra as Maybe
import String.Format as String
import Time
import Time.Extra as Time
import Ui.Alert as Alert
import Ui.Button as Button
import Ui.CodeBlock as CodeBlock
import Ui.Copy as Copy
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Modal as Modal
import Ui.Table as Table
import Uuid



-- MODEL


type alias Model =
    { uuidOrCurrent : UuidOrCurrent
    , apiKeys : ActionResult (List ApiKey)
    , createModalOpen : Bool
    , apiKey : ActionResult String
    , form : Form FormError ApiKeyCreateForm
    , copyModel : Copy.Model
    , apiKeyToDelete : Maybe ApiKey
    , deletingApiKey : ActionResult String
    }


initialModel : UuidOrCurrent -> Model
initialModel uuidOrCurrent =
    { uuidOrCurrent = uuidOrCurrent
    , apiKeys = Loading
    , createModalOpen = False
    , apiKey = Unset
    , form = ApiKeyCreateForm.init
    , copyModel = Copy.init
    , apiKeyToDelete = Nothing
    , deletingApiKey = Unset
    }


fetchData : AppState -> Cmd Msg
fetchData appState =
    ApiKeysApi.getApiKeys appState GetApiKeysComplete


{-| `ApiKeyCreateForm.validation`, with the expiration turned into the instant
the server expects.

The expiry is local midnight of the chosen day as an ISO instant, and only days
after today are allowed. `Ui.Field.date` gives the calendar date, so the conversion and
the check happen here.

-}
validation : AppState -> Validation FormError ApiKeyCreateForm
validation appState =
    V.succeed ApiKeyCreateForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "expiresAt" (V.string |> V.andThen (expiresAtValidation appState)))


expiresAtValidation : AppState -> String -> Validation FormError String
expiresAtValidation appState value =
    case parseDate value of
        Just ( year, month, day ) ->
            let
                expiresAt =
                    Time.partsToPosix appState.timeZone
                        { year = year
                        , month = month
                        , day = day
                        , hour = 0
                        , minute = 0
                        , second = 0
                        , millisecond = 0
                        }
            in
            if Time.posixToMillis expiresAt > Time.posixToMillis appState.currentTime then
                V.succeed (Iso8601.fromTime expiresAt)

            else
                V.fail (V.customError (FormError.Error (gettext "The expiration date must be in the future." appState.locale)))

        Nothing ->
            V.fail (V.customError (FormError.Error (gettext "Enter a date in the format YYYY-MM-DD." appState.locale)))


{-| `YYYY-MM-DD`, as `Ui.Field.date` gives it.
-}
parseDate : String -> Maybe ( Int, Time.Month, Int )
parseDate value =
    case List.map String.toInt (String.split "-" value) of
        [ Just year, Just monthNumber, Just day ] ->
            let
                months =
                    [ Time.Jan, Time.Feb, Time.Mar, Time.Apr, Time.May, Time.Jun, Time.Jul, Time.Aug, Time.Sep, Time.Oct, Time.Nov, Time.Dec ]
            in
            case List.getAt (monthNumber - 1) months of
                Just month ->
                    if monthNumber >= 1 && day >= 1 && day <= 31 then
                        Just ( year, month, day )

                    else
                        Nothing

                Nothing ->
                    Nothing

        _ ->
            Nothing



-- UPDATE


type Msg
    = GetApiKeysComplete (Result ApiError (List ApiKey))
    | OpenCreateModal
    | CloseCreateModal
    | FormMsg Form.Msg
    | FetchApiKeyComplete (Result ApiError String)
    | CopyMsg Copy.Msg
    | NewApiKeyDone
    | SetApiKeyToDelete (Maybe ApiKey)
    | DeleteApiKey
    | DeleteApiKeyComplete (Result ApiError ())
    | FocusDone


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetApiKeysComplete result ->
            RequestHelpers.applyResult
                { setResult = setApiKeys
                , defaultError = gettext "Unable to get API keys." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        OpenCreateModal ->
            ( { model | createModalOpen = True, apiKey = Unset, form = ApiKeyCreateForm.init }, Cmd.none )

        CloseCreateModal ->
            ( { model | createModalOpen = False, apiKey = Unset, form = ApiKeyCreateForm.init }, Cmd.none )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    ( { model | apiKey = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        ApiKeysApi.fetchApiKey appState (ApiKeyCreateForm.encode form) FetchApiKeyComplete
                    )

                _ ->
                    ( { model | form = Form.update (validation appState) formMsg model.form }
                    , Cmd.none
                    )

        FetchApiKeyComplete result ->
            let
                ( newModel, cmd ) =
                    RequestHelpers.applyResult
                        { setResult = setApiKey
                        , defaultError = gettext "Unable to create API key." appState.locale
                        , model = model
                        , result = result
                        , logoutMsg = cfg.logoutMsg
                        , locale = appState.locale
                        }

                focusCmd =
                    case result of
                        Ok _ ->
                            Cmd.none

                        Err _ ->
                            CommonForm.focusErrorSummary
            in
            ( newModel, Cmd.batch [ cmd, focusCmd ] )

        CopyMsg copyMsg ->
            let
                ( copyModel, copyCmd ) =
                    Copy.update copyMsg model.copyModel
            in
            ( { model | copyModel = copyModel }
            , Cmd.map (cfg.wrapMsg << CopyMsg) copyCmd
            )

        NewApiKeyDone ->
            ( { model
                | createModalOpen = False
                , apiKey = Unset
                , form = ApiKeyCreateForm.init
                , apiKeys = Loading
              }
            , Cmd.map cfg.wrapMsg (fetchData appState)
            )

        SetApiKeyToDelete mbApiKey ->
            ( { model | apiKeyToDelete = mbApiKey, deletingApiKey = Unset }, Cmd.none )

        DeleteApiKey ->
            case model.apiKeyToDelete of
                Just apiKey ->
                    ( { model | deletingApiKey = Loading }
                    , Cmd.map cfg.wrapMsg (ApiKeysApi.deleteApiKey appState apiKey.uuid DeleteApiKeyComplete)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteApiKeyComplete result ->
            case result of
                Ok _ ->
                    ( { model
                        | apiKeyToDelete = Nothing
                        , apiKeys = Loading
                        , deletingApiKey = Unset
                      }
                    , Cmd.map cfg.wrapMsg (Cmd.batch [ fetchData appState, Section.focusHeading FocusDone ])
                    )

                Err error ->
                    ( { model | deletingApiKey = ApiError.toActionResult appState (gettext "API key could not be deleted." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        FocusDone ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    if model.createModalOpen && not (ActionResult.isSuccess model.apiKey) then
        CommonForm.submitShortcut appState { result = model.apiKey, onSubmit = FormMsg Form.Submit }

    else
        Sub.none



-- VIEW


view : AppState -> Model -> List (Html Msg)
view appState model =
    Section.view appState
        { title = gettext "API Keys" appState.locale
        , guideLink = Just GuideLinks.profileApiKeys
        , content =
            [ Markdown.view { headingOffset = 1 }
                (String.format
                    (gettext "You can generate an API key for every application you use that needs access to the [API](%s)." appState.locale)
                    [ appState.apiUrl ++ "/swagger-ui/" ]
                )
            , Layout.inline
                [ Button.primary (gettext "Create" appState.locale) OpenCreateModal
                    |> Button.withIcon (Icon.solid "fa-plus")
                    |> Button.withTestId "api-keys_create"
                    |> Button.toHtml
                ]
            ]
                ++ Section.actionResult appState (viewApiKeys appState) model.apiKeys
                ++ [ viewCreateModal appState model
                   , viewDeleteModal appState model
                   ]
        }


viewApiKeys : AppState -> List ApiKey -> List (Html Msg)
viewApiKeys appState apiKeys =
    let
        activeKeys =
            apiKeys
                |> List.filter (ApiKey.isActive appState.currentTime)
                |> List.sortBy (String.toLower << .name)

        viewTime time =
            Html.text (TimeUtils.toReadableDate appState.timeZone time)

        deleteButton apiKey =
            [ Button.dangerIconOnly (Icon.solid "fa-trash") (gettext "Delete" appState.locale) (SetApiKeyToDelete (Just apiKey))
                |> Button.withAccessibleName (String.format (gettext "Delete %s" appState.locale) [ apiKey.name ])
                |> Button.withTestId "api-keys_delete-button"
            ]

        content =
            if List.isEmpty activeKeys then
                Alert.toHtml (Alert.info (gettext "You have no active API keys." appState.locale))

            else
                Table.table
                    [ Table.column (gettext "API Key Name" appState.locale) (Html.text << .name)
                    , Table.column (gettext "Created" appState.locale) (viewTime << .createdAt)
                    , Table.column (gettext "Expiration" appState.locale) (viewTime << .expiresAt)
                    , Table.actions (gettext "Actions" appState.locale) deleteButton
                    ]
                    activeKeys
                    |> Table.withRowTestId (\row -> "api-keys_row_" ++ Uuid.toString row.uuid)
                    |> Table.toHtml
    in
    Section.subsection
        (String.format (gettext "Active API Keys (%s)" appState.locale) [ String.fromInt (List.length activeKeys) ])
        [ content ]


viewCreateModal : AppState -> Model -> Html Msg
viewCreateModal appState model =
    let
        created =
            case model.apiKey of
                Success apiKey ->
                    Just apiKey

                _ ->
                    Nothing

        body =
            case created of
                Just apiKey ->
                    [ Layout.stack
                        [ CodeBlock.toHtml
                            { label = gettext "Your new API key" appState.locale
                            , language = "text"
                            , value = apiKey
                            }
                        , Alert.toHtml (Alert.warning (gettext "Make sure to save it, you will not be able to access it again." appState.locale))
                        ]
                    ]

                Nothing ->
                    [ Layout.stack
                        [ CommonForm.errorSummary model.apiKey
                        , Field.input appState model.form "name" (gettext "API Key Name" appState.locale)
                            |> Field.withHint (gettext "Give the API key a name to identify it, such as the name of the application using it or the purpose of the key." appState.locale)
                            |> Field.toHtml
                            |> Html.map FormMsg
                        , Field.date appState model.form "expiresAt" (gettext "Expiration" appState.locale)
                            |> Field.withHint (gettext "The date when the API key will no longer be valid." appState.locale)
                            |> Field.toHtml
                            |> Html.map FormMsg
                        ]
                    ]

        -- The modal is a form in both states, so it keeps the same children
        -- while it is on the page; once the key is shown, submitting it is Done.
        -- The copy button is in the header, as in the about dialog, rather than
        -- in the corner of the code block, where it sat over the key.
        modal =
            case created of
                Just apiKey ->
                    Modal.plain
                        { title = gettext "Your new API key" appState.locale
                        , open = model.createModalOpen
                        , onClose = NewApiKeyDone
                        , body = body
                        }
                        |> Modal.withHeaderControl
                            (Copy.labeledWithWords
                                { prompt = gettext "Click to copy" appState.locale
                                , done = gettext "Copied!" appState.locale
                                }
                                { key = "api-key"
                                , label = gettext "Copy" appState.locale
                                , value = apiKey
                                , model = model.copyModel
                                , toMsg = CopyMsg
                                }
                            )
                        |> Modal.withForm { onSubmit = NewApiKeyDone, submitLabel = gettext "Done" appState.locale }
                        |> Modal.withActions [ Button.submit (gettext "Done" appState.locale) ]

                Nothing ->
                    Modal.plain
                        { title = gettext "Create API Key" appState.locale
                        , open = model.createModalOpen
                        , onClose = CloseCreateModal
                        , body = body
                        }
                        |> Modal.withForm { onSubmit = FormMsg Form.Submit, submitLabel = gettext "Create" appState.locale }
                        |> Modal.withLoading (ActionResult.isLoading model.apiKey)
    in
    modal
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withTestId "api-keys_create-modal"
        |> Modal.toHtml


viewDeleteModal : AppState -> Model -> Html Msg
viewDeleteModal appState model =
    let
        apiKeyName =
            Maybe.unwrap "" .name model.apiKeyToDelete
    in
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete API Key" appState.locale
        , open = Maybe.isJust model.apiKeyToDelete
        , question = String.format (gettext "Are you sure you want to delete %s?" appState.locale) [ apiKeyName ]
        , entity = Html.text ""
        , onConfirm = DeleteApiKey
        , onClose = SetApiKeyToDelete Nothing
        , result = model.deletingApiKey
        , testId = Just "api-keys_delete"
        }
