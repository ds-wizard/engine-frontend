module Common.Pages.Settings.OpenIdDetail exposing
    ( Model
    , Msg
    , entityName
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Browser.Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.OpenIdClientDetail exposing (OpenIdClientDetail)
import Common.Api.OpenIdClients as OpenIdClientsApi
import Common.Components.Copyable as Copyable
import Common.Components.Form as CommonForm
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.OpenIdClientForm as OpenIdClientForm exposing (OpenIdClientForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.NotFound as NotFound
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.LinkedEntity as LinkedEntity
import Common.Pages.Settings.Components.OpenIdClientFields as OpenIdClientFields
import Common.Pages.Settings.Components.Section as Section
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import String.Format as String
import Task
import Ui.AppShell as AppShell
import Ui.Card as Card
import Ui.Copy as Copy
import Ui.Field as UiField
import Ui.Layout as Layout
import Uuid exposing (Uuid)


type alias Model =
    { uuid : Uuid
    , openIdClient : ActionResult OpenIdClientDetail
    , linkedUuid : Maybe Uuid
    , linkedEntity : LinkedEntity.Model
    , form : Form FormError OpenIdClientForm
    , formRemoved : Bool
    , savingOpenId : ActionResult ()
    , copy : Copy.Model
    }


type Msg
    = GetOpenIdComplete (Result ApiError ( OpenIdClientDetail, Maybe Uuid ))
    | PutOpenIdComplete (Result ApiError ())
    | FormMsg Form.Msg
    | CopyMsg Copy.Msg
    | LinkedEntityMsg LinkedEntity.Msg
    | Discard
    | FocusDone


initialModel : AppState -> Uuid -> Model
initialModel appState uuid =
    { uuid = uuid
    , openIdClient = Loading
    , linkedUuid = Nothing
    , linkedEntity = LinkedEntity.initialModel
    , form = OpenIdClientForm.initEmpty appState
    , formRemoved = False
    , savingOpenId = Unset
    , copy = Copy.init
    }


init : AppState -> Uuid -> ( Model, Cmd Msg )
init appState uuid =
    ( initialModel appState uuid
    , OpenIdClientsApi.getOpenIdClient appState uuid (LinkedEntity.decoder appState.extensions.openIdLink) GetOpenIdComplete
    )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetOpenIdComplete result ->
            case result of
                Ok ( openId, linkedUuid ) ->
                    ( { model | openIdClient = Success openId, linkedUuid = linkedUuid, form = OpenIdClientForm.init appState openId, formRemoved = False }
                    , Cmd.none
                    )

                Err error ->
                    if ApiError.isNotFound error then
                        ( model
                        , UpdateConfig.notFound cfg (NotFound.openIdConfig appState)
                        )

                    else
                        ( { model | openIdClient = ApiError.toActionResult appState (gettext "Unable to get OpenID config" appState.locale) error }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

        PutOpenIdComplete result ->
            case result of
                Ok _ ->
                    ( { model | savingOpenId = Success () }, CommonForm.reloadWhenSaved )

                Err error ->
                    ( { model | savingOpenId = ApiError.toActionResult appState (gettext "OpenID config could not be saved." appState.locale) error }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , CommonForm.focusErrorSummary
                        ]
                    )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    ( { model | savingOpenId = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        OpenIdClientsApi.putOpenIdClient appState (OpenIdClientForm.toOpenIdClientDetail model.uuid form) PutOpenIdComplete
                    )

                _ ->
                    let
                        formRemoved =
                            case formMsg of
                                Form.RemoveItem _ _ ->
                                    True

                                _ ->
                                    model.formRemoved
                    in
                    ( { model
                        | form = Form.update (OpenIdClientForm.validation appState) formMsg model.form
                        , formRemoved = formRemoved
                      }
                    , OpenIdClientFields.focusProviderTab (cfg.wrapMsg FocusDone) formMsg
                    )

        CopyMsg copyMsg ->
            let
                ( copy, cmd ) =
                    Copy.update copyMsg model.copy
            in
            ( { model | copy = copy }, Cmd.map (cfg.wrapMsg << CopyMsg) cmd )

        LinkedEntityMsg linkedEntityMsg ->
            case appState.extensions.openIdLink of
                Just linkedEntity ->
                    Tuple.mapFirst (\m -> { model | linkedEntity = m }) <|
                        LinkedEntity.update (UpdateConfig.map LinkedEntityMsg cfg)
                            appState
                            { linkedEntity = linkedEntity, apiPath = "/open-id-clients/" ++ Uuid.toString model.uuid }
                            linkedEntityMsg
                            model.linkedEntity

                Nothing ->
                    ( model, Cmd.none )

        Discard ->
            case model.openIdClient of
                Success openId ->
                    ( { model | form = OpenIdClientForm.init appState openId, formRemoved = False, savingOpenId = Unset }
                    , Cmd.map cfg.wrapMsg (Task.attempt (always FocusDone) (Browser.Dom.focus AppShell.mainHeadingId))
                    )

                _ ->
                    ( model, Cmd.none )

        FocusDone ->
            ( model, Cmd.none )


formChanged : Model -> Bool
formChanged model =
    model.formRemoved || FormUtils.containsChanges model.form


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    if formChanged model && ActionResult.isSuccess model.openIdClient then
        CommonForm.submitShortcut appState { result = model.savingOpenId, onSubmit = FormMsg Form.Submit }

    else
        Sub.none


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState model =
    { body =
        [ Layout.stack
            [ Section.header (gettext "Edit OpenID Config" appState.locale)
                [ Section.guideLink appState GuideLinks.settingsOpenId ]
            , Section.actionResult appState (viewOpenId appState model) model.openIdClient
            ]
        ]
    , footer =
        if ActionResult.isSuccess model.openIdClient then
            Just
                -- A successful save reloads the application: the bar goes
                -- first, showing the check on its Save.
                (CommonForm.unsavedBar appState
                    { changed = formChanged model && not (ActionResult.isSuccess model.savingOpenId)
                    , result = model.savingOpenId
                    , onSave = FormMsg Form.Submit
                    , onDiscard = Just Discard
                    }
                )

        else
            Nothing
    }


viewOpenId : AppState -> Model -> OpenIdClientDetail -> Html Msg
viewOpenId appState model openId =
    let
        isMicrosoftMode =
            OpenIdClientForm.isMicrosoftMode model.form

        ( callbackUrlLabel, logoutUrlLabel ) =
            if isMicrosoftMode then
                ( gettext "Redirect URI" appState.locale
                , gettext "Front-channel logout URL" appState.locale
                )

            else
                ( gettext "Callback URL" appState.locale
                , gettext "Logout URL" appState.locale
                )

        viewUrl key label url =
            UiField.readOnlyHtml
                { id = "openid-" ++ key
                , label = label
                , value =
                    [ Copyable.inline appState
                        { key = key
                        , label = String.format (gettext "Copy %s" appState.locale) [ label ]
                        , value = url
                        , model = model.copy
                        , toMsg = CopyMsg
                        }
                    ]
                }
                |> UiField.toHtml

        urls =
            Card.view
                [ viewUrl "callback-url" callbackUrlLabel (appState.clientUrl ++ "/open-id/" ++ Uuid.toString openId.uuid ++ "/callback")
                , viewUrl "logout-url" logoutUrlLabel (appState.apiUrl ++ "/open-id-clients/" ++ Uuid.toString openId.uuid ++ "/logout")
                ]

        linked =
            case appState.extensions.openIdLink of
                Just linkedEntity ->
                    List.map (Html.map LinkedEntityMsg) (LinkedEntity.view linkedEntity model.linkedUuid model.linkedEntity)

                Nothing ->
                    []
    in
    Layout.stack
        [ CommonForm.errorSummary model.savingOpenId
        , CommonForm.view
            { onSubmit = FormMsg Form.Submit
            , content =
                OpenIdClientFields.view appState { form = model.form, toMsg = FormMsg, afterName = [ urls ] }
                    ++ linked
            }
        ]


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (formChanged model && not (ActionResult.isSuccess model.savingOpenId))


{-| What the page is about, by name, for the window's title, once it has
arrived.
-}
entityName : Model -> Maybe String
entityName model =
    Maybe.map .name (ActionResult.toMaybe model.openIdClient)
