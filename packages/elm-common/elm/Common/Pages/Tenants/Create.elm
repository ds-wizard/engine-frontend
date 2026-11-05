module Common.Pages.Tenants.Create exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Tenants as TenantsApi
import Common.Components.Form as FormLayout
import Common.Components.Form.Field as Field
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.TenantCreateForm as TenantCreateForm exposing (TenantCreateForm)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Form.Field
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Normalize as Normalize


type alias Model =
    { savingTenant : ActionResult String
    , form : Form FormError TenantCreateForm
    }


type Msg
    = Cancel
    | FormMsg Form.Msg
    | PostTenantComplete (Result ApiError ())


initialModel : Model
initialModel =
    { savingTenant = Unset
    , form = TenantCreateForm.init
    }


init : ( Model, Cmd Msg )
init =
    -- Focus is left to the shell, which moves it to the page heading.
    ( initialModel, Cmd.none )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        Cancel ->
            ( model, Window.historyBack (Routes.toUrl appState Routes.tenantsIndex) )

        FormMsg formMsg ->
            handleForm cfg appState formMsg model

        PostTenantComplete result ->
            case result of
                Ok _ ->
                    ( model, Routes.navigate appState Routes.tenantsIndex )

                Err error ->
                    ( { model
                        | savingTenant = ApiError.toActionResult appState "Tenant could not be created." error
                        , form = FormUtils.setFormErrors appState error model.form
                      }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , FormLayout.focusErrorSummary
                        ]
                    )


handleForm : UpdateConfig Msg msg -> AppState -> Form.Msg -> Model -> ( Model, Cmd msg )
handleForm cfg appState formMsg model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just tenantCreateForm ) ->
            ( { model | savingTenant = Loading }
            , TenantsApi.postTenant appState (TenantCreateForm.encode tenantCreateForm) (cfg.wrapMsg << PostTenantComplete)
            )

        _ ->
            let
                newForm =
                    Form.update TenantCreateForm.validation formMsg model.form

                tenantIdEmpty =
                    Maybe.unwrap True String.isEmpty (Form.getFieldAsString "tenantId" model.form).value

                formWithTenantId =
                    case ( formMsg, tenantIdEmpty ) of
                        ( Form.Blur "tenantName", True ) ->
                            let
                                suggestedTenantId =
                                    (Form.getFieldAsString "tenantName" model.form).value
                                        |> Maybe.unwrap "" Normalize.slug
                            in
                            Form.update TenantCreateForm.validation (Form.Input "tenantId" Form.Text (Form.Field.String suggestedTenantId)) newForm

                        _ ->
                            newForm
            in
            ( { model | form = formWithTenantId }, Cmd.none )


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    FormLayout.submitShortcut appState { result = model.savingTenant, onSubmit = FormMsg Form.Submit }


view : AppState -> Model -> Html Msg
view appState model =
    let
        field toField path label =
            toField appState model.form path label
                |> Field.withRequired True
                |> Field.toHtml
                |> Html.map FormMsg
    in
    FormLayout.createPage appState
        { title = "Create Tenant"
        , description = Nothing
        , actions = []
        , content =
            [ field Field.input "tenantName" "Name"
            , field Field.input "tenantId" "Tenant ID"
            , field Field.email "email" "Email"
            , field Field.input "firstName" "First name"
            , field Field.input "lastName" "Last name"
            ]
        , submitLabel = "Create"
        , result = model.savingTenant
        , onSubmit = FormMsg Form.Submit
        , onCancel = Just Cancel
        }
