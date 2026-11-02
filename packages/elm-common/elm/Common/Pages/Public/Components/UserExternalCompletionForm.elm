module Common.Pages.Public.Components.UserExternalCompletionForm exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , submitMsg
    , update
    , view
    )

{-| The details an external identity provider did not send, asked for before
the account is created: email, first name and last name.

What the provider did send is shown and cannot be changed — the field is
disabled — so the account keeps the identity it was vouched for.

Built on `Common.Components.Form`. The panel and its title are the page's.

-}

import ActionResult exposing (ActionResult)
import Common.Api.Models.UserFromExternal as UserFromExternal exposing (UserFromExternal)
import Common.Components.Form as FormView
import Common.Components.Form.Field as FormField
import Common.Data.Context exposing (WithLocale)
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe


type alias Model =
    { userFromExternal : UserFromExternal
    , form : Form FormError UserFromExternal
    }


init : UserFromExternal -> Model
init userFromExternal =
    { userFromExternal = userFromExternal
    , form = UserFromExternal.initForm userFromExternal
    }


type Msg
    = FormMsg Form.Msg


{-| For the page's submit shortcut, which sends the same message as the form.
-}
submitMsg : Msg
submitMsg =
    FormMsg Form.Submit


type alias UpdateConfig msg =
    { submitMsg : UserFromExternal -> Cmd msg
    , wrapMsg : Msg -> msg
    }


update : UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update cfg msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just formData ) ->
                    ( model, cfg.submitMsg formData )

                _ ->
                    ( { model | form = Form.update UserFromExternal.validation formMsg model.form }
                    , Cmd.none
                    )


view : WithLocale a -> { completingRegistration : ActionResult String } -> Model -> Html Msg
view appState config model =
    let
        field constructor path label readonly =
            constructor appState model.form path label
                |> FormField.withRequired True
                |> FormField.withDisabled readonly
                |> FormField.toHtml
                |> Html.map FormMsg
    in
    FormView.view
        { onSubmit = submitMsg
        , content =
            [ FormView.errorSummary config.completingRegistration
            , field FormField.email "email" (gettext "Email" appState.locale) (Maybe.isJust model.userFromExternal.email)
            , field FormField.input "firstName" (gettext "First Name" appState.locale) (Maybe.isJust model.userFromExternal.firstName)
            , field FormField.input "lastName" (gettext "Last Name" appState.locale) (Maybe.isJust model.userFromExternal.lastName)
            , FormView.actions appState
                { submitLabel = gettext "Continue" appState.locale
                , result = config.completingRegistration
                , onCancel = Nothing
                }
            ]
        }
