module Common.Components.UserExternalCompletionForm exposing
    ( Model
    , Msg
    , UpdateConfig
    , ViewConfig
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.Models.UserFromExternal as UserFromExternal exposing (UserFromExternal)
import Common.Components.ActionButton as ActionButton
import Common.Components.FormGroup as FormGroup
import Common.Components.FormResult as FormResult
import Common.Data.Navigator exposing (Navigator)
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.ShortcutUtils as Shortcut
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div, form, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onSubmit)
import Maybe.Extra as Maybe
import Shortcut


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
                    ( model
                    , cfg.submitMsg formData
                    )

                _ ->
                    let
                        form =
                            Form.update UserFromExternal.validation formMsg model.form
                    in
                    ( { model | form = form }
                    , Cmd.none
                    )


type alias ViewConfig =
    { completingRegistration : ActionResult String
    , navigator : Navigator
    , locale : Gettext.Locale
    }


view : ViewConfig -> Model -> Html Msg
view cfg model =
    let
        emailFieldReadonly =
            Maybe.isJust model.userFromExternal.email

        firstNameFieldReadonly =
            Maybe.isJust model.userFromExternal.firstName

        lastNameFieldReadonly =
            Maybe.isJust model.userFromExternal.lastName

        shortcuts =
            [ Shortcut.submitShortcut cfg.navigator.isMac (FormMsg Form.Submit) ]
    in
    wrapper
        [ Shortcut.shortcutElement shortcuts
            []
            [ form [ class "card bg-light", onSubmit (FormMsg Form.Submit) ]
                [ div [ class "card-header fw-bold" ]
                    [ text (gettext "Complete Your Registration" cfg.locale)
                    ]
                , div [ class "card-body" ]
                    [ FormResult.view cfg.completingRegistration
                    , Html.map FormMsg <|
                        div [ class "form-group" ]
                            [ FormGroup.inputAttrs [ disabled emailFieldReadonly ] cfg.locale model.form "email" (gettext "Email" cfg.locale)
                            , FormGroup.inputAttrs [ disabled firstNameFieldReadonly ] cfg.locale model.form "firstName" (gettext "First Name" cfg.locale)
                            , FormGroup.inputAttrs [ disabled lastNameFieldReadonly ] cfg.locale model.form "lastName" (gettext "Last Name" cfg.locale)
                            ]
                    , div [ class "form-group" ]
                        [ ActionButton.submitWithAttrs
                            { label = gettext "Continue" cfg.locale
                            , result = cfg.completingRegistration
                            , attrs = [ class "w-100" ]
                            }
                        ]
                    ]
                ]
            ]
        ]


wrapper : List (Html msg) -> Html msg
wrapper children =
    div [ class "container" ]
        [ div [ class "row mt-5" ]
            [ div [ class "col-xl-4 col-lg-5 col-md-6 col-sm-8 mx-auto" ]
                children
            ]
        ]
