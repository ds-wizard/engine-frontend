module Organization.Update exposing (..)

import Auth.Models exposing (Session)
import Form exposing (Form)
import Jwt
import Msgs
import Organization.Models exposing (..)
import Organization.Msgs exposing (Msg(..))
import Organization.Requests exposing (..)
import Requests exposing (toCmd)
import Utils exposing (FormResult(..))


getCurrentOrganizationCmd : Session -> Cmd Msgs.Msg
getCurrentOrganizationCmd session =
    getCurrentOrganization session
        |> toCmd GetCurrentOrganizationCompleted Msgs.OrganizationMsg


putCurrentOrganizationCmd : Session -> OrganizationForm -> Cmd Msgs.Msg
putCurrentOrganizationCmd session form =
    form
        |> encodeOrganizationForm
        |> putCurrentOrganization session
        |> toCmd PutCurrentOrganizationCompleted Msgs.OrganizationMsg


getCurrentOrganizationCompleted : Model -> Result Jwt.JwtError Organization -> ( Model, Cmd Msgs.Msg )
getCurrentOrganizationCompleted model result =
    let
        newModel =
            case result of
                Ok organization ->
                    { model | form = initOrganizationForm organization }

                Err error ->
                    { model | loadingError = "Unable to get organization information." }
    in
    ( { newModel | loading = False }, Cmd.none )


putCurrentOrganizationCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
putCurrentOrganizationCompleted model result =
    let
        newResult =
            case result of
                Ok organization ->
                    Success "Organization was successfuly saved"

                Err error ->
                    Error "Organization could not be saved"
    in
    ( { model | result = newResult, saving = False }, Cmd.none )


handleForm : Form.Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg session model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                cmd =
                    putCurrentOrganizationCmd session form
            in
            ( { model | saving = True }, cmd )

        _ ->
            let
                form =
                    Form.update organizationFormValidation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetCurrentOrganizationCompleted result ->
            getCurrentOrganizationCompleted model result

        PutCurrentOrganizationCompleted result ->
            putCurrentOrganizationCompleted model result

        FormMsg formMsg ->
            handleForm formMsg session model
