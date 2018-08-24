module Organization.Update exposing (getCurrentOrganizationCmd, update)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Form exposing (Form)
import Jwt
import Msgs
import Organization.Models exposing (..)
import Organization.Msgs exposing (Msg(..))
import Organization.Requests exposing (..)
import Requests exposing (getResultCmd, toCmd)


getCurrentOrganizationCmd : Session -> Cmd Msgs.Msg
getCurrentOrganizationCmd session =
    getCurrentOrganization session
        |> toCmd GetCurrentOrganizationCompleted Msgs.OrganizationMsg


putCurrentOrganizationCmd : Session -> OrganizationForm -> String -> Cmd Msgs.Msg
putCurrentOrganizationCmd session form uuid =
    form
        |> encodeOrganizationForm uuid
        |> putCurrentOrganization session
        |> toCmd PutCurrentOrganizationCompleted Msgs.OrganizationMsg


getCurrentOrganizationCompleted : Model -> Result Jwt.JwtError Organization -> ( Model, Cmd Msgs.Msg )
getCurrentOrganizationCompleted model result =
    let
        newModel =
            case result of
                Ok organization ->
                    { model | form = initOrganizationForm organization, organization = Success organization }

                Err error ->
                    { model | organization = getServerErrorJwt error "Unable to get organization information." }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


putCurrentOrganizationCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
putCurrentOrganizationCompleted model result =
    let
        newResult =
            case result of
                Ok organization ->
                    Success "Organization was successfuly saved"

                Err error ->
                    getServerErrorJwt error "Organization could not be saved"

        cmd =
            getResultCmd result
    in
    ( { model | savingOrganization = newResult }, cmd )


handleForm : Form.Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg session model =
    case ( formMsg, Form.getOutput model.form, model.organization ) of
        ( Form.Submit, Just form, Success organization ) ->
            let
                cmd =
                    putCurrentOrganizationCmd session form organization.uuid
            in
            ( { model | savingOrganization = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update organizationFormValidation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )


{-| -}
update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetCurrentOrganizationCompleted result ->
            getCurrentOrganizationCompleted model result

        PutCurrentOrganizationCompleted result ->
            putCurrentOrganizationCompleted model result

        FormMsg formMsg ->
            handleForm formMsg session model
