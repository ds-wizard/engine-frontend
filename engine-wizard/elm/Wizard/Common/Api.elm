module Wizard.Common.Api exposing
    ( applyResult
    , applyResultCmd
    , applyResultTransform
    , applyResultTransformCmd
    , getResultCmd
    )

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError(..))
import Shared.Provisioning exposing (Provisioning)
import Shared.Utils exposing (dispatch)
import Wizard.Auth.Msgs
import Wizard.Msgs


getResultCmd : Result ApiError a -> Cmd Wizard.Msgs.Msg
getResultCmd result =
    case result of
        Ok _ ->
            Cmd.none

        Err error ->
            case error of
                BadStatus 401 _ ->
                    dispatch <| Wizard.Msgs.AuthMsg Wizard.Auth.Msgs.Logout

                _ ->
                    Cmd.none


applyResult :
    { a | provisioning : Provisioning }
    ->
        { setResult : ActionResult data -> model -> model
        , defaultError : String
        , model : model
        , result : Result ApiError data
        }
    -> ( model, Cmd Wizard.Msgs.Msg )
applyResult appState { setResult, defaultError, model, result } =
    applyResultTransform appState
        { setResult = setResult
        , defaultError = defaultError
        , model = model
        , result = result
        , transform = identity
        }


applyResultTransform :
    { a | provisioning : Provisioning }
    ->
        { setResult : ActionResult data2 -> model -> model
        , defaultError : String
        , model : model
        , result : Result ApiError data1
        , transform : data1 -> data2
        }
    -> ( model, Cmd Wizard.Msgs.Msg )
applyResultTransform appState { setResult, defaultError, model, result, transform } =
    applyResultTransformCmd appState
        { setResult = setResult
        , defaultError = defaultError
        , model = model
        , result = result
        , transform = transform
        , cmd = Cmd.none
        }


applyResultCmd :
    { a | provisioning : Provisioning }
    ->
        { setResult : ActionResult data -> model -> model
        , defaultError : String
        , model : model
        , result : Result ApiError data
        , cmd : Cmd Wizard.Msgs.Msg
        }
    -> ( model, Cmd Wizard.Msgs.Msg )
applyResultCmd appState { setResult, defaultError, model, result, cmd } =
    applyResultTransformCmd appState
        { setResult = setResult
        , defaultError = defaultError
        , model = model
        , result = result
        , transform = identity
        , cmd = cmd
        }


applyResultTransformCmd :
    { a | provisioning : Provisioning }
    ->
        { setResult : ActionResult data2 -> model -> model
        , defaultError : String
        , model : model
        , result : Result ApiError data1
        , transform : data1 -> data2
        , cmd : Cmd Wizard.Msgs.Msg
        }
    -> ( model, Cmd Wizard.Msgs.Msg )
applyResultTransformCmd appState { setResult, defaultError, model, result, transform, cmd } =
    case result of
        Ok data ->
            ( setResult (Success <| transform data) model
            , cmd
            )

        Err error ->
            ( setResult (ApiError.toActionResult appState defaultError error) model
            , getResultCmd result
            )
