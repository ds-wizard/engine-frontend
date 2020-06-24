module Wizard.Common.Api exposing
    ( applyResult
    , applyResultCmd
    , applyResultTransform
    , applyResultTransformCmd
    , getResultCmd
    )

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError(..))
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
    { setResult : ActionResult data -> model -> model
    , defaultError : String
    , model : model
    , result : Result ApiError data
    }
    -> ( model, Cmd Wizard.Msgs.Msg )
applyResult { setResult, defaultError, model, result } =
    applyResultTransform
        { setResult = setResult
        , defaultError = defaultError
        , model = model
        , result = result
        , transform = identity
        }


applyResultTransform :
    { setResult : ActionResult data2 -> model -> model
    , defaultError : String
    , model : model
    , result : Result ApiError data1
    , transform : data1 -> data2
    }
    -> ( model, Cmd Wizard.Msgs.Msg )
applyResultTransform { setResult, defaultError, model, result, transform } =
    applyResultTransformCmd
        { setResult = setResult
        , defaultError = defaultError
        , model = model
        , result = result
        , transform = transform
        , cmd = Cmd.none
        }


applyResultCmd :
    { setResult : ActionResult data -> model -> model
    , defaultError : String
    , model : model
    , result : Result ApiError data
    , cmd : Cmd Wizard.Msgs.Msg
    }
    -> ( model, Cmd Wizard.Msgs.Msg )
applyResultCmd { setResult, defaultError, model, result, cmd } =
    applyResultTransformCmd
        { setResult = setResult
        , defaultError = defaultError
        , model = model
        , result = result
        , transform = identity
        , cmd = cmd
        }


applyResultTransformCmd :
    { setResult : ActionResult data2 -> model -> model
    , defaultError : String
    , model : model
    , result : Result ApiError data1
    , transform : data1 -> data2
    , cmd : Cmd Wizard.Msgs.Msg
    }
    -> ( model, Cmd Wizard.Msgs.Msg )
applyResultTransformCmd { setResult, defaultError, model, result, transform, cmd } =
    case result of
        Ok data ->
            ( setResult (Success <| transform data) model
            , cmd
            )

        Err error ->
            ( setResult (ApiError.toActionResult defaultError error) model
            , getResultCmd result
            )
