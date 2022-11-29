module Wizard.Common.Api exposing
    ( applyResult
    , applyResultCmd
    , applyResultTransform
    , applyResultTransformCmd
    , getResultCmd
    )

import ActionResult exposing (ActionResult(..))
import Gettext
import Shared.Error.ApiError as ApiError exposing (ApiError(..))
import Shared.Utils exposing (dispatch)


getResultCmd : msg -> Result ApiError a -> Cmd msg
getResultCmd logoutMsg result =
    case result of
        Ok _ ->
            Cmd.none

        Err error ->
            case error of
                BadStatus 401 _ ->
                    dispatch logoutMsg

                _ ->
                    Cmd.none


applyResult :
    { a | locale : Gettext.Locale }
    ->
        { setResult : ActionResult data -> model -> model
        , defaultError : String
        , model : model
        , result : Result ApiError data
        , logoutMsg : msg
        }
    -> ( model, Cmd msg )
applyResult appState { setResult, defaultError, model, result, logoutMsg } =
    applyResultTransform appState
        { setResult = setResult
        , defaultError = defaultError
        , model = model
        , result = result
        , logoutMsg = logoutMsg
        , transform = identity
        }


applyResultTransform :
    { a | locale : Gettext.Locale }
    ->
        { setResult : ActionResult data2 -> model -> model
        , defaultError : String
        , model : model
        , result : Result ApiError data1
        , logoutMsg : msg
        , transform : data1 -> data2
        }
    -> ( model, Cmd msg )
applyResultTransform appState { setResult, defaultError, model, result, logoutMsg, transform } =
    applyResultTransformCmd appState
        { setResult = setResult
        , defaultError = defaultError
        , model = model
        , result = result
        , logoutMsg = logoutMsg
        , transform = transform
        , cmd = Cmd.none
        }


applyResultCmd :
    { a | locale : Gettext.Locale }
    ->
        { setResult : ActionResult data -> model -> model
        , defaultError : String
        , model : model
        , result : Result ApiError data
        , logoutMsg : msg
        , cmd : Cmd msg
        }
    -> ( model, Cmd msg )
applyResultCmd appState { setResult, defaultError, model, result, logoutMsg, cmd } =
    applyResultTransformCmd appState
        { setResult = setResult
        , defaultError = defaultError
        , model = model
        , result = result
        , logoutMsg = logoutMsg
        , transform = identity
        , cmd = cmd
        }


applyResultTransformCmd :
    { a | locale : Gettext.Locale }
    ->
        { setResult : ActionResult data2 -> model -> model
        , defaultError : String
        , model : model
        , result : Result ApiError data1
        , logoutMsg : msg
        , transform : data1 -> data2
        , cmd : Cmd msg
        }
    -> ( model, Cmd msg )
applyResultTransformCmd appState { setResult, defaultError, model, result, logoutMsg, transform, cmd } =
    case result of
        Ok data ->
            ( setResult (Success <| transform data) model
            , cmd
            )

        Err error ->
            ( setResult (ApiError.toActionResult appState defaultError error) model
            , getResultCmd logoutMsg result
            )
