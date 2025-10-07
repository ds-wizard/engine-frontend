module Common.Utils.RequestHelpers exposing
    ( applyResult
    , applyResultCmd
    , applyResultTransform
    , applyResultTransformCmd
    , getResultCmd
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Gettext
import Task.Extra as Task


getResultCmd : msg -> Result ApiError a -> Cmd msg
getResultCmd logoutMsg result =
    case result of
        Ok _ ->
            Cmd.none

        Err error ->
            case error of
                ApiError.BadStatus 401 _ ->
                    Task.dispatch logoutMsg

                _ ->
                    Cmd.none


applyResult :
    { setResult : ActionResult data -> model -> model
    , defaultError : String
    , model : model
    , result : Result ApiError data
    , logoutMsg : msg
    , locale : Gettext.Locale
    }
    -> ( model, Cmd msg )
applyResult { setResult, defaultError, model, result, logoutMsg, locale } =
    applyResultTransform
        { setResult = setResult
        , defaultError = defaultError
        , model = model
        , result = result
        , logoutMsg = logoutMsg
        , transform = identity
        , locale = locale
        }


applyResultTransform :
    { setResult : ActionResult data2 -> model -> model
    , defaultError : String
    , model : model
    , result : Result ApiError data1
    , logoutMsg : msg
    , transform : data1 -> data2
    , locale : Gettext.Locale
    }
    -> ( model, Cmd msg )
applyResultTransform { setResult, defaultError, model, result, logoutMsg, transform, locale } =
    applyResultTransformCmd
        { setResult = setResult
        , defaultError = defaultError
        , model = model
        , result = result
        , logoutMsg = logoutMsg
        , transform = transform
        , cmd = Cmd.none
        , locale = locale
        }


applyResultCmd :
    { setResult : ActionResult data -> model -> model
    , defaultError : String
    , model : model
    , result : Result ApiError data
    , logoutMsg : msg
    , cmd : Cmd msg
    , locale : Gettext.Locale
    }
    -> ( model, Cmd msg )
applyResultCmd { setResult, defaultError, model, result, logoutMsg, cmd, locale } =
    applyResultTransformCmd
        { setResult = setResult
        , defaultError = defaultError
        , model = model
        , result = result
        , logoutMsg = logoutMsg
        , transform = identity
        , cmd = cmd
        , locale = locale
        }


applyResultTransformCmd :
    { setResult : ActionResult data2 -> model -> model
    , defaultError : String
    , model : model
    , result : Result ApiError data1
    , logoutMsg : msg
    , transform : data1 -> data2
    , cmd : Cmd msg
    , locale : Gettext.Locale
    }
    -> ( model, Cmd msg )
applyResultTransformCmd { setResult, defaultError, model, result, logoutMsg, transform, cmd, locale } =
    case result of
        Ok data ->
            ( setResult (ActionResult.Success <| transform data) model
            , cmd
            )

        Err error ->
            ( setResult (ApiError.toActionResult { locale = locale } defaultError error) model
            , getResultCmd logoutMsg result
            )
