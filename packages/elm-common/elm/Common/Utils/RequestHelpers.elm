module Common.Utils.RequestHelpers exposing
    ( applyResult
    , applyResultCmd
    , applyResultOrNotFound
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


{-| `applyResult` for the one thing a page is about: when the server says there
is no such thing, the page does not show an error, it hands over to whatever
`notFoundCmd` does — for an application, showing its Not Found page. When the
server says the thing is not for this reader, it hands over to `notAllowedCmd`
the same way.

A function of its own rather than a field on `applyResult`, so the thirty
callers that have nothing to hand over stay as they are.

-}
applyResultOrNotFound :
    { setResult : ActionResult data -> model -> model
    , defaultError : String
    , notFoundCmd : Cmd msg
    , notAllowedCmd : Cmd msg
    , model : model
    , result : Result ApiError data
    , logoutMsg : msg
    , locale : Gettext.Locale
    }
    -> ( model, Cmd msg )
applyResultOrNotFound config =
    case config.result of
        Err error ->
            if ApiError.isNotFound error then
                ( config.model, config.notFoundCmd )

            else if ApiError.isForbidden error then
                ( config.model, config.notAllowedCmd )

            else
                applyResult
                    { setResult = config.setResult
                    , defaultError = config.defaultError
                    , model = config.model
                    , result = config.result
                    , logoutMsg = config.logoutMsg
                    , locale = config.locale
                    }

        Ok _ ->
            applyResult
                { setResult = config.setResult
                , defaultError = config.defaultError
                , model = config.model
                , result = config.result
                , logoutMsg = config.logoutMsg
                , locale = config.locale
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
