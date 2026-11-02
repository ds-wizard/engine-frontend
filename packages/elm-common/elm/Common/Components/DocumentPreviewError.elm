module Common.Components.DocumentPreviewError exposing
    ( DocumentPreviewError
    , fromApiError
    , view
    )

{-| A preview that could not be made: a project's, or a document template's in
its editor.

When the template fails, the server says so with the template engine's own
words in the error's parameters — the line, the expression, the traceback.
Those are what the author needs to fix it, and they are code, so they are shown
as code under the sentence rather than run into it.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.ServerError as ServerError
import Common.Data.Context exposing (WithLocale)
import Gettext exposing (gettext)
import Html exposing (Html)
import String.Format as String
import Ui.CodeBlock as CodeBlock
import Ui.Illustration as Illustration
import Ui.Page.Message as Message


type alias DocumentPreviewError =
    { message : String
    , log : Maybe String
    }


{-| The sentence, and the log when the server sent one. Any other error is the
sentence `ApiError.toActionResult` makes of it, or the default.
-}
fromApiError : WithLocale a -> String -> ApiError -> DocumentPreviewError
fromApiError appState defaultMessage apiError =
    case ApiError.toServerError apiError of
        Just (ServerError.SystemLogError data) ->
            if List.isEmpty data.params then
                { message = String.format data.defaultMessage data.params, log = Nothing }

            else
                { message = gettext "The document could not be generated." appState.locale
                , log = Just (String.join "\n\n" data.params)
                }

        _ ->
            case ApiError.toActionResult appState defaultMessage apiError of
                Error message ->
                    { message = message, log = Nothing }

                _ ->
                    { message = defaultMessage, log = Nothing }


{-| `Common.Components.Page.errorWithinPage`, with the log under it.
-}
view : WithLocale a -> DocumentPreviewError -> Html msg
view appState error =
    let
        withLog message =
            case error.log of
                Just log ->
                    Message.withDetails
                        (CodeBlock.toHtml
                            { label = gettext "Error details" appState.locale
                            , language = "text"
                            , value = log
                            }
                        )
                        message

                Nothing ->
                    message
    in
    Message.message
        { title = gettext "Error" appState.locale
        , description = error.message
        }
        |> Message.withIllustration (Illustration.toHtml Illustration.error)
        |> withLog
        |> Message.withinPage
        |> Message.announced
        |> Message.toHtml
