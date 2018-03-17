module Common.Form exposing (..)

import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Http exposing (Error(BadStatus), Response)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)


type CustomFormError
    = ConfirmationError
    | ServerValidationError String


type alias Error =
    { message : String
    , fieldErrors : List ( String, String )
    }


errorDecoder : Decoder Error
errorDecoder =
    decode Error
        |> required "message" Decode.string
        |> required "fieldErrors" (Decode.list <| fieldErrorDecoder)


fieldErrorDecoder : Decoder ( String, String )
fieldErrorDecoder =
    Decode.map2 (,) (index 0 Decode.string) (index 1 Decode.string)


setFormErrors : Http.Error -> Form CustomFormError a -> Form CustomFormError a
setFormErrors rawError form =
    case decodeError rawError of
        Just error ->
            List.foldl setFormError form error.fieldErrors

        _ ->
            form


decodeError : Http.Error -> Maybe Error
decodeError error =
    case error of
        BadStatus response ->
            case decodeString errorDecoder response.body of
                Ok error ->
                    Just error

                _ ->
                    Nothing

        _ ->
            Nothing


getErrorMessage : Http.Error -> String -> String
getErrorMessage error defaultMessage =
    case decodeError error of
        Just error ->
            error.message

        Nothing ->
            defaultMessage


setFormError : ( String, String ) -> Form CustomFormError a -> Form CustomFormError a
setFormError fieldError form =
    Form.update (createFieldValidation fieldError) Form.Validate form


createFieldValidation : ( String, String ) -> Validation CustomFormError a
createFieldValidation ( fieldName, fieldError ) =
    Validate.field fieldName (Validate.fail (customError (ServerValidationError fieldError)))
