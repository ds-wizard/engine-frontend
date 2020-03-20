module Wizard.Common.Form.Validate exposing
    ( confirmation
    , maybeString
    , regex
    , uuid
    )

import Form.Error as Error exposing (Error, ErrorValue(..))
import Form.Validate as V exposing (Validation)
import Regex exposing (Regex)
import Wizard.Common.Form exposing (CustomFormError(..))


confirmation : String -> Validation CustomFormError String -> Validation CustomFormError String
confirmation confirmationField =
    let
        validate original =
            V.field confirmationField
                (V.string
                    |> V.andThen
                        (\conf ->
                            if original == conf then
                                V.succeed conf

                            else
                                V.fail (V.customError ConfirmationError)
                        )
                )
    in
    V.andThen validate


maybeString : Validation CustomFormError (Maybe String)
maybeString =
    V.oneOf [ V.emptyString |> V.map (\_ -> Nothing), V.string |> V.map Just ]


regex : String -> Validation e String
regex r =
    V.string
        |> V.andThen
            (\s -> V.format (createRegex r) s |> V.mapError (\_ -> Error.value InvalidFormat))


uuid : Validation CustomFormError String
uuid =
    validateRegexWithCustomError uuidPattern InvalidUuid


validateRegexWithCustomError : Regex -> CustomFormError -> Validation CustomFormError String
validateRegexWithCustomError r customFormError =
    V.string
        |> V.andThen
            (\s ->
                V.format r s
                    |> V.mapError (\_ -> V.customError customFormError)
            )


uuidPattern : Regex
uuidPattern =
    let
        pattern =
            "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"

        options =
            { caseInsensitive = True, multiline = False }
    in
    Maybe.withDefault Regex.never <| Regex.fromStringWith options pattern


createRegex : String -> Regex
createRegex =
    Maybe.withDefault Regex.never << Regex.fromString
