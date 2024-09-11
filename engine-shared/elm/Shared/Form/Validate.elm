module Shared.Form.Validate exposing
    ( confirmation
    , dict
    , ifElse
    , kmId
    , maybeInt
    , maybeString
    , optionalInt
    , optionalString
    , organizationId
    , password
    , projectTag
    , projectTags
    , uuid
    , versionNumber
    )

import Dict exposing (Dict)
import Form.Error as Error exposing (ErrorValue(..))
import Form.Validate as V exposing (Validation)
import Gettext exposing (gettext)
import Regex exposing (Regex)
import Rumkin
import Shared.Form.FormError as FormError exposing (FormError(..))
import Shared.RegexPatterns as RegexPatterns
import Uuid exposing (Uuid)


confirmation : String -> Validation FormError String -> Validation FormError String
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


password : { a | locale : Gettext.Locale } -> Validation FormError String
password appState =
    V.string
        |> V.andThen
            (\value ->
                case (Rumkin.getStats value).strength of
                    Rumkin.VeryWeak ->
                        V.fail (V.customError (FormError.Error (gettext "This is a very weak password." appState.locale)))

                    Rumkin.Weak ->
                        V.fail (V.customError (FormError.Error (gettext "This is a weak password." appState.locale)))

                    _ ->
                        V.succeed value
            )


ifElse :
    String
    -> Validation e a
    -> Validation e a
    -> Validation e Bool
    -> Validation e a
ifElse dependentField trueValidation falseValidation =
    let
        validate main =
            if main then
                V.field dependentField trueValidation

            else
                V.field dependentField falseValidation
    in
    V.andThen validate


maybeString : Validation e (Maybe String)
maybeString =
    V.oneOf [ V.emptyString |> V.map (\_ -> Nothing), V.string |> V.map Just ]


maybeInt : Validation e (Maybe Int)
maybeInt =
    V.oneOf [ V.emptyString |> V.map (\_ -> Nothing), V.int |> V.map Just ]


optionalString : Validation e String
optionalString =
    V.oneOf [ V.emptyString, V.string ]


optionalInt : Validation e Int
optionalInt =
    V.oneOf [ V.emptyString |> V.map (\_ -> 0), V.int ]


regex : Regex -> String -> Validation FormError String
regex r error =
    V.string
        |> V.andThen
            (\s -> V.format r s |> V.mapError (\_ -> Error.value (CustomError (FormError.Error error))))


maybeRegex : Regex -> String -> Validation FormError (Maybe String)
maybeRegex r error =
    V.oneOf
        [ V.map (\_ -> Nothing) <| V.emptyString
        , V.map Just <| validateRegexWithCustomError r (FormError.Error error)
        ]


uuid : Validation FormError Uuid
uuid =
    validateRegexWithCustomError RegexPatterns.uuid InvalidUuid
        |> V.map Uuid.fromUuidString


organizationId : Validation FormError String
organizationId =
    regex RegexPatterns.organizationId "Organization ID can only contain alphanumeric characters and dots. It must start and end with an alphanumeric character."


kmId : Validation FormError String
kmId =
    regex RegexPatterns.kmId "Knowledge Model ID can only contain alphanumeric characters and hyphens. It must start and end with an alphanumeric character."


projectTag : { a | locale : Gettext.Locale } -> Validation FormError String
projectTag appState =
    validateRegexWithCustomError RegexPatterns.projectTag (FormError.Error (gettext "Comma (,) is not allowed in project tags." appState.locale))


projectTags : { a | locale : Gettext.Locale } -> Validation FormError (Maybe String)
projectTags appState =
    maybeRegex RegexPatterns.projectTag (gettext "Comma (,) is not allowed in project tags." appState.locale)


validateRegexWithCustomError : Regex -> e -> Validation e String
validateRegexWithCustomError r customFormError =
    V.string
        |> V.andThen
            (\s ->
                V.format r s
                    |> V.mapError (\_ -> V.customError customFormError)
            )


dict : Validation e a -> Validation e (Dict String a)
dict valueValidation =
    let
        validateEntry =
            V.succeed Tuple.pair
                |> V.andMap (V.field "key" V.string)
                |> V.andMap (V.field "value" valueValidation)
    in
    V.map Dict.fromList <| V.list validateEntry


versionNumber : Validation e Int
versionNumber =
    V.int |> V.andThen (V.minInt 0)
