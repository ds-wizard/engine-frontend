module Common.Utils.Form.Validate exposing
    ( authenticationServiceId
    , confirmation
    , dict
    , documentTemplateId
    , ifElse
    , kmId
    , kmSecret
    , localeId
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

import Common.Utils.Form.FormError as FormError exposing (FormError(..))
import Common.Utils.RegexPatterns as RegexPatterns
import Dict exposing (Dict)
import Form.Error as Error exposing (ErrorValue(..))
import Form.Validate as V exposing (Validation)
import Gettext exposing (gettext)
import Regex exposing (Regex)
import Rumkin
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


organizationId : { a | locale : Gettext.Locale } -> Validation FormError String
organizationId appState =
    regex RegexPatterns.organizationId (gettext "Fill in a valid organization ID." appState.locale)


kmId : { a | locale : Gettext.Locale } -> Validation FormError String
kmId appState =
    regex RegexPatterns.kmId (gettext "Fill in a valid knowledge model ID." appState.locale)


kmSecret : { a | locale : Gettext.Locale } -> Validation FormError String
kmSecret appState =
    regex RegexPatterns.kmSecret (gettext "Fill in a valid knowledge model secret name." appState.locale)


documentTemplateId : { a | locale : Gettext.Locale } -> Validation FormError String
documentTemplateId appState =
    regex RegexPatterns.documentTemplateId (gettext "Fill in a valid document template ID." appState.locale)


localeId : { a | locale : Gettext.Locale } -> Validation FormError String
localeId appState =
    regex RegexPatterns.localeId (gettext "Fill in a valid locale ID." appState.locale)


authenticationServiceId : { a | locale : Gettext.Locale } -> Validation FormError String
authenticationServiceId appState =
    regex RegexPatterns.authenticationServiceId (gettext "Use only lowercase alphanumeric characters or dash symbols." appState.locale)


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
