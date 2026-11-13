module Common.Utils.Form.Validate exposing
    ( authenticationServiceId
    , confirmation
    , dict
    , documentTemplateId
    , ifElse
    , kmId
    , kmSecret
    , localeId
    , maybeColor
    , maybeString
    , optionalInt
    , optionalString
    , password
    , projectTag
    , projectTags
    , url
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


{-| A new password. Where the server judges its strength
(`Extensions.passwordStrengthCheck`), any is let through, and the server's
answer is what stops a weak one: a verdict worked out here would disagree with
it both ways. Otherwise a very weak or weak one fails here, with the same
`FormError.PasswordTooWeak` as the server's verdict, so the field shows it the
same way: through its meter, which says it already, not a message under it.
-}
password : { a | locale : Gettext.Locale, extensions : { b | passwordStrengthCheck : Bool } } -> Validation FormError String
password appState =
    V.string
        |> V.andThen
            (\value ->
                case localStrength appState.extensions.passwordStrengthCheck value of
                    Just Rumkin.VeryWeak ->
                        V.fail (V.customError FormError.PasswordTooWeak)

                    Just Rumkin.Weak ->
                        V.fail (V.customError FormError.PasswordTooWeak)

                    _ ->
                        V.succeed value
            )


localStrength : Bool -> String -> Maybe Rumkin.Strength
localStrength checkedByServer value =
    if checkedByServer then
        Nothing

    else
        Just (Rumkin.getStats value).strength


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


{-| An optional hex color code: empty is `Nothing`, anything else has to be
`#` with three or six hex digits.
-}
maybeColor : Validation FormError (Maybe String)
maybeColor =
    V.oneOf
        [ V.map (\_ -> Nothing) <| V.emptyString
        , V.map Just <| validateRegexWithCustomError RegexPatterns.color InvalidColor
        ]


uuid : Validation FormError Uuid
uuid =
    validateRegexWithCustomError RegexPatterns.uuid InvalidUuid
        |> V.map Uuid.fromUuidString


url : { a | locale : Gettext.Locale } -> Validation FormError String
url appState =
    regex RegexPatterns.url (gettext "Fill in a valid URL." appState.locale)


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
