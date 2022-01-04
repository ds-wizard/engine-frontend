module Shared.Form.Validate exposing
    ( confirmation
    , dict
    , ifElse
    , kmId
    , maybeRegex
    , maybeString
    , optionalString
    , organizationId
    , projectTag
    , projectTags
    , regex
    , uuid
    , uuidString
    )

import Dict exposing (Dict)
import Form.Error as Error exposing (ErrorValue(..))
import Form.Validate as V exposing (Validation)
import Regex exposing (Regex)
import Shared.Form.FormError as FormError exposing (FormError(..))
import Shared.Locale exposing (l)
import Shared.Provisioning exposing (Provisioning)
import Uuid exposing (Uuid)


l_ : String -> { a | provisioning : Provisioning } -> String
l_ =
    l "Shared.Form.Validate"


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


regex : String -> Validation e String
regex r =
    V.string
        |> V.andThen
            (\s -> V.format (createRegex r) s |> V.mapError (\_ -> Error.value InvalidFormat))


maybeRegex : String -> String -> Validation FormError (Maybe String)
maybeRegex r error =
    V.oneOf
        [ V.map (\_ -> Nothing) <| V.emptyString
        , V.map Just <| validateRegexWithCustomError (createRegex r) (FormError.Error error)
        ]


uuidString : Validation FormError String
uuidString =
    validateRegexWithCustomError uuidPattern InvalidUuid


uuid : Validation FormError Uuid
uuid =
    validateRegexWithCustomError uuidPattern InvalidUuid
        |> V.map Uuid.fromUuidString


organizationId : Validation e String
organizationId =
    regex "^^(?![.])(?!.*[.]$)[a-zA-Z0-9.]+$"


kmId : Validation e String
kmId =
    regex "^^(?![-])(?!.*[-]$)[a-zA-Z0-9-]+$"


projectTag : { a | provisioning : Provisioning } -> Validation FormError String
projectTag appState =
    validateRegexWithCustomError (createRegex "^[^,]+$") (FormError.Error (l_ "projectTagError" appState))


projectTags : { a | provisioning : Provisioning } -> Validation FormError (Maybe String)
projectTags appState =
    maybeRegex "^[^,]+$" (l_ "projectTagError" appState)


validateRegexWithCustomError : Regex -> e -> Validation e String
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


dict : Validation e a -> Validation e (Dict String a)
dict valueValidation =
    let
        validateEntry =
            V.succeed Tuple.pair
                |> V.andMap (V.field "key" V.string)
                |> V.andMap (V.field "value" valueValidation)
    in
    V.map Dict.fromList <| V.list validateEntry
