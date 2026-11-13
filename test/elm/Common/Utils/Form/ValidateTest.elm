module Common.Utils.Form.ValidateTest exposing (suite)

import Common.Utils.Form.FormError exposing (FormError(..))
import Common.Utils.Form.Validate as Validate
import Expect
import Form
import Form.Error exposing (ErrorValue(..))
import Form.Field as Field
import Form.Validate as V
import Gettext
import Test exposing (Test, describe, test)


{-| A new password is refused as too weak with the error the server's verdict
has, so the field shows both the same way: through its meter.
-}
suite : Test
suite =
    describe "Common.Utils.Form.Validate"
        [ passwordTests
        , maybeColorTests
        ]


maybeColorTests : Test
maybeColorTests =
    describe "maybeColor"
        [ test "empty is no color" <|
            \_ -> validateColor "" |> Expect.equal (Ok Nothing)
        , test "a six-digit hex color passes" <|
            \_ -> validateColor "#8E44AD" |> Expect.equal (Ok (Just "#8E44AD"))
        , test "a three-digit hex color passes" <|
            \_ -> validateColor "#fff" |> Expect.equal (Ok (Just "#fff"))
        , test "anything else is refused" <|
            \_ -> validateColor "not-a-color" |> Expect.equal (Err (Just (CustomError InvalidColor)))
        , test "a hex color without # is refused" <|
            \_ -> validateColor "8E44AD" |> Expect.equal (Err (Just (CustomError InvalidColor)))
        ]


validateColor : String -> Result (Maybe (ErrorValue FormError)) (Maybe String)
validateColor value =
    let
        validation =
            V.field "color" Validate.maybeColor

        form =
            Form.initial [ ( "color", Field.string value ) ] validation
                |> Form.update validation Form.Submit
    in
    case Form.getOutput form of
        Just output ->
            Ok output

        Nothing ->
            Err (Form.getFieldAsString "color" form).liveError


passwordTests : Test
passwordTests =
    describe "password"
        [ test "a weak password is too weak, where the browser judges it" <|
            \_ ->
                validate False "abc"
                    |> Expect.equal (Err (Just (CustomError PasswordTooWeak)))
        , test "a strong one passes" <|
            \_ ->
                validate False "7Qw!pLz2vR#hT4xM"
                    |> Expect.equal (Ok "7Qw!pLz2vR#hT4xM")
        , test "any passes where the server judges it" <|
            \_ ->
                validate True "abc"
                    |> Expect.equal (Ok "abc")
        ]


validate : Bool -> String -> Result (Maybe (ErrorValue FormError)) String
validate checkedByServer value =
    let
        validation =
            V.field "password" (Validate.password { locale = Gettext.defaultLocale, extensions = { passwordStrengthCheck = checkedByServer } })

        form =
            Form.initial [ ( "password", Field.string value ) ] validation
                |> Form.update validation Form.Submit
    in
    case Form.getOutput form of
        Just output ->
            Ok output

        Nothing ->
            Err (Form.getFieldAsString "password" form).liveError
