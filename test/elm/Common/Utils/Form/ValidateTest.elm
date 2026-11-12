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
    describe "Common.Utils.Form.Validate.password"
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
