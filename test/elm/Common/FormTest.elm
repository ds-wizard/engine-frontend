module Common.FormTest exposing (suite)

import Common.Components.Form.Field as Field
import Common.Utils.Form.FormError exposing (FormError(..))
import Expect
import Form exposing (Form)
import Form.Error exposing (ErrorValue(..))
import Form.Field
import Form.Validate as V exposing (Validation)
import Gettext
import Test exposing (Test, describe, test)
import Version


{-| The adapter's own decisions: the id a path becomes, the path a list item
has, and which words a form error turns into — and when.

The last is the one that matters most. An error that shows while someone is
still typing, or never shows before a submit, is the adapter getting elm-form's
timing wrong, and nothing on screen says so.

-}
suite : Test
suite =
    describe "Common.Components.Form"
        [ fieldIdTests
        , listItemPathTests
        , errorTextTests
        , timingTests
        , versionTests
        ]


appState : { locale : Gettext.Locale }
appState =
    { locale = Gettext.defaultLocale }


fieldIdTests : Test
fieldIdTests =
    describe "fieldId"
        [ test "prefixes a plain path" <|
            \_ ->
                Field.fieldId "email"
                    |> Expect.equal "form-email"
        , test "turns the separators of a list path into hyphens" <|
            \_ ->
                Field.fieldId "parameters.0.name"
                    |> Expect.equal "form-parameters-0-name"
        , test "turns brackets into hyphens without leaving one at the end" <|
            \_ ->
                Field.fieldId "services[2].url"
                    |> Expect.equal "form-services-2-url"
        , test "keeps hyphens and underscores that are part of a name" <|
            \_ ->
                Field.fieldId "client_secret-visible__"
                    |> Expect.equal "form-client_secret-visible__"
        ]


listItemPathTests : Test
listItemPathTests =
    describe "listItemPath"
        [ test "names a field inside an item" <|
            \_ ->
                Field.listItemPath "parameters" 3 "value"
                    |> Expect.equal "parameters.3.value"
        , test "names the item itself when there is no field" <|
            \_ ->
                Field.listItemPath "projectTags" 0 ""
                    |> Expect.equal "projectTags.0"
        , test "nests" <|
            \_ ->
                Field.listItemPath (Field.listItemPath "services" 1 "headers") 0 "key"
                    |> Expect.equal "services.1.headers.0.key"
        , test "the secret's helper field is ignored as a change" <|
            \_ ->
                Field.secretRevealedPath "clientSecret"
                    |> String.endsWith "__"
                    |> Expect.equal True
        ]


errorTextTests : Test
errorTextTests =
    let
        message value error =
            Field.errorText appState "Name" (state value (Just error))
    in
    describe "errorText"
        [ test "says nothing without a live error" <|
            \_ ->
                Field.errorText appState "Name" (state (Just "x") Nothing)
                    |> Expect.equal Nothing
        , test "Empty" <|
            \_ ->
                message Nothing Empty
                    |> Expect.equal (Just "Name cannot be empty.")
        , test "InvalidString" <|
            \_ ->
                message (Just "") InvalidString
                    |> Expect.equal (Just "Name cannot be empty.")
        , test "an empty number field is empty, not an invalid number" <|
            \_ ->
                message (Just "") InvalidInt
                    |> Expect.equal (Just "Name cannot be empty.")
        , test "InvalidInt" <|
            \_ ->
                message (Just "1e") InvalidInt
                    |> Expect.equal (Just "This is not a valid number.")
        , test "InvalidFloat" <|
            \_ ->
                message (Just "x") InvalidFloat
                    |> Expect.equal (Just "This is not a valid number.")
        , test "InvalidEmail" <|
            \_ ->
                message (Just "a@") InvalidEmail
                    |> Expect.equal (Just "This is not a valid email.")
        , test "SmallerIntThan" <|
            \_ ->
                message (Just "0") (SmallerIntThan 1)
                    |> Expect.equal (Just "This should not be less than 1.")
        , test "GreaterIntThan" <|
            \_ ->
                message (Just "9") (GreaterIntThan 5)
                    |> Expect.equal (Just "This should not be more than 5.")
        , test "SmallerFloatThan" <|
            \_ ->
                message (Just "0") (SmallerFloatThan 0.5)
                    |> Expect.equal (Just "This should not be less than 0.5.")
        , test "GreaterFloatThan" <|
            \_ ->
                message (Just "9") (GreaterFloatThan 2.5)
                    |> Expect.equal (Just "This should not be more than 2.5.")
        , test "ConfirmationError" <|
            \_ ->
                message (Just "a") (CustomError ConfirmationError)
                    |> Expect.equal (Just "Passwords do not match!")
        , test "InvalidUuid" <|
            \_ ->
                message (Just "a") (CustomError InvalidUuid)
                    |> Expect.equal (Just "This is not a valid UUID.")
        , test "PasswordTooWeak" <|
            \_ ->
                message (Just "a") (CustomError PasswordTooWeak)
                    |> Expect.equal (Just "This password is not strong enough.")
        , test "ServerValidationError is the server's words" <|
            \_ ->
                message (Just "a") (CustomError (ServerValidationError "Already taken."))
                    |> Expect.equal (Just "Already taken.")
        , test "Error is the validation's words" <|
            \_ ->
                message (Just "a") (CustomError (Error "Use lowercase."))
                    |> Expect.equal (Just "Use lowercase.")
        , test "anything else is invalid" <|
            \_ ->
                message (Just "a") NotIncludedIn
                    |> Expect.equal (Just "Invalid value.")
        ]


timingTests : Test
timingTests =
    let
        emailError form =
            Field.errorText appState "Email" (Form.getFieldAsString "email" form)

        typed value =
            Form.update validation (Form.Input "email" Form.Text (Form.Field.String value)) initial
    in
    describe "when an error shows"
        [ test "not before anything has happened" <|
            \_ ->
                emailError initial
                    |> Expect.equal Nothing
        , test "not while the field is being typed into" <|
            \_ ->
                typed "a@"
                    |> emailError
                    |> Expect.equal Nothing
        , test "once focus leaves the field" <|
            \_ ->
                typed "a@"
                    |> Form.update validation (Form.Blur "email")
                    |> emailError
                    |> Expect.equal (Just "This is not a valid email.")
        , test "on submit, for a field nobody touched" <|
            \_ ->
                initial
                    |> Form.update validation Form.Submit
                    |> emailError
                    |> Expect.equal (Just "Email cannot be empty.")
        ]


state : Maybe String -> Maybe (ErrorValue FormError) -> Form.FieldState FormError String
state value liveError =
    { path = "name"
    , value = value
    , error = liveError
    , liveError = liveError
    , isDirty = False
    , isChanged = False
    , hasFocus = False
    }


type alias Signup =
    { email : String }


initial : Form FormError Signup
initial =
    Form.initial [] validation


validation : Validation FormError Signup
validation =
    V.succeed Signup
        |> V.andMap (V.field "email" V.email)


{-| The version group's arithmetic: which versions are offered, and that choosing
one fills in all three numbers.
-}
versionTests : Test
versionTests =
    let
        fields =
            { majorField = "major", minorField = "minor", patchField = "patch" }

        versionForm =
            Form.initial [] versionValidation

        values form =
            List.map (\path -> (Form.getFieldAsString path form).value) [ "major", "minor", "patch" ]
    in
    describe "version"
        [ test "suggests the next major, minor and patch" <|
            \_ ->
                Field.versionSuggestions (Just (Version.create 2 3 4))
                    |> List.map (\s -> ( s.label, ( s.major, s.minor, s.patch ) ))
                    |> Expect.equal
                        [ ( "3.0.0", ( 3, 0, 0 ) )
                        , ( "2.4.0", ( 2, 4, 0 ) )
                        , ( "2.3.5", ( 2, 3, 5 ) )
                        ]
        , test "suggests the first versions when there is none yet" <|
            \_ ->
                Field.versionSuggestions Nothing
                    |> List.map .label
                    |> Expect.equal [ "1.0.0", "0.1.0", "0.0.1" ]
        , test "setVersion fills in all three fields" <|
            \_ ->
                versionForm
                    |> Field.setVersion versionValidation fields { label = "1.2.3", major = 1, minor = 2, patch = 3 }
                    |> values
                    |> Expect.equal [ Just "1", Just "2", Just "3" ]
        , test "setVersion makes the form valid when the numbers are" <|
            \_ ->
                versionForm
                    |> Field.setVersion versionValidation fields { label = "0.0.1", major = 0, minor = 0, patch = 1 }
                    |> Form.getOutput
                    |> Expect.equal (Just ( 0, 0, 1 ))
        ]


versionValidation : Validation FormError ( Int, Int, Int )
versionValidation =
    V.map3 (\major minor patch -> ( major, minor, patch ))
        (V.field "major" V.int)
        (V.field "minor" V.int)
        (V.field "patch" V.int)
