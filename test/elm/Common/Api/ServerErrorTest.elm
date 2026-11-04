module Common.Api.ServerErrorTest exposing (serverErrorTests)

import Common.Api.ServerError as ServerError
import Expect
import Gettext
import Test exposing (Test, describe, test)


toText : String -> String -> List String -> Maybe String
toText code defaultMessage params =
    ServerError.messageToText { locale = Gettext.defaultLocale }
        { code = code, defaultMessage = defaultMessage, params = params }


serverErrorTests : Test
serverErrorTests =
    describe "ServerError.messageToText"
        [ test "a message we know is in the user's language" <|
            \_ ->
                toText "error.service.workspace.not_member" "The user is not a member of the workspace" []
                    |> Expect.equal (Just "The user is not a member of the workspace.")
        , test "a message we do not know is in the server's words" <|
            \_ ->
                toText "error.service.something.new" "Something ('%s') went wrong" [ "this" ]
                    |> Expect.equal (Just "Something ('this') went wrong")
        , test "a message with no words is nothing" <|
            \_ ->
                toText "error.service.something.new" "" []
                    |> Expect.equal Nothing
        ]
