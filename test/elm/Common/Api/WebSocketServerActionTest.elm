module Common.Api.WebSocketServerActionTest exposing (webSocketServerActionTests)

import Common.Api.Models.WebSockets.WebSocketServerAction as WebSocketServerAction exposing (WebSocketServerAction(..))
import Common.Api.ServerError as ServerError
import Dict
import Expect
import Json.Decode as D
import Test exposing (Test, describe, test)


decode : String -> Result D.Error (WebSocketServerAction ())
decode =
    D.decodeString (WebSocketServerAction.decoder (D.succeed ()))


webSocketServerActionTests : Test
webSocketServerActionTests =
    describe "WebSocketServerAction"
        [ test "a rejected event is a form error, field by field" <|
            \_ ->
                decode
                    """
                    {"data":{"fieldErrors":{"parentUuid":[{"code":"error.validation.km_event_entity_absence","defaultMessage":"'%s' is not an existing %s","params":["00000000-0000-0000-0000-000000000000","knowledge model"]}]},"formErrors":[{"code":"error.validation.km_event_rejected","defaultMessage":"The event '%s' was rejected; reload the editor","params":["47679be7-01c7-4213-ba95-2a53da329ecf"]}],"status":400,"type":"UserFormError"},"type":"Error_ServerAction"}
                    """
                    |> Expect.equal
                        (Ok
                            (Error
                                (ServerError.UserFormError
                                    { formErrors = [ { code = "error.validation.km_event_rejected", params = [ "47679be7-01c7-4213-ba95-2a53da329ecf" ] } ]
                                    , fieldErrors =
                                        Dict.fromList
                                            [ ( "parentUuid", [ { code = "error.validation.km_event_entity_absence", params = [ "00000000-0000-0000-0000-000000000000", "knowledge model" ] } ] ) ]
                                    }
                                )
                            )
                        )
        , test "a simple error carries its code" <|
            \_ ->
                decode
                    """
                    {"data":{"error":{"code":"error.api.websocket.cant_deserialize_obj","defaultMessage":"Deserialization failed","params":[]},"status":400,"type":"UserSimpleError"},"type":"Error_ServerAction"}
                    """
                    |> Expect.equal (Ok (Error (ServerError.UserSimpleError { code = "error.api.websocket.cant_deserialize_obj", params = [] })))
        , test "an error with nothing but a code is still an error" <|
            \_ ->
                decode """{"data":{"message":{"code":"error.service.qtn.collaboration.force_disconnect"}},"type":"Error_ServerAction"}"""
                    |> Expect.equal (Ok (Error (ServerError.UserSimpleError { code = "error.service.qtn.collaboration.force_disconnect", params = [] })))
        ]
