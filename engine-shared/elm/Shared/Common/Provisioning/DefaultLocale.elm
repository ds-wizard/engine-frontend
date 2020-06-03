module Shared.Common.Provisioning.DefaultLocale exposing (..)


locale : List ( String, String )
locale =
    [ ( "Shared.Form.error.confirmationError", "Passwords don't match" )
    , ( "Shared.Form.error.default", "Invalid value" )
    , ( "Shared.Form.error.empty", "%s cannot be empty" )
    , ( "Shared.Form.error.greaterFloatThan", "This should not be more than %s" )
    , ( "Shared.Form.error.integrationIdAlreadyUsed", "This integration ID is already used for different integration" )
    , ( "Shared.Form.error.invalidEmail", "This is not a valid email" )
    , ( "Shared.Form.error.invalidFloat", "This is not a valid number" )
    , ( "Shared.Form.error.invalidString", "%s cannot be empty" )
    , ( "Shared.Form.error.invalidUuid", "This is not a valid UUID" )
    , ( "Shared.Form.error.smallerFloatThan", "This should not be less than %s" )
    ]
