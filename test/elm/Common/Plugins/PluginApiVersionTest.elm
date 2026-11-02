module Common.Plugins.PluginApiVersionTest exposing (suite)

import Common.Plugins.Plugin as Plugin
import Expect
import Test exposing (Test, describe, test)
import Version exposing (Version)


supported : Version -> Bool
supported version =
    Plugin.isApiVersionSupported { pluginApiVersion = version }


suite : Test
suite =
    describe "Common.Plugins.Plugin.isApiVersionSupported (API 0.5.0)"
        [ test "a plugin of this API is taken" <|
            \_ -> Expect.equal True (supported (Version.create 0 5 0))
        , test "an older minor before 1.0 is not: its data had another shape" <|
            \_ -> Expect.equal False (supported (Version.create 0 4 0))
        , test "a newer one is not" <|
            \_ -> Expect.equal False (supported (Version.create 0 5 1))
        , test "another major is not" <|
            \_ -> Expect.equal False (supported (Version.create 1 0 0))
        ]
