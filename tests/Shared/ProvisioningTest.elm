module Shared.ProvisioningTest exposing (foldlTests, mergeTests)

import Dict
import Expect
import Shared.Provisioning as Provisioning exposing (Provisioning)
import Test exposing (Test, describe)
import TestUtils exposing (parametrized)


iconKey : String
iconKey =
    "menu.home"


withIconSet1 : Provisioning
withIconSet1 =
    { locale = Dict.empty
    , iconSet = Dict.fromList [ ( iconKey, "fas fa-home" ) ]
    }


withIconSet2 : Provisioning
withIconSet2 =
    { locale = Dict.empty
    , iconSet = Dict.fromList [ ( iconKey, "fas fa-building" ) ]
    }


withIconSet3 : Provisioning
withIconSet3 =
    { locale = Dict.empty
    , iconSet = Dict.fromList [ ( iconKey, "fas fa-warehouse" ) ]
    }


localeKey : String
localeKey =
    "Wizard.Common.View.Layout.header.logIn"


withLocale1 : Provisioning
withLocale1 =
    { locale = Dict.fromList [ ( localeKey, "Log in" ) ]
    , iconSet = Dict.empty
    }


withLocale2 : Provisioning
withLocale2 =
    { locale = Dict.fromList [ ( localeKey, "Přihlásit se" ) ]
    , iconSet = Dict.empty
    }


withLocale3 : Provisioning
withLocale3 =
    { locale = Dict.fromList [ ( localeKey, "登录" ) ]
    , iconSet = Dict.empty
    }


mergeTests : Test
mergeTests =
    describe "merge"
        [ parametrized
            [ ( withIconSet1, Provisioning.default, "fas fa-home" )
            , ( Provisioning.default, withIconSet1, "fas fa-home" )
            , ( withIconSet1, withIconSet2, "fas fa-home" )
            , ( withIconSet2, withIconSet1, "fas fa-building" )
            ]
            "iconSet"
          <|
            \( p1, p2, expected ) ->
                let
                    merged =
                        Provisioning.merge p1 p2
                in
                Expect.equal (Dict.get iconKey merged.iconSet) (Just expected)
        , parametrized
            [ ( withLocale1, Provisioning.default, "Log in" )
            , ( Provisioning.default, withLocale1, "Log in" )
            , ( withLocale1, withLocale2, "Log in" )
            , ( withLocale2, withLocale1, "Přihlásit se" )
            ]
            "locale"
          <|
            \( p1, p2, expected ) ->
                let
                    merged =
                        Provisioning.merge p1 p2
                in
                Expect.equal (Dict.get localeKey merged.locale) (Just expected)
        ]


foldlTests : Test
foldlTests =
    describe "foldl"
        [ parametrized
            [ ( [ withIconSet1, withIconSet2, withIconSet3 ], "fas fa-warehouse" )
            , ( [ withIconSet1, withIconSet3, withIconSet2 ], "fas fa-building" )
            , ( [ withIconSet3, withIconSet2, withIconSet1 ], "fas fa-home" )
            ]
            "iconSet"
          <|
            \( provisionings, expected ) ->
                let
                    result =
                        Provisioning.foldl provisionings
                in
                Expect.equal (Dict.get iconKey result.iconSet) (Just expected)
        , parametrized
            [ ( [ withLocale1, withLocale2, withLocale3 ], "登录" )
            , ( [ withLocale3, withLocale1, withLocale2 ], "Přihlásit se" )
            , ( [ withIconSet2, withIconSet3, withLocale1 ], "Log in" )
            ]
            "locale"
          <|
            \( provisionings, expected ) ->
                let
                    result =
                        Provisioning.foldl provisionings
                in
                Expect.equal (Dict.get localeKey result.locale) (Just expected)
        ]
