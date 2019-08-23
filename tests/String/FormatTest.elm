module String.FormatTest exposing (formatHtmlTests, formatTests)

import Expect
import Html exposing (a, i, strong, text)
import String.Format exposing (format, formatHtml)
import Test exposing (Test, describe)
import TestUtils exposing (parametrized)


formatTests : Test
formatTests =
    describe "String Format"
        [ parametrized
            [ ( "My String", [], "My String" )
            , ( "My %s String", [ "Super" ], "My Super String" )
            , ( "My String %s", [ "Super" ], "My String Super" )
            , ( "%s My String", [ "Super" ], "Super My String" )
            , ( "My %%s String", [ "Super" ], "My %s String" )
            , ( "My %s String", [], "My %s String" )
            , ( "My %s String %s %s", [ "Super", "abc", "123" ], "My Super String abc 123" )
            , ( "My %s String %s %s", [ "Super", "abc" ], "My Super String abc %s" )
            ]
            "works"
          <|
            \( str, terms, expected ) -> Expect.equal expected <| format str terms
        ]


formatHtmlTests : Test
formatHtmlTests =
    describe "String Format HTML"
        [ parametrized
            [ ( "My String", [], [ text "My String" ] )
            , ( "My %h String", [ strong [] [ text "Super" ] ], [ text "My ", strong [] [ text "Super" ], text " String" ] )
            , ( "My String %h", [ strong [] [ text "Super" ] ], [ text "My String ", strong [] [ text "Super" ] ] )
            , ( "%h My String", [ strong [] [ text "Super" ] ], [ strong [] [ text "Super" ], text " My String" ] )
            , ( "My %%h String", [ strong [] [ text "Super" ] ], [ text "My %h String" ] )
            , ( "My %h String", [], [ text "My %h String" ] )
            , ( "My %h String %h %h", [ strong [] [ text "Super" ], i [] [ text "abc" ], a [] [ text "123" ] ], [ text "My ", strong [] [ text "Super" ], text " String ", i [] [ text "abc" ], text " ", a [] [ text "123" ] ] )
            , ( "My %h String %h %h", [ strong [] [ text "Super" ], i [] [ text "abc" ] ], [ text "My ", strong [] [ text "Super" ], text " String ", i [] [ text "abc" ], text " %h" ] )
            ]
            "works"
          <|
            \( str, elements, expected ) -> Expect.equal expected <| formatHtml str elements
        ]
