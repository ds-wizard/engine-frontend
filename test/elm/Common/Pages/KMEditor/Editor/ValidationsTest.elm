module Common.Pages.KMEditor.Editor.ValidationsTest exposing (suite)

{-| Which rules a value question offers, and what survives a change of type.

The rule about hidden validations is the one worth pinning: the wizard drops a
rule the current type cannot use as soon as another is added, without saying
so. Here the list is carried whole, so a type changed by mistake and changed
back loses nothing.

-}

import Common.Api.Models.KnowledgeModel.Question.QuestionValidation as QuestionValidation exposing (QuestionValidation)
import Common.Api.Models.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))
import Common.Pages.KMEditor.Editor.Validations as Validations
import Expect
import Gettext
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, text)


{-| The rules a change would produce are not what is under test here — which
rules are on offer is — so the message carries nothing.
-}
type Msg
    = Changed


config : QuestionValueType -> List QuestionValidation -> Validations.Config Msg
config valueType validations =
    { locale = Gettext.defaultLocale
    , id = "validations"
    , valueType = valueType
    , validations = validations
    , onChange = always Changed
    }


{-| The kinds on offer are the options of a row's select, so there has to be a
row: `seed` is a rule the type can already use.
-}
offers : QuestionValueType -> QuestionValidation -> String -> Expect.Expectation
offers valueType seed label =
    case Validations.view (config valueType [ seed ]) of
        Just html ->
            Query.fromHtml html |> Query.has [ text label ]

        Nothing ->
            Expect.fail "the type offers no validations at all"


doesNotOffer : QuestionValueType -> QuestionValidation -> String -> Expect.Expectation
doesNotOffer valueType seed label =
    case Validations.view (config valueType [ seed ]) of
        Just html ->
            Query.fromHtml html |> Query.hasNot [ text label ]

        Nothing ->
            Expect.fail "the type offers no validations at all"


suite : Test
suite =
    describe "Common.Pages.KMEditor.Editor.Validations"
        [ describe "which rules a value type offers"
            [ test "a string can be measured by length" <|
                \_ -> offers StringQuestionValueType QuestionValidation.regex "Min Length"
            , test "a string can be an ORCID" <|
                \_ -> offers StringQuestionValueType QuestionValidation.regex "ORCID"
            , test "a number cannot be measured by length" <|
                \_ -> doesNotOffer NumberQuestionValueType QuestionValidation.minNumber "Min Length"
            , test "a number has a floor and a ceiling" <|
                \_ -> offers NumberQuestionValueType QuestionValidation.minNumber "Min Number"
            , test "a date has a range of dates" <|
                \_ -> offers DateQuestionValueType QuestionValidation.fromDate "From Date"
            , test "an email is limited by domain" <|
                \_ -> offers EmailQuestionValueType QuestionValidation.domain "Domain"
            , test "a color has no rule worth writing, so there is no section" <|
                \_ -> Expect.equal Nothing (Maybe.map (always ()) (Validations.view (config ColorQuestionValueType [])))
            ]
        , describe "a rule the type cannot use"
            [ test "is not shown" <|
                \_ ->
                    case Validations.view (config NumberQuestionValueType [ QuestionValidation.minLength ]) of
                        Just html ->
                            Query.fromHtml html
                                |> Query.findAll [ class "ui-list-field__row" ]
                                |> Query.count (Expect.equal 0)

                        Nothing ->
                            Expect.fail "the type offers no validations at all"
            , test "but a rule it can use still is" <|
                \_ ->
                    case Validations.view (config NumberQuestionValueType [ QuestionValidation.minLength, QuestionValidation.minNumber ]) of
                        Just html ->
                            Query.fromHtml html
                                |> Query.findAll [ class "ui-list-field__row" ]
                                |> Query.count (Expect.equal 1)

                        Nothing ->
                            Expect.fail "the type offers no validations at all"
            ]
        ]
