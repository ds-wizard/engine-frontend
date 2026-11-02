module Common.Pages.KMEditor.Editor.Validations exposing (Config, view)

{-| What a value question will accept: a list of rules, each a kind and a value.

**Which rules are offered depends on the value type.** A minimum length means
nothing to a date and a range of dates means nothing to a string, so the kinds
on offer are filtered by the type the question holds, and a color — which has
no rule worth writing — offers none at all.

**Changing a rule's kind resets its value**, because the value belongs to the
kind: the number in "at least 10 characters" is not the date in "no earlier
than". The wizard does the same.

**A rule the value type does not allow is kept but not shown.** That happens
when the type is changed under a rule that was valid before. The wizard drops
those the moment another rule is added, silently; here they are carried through
every edit, so changing the type back brings them with it.

It is a `Ui.ListField`: a set of rows with a remove on each and one way to add
another, which is what every other list of rows in a form is.

-}

import Common.Api.Models.KnowledgeModel.Question.QuestionValidation as QuestionValidation exposing (QuestionValidation)
import Common.Api.Models.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))
import Gettext exposing (gettext)
import Html exposing (Html)
import List.Extra as List
import Ui.Field as Field
import Ui.ListField as ListField


type alias Config msg =
    { locale : Gettext.Locale
    , id : String
    , valueType : QuestionValueType
    , validations : List QuestionValidation
    , onChange : List QuestionValidation -> msg
    }


view : Config msg -> Maybe (Html msg)
view config =
    case defaultFor config.valueType of
        Just default ->
            let
                -- The ones this value type allows, with where each sits in the
                -- list as a whole: a rule hidden by a change of type keeps its
                -- place, so removing a shown one removes the right rule.
                shown =
                    config.validations
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( _, validation ) -> List.member (QuestionValidation.toOptionString validation) (List.map .value (kindsFor config)))
            in
            Just
                (ListField.toHtml
                    { id = config.id
                    , label = gettext "Validations" config.locale
                    , rows = List.indexedMap (row config) shown
                    , addLabel = gettext "Add validation" config.locale
                    , onAdd = config.onChange (config.validations ++ [ default ])
                    , onRemove = Just (\position -> config.onChange (removeAt (indexOf position shown) config.validations))
                    , hint = Nothing
                    , error = Nothing
                    }
                )

        Nothing ->
            Nothing


row : Config msg -> Int -> ( Int, QuestionValidation ) -> ListField.Row msg
row config position ( index, validation ) =
    let
        rowId name =
            config.id ++ "-" ++ String.fromInt position ++ "-" ++ name
    in
    ListField.row
        { label = labelOf config validation
        , content =
            Field.toHtml
                (Field.select
                    { id = rowId "type"
                    , label = gettext "Validation Type" config.locale
                    , value = QuestionValidation.toOptionString validation
                    , options = List.map (\kind -> ( kind.value, kind.label )) (kindsFor config)
                    , onInput = \chosen -> config.onChange (setAt index (kindFromString chosen) config.validations)
                    }
                )
                :: valueField config
                    (rowId "value")
                    validation
                    (\next -> config.onChange (setAt index next config.validations))
        }
        -- Positional, as its fields' ids are: the rules have no key of their own.
        |> ListField.withRowTestId (rowId "row")


{-| The row's name among its siblings, which is what its remove button is
called: the kind, since that is what tells one rule from another.
-}
labelOf : Config msg -> QuestionValidation -> String
labelOf config validation =
    kindsFor config
        |> List.find (\kind -> kind.value == QuestionValidation.toOptionString validation)
        |> Maybe.map .label
        |> Maybe.withDefault (QuestionValidation.toOptionString validation)


{-| The control the rule's value needs, or nothing where the rule is the whole
of it — an identifier is an ORCID or it is not.
-}
valueField : Config msg -> String -> QuestionValidation -> (QuestionValidation -> msg) -> List (Html msg)
valueField config id validation onChange =
    let
        label =
            gettext "Value" config.locale

        whole wrap data =
            [ Field.toHtml
                (Field.input
                    { id = id
                    , label = label
                    , value = String.fromInt data.value
                    , onInput = \next -> onChange (wrap { value = Maybe.withDefault 0 (String.toInt next) })
                    }
                    |> Field.withInputMode "numeric"
                )
            ]

        fractional wrap data =
            [ Field.toHtml
                (Field.input
                    { id = id
                    , label = label
                    , value = String.fromFloat data.value
                    , onInput = \next -> onChange (wrap { value = Maybe.withDefault 0 (String.toFloat next) })
                    }
                    |> Field.withInputMode "decimal"
                )
            ]

        text wrap data =
            [ Field.toHtml
                (Field.input
                    { id = id
                    , label = label
                    , value = data.value
                    , onInput = \next -> onChange (wrap { value = next })
                    }
                )
            ]

        moment field wrap data =
            [ Field.toHtml
                (field
                    { id = id
                    , label = label
                    , value = data.value
                    , onChange = \next -> onChange (wrap { value = next })
                    }
                )
            ]
    in
    case validation of
        QuestionValidation.MinLength data ->
            whole QuestionValidation.MinLength data

        QuestionValidation.MaxLength data ->
            whole QuestionValidation.MaxLength data

        QuestionValidation.Regex data ->
            text QuestionValidation.Regex data

        QuestionValidation.Orcid ->
            []

        QuestionValidation.Doi ->
            []

        QuestionValidation.MinNumber data ->
            fractional QuestionValidation.MinNumber data

        QuestionValidation.MaxNumber data ->
            fractional QuestionValidation.MaxNumber data

        QuestionValidation.FromDate data ->
            moment Field.date QuestionValidation.FromDate data

        QuestionValidation.ToDate data ->
            moment Field.date QuestionValidation.ToDate data

        QuestionValidation.FromDateTime data ->
            moment Field.dateTime QuestionValidation.FromDateTime data

        QuestionValidation.ToDateTime data ->
            moment Field.dateTime QuestionValidation.ToDateTime data

        QuestionValidation.FromTime data ->
            moment Field.time QuestionValidation.FromTime data

        QuestionValidation.ToTime data ->
            moment Field.time QuestionValidation.ToTime data

        QuestionValidation.Domain data ->
            text QuestionValidation.Domain data



-- THE KINDS THERE ARE


type alias Kind =
    { value : String
    , label : String
    , make : QuestionValidation
    }


{-| The kinds this question's value type allows, in the order the wizard offers
them.
-}
kindsFor : Config msg -> List Kind
kindsFor config =
    List.filter (\kind -> List.member config.valueType (QuestionValidation.validValueTypes kind.make)) (allKinds config)


allKinds : Config msg -> List Kind
allKinds config =
    [ Kind "MinLength" (gettext "Min Length" config.locale) QuestionValidation.minLength
    , Kind "MaxLength" (gettext "Max Length" config.locale) QuestionValidation.maxLength
    , Kind "Regex" (gettext "Regex" config.locale) QuestionValidation.regex
    , Kind "Orcid" (gettext "ORCID" config.locale) QuestionValidation.orcid
    , Kind "Doi" (gettext "DOI" config.locale) QuestionValidation.doi
    , Kind "MinNumber" (gettext "Min Number" config.locale) QuestionValidation.minNumber
    , Kind "MaxNumber" (gettext "Max Number" config.locale) QuestionValidation.maxNumber
    , Kind "FromDate" (gettext "From Date" config.locale) QuestionValidation.fromDate
    , Kind "ToDate" (gettext "To Date" config.locale) QuestionValidation.toDate
    , Kind "FromDateTime" (gettext "From Date Time" config.locale) QuestionValidation.fromDateTime
    , Kind "ToDateTime" (gettext "To Date Time" config.locale) QuestionValidation.toDateTime
    , Kind "FromTime" (gettext "From Time" config.locale) QuestionValidation.fromTime
    , Kind "ToTime" (gettext "To Time" config.locale) QuestionValidation.toTime
    , Kind "Domain" (gettext "Domain" config.locale) QuestionValidation.domain
    ]


kindFromString : String -> QuestionValidation
kindFromString value =
    case value of
        "MinLength" ->
            QuestionValidation.minLength

        "MaxLength" ->
            QuestionValidation.maxLength

        "Regex" ->
            QuestionValidation.regex

        "Orcid" ->
            QuestionValidation.orcid

        "Doi" ->
            QuestionValidation.doi

        "MinNumber" ->
            QuestionValidation.minNumber

        "MaxNumber" ->
            QuestionValidation.maxNumber

        "FromDate" ->
            QuestionValidation.fromDate

        "ToDate" ->
            QuestionValidation.toDate

        "FromDateTime" ->
            QuestionValidation.fromDateTime

        "ToDateTime" ->
            QuestionValidation.toDateTime

        "FromTime" ->
            QuestionValidation.fromTime

        "ToTime" ->
            QuestionValidation.toTime

        _ ->
            QuestionValidation.domain


{-| What "Add validation" makes, which is the first rule that makes sense for
the value type. A color has none, and that is what hides the whole section.
-}
defaultFor : QuestionValueType -> Maybe QuestionValidation
defaultFor valueType =
    case valueType of
        StringQuestionValueType ->
            Just QuestionValidation.minLength

        TextQuestionValueType ->
            Just QuestionValidation.minLength

        NumberQuestionValueType ->
            Just QuestionValidation.minNumber

        DateQuestionValueType ->
            Just QuestionValidation.fromDate

        DateTimeQuestionValueType ->
            Just QuestionValidation.fromDateTime

        TimeQuestionValueType ->
            Just QuestionValidation.fromTime

        EmailQuestionValueType ->
            Just QuestionValidation.domain

        UrlQuestionValueType ->
            Just QuestionValidation.regex

        ColorQuestionValueType ->
            Nothing



-- ODDS AND ENDS


indexOf : Int -> List ( Int, QuestionValidation ) -> Int
indexOf position shown =
    shown
        |> List.getAt position
        |> Maybe.map Tuple.first
        |> Maybe.withDefault -1


setAt : Int -> a -> List a -> List a
setAt index value =
    List.indexedMap
        (\i item ->
            if i == index then
                value

            else
                item
        )


removeAt : Int -> List a -> List a
removeAt index list =
    List.take index list ++ List.drop (index + 1) list
