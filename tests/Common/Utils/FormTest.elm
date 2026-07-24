module Common.Utils.FormTest exposing (moveListItemTest)

import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Expect
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Test exposing (Test, describe, test)


type alias Link =
    { icon : String, title : String, url : String, newWindow : Bool }


type alias TestForm =
    { links : List Link }


validation : Validation FormError TestForm
validation =
    V.succeed TestForm
        |> V.andMap (V.field "customMenuLinks" (V.list linkValidation))


linkValidation : Validation FormError Link
linkValidation =
    V.succeed Link
        |> V.andMap (V.field "icon" V.string)
        |> V.andMap (V.field "title" V.string)
        |> V.andMap (V.field "url" V.string)
        |> V.andMap (V.field "newWindow" V.bool)


linkField : String -> Bool -> Field.Field
linkField name newWindow =
    Field.group
        [ ( "icon", Field.string (name ++ "-icon") )
        , ( "title", Field.string (name ++ "-title") )
        , ( "url", Field.string (name ++ "-url") )
        , ( "newWindow", Field.bool newWindow )
        ]


{-| Three links "a" (newWindow off), "b" (newWindow on), "c" (newWindow off).
-}
initialForm : Form FormError TestForm
initialForm =
    Form.initial
        [ ( "customMenuLinks"
          , Field.list
                [ linkField "a" False
                , linkField "b" True
                , linkField "c" False
                ]
          )
        ]
        validation


fields : List String
fields =
    [ "icon", "title", "url", "newWindow" ]


stringAt : Int -> String -> Form FormError TestForm -> Maybe String
stringAt index name form =
    (Form.getFieldAsString ("customMenuLinks." ++ String.fromInt index ++ "." ++ name) form).value


newWindowAt : Int -> Form FormError TestForm -> Maybe Bool
newWindowAt index form =
    (Form.getFieldAsBool ("customMenuLinks." ++ String.fromInt index ++ ".newWindow") form).value


moveListItemTest : Test
moveListItemTest =
    describe "Common.Utils.Form.moveListItem"
        [ test "moves an item down by swapping all its field values with the next item" <|
            \_ ->
                let
                    moved =
                        FormUtils.moveListItem validation "customMenuLinks" fields 0 1 initialForm
                in
                Expect.all
                    [ \m -> Expect.equal (Just "b-icon") (stringAt 0 "icon" m)
                    , \m -> Expect.equal (Just "b-title") (stringAt 0 "title" m)
                    , \m -> Expect.equal (Just "b-url") (stringAt 0 "url" m)
                    , \m -> Expect.equal (Just True) (newWindowAt 0 m)
                    , \m -> Expect.equal (Just "a-icon") (stringAt 1 "icon" m)
                    , \m -> Expect.equal (Just False) (newWindowAt 1 m)
                    , \m -> Expect.equal (Just "c-icon") (stringAt 2 "icon" m)
                    , \m -> Expect.equal (Just False) (newWindowAt 2 m)
                    ]
                    moved
        , test "moves an item up by swapping all its field values with the previous item" <|
            \_ ->
                let
                    moved =
                        FormUtils.moveListItem validation "customMenuLinks" fields 2 1 initialForm
                in
                Expect.all
                    [ \m -> Expect.equal (Just "a-icon") (stringAt 0 "icon" m)
                    , \m -> Expect.equal (Just "c-icon") (stringAt 1 "icon" m)
                    , \m -> Expect.equal (Just False) (newWindowAt 1 m)
                    , \m -> Expect.equal (Just "b-icon") (stringAt 2 "icon" m)
                    , \m -> Expect.equal (Just True) (newWindowAt 2 m)
                    ]
                    moved
        , test "marks the form as changed so the reorder can be saved" <|
            \_ ->
                FormUtils.moveListItem validation "customMenuLinks" fields 0 1 initialForm
                    |> FormUtils.containsChanges
                    |> Expect.equal True
        ]
