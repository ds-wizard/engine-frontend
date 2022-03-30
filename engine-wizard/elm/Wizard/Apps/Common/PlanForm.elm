module Wizard.Apps.Common.PlanForm exposing (PlanForm, encode, init, initEmpty, validation)

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Iso8601
import Json.Encode as E
import Json.Encode.Extra as E
import Maybe.Extra as Maybe
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.Plan exposing (Plan)
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V
import Time.Extra as Time
import Wizard.Common.AppState exposing (AppState)


type alias PlanForm =
    { name : String
    , users : Maybe Int
    , sinceDay : Int
    , sinceMonth : Int
    , sinceYear : Int
    , untilDay : Int
    , untilMonth : Int
    , untilYear : Int
    , test : Bool
    }


initEmpty : Form FormError PlanForm
initEmpty =
    Form.initial [] validation


init : AppState -> Plan -> Form FormError PlanForm
init appState plan =
    let
        since =
            Time.posixToParts appState.timeZone plan.since

        until =
            Time.posixToParts appState.timeZone plan.until

        fields =
            [ ( "name", Field.string plan.name )
            , ( "users", Field.string (Maybe.unwrap "" String.fromInt plan.users) )
            , ( "sinceDay", Field.string (String.fromInt since.day) )
            , ( "sinceMonth", Field.string (String.fromInt (TimeUtils.monthToInt since.month)) )
            , ( "sinceYear", Field.string (String.fromInt since.year) )
            , ( "untilDay", Field.string (String.fromInt until.day) )
            , ( "untilMonth", Field.string (String.fromInt (TimeUtils.monthToInt until.month)) )
            , ( "untilYear", Field.string (String.fromInt until.year) )
            , ( "test", Field.bool plan.test )
            ]
    in
    Form.initial fields validation


validation : Validation FormError PlanForm
validation =
    V.succeed PlanForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "users" V.maybeInt)
        |> V.andMap (V.field "sinceDay" V.int)
        |> V.andMap (V.field "sinceMonth" V.int)
        |> V.andMap (V.field "sinceYear" V.int)
        |> V.andMap (V.field "untilDay" V.int)
        |> V.andMap (V.field "untilMonth" V.int)
        |> V.andMap (V.field "untilYear" V.int)
        |> V.andMap (V.field "test" V.bool)


encode : AppState -> PlanForm -> E.Value
encode appState form =
    let
        since =
            TimeUtils.fromYMD appState.timeZone form.sinceYear form.sinceMonth form.sinceDay

        until =
            TimeUtils.fromYMD appState.timeZone form.untilYear form.untilMonth form.untilDay
    in
    E.object
        [ ( "name", E.string form.name )
        , ( "users", E.maybe E.int form.users )
        , ( "since", Iso8601.encode since )
        , ( "until", Iso8601.encode until )
        , ( "test", E.bool form.test )
        ]
