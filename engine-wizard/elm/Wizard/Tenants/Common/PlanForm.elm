module Wizard.Tenants.Common.PlanForm exposing (PlanForm, encode, init, initEmpty, validation)

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
    , sinceDay : Maybe Int
    , sinceMonth : Maybe Int
    , sinceYear : Maybe Int
    , untilDay : Maybe Int
    , untilMonth : Maybe Int
    , untilYear : Maybe Int
    , test : Bool
    }


initEmpty : Form FormError PlanForm
initEmpty =
    Form.initial [] validation


init : AppState -> Plan -> Form FormError PlanForm
init appState plan =
    let
        since =
            Maybe.map (Time.posixToParts appState.timeZone) plan.since

        until =
            Maybe.map (Time.posixToParts appState.timeZone) plan.until

        fields =
            [ ( "name", Field.string plan.name )
            , ( "users", Field.string (Maybe.unwrap "" String.fromInt plan.users) )
            , ( "sinceDay", Field.string (Maybe.unwrap "" (String.fromInt << .day) since) )
            , ( "sinceMonth", Field.string (Maybe.unwrap "" (String.fromInt << TimeUtils.monthToInt << .month) since) )
            , ( "sinceYear", Field.string (Maybe.unwrap "" (String.fromInt << .year) since) )
            , ( "untilDay", Field.string (Maybe.unwrap "" (String.fromInt << .day) until) )
            , ( "untilMonth", Field.string (Maybe.unwrap "" (String.fromInt << TimeUtils.monthToInt << .month) until) )
            , ( "untilYear", Field.string (Maybe.unwrap "" (String.fromInt << .year) until) )
            , ( "test", Field.bool plan.test )
            ]
    in
    Form.initial fields validation


validation : Validation FormError PlanForm
validation =
    V.succeed PlanForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "users" V.maybeInt)
        |> V.andMap (V.field "sinceDay" V.maybeInt)
        |> V.andMap (V.field "sinceMonth" V.maybeInt)
        |> V.andMap (V.field "sinceYear" V.maybeInt)
        |> V.andMap (V.field "untilDay" V.maybeInt)
        |> V.andMap (V.field "untilMonth" V.maybeInt)
        |> V.andMap (V.field "untilYear" V.maybeInt)
        |> V.andMap (V.field "test" V.bool)


encode : AppState -> PlanForm -> E.Value
encode appState form =
    let
        since =
            case ( form.sinceYear, form.sinceMonth, form.sinceDay ) of
                ( Just sinceYear, Just sinceMonth, Just sinceDay ) ->
                    Just <| TimeUtils.fromYMD appState.timeZone sinceYear sinceMonth sinceDay

                _ ->
                    Nothing

        until =
            case ( form.untilYear, form.untilMonth, form.untilDay ) of
                ( Just untilYear, Just untilMonth, Just untilDay ) ->
                    Just <| TimeUtils.fromYMD appState.timeZone untilYear untilMonth untilDay

                _ ->
                    Nothing
    in
    E.object
        [ ( "name", E.string form.name )
        , ( "users", E.maybe E.int form.users )
        , ( "since", E.maybe Iso8601.encode since )
        , ( "until", E.maybe Iso8601.encode until )
        , ( "test", E.bool form.test )
        ]
