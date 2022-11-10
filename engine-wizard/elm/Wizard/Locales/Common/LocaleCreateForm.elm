module Wizard.Locales.Common.LocaleCreateForm exposing
    ( LocaleCreateFrom
    , init
    , validation
    )

import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Shared.Form.FormError exposing (FormError)


type alias LocaleCreateFrom =
    { name : String
    , description : String
    , code : String
    , localeId : String
    , localeMajor : Int
    , localeMinor : Int
    , localePatch : Int
    , license : String
    , readme : String
    , appMajor : Int
    , appMinor : Int
    , appPatch : Int
    }


init : Form FormError LocaleCreateFrom
init =
    Form.initial [] validation


validation : Validation FormError LocaleCreateFrom
validation =
    V.succeed LocaleCreateFrom
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "description" V.string)
        |> V.andMap (V.field "code" V.string)
        |> V.andMap (V.field "localeId" V.string)
        |> V.andMap (V.field "localeMajor" (V.int |> V.andThen (V.minInt 0)))
        |> V.andMap (V.field "localeMinor" (V.int |> V.andThen (V.minInt 0)))
        |> V.andMap (V.field "localePatch" (V.int |> V.andThen (V.minInt 0)))
        |> V.andMap (V.field "license" V.string)
        |> V.andMap (V.field "readme" V.string)
        |> V.andMap (V.field "appMajor" (V.int |> V.andThen (V.minInt 0)))
        |> V.andMap (V.field "appMinor" (V.int |> V.andThen (V.minInt 0)))
        |> V.andMap (V.field "appPatch" (V.int |> V.andThen (V.minInt 0)))
