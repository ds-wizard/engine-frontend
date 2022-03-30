module Wizard.Apps.Common.AppEditForm exposing (AppEditForm, encode, init, initEmpty, validation)

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Data.AppDetail exposing (AppDetail)
import Shared.Form.FormError exposing (FormError)


type alias AppEditForm =
    { appId : String
    , name : String
    }


initEmpty : Form FormError AppEditForm
initEmpty =
    Form.initial [] validation


init : AppDetail -> Form FormError AppEditForm
init appDetail =
    let
        fields =
            [ ( "appId", Field.string appDetail.appId )
            , ( "name", Field.string appDetail.name )
            ]
    in
    Form.initial fields validation


validation : Validation FormError AppEditForm
validation =
    V.succeed AppEditForm
        |> V.andMap (V.field "appId" V.string)
        |> V.andMap (V.field "name" V.string)


encode : AppEditForm -> E.Value
encode form =
    E.object
        [ ( "appId", E.string form.appId )
        , ( "name", E.string form.name )
        ]
