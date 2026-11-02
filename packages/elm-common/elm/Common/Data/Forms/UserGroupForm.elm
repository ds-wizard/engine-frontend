module Common.Data.Forms.UserGroupForm exposing
    ( UserGroupForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Common.Utils.Form.Field as Field
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Json.Encode.Extra as E


type alias UserGroupForm =
    { name : String
    , description : Maybe String
    , private : Bool
    }


initEmpty : Form FormError UserGroupForm
initEmpty =
    Form.initial [] validation


init : { a | name : String, description : Maybe String, private : Bool } -> Form FormError UserGroupForm
init userGroup =
    Form.initial
        [ ( "name", Field.string userGroup.name )
        , ( "description", Field.maybeString userGroup.description )
        , ( "private", Field.bool userGroup.private )
        ]
        validation


validation : Validation FormError UserGroupForm
validation =
    V.succeed UserGroupForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "description" V.maybeString)
        |> V.andMap (V.field "private" V.bool)


encode : UserGroupForm -> E.Value
encode form =
    E.object
        [ ( "name", E.string form.name )
        , ( "description", E.maybe E.string form.description )
        , ( "private", E.bool form.private )
        ]
