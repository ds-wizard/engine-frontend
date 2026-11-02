module Common.Api.Models.UserFromExternal exposing
    ( UserFromExternal
    , decoder
    , encode
    , initForm
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias UserFromExternal =
    { hash : String
    , email : Maybe String
    , firstName : Maybe String
    , lastName : Maybe String
    }


decoder : Decoder UserFromExternal
decoder =
    D.succeed UserFromExternal
        |> D.required "hash" D.string
        |> D.required "email" (D.maybe D.string)
        |> D.required "firstName" (D.maybe D.string)
        |> D.required "lastName" (D.maybe D.string)


encode : UserFromExternal -> E.Value
encode user =
    E.object
        [ ( "hash", E.string user.hash )
        , ( "email", E.maybe E.string user.email )
        , ( "firstName", E.maybe E.string user.firstName )
        , ( "lastName", E.maybe E.string user.lastName )
        ]


initForm : UserFromExternal -> Form FormError UserFromExternal
initForm user =
    let
        fields =
            [ ( "hash", Field.string user.hash )
            , ( "email", Field.string (Maybe.withDefault "" user.email) )
            , ( "firstName", Field.string (Maybe.withDefault "" user.firstName) )
            , ( "lastName", Field.string (Maybe.withDefault "" user.lastName) )
            ]
    in
    Form.initial fields validation


validation : Validation FormError UserFromExternal
validation =
    V.succeed UserFromExternal
        |> V.andMap (V.field "hash" V.string)
        |> V.andMap (V.field "email" V.email |> V.map Just)
        |> V.andMap (V.field "firstName" V.string |> V.map Just)
        |> V.andMap (V.field "lastName" V.string |> V.map Just)
