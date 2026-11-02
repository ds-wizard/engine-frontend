module Common.Api.Models.Settings.UsersSettings exposing
    ( UsersSettings
    , decoder
    , default
    , encode
    , initEmptyForm
    , initForm
    , section
    , validation
    )

{-| What is set about the people of an instance: the affiliations offered to
somebody signing up or editing a profile.
-}

import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import List.Extra as List


type alias UsersSettings =
    { affiliations : List String }


default : UsersSettings
default =
    { affiliations = [] }


section : { name : String, decoder : Decoder UsersSettings, encode : UsersSettings -> E.Value }
section =
    { name = "users", decoder = decoder, encode = encode }


decoder : Decoder UsersSettings
decoder =
    D.succeed UsersSettings
        |> D.required "affiliations" (D.list D.string)


encode : UsersSettings -> E.Value
encode settings =
    E.object
        [ ( "affiliations", E.list E.string settings.affiliations ) ]



-- Form


{-| One affiliation per line.
-}
validation : Validation FormError UsersSettings
validation =
    V.succeed UsersSettings
        |> V.andMap (V.field "affiliations" (V.map toLines (V.oneOf [ V.string, V.emptyString ])))


toLines : String -> List String
toLines =
    String.split "\n"
        >> List.map String.trim
        >> List.filterNot String.isEmpty


initEmptyForm : Form FormError UsersSettings
initEmptyForm =
    Form.initial [] validation


initForm : UsersSettings -> Form FormError UsersSettings
initForm settings =
    Form.initial [ ( "affiliations", Field.string (String.join "\n" settings.affiliations) ) ] validation
