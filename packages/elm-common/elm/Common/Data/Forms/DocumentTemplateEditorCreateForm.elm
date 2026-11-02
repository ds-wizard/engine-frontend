module Common.Data.Forms.DocumentTemplateEditorCreateForm exposing (DocumentTemplateEditorCreateForm, encode, init, validation)

import Common.Data.AppState exposing (AppState)
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Maybe.Extra as Maybe
import String exposing (fromInt)
import Uuid exposing (Uuid)


type alias DocumentTemplateEditorCreateForm =
    { name : String
    , id : String
    , versionMajor : Int
    , versionMinor : Int
    , versionPatch : Int
    , basedOn : Maybe String
    }


init : AppState -> Maybe Uuid -> Form FormError DocumentTemplateEditorCreateForm
init appState mbBasedOn =
    let
        initials =
            case mbBasedOn of
                Just basedOn ->
                    [ ( "basedOn", Field.string (Uuid.toString basedOn) ) ]

                Nothing ->
                    []
    in
    Form.initial initials (validation appState)


validation : AppState -> Validation FormError DocumentTemplateEditorCreateForm
validation appState =
    V.succeed DocumentTemplateEditorCreateForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "id" (V.documentTemplateId appState))
        |> V.andMap (V.field "versionMajor" V.versionNumber)
        |> V.andMap (V.field "versionMinor" V.versionNumber)
        |> V.andMap (V.field "versionPatch" V.versionNumber)
        |> V.andMap (V.field "basedOn" V.maybeString)


encode : DocumentTemplateEditorCreateForm -> E.Value
encode form =
    let
        basedOn =
            Maybe.unwrap E.null E.string form.basedOn

        version =
            String.join "." <| List.map fromInt [ form.versionMajor, form.versionMinor, form.versionPatch ]
    in
    E.object
        [ ( "name", E.string form.name )
        , ( "id", E.string form.id )
        , ( "version", E.string version )
        , ( "basedOn", basedOn )
        ]
