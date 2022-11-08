module Wizard.DocumentTemplateEditors.Common.DocumentTemplateEditorCreateForm exposing (DocumentTemplateEditorCreateForm, encode, init, validation)

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Maybe.Extra as Maybe
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V
import String exposing (fromInt)


type alias DocumentTemplateEditorCreateForm =
    { name : String
    , templateId : String
    , versionMajor : Int
    , versionMinor : Int
    , versionPatch : Int
    , basedOn : Maybe String
    }


init : Maybe String -> Form FormError DocumentTemplateEditorCreateForm
init mbBasedOn =
    let
        initials =
            case mbBasedOn of
                Just basedOn ->
                    [ ( "basedOn", Field.string basedOn ) ]

                Nothing ->
                    []
    in
    Form.initial initials validation


validation : Validation FormError DocumentTemplateEditorCreateForm
validation =
    V.succeed DocumentTemplateEditorCreateForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "templateId" V.string)
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
        , ( "templateId", E.string form.templateId )
        , ( "version", E.string version )
        , ( "basedOn", basedOn )
        ]
