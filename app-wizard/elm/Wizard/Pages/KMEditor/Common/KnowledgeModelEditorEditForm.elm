module Wizard.Pages.KMEditor.Common.KnowledgeModelEditorEditForm exposing
    ( KnowledgeModelEditorEditForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Version
import Wizard.Api.Models.KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Wizard.Data.AppState exposing (AppState)


type alias KnowledgeModelEditorEditForm =
    { name : String
    , description : String
    , kmId : String
    , versionMajor : Int
    , versionMinor : Int
    , versionPatch : Int
    , license : String
    , readme : String
    }


init : AppState -> KnowledgeModelEditorDetail -> Form FormError KnowledgeModelEditorEditForm
init appState kmEditor =
    let
        initials =
            [ ( "name", Field.string kmEditor.name )
            , ( "description", Field.string kmEditor.description )
            , ( "kmId", Field.string kmEditor.kmId )
            , ( "versionMajor", Field.string (String.fromInt (Version.getMajor kmEditor.version)) )
            , ( "versionMinor", Field.string (String.fromInt (Version.getMinor kmEditor.version)) )
            , ( "versionPatch", Field.string (String.fromInt (Version.getPatch kmEditor.version)) )
            , ( "license", Field.string kmEditor.license )
            , ( "readme", Field.string kmEditor.readme )
            ]
    in
    Form.initial initials (validation appState)


initEmpty : AppState -> Form FormError KnowledgeModelEditorEditForm
initEmpty appState =
    Form.initial [] (validation appState)


validation : AppState -> Validation FormError KnowledgeModelEditorEditForm
validation appState =
    V.succeed KnowledgeModelEditorEditForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "description" V.string)
        |> V.andMap (V.field "kmId" (V.kmId appState))
        |> V.andMap (V.field "versionMajor" V.versionNumber)
        |> V.andMap (V.field "versionMinor" V.versionNumber)
        |> V.andMap (V.field "versionPatch" V.versionNumber)
        |> V.andMap (V.field "license" V.string)
        |> V.andMap (V.field "readme" V.string)


encode : KnowledgeModelEditorEditForm -> E.Value
encode form =
    let
        version =
            String.join "." <| List.map String.fromInt [ form.versionMajor, form.versionMinor, form.versionPatch ]
    in
    E.object
        [ ( "name", E.string form.name )
        , ( "description", E.string form.description )
        , ( "kmId", E.string form.kmId )
        , ( "version", E.string version )
        , ( "license", E.string form.license )
        , ( "readme", E.string form.readme )
        ]
