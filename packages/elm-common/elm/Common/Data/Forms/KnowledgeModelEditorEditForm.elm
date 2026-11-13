module Common.Data.Forms.KnowledgeModelEditorEditForm exposing
    ( KnowledgeModelEditorEditForm
    , encode
    , encodeEditor
    , init
    , initEmpty
    , validation
    )

import Common.Api.Models.KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Common.Data.AppState exposing (AppState)
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Version


type alias KnowledgeModelEditorEditForm =
    { name : String
    , description : String
    , id : String
    , versionMajor : Int
    , versionMinor : Int
    , versionPatch : Int
    , language : String
    , license : String
    , readme : String
    }


init : AppState -> KnowledgeModelEditorDetail -> Form FormError KnowledgeModelEditorEditForm
init appState kmEditor =
    let
        initials =
            [ ( "name", Field.string kmEditor.name )
            , ( "description", Field.string kmEditor.description )
            , ( "id", Field.string kmEditor.id )
            , ( "versionMajor", Field.string (String.fromInt (Version.getMajor kmEditor.version)) )
            , ( "versionMinor", Field.string (String.fromInt (Version.getMinor kmEditor.version)) )
            , ( "versionPatch", Field.string (String.fromInt (Version.getPatch kmEditor.version)) )
            , ( "language", Field.string kmEditor.language )
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
        |> V.andMap (V.field "id" (V.kmId appState))
        |> V.andMap (V.field "versionMajor" V.versionNumber)
        |> V.andMap (V.field "versionMinor" V.versionNumber)
        |> V.andMap (V.field "versionPatch" V.versionNumber)
        |> V.andMap (V.field "language" V.string)
        |> V.andMap (V.field "license" V.string)
        |> V.andMap (V.field "readme" V.string)


encode : KnowledgeModelEditorEditForm -> E.Value
encode form =
    encodeSettings
        { name = form.name
        , description = form.description
        , id = form.id
        , version = String.join "." <| List.map String.fromInt [ form.versionMajor, form.versionMinor, form.versionPatch ]
        , language = form.language
        , license = form.license
        , readme = form.readme
        }


{-| The editor's settings as they are, sent the way the form sends them — for a
change made outside the form, such as a rename from the page's title. Nothing
is validated: these are what the server already holds, and a new editor holds
an empty description, license and readme, which the form does not accept until
they are filled in.
-}
encodeEditor : KnowledgeModelEditorDetail -> E.Value
encodeEditor kmEditor =
    encodeSettings
        { name = kmEditor.name
        , description = kmEditor.description
        , id = kmEditor.id
        , version = Version.toString kmEditor.version
        , language = kmEditor.language
        , license = kmEditor.license
        , readme = kmEditor.readme
        }


encodeSettings :
    { name : String
    , description : String
    , id : String
    , version : String
    , language : String
    , license : String
    , readme : String
    }
    -> E.Value
encodeSettings settings =
    E.object
        [ ( "name", E.string settings.name )
        , ( "description", E.string settings.description )
        , ( "id", E.string settings.id )
        , ( "version", E.string settings.version )
        , ( "language", E.string settings.language )
        , ( "license", E.string settings.license )
        , ( "readme", E.string settings.readme )
        ]
