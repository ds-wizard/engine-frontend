module Wizard.Pages.KMEditor.Common.KnowledgeModelEditorCreateForm exposing
    ( KnowledgeModelEditorCreateForm
    , encode
    , init
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Maybe.Extra as Maybe
import Wizard.Data.AppState exposing (AppState)


type alias KnowledgeModelEditorCreateForm =
    { name : String
    , kmId : String
    , versionMajor : Int
    , versionMinor : Int
    , versionPatch : Int
    , previousKnowledgeModelPackageId : Maybe String
    }


init : AppState -> Maybe String -> Form FormError KnowledgeModelEditorCreateForm
init appState selectedKnowledgeModelPackage =
    let
        initials =
            case selectedKnowledgeModelPackage of
                Just kmPackageId ->
                    [ ( "previousKnowledgeModelPackageId", Field.string kmPackageId ) ]

                _ ->
                    []
    in
    Form.initial initials (validation appState)


validation : AppState -> Validation FormError KnowledgeModelEditorCreateForm
validation appState =
    V.succeed KnowledgeModelEditorCreateForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "kmId" (V.kmId appState))
        |> V.andMap (V.field "versionMajor" V.versionNumber)
        |> V.andMap (V.field "versionMinor" V.versionNumber)
        |> V.andMap (V.field "versionPatch" V.versionNumber)
        |> V.andMap (V.field "previousKnowledgeModelPackageId" V.maybeString)


encode : KnowledgeModelEditorCreateForm -> E.Value
encode form =
    let
        parentPackage =
            Maybe.unwrap E.null E.string form.previousKnowledgeModelPackageId

        version =
            String.join "." <| List.map String.fromInt [ form.versionMajor, form.versionMinor, form.versionPatch ]
    in
    E.object
        [ ( "name", E.string form.name )
        , ( "kmId", E.string form.kmId )
        , ( "version", E.string version )
        , ( "previousKnowledgeModelPackageId", parentPackage )
        ]
