module Common.Data.Forms.KnowledgeModelEditorCreateForm exposing
    ( KnowledgeModelEditorCreateForm
    , encode
    , init
    , validation
    )

import Common.Data.AppState exposing (AppState)
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Maybe.Extra as Maybe


type alias KnowledgeModelEditorCreateForm =
    { name : String
    , id : String
    , versionMajor : Int
    , versionMinor : Int
    , versionPatch : Int
    , language : String
    , previousPackageUuid : Maybe String
    }


init : AppState -> Maybe String -> Form FormError KnowledgeModelEditorCreateForm
init appState selectedKnowledgeModelPackage =
    let
        initials =
            ( "language", Field.string "en" )
                :: (case selectedKnowledgeModelPackage of
                        Just kmPackageUuid ->
                            [ ( "previousPackageUuid", Field.string kmPackageUuid ) ]

                        _ ->
                            []
                   )
    in
    Form.initial initials (validation appState)


validation : AppState -> Validation FormError KnowledgeModelEditorCreateForm
validation appState =
    V.succeed KnowledgeModelEditorCreateForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "id" (V.kmId appState))
        |> V.andMap (V.field "versionMajor" V.versionNumber)
        |> V.andMap (V.field "versionMinor" V.versionNumber)
        |> V.andMap (V.field "versionPatch" V.versionNumber)
        |> V.andMap (V.field "language" V.string)
        |> V.andMap (V.field "previousPackageUuid" V.maybeString)


encode : KnowledgeModelEditorCreateForm -> E.Value
encode form =
    let
        parentPackage =
            Maybe.unwrap E.null E.string form.previousPackageUuid

        version =
            String.join "." <| List.map String.fromInt [ form.versionMajor, form.versionMinor, form.versionPatch ]
    in
    E.object
        [ ( "name", E.string form.name )
        , ( "id", E.string form.id )
        , ( "version", E.string version )
        , ( "language", E.string form.language )
        , ( "previousPackageUuid", parentPackage )
        ]
