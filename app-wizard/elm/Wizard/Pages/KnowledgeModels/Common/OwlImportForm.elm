module Wizard.Pages.KnowledgeModels.Common.OwlImportForm exposing
    ( OwlImportForm
    , init
    , validation
    )

import Common.Utils.Form.Field as Field
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Wizard.Data.AppState exposing (AppState)


type alias OwlImportForm =
    { name : String
    , organizationId : String
    , kmId : String
    , version : String
    , previousPackageId : Maybe String
    , rootElement : String
    }


init : AppState -> Form FormError OwlImportForm
init appState =
    let
        initialData =
            [ ( "name", Field.maybeString appState.config.owl.name )
            , ( "organizationId", Field.maybeString appState.config.owl.organizationId )
            , ( "kmId", Field.maybeString appState.config.owl.kmId )
            , ( "version", Field.maybeString appState.config.owl.version )
            , ( "previousPackageId", Field.maybeString appState.config.owl.previousPackageId )
            , ( "rootElement", Field.maybeString appState.config.owl.rootElement )
            ]
    in
    Form.initial initialData validation


validation : Validation FormError OwlImportForm
validation =
    V.succeed OwlImportForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "organizationId" V.string)
        |> V.andMap (V.field "kmId" V.string)
        |> V.andMap (V.field "version" V.string)
        |> V.andMap (V.field "previousPackageId" V.maybeString)
        |> V.andMap (V.field "rootElement" V.string)
