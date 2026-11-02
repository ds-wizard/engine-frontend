module Common.Data.Forms.TenantLimitsForm exposing
    ( TenantLimitsForm
    , encode
    , init
    , validation
    )

import Common.Api.Models.Usage exposing (Usage)
import Common.Utils.Form.Field as Field
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Encode as E


type alias TenantLimitsForm =
    { activeUsers : Int
    , documentTemplateDrafts : Int
    , documentTemplates : Int
    , documents : Int
    , knowledgeModelEditors : Int
    , knowledgeModels : Int
    , questionnaires : Int
    , storage : Int
    , users : Int
    }


{-| The form, filled in with the limits the tenant has now.
-}
init : Usage -> Form FormError TenantLimitsForm
init usage =
    let
        fields =
            [ ( "activeUsers", Field.int usage.activeUsers.max )
            , ( "documentTemplateDrafts", Field.int usage.documentTemplateDrafts.max )
            , ( "documentTemplates", Field.int usage.documentTemplates.max )
            , ( "documents", Field.int usage.documents.max )
            , ( "knowledgeModelEditors", Field.int usage.knowledgeModelEditors.max )
            , ( "knowledgeModels", Field.int usage.knowledgeModels.max )
            , ( "questionnaires", Field.int usage.projects.max )
            , ( "storage", Field.int usage.storage.max )
            , ( "users", Field.int usage.users.max )
            ]
    in
    Form.initial fields validation


validation : Validation FormError TenantLimitsForm
validation =
    V.succeed TenantLimitsForm
        |> V.andMap (V.field "activeUsers" V.int)
        |> V.andMap (V.field "documentTemplateDrafts" V.int)
        |> V.andMap (V.field "documentTemplates" V.int)
        |> V.andMap (V.field "documents" V.int)
        |> V.andMap (V.field "knowledgeModelEditors" V.int)
        |> V.andMap (V.field "knowledgeModels" V.int)
        |> V.andMap (V.field "questionnaires" V.int)
        |> V.andMap (V.field "storage" V.int)
        |> V.andMap (V.field "users" V.int)


encode : TenantLimitsForm -> E.Value
encode form =
    E.object
        [ ( "activeUsers", E.int form.activeUsers )
        , ( "documentTemplateDrafts", E.int form.documentTemplateDrafts )
        , ( "documentTemplates", E.int form.documentTemplates )
        , ( "documents", E.int form.documents )
        , ( "knowledgeModelEditors", E.int form.knowledgeModelEditors )
        , ( "knowledgeModels", E.int form.knowledgeModels )
        , ( "questionnaires", E.int form.questionnaires )
        , ( "storage", E.int form.storage )
        , ( "users", E.int form.users )
        ]
