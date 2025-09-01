module Wizard.Tenants.Common.TenantLimitsForm exposing
    ( TenantLimitsForm
    , encode
    , init
    , validation
    )

import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Utils.Form.Field as Field
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Api.Models.TenantDetail exposing (TenantDetail)


type alias TenantLimitsForm =
    { activeUsers : Int
    , branches : Int
    , documentTemplateDrafts : Int
    , documentTemplates : Int
    , documents : Int
    , knowledgeModels : Int
    , locales : Int
    , questionnaires : Int
    , storage : Int
    , users : Int
    }


init : TenantDetail -> Form FormError TenantLimitsForm
init tenantDetail =
    let
        fields =
            [ ( "activeUsers", Field.int tenantDetail.usage.activeUsers.max )
            , ( "branches", Field.int tenantDetail.usage.branches.max )
            , ( "documentTemplateDrafts", Field.int tenantDetail.usage.documentTemplateDrafts.max )
            , ( "documentTemplates", Field.int tenantDetail.usage.documentTemplates.max )
            , ( "documents", Field.int tenantDetail.usage.documents.max )
            , ( "knowledgeModels", Field.int tenantDetail.usage.knowledgeModels.max )
            , ( "locales", Field.int tenantDetail.usage.locales.max )
            , ( "questionnaires", Field.int tenantDetail.usage.questionnaires.max )
            , ( "storage", Field.int tenantDetail.usage.storage.max )
            , ( "users", Field.int tenantDetail.usage.users.max )
            ]
    in
    Form.initial fields validation


validation : Validation FormError TenantLimitsForm
validation =
    V.succeed TenantLimitsForm
        |> V.andMap (V.field "activeUsers" V.int)
        |> V.andMap (V.field "branches" V.int)
        |> V.andMap (V.field "documentTemplateDrafts" V.int)
        |> V.andMap (V.field "documentTemplates" V.int)
        |> V.andMap (V.field "documents" V.int)
        |> V.andMap (V.field "knowledgeModels" V.int)
        |> V.andMap (V.field "locales" V.int)
        |> V.andMap (V.field "questionnaires" V.int)
        |> V.andMap (V.field "storage" V.int)
        |> V.andMap (V.field "users" V.int)


encode : TenantLimitsForm -> E.Value
encode form =
    E.object
        [ ( "activeUsers", E.int form.activeUsers )
        , ( "branches", E.int form.branches )
        , ( "documentTemplateDrafts", E.int form.documentTemplateDrafts )
        , ( "documentTemplates", E.int form.documentTemplates )
        , ( "documents", E.int form.documents )
        , ( "knowledgeModels", E.int form.knowledgeModels )
        , ( "locales", E.int form.locales )
        , ( "questionnaires", E.int form.questionnaires )
        , ( "storage", E.int form.storage )
        , ( "users", E.int form.users )
        ]
