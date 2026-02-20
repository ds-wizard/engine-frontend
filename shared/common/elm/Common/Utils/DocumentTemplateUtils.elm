module Common.Utils.DocumentTemplateUtils exposing (getId)

import Version exposing (Version)


getId : { a | organizationId : String, templateId : String, version : Version } -> String
getId template =
    template.organizationId ++ ":" ++ template.templateId ++ ":" ++ Version.toString template.version
