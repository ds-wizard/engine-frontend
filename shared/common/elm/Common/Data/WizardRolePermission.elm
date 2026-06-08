module Common.Data.WizardRolePermission exposing
    ( all
    , allGroups
    , devUse
    , documentTemplateEditorUseDescriptor
    , documentTemplateEditorsUse
    , documentTemplatesManage
    , documentTemplatesManageDescriptor
    , documentTemplatesPermissionsGroup
    , knowledgeModelEditorsUse
    , knowledgeModelEditorsUseDescriptor
    , knowledgeModelsManage
    , knowledgeModelsManageDescriptor
    , knowledgeModelsPermissionsGroup
    , localesManage
    , localesManageDescriptor
    , projectPermissionsGroup
    , projectTemplatesManage
    , projectTemplatesManageDescriptor
    , projectsComment
    , projectsCommentDescriptor
    , projectsEdit
    , projectsEditDescriptor
    , projectsManage
    , projectsManageDescriptor
    , projectsView
    , projectsViewDescriptor
    , settingsManage
    , settingsManageDescriptor
    , settingsPermissionsGroup
    , tenantsManage
    , usersManage
    , usersManageDescriptor
    )

import Common.Api.Models.RolePermission as RolePermission exposing (PermissionDescriptor, PermissionGroup, RolePermission)
import Gettext exposing (gettext)


all : List PermissionDescriptor
all =
    [ projectTemplatesManageDescriptor
    , projectsViewDescriptor
    , projectsCommentDescriptor
    , projectsEditDescriptor
    , projectsManageDescriptor
    , knowledgeModelEditorsUseDescriptor
    , knowledgeModelsManageDescriptor
    , documentTemplateEditorUseDescriptor
    , documentTemplatesManageDescriptor
    , localesManageDescriptor
    , usersManageDescriptor
    , settingsManageDescriptor
    ]


allGroups : List PermissionGroup
allGroups =
    [ projectPermissionsGroup
    , knowledgeModelsPermissionsGroup
    , documentTemplatesPermissionsGroup
    , settingsPermissionsGroup
    ]


projectPermissionsGroup : PermissionGroup
projectPermissionsGroup =
    { label = gettext "Project Permissions"
    , permissions =
        [ projectTemplatesManageDescriptor
        , projectsViewDescriptor
        , projectsCommentDescriptor
        , projectsEditDescriptor
        , projectsManageDescriptor
        ]
    }


knowledgeModelsPermissionsGroup : PermissionGroup
knowledgeModelsPermissionsGroup =
    { label = gettext "Knowledge Model Permissions"
    , permissions =
        [ knowledgeModelEditorsUseDescriptor
        , knowledgeModelsManageDescriptor
        ]
    }


documentTemplatesPermissionsGroup : PermissionGroup
documentTemplatesPermissionsGroup =
    { label = gettext "Document Template Permissions"
    , permissions =
        [ documentTemplateEditorUseDescriptor
        , documentTemplatesManageDescriptor
        ]
    }


settingsPermissionsGroup : PermissionGroup
settingsPermissionsGroup =
    { label = gettext "Settings Permissions"
    , permissions =
        [ localesManageDescriptor
        , usersManageDescriptor
        , settingsManageDescriptor
        ]
    }


devUse : RolePermission
devUse =
    RolePermission.fromString "DevUseRolePermission"



--


documentTemplateEditorsUse : RolePermission
documentTemplateEditorsUse =
    RolePermission.fromString "DocumentTemplateEditorsUseRolePermission"


documentTemplateEditorUseDescriptor : PermissionDescriptor
documentTemplateEditorUseDescriptor =
    { permission = documentTemplateEditorsUse
    , label = gettext "Use Document Template Editor"
    , description = gettext "Allows users to view, create, edit, and delete Document Template Editors, as well as publish Document Templates from them.\n\nThis permission also requires the **Manage Document Templates** permission."
    , impliedPermissions = [ documentTemplatesManage ]
    }



--


documentTemplatesManage : RolePermission
documentTemplatesManage =
    RolePermission.fromString "DocumentTemplatesManageRolePermission"


documentTemplatesManageDescriptor : PermissionDescriptor
documentTemplatesManageDescriptor =
    { permission = documentTemplatesManage
    , label = gettext "Manage Document Templates"
    , description = gettext "Allows users to import, export, and delete Document Templates, as well as set them as deprecated or restore them."
    , impliedPermissions = []
    }



--


knowledgeModelEditorsUse : RolePermission
knowledgeModelEditorsUse =
    RolePermission.fromString "KnowledgeModelEditorsUseRolePermission"


knowledgeModelEditorsUseDescriptor : PermissionDescriptor
knowledgeModelEditorsUseDescriptor =
    { permission = knowledgeModelEditorsUse
    , label = gettext "Use Knowledge Model Editor"
    , description = gettext "Allows users to view, create, edit, and delete Knowledge Model Editors, migrate them, and publish Knowledge Models from them.\n\nThis permission also requires the **Manage Knowledge Models** permission."
    , impliedPermissions = [ knowledgeModelsManage ]
    }



--


knowledgeModelsManage : RolePermission
knowledgeModelsManage =
    RolePermission.fromString "KnowledgeModelsManageRolePermission"


knowledgeModelsManageDescriptor : PermissionDescriptor
knowledgeModelsManageDescriptor =
    { permission = knowledgeModelsManage
    , label = gettext "Manage Knowledge Models"
    , description = gettext "Allows users to import, export, and delete Knowledge Models, as well as set them as deprecated, restore them, or set them as public.\n\nThis permission also allows users to manage Knowledge Model Secrets."
    , impliedPermissions = []
    }



--


localesManage : RolePermission
localesManage =
    RolePermission.fromString "LocalesManageRolePermission"


localesManageDescriptor : PermissionDescriptor
localesManageDescriptor =
    { permission = localesManage
    , label = gettext "Manage Locales"
    , description = gettext "Allows users to create, import, export, and delete Locales, as well as set a Locale as the default and enable or disable Locales."
    , impliedPermissions = []
    }



--


projectsComment : RolePermission
projectsComment =
    RolePermission.fromString "ProjectsCommentRolePermission"


projectsCommentDescriptor : PermissionDescriptor
projectsCommentDescriptor =
    { permission = projectsComment
    , label = gettext "Comment on ALL Projects"
    , description = gettext "Allows users to comment on ALL Projects, regardless of Project sharing and visibility settings.\n\nThis permission also requires the **View ALL Projects** permission."
    , impliedPermissions = [ projectsView ]
    }



--


projectsEdit : RolePermission
projectsEdit =
    RolePermission.fromString "ProjectsEditRolePermission"


projectsEditDescriptor : PermissionDescriptor
projectsEditDescriptor =
    { permission = projectsEdit
    , label = gettext "Edit ALL Projects"
    , description = gettext "Allows users to edit ALL Projects, regardless of Project sharing and visibility settings.\n\nThis permission also requires the **View ALL Projects** and **Comment on ALL Projects** permissions."
    , impliedPermissions = [ projectsView, projectsComment ]
    }



--


projectsManage : RolePermission
projectsManage =
    RolePermission.fromString "ProjectsManageRolePermission"


projectsManageDescriptor : PermissionDescriptor
projectsManageDescriptor =
    { permission = projectsManage
    , label = gettext "Manage ALL Projects"
    , description = gettext "Allows users to manage ALL Projects as if they were owners, regardless of Project sharing and visibility settings.\n\nThis permission also requires the **View ALL Projects**, **Comment on ALL Projects**, and **Edit ALL Projects** permissions."
    , impliedPermissions = [ projectsView, projectsComment, projectsEdit ]
    }



--


projectsView : RolePermission
projectsView =
    RolePermission.fromString "ProjectsViewRolePermission"


projectsViewDescriptor : PermissionDescriptor
projectsViewDescriptor =
    { permission = projectsView
    , label = gettext "View ALL Projects"
    , description = gettext "Allows users to view ALL Projects, regardless of Project sharing and visibility settings."
    , impliedPermissions = []
    }



--


projectTemplatesManage : RolePermission
projectTemplatesManage =
    RolePermission.fromString "ProjectTemplatesManageRolePermission"


projectTemplatesManageDescriptor : PermissionDescriptor
projectTemplatesManageDescriptor =
    { permission = projectTemplatesManage
    , label = gettext "Manage Project Templates"
    , description = gettext "Allows users to set Projects as Project templates.\n\nUsers with this permission can also create new Projects directly from Knowledge Models, even when project creation is restricted to templates."
    , impliedPermissions = []
    }



--


settingsManage : RolePermission
settingsManage =
    RolePermission.fromString "SettingsManageRolePermission"


settingsManageDescriptor : PermissionDescriptor
settingsManageDescriptor =
    { permission = settingsManage
    , label = gettext "Manage Settings"
    , description = gettext "Allows users to view and manage application settings."
    , impliedPermissions = []
    }



--


tenantsManage : RolePermission
tenantsManage =
    RolePermission.fromString "TenantsManageRolePermission"



--


usersManage : RolePermission
usersManage =
    RolePermission.fromString "UsersManageRolePermission"


usersManageDescriptor : PermissionDescriptor
usersManageDescriptor =
    { permission = usersManage
    , label = gettext "Manage Users"
    , description = gettext "Allows users to view, create, edit, and delete user accounts."
    , impliedPermissions = []
    }
