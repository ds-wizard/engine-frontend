module Common.Data.WizardRolePermission exposing
    ( all
    , allGroups
    , allSettingsManageDescriptor
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
    , peoplePermissionsGroup
    , projectPermissionsGroup
    , projectTemplatesManage
    , projectTemplatesManageDescriptor
    , projectsComment
    , projectsCommentDescriptor
    , projectsCreate
    , projectsCreateDescriptor
    , projectsEdit
    , projectsEditDescriptor
    , projectsManage
    , projectsManageDescriptor
    , projectsView
    , projectsViewDescriptor
    , rolesManage
    , rolesManageDescriptor
    , settingsManage
    , settingsManageDescriptor
    , settingsPermissionsGroup
    , tenantsManage
    , userGroupsCreate
    , userGroupsCreateDescriptor
    , userGroupsManage
    , userGroupsManageDescriptor
    , usersManage
    , usersManageDescriptor
    , workspaceSettingsManage
    , workspaceSettingsManageDescriptor
    )

{-| The permissions a role is made of, each with where it can be given
(`PermissionScope`), and the groups a role editor shows them in.

The strings are `resource.action`. `devUse` and `tenantsManage` are the
instance operator's: no role editor offers them.

-}

import Common.Api.Models.RolePermission as RolePermission exposing (PermissionDescriptor, PermissionGroup, PermissionScope(..), RolePermission)
import Gettext exposing (gettext)


all : List PermissionDescriptor
all =
    List.concatMap .permissions allGroups


allGroups : List PermissionGroup
allGroups =
    [ projectPermissionsGroup
    , knowledgeModelsPermissionsGroup
    , documentTemplatesPermissionsGroup
    , peoplePermissionsGroup
    , settingsPermissionsGroup
    ]


projectPermissionsGroup : PermissionGroup
projectPermissionsGroup =
    { label = gettext "Project Permissions"
    , permissions =
        [ projectsCreateDescriptor
        , projectTemplatesManageDescriptor
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


peoplePermissionsGroup : PermissionGroup
peoplePermissionsGroup =
    { label = gettext "People Permissions"
    , permissions =
        [ usersManageDescriptor
        , rolesManageDescriptor
        , userGroupsCreateDescriptor
        , userGroupsManageDescriptor
        ]
    }


settingsPermissionsGroup : PermissionGroup
settingsPermissionsGroup =
    { label = gettext "Settings Permissions"
    , permissions = [ allSettingsManageDescriptor ]
    }


devUse : RolePermission
devUse =
    RolePermission.fromString "dev.use"


tenantsManage : RolePermission
tenantsManage =
    RolePermission.fromString "tenants.manage"



--


projectsView : RolePermission
projectsView =
    RolePermission.fromString "projects.view"


projectsViewDescriptor : PermissionDescriptor
projectsViewDescriptor =
    { permission = projectsView
    , scope = OrganizationAndWorkspace
    , label = gettext "View ALL Projects"
    , description = gettext "Allows users to view ALL Projects, regardless of Project sharing and visibility settings."
    , impliedPermissions = []
    }



--


projectsComment : RolePermission
projectsComment =
    RolePermission.fromString "projects.comment"


projectsCommentDescriptor : PermissionDescriptor
projectsCommentDescriptor =
    { permission = projectsComment
    , scope = OrganizationAndWorkspace
    , label = gettext "Comment on ALL Projects"
    , description = gettext "Allows users to comment on ALL Projects, regardless of Project sharing and visibility settings.\n\nThis permission also requires the **View ALL Projects** permission."
    , impliedPermissions = [ projectsView ]
    }



--


projectsEdit : RolePermission
projectsEdit =
    RolePermission.fromString "projects.edit"


projectsEditDescriptor : PermissionDescriptor
projectsEditDescriptor =
    { permission = projectsEdit
    , scope = OrganizationAndWorkspace
    , label = gettext "Edit ALL Projects"
    , description = gettext "Allows users to edit ALL Projects, regardless of Project sharing and visibility settings.\n\nThis permission also requires the **View ALL Projects** and **Comment on ALL Projects** permissions."
    , impliedPermissions = [ projectsView, projectsComment ]
    }



--


projectsManage : RolePermission
projectsManage =
    RolePermission.fromString "projects.manage"


projectsManageDescriptor : PermissionDescriptor
projectsManageDescriptor =
    { permission = projectsManage
    , scope = OrganizationAndWorkspace
    , label = gettext "Manage ALL Projects"
    , description = gettext "Allows users to manage ALL Projects as if they were owners, regardless of Project sharing and visibility settings.\n\nThis permission also requires the **View ALL Projects**, **Comment on ALL Projects**, and **Edit ALL Projects** permissions."
    , impliedPermissions = [ projectsView, projectsComment, projectsEdit ]
    }



--


projectTemplatesManage : RolePermission
projectTemplatesManage =
    RolePermission.fromString "projects.manageTemplates"


projectTemplatesManageDescriptor : PermissionDescriptor
projectTemplatesManageDescriptor =
    { permission = projectTemplatesManage
    , scope = OrganizationAndWorkspace
    , label = gettext "Manage Project Templates"
    , description = gettext "Allows users to set Projects as Project templates.\n\nUsers with this permission can also create new Projects directly from Knowledge Models, even when project creation is restricted to templates."
    , impliedPermissions = []
    }



--


projectsCreate : RolePermission
projectsCreate =
    RolePermission.fromString "projects.create"


projectsCreateDescriptor : PermissionDescriptor
projectsCreateDescriptor =
    { permission = projectsCreate
    , scope = OrganizationAndWorkspace
    , label = gettext "Create Projects"
    , description = gettext "Allows users to create new Projects."
    , impliedPermissions = []
    }



--


knowledgeModelEditorsUse : RolePermission
knowledgeModelEditorsUse =
    RolePermission.fromString "knowledgeModels.useEditor"


knowledgeModelEditorsUseDescriptor : PermissionDescriptor
knowledgeModelEditorsUseDescriptor =
    { permission = knowledgeModelEditorsUse
    , scope = OrganizationAndWorkspace
    , label = gettext "Use Knowledge Model Editor"
    , description = gettext "Allows users to view, create, edit, and delete Knowledge Model Editors, migrate them, and publish Knowledge Models from them.\n\nThis permission also requires the **Manage Knowledge Models** permission."
    , impliedPermissions = [ knowledgeModelsManage ]
    }



--


knowledgeModelsManage : RolePermission
knowledgeModelsManage =
    RolePermission.fromString "knowledgeModels.manage"


knowledgeModelsManageDescriptor : PermissionDescriptor
knowledgeModelsManageDescriptor =
    { permission = knowledgeModelsManage
    , scope = OrganizationAndWorkspace
    , label = gettext "Manage Knowledge Models"
    , description = gettext "Allows users to import, export, and delete Knowledge Models, as well as set them as deprecated, restore them, or set them as public.\n\nThis permission also allows users to manage Knowledge Model Secrets."
    , impliedPermissions = []
    }



--


documentTemplateEditorsUse : RolePermission
documentTemplateEditorsUse =
    RolePermission.fromString "documentTemplates.useEditor"


documentTemplateEditorUseDescriptor : PermissionDescriptor
documentTemplateEditorUseDescriptor =
    { permission = documentTemplateEditorsUse
    , scope = OrganizationAndWorkspace
    , label = gettext "Use Document Template Editor"
    , description = gettext "Allows users to view, create, edit, and delete Document Template Editors, as well as publish Document Templates from them.\n\nThis permission also requires the **Manage Document Templates** permission."
    , impliedPermissions = [ documentTemplatesManage ]
    }



--


documentTemplatesManage : RolePermission
documentTemplatesManage =
    RolePermission.fromString "documentTemplates.manage"


documentTemplatesManageDescriptor : PermissionDescriptor
documentTemplatesManageDescriptor =
    { permission = documentTemplatesManage
    , scope = OrganizationAndWorkspace
    , label = gettext "Manage Document Templates"
    , description = gettext "Allows users to import, export, and delete Document Templates, as well as set them as deprecated or restore them."
    , impliedPermissions = []
    }



--


usersManage : RolePermission
usersManage =
    RolePermission.fromString "users.manage"


usersManageDescriptor : PermissionDescriptor
usersManageDescriptor =
    { permission = usersManage
    , scope = OrganizationOnly
    , label = gettext "Manage Users"
    , description = gettext "Allows users to view, create, edit, and delete user accounts, and to set their role."
    , impliedPermissions = []
    }



--


rolesManage : RolePermission
rolesManage =
    RolePermission.fromString "roles.manage"


rolesManageDescriptor : PermissionDescriptor
rolesManageDescriptor =
    { permission = rolesManage
    , scope = OrganizationAndWorkspace
    , label = gettext "Manage Roles"
    , description = gettext "Allows users to create, edit, and delete roles and to choose the default role."
    , impliedPermissions = []
    }



--


userGroupsCreate : RolePermission
userGroupsCreate =
    RolePermission.fromString "userGroups.create"


userGroupsCreateDescriptor : PermissionDescriptor
userGroupsCreateDescriptor =
    { permission = userGroupsCreate
    , scope = OrganizationAndWorkspace
    , label = gettext "Create User Groups"
    , description = gettext "Allows users to create new user groups."
    , impliedPermissions = []
    }


userGroupsManage : RolePermission
userGroupsManage =
    RolePermission.fromString "userGroups.manage"


userGroupsManageDescriptor : PermissionDescriptor
userGroupsManageDescriptor =
    { permission = userGroupsManage
    , scope = OrganizationAndWorkspace
    , label = gettext "Manage User Groups"
    , description = gettext "Allows users to edit and delete ALL user groups, not only their own."
    , impliedPermissions = []
    }



--


workspaceSettingsManage : RolePermission
workspaceSettingsManage =
    RolePermission.fromString "workspaceSettings.manage"


workspaceSettingsManageDescriptor : PermissionDescriptor
workspaceSettingsManageDescriptor =
    { permission = workspaceSettingsManage
    , scope = OrganizationAndWorkspace
    , label = gettext "Manage General Settings"
    , description = gettext "Allows users to manage the settings of projects, document submission, support, the dashboard and the menu, and to configure plugins."
    , impliedPermissions = []
    }



--


settingsManage : RolePermission
settingsManage =
    RolePermission.fromString "settings.manage"


settingsManageDescriptor : PermissionDescriptor
settingsManageDescriptor =
    { permission = settingsManage
    , scope = OrganizationOnly
    , label = gettext "Manage Settings"
    , description = gettext "Allows users to manage authentication, the login screen, look and feel, features, locales, the registry, and which plugins are enabled."
    , impliedPermissions = []
    }


{-| One switch for every setting, where the general settings are not told from
the rest: `settings.manage`, and with it `workspaceSettings.manage`, which no
switch offers then.
-}
allSettingsManageDescriptor : PermissionDescriptor
allSettingsManageDescriptor =
    { permission = settingsManage
    , scope = OrganizationOnly
    , label = gettext "Manage Settings"
    , description = gettext "Allows users to manage all settings: authentication, login screen, look and feel, features, locales, registry, projects, document submission, support, the dashboard and the menu, and plugins."
    , impliedPermissions = [ workspaceSettingsManage ]
    }
