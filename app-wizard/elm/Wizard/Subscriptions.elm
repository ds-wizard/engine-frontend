module Wizard.Subscriptions exposing (subscriptions)

import Shared.Utils.Driver as Driver
import Wizard.Components.Menu.Subscriptions
import Wizard.Models exposing (Model)
import Wizard.Msgs exposing (Msg(..))
import Wizard.Pages.Auth.Subscriptions
import Wizard.Pages.Comments.Subscriptions
import Wizard.Pages.Dev.Subscriptions
import Wizard.Pages.DocumentTemplateEditors.Subscriptions
import Wizard.Pages.DocumentTemplates.Subscriptions
import Wizard.Pages.Documents.Subscriptions
import Wizard.Pages.KMEditor.Subscriptions
import Wizard.Pages.KnowledgeModels.Subscriptions
import Wizard.Pages.Locales.Subscriptions
import Wizard.Pages.ProjectActions.Subscriptions
import Wizard.Pages.ProjectFiles.Subscriptions
import Wizard.Pages.ProjectImporters.Subscriptions
import Wizard.Pages.Projects.Subscriptions
import Wizard.Pages.Public.Subscriptions
import Wizard.Pages.Tenants.Subscriptions
import Wizard.Pages.Users.Subscriptions
import Wizard.Ports as Ports
import Wizard.Routes as Routes


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        currentViewSubscriptions =
            case model.appState.route of
                Routes.TenantsRoute route ->
                    Sub.map TenantsMsg <| Wizard.Pages.Tenants.Subscriptions.subscriptions route model.tenantsModel

                Routes.DevRoute route ->
                    Sub.map AdminMsg <| Wizard.Pages.Dev.Subscriptions.subscriptions route model.adminModel

                Routes.CommentsRoute _ _ ->
                    Sub.map CommentsMsg <| Wizard.Pages.Comments.Subscriptions.subscriptions model.commentsModel

                Routes.DocumentsRoute _ ->
                    Sub.map DocumentsMsg <| Wizard.Pages.Documents.Subscriptions.subscriptions model.documentsModel

                Routes.DocumentTemplateEditorsRoute route ->
                    Wizard.Pages.DocumentTemplateEditors.Subscriptions.subscriptions DocumentTemplateEditorsMsg OnTime route model.documentTemplateEditorsModel

                Routes.DocumentTemplatesRoute route ->
                    Sub.map DocumentTemplatesMsg <| Wizard.Pages.DocumentTemplates.Subscriptions.subscriptions route model.documentTemplatesModel

                Routes.KMEditorRoute route ->
                    Wizard.Pages.KMEditor.Subscriptions.subscriptions KMEditorMsg route model.kmEditorModel

                Routes.KnowledgeModelsRoute route ->
                    Sub.map KnowledgeModelsMsg <| Wizard.Pages.KnowledgeModels.Subscriptions.subscriptions route model.kmPackagesModel

                Routes.LocalesRoute route ->
                    Sub.map LocaleMsg <| Wizard.Pages.Locales.Subscriptions.subscriptions route model.localeModel

                Routes.ProjectActionsRoute _ ->
                    Sub.map ProjectActionsMsg <| Wizard.Pages.ProjectActions.Subscriptions.subscriptions model.projectActionsModel

                Routes.ProjectFilesRoute _ ->
                    Sub.map ProjectFilesMsg <| Wizard.Pages.ProjectFiles.Subscriptions.subscriptions model.projectFilesModel

                Routes.ProjectImportersRoute _ ->
                    Sub.map ProjectImportersMsg <| Wizard.Pages.ProjectImporters.Subscriptions.subscriptions model.projectImportersModel

                Routes.ProjectsRoute route ->
                    Sub.map ProjectsMsg <| Wizard.Pages.Projects.Subscriptions.subscriptions route model.projectsModel

                Routes.PublicRoute route ->
                    Sub.map PublicMsg <| Wizard.Pages.Public.Subscriptions.subscriptions route

                Routes.UsersRoute route ->
                    Sub.map UsersMsg <| Wizard.Pages.Users.Subscriptions.subscriptions route model.users

                _ ->
                    Sub.none

        authSubscriptions =
            Wizard.Pages.Auth.Subscriptions.subscriptions model

        menuSubscriptions =
            Wizard.Components.Menu.Subscriptions.subscriptions model.menuModel

        historySubscriptions =
            Ports.historyBackCallback HistoryBackCallback

        tourSubscriptions =
            Driver.onTourDone TourDone
    in
    Sub.batch
        [ currentViewSubscriptions
        , authSubscriptions
        , menuSubscriptions
        , historySubscriptions
        , tourSubscriptions
        ]
