const ExtensionUtils = imports.misc.extensionUtils
const Extension = ExtensionUtils.getCurrentExtension()
const Clutter = imports.gi.Clutter

const { WorkspacesView } = imports.ui.workspacesView
const { WindowManager } = imports.ui.windowManager
const { Workspace } = imports.ui.workspace
const { overview } = imports.ui.main
const { Logger } = Extension.imports.utils
const { CustomWindowManager } = Extension.imports.window.customWindowManager
const { CustomWorkspaceView } = Extension.imports.customWorkspaceView
const { CustomWorkspace } = Extension.imports.customWorkspace
const { Search } = Extension.imports.search

var initializeWindowManager = (injector, search, settings) => {
  injector.inject(CustomWindowManager, WindowManager, parent => {
    return new CustomWindowManager(search, overview, settings)
  })
}

var initializeWorkspace = (injector, settings, overlays, windowOverlayFactory) => {
  const logger = new Logger('Custom Workspace', settings)

  injector.inject(CustomWorkspace, Workspace, parent => {
    return new CustomWorkspace(logger, overlays, windowOverlayFactory, parent)
  })
}

var initializeWorkspaceView = (injector, logger, search, windowSelector, settings, overlays) => {
  logger.info('Initialize WorkspaceView')
  injector.inject(CustomWorkspaceView, WorkspacesView, parent => {
    var workspaceManager = global.workspace_manager
    if (!workspaceManager) {
      workspaceManager = global.screen
    }

    return new CustomWorkspaceView(
      logger,
      search,
      windowSelector,
      global.stage,
      parent._workspaces,
      workspaceManager,
      Clutter,
      settings,
      overlays
    )
  })
}

var initializeSearch = (settings) => {
  return new Search(overview, new Logger('Search', settings))
}


