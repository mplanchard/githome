const ExtensionUtils = imports.misc.extensionUtils
const Extension = ExtensionUtils.getCurrentExtension()
const Clutter = imports.gi.Clutter

const { Label } = Extension.imports.components
const { WorkspacesView } = imports.ui.workspacesView
const { WindowOverlay } = imports.ui.workspace
const { WindowManager } = imports.ui.windowManager
const { overview } = imports.ui.main
const { Logger } = Extension.imports.utils
const { CustomWindowManager } = Extension.imports.window.customWindowManager
const { CustomWindowOverlay } = Extension.imports.window.customWindowOverlay
const { CustomWorkspaceView } = Extension.imports.customWorkspaceView
const { Search } = Extension.imports.search

var initializeWindowManager = (injector, search, settings) => {
  injector.inject(CustomWindowManager, WindowManager, parent => {
    return new CustomWindowManager(search, overview, settings)
  })
}

var initializeWindowOverlay = (injector, windowSelector, logger, overlays, settings) => {
  injector.inject(CustomWindowOverlay, WindowOverlay, parent => {
    const customWindow = new CustomWindowOverlay(
      logger,
      windowSelector,
      new Label(settings, parent._parentActor),
      parent._windowClone,
      parent._windowClone.metaWindow,
      3,
      overlays,
      settings
    )
    overlays.addWindow(customWindow)
    return customWindow
  })
}

var initializeWorkspaceView = (injector, logger, search, windowSelector, settings, overlays) => {
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


