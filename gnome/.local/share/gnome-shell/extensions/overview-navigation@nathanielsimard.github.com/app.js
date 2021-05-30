const ExtensionUtils = imports.misc.extensionUtils
const Extension = ExtensionUtils.getCurrentExtension()
const { CustomWindowOverlaySubject } = Extension.imports.subject.customWindowOverlaySubject
const { Injector } = Extension.imports.injector
const { Logger } = Extension.imports.utils
const { TagGenerator } = Extension.imports.tagGenerator
const { NATURAL_ORDERING, LOWER_CASE_KEY_SYMBOLS, UPPER_CASE_KEY_SYMBOLS } = Extension.imports.keySymbols
const {
  initializeWindowManager,
  initializeWindowOverlay,
  initializeWorkspaceView,
  initializeSearch
} = Extension.imports.bootstrap.customComponents

const Settings = Extension.imports.settings
const WindowSelector = Extension.imports.window.windowSelector

var Main = class Main {
  constructor () {
    const keySymbols = { ...LOWER_CASE_KEY_SYMBOLS, ...UPPER_CASE_KEY_SYMBOLS }
    const settings = Settings.initialize()
    const overlays = new CustomWindowOverlaySubject(new Logger('CustomWindowOverlays', settings))

    const tagGenerator = new TagGenerator(keySymbols, NATURAL_ORDERING)
    const windowSelector = WindowSelector.create(keySymbols, tagGenerator, new Logger('WindowSelector', settings))

    this.search = initializeSearch(settings)
    this.injector = new Injector(new Logger('Injector', settings))

    initializeWindowManager(this.injector, this.search, settings)
    initializeWindowOverlay(
      this.injector,
      windowSelector,
      new Logger('CustomWindowOverlay', settings),
      overlays,
      settings
    )

    initializeWorkspaceView(
      this.injector,
      new Logger('CustomWorkspaceView', settings),
      this.search,
      windowSelector,
      settings,
      overlays
    )
  }

  start () {
    this.injector.enable()
  }

  stop () {
    this.injector.disable()
    this.search.enable()
  }
}


