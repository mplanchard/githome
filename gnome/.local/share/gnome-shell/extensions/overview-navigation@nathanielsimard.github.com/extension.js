const ExtensionUtils = imports.misc.extensionUtils
const Extension = ExtensionUtils.getCurrentExtension()
const { Main } = Extension.imports.app
const { Logger } = Extension.imports.utils

let main, logger
/*eslint-disable */
function init() {
  /* eslint-enable */
  try {
    logger = new Logger('Extension')
    main = new Main()
    logger.info('Initialized')
  } catch (err) {
    logger.error(err)
  }
}

/*eslint-disable */
function enable() {
  /* eslint-enable */
  try {
    logger.info('Enabling extension ...')
    main.start()
    logger.info('Enabled')
  } catch (err) {
    logger.error(err)
  }
}

/*eslint-disable */
function disable() {
  /* eslint-enable */
  try {
    logger.info('Disabling extension ...')
    main.stop()
    logger.info('Disabled')
  } catch (err) {
    logger.error(err)
  }
}
