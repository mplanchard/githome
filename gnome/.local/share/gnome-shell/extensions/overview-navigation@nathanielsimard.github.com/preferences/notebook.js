const ExtensionUtils = imports.misc.extensionUtils
const Extension = ExtensionUtils.getCurrentExtension()
const Gtk = imports.gi.Gtk
const { Widget } = Extension.imports.preferences.widget

var NotebookPage = class NotebookPage extends Widget {
  constructor (name) {
    super(
      new Gtk.Box({
        'margin-top': 10,
        spacing: 5
      })
    )
    this.name = name
    this.parent.set_orientation(Gtk.Orientation.VERTICAL)
  }

  register (notebook) {
    const label = new Gtk.Label({ label: this.name })
    notebook.append_page(this.parent, label)
  }
}


