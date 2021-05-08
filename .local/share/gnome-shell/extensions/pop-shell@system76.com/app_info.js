const Me = imports.misc.extensionUtils.getCurrentExtension();
const { Gio } = imports.gi;
const error = Me.imports.error;
const result = Me.imports.result;
const once_cell = Me.imports.once_cell;
const { Err, Ok } = result;
const OnceCell = once_cell.OnceCell;
var AppInfo = class AppInfo {
    constructor(path, app_info) {
        var _a;
        this.categories_ = new OnceCell();
        this.comment_ = new OnceCell();
        this.exec_ = new OnceCell();
        this.generic = new OnceCell();
        this.keywords_ = new OnceCell();
        this.app_info = app_info;
        this.desktop_name = path.split('/').slice(-1)[0];
        const pos = this.desktop_name.lastIndexOf('.');
        this.desktop_name = this.desktop_name.slice(0, pos);
        this.name_ = (_a = this.string("Name")) !== null && _a !== void 0 ? _a : "unknown";
    }
    static try_from(path) {
        const app_info = Gio.DesktopAppInfo.new_from_filename(path);
        return app_info
            ? Ok(new AppInfo(path, app_info))
            : Err(new error.Error(`failed to open app info for ${path}`));
    }
    get filename() {
        return this.app_info.filename;
    }
    categories() {
        return this.categories_.get_or_init(() => this.app_info.get_categories());
    }
    comment() {
        return this.comment_.get_or_init(() => this.string("Comment"));
    }
    exec() {
        return this.exec_.get_or_init(() => this.string("Exec"));
    }
    icon() {
        return this.app_info.get_icon();
    }
    generic_name() {
        return this.generic.get_or_init(() => this.app_info.get_generic_name());
    }
    keywords() {
        return this.keywords_.get_or_init(() => this.app_info.get_keywords());
    }
    launch() {
        return this.app_info.launch([], null)
            ? Ok(null)
            : Err(new error.Error(`failed to launch ${this.filename}`));
    }
    name() {
        return this.name_;
    }
    display() {
        return `AppInfo {
    categories: ${this.categories()},
    comment: ${this.comment()},
    exe: ${this.exec()}
    filename: ${this.filename},
    generic name: ${this.generic_name()},
    icon: ${this.icon()},
    keywords: ${this.keywords()},
    name: ${this.name()},
}`;
    }
    string(name) {
        return this.app_info.get_string(name);
    }
}
function* load_desktop_entries(path) {
    let dir = Gio.file_new_for_path(path);
    if (!dir.query_exists(null)) {
        return new error.Error(`desktop path is missing: ${path}`);
    }
    let entries, entry;
    try {
        entries = dir.enumerate_children('standard::*', Gio.FileQueryInfoFlags.NONE, null);
    }
    catch (e) {
        return new error.Error(String(e))
            .context(`failed to enumerate children of ${path}`);
    }
    while ((entry = entries.next_file(null)) != null) {
        const ft = entry.get_file_type();
        if (!(ft == Gio.FileType.REGULAR || ft == Gio.FileType.SYMBOLIC_LINK)) {
            continue;
        }
        const name = entry.get_name();
        if (name.indexOf('.desktop') > -1) {
            const desktop_path = path + '/' + name;
            const info = AppInfo.try_from(desktop_path);
            if (info.kind === result.OK) {
                const exec = info.value.exec();
                const show = info.value.app_info.should_show()
                    || (exec === null || exec === void 0 ? void 0 : exec.startsWith('gnome-control-center'));
                if (show)
                    yield info;
            }
        }
    }
}
//# sourceMappingURL=app_info.js.map