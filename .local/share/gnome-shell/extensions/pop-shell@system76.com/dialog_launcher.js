const Me = imports.misc.extensionUtils.getCurrentExtension();
const { Clutter, Gio, GLib, Meta } = imports.gi;
const app_info = Me.imports.app_info;
const error = Me.imports.error;
const lib = Me.imports.lib;
const log = Me.imports.log;
const result = Me.imports.result;
const search = Me.imports.dialog_search;
const launch = Me.imports.launcher_service;
const plugins = Me.imports.launcher_plugins;
const levenshtein = Me.imports.levenshtein;
const { OK } = result;
const HOME_DIR = GLib.get_home_dir();
const DATA_DIRS = GLib.get_system_data_dirs();
const SEARCH_PATHS = [
    ["System", "/usr/share/applications/"],
    ["System (Local)", "/usr/local/share/applications/"],
    ["User", HOME_DIR + "/.local/share/applications/"],
    ["Flatpak (System)", "/var/lib/flatpak/exports/share/applications/"],
    ["Flatpak (User)", HOME_DIR + "/.local/share/flatpak/exports/share/applications/"],
    ["Snap (System)", "/var/lib/snapd/desktop/applications/"]
];
var Launcher = class Launcher extends search.Search {
    constructor(ext) {
        let cancel = () => {
            ext.overlay.visible = false;
            this.stop_services(ext);
        };
        let search = (pattern) => {
            this.options.splice(0);
            if (pattern.length == 0) {
                this.list_workspace(ext);
                return this.options;
            }
            this.last_plugin = null;
            let windows = new Array();
            this.service.query(ext, pattern, (plugin, response) => {
                if (response.event === "queried") {
                    for (const selection of response.selections) {
                        if (!this.last_plugin)
                            this.last_plugin = plugin;
                        let icon = null;
                        if (selection.icon) {
                            icon = { name: selection.icon };
                        }
                        else if (selection.content_type) {
                            icon = { gicon: Gio.content_type_get_icon(selection.content_type) };
                        }
                        this.options.push(new launch.SearchOption(selection.name, selection.description, plugin.config.icon, icon, this.icon_size(), { plugin, id: selection.id }));
                    }
                }
            });
            const needles = pattern.toLowerCase().split(' ');
            const contains_pattern = (haystack, needles) => {
                const hay = haystack.toLowerCase();
                return needles.every((n) => hay.includes(n));
            };
            for (const window of ext.tab_list(Meta.TabList.NORMAL, null)) {
                const retain = contains_pattern(window.name(ext), needles)
                    || contains_pattern(window.meta.get_title(), needles);
                if (retain) {
                    windows.push(window_selection(ext, window, this.icon_size()));
                }
            }
            for (const [where, app] of this.desktop_apps) {
                const retain = contains_pattern(app.name(), needles)
                    || contains_pattern(app.desktop_name, needles)
                    || lib.ok(app.generic_name(), (s) => contains_pattern(s, needles))
                    || lib.ok(app.comment(), (s) => contains_pattern(s, needles))
                    || lib.ok(app.categories(), (s) => contains_pattern(s, needles));
                if (retain) {
                    const generic = app.generic_name();
                    this.options.push(new launch.SearchOption(app.name(), generic ? generic + " — " + where : where, 'application-default-symbolic', { gicon: app.icon() }, this.icon_size(), { app }));
                }
            }
            const sorter = (a, b) => {
                const a_name = a.title.toLowerCase();
                const b_name = b.title.toLowerCase();
                const pattern_lower = pattern.toLowerCase();
                let a_name_weight = levenshtein.compare(pattern_lower, a_name);
                let b_name_weight = levenshtein.compare(pattern_lower, b_name);
                if (a.description) {
                    a_name_weight = Math.min(a_name_weight, levenshtein.compare(pattern_lower, a.description.toLowerCase()));
                }
                if (b.description) {
                    b_name_weight = Math.min(b_name_weight, levenshtein.compare(pattern_lower, b.description.toLowerCase()));
                }
                return a_name_weight > b_name_weight ? 1 : 0;
            };
            windows.sort(sorter);
            this.options.sort(sorter);
            this.options = windows.concat(this.options);
            this.options.splice(this.list_max());
            return this.options;
        };
        let select = (id) => {
            ext.overlay.visible = false;
            if (id >= this.options.length)
                return;
            const selected = this.options[id];
            if (selected) {
                if ("window" in selected.id) {
                    const win = selected.id.window;
                    if (win.workspace_id() == ext.active_workspace()) {
                        const { x, y, width, height } = win.rect();
                        ext.overlay.x = x;
                        ext.overlay.y = y;
                        ext.overlay.width = width;
                        ext.overlay.height = height;
                        ext.overlay.visible = true;
                    }
                }
            }
        };
        let apply = (index) => {
            ext.overlay.visible = false;
            const selected = this.options[index];
            if (typeof selected === 'undefined') {
                return true;
            }
            const option = selected.id;
            if ("window" in option) {
                option.window.activate();
            }
            else if ("app" in option) {
                const result = option.app.launch();
                if (result instanceof error.Error) {
                    log.error(result.format());
                }
                else {
                    let exec_name = option.app.app_info.get_executable();
                    if (exec_name === "gnome-control-center") {
                        for (const window of ext.tab_list(Meta.TabList.NORMAL, null)) {
                            if (window.meta.get_title() === "Settings") {
                                window.meta.activate(global.get_current_time());
                                break;
                            }
                        }
                    }
                }
            }
            else if ("plugin" in option) {
                const { plugin, id } = option;
                plugins.Plugin.submit(ext, plugin, id);
                const response = plugins.Plugin.listen(plugin);
                if (response) {
                    if (response.event === "fill") {
                        this.set_text(response.text);
                        return true;
                    }
                }
            }
            return false;
        };
        let complete = () => {
            if (this.last_plugin) {
                plugins.Plugin.complete(ext, this.last_plugin);
                const res = plugins.Plugin.listen(this.last_plugin);
                if (res && res.event === "fill") {
                    this.set_text(res.text);
                    return true;
                }
            }
            return false;
        };
        const quit = (id) => {
            const selected = this.options[id];
            if (typeof selected === 'undefined') {
                return true;
            }
            const option = selected.id;
            if ("window" in option) {
                option.window.meta.delete(global.get_current_time());
                cancel();
                this.close();
            }
        };
        super(cancel, search, complete, select, apply, quit);
        this.dialog.dialogLayout._dialog.y_align = Clutter.ActorAlign.START;
        this.dialog.dialogLayout._dialog.x_align = Clutter.ActorAlign.START;
        this.dialog.dialogLayout.y = 48;
        this.service = new launch.LauncherService();
        this.last_plugin = null;
        this.options = new Array();
        this.desktop_apps = new Array();
        this.mode = -1;
    }
    clear() {
        super.clear();
        this.last_plugin = null;
    }
    load_desktop_files() {
        lib.bench("load_desktop_files", () => {
            this.desktop_apps.splice(0);
            for (const [where, path] of SEARCH_PATHS) {
                for (const result of app_info.load_desktop_entries(path)) {
                    if (result.kind == OK) {
                        const value = result.value;
                        this.desktop_apps.push([where, value]);
                    }
                    else {
                        const why = result.value;
                        log.warn(why.context(`failed to load desktop app`).format());
                    }
                }
            }
            for (const _path of DATA_DIRS) {
                const path = _path.replace(/\/$/, '') + "/applications";
                for (const result of app_info.load_desktop_entries(path)) {
                    if (result.kind == OK) {
                        const value = result.value;
                        const existAt = this.desktop_apps.findIndex(([_, app]) => app.exec() == value.exec());
                        if (existAt == -1) {
                            this.desktop_apps.push(['System', value]);
                        }
                    }
                    else {
                        const why = result.value;
                        log.warn(why.context(`failed to load desktop app`).format());
                    }
                }
            }
        });
    }
    list_workspace(ext) {
        let show_all_workspaces = true;
        const active = ext.active_workspace();
        for (const window of ext.tab_list(Meta.TabList.NORMAL, null)) {
            if (show_all_workspaces || window.workspace_id() === active) {
                this.options.push(window_selection(ext, window, this.icon_size()));
                if (this.options.length == this.list_max())
                    break;
            }
        }
    }
    open(ext) {
        const mon = ext.monitor_work_area(ext.active_monitor());
        this.options.splice(0);
        this.clear();
        this.list_workspace(ext);
        this.update_search_list(this.options);
        super._open(global.get_current_time(), false);
        this.dialog.dialogLayout.x = (mon.width / 2) - (this.dialog.dialogLayout.width / 2);
        this.dialog.dialogLayout.y = (mon.height / 2) - (this.dialog.dialogLayout.height / 2);
    }
    stop_services(ext) {
        this.service.stop_services(ext);
    }
}
function window_selection(ext, window, icon_size) {
    let name = window.name(ext);
    let title = window.meta.get_title();
    return new launch.SearchOption(title, name, 'focus-windows-symbolic', {
        widget: window.icon(ext, icon_size)
    }, icon_size, { window });
}
//# sourceMappingURL=dialog_launcher.js.map