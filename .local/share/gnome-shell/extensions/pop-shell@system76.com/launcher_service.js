const Me = imports.misc.extensionUtils.getCurrentExtension();
const { Clutter, Gio, GLib, Pango, St } = imports.gi;
const plugins = Me.imports.launcher_plugins;
const { Plugin } = plugins;
const plugin_scripts = Me.imports.plugin_scripts;
const plugin_shell = Me.imports.plugin_shell;
var BUILTINS = [
    {
        backend: {
            builtin: (() => {
                const plug = new plugin_scripts.ScriptsBuiltin();
                plug.init();
                return plug;
            })()
        },
        config: {
            name: "Shell Scripts",
            description: "User-defined shell scripts to execute",
            pattern: "",
            exec: "",
            icon: "utilities-terminal"
        },
        pattern: null
    },
    {
        backend: {
            builtin: new plugin_shell.ShellBuiltin()
        },
        config: {
            name: "Shell Shortcuts",
            description: "Access shell features from the keyboard",
            pattern: "",
            exec: "",
            icon: `${Me.path}/icons/pop-shell-auto-on-symbolic.svg`
        },
        pattern: null
    }
];
const LOCAL_PLUGINS = GLib.get_home_dir() + "/.local/share/pop-shell/launcher/";
const SYSTEM_PLUGINS = "/usr/lib/pop-shell/launcher/";
var LauncherService = class LauncherService {
    constructor() {
        this.plugins = new Map();
        this.register_plugins();
    }
    destroy(ext) {
        for (const plugin of this.plugins.values())
            Plugin.quit(ext, plugin);
    }
    query(ext, query, callback) {
        for (const plugin of this.match_query(ext, query)) {
            if (Plugin.query(ext, plugin, query)) {
                const res = Plugin.listen(plugin);
                if (res)
                    callback(plugin, res);
            }
            else {
                Plugin.quit(ext, plugin);
            }
        }
    }
    stop_services(ext) {
        for (const plugin of this.plugins.values()) {
            if ('proc' in plugin.backend) {
                Plugin.quit(ext, plugin);
                plugin.backend.proc = null;
            }
        }
    }
    register_plugins() {
        this.register_plugin_directory(LOCAL_PLUGINS);
        this.register_plugin_directory(SYSTEM_PLUGINS);
    }
    register_plugin_directory(directory) {
        global.log(`checking for plugins in ${directory}`);
        let dir = Gio.file_new_for_path(directory);
        if (!dir.query_exists(null))
            return;
        try {
            let entries = dir.enumerate_children('standard::*', Gio.FileQueryInfoFlags.NONE, null);
            let entry;
            while ((entry = entries.next_file(null)) !== null) {
                if (entry.get_file_type() === 2) {
                    let name = entry.get_name();
                    const metapath = directory + '/' + name + '/meta.json';
                    const metadata = Gio.file_new_for_path(metapath);
                    if (!metadata.query_exists(null))
                        continue;
                    let config = Plugin.read(metapath);
                    if (!config || this.plugins.has(config.name))
                        continue;
                    let cmd = directory + '/' + name + '/' + config.exec;
                    global.log(`found plugin: ${config.name}`);
                    let pattern = config.pattern ? new RegExp(config.pattern) : null;
                    this.plugins.set(config.name, { config, backend: { cmd, proc: null }, pattern });
                }
            }
        }
        catch (e) {
            global.log(`error enumerating: ${e}`);
        }
    }
    *match_query(ext, query) {
        for (const plugin of BUILTINS) {
            if (!plugin.pattern || plugin.pattern.test(query)) {
                yield plugin;
            }
        }
        for (const plugin of this.plugins.values()) {
            if (!plugin.pattern || plugin.pattern.test(query)) {
                yield plugin;
            }
            else {
                Plugin.quit(ext, plugin);
            }
        }
    }
}
var SearchOption = class SearchOption {
    constructor(title, description, category_icon, icon, icon_size, id) {
        this.shortcut = new St.Label({ text: "", y_align: Clutter.ActorAlign.CENTER, style: "padding-left: 6px;padding-right: 6px" });
        this.title = title;
        this.description = description;
        this.id = id;
        let cat_icon;
        const cat_icon_file = Gio.File.new_for_path(category_icon);
        if (cat_icon_file.query_exists(null)) {
            cat_icon = new St.Icon({
                gicon: Gio.icon_new_for_string(category_icon),
                icon_size: icon_size / 2,
                style_class: "pop-shell-search-icon"
            });
        }
        else {
            cat_icon = new St.Icon({
                icon_name: category_icon,
                icon_size: icon_size / 2,
                style_class: "pop-shell-search-cat"
            });
        }
        let layout = new St.BoxLayout({});
        cat_icon.set_y_align(Clutter.ActorAlign.CENTER);
        let label = new St.Label({ text: title });
        label.clutter_text.set_ellipsize(Pango.EllipsizeMode.END);
        layout.add_child(cat_icon);
        if (icon) {
            let app_icon;
            if ("name" in icon) {
                const file = Gio.File.new_for_path(icon.name);
                if (file.query_exists(null)) {
                    app_icon = new St.Icon({
                        gicon: Gio.icon_new_for_string(icon.name),
                        icon_size,
                        style_class: "pop-shell-search-icon"
                    });
                }
                else {
                    app_icon = new St.Icon({
                        icon_name: icon.name,
                        icon_size,
                        style_class: "pop-shell-search-icon"
                    });
                }
            }
            else if ("gicon" in icon) {
                app_icon = new St.Icon({
                    gicon: icon.gicon,
                    icon_size,
                    style_class: "pop-shell-search-icon"
                });
            }
            else {
                app_icon = icon.widget;
                app_icon.style_class = "pop-shell-search-icon";
            }
            app_icon.set_y_align(Clutter.ActorAlign.CENTER);
            layout.add_child(app_icon);
        }
        let info_box = new St.BoxLayout({ y_align: Clutter.ActorAlign.CENTER, vertical: true, x_expand: true });
        info_box.add_child(label);
        if (description) {
            info_box.add_child(new St.Label({ text: description, style: "font-size: small" }));
        }
        layout.add_child(info_box);
        layout.add_child(this.shortcut);
        this.widget = new St.Button({ style_class: "pop-shell-search-element" });
        this.widget.add_actor(layout);
    }
}
//# sourceMappingURL=launcher_service.js.map