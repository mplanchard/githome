const Me = imports.misc.extensionUtils.getCurrentExtension();
const { Gio, GLib } = imports.gi;
const plugins = Me.imports.launcher_plugins;
const LOCAL = GLib.get_home_dir() + "/.local/share/pop-shell/scripts/";
const SYSTEM = "/usr/lib/pop-shell/scripts/";
var ScriptsBuiltin = class ScriptsBuiltin extends plugins.Builtin {
    constructor() {
        super(...arguments);
        this.scripts = new Array();
        this.filtered = new Array();
        this.sums = new Set();
    }
    init() {
        this.sums.clear();
        this.scripts.splice(0);
        this.load_from(LOCAL);
        this.load_from(SYSTEM);
    }
    load_from(path) {
        try {
            const dir = Gio.file_new_for_path(path);
            if (!dir.query_exists(null))
                return;
            const entries = dir.enumerate_children('standard::*', Gio.FileQueryInfoFlags.NONE, null);
            let entry;
            while ((entry = entries.next_file(null)) !== null) {
                const name = entry.get_name();
                if (this.sums.has(name))
                    continue;
                const metadata = {
                    name,
                    path: path + name,
                    keywords: new Array(),
                    description: null,
                };
                try {
                    const stream = Gio.DataInputStream.new(Gio.File.new_for_path(metadata.path).read(null));
                    while (true) {
                        const [bytes] = stream.read_line(null);
                        if (!bytes)
                            break;
                        let line = imports.byteArray.toString(bytes);
                        if (!line.startsWith("#"))
                            break;
                        line = line.substring(1).trim();
                        if (line.startsWith("name:")) {
                            metadata.name = line.substring(5).trim();
                        }
                        else if (line.startsWith("description:")) {
                            metadata.description = line.substring(12).trim();
                        }
                        else if (line.startsWith("icon:")) {
                            metadata.icon = line.substring(5).trim();
                        }
                        else if (line.startsWith("keywords:")) {
                            metadata.keywords = line.substring(9).trim().split(/\s+/);
                        }
                    }
                    this.scripts.push(metadata);
                    this.sums.add(name);
                }
                catch (e) {
                    log(`failed to read from script at ${metadata.path}: ${e}`);
                    continue;
                }
            }
        }
        catch (e) {
            log(`failure to collect scripts for script plugin: ${e}`);
        }
    }
    query(_, query) {
        var _a;
        this.filtered.splice(0);
        this.selections.splice(0);
        query = query.toLowerCase();
        let id = 0;
        for (const script of this.scripts) {
            let should_include = script.name.toLowerCase().includes(query)
                || ((_a = script.description) === null || _a === void 0 ? void 0 : _a.toLowerCase().includes(query))
                || script.keywords.reduce((acc, next) => acc || next.includes(query), false);
            if (should_include) {
                const selection = {
                    id,
                    name: script.name,
                    description: script.description,
                };
                if (script.icon)
                    selection.icon = script.icon;
                this.selections.push(selection);
                this.filtered.push(script);
                id += 1;
            }
        }
        return { event: "queried", selections: this.selections };
    }
    submit(_, id) {
        let script = this.filtered[id];
        if (script) {
            try {
                GLib.spawn_command_line_async(`sh ${script.path}`);
            }
            catch (e) {
                log(`failed to spawn script at ${script.path}: ${e}`);
            }
        }
        return { event: "noop" };
    }
}
//# sourceMappingURL=plugin_scripts.js.map