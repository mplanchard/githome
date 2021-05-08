const Me = imports.misc.extensionUtils.getCurrentExtension();
const { Gio, GLib } = imports.gi;
const utils = Me.imports.utils;
var Builtin = class Builtin {
    constructor() {
        this.last_response = null;
        this.selections = new Array();
    }
    handle(ext, event) {
        switch (event.event) {
            case "complete":
                this.last_response = { event: "noop" };
                break;
            case "query":
                this.last_response = this.query(ext, event.value);
                break;
            case "submit":
                this.last_response = this.submit(ext, event.id);
                break;
            default:
                this.last_response = { event: "noop" };
        }
    }
}
var Response;
(function (Response) {
    function parse(input) {
        try {
            let object = JSON.parse(input);
            switch (object.event) {
                case "close":
                case "fill":
                case "queried":
                    return object;
            }
        }
        catch (e) {
        }
        return null;
    }
    Response.parse = parse;
})(Response || (Response = {}));
var Plugin;
(function (Plugin) {
    function read(file) {
        global.log(`found plugin at ${file}`);
        try {
            let [ok, contents] = Gio.file_new_for_path(file)
                .load_contents(null);
            if (ok)
                return parse(imports.byteArray.toString(contents));
        }
        catch (e) {
        }
        return null;
    }
    Plugin.read = read;
    function parse(input) {
        try {
            return JSON.parse(input);
        }
        catch (e) {
            return null;
        }
    }
    Plugin.parse = parse;
    function listen(plugin) {
        if ('builtin' in plugin.backend) {
            return plugin.backend.builtin.last_response;
        }
        else {
            const backend = plugin.backend;
            if (!backend.proc) {
                const proc = Plugin.start(backend);
                if (proc) {
                    backend.proc = proc;
                }
                else {
                    return null;
                }
            }
            try {
                let [bytes,] = backend.proc.stdout.read_line(null);
                return Response.parse(imports.byteArray.toString(bytes));
            }
            catch (e) {
                return null;
            }
        }
    }
    Plugin.listen = listen;
    function complete(ext, plugin) {
        return send(ext, plugin, { event: "complete" });
    }
    Plugin.complete = complete;
    function query(ext, plugin, value) {
        return send(ext, plugin, { event: "query", value });
    }
    Plugin.query = query;
    function quit(ext, plugin) {
        if ('proc' in plugin.backend) {
            if (plugin.backend.proc) {
                send(ext, plugin, { event: "quit" });
                plugin.backend.proc = null;
            }
        }
        else {
            send(ext, plugin, { event: "quit" });
        }
    }
    Plugin.quit = quit;
    function submit(ext, plugin, id) {
        return send(ext, plugin, { event: "submit", id });
    }
    Plugin.submit = submit;
    function send(ext, plugin, event) {
        const backend = plugin.backend;
        if ('builtin' in backend) {
            backend.builtin.handle(ext, event);
            return true;
        }
        else {
            let string = JSON.stringify(event);
            if (!backend.proc) {
                backend.proc = start(backend);
            }
            function attempt(name, plugin, string) {
                if (!plugin.proc)
                    return false;
                try {
                    plugin.proc.stdin.write_bytes(new GLib.Bytes(string + "\n"), null);
                    return true;
                }
                catch (e) {
                    global.log(`failed to send message to ${name}: ${e}`);
                    return false;
                }
            }
            if (!attempt(plugin.config.name, backend, string)) {
                backend.proc = start(backend);
                if (!attempt(plugin.config.name, backend, string))
                    return false;
            }
        }
        return true;
    }
    Plugin.send = send;
    function start(plugin) {
        return utils.async_process_ipc([plugin.cmd]);
    }
    Plugin.start = start;
})(Plugin || (Plugin = {}));
//# sourceMappingURL=launcher_plugins.js.map