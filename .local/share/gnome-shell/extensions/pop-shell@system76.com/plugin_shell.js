const Me = imports.misc.extensionUtils.getCurrentExtension();
const plugins = Me.imports.launcher_plugins;
const SELECTIONS = [
    {
        id: 0,
        name: "Toggle Active Hint",
        description: "Decorates the actively-focused window"
    },
    {
        id: 1,
        name: "Toggle Orientation",
        description: "Toggles the orientation of a tiling branch"
    },
    {
        id: 2,
        name: "Toggle Stacking",
        description: "Stacks a window, or unstacks a stack with a single window"
    },
    {
        id: 3,
        name: "Toggle Window Titles",
        description: "Shows or hides window title bars in X11",
    },
    {
        id: 4,
        name: "Toggle Window Tiling",
        description: "Tiled windows are arranged side by side on the screen"
    }
];
var ShellBuiltin = class ShellBuiltin extends plugins.Builtin {
    init() { }
    query(_, query) {
        query = query.toLowerCase();
        return {
            event: "queried",
            selections: SELECTIONS.filter((selection) => selection.name.toLowerCase().includes(query))
        };
    }
    submit(ext, id) {
        switch (id) {
            case 0:
                ext.settings.set_active_hint(!ext.settings.active_hint());
                break;
            case 1:
                ext.tiler.toggle_orientation(ext);
                break;
            case 2:
                ext.tiler.toggle_stacking(ext);
                break;
            case 3:
                ext.settings.set_show_title(!ext.settings.show_title());
                break;
            case 4:
                ext.toggle_tiling();
                break;
        }
        return { event: "noop" };
    }
}
//# sourceMappingURL=plugin_shell.js.map