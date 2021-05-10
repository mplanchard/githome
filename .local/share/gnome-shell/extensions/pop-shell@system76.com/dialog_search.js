const Me = imports.misc.extensionUtils.getCurrentExtension();
const Lib = Me.imports.lib;
const { Clutter, Shell, St } = imports.gi;
const { ModalDialog } = imports.ui.modalDialog;
const { overview, wm } = imports.ui.main;
const { Overview } = imports.ui.overview;
let overview_toggle = null;
var Search = class Search {
    constructor(cancel, search, complete, select, apply, quit) {
        this.dialog = new ModalDialog({
            styleClass: "pop-shell-search modal-dialog",
            destroyOnClose: false,
            shellReactive: true,
            shouldFadeIn: false,
            shouldFadeOut: false
        });
        this.apply_cb = apply;
        this.cancel_cb = cancel;
        this.complete_cb = complete;
        this.select_cb = select;
        this.quit_cb = quit;
        this.active_id = 0;
        this.widgets = [];
        this.entry = new St.Entry({
            style_class: "pop-shell-entry",
            can_focus: true,
            x_expand: true
        });
        this.entry.set_hint_text('  Type to search apps');
        this.text = this.entry.get_clutter_text();
        this.text.set_use_markup(true);
        this.dialog.setInitialKeyFocus(this.text);
        this.text.connect("activate", () => this.activate_option(this.active_id));
        this.text.connect("text-changed", (entry) => {
            this.clear();
            const update = search(entry.get_text().trim());
            if (update)
                this.update_search_list(update);
        });
        this.text.connect("key-press-event", (_, event) => {
            if (event.get_flags() != Clutter.EventFlags.NONE) {
                return;
            }
            let c = event.get_key_symbol();
            if (c === 65307) {
                this.reset();
                this.close();
                cancel();
                return;
            }
            else if (c === 65289) {
                if (this.complete_cb()) {
                    return;
                }
            }
            let s = event.get_state();
            if (c == 65362 || c == 65056 || (s == Clutter.ModifierType.CONTROL_MASK && c == 107) || (s == Clutter.ModifierType.CONTROL_MASK && c == 112)) {
                if (0 < this.active_id) {
                    this.select_id(this.active_id - 1);
                }
                else if (this.active_id == 0) {
                    this.select_id(this.widgets.length - 1);
                }
            }
            else if (c == 65364 || c == 65289 || (s == Clutter.ModifierType.CONTROL_MASK && c == 106) || (s == Clutter.ModifierType.CONTROL_MASK && c == 110)) {
                if (this.active_id + 1 < this.widgets.length) {
                    this.select_id(this.active_id + 1);
                }
                else if (this.active_id + 1 == this.widgets.length) {
                    this.select_id(0);
                }
            }
            else if (s == Clutter.ModifierType.CONTROL_MASK && c == 49) {
                this.activate_option(0);
                return;
            }
            else if (s == Clutter.ModifierType.CONTROL_MASK && c == 50) {
                this.activate_option(1);
                return;
            }
            else if (s == Clutter.ModifierType.CONTROL_MASK && c == 51) {
                this.activate_option(2);
                return;
            }
            else if (s == Clutter.ModifierType.CONTROL_MASK && c == 52) {
                this.activate_option(3);
                return;
            }
            else if (s == Clutter.ModifierType.CONTROL_MASK && c == 53) {
                this.activate_option(4);
                return;
            }
            else if (s == Clutter.ModifierType.CONTROL_MASK && c == 54) {
                this.activate_option(5);
                return;
            }
            else if (s == Clutter.ModifierType.CONTROL_MASK && c == 55) {
                this.activate_option(6);
                return;
            }
            else if (s == Clutter.ModifierType.CONTROL_MASK && c == 56) {
                this.activate_option(7);
                return;
            }
            else if (s == Clutter.ModifierType.CONTROL_MASK && c == 57) {
                this.activate_option(8);
                return;
            }
            else if (s == Clutter.ModifierType.CONTROL_MASK && c == 113) {
                this.quit_cb(this.active_id);
                return;
            }
            this.select_cb(this.active_id);
        });
        this.list = new St.BoxLayout({
            styleClass: "pop-shell-search-list",
            vertical: true,
        });
        const scroller = new St.ScrollView();
        scroller.add_actor(this.list);
        this.dialog.contentLayout.add(this.entry);
        this.dialog.contentLayout.add(scroller);
        this.scroller = scroller;
        this.dialog.contentLayout.width = Math.max(Lib.current_monitor().width / 4, 640);
    }
    activate_option(id) {
        const cont = this.apply_cb(id);
        if (!cont) {
            this.reset();
            this.close();
            this.cancel_cb();
        }
    }
    clear() {
        this.list.remove_all_children();
        this.list.hide();
        this.widgets = [];
        this.active_id = 0;
    }
    close() {
        this.remove_injections();
        this.dialog.close(global.get_current_time());
        wm.allowKeybinding('overlay-key', Shell.ActionMode.NORMAL | Shell.ActionMode.OVERVIEW);
    }
    _open(timestamp, on_primary) {
        this.dialog.open(timestamp, on_primary);
        wm.allowKeybinding('overlay-key', Shell.ActionMode.ALL);
        overview_toggle = Overview.prototype['toggle'];
        Overview.prototype['toggle'] = () => {
            if (this.dialog.is_visible()) {
                this.reset();
                this.close();
                this.cancel_cb();
            }
            else {
                this.remove_injections();
                overview.toggle();
            }
        };
    }
    get_text() {
        return this.text.get_text();
    }
    icon_size() {
        return 34;
    }
    list_max() {
        return 9;
    }
    reset() {
        this.clear();
        this.text.set_text(null);
    }
    show() {
        this.dialog.show_all();
        this.clear();
        this.entry.grab_key_focus();
    }
    select() {
        const widget = this.widgets[this.active_id];
        widget.add_style_pseudo_class("select");
        try {
            imports.misc.util.ensureActorVisibleInScrollView(this.scroller, widget);
        }
        catch (_error) {
        }
    }
    select_id(id) {
        this.unselect();
        this.active_id = id;
        this.select();
    }
    unselect() {
        this.widgets[this.active_id].remove_style_pseudo_class("select");
    }
    update_search_list(list) {
        let initial_cursor = Lib.cursor_rect();
        Lib.join(list.values(), (option) => {
            const id = this.widgets.length;
            const { widget, shortcut } = option;
            if (id < 9) {
                shortcut.set_text(`Ctrl + ${id + 1}`);
                shortcut.show();
            }
            else {
                shortcut.hide();
            }
            widget.connect('clicked', () => this.activate_option(id));
            widget.connect('notify::hover', () => {
                const { x, y } = Lib.cursor_rect();
                if (x === initial_cursor.x && y === initial_cursor.y)
                    return;
                this.select_id(id);
                this.select_cb(id);
            });
            this.widgets.push(widget);
            this.list.add(widget);
        }, () => this.list.add(Lib.separator()));
        this.list.show();
        if (this.widgets.length != 0) {
            this.select();
            this.select_cb(0);
        }
        const vscroll = this.scroller.get_vscroll_bar();
        if (this.scroller.vscrollbar_visible) {
            vscroll.show();
        }
        else {
            vscroll.hide();
        }
    }
    set_text(text) {
        this.text.set_text(text);
    }
    remove_injections() {
        if (overview_toggle !== null) {
            Overview.prototype['toggle'] = overview_toggle;
            overview_toggle = null;
        }
    }
}
//# sourceMappingURL=dialog_search.js.map