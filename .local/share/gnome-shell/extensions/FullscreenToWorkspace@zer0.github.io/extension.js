const Meta = imports.gi.Meta;

// TODO: pass manager instead of display

const change_workspace = (win, manager, index) => {
    win.change_workspace_by_index(index, 1);
	manager.get_workspace_by_index(index).activate(global.get_current_time());
};

const first_empty_workspace_index = (manager) => {
	const n = manager.get_n_workspaces();
	for (let i = 0; i < n; ++i) {
		let win_count = manager.get_workspace_by_index(i)
							.list_windows()
							.filter(w => !w.is_always_on_all_workspaces()).length;
		if (win_count < 1) { return i; }
	}
	// return last workspace
	return n - 1;
}

const _old_workspaces = {};

function check(win) {
	const manager = win.get_display().get_workspace_manager();
 	if (win.window_type !== Meta.WindowType.NORMAL) {
		return;
	}
	if (!win.is_fullscreen()) {
		// Check if it was fullscreen before
		let name = win.get_gtk_unique_bus_name();
		if (_old_workspaces[name] !== undefined) {
			change_workspace(win, manager, _old_workspaces[name]);
			_old_workspaces[name] = undefined;	// remove it from array since we revert it back
		}
		return;
	}
	let other_wins = win.get_workspace().list_windows()
		.filter(w => w !== win && !w.is_always_on_all_workspaces());
	if (other_wins.length > 0) {
		let name = win.get_gtk_unique_bus_name();
		_old_workspaces[name] = win.get_workspace().index();
		let emptyworkspace = 1;
		emptyworkspace = first_empty_workspace_index(manager);
		if (emptyworkspace === _old_workspaces[name]) {
			return;
		}
		change_workspace(win, manager, emptyworkspace);
	}
}

let _handle = null;

function enable() {
	_handle = global.window_manager.connect('size-changed', (_, act) => {
		const win = act.meta_window;
		check(win);
	});
}

function disable() {
	global.window_manager.disconnect(_handle);
}