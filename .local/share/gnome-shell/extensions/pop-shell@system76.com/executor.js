var __classPrivateFieldGet = (this && this.__classPrivateFieldGet) || function (receiver, privateMap) {
    if (!privateMap.has(receiver)) {
        throw new TypeError("attempted to get private field on non-instance");
    }
    return privateMap.get(receiver);
};
var __classPrivateFieldSet = (this && this.__classPrivateFieldSet) || function (receiver, privateMap, value) {
    if (!privateMap.has(receiver)) {
        throw new TypeError("attempted to set private field on non-instance");
    }
    privateMap.set(receiver, value);
    return value;
};
var _event_loop, _events, _iterable, _signal, _channel, _signal_1;
const Me = imports.misc.extensionUtils.getCurrentExtension();
const GLib = imports.gi.GLib;
var GLibExecutor = class GLibExecutor {
    constructor() {
        _event_loop.set(this, null);
        _events.set(this, new Array());
    }
    wake(system, event) {
        __classPrivateFieldGet(this, _events).unshift(event);
        if (__classPrivateFieldGet(this, _event_loop))
            return;
        __classPrivateFieldSet(this, _event_loop, GLib.idle_add(GLib.PRIORITY_DEFAULT, () => {
            let event = __classPrivateFieldGet(this, _events).pop();
            if (event)
                system.run(event);
            if (__classPrivateFieldGet(this, _events).length === 0) {
                __classPrivateFieldSet(this, _event_loop, null);
                return false;
            }
            return true;
        }));
    }
}
_event_loop = new WeakMap(), _events = new WeakMap();
var OnceExecutor = class OnceExecutor {
    constructor(iterable) {
        _iterable.set(this, void 0);
        _signal.set(this, null);
        __classPrivateFieldSet(this, _iterable, iterable);
    }
    start(delay, apply, then) {
        this.stop();
        const iterator = __classPrivateFieldGet(this, _iterable)[Symbol.iterator]();
        __classPrivateFieldSet(this, _signal, GLib.timeout_add(GLib.PRIORITY_DEFAULT, delay, () => {
            const next = iterator.next().value;
            if (typeof next === 'undefined') {
                if (then)
                    GLib.timeout_add(GLib.PRIORITY_DEFAULT, delay, () => {
                        then();
                        return false;
                    });
                return false;
            }
            return apply(next);
        }));
    }
    stop() {
        if (__classPrivateFieldGet(this, _signal) !== null)
            GLib.source_remove(__classPrivateFieldGet(this, _signal));
    }
}
_iterable = new WeakMap(), _signal = new WeakMap();
var ChannelExecutor = class ChannelExecutor {
    constructor() {
        _channel.set(this, new Array());
        _signal_1.set(this, null);
    }
    clear() { __classPrivateFieldGet(this, _channel).splice(0); }
    get length() { return __classPrivateFieldGet(this, _channel).length; }
    send(v) {
        __classPrivateFieldGet(this, _channel).push(v);
    }
    start(delay, apply) {
        this.stop();
        __classPrivateFieldSet(this, _signal_1, GLib.timeout_add(GLib.PRIORITY_DEFAULT, delay, () => {
            const e = __classPrivateFieldGet(this, _channel).shift();
            return typeof e === 'undefined' ? true : apply(e);
        }));
    }
    stop() {
        if (__classPrivateFieldGet(this, _signal_1) !== null)
            GLib.source_remove(__classPrivateFieldGet(this, _signal_1));
    }
}
_channel = new WeakMap(), _signal_1 = new WeakMap();
//# sourceMappingURL=executor.js.map