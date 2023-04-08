function check_browser_compatability() {
    return `=== Browser Compatability ===\n` +
        `template element: ${'content' in document.createElement('template')}\n`
}

function read_user_language() {
    let languages = navigator.languages ?? [navigator.language]

    const lang = location.search.match(/lang=([a-z]+)/)
    if (lang) languages = [lang[1]]

    for (const lang of languages) {
        if (lang.startsWith('zh')) return 'zh'
        return 'en'
    }
}

// function html_to_element(html) {
//     const container = document.createElement('template')
//     container.innerHTML = html.trim()
//     return container.content.firstChild
// }

const game = {
    handlers: Object.create(null),

    ptr: null,

    // move as much as possible to wasm and flatten this object
    // lang should be kept front-end since there are text exlusive on the front end
    config: (() => {
        const init = {
            'lang': read_user_language(),
        }

        const config = Object.create(null)
        for (const [name, value] of Object.entries(init)) {
            config['_'+name] = value
            Object.defineProperty(config, name, {
                get() { return this['_'+name] },
                set(value) {
                    this['_'+name] = value
                    game.dispatch_message('config.'+name, value)
                }
            })
        }

        return config
    })(),

    on(event, callback) {
        game.handlers[event] ??= [] // WeakSet is not iterable
        game.handlers[event].push(new WeakRef(callback))
    },

    dispatch_message(message) {
        console.log("dispatch_message", message)
        const callbacks = game.handlers[message.event] ?? []
        if (!callbacks.length) console.warn("message no handler", message)
        for (let i = 0; i < callbacks.length; i++) {
            const callback = callbacks[i].deref()
            if (callback) {
                callback(message)
            } else {
                callbacks[i] = callbacks[callbacks.length - 1]
                callbacks.pop()
                i--
            }
        }
    },

    /// reads the json buffer and returns the parsed json
    read_wasm_json() {
        const [ptr, len] = new Uint32Array(ca.memory.buffer, ca.JSON_BUFFER, 2)
        const str = new TextDecoder().decode(new Uint8Array(ca.memory.buffer, ptr, len))
        ca.free_json_buffer()
        return JSON.parse(str)
    },

    /// reads the json buffer and process the messages
    process_back_message() {
        for (const message of game.read_wasm_json())
            game.dispatch_message(message)
    },

    /// the returned message is left in the buffer and should be read immediately
    post_message_back(e) {
        console.log("post_message_back", e)
        const str = new TextEncoder().encode(JSON.stringify(e))
        ca.alloc_json_buffer(str.length)
        const buffer = new Uint32Array(ca.memory.buffer, ca.JSON_BUFFER, 2)
        new Uint8Array(ca.memory.buffer, buffer[0], str.length).set(str)
        buffer[1] = str.length
        ca.poll(game.ptr)
    },

    /// this is the method to be used most of the time
    pool_with_message(e) {
        game.post_message_back(e)
        game.process_back_message()
    },

    log(lang_dict) {
        game.dispatch_message({
            event: 'log',
            content: lang_dict
        })
    },

    step() {
        game.pool_with_message({event: 'step'})
    },

    init_game() {
        if (game.ptr) throw new Error('game already initialized')
        game.ptr = ca.game_new(1145141919)
        game.pool_with_message({event: 'init'})
        game.show_help()
    },

    show_help() {
        game.log({
            zh: `游戏初始化完成。点击“前进一天”推动游戏进程。点击建筑名称打开详情页。`,
            en: `Game initialized. Click "step" to advance the game. Click building name to open details.`,
        })
    }
}

addEventListener("DOMContentLoaded", async () => {
    const browser_compatability = check_browser_compatability()
    console.info(browser_compatability)

    await wasm_ready

    game.init_game()
})

/*
some notes:
    1. if event handler is not triggered, it is almost certainly because of WeakRef
    2. create typed array view everytime because the memory may be moved
*/
