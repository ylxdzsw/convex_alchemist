function check_browser_compatability() {
    if (!('content' in document.createElement('template')))
        alert("Your browser is too old to run this game. Please upgrade to a modern browser.")

    try {
        compress("test")
    } catch {
        alert("Your browser does not support compression. Please upgrade to a modern browser.")
    }
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

function trim_text_node(node) {
    if (node.nodeType === Node.TEXT_NODE) {
        node.textContent = node.textContent.trim()
    } else {
        for (const child of node.childNodes) {
            trim_text_node(child)
        }
    }
}

async function compress(str) {
    const encoded = new TextEncoder().encode(str)
    const compressor = new CompressionStream("deflate-raw")
    const writer = compressor.writable.getWriter()
    writer.write(encoded).then(() => writer.close())
    return await new Response(compressor.readable).arrayBuffer()
}

async function decompress(buffer) {
    const decompressor = new DecompressionStream("deflate-raw")
    const writer = decompressor.writable.getWriter()
    writer.write(buffer).then(() => writer.close())
    return await new Response(decompressor.readable).text()
}

const game = {
    handlers: Object.create(null),

    ptr: null,

    lang: read_user_language(),

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
    read_wasm_json(parse = true) {
        const [ptr, len] = new Uint32Array(ca.memory.buffer, ca.JSON_BUFFER, 2)
        const str = new TextDecoder().decode(new Uint8Array(ca.memory.buffer, ptr, len))
        ca.free_json_buffer()
        if (!parse) return str
        return JSON.parse(str)
    },

    /// write to the json buffer. An API must be used to let the engine read and free the buffer
    write_wasm_json(e, stringify = true) {
        if (stringify) e = JSON.stringify(e)
        const encoded = new TextEncoder().encode(e)
        ca.alloc_json_buffer(encoded.length)
        const buffer = new Uint32Array(ca.memory.buffer, ca.JSON_BUFFER, 2)
        new Uint8Array(ca.memory.buffer, buffer[0], encoded.length).set(encoded)
        buffer[1] = encoded.length
    },

    /// reads the json buffer and process the messages
    process_back_message() {
        for (const message of game.read_wasm_json())
            game.dispatch_message(message)
    },

    /// the returned message is left in the buffer and should be read immediately
    post_message_back(e) {
        console.log("post_message_back", e)
        game.write_wasm_json(e)
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

    async step() {
        game.pool_with_message({event: 'step'})
        for (const building of document.querySelectorAll('ca-building.expanded'))
            game.pool_with_message({event: `${building.name}.detail`})
        await game.save_to_local_storage()
    },

    init_game() {
        if (game.ptr) throw new Error('game already initialized')
        game.ptr = ca.game_new()
        game.pool_with_message({event: 'init', rand_seed: 1145141919})
        game.log({
            zh: `游戏初始化完成。`,
            en: `Game initialized.`
        })
    },

    dump() {
        ca.game_dump(game.ptr)
        return game.read_wasm_json(false)
    },

    load(data_str) {
        if (game.ptr) ca.game_free(game.ptr)
        game.ptr = ca.game_new()
        game.write_wasm_json(data_str, false)
        const time = Date.now()
        ca.game_load(game.ptr)
        console.info("load time", Date.now() - time)
        game.dispatch_message({ event: 'reset' })
        game.process_back_message()
    },

    timewarp(day) {
        ca.game_timewarp(game.ptr, day)
        game.dispatch_message({ event: 'reset' })
        game.process_back_message()
    },

    async export() {
        const str = game.dump()
        const compressed = await compress(str)
        const blob = new Blob([compressed], {type: "application/octet-stream"})
        const a = document.createElement('a')
        a.href = URL.createObjectURL(blob)
        a.download = 'save.ca'
        a.click()
    },

    async import() {
        const input = document.createElement('input')
        input.type = 'file'
        input.accept = '.ca'
        input.onchange = async () => {
            const buffer = await input.files[0].arrayBuffer()
            const decompressed = await decompress(buffer)
            game.load(decompressed)
            game.log({
                zh: `游戏存档读取完成。`,
                en: `Save file loaded.`
            })
        }
        input.click()
    },

    async save_to_local_storage() {
        localStorage.setItem('save.ca', game.dump())
    },

    async load_from_local_storage() {
        game.load(localStorage.getItem('save.ca'), false)
        game.log({
            zh: `游戏存档读取完成。`,
            en: `Save file loaded.`
        })
    },

    show_help() {
        game.log({
            zh: `点击“前进一天”推动游戏进程。点击建筑名称打开详情页。`,
            en: `Click "step" to advance the game. Click building name to open details.`,
        })
    },
}

addEventListener("DOMContentLoaded", async () => {
    check_browser_compatability()

    await wasm_ready

    game.init_game()

    if (/devmode/.test(location.search))
        game.dispatch_message({ event: 'config.devmode' })

    if (localStorage?.getItem('save.ca')) {
        try {
            await game.load_from_local_storage()
        } catch (e) {
            console.error(e)
            game.log({
                zh: `游戏存档读取失败，可能已经损坏。继续游戏将覆盖存档。`,
                en: `Save file corrupted. Continue will overwrite the save file.`,
            })
        }
    } else {
        game.show_help()
    }
})

/*
some notes:
    1. if event handler is not triggered, it is almost certainly because of WeakRef
    2. create typed array view everytime because the memory may be moved
*/
