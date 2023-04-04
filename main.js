
function check_browser_compatability() {
    return `=== Browser Compatability ===\n` +
        `template element: ${'content' in document.createElement('template')}\n`
}

function exp_num_to_tex_str(exp, format) {
    function keep_2_digits(num) {
        return Math.floor(num * 100) / 100
    }

    function keep_3_digits(num) {
        return Math.floor(num * 1000) / 1000
    }

    if (exp < Math.log(10000)) {
        return keep_2_digits(Math.exp(exp))
    }

    if (exp >= 10000) {
        return `e^{e^{${keep_3_digits(Math.log(exp))}}}`
    }

    switch (format) {
        case 'a': {
            if (exp > Math.log(1000) * 26) {
                return exp_num_to_tex_str(exp, '10')
            }

            const scale = Math.floor(exp / Math.log(1000))
            const significand = Math.exp(exp - scale * Math.log(1000))
            return `${keep_2_digits(significand)}${String.fromCharCode('a'.charCodeAt(0) + scale)}`
        }
        case '10': {
            const scale = Math.floor(exp / Math.LN10)
            const significand = Math.exp(exp - scale * Math.LN10)
            return String.raw`${keep_2_digits(significand)}\times{}10^{${scale}}`
        }
        case 'e': {
            return String.raw`e^{${keep_3_digits(exp)}}`
        }

        default:
            throw new Error(`Unknown format: ${format}`)
    }
}

;(async () => {
    const browser_compatability = check_browser_compatability()
    console.log(browser_compatability)

    await wasm_ready

    console.log(ca)

    const ptr_buffer = ca.alloc_memory(12) // 4 * u32
    ca.hello(ptr_buffer)
    const [ptr, len, capacity] = new Uint32Array(ca.memory.buffer, ptr_buffer, 3)
    console.log(ptr, len, capacity)
    const str = new TextDecoder().decode(new Uint8Array(ca.memory.buffer, ptr, len))
    console.log(str)
    console.log(JSON.parse(str))
    ca.free_memory(ptr, capacity)
    ca.free_memory(ptr_buffer, 12)


})()
