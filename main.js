
function check_browser_compatability() {
    return `=== Browser Compatability ===\n` +
        `template element: ${'content' in document.createElement('template')}\n`
}

// display exp number as tex, html, or plaintext string (type == 'tex' | 'html' | 'plain')
// different formats have different ranges. The checks should be done before calling this function 
function exp_num_to_str(exp, format, type) {
    const keep_n_digits = (num, n) => Math.floor(num * 10 ** n + 1e-9) / 10 ** n

    if (exp == -Infinity)
        return '0'

    switch (format) {
        case 'a': {
            if (exp < 0 || exp > Math.log(1000) * 27 - 1e-9)
                throw new Error(`out of range: exp=${exp}, format=${format}`)

            if (exp < Math.log(1000))
                return keep_n_digits(Math.exp(exp), 2)

            const scale = Math.floor(exp / Math.log(1000) + 1e-9)
            const significand = Math.exp(exp - scale * Math.log(1000))
            const significand_str = significand < 10 ? keep_n_digits(significand, 3) :
                                    significand < 100 ? keep_n_digits(significand, 2) :
                                    keep_n_digits(significand, 1)
            return `${significand_str}${String.fromCharCode('a'.charCodeAt(0) - 1 + scale)}`
        }
        case '10': {
            const scale = Math.floor(exp / Math.LN10 + 1e-9)
            const significand = Math.exp(exp - scale * Math.LN10)
            switch (type) {
                case 'tex': return String.raw`${keep_n_digits(significand, 2)}\times{}10^{${scale}}`
                case 'html': return `${keep_n_digits(significand, 2)}×10<sup>${scale}</sup>`
                case 'plain': return `${keep_n_digits(significand, 2)}E${scale}`
            }
        }
        case 'e': {
            switch (type) {
                case 'tex': return String.raw`e^{${keep_n_digits(exp, 3)}}`
                case 'html': return `e<sup>${keep_n_digits(exp, 3)}</sup>`
                case 'plain': return `e${keep_n_digits(exp, 3)}`
            }
        }
        case 'd': {
            if (exp >= Math.LN10 * 10 || exp < Math.log(0.001))
                throw new Error(`out of range: exp=${exp}, format=${format}`)
            return keep_n_digits(Math.exp(exp), exp > 0 ? 2 : 4)
        }
        case 'ee': {
            if (exp < 1e-9)
                throw new Error(`out of range: exp=${exp}, format=${format}`)

            switch (type) {
                case 'tex': return `e^{e^{${keep_n_digits(Math.log(exp), 3)}}}`
                case 'html': return `e<sup>e<sup>${keep_n_digits(Math.log(exp), 3)}</sup></sup>`
                case 'plain': return `ee${keep_n_digits(Math.log(exp), 3)}`
            }
        }
        default: {
            throw new Error(`Unknown format: ${format}`)
        }
    }
}

function html_to_element(html) {
    const container = document.createElement('template')
    container.innerHTML = html.trim()
    return container.content.firstChild
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
