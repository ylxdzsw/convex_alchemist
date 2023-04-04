
function check_browser_compatability() {
    return `=== Browser Compatability ===\n` +
        `template element: ${'content' in document.createElement('template')}\n`
}

;(async () => {
    const browser_compatability = check_browser_compatability()
    console.log(browser_compatability)


})()
