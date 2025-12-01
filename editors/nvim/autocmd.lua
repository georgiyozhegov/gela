-- Gela highlighting configuration
vim.api.nvim_create_autocmd({
    "BufRead",
    "BufNewFile",
}, {
    pattern = "*.ga",
    callback = function()
        vim.cmd("set filetype=gela")
    end,
})
