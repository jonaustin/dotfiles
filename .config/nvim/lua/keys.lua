-- ========================================================================== --
-- ==                             KEYBINDINGS                              == --
-- ========================================================================== --
-- See `:help vim.keymap.set()`

vim.g.mapleader = ','
vim.g.maplocalleader = ','

-- Shortcuts
local map = vim.keymap.set

-- Move to window using the <ctrl> hjkl keys
map("n", "<C-h>", "<C-w>h", { desc = "Go to left window", remap = true })
map("n", "<C-j>", "<C-w>j", { desc = "Go to lower window", remap = true })
map("n", "<C-k>", "<C-w>k", { desc = "Go to upper window", remap = true })
map("n", "<C-l>", "<C-w>l", { desc = "Go to right window", remap = true })

-- tabs
map("n", "<S-h>", "gT", { desc = "Prev tab" })
map("n", "<S-l>", "gt", { desc = "Next tab" })

-- QoL
map("i", "jk", "<esc>")
map("i", "jj", "<esc>")
map("n", "<C-q>", "<cmd>quit<cr>", { desc = "Quit vim" })
-- map("<leader>.", "<cmd>nohl<cr>")

-- clipboard
map("n", "<leader>ya", "<cmd>%y+<cr>") -- yank entire buffer

-- lazy
map("n", "<leader>l", "<cmd>Lazy<cr>", { desc = "Lazy" })

-- quickfix
map("n", "[q", vim.cmd.cprev, { desc = "Previous quickfix" })
map("n", "]q", vim.cmd.cnext, { desc = "Next quickfix" })

-- [[ Basic Keymaps ]]

-- Keymaps for better default experience
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })

-- Trim whitespace
-- maybe use: https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-trailspace.md
vim.keymap.set('n', '<leader>W', ':%s/\\s\\+$//e<CR>', { noremap = true, silent = true })

-- split with definition code file
vim.keymap.set('n', 'gv', '<c-w>v<cmd>lua vim.lsp.buf.definition()<CR>', opts)

local opts = { noremap=true, silent=true }

-- See `:help vim.lsp.*` for documentation on any of the below functions
map('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
map('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
map('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
map('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
-- map('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
-- map('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
-- map('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
-- map('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
map('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
map('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
map('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
map('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
-- map('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
-- map('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
-- map('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
-- map('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
-- map("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
