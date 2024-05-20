-- ### things to fix / figure out:
-- read through https://github.com/nvim-lua/kickstart.nvim
-- easy/quick folding nvim-ufo
--	za - toggle fold under cursor
-- https://github.com/cshuaimin/ssr.nvim - structural search and replace
-- https://github.com/folke/flash.nvim - maybe replace sneak
-- https://github.com/sindrets/diffview.nvim - better diff viewer
-- https://github.com/VonHeikemen/lazy-template
-- replace maximize maybe with https://github.com/folke/dot/blob/master/nvim/lua/plugins/ui.lua#L29C6-L29C29

local home = os.getenv('HOME')

vim.opt.number = true
vim.wo.relativenumber = true
vim.opt.mouse = 'a'
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = false
vim.opt.wrap = true
vim.opt.breakindent = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = false
vim.o.autoindent = true            -- indent at the same level of the previous line
vim.o.wrap = true                  -- wrap long lines
vim.o.autoread = true              -- auto reload file if it changes outside of vim
vim.o.guicursor = 'a:hor20-Cursor' -- underline cursor
vim.o.termguicolors = true         -- 24bit (true) colors
vim.o.breakindent = true           -- wrap lines with same indent

-- LSP
-- disable noisy linting messages my default
vim.diagnostic.config({
	virtual_text = false
})


-- Speed
vim.opt.lazyredraw = true -- fix slowdown issues when moving cursor with syntax on
vim.opt.ttyfast    = true -- assume fast connection (smoother redraw)
vim.opt.synmaxcol  = 1024 -- Syntax coloring lines that are too long just slows down the world

-- give in to my muscle memory
vim.o.undofile     = true
vim.o.backup       = true
vim.o.backupdir    = home .. "/.vimbackup"
vim.o.directory    = home .. "/.vimswap"
vim.o.viewdir      = home .. "/.vimviews"
vim.o.undodir      = home .. "/.vimundo"

-- Case-insensitive searching UNLESS \C or Capital in search
vim.o.ignorecase   = true
vim.o.smartcase    = true

-- Keep signcolumn on by default
vim.wo.signcolumn  = 'yes'

-- Decrease update time
vim.o.updatetime   = 250
vim.o.timeoutlen   = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt  = 'menuone,noselect'

-- deal with mac vs linux clipboard
if vim.fn.has('unix') == 1 then
	if vim.fn.has('mac') == 1 then
		-- macOS
		vim.o.clipboard = 'unnamed'
	else
		-- Linux, BSD, etc.
		vim.o.clipboard = 'unnamedplus'
	end
end

-- ========================================================================== --
-- ==                             KEYBINDINGS                              == --
-- ========================================================================== --


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

-- ========================================================================== --
-- ==                               COMMANDS                               == --
-- ========================================================================== --

vim.api.nvim_create_user_command('ReloadConfig', 'source $MYVIMRC', {})

-- local group = vim.api.nvim_create_augroup('user_cmds', {clear = true})



-- ========================================================================== --
-- ==                               PLUGINS                                == --
-- ========================================================================== --

local lazy = {}

function lazy.install(path)
	if not vim.loop.fs_stat(path) then
		print('Installing lazy.nvim....')
		vim.fn.system({
			'git',
			'clone',
			'--filter=blob:none',
			'https://github.com/folke/lazy.nvim.git',
			'--branch=stable',
			path,
		})
	end
end

function lazy.setup(plugins)
	if vim.g.plugins_ready then
		return
	end

	lazy.install(lazy.path)

	vim.opt.rtp:prepend(lazy.path)

	require('lazy').setup(plugins, lazy.opts)
	vim.g.plugins_ready = true
end

lazy.path = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
lazy.opts = {}

lazy.setup({
	-- coding
	'fatih/vim-go',
	'vim-ruby/vim-ruby',
	'tpope/vim-rails',
	'vim-vaultproject',
	{ 'SmiteshP/nvim-navic',   dependencies = { 'neovim/nvim-lspconfig' }, lsp = { auto_attach = true, } }, -- show current code context
	{ 'RaafatTurki/corn.nvim', opts = {} },                                                                -- put annoying lsp linter messages in their place

	-- Git related plugins
	'tpope/vim-fugitive',
	'tpope/vim-rhubarb', -- :GBrowse

	-- Detect tabstop and shiftwidth automatically
	'tpope/vim-sleuth',

	'tpope/vim-repeat',
	'tpope/vim-surround',     -- use treesitter instead?
	'mbbill/undotree',
	'junegunn/vim-easy-align', -- :EasyAlign /<regex>/ ---- note: maybe replace with mini.align ---- note: maybe replace with mini.align
	{ 'xolox/vim-session', dependencies = { 'xolox/vim-misc' } },
	{
		'szw/vim-maximizer',
		init = function()
			vim.g.maximizer_set_default_mapping = 0 -- vim-maximizer; disable F3 since it stomps on nvim-dap
		end
	},
	'justinmk/vim-sneak', -- s<2 chars>

	-- colorschemes
	{
		'folke/tokyonight.nvim',
		opts = { transparent = vim.g.transparent_enabled },
	},
	'xiyaowong/transparent.nvim',

	-- integrations
	-- 'stevearc/oil.nvim', -- edit your filesystem like a buffer
	'christoomey/vim-tmux-navigator', -- seamless navigation between vim and tmux splits
	{
		"nvim-tree/nvim-tree.lua",     -- file explorer
		version = "*",
		lazy = false,
		-- dependencies = {
		--   "nvim-tree/nvim-web-devicons",
		-- },
		config = function()
			require("nvim-tree").setup {}
		end,
	},

	'kyazdani42/nvim-web-devicons', -- for nvim-tree
	'nvim-lualine/lualine.nvim',
	'nvim-lua/plenary.nvim',       -- base lib used by other plugins
	'majutsushi/tagbar',           -- side pane with list of functions,etc
	'tpope/vim-commentary',

	-- AI
	-- 'github/copilot.vim',
	{
		"zbirenbaum/copilot.lua",
		opts = {
			filetypes = { ["*"] = true },
		},
	},
	{
		"zbirenbaum/copilot-cmp",
		config = function()
			require("copilot_cmp").setup()
		end
	},
	{
		'bakks/butterfish.nvim',
		dependencies = { 'tpope/vim-commentary' },
		config = function()
			require('butterfish')
		end
	},
	{ 'Bryley/neoai.nvim', dependencies = { "MunifTanjim/nui.nvim", } },
	{
		"CopilotC-Nvim/CopilotChat.nvim",
		opts = {
			show_help = "yes",      -- Show help text for CopilotChatInPlace, default: yes
			debug = false,          -- Enable or disable debug mode, the log file will be in ~/.local/state/nvim/CopilotChat.nvim.log
			disable_extra_info = 'no', -- Disable extra information (e.g: system prompt) in the response.
			language = "English",   -- Copilot answer language settings when using default prompts. Default language is English.
			mode = "split",         -- newbuffer or split  , default: newbuffer
			-- proxy = "socks5://127.0.0.1:3000", -- Proxies requests via https or socks.
			-- temperature = 0.1,
		},
		build = function()
			vim.defer_fn(function()
				vim.cmd("UpdateRemotePlugins")
				vim.notify("CopilotChat - Updated remote plugins. Please restart Neovim.")
			end, 3000)
		end,
		event = "VeryLazy",
		keys = {
			{ "<leader>ccb", "<cmd>CopilotChatBuffer<cr>",  desc = "CopilotChat - Chat with current buffer" },
			{ "<leader>cce", "<cmd>CopilotChatExplain<cr>", desc = "CopilotChat - Explain code" },
			{ "<leader>cct", "<cmd>CopilotChatTests<cr>",   desc = "CopilotChat - Generate tests" },
			{
				"<leader>ccT",
				"<cmd>CopilotChatVsplitToggle<cr>",
				desc = "CopilotChat - Toggle Vsplit", -- Toggle vertical split
			},
			{
				"<leader>ccv",
				":CopilotChatVisual",
				mode = "x",
				desc = "CopilotChat - Open in vertical split",
			},
			{
				"<leader>ccx",
				":CopilotChatInPlace<cr>",
				mode = "x",
				desc = "CopilotChat - Run in-place code",
			},
			{
				"<leader>ccf",
				"<cmd>CopilotChatFixDiagnostic<cr>", -- Get a fix for the diagnostic message under the cursor.
				desc = "CopilotChat - Fix diagnostic",
			},
			{
				"<leader>ccr",
				"<cmd>CopilotChatReset<cr>", -- Reset chat history and clear buffer.
				desc = "CopilotChat - Reset chat history and clear buffer",
			}
		},
	},
	-- LSP
	{
		-- LSP Configuration & Plugins
		-- vim.lsp.set_log_level("DEBUG")
		'neovim/nvim-lspconfig',
		dependencies = {
			-- Automatically install LSPs to stdpath for neovim
			{ 'williamboman/mason.nvim', config = true },
			'williamboman/mason-lspconfig.nvim',

			-- Useful status updates for LSP
			-- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
			{ 'j-hui/fidget.nvim',       opts = {} }, -- notifications in lower right corner

			'folke/neodev.nvim',                   -- nvim lua development stuff
		},
	},

	{
		-- Autocompletion
		'hrsh7th/nvim-cmp',
		dependencies = {
			-- Snippet Engine & its associated nvim-cmp source
			'L3MON4D3/LuaSnip',
			'saadparwaiz1/cmp_luasnip',

			-- Adds LSP completion capabilities
			'hrsh7th/cmp-nvim-lsp',
			'hrsh7th/cmp-path',
			'hrsh7th/cmp-buffer',

			-- Adds a number of user-friendly snippets
			'rafamadriz/friendly-snippets',
		},
	},

	{
		"mfussenegger/nvim-lint",
		event = {
			"BufReadPre",
			"BufNewFile",
		},
		config = function()
			local lint = require("lint")

			lint.linters_by_ft = {
				javascript      = { "eslint_d" },
				javascriptreact = { "eslint_d" },
				kotlin          = { "ktlint" },
				-- lua = { "luacheck" },
				markdown        = { "markdownlint" },
				puppet          = { "puppet-lint" },
				ruby            = { "rubocop" },
				svelte          = { "eslint_d" },
				terraform       = { "tflint", "tfsec" },
				typescript      = { "eslint_d" },
				zsh             = { "shellcheck" },
			}

			local lint_augroup = vim.api.nvim_create_augroup("lint", { clear = true })

			vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "InsertLeave" }, {
				group = lint_augroup,
				callback = function()
					lint.try_lint()
				end,
			})

			vim.keymap.set("n", "<leader>ll", function()
				lint.try_lint()
			end, { desc = "Trigger linting for current file" })
		end,
	},

	{
		"stevearc/conform.nvim",
		event = { "BufReadPre", "BufNewFile" },
		config = function()
			require("conform").setup({
				formatters_by_ft = {
					bash = { "shfmt" },
					css = { { "prettierd", "prettier" } },
					erb = { "htmlbeautifier" },
					graphql = { { "prettierd", "prettier" } },
					go = { "gofmt", "goimports" },
					html = { "htmlbeautifier" },
					java = { "google-java-format" },
					javascript = { { "prettierd", "prettier" } },
					javascriptreact = { { "prettierd", "prettier" } },
					json = { { "prettierd", "prettier" } },
					-- lua = { "stylua" }, -- ugh fix way too much indenting
					markdown = { "markdownlint", "markdown-toc" },
					proto = { "buf" },
					python = { "black", "isort" },
					ruby = { "rufo" }, -- from the creator of Crystal
					rust = { "rustfmt" },
					scss = { { "prettierd", "prettier" } },
					svelte = { { "prettierd", "prettier" } },
					terraform = { "terraform_fmt" },
					hcl = { "terragrunt_hclfmt" },
					toml = { "taplo" },
					typescript = { { "prettierd", "prettier" } },
					typescriptreact = { { "prettierd", "prettier" } },
					yaml = { "yamlfix" },
					["_"] = { "trim_whitespace" },
					["*"] = { "codespell" },
				},
				format_on_save = {
					lsp_fallback = true,
					timeout_ms = 500,
				},
			})

			vim.keymap.set({ "n", "v" }, "<leader>cl", function()
				require("conform").format({
					lsp_fallback = true,
					async = false,
					timeout_ms = 500,
				})
			end, { desc = "Format file or range (in visual mode)" })
		end,
	},
	{
		'WhoIsSethDaniel/mason-tool-installer.nvim',
		config = {
			-- a list of all tools you want to ensure are installed upon
			-- start
			ensure_installed = {
				-- you can pin a tool to a particular version
				-- { 'golangci-lint', version = 'v1.47.0' },

				-- you can turn off/on auto_update per tool
				{ 'bash-language-server', auto_update = true },

				'black',
				'codespell',
				'editorconfig-checker',
				'eslint_d',
				'gofumpt',
				'golangci-lint',
				'golines',
				'gomodifytags',
				'gopls',
				'gotests',
				'impl',
				'isort',
				'json-to-struct',
				'lua-language-server',
				'luacheck',
				'markdown-toc',
				'misspell',
				'revive',
				'rubocop',
				'rufo',
				'shellcheck',
				'shfmt',
				'staticcheck',
				'stylua',
				'vim-language-server',
				'vint',
			},

			-- Disable integration with other Mason plugins. This removes
			-- the ability to to use the alternative names of packages provided
			-- by these plugins but disables them from immediately becoming loaded
			integrations = {
				["mason-lspconfig"] = true,
				["mason-null-ls"] = true,
				["mason-nvim-dap"] = true,
			}
		}
	},


	{ 'folke/which-key.nvim',  opts = {} }, -- Useful plugin to show pending keybinds.
	{
		-- Adds git related signs to the gutter, as well as utilities for managing changes
		'lewis6991/gitsigns.nvim',
		opts = {
			-- See `:help gitsigns.txt`
			signs = {
				add = { text = '+' },
				change = { text = '~' },
				delete = { text = '_' },
				topdelete = { text = '‾' },
				changedelete = { text = '~' },
			},
			on_attach = function(bufnr)
				local gs = package.loaded.gitsigns

				local function map(mode, l, r, opts)
					opts = opts or {}
					opts.buffer = bufnr
					vim.keymap.set(mode, l, r, opts)
				end

				-- Navigation
				map({ 'n', 'v' }, ']c', function()
					if vim.wo.diff then
						return ']c'
					end
					vim.schedule(function()
						gs.next_hunk()
					end)
					return '<Ignore>'
				end, { expr = true, desc = 'Jump to next hunk' })

				map({ 'n', 'v' }, '[c', function()
					if vim.wo.diff then
						return '[c'
					end
					vim.schedule(function()
						gs.prev_hunk()
					end)
					return '<Ignore>'
				end, { expr = true, desc = 'Jump to previous hunk' })

				-- Actions
				-- visual mode
				map('v', '<leader>hs', function()
					gs.stage_hunk { vim.fn.line '.', vim.fn.line 'v' }
				end, { desc = 'stage git hunk' })
				map('v', '<leader>hr', function()
					gs.reset_hunk { vim.fn.line '.', vim.fn.line 'v' }
				end, { desc = 'reset git hunk' })
				-- normal mode
				map('n', '<leader>hs', gs.stage_hunk, { desc = 'git stage hunk' })
				map('n', '<leader>hr', gs.reset_hunk, { desc = 'git reset hunk' })
				map('n', '<leader>hS', gs.stage_buffer, { desc = 'git Stage buffer' })
				map('n', '<leader>hu', gs.undo_stage_hunk, { desc = 'undo stage hunk' })
				map('n', '<leader>hR', gs.reset_buffer, { desc = 'git Reset buffer' })
				map('n', '<leader>hp', gs.preview_hunk, { desc = 'preview git hunk' })
				map('n', '<leader>hb', function()
					gs.blame_line { full = false }
				end, { desc = 'git blame line' })
				map('n', '<leader>hd', gs.diffthis, { desc = 'git diff against index' })
				map('n', '<leader>hD', function()
					gs.diffthis '~'
				end, { desc = 'git diff against last commit' })

				-- Toggles
				map('n', '<leader>tb', gs.toggle_current_line_blame, { desc = 'toggle git blame line' })
				map('n', '<leader>td', gs.toggle_deleted, { desc = 'toggle git show deleted' })

				-- Text object
				map({ 'o', 'x' }, 'ih', ':<C-U>Gitsigns select_hunk<CR>', { desc = 'select git hunk' })
			end,
		},
	},

	{ 'kevinhwang91/nvim-ufo', dependencies = { 'kevinhwang91/promise-async' } },

	-- Fuzzy Finder (files, lsp, etc)
	{
		'nvim-telescope/telescope.nvim',
		dependencies = {
			'nvim-lua/plenary.nvim',
			{
				'nvim-telescope/telescope-fzf-native.nvim',
				build = 'make',
				cond = function()
					return vim.fn.executable 'make' == 1
				end,
			},
		},
	},

	{
		-- Highlight, edit, and navigate code
		'nvim-treesitter/nvim-treesitter',
		dependencies = {
			'nvim-treesitter/nvim-treesitter-textobjects',
		},
		build = ':TSUpdate',
	},
	{
		"folke/trouble.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		opts = {},
	},
	{
		'Wansmer/treesj',
		keys = { '<space>m', '<space>j', '<space>s' },
		dependencies = { 'nvim-treesitter/nvim-treesitter' },
		config = function()
			require('treesj').setup({})
		end,
	},
	{ 'dstein64/vim-startuptime' },                   -- :StartupTime
	{ "stevearc/dressing.nvim",  event = "VeryLazy" }, -- purtify

	-- <leader>z - focus mode
	"folke/twilight.nvim",
	{
		"folke/zen-mode.nvim",
		cmd = "ZenMode",
		opts = {
			plugins = {
				gitsigns = true,
				tmux = true,
				kitty = { enabled = false, font = "+2" },
			},
		},
		keys = { { "<leader>z", "<cmd>ZenMode<cr>", desc = "Zen Mode" } },
	},

	require 'plugins.debug',

	-- NOTE: The import below can automatically add your own plugins, configuration, etc from `lua/custom/plugins/*.lua`
	--    You can use this folder to prevent any conflicts with this init.lua if you're interested in keeping
	--    up-to-date with whatever is in the kickstart repo.
	--    Uncomment the following line and add your plugins to `lua/custom/plugins/*.lua` to get going.
	--
	--    For additional information see: https://github.com/folke/lazy.nvim#-structuring-your-plugins
	-- { import = 'custom.plugins' },
})

-- [[ Setting options ]]
-- See `:help vim.o`


-- [[ Basic Keymaps ]]

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
-- vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })

-- [[ Configure Telescope ]]
-- See `:help telescope` and `:help telescope.setup()`
require('telescope').setup {
	defaults = {
		mappings = {
			i = {
				['<C-u>'] = false,
				['<C-d>'] = false,
			},
		},
	},
}

-- Enable telescope fzf native, if installed
pcall(require('telescope').load_extension, 'fzf')

-- Telescope live_grep in git root
-- Function to find the git root directory based on the current buffer's path
local function find_git_root()
	-- Use the current buffer's path as the starting point for the git search
	local current_file = vim.api.nvim_buf_get_name(0)
	local current_dir
	local cwd = vim.fn.getcwd()
	-- If the buffer is not associated with a file, return nil
	if current_file == '' then
		current_dir = cwd
	else
		-- Extract the directory from the current file's path
		current_dir = vim.fn.fnamemodify(current_file, ':h')
	end

	-- Find the Git root directory from the current file's path
	local git_root = vim.fn.systemlist('git -C ' .. vim.fn.escape(current_dir, ' ') .. ' rev-parse --show-toplevel')[1]
	if vim.v.shell_error ~= 0 then
		print 'Not a git repository. Searching on current working directory'
		return cwd
	end
	return git_root
end

-- Custom live_grep function to search in git root
local function live_grep_git_root()
	local git_root = find_git_root()
	if git_root then
		require('telescope.builtin').live_grep {
			search_dirs = { git_root },
		}
	end
end

vim.api.nvim_create_user_command('LiveGrepGitRoot', live_grep_git_root, {})

-- See `:help telescope.builtin`
vim.keymap.set('n', '<leader>?', require('telescope.builtin').oldfiles, { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader><space>', require('telescope.builtin').buffers, { desc = '[ ] Find existing buffers' })
vim.keymap.set('n', '<leader>/', function()
	-- You can pass additional configuration to telescope to change theme, layout, etc.
	require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
		winblend = 10,
		previewer = false,
	})
end, { desc = '[/] Fuzzily search in current buffer' })

local function telescope_live_grep_open_files()
	require('telescope.builtin').live_grep {
		grep_open_files = true,
		prompt_title = 'Live Grep in Open Files',
	}
end
vim.keymap.set('n', '<leader>s/', telescope_live_grep_open_files, { desc = '[S]earch [/] in Open Files' })
vim.keymap.set('n', '<leader>ss', require('telescope.builtin').builtin, { desc = '[S]earch [S]elect Telescope' })
vim.keymap.set('n', '<leader>gf', require('telescope.builtin').git_files, { desc = 'Search [G]it [F]iles' })
vim.keymap.set('n', '<leader>sf', require('telescope.builtin').find_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>sh', require('telescope.builtin').help_tags, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sw', require('telescope.builtin').grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>sg', require('telescope.builtin').live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>sG', ':LiveGrepGitRoot<cr>', { desc = '[S]earch by [G]rep on Git Root' })
vim.keymap.set('n', '<leader>sd', require('telescope.builtin').diagnostics, { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<leader>sr', require('telescope.builtin').resume, { desc = '[S]earch [R]esume' })
vim.keymap.set('n', '<C-t>', require('telescope.builtin').find_files, { noremap = true, desc = 'Find files' })
vim.keymap.set('n', '<C-p>', require('telescope.builtin').live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>fc', require('telescope.builtin').colorscheme, { desc = 'Find Colorschemes' })

-- [[ Configure Treesitter ]]
-- See `:help nvim-treesitter`
-- Defer Treesitter setup after first render to improve startup time of 'nvim {filename}'
vim.defer_fn(function()
	require('nvim-treesitter.configs').setup {
		-- Add languages to be installed here that you want installed for treesitter
		ensure_installed = { 'c', 'cpp', 'go', 'lua', 'python', 'rust', 'tsx', 'javascript', 'typescript', 'vimdoc', 'vim', 'bash', 'ruby', 'hcl', 'terraform' },

		-- Autoinstall languages that are not installed.
		auto_install = false,
		-- Install languages synchronously (only applied to `ensure_installed`)
		sync_install = false,
		-- List of parsers to ignore installing
		ignore_install = {},
		-- You can specify additional Treesitter modules here: -- For example: -- playground = {--enable = true,-- },
		modules = {},
		highlight = { enable = true },
		indent = { enable = false }, -- true screws up indents with custom tabstop (e.g. tabstop=2 in go files)
		incremental_selection = {
			enable = true,
			keymaps = {
				init_selection = '<c-space>',
				node_incremental = '<c-space>',
				scope_incremental = '<c-s>',
				node_decremental = '<M-space>',
			},
		},
		textobjects = {
			select = {
				enable = true,
				lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
				keymaps = {
					-- You can use the capture groups defined in textobjects.scm
					['aa'] = '@parameter.outer',
					['ia'] = '@parameter.inner',
					['af'] = '@function.outer',
					['if'] = '@function.inner',
					['ac'] = '@class.outer',
					['ic'] = '@class.inner',
				},
			},
			move = {
				enable = true,
				set_jumps = true, -- whether to set jumps in the jumplist
				goto_next_start = {
					[']m'] = '@function.outer',
					[']]'] = '@class.outer',
				},
				goto_next_end = {
					[']M'] = '@function.outer',
					[']['] = '@class.outer',
				},
				goto_previous_start = {
					['[m'] = '@function.outer',
					['[['] = '@class.outer',
				},
				goto_previous_end = {
					['[M'] = '@function.outer',
					['[]'] = '@class.outer',
				},
			},
			swap = {
				enable = true,
				swap_next = {
					['<leader>a'] = '@parameter.inner',
				},
				swap_previous = {
					['<leader>A'] = '@parameter.inner',
				},
			},
		},
	}
end, 0)

-- [[ Configure LSP ]]
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(_, bufnr)
	-- For LSP related items. It sets the mode, buffer and description for us each time.
	local nmap = function(keys, func, desc)
		if desc then
			desc = 'LSP: ' .. desc
		end

		vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
	end

	nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
	nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

	nmap('gd', require('telescope.builtin').lsp_definitions, '[G]oto [D]efinition')
	nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
	nmap('gI', require('telescope.builtin').lsp_implementations, '[G]oto [I]mplementation')
	nmap('<leader>D', require('telescope.builtin').lsp_type_definitions, 'Type [D]efinition')
	nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
	nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

	-- See `:help K` for why this keymap
	nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
	nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

	-- Lesser used LSP functionality
	nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
	nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
	nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
	nmap('<leader>wl', function()
		print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
	end, '[W]orkspace [L]ist Folders')

	-- Create a command `:Format` local to the LSP buffer
	vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
		vim.lsp.buf.format()
	end, { desc = 'Format current buffer with LSP' })
end

-- document existing key chains
require('which-key').register {
	['<leader>c'] = { name = '[C]ode', _ = 'which_key_ignore' },
	['<leader>d'] = { name = '[D]ocument', _ = 'which_key_ignore' },
	['<leader>g'] = { name = '[G]it', _ = 'which_key_ignore' },
	['<leader>h'] = { name = 'Git [H]unk', _ = 'which_key_ignore' },
	['<leader>r'] = { name = '[R]ename', _ = 'which_key_ignore' },
	['<leader>s'] = { name = '[S]earch', _ = 'which_key_ignore' },
	['<leader>t'] = { name = '[T]oggle', _ = 'which_key_ignore' },
	['<leader>w'] = { name = '[W]orkspace', _ = 'which_key_ignore' },
}
-- register which-key VISUAL mode
-- required for visual <leader>hs (hunk stage) to work
require('which-key').register({
	['<leader>'] = { name = 'VISUAL <leader>' },
	['<leader>h'] = { 'Git [H]unk' },
}, { mode = 'v' })

-- mason-lspconfig requires that these setup functions are called in this order
-- before setting up the servers.
require('mason').setup()
require('mason-lspconfig').setup()
--  https://github.com/williamboman/mason-lspconfig.nvim
local servers = {
	-- clangd = {},
	-- snyk_ls = {},
	bashls = {},
	htmx = {},
	jsonls = {},
	eslint = {},
	dockerls = {},
	docker_compose_language_service = {},
	gopls = {},
	pyright = {},
	ruby_lsp = {},
	solargraph = {
		cmd = { os.getenv("HOME") .. "/.rbenv/shims/solargraph", 'stdio' },
		-- root_dir = nvim_lsp.util.root_pattern("Gemfile", ".git", "."),
		settings = {
			solargraph = {
				autoformat = false,
				completion = true,
				diagnostic = false,
				folding = true,
				references = true,
				rename = true,
				symbols = true
			}
		}
	},
	-- rust_analyzer = {},
	tsserver = {}, -- note: must run terrafor/terragrunt init first for lsp to work
	html = { filetypes = { 'html', 'twig', 'hbs' } },
	terraformls = {},
	tflint = {},
	sqls = {},
	lua_ls = {
		Lua = {
			runtime = {
				-- Tell the language server which version of Lua you're using
				-- (most likely LuaJIT in the case of Neovim)
				version = 'LuaJIT',
			},
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = {
					'vim',
					'require'
				},
			},
			workspace = {
				-- Make the server aware of Neovim runtime files
				library = vim.api.nvim_get_runtime_file("", true),
				-- checkThirdParty = false
			},
			-- Do not send telemetry data containing a randomized but unique identifier
			-- is this still needed?
			telemetry = { enable = false },

			-- telemetry = { enable = false },
			-- NOTE: toggle below to ignore Lua_LS's noisy `missing-fields` warnings
			-- diagnostics = { disable = { 'missing-fields' } },
		},
	},
}

-- Setup neovim lua configuration
require('neodev').setup()

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
require('cmp_nvim_lsp').setup()

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup {
	ensure_installed = vim.tbl_keys(servers),
}

mason_lspconfig.setup_handlers {
	function(server_name)
		require('lspconfig')[server_name].setup {
			capabilities = capabilities,
			on_attach = on_attach,
			settings = servers[server_name],
			filetypes = (servers[server_name] or {}).filetypes,
		}
	end,
}

-- Configure nvim-cmp
-- See `:help cmp`
local cmp = require 'cmp'
local luasnip = require 'luasnip'
require('luasnip.loaders.from_vscode').lazy_load()
luasnip.config.setup {}

cmp.setup {
	snippet = {
		expand = function(args)
			luasnip.lsp_expand(args.body)
		end,
	},
	completion = {
		completeopt = 'menu,menuone,noinsert', -- display completion men even if there is only one item and don't autoinsert
	},
	mapping = cmp.mapping.preset.insert {
		['<C-n>'] = cmp.mapping.select_next_item(),
		['<C-p>'] = cmp.mapping.select_prev_item(),
		['<C-b>'] = cmp.mapping.scroll_docs(-4),
		['<C-f>'] = cmp.mapping.scroll_docs(4),
		-- ['<C-Space>'] = cmp.mapping.complete {},
		['<C-y>'] = cmp.mapping.complete {}, -- c-space is currently set to treesitter select node
		['<CR>'] = cmp.mapping.confirm {
			behavior = cmp.ConfirmBehavior.Replace,
			select = true,
		},
		['<Tab>'] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			elseif luasnip.expand_or_locally_jumpable() then
				luasnip.expand_or_jump()
			else
				fallback()
			end
		end, { 'i', 's' }),
		['<S-Tab>'] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			elseif luasnip.locally_jumpable(-1) then
				luasnip.jump(-1)
			else
				fallback()
			end
		end, { 'i', 's' }),
	},
	sources = {
		{ name = 'copilot' },
		{ name = 'nvim_lsp' },
		{ name = 'luasnip' },
		{ name = 'path' },
		{ name = 'buffer' },
	},
}

-- ========================================================================== --
-- ==                         PLUGIN CONFIGURATION                         == --
-- ========================================================================== --

-- Colorscheme
vim.opt.termguicolors = true
vim.cmd.colorscheme('tokyonight-night')

-- misc
vim.g.session_autoload = 'no'

-- terraform
-- fixme: convert this
-- vim.cmd([[silent! autocmd! filetypedetect BufRead,BufNewFile *.tf]])
-- vim.cmd([[autocmd BufRead,BufNewFile *.hcl set filetype=hcl]])
-- vim.cmd([[autocmd BufRead,BufNewFile .terraformrc,terraform.rc set filetype=hcl]])
-- vim.cmd([[autocmd BufRead,BufNewFile *.tf,*.tfvars set filetype=terraform]])
-- vim.cmd([[autocmd BufRead,BufNewFile *.tfstate,*.tfstate.backup set filetype=json]])
-- Define a function to set filetype based on file pattern
local function set_filetype(pattern, filetype)
	vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
		pattern = pattern,
		command = "set filetype=" .. filetype,
	})
end

-- Set filetype for various file patterns
set_filetype({ "*.tf", "*.tfvars" }, "terraform")
set_filetype({ "*.hcl", ".terraformrc", "terraform.rc" }, "hcl")
set_filetype({ "*.tfstate", "*.tfstate.backup" }, "json")

map("n", "<S-q>", "<cmd>NvimTreeToggle<cr>", { desc = "Toggle File tree" })

-- Golang / vim-go
vim.api.nvim_create_autocmd("FileType", {
	pattern = "go",
	callback = function()
		vim.api.nvim_set_keymap("n", "<leader>gt", ":Tagbar<CR>", { noremap = true, silent = true })
		-- why do none of these actually work??
		vim.opt.expandtab = false
		vim.opt.shiftwidth = 2
		vim.opt.softtabstop = 2
		vim.opt.tabstop = 2
	end,
})
vim.g.go_def_mapping_enabled = 0 -- keep my ctrl-t


-- must be before lualine
require('nvim-navic').setup { lsp = { auto_attach = true, } }
local navic = require("nvim-navic")

-- lualine.nvim (statusline)
vim.opt.showmode = false
require('lualine').setup({
	options = {
		icons_enabled = false,
		theme = 'tokyonight',
		component_separators = '|',
		section_separators = '',
	},
	winbar = {
		lualine_c = {
			{
				function()
					return navic.get_location()
				end,
				cond = function()
					return navic.is_available()
				end
			},
		}
	}
})

-- neoai
require("neoai").setup({
	ui = {
		output_popup_text = "NeoAI",
		input_popup_text = "Prompt",
		width = 30,             -- As percentage eg. 30%
		output_popup_height = 80, -- As percentage eg. 80%
		submit = "<Enter>",     -- Key binding to submit the prompt
	},
	models = {
		{
			name = "openai",
			model = "gpt-3.5-turbo",
			-- model = "gpt-4o", -- expensive
			params = nil,
		},
	},
	register_output = {
		["g"] = function(output)
			return output
		end,
		["c"] = require("neoai.utils").extract_code_snippets,
	},
	inject = {
		cutoff_width = 75,
	},
	prompts = {
		context_prompt = function(context)
			return "Hey, I'd like to provide some context for future "
					.. "messages. Here is the code/text that I want to refer "
					.. "to in our upcoming conversations:\n\n"
					.. context
		end,
	},
	mappings = {
		["select_up"] = "<C-k>",
		["select_down"] = "<C-j>",
	},
	open_ai = {
		api_key = {
			env = "OPENAI_API_KEY",
			value = nil,
			-- `get` is is a function that retrieves an API key, can be used to override the default method.
			-- get = function() ... end

			-- Here is some code for a function that retrieves an API key. You can use it with
			-- the Linux 'pass' application.
			-- get = function()
			--     local key = vim.fn.system("pass show openai/mytestkey")
			--     key = string.gsub(key, "\n", "")
			--     return key
			-- end,
		},
	},
	shortcuts = {
		{
			name = "textify",
			key = "<leader>as",
			desc = "fix text with AI",
			use_context = true,
			prompt = [[
        Please rewrite the text to make it more readable, clear,
        concise, and fix any grammatical, punctuation, or spelling
        errors
        ]],
			modes = { "v" },
			strip_function = nil,
		},
		{
			name = "gitcommit",
			key = "<leader>ag",
			desc = "generate git commit message",
			use_context = false,
			prompt = function()
				return [[
          Using the following git diff generate a consise and
          clear git commit message, with a short title summary
          that is 75 characters or less:
          ]] .. vim.fn.system("git diff --cached")
			end,
			modes = { "n" },
			strip_function = nil,
		},
	},
})

-- [[ Custom Commands ]]
vim.api.nvim_create_user_command('DiagnosticToggle', function()
	local current_state = vim.diagnostic.config().virtual_text
	vim.diagnostic.config({
		virtual_text = not current_state
	})
end, {})

local autocmd = vim.api.nvim_create_autocmd

-- When opening a file, jump to the last known cursor position
autocmd("BufReadPost", {
	pattern = "*",
	callback = function()
		local last_pos = vim.fn.line("'\"")
		if last_pos > 0 and last_pos <= vim.fn.line("$") then
			vim.api.nvim_win_set_cursor(0, { last_pos, 0 })
		end
	end,
})


-- folding / https://github.com/kevinhwang91/nvim-ufo
vim.o.foldcolumn = '0' -- '1' to show folding in left gutter
vim.o.foldlevel = 99   -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 99
vim.o.foldenable = true

-- Using ufo provider need remap `zR` and `zM`
vim.keymap.set('n', 'zR', require('ufo').openAllFolds)
vim.keymap.set('n', 'zM', require('ufo').closeAllFolds)
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.foldingRange = {
	dynamicRegistration = false,
	lineFoldingOnly = true
}
local language_servers = require("lspconfig").util.available_servers() -- or list servers manually like {'gopls', 'clangd'}
for _, ls in ipairs(language_servers) do
	require('lspconfig')[ls].setup({
		capabilities = capabilities
		-- you can add other fields for setting up lsp server in this table
	})
end
require('ufo').setup()

-- Tips I always forget
-- vertical split -> horizontal: ctrl+w then J
-- horizontal split -> vertical: ctrl+w H or ctrl+w L
-- reload all buffers - :bufdo e
-- :w !sudo tee %
-- gx - open link in browser
-- :Ack <C-R><C-W> " use c-r/c-w to paste word under cursor into ex command prompt
-- `. - go to last line edited / '' - go to start of last line edited
-- g; / g, - jump through changelist (:help changelist)
-- change all buffers to tabs - :tab sball
-- gf in new tab: <c-w>gF - open in a new tab (Ctrl-w gF)
-- verbose <cmd/func> - debug info
-- vim --startuptime /tmp/startup.log +q && vim /tmp/startup.log
-- :messages if a message scrolls by too fast (e.g. error on startup)
-- C-wL vertical (top/bot) split to horiz (left/right) split (C-wJ to go back)
-- c-a / c-x -- increment / decrement number
-- delete blank lines -> :g/^$/d
--       or :%s/\n\n/\r/
-- delete multiple blank lines: :%!cat -s
-- verbose Xmap <leader>c # show imap/nmap/map/etc for <leader>c or whatnot
-- show value of set var with e.g. `set modeline?`; let is just the var `let g:plugin_var`
-- :enew|pu=execute('<colon command>') " copy the output of any :colon command to a new buffer
-- zz/. - center current liner horizontally on the screen (z -/+ or b/t to put current line at bottom/top)
-- LSPStop - brute force way to disable annoying inline linting messages
-- Telescope
--   commands
--   command_history
