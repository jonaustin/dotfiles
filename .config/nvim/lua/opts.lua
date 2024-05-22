-- [[ Setting options ]]
-- See `:help vim.o`

local home = os.getenv('HOME')

-- misc
vim.g.session_autoload = 'no'

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
