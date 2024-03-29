-- ========================================================================== --
-- ==                           EDITOR SETTINGS                            == --
-- ========================================================================== --

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
vim.o.autoindent = true -- indent at the same level of the previous line
vim.o.wrap = true -- wrap long lines
vim.o.autoread = true -- auto reload file if it changes outside of vim
vim.o.guicursor='a:hor20-Cursor' -- underline cursor
vim.o.termguicolors = true -- 24bit (true) colors
vim.o.breakindent = true -- wrap lines with same indent

-- Speed
vim.opt.lazyredraw = true -- fix slowdown issues when moving cursor with syntax on
vim.opt.ttyfast = true -- assume fast connection (smoother redraw)
vim.opt.synmaxcol=1024 -- Syntax coloring lines that are too long just slows down the world

-- give in to my muscle memory
vim.o.undofile = true
vim.o.backup = true
vim.o.backupdir = home .. "/.vimbackup"
vim.o.directory = home .. "/.vimswap"
vim.o.viewdir   = home .. "/.vimviews"
vim.o.undodir   = home .. "/.vimundo"

-- Case-insensitive searching UNLESS \C or Capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

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
map("n", "<C-q>", "<cmd>quit<cr>", { desc = "Quit vim"})
--map("<leader>W", "<cmd>%s/\s\+$//<cr><cmd>let @/=''<CR>")
-- map("<leader>.", "<cmd>nohl<cr>")

-- clipboard
map("n", "<leader>ya", "<cmd>%y+<cr>")

-- lazy
map("n", "<leader>l", "<cmd>Lazy<cr>", { desc = "Lazy" })

-- quickfix
map("n", "[q", vim.cmd.cprev, { desc = "Previous quickfix" })
map("n", "]q", vim.cmd.cnext, { desc = "Next quickfix" })

-- ========================================================================== --
-- ==                               COMMANDS                               == --
-- ========================================================================== --

vim.api.nvim_create_user_command('ReloadConfig', 'source $MYVIMRC', {})

local group = vim.api.nvim_create_augroup('user_cmds', {clear = true})



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

  -- Git related plugins
  'tpope/vim-fugitive',
  'tpope/vim-rhubarb', -- :GBrowse

  -- Detect tabstop and shiftwidth automatically
  'tpope/vim-sleuth',

  'tpope/vim-repeat',
  'tpope/vim-surround', -- use treesitter instead?
  'mbbill/undotree',
  'junegunn/vim-easy-align', -- :EasyAlign /<regex>/
  {'xolox/vim-session', dependencies = {'xolox/vim-misc'}},
  'szw/vim-maximizer', -- F3
  'justinmk/vim-sneak', -- <leader>s<2 chars>

  -- colorschemes
  'folke/tokyonight.nvim',

  -- integrations
  -- 'stevearc/oil.nvim', -- edit your filesystem like a buffer
  'christoomey/vim-tmux-navigator',
  {
    "nvim-tree/nvim-tree.lua",
    version = "*",
    lazy = false,
    -- dependencies = {
      --   "nvim-tree/nvim-web-devicons",
      -- },
      config = function()
        require("nvim-tree").setup {}
      end,
    },

    'kyazdani42/nvim-web-devicons',
    'nvim-lualine/lualine.nvim',
    'nvim-lua/plenary.nvim',
    'majutsushi/tagbar',
    'github/copilot.vim',
    'tpope/vim-commentary',
    {'bakks/butterfish.nvim', dependencies = {'tpope/vim-commentary'}},
    {'Bryley/neoai.nvim', dependencies = { "MunifTanjim/nui.nvim", }},
    {"jellydn/CopilotChat.nvim",
    opts = {
      mode = "split", -- newbuffer or split  , default: newbuffer
    },
    build = function()
      vim.defer_fn(function()
        vim.cmd("UpdateRemotePlugins")
        vim.notify("CopilotChat - Updated remote plugins. Please restart Neovim.")
      end, 3000)
    end,
    event = "VeryLazy",
    keys = {
      { "<leader>cce", "<cmd>CopilotChatExplain<cr>", desc = "CopilotChat - Explain code" },
      { "<leader>cct", "<cmd>CopilotChatTests<cr>", desc = "CopilotChat - Generate tests" },
    },
  },

  -- kickstart

  -- NOTE: This is where your plugins related to LSP can be installed.
  --  The configuration is done below. Search for lspconfig to find it below.
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
      { 'j-hui/fidget.nvim', opts = {} },

      'folke/neodev.nvim', -- nvim lua development stuff
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
      'hrsh7th/cmp-path', -- completion for local files

      -- Adds a number of user-friendly snippets
      'rafamadriz/friendly-snippets',
    },
  },

  -- Useful plugin to show you pending keybinds.
  { 'folke/which-key.nvim', opts = {} },
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

  -- Fuzzy Finder (files, lsp, etc)
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x', -- update this
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

  -- NOTE: Next Step on Your Neovim Journey: Add/Configure additional "plugins" for kickstart
  --       These are some example plugins that I've included in the kickstart repository.
  --       Uncomment any of the lines below to enable them.
  -- require 'kickstart.plugins.autoformat',
  -- require 'kickstart.plugins.debug',

  -- NOTE: The import below can automatically add your own plugins, configuration, etc from `lua/custom/plugins/*.lua`
  --    You can use this folder to prevent any conflicts with this init.lua if you're interested in keeping
  --    up-to-date with whatever is in the kickstart repo.
  --    Uncomment the following line and add your plugins to `lua/custom/plugins/*.lua` to get going.
  --
  --    For additional information see: https://github.com/folke/lazy.nvim#-structuring-your-plugins
  -- { import = 'custom.plugins' },
}, {})

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
vim.keymap.set('n', '<C-t>', require('telescope.builtin').find_files, { desc = 'Find files' })
vim.keymap.set('n', '<C-p>', require('telescope.builtin').live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>fc', require('telescope.builtin').colorscheme, { desc = 'Find Colorschemes' })

-- [[ Configure Treesitter ]]
-- See `:help nvim-treesitter`
-- Defer Treesitter setup after first render to improve startup time of 'nvim {filename}'
vim.defer_fn(function()
  require('nvim-treesitter.configs').setup {
    -- Add languages to be installed here that you want installed for treesitter
    ensure_installed = { 'c', 'cpp', 'go', 'lua', 'python', 'rust', 'tsx', 'javascript', 'typescript', 'vimdoc', 'vim', 'bash', 'ruby', 'hcl', 'terraform' },

    -- Autoinstall languages that are not installed. Defaults to false (but you can change for yourself!)
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
  ruby_ls = {},
  solargraph = {
    cmd = { os.getenv( "HOME" ) .. "/.rbenv/shims/solargraph", 'stdio' },
    -- root_dir = nvim_lsp.util.root_pattern("Gemfile", ".git", "."),
    settings = {
      solargraph = {
        autoformat = true,
        completion = true,
        diagnostic = true,
        folding = true,
        references = true,
        rename = true,
        symbols = true
      }
    }
  },
  -- rust_analyzer = {},
  tsserver = {}, -- note: must run terrafor/terragrunt init first for lsp to work
  html = { filetypes = { 'html', 'twig', 'hbs'} },
  terraformls = {},
  tflint = {},
  sqls = {},
  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
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
    completeopt = 'menu,menuone,noinsert',
  },
  mapping = cmp.mapping.preset.insert {
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete {},
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
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = 'path' },
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
vim.g.terraform_fmt_on_save=1
vim.g.terraform_align=1
vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = "*.hcl",
  callback = function()
    vim.cmd("setfiletype terraform")
  end
})
-- fixme: convert this
-- vim.cmd([[silent! autocmd! filetypedetect BufRead,BufNewFile *.tf]])
-- vim.cmd([[autocmd BufRead,BufNewFile *.hcl set filetype=hcl]])
-- vim.cmd([[autocmd BufRead,BufNewFile .terraformrc,terraform.rc set filetype=hcl]])
-- vim.cmd([[autocmd BufRead,BufNewFile *.tf,*.tfvars set filetype=terraform]])
-- vim.cmd([[autocmd BufRead,BufNewFile *.tfstate,*.tfstate.backup set filetype=json]])
-- Define a function to set filetype based on file pattern
local function set_filetype(pattern, filetype)
  vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
    pattern = pattern,
    command = "set filetype="..filetype,
  })
end

-- Set filetype for various file patterns
set_filetype("*.tf", "terraform") -- No longer silent for *.tf
set_filetype("*.hcl", "hcl")
set_filetype({".terraformrc", "terraform.rc"}, "hcl")
set_filetype("*.tfvars", "terraform")
set_filetype({"*.tfstate", "*.tfstate.backup"}, "json")

map("n", "<S-q>", "<cmd>NvimTreeToggle<cr>", { desc = "Toggle File tree"})

-- Golang
vim.api.nvim_create_autocmd("FileType", {
  pattern = "go",
  callback = function()
    vim.api.nvim_set_keymap("n", "<leader>gt", ":Tagbar<CR>", { noremap = true, silent = true })
    -- why do none of these actually work??
    vim.opt.expandtab = false
    vim.opt.shiftwidth = 2
    vim.opt.softtabstop = 2
    vim.opt.tabstop = 2 -- why the hell doesn't vim-go respect my tabstop? i.e. this does nothing (works fine in old vimrc though..)
  end,
})

-- lualine.nvim (statusline)
vim.opt.showmode = false
require('lualine').setup({
  options = {
    icons_enabled = false,
    theme = 'tokyonight',
    component_separators = '|',
    section_separators = '',
  },
})

-- butterfish
local butterfish = require('butterfish')
local opts = {noremap = true, silent = true}
-- vim.keymap.set('n', '<leader>p', ':BFFilePrompt ',   opts)
-- vim.keymap.set('n', '<leader>r', ':BFRewrite ',      opts)
-- vim.keymap.set('v', '<leader>r', ':BFRewrite ',      opts)
-- vim.keymap.set('n', '<leader>c', ':BFComment<CR>',   opts)
-- vim.keymap.set('v', '<leader>c', ':BFComment<CR>',   opts)
-- vim.keymap.set('n', '<leader>e', ':BFExplain<CR>',   opts)
-- vim.keymap.set('v', '<leader>e', ':BFExplain<CR>',   opts)
-- vim.keymap.set('n', '<leader>f', ':BFFix<CR>',       opts)
-- vim.keymap.set('n', '<leader>i', ':BFImplement<CR>', opts)
-- vim.keymap.set('n', '<leader>d', ':BFEdit ',         opts)
-- vim.keymap.set('n', '<leader>h', ':BFHammer<CR>',    opts)
-- vim.keymap.set('n', '<leader>q', ':BFQuestion ',     opts)
-- vim.keymap.set('v', '<leader>q', ':BFQuestion ',     opts)

-- neoai
require("neoai").setup({
  ui = {
    output_popup_text = "NeoAI",
    input_popup_text = "Prompt",
    width = 30, -- As percentage eg. 30%
    output_popup_height = 80, -- As percentage eg. 80%
    submit = "<Enter>", -- Key binding to submit the prompt
  },
  models = {
    {
      name = "openai",
      model = "gpt-3.5-turbo",
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
