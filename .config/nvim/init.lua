-- easy/quick folding nvim-ufo
--	za - toggle fold under cursor; zM close all, zR open all
-- https://github.com/cshuaimin/ssr.nvim - structural search and replace
--   or maybe https://github.com/nvim-pack/nvim-spectre
-- https://github.com/folke/flash.nvim - maybe replace sneak
-- https://github.com/sindrets/diffview.nvim - better diff viewer
-- https://github.com/VonHeikemen/lazy-template
-- replace maximize maybe with https://github.com/folke/dot/blob/master/nvim/lua/plugins/ui.lua#L29C6-L29C29
-- https://github.com/AckslD/nvim-neoclip.lua
-- https://github.com/ThePrimeagen/refactoring.nvim
-- https://github.com/DNLHC/glance.nvim

require 'opts'
require 'keys'
require 'filetypes'

-- ========================================================================== --
-- ==                               COMMANDS                               == --
-- ========================================================================== --

-- local group = vim.api.nvim_create_augroup('user_cmds', {clear = true})

-- ========================================================================== --
-- ==                               PLUGINS                                == --
-- ========================================================================== --
local lazy = {}

function lazy.install(path)
  if not vim.loop.fs_stat(path) then
    print 'Installing lazy.nvim....'
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

lazy.path = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
lazy.opts = {
  change_detection = { enabled = false } -- disable as it's annoying and doesn't really work
}

-- https://github.com/folke/lazy.nvim#-structuring-your-plugins
lazy.setup 'plugins'

-- [[ Configure Telescope ]]
-- See `:help telescope` and `:help telescope.setup()`
require('telescope').setup({
  defaults = {
    mappings = {
      i = {
        ['<C-u>'] = false,
        ['<C-d>'] = false,
      },
    },
  },
})

-- Enable telescope fzf native, if installed
pcall(require('telescope').load_extension, 'fzf')
require('telescope').load_extension 'refactoring'
vim.keymap.set({ 'n', 'x' }, '<leader>rr', function()
  require('telescope').extensions.refactoring.refactors()
end)
require('telescope').load_extension 'dap'

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
    require('telescope.builtin').live_grep({
      search_dirs = { git_root },
    })
  end
end

vim.api.nvim_create_user_command('LiveGrepGitRoot', live_grep_git_root, {})

-- See `:help telescope.builtin`
vim.keymap.set('n', '<leader>?', require('telescope.builtin').oldfiles, { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader><space>', require('telescope.builtin').buffers, { desc = '[ ] Find existing buffers' })
vim.keymap.set('n', '<leader>/', function()
  -- You can pass additional configuration to telescope to change theme, layout, etc.
  require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown({
    winblend = 10,
    previewer = false,
  }))
end, { desc = '[/] Fuzzily search in current buffer' })

local function telescope_live_grep_open_files()
  require('telescope.builtin').live_grep({
    grep_open_files = true,
    prompt_title = 'Live Grep in Open Files',
  })
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
  require('nvim-treesitter.configs').setup({
    -- Add languages to be installed here that you want installed for treesitter
    ensure_installed = {
      'asm',
      'astro',
      'bash',
      'c',
      'cpp',
      'c_sharp',
      'css',
      'diff',
      'dockerfile',
      'git_config',
      'git_rebase',
      'gitattributes',
      'gitcommit',
      'gitignore',
      'go',
      'gomod',
      'gosum',
      'gotmpl',
      'gowork',
      'hcl',
      'helm',
      'html',
      'htmldjango',
      'java',
      'javascript',
      'jsdoc',
      'json',
      'jsonc',
      'kdl',
      'lua',
      'luadoc',
      'luap',
      'luau',
      'markdown',
      'markdown_inline',
      'ocaml',
      'ocaml_interface',
      'python',
      'query',
      'regex',
      'requirements',
      'rust',
      'scheme',
      'sql',
      'svelte',
      'terraform',
      'toml',
      'tsx',
      'typescript',
      'vim',
      'vimdoc',
      'vue',
      'xml',
      'yaml',
    },

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
  })
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

-- fixme: move into whichkey.lua
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
  -- sorbet = {}, -- doesnt seem to work
  ruby_lsp = {}, -- annoyingly doesnt support go to definition; just use vim-rails gf
  -- solargraph = {
  -- 	cmd = { os.getenv("HOME") .. "/.local/share/mise/installs/ruby/2.7.8/bin/solargraph", 'stdio' },
  -- 	-- root_dir = nvim_lsp.util.root_pattern("Gemfile", ".git", "."),
  -- 	settings = {
  -- 		solargraph = {
  -- 			autoformat = false,
  -- 			completion = true,
  -- 			diagnostic = false,
  -- 			folding = true,
  -- 			references = true,
  -- 			rename = true,
  -- 			symbols = true
  -- 		}
  -- 	}
  -- },
  -- rust_analyzer = {},
  -- ts_ls = {},
  html = { filetypes = { 'html', 'twig', 'hbs' } },
  terraformls = {}, -- note: must run terrafor/terragrunt init first for lsp to work
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
          'require',
        },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file('', true),
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
require('neodev').setup({
  -- enable type checking, docs, and autocompletion for all api functions
  library = { plugins = { 'nvim-dap-ui' }, types = true }, -- https://github.com/rcarriga/nvim-dap-ui?tab=readme-ov-file#installation
})

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
require('cmp_nvim_lsp').setup()

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup({
  ensure_installed = vim.tbl_keys(servers),
})

mason_lspconfig.setup_handlers({
  function(server_name)
    require('lspconfig')[server_name].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      settings = servers[server_name],
      filetypes = (servers[server_name] or {}).filetypes,
    })
  end,
})

-- Configure nvim-cmp
-- See `:help cmp`
local cmp = require 'cmp'
local luasnip = require 'luasnip'
require('luasnip.loaders.from_vscode').lazy_load()
luasnip.config.setup({})

cmp.setup({
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  completion = {
    completeopt = 'menu,menuone,noinsert,popup', -- display completion men even if there is only one item and don't autoinsert
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-e>'] = cmp.mapping.abort(),
    -- ['<C-Space>'] = cmp.mapping.complete {},
    ['<C-y>'] = cmp.mapping.complete({}), -- c-space is currently set to treesitter select node
    ['<CR>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    }),
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
  }),
  sources = { -- note: order matters; determines which show up first in completion menu
    { name = 'nvim_lsp' },
    { name = 'copilot' },
    { name = 'luasnip' },
    { name = 'path' },
    { name = 'buffer' },
  },
})

-- ========================================================================== --
-- ==                         PLUGIN CONFIGURATION                         == --
-- ========================================================================== --

-- Colorscheme
vim.opt.termguicolors = true
vim.cmd.colorscheme 'tokyonight-night'

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
vim.keymap.set('n', '<S-q>', '<cmd>NvimTreeToggle<cr>', { desc = 'Toggle File tree' })

-- Golang / vim-go
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'go',
  callback = function()
    vim.api.nvim_set_keymap('n', '<leader>gt', ':Tagbar<CR>', { noremap = true, silent = true })
    -- why do none of these actually work??
    vim.opt.expandtab = false
    vim.opt.shiftwidth = 2
    vim.opt.softtabstop = 2
    vim.opt.tabstop = 2
  end,
})
vim.g.go_def_mapping_enabled = 0 -- keep my ctrl-t; just use :GoDef or C-]

-- must be before lualine
-- require('nvim-navic').setup { lsp = { auto_attach = true, } }
-- local navic = require("nvim-navic")

-- -- lualine.nvim (statusline)
-- vim.opt.showmode = false
-- require('lualine').setup({
-- 	options = {
-- 		icons_enabled = false,
-- 		theme = 'tokyonight',
-- 		component_separators = '|',
-- 		section_separators = '',
-- 	},
-- 	winbar = {
-- 		lualine_c = {
-- 			{
-- 				function()
-- 					return navic.get_location()
-- 				end,
-- 				cond = function()
-- 					return navic.is_available()
-- 				end
-- 			},
-- 		}
-- 	}
-- })

-- neoai
require('neoai').setup({
  ui = {
    output_popup_text = 'NeoAI',
    input_popup_text = 'Prompt',
    width = 30, -- As percentage eg. 30%
    output_popup_height = 80, -- As percentage eg. 80%
    submit = '<Enter>', -- Key binding to submit the prompt
  },
  models = {
    {
      name = 'openai',
      model = 'gpt-3.5-turbo',
      -- model = "gpt-4o", -- expensive
      params = nil,
    },
  },
  register_output = {
    ['g'] = function(output)
      return output
    end,
    ['c'] = require('neoai.utils').extract_code_snippets,
  },
  inject = {
    cutoff_width = 75,
  },
  prompts = {
    context_prompt = function(context)
      return "Hey, I'd like to provide some context for future "
        .. 'messages. Here is the code/text that I want to refer '
        .. 'to in our upcoming conversations:\n\n'
        .. context
    end,
  },
  mappings = {
    ['select_up'] = '<C-k>',
    ['select_down'] = '<C-j>',
  },
  open_ai = {
    api_key = {
      env = 'OPENAI_API_KEY',
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
      name = 'textify',
      key = '<leader>as',
      desc = 'fix text with AI',
      use_context = true,
      prompt = [[
        Please rewrite the text to make it more readable, clear,
        concise, and fix any grammatical, punctuation, or spelling
        errors
        ]],
      modes = { 'v' },
      strip_function = nil,
    },
    {
      name = 'gitcommit',
      key = '<leader>ag',
      desc = 'generate git commit message',
      use_context = false,
      prompt = function()
        return [[
          Using the following git diff generate a consise and
          clear git commit message, with a short title summary
          that is 75 characters or less:
          ]] .. vim.fn.system 'git diff --cached'
      end,
      modes = { 'n' },
      strip_function = nil,
    },
  },
})

-- [[ Custom Commands ]]
vim.api.nvim_create_user_command('DiagnosticToggle', function()
  local current_state = vim.diagnostic.config().virtual_text
  vim.diagnostic.config({ virtual_text = not current_state, })
end, {})

-- typos
vim.api.nvim_create_user_command('Q', 'quit', {})
vim.api.nvim_create_user_command('Qa', 'quitall', {})
vim.api.nvim_create_user_command('Wq', 'wq', {})

local autocmd = vim.api.nvim_create_autocmd

-- When opening a file, jump to the last known cursor position
autocmd('BufReadPost', {
  pattern = '*',
  callback = function()
    local last_pos = vim.fn.line '\'"'
    if last_pos > 0 and last_pos <= vim.fn.line '$' then
      vim.api.nvim_win_set_cursor(0, { last_pos, 0 })
    end
  end,
})

-- folding / https://github.com/kevinhwang91/nvim-ufo
vim.o.foldcolumn = '0' -- '1' to show folding in left gutter
vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 99
vim.o.foldenable = true

-- Using ufo provider need remap `zR` and `zM`
vim.keymap.set('n', 'zR', require('ufo').openAllFolds)
vim.keymap.set('n', 'zM', require('ufo').closeAllFolds)
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.foldingRange = {
  dynamicRegistration = false,
  lineFoldingOnly = true,
}

-- fixme: for some reason they made this method private: https://github.com/neovim/nvim-lspconfig/issues/3588
local language_servers = require('lspconfig').util.available_servers() -- or list servers manually like {'gopls', 'clangd'}
for _, ls in ipairs(language_servers) do
  require('lspconfig')[ls].setup({
    capabilities = capabilities,
    -- you can add other fields for setting up lsp server in this table
  })
end
require('ufo').setup()

-- nvim-tree preview
local api = require 'nvim-tree.api'
local preview = require 'nvim-tree-preview'

local on_attach = function(bufnr)
  local function opts(desc)
    return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
  end

  vim.keymap.set('n', '<Tab>', function()
    local ok, node = pcall(api.tree.get_node_under_cursor)
    if ok and node then
      if node.type == 'directory' then
        api.node.open.edit()
      else
        preview.node(node, { toggle_focus = true })
      end
    end
  end, opts 'Preview')
  vim.keymap.set('n', 'P', preview.watch, opts 'Preview (Watch)')
  vim.keymap.set('n', '<Esc>', preview.unwatch, opts 'Close Preview/Unwatch')

  vim.keymap.set('n', '<C-]>', api.tree.change_root_to_node, opts 'CD')
  vim.keymap.set('n', '<C-e>', api.node.open.replace_tree_buffer, opts 'Open: In Place')
  vim.keymap.set('n', '<C-k>', api.node.show_info_popup, opts 'Info')
  vim.keymap.set('n', '<C-r>', api.fs.rename_sub, opts 'Rename: Omit Filename')
  vim.keymap.set('n', '<C-t>', api.node.open.tab, opts 'Open: New Tab')
  vim.keymap.set('n', '<C-v>', api.node.open.vertical, opts 'Open: Vertical Split')
  vim.keymap.set('n', '<C-x>', api.node.open.horizontal, opts 'Open: Horizontal Split')
  vim.keymap.set('n', '<BS>', api.node.navigate.parent_close, opts 'Close Directory')
  vim.keymap.set('n', '<CR>', api.node.open.edit, opts 'Open')
  -- vim.keymap.set('n', '<Tab>', api.node.open.preview, opts 'Open Preview')
  vim.keymap.set('n', '>', api.node.navigate.sibling.next, opts 'Next Sibling')
  vim.keymap.set('n', '<', api.node.navigate.sibling.prev, opts 'Previous Sibling')
  vim.keymap.set('n', '.', api.node.run.cmd, opts 'Run Command')
  vim.keymap.set('n', '-', api.tree.change_root_to_parent, opts 'Up')
  vim.keymap.set('n', 'a', api.fs.create, opts 'Create File Or Directory')
  -- vim.keymap.set('n', 'bd', api.marks.bulk.delete, opts 'Delete Bookmarked')
  vim.keymap.set('n', 'bt', api.marks.bulk.trash, opts 'Trash Bookmarked')
  vim.keymap.set('n', 'bmv', api.marks.bulk.move, opts 'Move Bookmarked')
  vim.keymap.set('n', 'B', api.tree.toggle_no_buffer_filter, opts 'Toggle Filter: No Buffer')
  vim.keymap.set('n', 'c', api.fs.copy.node, opts 'Copy')
  vim.keymap.set('n', 'C', api.tree.toggle_git_clean_filter, opts 'Toggle Filter: Git Clean')
  vim.keymap.set('n', '[c', api.node.navigate.git.prev, opts 'Prev Git')
  vim.keymap.set('n', ']c', api.node.navigate.git.next, opts 'Next Git')
  vim.keymap.set('n', 'd', api.fs.remove, opts 'Delete')
  vim.keymap.set('n', 'D', api.fs.trash, opts 'Trash')
  vim.keymap.set('n', 'E', api.tree.expand_all, opts 'Expand All')
  vim.keymap.set('n', 'e', api.fs.rename_basename, opts 'Rename: Basename')
  vim.keymap.set('n', ']e', api.node.navigate.diagnostics.next, opts 'Next Diagnostic')
  vim.keymap.set('n', '[e', api.node.navigate.diagnostics.prev, opts 'Prev Diagnostic')
  vim.keymap.set('n', 'F', api.live_filter.clear, opts 'Live Filter: Clear')
  vim.keymap.set('n', 'f', api.live_filter.start, opts 'Live Filter: Start')
  vim.keymap.set('n', 'g?', api.tree.toggle_help, opts 'Help')
  vim.keymap.set('n', 'gy', api.fs.copy.absolute_path, opts 'Copy Absolute Path')
  vim.keymap.set('n', 'H', api.tree.toggle_hidden_filter, opts 'Toggle Filter: Dotfiles')
  vim.keymap.set('n', 'I', api.tree.toggle_gitignore_filter, opts 'Toggle Filter: Git Ignore')
  vim.keymap.set('n', 'J', api.node.navigate.sibling.last, opts 'Last Sibling')
  vim.keymap.set('n', 'K', api.node.navigate.sibling.first, opts 'First Sibling')
  vim.keymap.set('n', 'M', api.tree.toggle_no_bookmark_filter, opts 'Toggle Filter: No Bookmark')
  vim.keymap.set('n', 'm', api.marks.toggle, opts 'Toggle Bookmark')
  vim.keymap.set('n', 'o', api.node.open.edit, opts 'Open')
  vim.keymap.set('n', 'O', api.node.open.no_window_picker, opts 'Open: No Window Picker')
  vim.keymap.set('n', 'p', api.fs.paste, opts 'Paste')
  -- vim.keymap.set('n', 'P', api.node.navigate.parent, opts 'Parent Directory')
  vim.keymap.set('n', 'q', api.tree.close, opts 'Close')
  vim.keymap.set('n', 'r', api.fs.rename, opts 'Rename')
  vim.keymap.set('n', 'R', api.tree.reload, opts 'Refresh')
  vim.keymap.set('n', 's', api.node.run.system, opts 'Run System')
  vim.keymap.set('n', 'S', api.tree.search_node, opts 'Search')
  vim.keymap.set('n', 'u', api.fs.rename_full, opts 'Rename: Full Path')
  vim.keymap.set('n', 'U', api.tree.toggle_custom_filter, opts 'Toggle Filter: Hidden')
  vim.keymap.set('n', 'W', api.tree.collapse_all, opts 'Collapse')
  vim.keymap.set('n', 'x', api.fs.cut, opts 'Cut')
  vim.keymap.set('n', 'y', api.fs.copy.filename, opts 'Copy Name')
  vim.keymap.set('n', 'Y', api.fs.copy.relative_path, opts 'Copy Relative Path')
  vim.keymap.set('n', '<2-LeftMouse>', api.node.open.edit, opts 'Open')
  vim.keymap.set('n', '<2-RightMouse>', api.tree.change_root_to_node, opts 'CD')
  -- END_DEFAULT_ON_ATTACH
end
require('nvim-tree').setup({ on_attach = on_attach })

vim.api.nvim_set_option('guicursor', vim.o.guicursor .. ',n:block')
require'lspconfig'.pyright.setup{}
-- require("codecompanion").setup{
--   opts = {
--     log_level = "DEBUG",
--   },
-- }

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
-- :new|pu=execute('<colon command>') " copy the output of any :colon command to a new buffer
-- zz/. - center current liner horizontally on the screen (z -/+ or b/t to put current line at bottom/top)
-- LSPStop - brute force way to disable annoying inline linting messages; just use :DiagnosticToggle though
-- Telescope
--   commands
--   command_history
-- gf in new tab -- c-w gf
