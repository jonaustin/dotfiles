return {
  -- mini
  -- { 'echasnovski/mini.nvim', version = false },

  -- coding
  'fatih/vim-go',
  'vim-ruby/vim-ruby',
  'tpope/vim-rails',
  -- 'vim-vaultproject',
  -- { 'SmiteshP/nvim-navic',   dependencies = { 'neovim/nvim-lspconfig' }, lsp = { auto_attach = true, } }, -- show current code context
  -- { 'RaafatTurki/corn.nvim', opts = {} }, -- put annoying lsp linter messages in their place

  -- Git related plugins
  'tpope/vim-fugitive',
  'tpope/vim-rhubarb', -- :GBrowse

  -- Detect tabstop and shiftwidth automatically
  'tpope/vim-sleuth',

  'tpope/vim-repeat',
  'tpope/vim-surround', -- use treesitter instead?
  'mbbill/undotree',
  'junegunn/vim-easy-align', -- :EasyAlign /<regex>/ ---- note: maybe replace with mini.align ---- note: maybe replace with mini.align
  { 'xolox/vim-session', dependencies = { 'xolox/vim-misc' } },
  {
    'szw/vim-maximizer',
    init = function()
      vim.g.maximizer_set_default_mapping = 0 -- vim-maximizer; disable F3 since it stomps on nvim-dap
    end,
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
    'nvim-tree/nvim-tree.lua', -- file explorer
    version = '*',
    lazy = false,
    -- dependencies = {
    --   "nvim-tree/nvim-web-devicons",
    -- },
    config = function()
      require('nvim-tree').setup({
        on_attach = on_attach
      })
    end,
  },

  'kyazdani42/nvim-web-devicons', -- for nvim-tree
  'nvim-lualine/lualine.nvim',
  'nvim-lua/plenary.nvim', -- base lib used by other plugins
  'majutsushi/tagbar', -- side pane with list of functions,etc

  -- LSP
  {
    -- LSP Configuration & Plugins
    -- vim.lsp.set_log_level("DEBUG")
    'neovim/nvim-lspconfig',
    commit = "cb33dea",
    -- tag = 'v1.8.0',
    -- tag = 'v1.8.0',
    dependencies = {
      -- Automatically install LSPs to stdpath for neovim
      { 'williamboman/mason.nvim', config = true },
      { 'williamboman/mason-lspconfig.nvim', branch = 'v1.x' },

      -- Useful status updates for LSP
      -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
      { 'j-hui/fidget.nvim', opts = {} }, -- notifications in lower right corner
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
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-buffer',

      -- Adds a number of user-friendly snippets
      'rafamadriz/friendly-snippets',
    },
  },

  {
    'mfussenegger/nvim-lint', -- linter
    event = {
      'BufReadPre',
      'BufNewFile',
    },
    config = function()
      local lint = require 'lint'

      lint.linters_by_ft = {
        javascript = { 'eslint_d' },
        javascriptreact = { 'eslint_d' },
        kotlin = { 'ktlint' },
        -- lua = { "luacheck" },
        markdown = { 'markdownlint' },
        python = { 'ruff' },
        puppet = { 'puppet-lint' },
        -- ruby = { 'rubocop' }, -- { "rufo" }, -- needs ruby 3.0
        svelte = { 'eslint_d' },
        terraform = { 'tflint', 'tfsec' },
        typescript = { 'eslint_d' },
        zsh = { 'shellcheck' },
      }

      local lint_augroup = vim.api.nvim_create_augroup('lint', { clear = true })

      vim.api.nvim_create_autocmd({ 'BufEnter', 'BufWritePost', 'InsertLeave' }, {
        group = lint_augroup,
        callback = function()
          lint.try_lint()
        end,
      })

      vim.keymap.set('n', '<leader>cL', function()
        lint.try_lint()
      end, { desc = 'Trigger linting for current file' })
    end,
  },

  {
    'stevearc/conform.nvim', -- formatter
    keys = {
      {
        '<leader>cf',
        function()
          require('conform').format({ async = true, lsp_format = 'fallback' })
        end,
        mode = '',
        desc = 'Format buffer',
      },
    },
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      require('conform').setup({
        formatters_by_ft = {
          bash = { 'shfmt' },
          css = { { 'prettierd', 'prettier' } },
          erb = { 'htmlbeautifier' },
          graphql = { { 'prettierd', 'prettier' } },
          go = { 'gofmt', 'goimports' },
          html = { 'htmlbeautifier' },
          -- java = { 'google-java-format' },
          javascript = { { 'prettierd', 'prettier' } },
          javascriptreact = { { 'prettierd', 'prettier' } },
          json = { { 'prettierd', 'prettier' } },
          lua = { 'stylua' },
          markdown = { 'markdownlint', 'markdown-toc' },
          proto = { 'buf' },
          python = { 'black', 'isort' },
          -- ruby = { 'rufo' }, -- from the creator of Crystal
          rust = { 'rustfmt' },
          scss = { { 'prettierd', 'prettier' } },
          svelte = { { 'prettierd', 'prettier' } },
          terraform = { 'terraform_fmt' },
          hcl = { 'terragrunt_hclfmt' },
          toml = { 'taplo' },
          typescript = { { 'prettierd', 'prettier' } },
          typescriptreact = { { 'prettierd', 'prettier' } },
          yaml = { 'yamlfix' },
          ['_'] = { 'trim_whitespace' },
          -- ['*'] = { 'codespell' },
        },
        -- format_on_save = {
        --   lsp_fallback = true,
        --   timeout_ms = 500,
        -- },
      })

      vim.keymap.set({ 'n', 'v' }, '<leader>cl', function()
        require('conform').format({
          lsp_fallback = true,
          async = false,
          timeout_ms = 500,
        })
      end, { desc = 'Format file or range (in visual mode)' })
    end,
  },
  -- {
  --   'WhoIsSethDaniel/mason-tool-installer.nvim',
  --   opts = {
  --     -- a list of all tools you want to ensure are installed upon
  --     -- start
  --     ensure_installed = {
  --       -- you can pin a tool to a particular version
  --       -- { 'golangci-lint', version = 'v1.47.0' },
  --
  --       -- you can turn off/on auto_update per tool
  --       { 'bash-language-server', auto_update = true },
  --
  --       'black',
  --       'codespell',
  --       'delve',
  --       'dockerls',
  --       'editorconfig-checker',
  --       'eslint_d',
  --       'eslint',
  --       'gofumpt',
  --       'golangci-lint',
  --       'golines',
  --       'gomodifytags',
  --       'gopls',
  --       'gotests',
  --       'htmx',
  --       'html',
  --       'impl',
  --       'isort',
  --       'json-to-struct',
  --       'jsonls',
  --       'lua-language-server',
  --       -- 'luacheck',
  --       'markdown-toc',
  --       'misspell',
  --       'pyright',
  --       'revive',
  --       'rubocop',
  --       'ruby-lsp',
  --       'ruff',
  --       'rufo',
  --       'shellcheck',
  --       'shfmt',
  --       -- 'sorbet',
  --       'sqls',
  --       'staticcheck',
  --       'stylua',
  --       'tflint',
  --       'terraformls',
  --       'ts_ls',
  --       'vim-language-server',
  --       'vint',
  --     },
  --
  --     -- Disable integration with other Mason plugins. This removes
  --     -- the ability to to use the alternative names of packages provided
  --     -- by these plugins but disables them from immediately becoming loaded
  --     integrations = {
  --       ['mason-lspconfig'] = true,
  --       ['mason-nvim-dap'] = true,
  --     },
  --   },
  -- },

  {
    'folke/which-key.nvim',
    version = '2.1.0',
    config = function()
      require('whichkey').setup()
    end,
  },
  {
    'mrjones2014/legendary.nvim',
    -- since legendary.nvim handles all your keymaps/commands,
    -- its recommended to load legendary.nvim before other plugins
    priority = 10000,
    lazy = false,
    -- sqlite is only needed if you want to use frecency sorting
    dependencies = { 'kkharji/sqlite.lua' }
  },
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
          gs.stage_hunk({ vim.fn.line '.', vim.fn.line 'v' })
        end, { desc = 'stage git hunk' })
        map('v', '<leader>hr', function()
          gs.reset_hunk({ vim.fn.line '.', vim.fn.line 'v' })
        end, { desc = 'reset git hunk' })
        -- normal mode
        map('n', '<leader>hs', gs.stage_hunk, { desc = 'git stage hunk' })
        map('n', '<leader>hr', gs.reset_hunk, { desc = 'git reset hunk' })
        map('n', '<leader>hS', gs.stage_buffer, { desc = 'git Stage buffer' })
        map('n', '<leader>hu', gs.undo_stage_hunk, { desc = 'undo stage hunk' })
        map('n', '<leader>hR', gs.reset_buffer, { desc = 'git Reset buffer' })
        map('n', '<leader>hp', gs.preview_hunk, { desc = 'preview git hunk' })
        map('n', '<leader>hb', function()
          gs.blame_line({ full = false })
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
      'nvim-telescope/telescope-dap.nvim',
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
    'folke/trouble.nvim',
    dependencies = {
      'nvim-tree/nvim-web-devicons',
      'b0o/nvim-tree-preview.lua',
    },
    opts = {},
  },
  {
    'Wansmer/treesj', -- split/join
    keys = { '<space>m', '<space>j', '<space>s' },
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    config = function()
      require('treesj').setup({
        max_join_length = 500,
      })
    end,
  },
  { 'dstein64/vim-startuptime' }, -- :StartupTime
  { 'stevearc/dressing.nvim', event = 'VeryLazy' }, -- purtify

  -- <leader>z - focus mode
  'folke/twilight.nvim',
  {
    'folke/zen-mode.nvim',
    cmd = 'ZenMode',
    opts = {
      plugins = {
        gitsigns = true,
        tmux = true,
        kitty = { enabled = false, font = '+2' },
      },
    },
    keys = { { '<leader>z', '<cmd>ZenMode<cr>', desc = 'Zen Mode' } },
  },
  {
    'pwntester/octo.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope.nvim',
      'nvim-tree/nvim-web-devicons',
    },
    config = function()
      require('octo').setup({ enable_builtin = true })
      vim.cmd [[hi OctoEditable guibg=none]]
    end,
    keys = {
      { '<leader>O', '<cmd>Octo<cr>', desc = 'Octo' },
    },
  },
  {
    'stevearc/overseer.nvim',
    keys = {
      { '<leader>ttR', '<cmd>OverseerRunCmd<cr>', desc = 'Run Command' },
      { '<leader>tta', '<cmd>OverseerTaskAction<cr>', desc = 'Task Action' },
      { '<leader>ttb', '<cmd>OverseerBuild<cr>', desc = 'Build' },
      { '<leader>ttc', '<cmd>OverseerClose<cr>', desc = 'Close' },
      { '<leader>ttd', '<cmd>OverseerDeleteBundle<cr>', desc = 'Delete Bundle' },
      { '<leader>ttl', '<cmd>OverseerLoadBundle<cr>', desc = 'Load Bundle' },
      { '<leader>tto', '<cmd>OverseerOpen<cr>', desc = 'Open' },
      { '<leader>ttq', '<cmd>OverseerQuickAction<cr>', desc = 'Quick Action' },
      { '<leader>ttr', '<cmd>OverseerRun<cr>', desc = 'Run' },
      { '<leader>tts', '<cmd>OverseerSaveBundle<cr>', desc = 'Save Bundle' },
      { '<leader>ttt', '<cmd>OverseerToggle<cr>', desc = 'Toggle' },
    },
    opts = {},
    config = function()
      require('overseer').setup({
        component_aliases = {
          default = {
            'on_output_summarize',
            'on_exit_set_status',
            'on_complete_notify',
            'on_complete_dispose',
          },
        },
      })
    end,
  },
  { 'sindrets/diffview.nvim' },
  {
    'stevearc/aerial.nvim', -- tagBar like panel
    opts = {},
    -- Optional dependencies
    dependencies = {
      'nvim-treesitter/nvim-treesitter',
      'nvim-tree/nvim-web-devicons',
    },
  },
  { 'theHamsta/nvim-dap-virtual-text', opts = {} },
  { 'ThePrimeagen/refactoring.nvim', opts = {} },
  {
    'amitds1997/remote-nvim.nvim',
    version = '*', -- Pin to GitHub releases
    dependencies = {
      'nvim-lua/plenary.nvim', -- For standard functions
      'MunifTanjim/nui.nvim', -- To build the plugin UI
      'nvim-telescope/telescope.nvim', -- For picking b/w different remote methods
    },
    config = true,
  },

  -- NOTE: The import below can automatically add your own plugins, configuration, etc from `lua/custom/plugins/*.lua`
  --    You can use this folder to prevent any conflicts with this init.lua if you're interested in keeping
  --    up-to-date with whatever is in the kickstart repo.
  --    Uncomment the following line and add your plugins to `lua/custom/plugins/*.lua` to get going.
  --
  --    For additional information see: https://github.com/folke/lazy.nvim#-structuring-your-plugins
  -- { import = 'custom.plugins' },
}
