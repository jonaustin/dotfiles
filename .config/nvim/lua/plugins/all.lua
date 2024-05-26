return {
  -- coding
  'fatih/vim-go',
  'vim-ruby/vim-ruby',
  'tpope/vim-rails',
  'vim-vaultproject',
  { 'SmiteshP/nvim-navic',   dependencies = { 'neovim/nvim-lspconfig' }, lsp = { auto_attach = true, } }, -- show current code context
  { 'RaafatTurki/corn.nvim', opts = {} },                                                                 -- put annoying lsp linter messages in their place

  -- Git related plugins
  'tpope/vim-fugitive',
  'tpope/vim-rhubarb', -- :GBrowse

  -- Detect tabstop and shiftwidth automatically
  'tpope/vim-sleuth',

  'tpope/vim-repeat',
  'tpope/vim-surround',      -- use treesitter instead?
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
    "nvim-tree/nvim-tree.lua",      -- file explorer
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
  'nvim-lua/plenary.nvim',        -- base lib used by other plugins
  'majutsushi/tagbar',            -- side pane with list of functions,etc
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
      show_help = "yes",         -- Show help text for CopilotChatInPlace, default: yes
      debug = false,             -- Enable or disable debug mode, the log file will be in ~/.local/state/nvim/CopilotChat.nvim.log
      disable_extra_info = 'no', -- Disable extra information (e.g: system prompt) in the response.
      language = "English",      -- Copilot answer language settings when using default prompts. Default language is English.
      mode = "split",            -- newbuffer or split  , default: newbuffer
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

      'folke/neodev.nvim',                      -- nvim lua development stuff
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


  {
    'folke/which-key.nvim',
    config = function()
      require("whichkey").setup()
    end,
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

  { 'kevinhwang91/nvim-ufo',   dependencies = { 'kevinhwang91/promise-async' } },

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
    "folke/trouble.nvim",
    dependencies = {
      'nvim-tree/nvim-web-devicons',
      'b0o/nvim-tree-preview.lua',
    },
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
  { 'dstein64/vim-startuptime' },                    -- :StartupTime
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
  {
    'pwntester/octo.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope.nvim',
      'nvim-tree/nvim-web-devicons',
    },
    config = function()
      require("octo").setup({ enable_builtin = true })
      vim.cmd([[hi OctoEditable guibg=none]])
    end,
    keys = {
      { "<leader>O", "<cmd>Octo<cr>", desc = "Octo" },
    }
  },
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-neotest/nvim-nio",
      "nvim-lua/plenary.nvim",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-treesitter/nvim-treesitter",
      'stevearc/overseer.nvim',
      -- adapters
      "nvim-neotest/neotest-go",
      "nvim-neotest/neotest-python",
      'olimorris/neotest-rspec',
    },
    config = function()
      -- get neotest namespace (api call creates or returns namespace)
      local neotest_ns = vim.api.nvim_create_namespace("neotest")
      vim.diagnostic.config({
        virtual_text = {
          format = function(diagnostic)
            local message =
                diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
            return message
          end,
        },
      }, neotest_ns)
      require("neotest").setup({
        adapters = {
          require("neotest-go"),
          require("neotest-rspec"),
          require("neotest-python"),
        },
        consumers = {
          overseer = require("neotest.consumers.overseer"),
        },
        overseer = {
          enabled = true,
          -- don't run with overseer by default because it just break egh
          force_default = false, -- to run tests with overseer use neotest.overseer.run({}) -- this still doesn't populate neotest summary correct egh.
        },
      })
    end,
  },
  {
    "stevearc/overseer.nvim",
    keys = {
      { "<leader>ttR", "<cmd>OverseerRunCmd<cr>",       desc = "Run Command" },
      { "<leader>tta", "<cmd>OverseerTaskAction<cr>",   desc = "Task Action" },
      { "<leader>ttb", "<cmd>OverseerBuild<cr>",        desc = "Build" },
      { "<leader>ttc", "<cmd>OverseerClose<cr>",        desc = "Close" },
      { "<leader>ttd", "<cmd>OverseerDeleteBundle<cr>", desc = "Delete Bundle" },
      { "<leader>ttl", "<cmd>OverseerLoadBundle<cr>",   desc = "Load Bundle" },
      { "<leader>tto", "<cmd>OverseerOpen<cr>",         desc = "Open" },
      { "<leader>ttq", "<cmd>OverseerQuickAction<cr>",  desc = "Quick Action" },
      { "<leader>ttr", "<cmd>OverseerRun<cr>",          desc = "Run" },
      { "<leader>tts", "<cmd>OverseerSaveBundle<cr>",   desc = "Save Bundle" },
      { "<leader>ttt", "<cmd>OverseerToggle<cr>",       desc = "Toggle" },
    },
    opts = {},
    config = function()
      require('overseer').setup({
        component_aliases = {
          default = {
            "on_output_summarize",
            "on_exit_set_status",
            "on_complete_notify",
            "on_complete_dispose",
          },
        }
      })
    end,
  },
  { 'sindrets/diffview.nvim' },
  {
    'stevearc/aerial.nvim',
    opts = {},
    -- Optional dependencies
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons"
    },
  }

  -- NOTE: The import below can automatically add your own plugins, configuration, etc from `lua/custom/plugins/*.lua`
  --    You can use this folder to prevent any conflicts with this init.lua if you're interested in keeping
  --    up-to-date with whatever is in the kickstart repo.
  --    Uncomment the following line and add your plugins to `lua/custom/plugins/*.lua` to get going.
  --
  --    For additional information see: https://github.com/folke/lazy.nvim#-structuring-your-plugins
  -- { import = 'custom.plugins' },
}
