return {
  -- AI
  -- 'github/copilot.vim',
  {
    "zbirenbaum/copilot.lua",
    opts = {
      suggestion = {
        enabled = true,
        auto_trigger = false,
        hide_during_completion = true,
        debounce = 75,
        keymap = {
          -- insert mode only!
          accept = "<M-l>",
          next = "<M-]>",
          prev = "<M-[>",
          dismiss = "<C-]>",
        },
      },
      filetypes = { ["*"] = true, markdown = false },
    },
  },
  {
    "zbirenbaum/copilot-cmp", -- enables copilot as a normal completion option with copilot-cmp
    dependencies = { 'zbirenbaum/copilot.lua' },
    config = function()
      require("copilot_cmp").setup()
    end
  },
  -- {
  --   'bakks/butterfish.nvim',
  --   dependencies = { 'tpope/vim-commentary' },
  --   config = function()
  --     require('butterfish')
  --   end
  -- },
  { 'Bryley/neoai.nvim', dependencies = { "MunifTanjim/nui.nvim", } },
  {
    -- Best ai plugin i've found so far
    -- see for inspirado: https://github.com/jellydn/lazy-nvim-ide/blob/main/lua/plugins/extras/copilot-chat-v2.lua
    "CopilotC-Nvim/CopilotChat.nvim",
    config = function()
      local opts = {
        show_help = "yes",         -- Show help text for CopilotChatInPlace, default: yes
        debug = true,              -- Enable or disable debug mode, the log file will be in ~/.local/state/nvim/CopilotChat.nvim.log
        disable_extra_info = 'no', -- Disable extra information (e.g: system prompt) in the response.
        language = "English",      -- Copilot answer language settings when using default prompts. Default language is English.
        mode = "split",            -- newbuffer or split  , default: newbuffer
        model = 'gpt-4o', --'claude-3.5-sonnet'; need to enable in github org
        mappings = {
          reset = { 
            normal = "<C-x>",
            insert = "<C-x>",
          },
        },
        -- proxy = "socks5://127.0.0.1:3000", -- Proxies requests via https or socks.
        temperature = 0.1,
        chat_autocomplete=true -- nvim-cmp/blink/etc integration
      }

      -- disable default <tab> complete mapping for copilot chat when doing this
      require('CopilotChat').setup(opts)
      ---
    end,
    branch = 'main',
    build = function()
      vim.defer_fn(function()
        vim.cmd("UpdateRemotePlugins")
        vim.notify("CopilotChat - Updated remote plugins. Please restart Neovim.")
      end, 3000)
    end,
    event = "VeryLazy",
    keys = {
      { "<leader>ccb", "<cmd>lua require('CopilotChat').ask(input, { selection = require('CopilotChat.select').buffer})<cr>", desc = "Chat with buffer" },
      { "<leader>cce", "<cmd>CopilotChatExplain<cr>", desc = "CopilotChat - Explain code" },
      { "<leader>cct", "<cmd>CopilotChatTests<cr>",   desc = "CopilotChat - Generate tests" },
      { "<leader>ccT", "<cmd>CopilotChatToggle<cr>", desc = "CopilotChatToggle" },
      { "<leader>ccf", "<cmd>CopilotChatFixDiagnostic<cr>", desc = "CopilotChat - Fix diagnostic" }, -- Get a fix for the diagnostic message under the cursor.
      { "<leader>ccr", "<cmd>CopilotChatReset<cr>", desc = "CopilotChat - Reset chat history and clear buffer" }, -- Reset chat history and clear buffer.
      { "<leader>cch",
        function()
          local actions = require 'CopilotChat.actions'
          require('CopilotChat.integrations.telescope').pick(actions.help_actions())
        end,
        desc = "CopilotChat - Help actions",
      },
      -- Show prompts actions with telescope
      {
        "<leader>ccp",
        function()
          local actions = require 'CopilotChat.actions'
          require('CopilotChat.integrations.telescope').pick(actions.prompt_actions())
        end,
        desc = "CopilotChat - Prompt actions",
      },
    },
  },
  {
    "yetone/avante.nvim",
    event = "VeryLazy",
    lazy = false,
    version = false, -- set this if you want to always pull the latest change
    opts = {
      ---@alias Provider "claude" | "openai" | "azure" | "gemini" | "cohere" | "copilot" | string
      provider = "claude", -- note: change this to copilot and just use claude in copilot
      auto_suggestions_provider = "claude", -- Since auto-suggestions are a high-frequency operation and therefore expensive, it is recommended to specify an inexpensive provider or even a free provider: copilot
      claude = {
        endpoint = "https://api.anthropic.com",
        model = "claude-3-5-sonnet-20241022",
        temperature = 0,
        max_tokens = 4096,
      },
    },
    build = "make",
    dependencies = {
      "stevearc/dressing.nvim",
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      --- The below dependencies are optional,
      "hrsh7th/nvim-cmp", -- autocompletion for avante commands and mentions
      "nvim-tree/nvim-web-devicons", -- or echasnovski/mini.icons
      "zbirenbaum/copilot.lua", -- for providers='copilot'
      {
        -- support for image pasting
        "HakonHarnes/img-clip.nvim",
        event = "VeryLazy",
        opts = {
          -- recommended settings
          default = {
            embed_image_as_base64 = false,
            prompt_for_file_name = false,
            drag_and_drop = {
              insert_mode = true,
            },
          },
        },
      },
      {
        -- Make sure to set this up properly if you have lazy=true
        'MeanderingProgrammer/render-markdown.nvim',
        opts = {
          file_types = { "markdown", "Avante" },
        },
        ft = { "markdown", "Avante" },
      },
    },
  },
  {
    -- i want to like this, but it's practically unusable (i.e. try asking it to delete the neoai plugin line from this file, jeez.
    -- "olimorris/codecompanion.nvim",
    -- dependencies = {
    --   "nvim-lua/plenary.nvim",
    --   "nvim-treesitter/nvim-treesitter",
    --   "hrsh7th/nvim-cmp", -- Optional: For using slash commands and variables in the chat buffer
    --   "nvim-telescope/telescope.nvim", -- Optional: For using slash commands
    --   { "MeanderingProgrammer/render-markdown.nvim", ft = { "markdown", "codecompanion" } }, -- Optional: For prettier markdown rendering
    --   { "stevearc/dressing.nvim", opts = {} }, -- Optional: Improves `vim.ui.select`
    -- },
    -- config = function()
    --   require("codecompanion").setup({
    --     strategies = {
    --       chat = {
    --         adapter = "copilot", --"anthropic",
    --       },
    --       inline = {
    --         adapter = "copilot",
    --       },
    --     },
    --   })
    -- end,
    -- keys = {
    --   { "<leader>cca", ":CodeCompanionActions<CR>", desc = "CodeCompanion - Actions" },
    --   { "<leader>ccc", ":CodeCompanionChat<CR>", desc = "CodeCompanion - Chat" },
    -- },
  }
}
