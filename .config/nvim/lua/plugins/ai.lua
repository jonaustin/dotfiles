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
    -- see for inspirado: https://github.com/jellydn/lazy-nvim-ide/blob/main/lua/plugins/extras/copilot-chat-v2.lua
    "CopilotC-Nvim/CopilotChat.nvim",
    config = function()
      local opts = {
        show_help = "yes",         -- Show help text for CopilotChatInPlace, default: yes
        debug = true,              -- Enable or disable debug mode, the log file will be in ~/.local/state/nvim/CopilotChat.nvim.log
        disable_extra_info = 'no', -- Disable extra information (e.g: system prompt) in the response.
        language = "English",      -- Copilot answer language settings when using default prompts. Default language is English.
        mode = "split",            -- newbuffer or split  , default: newbuffer
        model = 'gpt-4o',
        mappings = {
          reset = { 
            normal = "<C-x>",
            insert = "<C-x>",
          },
        },
        -- proxy = "socks5://127.0.0.1:3000", -- Proxies requests via https or socks.
        temperature = 0.1,
      }

      --- nvim-cmp integration
      require("CopilotChat.integrations.cmp").setup()
      -- disable default <tab> complete mapping for copilot chat when doing this
      require('CopilotChat').setup(opts)
      ---
    end,
    branch = 'canary',
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
    -- note: i've got copilot so not sure how useful this is
    --:Chat with text selection will trigger the completion command, ChatGPT will try to complete the selected code snippet. 
    --:Chat some instructions with text selection and command args will invoke the code_edit command. This will treat the command args as instructions on what to do with the code snippet. e.g. :Chat refactor to use iteration
    "dpayne/CodeGPT.nvim",
    dependencies = {
      'nvim-lua/plenary.nvim',
      'MunifTanjim/nui.nvim',
    },
    config = function()
      opts = {
        model = "gpt-4o-mini",
        max_tokens = 4096,
        temperature = 0.6,
        number_of_choices = 1,
        system_message_template = "",
        user_message_template = "",
        callback_type = "replace_lines",
      }
    end
  }
}
