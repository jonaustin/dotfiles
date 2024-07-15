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
  {
    'bakks/butterfish.nvim',
    dependencies = { 'tpope/vim-commentary' },
    config = function()
      require('butterfish')
    end
  },
  { 'Bryley/neoai.nvim', dependencies = { "MunifTanjim/nui.nvim", } },
  {
    -- see for inspirado: https://github.com/jellydn/lazy-nvim-ide/blob/main/lua/plugins/extras/copilot-chat-v2.lua
    "CopilotC-Nvim/CopilotChat.nvim",
    opts = {
      show_help = "yes",         -- Show help text for CopilotChatInPlace, default: yes
      debug = true,              -- Enable or disable debug mode, the log file will be in ~/.local/state/nvim/CopilotChat.nvim.log
      disable_extra_info = 'no', -- Disable extra information (e.g: system prompt) in the response.
      language = "English",      -- Copilot answer language settings when using default prompts. Default language is English.
      mode = "split",            -- newbuffer or split  , default: newbuffer
      model = 'gpt-4',
      mappings = {
        reset = { 
          normal = "<C-x>",
          insert = "<C-x>",
        },
      },

      -- proxy = "socks5://127.0.0.1:3000", -- Proxies requests via https or socks.
      -- temperature = 0.1,
    },
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
      { "<leader>ccr", "<cmd>CopilotChatReset<cr>", desc = "CopilotChat - Reset chat history and clear buffer" } -- Reset chat history and clear buffer.
    },
  },
  {
    "dpayne/CodeGPT.nvim",
    dependencies = {
      'nvim-lua/plenary.nvim',
      'MunifTanjim/nui.nvim',
    },
    config = function()
      require("codegpt.config")
    end
  }
}
