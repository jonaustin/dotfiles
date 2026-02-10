return {
  -- GitHub Copilot
  -- {
  --   "zbirenbaum/copilot.lua",
  --   opts = {
  --     suggestion = {
  --       enabled = true,
  --       auto_trigger = false,
  --       hide_during_completion = true,
  --       debounce = 75,
  --       keymap = {
  --         accept = "<M-l>",
  --         next = "<M-]>",
  --         prev = "<M-[>",
  --         dismiss = "<C-]>",
  --       },
  --     },
  --     filetypes = { ["*"] = true, markdown = false },
  --   },
  -- },
  -- {
  --   "giuxtaposition/blink-cmp-copilot",
  --   dependencies = { "zbirenbaum/copilot.lua" },
  --   specs = {
  --     {
  --       "saghen/blink.cmp",
  --       optional = true,
  --       opts = {
  --         sources = {
  --           default = { "copilot" },
  --           providers = {
  --             copilot = {
  --               name = "copilot",
  --               module = "blink-cmp-copilot",
  --               score_offset = 100,
  --               async = true,
  --             },
  --           },
  --         },
  --       },
  --     },
  --   },
  -- },
  --
  -- ClaudeCode - custom keymaps extending LazyVim extra
  {
    "coder/claudecode.nvim",
    keys = {
      { "<leader>ax", "", desc = "+ClaudeCode", mode = { "n", "v" } },
      { "<leader>axx", "<cmd>ClaudeCode<cr>", desc = "Toggle Terminal" },
      { "<leader>axf", "<cmd>ClaudeCodeFocus<cr>", desc = "Focus Terminal" },
      { "<leader>axm", "<cmd>ClaudeCodeSelectModel<cr>", desc = "Select Model" },
      { "<leader>axs", "<cmd>ClaudeCodeSend<cr>", desc = "Send Selection", mode = "v" },
      { "<leader>axa", "<cmd>ClaudeCodeAdd %<cr>", desc = "Add Current File" },
      { "<leader>axt", "<cmd>ClaudeCodeTreeAdd<cr>", desc = "Add from Tree" },
      { "<leader>axd", "<cmd>ClaudeCodeDiffAccept<cr>", desc = "Accept Diff" },
      { "<leader>axD", "<cmd>ClaudeCodeDiffDeny<cr>", desc = "Deny Diff" },
    },
  },

  -- CopilotChat - custom configuration extending LazyVim extra
  {
    "CopilotC-Nvim/CopilotChat.nvim",
    keys = {
      { "<leader>az", "", desc = "+CopilotChat", mode = { "n", "v" } },
      { "<leader>azz", "<cmd>CopilotChatToggle<cr>", desc = "Toggle Chat" },
      { "<leader>azq", "<cmd>CopilotChatClose<cr><cmd>CopilotChatReset<cr>", desc = "Reset & Close" },
      {
        "<leader>azc",
        function()
          local input = vim.fn.input("Quick Chat: ")
          if input ~= "" then
            require("CopilotChat").ask(input)
          end
        end,
        desc = "Quick Chat",
        mode = { "n", "v" },
      },
      { "<leader>aze", "<cmd>CopilotChatExplain<cr>", desc = "Explain", mode = { "n", "v" } },
      { "<leader>azr", "<cmd>CopilotChatReview<cr>", desc = "Review", mode = { "n", "v" } },
      { "<leader>azf", "<cmd>CopilotChatFix<cr>", desc = "Fix", mode = { "n", "v" } },
      { "<leader>azo", "<cmd>CopilotChatOptimize<cr>", desc = "Optimize", mode = { "n", "v" } },
      { "<leader>azd", "<cmd>CopilotChatDocs<cr>", desc = "Generate Docs", mode = { "n", "v" } },
      { "<leader>azt", "<cmd>CopilotChatTests<cr>", desc = "Generate Tests", mode = { "n", "v" } },
      { "<leader>azm", "<cmd>CopilotChatCommit<cr>", desc = "Commit Message" },
      { "<leader>azs", "<cmd>CopilotChatModels<cr>", desc = "Select Model" },
    },
    opts = function(_, opts)
      -- Add LM Studio provider
      local config = require("CopilotChat.config")

      -- Cache for LM Studio models (fetched async)
      local lmstudio_models = {}

      -- Fetch models async (deferred to escape fast event context)
      vim.schedule(function()
        vim.system({ "curl", "-s", "http://localhost:1234/v1/models" }, {}, function(result)
          if result.code ~= 0 then
            return
          end
          local ok, data = pcall(vim.json.decode, result.stdout)
          if ok and data and data.data then
            for _, model in ipairs(data.data) do
              table.insert(lmstudio_models, { id = model.id, name = model.id })
            end
          end
        end)
      end)

      config.providers.lmstudio = {
        prepare_input = config.providers.copilot.prepare_input,
        prepare_output = config.providers.copilot.prepare_output,
        get_models = function()
          return lmstudio_models
        end,
        get_url = function()
          return "http://localhost:1234/v1/chat/completions"
        end,
      }

      -- Apply custom options
      return vim.tbl_deep_extend("force", opts, {
        show_help = "yes",
        debug = false,
        disable_extra_info = "no",
        language = "English",
        mode = "split",
        model = "claude-sonnet-4.5",
        mappings = {
          reset = {
            normal = "<C-x>",
            insert = "<C-x>",
          },
        },
        temperature = 0.1,
        chat_autocomplete = true,
      })
    end,
  },

  -- Avante.nvim (disabled - using CopilotChat instead)
  -- {
  --   "yetone/avante.nvim",
  --   event = "VeryLazy",
  --   version = false,
  --   opts = {
  --     debug = false,
  --     provider = "lmstudioQwen",
  --     cursor_applying_provider = "lmstudioQwen",
  --     behaviour = {
  --       enable_cursor_planning_mode = true,
  --       auto_suggestions = false,
  --       auto_suggestions_debounce = 500,
  --       auto_set_highlight_group = true,
  --       auto_set_keymaps = true,
  --       auto_apply_diff_after_generation = false,
  --       support_paste_from_clipboard = false,
  --       minimize_diff = true,
  --     },
  --     providers = {
  --       lmstudioQwen = {
  --         __inherited_from = "openai",
  --         api_key_name = "",
  --         endpoint = "http://127.0.0.1:1234/v1",
  --         model = "qwen2.5-coder-32b-instruct-mlx@4bit",
  --       },
  --       lmstudioLlama33 = {
  --         __inherited_from = "openai",
  --         api_key_name = "",
  --         endpoint = "http://127.0.0.1:1234/v1",
  --         model = "llama-3.3-70b-instruct",
  --       },
  --       claude = {
  --         endpoint = "https://api.anthropic.com",
  --         model = "claude-3-5-sonnet-20241022",
  --         extra_request_body = {
  --           temperature = 0.1,
  --           max_tokens = 4096,
  --         },
  --       },
  --       copilot = {
  --         endpoint = "https://api.githubcopilot.com/",
  --         model = "claude-sonnet-4-5",
  --         proxy = nil,
  --         allow_insecure = false,
  --         timeout = 30000,
  --         extra_request_body = {
  --           temperature = 0.1,
  --           max_tokens = 8192,
  --         },
  --       },
  --     },
  --     windows = {
  --       position = "left",
  --       width = 45,
  --     },
  --   },
  --   keys = {
  --     { "<leader>cca", "<cmd>AvanteToggle<cr>", desc = "Avante - Toggle window" },
  --   },
  --   build = "make",
  --   dependencies = {
  --     "stevearc/dressing.nvim",
  --     "nvim-lua/plenary.nvim",
  --     "MunifTanjim/nui.nvim",
  --     "nvim-tree/nvim-web-devicons",
  --     "zbirenbaum/copilot.lua",
  --     {
  --       "HakonHarnes/img-clip.nvim",
  --       event = "VeryLazy",
  --       opts = {
  --         default = {
  --           embed_image_as_base64 = false,
  --           prompt_for_file_name = false,
  --           drag_and_drop = {
  --             insert_mode = true,
  --           },
  --         },
  --       },
  --     },
  --     {
  --       "MeanderingProgrammer/render-markdown.nvim",
  --       opts = {
  --         file_types = { "markdown", "Avante" },
  --       },
  --       ft = { "markdown", "Avante" },
  --     },
  --   },
  -- },

  -- CodeCompanion (disabled - using CopilotChat instead)
  -- {
  --   "olimorris/codecompanion.nvim",
  --   dependencies = {
  --     "nvim-lua/plenary.nvim",
  --     "nvim-treesitter/nvim-treesitter",
  --     "nvim-telescope/telescope.nvim",
  --     { "MeanderingProgrammer/render-markdown.nvim", ft = { "markdown", "codecompanion" } },
  --     { "stevearc/dressing.nvim", opts = {} },
  --   },
  --   config = function()
  --     require("codecompanion").setup({
  --       strategies = {
  --         chat = {
  --           adapter = "copilot",
  --         },
  --         inline = {
  --           adapter = "copilot",
  --         },
  --       },
  --     })
  --   end,
  --   keys = {
  --     { "<leader>ccc", ":CodeCompanionChat<CR>", desc = "CodeCompanion - Chat" },
  --   },
  -- },

  -- WTF.nvim - AI debugging assistant
  {
    "piersolenski/wtf.nvim",
    dependencies = {
      "MunifTanjim/nui.nvim",
    },
    opts = {},
    keys = {
      {
        "<leader>wa",
        mode = { "n", "x" },
        function()
          require("wtf").ai()
        end,
        desc = "Debug diagnostic with AI",
      },
      {
        mode = { "n" },
        "<leader>ws",
        function()
          require("wtf").search()
        end,
        desc = "Search diagnostic with Google",
      },
      {
        mode = { "n" },
        "<leader>wh",
        function()
          require("wtf").history()
        end,
        desc = "Populate quickfix with chat history",
      },
      {
        mode = { "n" },
        "<leader>wg",
        function()
          require("wtf").grep_history()
        end,
        desc = "Grep chat history with Telescope",
      },
    },
  },

  -- NeoAI (disabled - using CopilotChat instead)
  -- {
  --   "Bryley/neoai.nvim",
  --   dependencies = { "MunifTanjim/nui.nvim" },
  --   config = function()
  --     require("neoai").setup({
  --       ui = {
  --         output_popup_text = "NeoAI",
  --         input_popup_text = "Prompt",
  --         width = 30,
  --         output_popup_height = 80,
  --         submit = "<Enter>",
  --       },
  --       models = {
  --         {
  --           name = "openai",
  --           model = "gpt-3.5-turbo",
  --           params = nil,
  --         },
  --       },
  --       register_output = {
  --         ["g"] = function(output)
  --           return output
  --         end,
  --         ["c"] = require("neoai.utils").extract_code_snippets,
  --       },
  --       inject = {
  --         cutoff_width = 75,
  --       },
  --       prompts = {
  --         context_prompt = function(context)
  --           return "Hey, I'd like to provide some context for future "
  --             .. "messages. Here is the code/text that I want to refer "
  --             .. "to in our upcoming conversations:\n\n"
  --             .. context
  --         end,
  --       },
  --       mappings = {
  --         ["select_up"] = "<C-k>",
  --         ["select_down"] = "<C-j>",
  --       },
  --       open_ai = {
  --         api_key = {
  --           env = "OPENAI_API_KEY",
  --           value = nil,
  --         },
  --       },
  --       shortcuts = {
  --         {
  --           name = "textify",
  --           key = "<leader>as",
  --           desc = "fix text with AI",
  --           use_context = true,
  --           prompt = [[
  --             Please rewrite the text to make it more readable, clear,
  --             concise, and fix any grammatical, punctuation, or spelling
  --             errors
  --           ]],
  --           modes = { "v" },
  --           strip_function = nil,
  --         },
  --         {
  --           name = "gitcommit",
  --           key = "<leader>ag",
  --           desc = "generate git commit message",
  --           use_context = false,
  --           prompt = function()
  --             return [[
  --               Using the following git diff generate a concise and
  --               clear git commit message, with a short title summary
  --               that is 75 characters or less:
  --             ]] .. vim.fn.system("git diff --cached")
  --           end,
  --           modes = { "n" },
  --           strip_function = nil,
  --         },
  --       },
  --     })
  --   end,
  -- },
}
