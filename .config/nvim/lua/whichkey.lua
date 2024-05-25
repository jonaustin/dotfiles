local M = {}

function M.setup()
	local whichkey = require "which-key"

	local conf = {
		window = {
			border = "single", -- none, single, double, shadow
			position = "bottom", -- bottom, top
		},
	}

	local opts = {
		mode = "n",   -- Normal mode
		prefix = "<leader>",
		buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
		silent = true, -- use `silent` when creating keymaps
		noremap = true, -- use `noremap` when creating keymaps
		nowait = false, -- use `nowait` when creating keymaps
	}

	local mappings = {
		-- ["w"] = { "<cmd>update!<CR>", "Save" },
		["q"] = { "<cmd>q!<CR>", "Quit" },
		['c'] = { name = '[C]ode', _ = 'which_key_ignore' },
		['d'] = { name = '[D]ocument', _ = 'which_key_ignore' },
		['g'] = { name = '[G]it', _ = 'which_key_ignore' },
		['h'] = { name = 'Git [H]unk', _ = 'which_key_ignore' },
		['r'] = { name = '[R]ename', _ = 'which_key_ignore' },
		['s'] = { name = '[S]earch', _ = 'which_key_ignore' },
		-- ['t'] = { name = '[T]oggle', _ = 'which_key_ignore' },
		-- ['w'] = { name = '[W]orkspace', _ = 'which_key_ignore' },

		-- b = {
		-- 	name = "Buffer",
		-- 	c = { "<Cmd>bd!<Cr>", "Close current buffer" },
		-- 	D = { "<Cmd>%bd|e#|bd#<Cr>", "Delete all buffers" },
		-- },

		n = {
			name = "Neotest",
			a = { "<cmd>lua require('neotest').run.attach()<cr>", "Attach" },
			f = { "<cmd>lua require('neotest').run.run(vim.fn.expand('%'))<cr>", "Run File" },
			F = { "<cmd>lua require('neotest').run.run({vim.fn.expand('%'), strategy = 'dap'})<cr>", "Debug File" },
			l = { "<cmd>lua require('neotest').run.run_last()<cr>", "Run Last" },
			L = { "<cmd>lua require('neotest').run.run_last({ strategy = 'dap' })<cr>", "Debug Last" },
			n = { "<cmd>lua require('neotest').run.run()<cr>", "Run Nearest" },
			N = { "<cmd>lua require('neotest').run.run({strategy = 'dap'})<cr>", "Debug Nearest" },
			o = { "<cmd>lua require('neotest').output.open({ enter = true })<cr>", "Output" },
			S = { "<cmd>lua require('neotest').run.stop()<cr>", "Stop" },
			s = { "<cmd>lua require('neotest').summary.toggle()<cr>", "Summary" },
			d = {
				name = "DAP",
				u = { "<cmd>lua require('dapui').toggle()<CR>", "DAP UI" },
				c = { "<cmd>lua require('dap').continue()<cr>", 'Debug: Start/Continue' },
				i = { "<cmd>lua require('dap').step_into()<cr>", 'Debug: Step Into' },
				o = { "<cmd>lua require('dap').step_over()<cr>", 'Debug: Step Over' },
				b = { "<cmd>lua require('dap').toggle_breakpoint()<cr>", 'Debug: Toggle Breakpoint' },
			},
		},

		o = {
			name = "Overseer",
			C = { "<cmd>OverseerClose<cr>", "OverseerClose" },
			a = { "<cmd>OverseerTaskAction<cr>", "OverseerTaskAction" },
			b = { "<cmd>OverseerBuild<cr>", "OverseerBuild" },
			c = { "<cmd>OverseerRunCmd<cr>", "OverseerRunCmd" },
			d = { "<cmd>OverseerDeleteBundle<cr>", "OverseerDeleteBundle" },
			l = { "<cmd>OverseerLoadBundle<cr>", "OverseerLoadBundle" },
			o = { "<cmd>OverseerOpen!<cr>", "OverseerOpen" },
			q = { "<cmd>OverseerQuickAction<cr>", "OverseerQuickAction" },
			r = { "<cmd>OverseerRun<cr>", "OverseerRun" },
			s = { "<cmd>OverseerSaveBundle<cr>", "OverseerSaveBundle" },
			t = { "<cmd>OverseerToggle!<cr>", "OverseerToggle" },
			n = { "<cmd>lua require('neotest').overseer.run({})<cr>", "neotest" },
		},
	}

	whichkey.setup(conf)
	whichkey.register(mappings, opts)
end

return M
