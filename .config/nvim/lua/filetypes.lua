local function set_filetype(pattern, filetype)
	vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
		pattern = pattern,
		command = "set filetype=" .. filetype,
	})
end

-- Set filetype for various file patterns
set_filetype({ "*.tf", "*.tfvars" }, "terraform")
set_filetype({ "*.hcl", ".terraformrc", "terraform.rc" }, "hcl")
set_filetype({ "*.tfstate", "*.tfstate.backup" }, "json")
set_filetype({ "docker-compose.yml" }, "yaml.docker-compose")

-- Fix commentstring for terraform
-- instead perhaps use: https://github.com/JoosepAlviste/nvim-ts-context-commentstring
vim.api.nvim_create_autocmd("FileType", {
  pattern = "terraform",
  callback = function()
    vim.bo.commentstring = "# %s"
  end
})
