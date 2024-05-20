local wezterm = require 'wezterm';
local act = wezterm.action

-- Equivalent to POSIX basename(3)
local function basename(s)
  return string.gsub(s, "(.*[/\\])(.*)", "%2")
end

local SOLID_LEFT_ARROW = utf8.char(0xe0ba)
local SOLID_LEFT_MOST = utf8.char(0x2588)
local SOLID_RIGHT_ARROW = utf8.char(0xe0bc)

local SH_ICON = ""
local TMUX_ICON = ""
local VIM_ICON = utf8.char(0xe62b)
local PAGER_ICON = utf8.char(0xf718)
local FUZZY_ICON = utf8.char(0xf0b0)
local PYTHON_ICON = utf8.char(0xf820)
local NODE_ICON = utf8.char(0xe74e)
local HOURGLASS_ICON = utf8.char(0xf252)

local SUP_IDX = { "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", "¹⁰",
  "¹¹", "¹²", "¹³", "¹⁴", "¹⁵", "¹⁶", "¹⁷", "¹⁸", "¹⁹", "²⁰" }
local SUB_IDX = { "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉", "₁₀",
  "₁₁", "₁₂", "₁₃", "₁₄", "₁₅", "₁₆", "₁₇", "₁₈", "₁₉", "₂₀" }

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
  local edge_background = "#121212"
  local background = "#4E4E4E"
  local foreground = "#1C1B19"
  local dim_foreground = "#3A3A3A"

  if tab.is_active then
    background = "#0c1259"
    foreground = "#1C1B19"
  elseif hover then
    background = "#FF8700"
    foreground = "#1C1B19"
  end

  local edge_foreground = background
  local process_name = tab.active_pane.foreground_process_name
  local pane_title = tab.active_pane.title
  local exec_name = basename(process_name)
  local title_with_icon

  if exec_name == "nvim" then
    title_with_icon = VIM_ICON --.. pane_title:gsub("^(%S+)%s+(%d+/%d+) %- nvim", " %2 %1")
  elseif exec_name == "bat" or exec_name == "less" or exec_name == "moar" then
    title_with_icon = PAGER_ICON .. " " .. exec_name:upper()
  elseif exec_name == "fzf" then
    title_with_icon = FUZZY_ICON .. " " .. exec_name:upper()
  elseif exec_name == "python" or exec_name == "hiss" then
    title_with_icon = PYTHON_ICON .. " " .. exec_name
  elseif exec_name == "node" then
    title_with_icon = NODE_ICON .. " " .. exec_name:upper()
  elseif exec_name == "zsh" then
    title_with_icon = SH_ICON   -- .. " " .. exec_name
  elseif exec_name == "tmux" then
    title_with_icon = TMUX_ICON -- .. " " .. exec_name
  else
    title_with_icon = HOURGLASS_ICON .. " " .. exec_name
  end
  local left_arrow = SOLID_LEFT_ARROW
  if tab.tab_index == 0 then
    left_arrow = SOLID_LEFT_MOST
  end
  local id = SUB_IDX[tab.tab_index + 1]
  local pid = SUP_IDX[tab.active_pane.pane_index + 1]
  local title = " " .. wezterm.truncate_right(title_with_icon, max_width - 6) .. " "

  return {
    { Attribute = { Intensity = "Bold" } },
    { Background = { Color = edge_background } },
    { Foreground = { Color = edge_foreground } },
    { Text = left_arrow },
    { Background = { Color = background } },
    { Foreground = { Color = foreground } },
    { Text = id },
    { Text = title },
    { Foreground = { Color = dim_foreground } },
    { Text = pid },
    { Background = { Color = edge_background } },
    { Foreground = { Color = edge_foreground } },
    { Text = SOLID_RIGHT_ARROW },
    { Attribute = { Intensity = "Normal" } },
  }
end)

-- for neovim zen mode--
-- https://github.com/folke/zen-mode.nvim?tab=readme-ov-file#wezterm
wezterm.on('user-var-changed', function(window, pane, name, value)
  local overrides = window:get_config_overrides() or {}
  if name == "ZEN_MODE" then
    local incremental = value:find("+")
    local number_value = tonumber(value)
    if incremental ~= nil then
      while (number_value > 0) do
        window:perform_action(wezterm.action.IncreaseFontSize, pane)
        number_value = number_value - 1
      end
      overrides.enable_tab_bar = false
    elseif number_value < 0 then
      window:perform_action(wezterm.action.ResetFontSize, pane)
      overrides.font_size = nil
      overrides.enable_tab_bar = true
    else
      overrides.font_size = number_value
      overrides.enable_tab_bar = false
    end
  end
  window:set_config_overrides(overrides)
end)

return {
  color_scheme = "tokyonight",
  font_dirs = { "fonts" },
  font_size = 10.0,
  dpi = 192.0,
  freetype_load_target = "Normal",
  font = wezterm.font_with_fallback({
    "InconsolataGo Nerd Font Mono",
  }),
  tab_max_width = 60,
  hide_tab_bar_if_only_one_tab = true,
  enable_scroll_bar = false,
  use_fancy_tab_bar = false,
  window_background_opacity = 0.94,
  default_cursor_style = "BlinkingUnderline",
  set_environment_variables = {
    LANG = "en_US.UTF-8",
    PATH = wezterm.executable_dir .. ";" .. os.getenv("PATH"),
  },
  colors = {
    tab_bar = {
      background = "#121212",
      new_tab = { bg_color = "#121212", fg_color = "#FCE8C3", intensity = "Bold" },
      new_tab_hover = { bg_color = "#121212", fg_color = "#FBB829", intensity = "Bold" },
      active_tab = { bg_color = "#121212", fg_color = "#5156f5" },
    }
  },
  window_background_gradient = {
    orientation = "Vertical",
    interpolation = "Linear",
    blend = "Rgb",
    colors = {
      "#121212",
      "#202020"
    }
  },
  visual_bell = {
    fade_in_duration_ms = 75,
    fade_out_duration_ms = 75,
    target = "CursorColor",
  },
  launch_menu = {
    {
      label = "firefox",
      args = { "firefox" },
    },
    {
      label = "pcmanfm",
      args = { "pcmanfm" },
    },
  },
  -- disable_default_key_bindings = true,
  leader = { key = 'q', mods = 'SHIFT|CTRL', timeout_milliseconds = 1000 },
  keys = {
    -- fixme: use super key; change i3
    { key = 'V',   mods = 'SHIFT|CTRL', action = act.PasteFrom 'Clipboard' },
    { key = 'v',   mods = 'SHIFT|CTRL', action = act.PasteFrom 'Clipboard' },
    { key = 'C',   mods = 'SHIFT|CTRL', action = act.CopyTo 'Clipboard' },
    { key = 'c',   mods = 'SHIFT|CTRL', action = act.CopyTo 'Clipboard' },

    { key = 'Tab', mods = 'CTRL',       action = act.ActivateTabRelative(1) },
    { key = 'Tab', mods = 'SHIFT|CTRL', action = act.ActivateTabRelative(-1) },
    { key = 't',   mods = 'SUPER',      action = act.SpawnTab 'CurrentPaneDomain' },
    { key = '1',   mods = 'SHIFT|CTRL', action = act.ActivateTab(0) },
    { key = '2',   mods = 'SHIFT|CTRL', action = act.ActivateTab(1) },
    { key = '3',   mods = 'SHIFT|CTRL', action = act.ActivateTab(2) },
    { key = '4',   mods = 'SHIFT|CTRL', action = act.ActivateTab(3) },
    { key = '5',   mods = 'SHIFT|CTRL', action = act.ActivateTab(4) },
    { key = '6',   mods = 'SHIFT|CTRL', action = act.ActivateTab(5) },
    { key = '7',   mods = 'SHIFT|CTRL', action = act.ActivateTab(6) },
    { key = '8',   mods = 'SHIFT|CTRL', action = act.ActivateTab(7) },
    { key = '9',   mods = 'SHIFT|CTRL', action = act.ActivateTab(-1) },

    { key = '0',   mods = 'CTRL',       action = act.ResetFontSize },
    { key = '0',   mods = 'SHIFT|CTRL', action = act.ResetFontSize },
    { key = '+',   mods = 'CTRL',       action = act.IncreaseFontSize },
    { key = '+',   mods = 'SHIFT|CTRL', action = act.IncreaseFontSize },
    { key = '=',   mods = 'CTRL',       action = act.IncreaseFontSize },
    { key = '=',   mods = 'SHIFT|CTRL', action = act.IncreaseFontSize },
    { key = '=',   mods = 'SUPER',      action = act.IncreaseFontSize },
    { key = '-',   mods = 'CTRL',       action = act.DecreaseFontSize },
    { key = '-',   mods = 'SHIFT|CTRL', action = act.DecreaseFontSize },
    { key = '-',   mods = 'SUPER',      action = act.DecreaseFontSize },
    -- {key="g", mods="LEADER", action="ShowTabNavigator"},
    -- {key="c", mods="LEADER", action="ShowLauncher"},
    -- {key="r", mods="LEADER", action="ReloadConfiguration"},
    -- {key="x", mods="LEADER", action=wezterm.action{CloseCurrentPane={confirm=true}}},
    -- {key="x", mods="LEADER|SHIFT", action=wezterm.action{CloseCurrentTab={confirm=true}}},
    -- {key="`", mods="LEADER", action=wezterm.action{SendString="`"}},
  }
}
