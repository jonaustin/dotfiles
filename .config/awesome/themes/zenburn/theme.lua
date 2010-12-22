-------------------------------
--  "Zenburn" awesome theme  --
--    By Adrian C. (anrxc)   --
-------------------------------

-- Alternative icon sets and widget icons:
--  * http://awesome.naquadah.org/wiki/Nice_Icons

-- {{{ Main
theme = {}
theme.wallpaper_cmd = { "awsetbg /home/jon/.config/awesome/themes/zenburn/zenburn-background.png" } --calvin_hobbes.png" }
-- }}}

-- {{{ Styles
theme.font      = "Terminus 8"

-- {{{ Colors
theme.fg_normal = "#D8dada"
theme.fg_focus  = "#F0DFAF"
theme.fg_urgent = "#CC9393"
theme.bg_normal = "#1f1f1f"
theme.bg_focus  = "#1E2320"
theme.bg_urgent = "#3F3F3F"
-- }}}

-- {{{ Borders
theme.border_width  = "1"
theme.border_normal = "#3F3F3F"
theme.border_focus  = "#6F6F6F"
theme.border_marked = "#CC9393"
-- }}}

-- {{{ Titlebars
theme.titlebar_bg_focus  = "#3F3F3F"
theme.titlebar_bg_normal = "#3F3F3F"
-- }}}

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- Example:
--theme.taglist_bg_focus = "#CC9393"
-- }}}

-- {{{ Widgets
-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
theme.fg_widget        = "#AECF96"
theme.fg_center_widget = "#88A175"
theme.fg_end_widget    = "#FF5656"
theme.fg_off_widget    = "#494B4F"
theme.fg_netup_widget  = "#7F9F7F"
theme.fg_netdn_widget  = "#CC9393"
theme.bg_widget        = "#3F3F3F"
theme.border_widget    = "#3F3F3F"
-- }}}

-- {{{ Mouse finder
theme.mouse_finder_color = "#CC9393"
-- mouse_finder_[timeout|animate_timeout|radius|factor]
-- }}}

-- {{{ Menu
-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_height = "15"
theme.menu_width  = "100"
-- }}}

-- {{{ Icons
-- {{{ Taglist
theme.taglist_squares_sel   = "/home/jon/.config/awesome/themes/zenburn/taglist/squarefz.png"
theme.taglist_squares_unsel = "/home/jon/.config/awesome/themes/zenburn/taglist/squarez.png"
--theme.taglist_squares_resize = "false"
-- }}}

-- {{{ Misc
theme.awesome_icon           = "/home/jon/.config/awesome/themes/icons/awesome16.png"
theme.menu_submenu_icon      = "/home/jon/.config/awesome/themes/default/submenu.png"
theme.tasklist_floating_icon = "/home/jon/.config/awesome/themes/default/tasklist/floatingw.png"
-- }}}

-- {{{ Layout
theme.layout_tile       = "/home/jon/.config/awesome/themes/icons/layouts/tile.png"
theme.layout_tileleft   = "/home/jon/.config/awesome/themes/icons/layouts/tileleft.png"
theme.layout_tilebottom = "/home/jon/.config/awesome/themes/icons/layouts/tilebottom.png"
theme.layout_tiletop    = "/home/jon/.config/awesome/themes/icons/layouts/tiletop.png"
theme.layout_fairv      = "/home/jon/.config/awesome/themes/icons/layouts/fairv.png"
theme.layout_fairh      = "/home/jon/.config/awesome/themes/icons/layouts/fairh.png"
theme.layout_spiral     = "/home/jon/.config/awesome/themes/icons/layouts/spiral.png"
theme.layout_dwindle    = "/home/jon/.config/awesome/themes/icons/layouts/dwindle.png"
theme.layout_max        = "/home/jon/.config/awesome/themes/icons/layouts/max.png"
theme.layout_fullscreen = "/home/jon/.config/awesome/themes/icons/layouts/fullscreen.png"
theme.layout_magnifier  = "/home/jon/.config/awesome/themes/icons/layouts/magnifier.png"
theme.layout_floating   = "/home/jon/.config/awesome/themes/icons/layouts/floating.png"
-- }}}

-- {{{ Widget icons
theme.widget_cpu    = "/home/jon/.config/awesome/themes/icons/cpu.png"
theme.widget_bat    = "/home/jon/.config/awesome/themes/icons/bat.png"
theme.widget_mem    = "/home/jon/.config/awesome/themes/icons/mem.png"
theme.widget_fs     = "/home/jon/.config/awesome/themes/icons/disk.png"
theme.widget_netdown    = "/home/jon/.config/awesome/themes/icons/down.png"
theme.widget_netup  = "/home/jon/.config/awesome/themes/icons/up.png"
theme.widget_mail   = "/home/jon/.config/awesome/themes/icons/mail.png"
theme.widget_vol    = "/home/jon/.config/awesome/themes/icons/vol.png"
theme.widget_org    = "/home/jon/.config/awesome/themes/icons/cal.png"
theme.widget_date   = "/home/jon/.config/awesome/themes/icons/time.png"
theme.widget_crypto = "/home/jon/.config/awesome/themes/icons/crypto.png"
-- }}}

-- {{{ Titlebar
theme.titlebar_close_button_focus  = "/home/jon/.config/awesome/themes/zenburn/titlebar/close_focus.png"
theme.titlebar_close_button_normal = "/home/jon/.config/awesome/themes/zenburn/titlebar/close_normal.png"

theme.titlebar_ontop_button_focus_active  = "/home/jon/.config/awesome/themes/zenburn/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active = "/home/jon/.config/awesome/themes/zenburn/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive  = "/home/jon/.config/awesome/themes/zenburn/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive = "/home/jon/.config/awesome/themes/zenburn/titlebar/ontop_normal_inactive.png"

theme.titlebar_sticky_button_focus_active  = "/home/jon/.config/awesome/themes/zenburn/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active = "/home/jon/.config/awesome/themes/zenburn/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive  = "/home/jon/.config/awesome/themes/zenburn/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive = "/home/jon/.config/awesome/themes/zenburn/titlebar/sticky_normal_inactive.png"

theme.titlebar_floating_button_focus_active  = "/home/jon/.config/awesome/themes/zenburn/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active = "/home/jon/.config/awesome/themes/zenburn/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive  = "/home/jon/.config/awesome/themes/zenburn/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive = "/home/jon/.config/awesome/themes/zenburn/titlebar/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active  = "/home/jon/.config/awesome/themes/zenburn/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active = "/home/jon/.config/awesome/themes/zenburn/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = "/home/jon/.config/awesome/themes/zenburn/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = "/home/jon/.config/awesome/themes/zenburn/titlebar/maximized_normal_inactive.png"
-- }}}
-- }}}

return theme
