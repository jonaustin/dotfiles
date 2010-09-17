-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")
-- Quake-style terminal
require("teardrop")
-- Widget libraries
require("vicious")
require("bashets")


-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/themes/zenburn/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvt"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"
altkey = "Alt_L" -- this doesn't appear to work..

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.floating,       --1
    awful.layout.suit.tile,           --2
    awful.layout.suit.tile.left,      --3
    awful.layout.suit.tile.bottom,    --4
    awful.layout.suit.tile.top,       --5
    awful.layout.suit.fair,           --6
    awful.layout.suit.fair.horizontal,--7
    awful.layout.suit.spiral,         --8
    awful.layout.suit.spiral.dwindle, --9
    awful.layout.suit.max,            --10
    awful.layout.suit.max.fullscreen, --11
    awful.layout.suit.magnifier       --12
}
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
tags.setup = {
    { name = "term",  layout = layouts[2]  },
    { name = "term2", layout = layouts[2]  },
    { name = "vimp",   layout = layouts[2]  },
    { name = "firefox",  layout = layouts[2]  },
    { name = "5",    layout = layouts[2], mwfact = 0.13 },
    { name = "6",     layout = layouts[2], }, --hide   = true },
    { name = "7",     layout = layouts[2], }, --hide   = true },
    { name = "files",   layout = layouts[10]  },
    { name = "chrome", layout = layouts[10]  }
}

for s = 1, screen.count() do
    tags[s] = {}
    for i, t in ipairs(tags.setup) do
        tags[s][i] = tag({ name = t.name })
        tags[s][i].screen = s
        awful.tag.setproperty(tags[s][i], "layout", t.layout)
        awful.tag.setproperty(tags[s][i], "mwfact", t.mwfact)
        awful.tag.setproperty(tags[s][i], "hide",   t.hide)
    end
    tags[s][1].selected = true
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awful.util.getdir("config") .. "/rc.lua" },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })
-- }}}

-- {{{ Wibox
-- 
--
-- {{{ Reusable separators
spacer    = widget({ type = "textbox" })
separator = widget({ type = "textbox" })
spacer.text     = " "
separator.text  = "|"
-- }}}

-- {{{ Network usage widget
-- Initialize widget
netwidget = widget({ type = "textbox" })
-- Register widget
vicious.register(netwidget, vicious.widgets.net, '<span color="#CC9393">${eth0 down_kb}</span> <span color="#7F9F7F">${eth0 up_kb}</span>', 3)
dnicon = widget({ type = "imagebox" })
upicon = widget({ type = "imagebox" })
dnicon.image = image(beautiful.widget_netdown)
upicon.image = image(beautiful.widget_netup)
-- }}}

-- Initialize CPU widget
cpuwidget = widget({ type = "textbox" })
-- Register widget
vicious.register(cpuwidget, vicious.widgets.cpu, "$1%")

-- Initialize Uptime widget
uptimewidget = widget({ type = "textbox" })
-- Register widget
vicious.register(uptimewidget, vicious.widgets.uptime, "$1%")
cpuicon = widget({ type = "imagebox" })
cpuicon.image = image(beautiful.widget_cpu)

-- Initialize MPD widget
mpdwidget = widget({ type = "textbox" })
-- Register widget
vicious.register(mpdwidget, vicious.widgets.mpd, "$1%")

--myMPDwidget = widget({ type = "textbox", name = "myMPDwidget"}) 
--bashets.register(myMPDwidget, "mpd.sh", "$1 $2 $3", 1, "|")

-- Initialize Memory widget
memwidget = widget({ type = "textbox" })
-- Register widget
vicious.register(memwidget, vicious.widgets.mem, "$1% ($2MB/$3MB)", 13)
memicon = widget({ type = "imagebox" })
memicon.image = image(beautiful.widget_mem)




-- {{{ Volume level
volicon = widget({ type = "imagebox" })
volicon.image = image(beautiful.widget_vol)
-- Initialize widgets
volwidget = widget({ type = "textbox" })
vicious.register(volwidget, vicious.widgets.volume, "$1%", 2, "Master")
-- }}}

-- Create a textclock widget
mytextclock = awful.widget.textclock({ align = "right" })

-- Create a systray
mysystray = widget({ type = "systray" })

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, awful.tag.viewnext),
                    awful.button({ }, 5, awful.tag.viewprev)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if not c:isvisible() then
                                                  awful.tag.viewonly(c:tags()[1])
                                              end
                                              client.focus = c
                                              c:raise()
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(function(c)
                                              return awful.widget.tasklist.label.currenttags(c, s)
                                          end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            mylauncher,
            mytaglist[s],
            mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        mylayoutbox[s],
        mytextclock,
        spacer, separator, spacer, volwidget, volicon,
        --spacer, separator, spacer, cpuwidget, cpuicon,
        spacer, separator, spacer, uptimewidget, cpuicon,
        spacer, separator, spacer, memwidget, memicon,
        spacer, separator, spacer, upicon, netwidget, dnicon,
        spacer, separator, spacer, mpdwidget,
        --spacer, separator, spacer, myMPDwidget,
        s == 1 and mysystray or nil,
        mytasklist[s],
        layout = awful.widget.layout.horizontal.rightleft
    }
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    -- {{{ Applications
    awful.key({ modkey, "Shift" }, "w", function () awful.util.spawn("firefox", false) end),
    awful.key({ modkey }, "y", function () awful.util.spawn("firefox", false) end),
    awful.key({ modkey }, "w", function () awful.util.spawn("vimprobable", false) end),
    awful.key({ modkey }, "q", function () awful.util.spawn("vimprobable", false) end),
    -- awful.key({ modkey }, "F1",    function () awful.util.spawn("urxvt", false) end),
    awful.key({ modkey }, "grave", function () teardrop.toggle('urxvt','top','center',0.99999,0.3) end),
    --awful.key({ altkey }, "grave", function () teardrop.toggle("urxvt") end),
    awful.key({ modkey }, "F1", function () teardrop.toggle('urxvt','top','center',0.99999,0.3) end),
    awful.key({ modkey }, "a", function ()
        awful.util.spawn("urxvt -title Alpine -e alpine_exp", false)
    end),
    awful.key({ modkey }, "t",    function () awful.util.spawn("thunar", false) end),
    -- }}}

    -- {{{ Multimedia keys
    awful.key({}, "#107", function () awful.util.spawn("/home/jon/bin/softer", false) end),
    awful.key({}, "#78", function () awful.util.spawn("/home/jon/bin/louder", false) end),
    awful.key({}, "#127", function () awful.util.spawn("/home/jon/bin/mute", false) end),
    awful.key({ "Shift" }, "#107", function () awful.util.spawn("amixer -D hw:1 set Speaker 1-", false) end),
    awful.key({ "Shift" }, "#78", function () awful.util.spawn("amixer -D hw:1 set Speaker 1+", false) end),
    awful.key({ modkey }, "d",function () awful.util.spawn("/home/jon/bin/mpd_status.sh 6600", false) end),
    awful.key({ modkey, "Shift" }, "d",function () awful.util.spawn("/home/jon/bin/mpd_status.sh 6602", false) end),
    awful.key({ modkey, "Control" }, "d",function () awful.util.spawn("mpc -p 6600 del 0", false) end),
    awful.key({ modkey, "Shift", "Control" }, "d",function () awful.util.spawn("mpc -p 6602 del 0", false) end),
    awful.key({ modkey }, "n",function () awful.util.spawn_with_shell("mpc -p 6600 next; /home/jon/bin/mpd_status.sh 6600", false) end),
    awful.key({ modkey, "Shift" }, "n",function () awful.util.spawn_with_shell("mpc -p 6602 next; /home/jon/bin/mpd_status.sh 6602", false) end),
    awful.key({ modkey }, "v",function () awful.util.spawn_with_shell("mpc -p 6600 prev; /home/jon/bin/mpd_status.sh 6600", false) end),
    awful.key({ modkey, "Shift" }, "v",function () awful.util.spawn_with_shell("mpc -p 6602 prev; /home/jon/bin/mpd_status.sh 6602", false) end),
    awful.key({ modkey }, "x",function () awful.util.spawn_with_shell("mpc -p 6600 toggle", false) end),
    awful.key({ modkey, "Shift" }, "x",function () awful.util.spawn_with_shell("mpc -p 6602 toggle", false) end),
    awful.key({ modkey }, "p", function () awful.util.spawn("/usr/bin/projectM-libvisual-alsa") end),
--    awful.key({ modkey }, "z", function () awful.util.spawn("/home/jon/bin/cur_task.sh") end),
    awful.key({ modkey }, "u", function () awful.util.spawn("/home/jon/bin/uptime.sh") end),
    awful.key({ modkey, "Shift" }, "u", function () awful.util.spawn("/home/jon/bin/cpu_hogs.sh") end),
    awful.key({ modkey, "Shift" }, "d", function () awful.util.spawn("/home/jon/bin/datetime.sh") end),
    -- }}}


    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    -- switch to other screen
    awful.key({ modkey }, "]", function () awful.screen.focus(1) end),
    awful.key({ modkey }, "[", function () awful.screen.focus(2) end),
--    awful.key({ modkey,           }, "w", function () mymainmenu:show(true)        end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
--    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "F2",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey, "Shift"   }, "n",      function (c) c.minimized = not c.minimized    end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end),
    awful.key({ modkey }, "b", 
        function () -- Hide the wibox
          if mywibox[mouse.screen].screen == nil then mywibox[mouse.screen].screen = mouse.screen
          else mywibox[mouse.screen].screen = nil end
        end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
