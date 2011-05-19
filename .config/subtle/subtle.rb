# = Subtle config
# == Options {{{
#
# Following options change behaviour and sizes of the window manager:
#
# Border size in pixel of the windows

# Window move/resize steps in pixel per keypress
set :step, 5

# Window screen border snapping
set :snap, 10

# Default starting gravity for windows (0 = gravity of last client)
set :gravity, :center

# Make transient windows urgent
set :urgent, true

# Honor resize size hints globally
set :resize, false

# Font string either take from e.g. xfontsel or use xft
set :font, "-artwiz-snap-*-*-*-*-*-*-*-*-*-*-*-*"
#set :font, "xft:sans-8"

# Separator between sublets
set :separator, "|"

# Set the WM_NAME of subtle (Java quirk)
# set _wmname, "LG3D"
# }}}

# == Screen {{{
# Notes {{{
# Generally subtle comes with two panels per screen, one on the top and one at
# the bottom. Each panel can be configured with different panel items and
# sublets screen wise. The default config uses top panel on the first screen
# only, it's up to the user to enable the bottom panel or disable either one
# or both.
#
# Empty panels are hidden.
#
# Following items are available:
#
# [*:views*]     List of views with buttons
# [*:title*]     Title of the current active window
# [*:tray*]      Systray icons (Can be used once)
# [*:sublets*]   Catch-all for installed sublets
# [*:sublet*]    Name of a sublet for direct placement
# [*:spacer*]    Variable spacer (free width / count of spacers)
# [*:center*]    Enclose items with :center to center them on the panel
# [*:separator*] Insert separator
#
# === Link
#
# http://subforge.org/projects/subtle/wiki/Panel
# }}}

screen 1 do
  # Add stipple to panels
  stipple false

  # Content of the top panel
  #top     [ :views, :title, :spacer, :tray, :sublets ]
  top     [:views, :title, :spacer, :center, :volume, :mpd, :center, :sublets, :separator, :tray, :fuzzytime]

  # Content of the bottom panel
  bottom  [ ]
end

# Example for a second screen:
#screen 2 do
#  # Add stipple to panels
#  stipple false
#
#  # Content of the top panel
#  top     [ :views, :title, :spacer ]
#
#  # Content of the bottom panel
#  bottom  [ ]
#end
# }}}

# == Colors {{{
# Notes {{{
# Colors directly define the look of subtle, valid values are:
#
# [*hexadecimal*] #0000ff
# [*decimal*]     (0, 0, 255)
# [*names*]       blue
#
# Whenever there is no valid value for a color set - subtle will use a default
# one. There is only one exception to this: If no background color is given no
# color will be set. This will ensure a custom background pixmap won't be
# overwritten.
#
# === Link
#
# http://subforge.org/projects/subtle/wiki/Themes
# }}}

style :title do
  padding     4, 4, 2, 2
  border      "#1a1a1a", 1
  foreground  "#ffffff"
  background  "#1a1a1a"
end

style :focus do
  padding     4, 4, 2, 2
  border      "#1a1a1a", 1
  foreground  "#ffffff"
  background  "#595959"
end

style :urgent do
  padding     4, 4, 2, 2
  border      1
  foreground  "#DF8787"
end

style :occupied do
  padding     4, 4, 2, 2
  border      "#1a1a1a", 1
  foreground  "#777777"
  background  "#404040"
end

style :views do
  padding     4, 4, 2, 2
  border      "#1a1a1a", 1
  foreground  "#a8a8a8"
  background  "#1a1a1a"
end

style :sublets do
  padding     4, 4, 2, 2
  border      "#1a1a1a", 1
  foreground  "#a8a8a8"
  background  "#1a1a1a"
end

style :separator do
  padding     4, 4, 2, 2
  border      1
  background  "#1a1a1a"
  foreground  "#DF8787"
end

style :clients do
  active      "#a8a8a8", 0
  inactive    "#404040", 0
  margin      1
end

style :subtle do
  padding     0, 0, 0, 0
  panel       "#1a1a1a"
  stipple     "#595959"
end



#color :urgent_bg,       "#404040"
#color :urgent_border,   "#1a1a1a"



#color :sublets_fg,      "#595959"


#color :background,     "#404040"

# }}}

# == Gravities {{{
# Notes {{{
# Gravities are predefined sizes a window can be set to. There are several ways
# to set a certain gravity, most convenient is to define a gravity via a tag or
# change them during runtime via grab. Subtler and subtlext can also modify
# gravities.
#
# A gravity consists of four values which are a percentage value of the screen
# size. The first two values are x and y starting at the center of the screen
# and he last two values are the width and height.
#
# === Example
#
# Following defines a gravity for a window with 100% width and height:
#
#   gravity :example, [ 0, 0, 100, 100 ]
#
# === Link
#
# http://subforge.org/projects/subtle/wiki/Gravity
# }}}

  # Top left
gravity :top_left,       [   0,   0,  50,  50 ]
gravity :top_left66,     [   0,   0,  50,  66 ]
gravity :top_left33,     [   0,   0,  50,  34 ]

  # Top
gravity :top,            [   0,   0, 100,  50 ]
gravity :top66,          [   0,   0, 100,  66 ]
gravity :top33,          [   0,   0, 100,  34 ]

  # Top right
gravity :top_right,      [ 100,   0,  50,  50 ]
gravity :top_right66,    [ 100,   0,  50,  66 ]
gravity :top_right33,    [ 100,   0,  50,  34 ]

  # Left
gravity :left,           [   0,   0,  50, 100 ]
gravity :left66,         [   0,  50,  50,  34 ]
gravity :left33,         [   0,  50,  25,  34 ]

  # Center
gravity :center,         [   0,   0, 100, 100 ]
gravity :center66,       [   0,  50, 100,  34 ]
gravity :center33,       [  50,  50,  50,  34 ]

  # Right
gravity :right,          [ 100,   0,  50, 100 ]
gravity :right66,        [ 100,  50,  50,  34 ]
gravity :right33,        [ 100,  50,  25,  34 ]

  # Bottom left
gravity :bottom_left,    [   0, 100,  50,  50 ]
gravity :bottom_left66,  [   0, 100,  50,  66 ]
gravity :bottom_left33,  [   0, 100,  50,  34 ]

  # Bottom
gravity :bottom,         [   0, 100, 100,  50 ]
gravity :bottom66,       [   0, 100, 100,  66 ]
gravity :bottom33,       [   0, 100, 100,  34 ]

  # Bottom right
gravity :bottom_right,   [ 100, 100,  50,  50 ]
gravity :bottom_right66, [ 100, 100,  50,  66 ]
gravity :bottom_right33, [ 100, 100,  50,  34 ]

  # Gimp
gravity :gimp_image,     [  50,  50,  80, 100 ]
gravity :gimp_toolbox,   [   0,   0,  10, 100 ]
gravity :gimp_dock,      [ 100,   0,  10, 100 ]
# }}}

# == Grabs {{{
# Notes {{{
# Grabs are keyboard and mouse actions within subtle, every grab can be
# assigned either to a key and/or to a mouse button combination. A grab
# consists of a chain and an action.
#
# === Finding keys
#
# The best resource for getting the correct key names is
# */usr/include/X11/keysymdef.h*, but to make life easier here are some hints
# about it:
#
# * Numbers and letters keep their names, so *a* is *a* and *0* is *0*
# * Keypad keys need *KP_* as prefix, so *KP_1* is *1* on the keypad
# * Strip the *XK_* from the key names if looked up in
#   /usr/include/X11/keysymdef.h
# * Keys usually have meaningful english names
# * Modifier keys have special meaning (Alt (A), Control (C), Meta (M),
#   Shift (S), Super (W))
#
# === Chaining
#
# Chains are a combination of keys and modifiers to one key and can be used in
# various ways to trigger an action. In subtle there are two ways to define
# chains for grabs:
#
#   1. Default way*: Add modifiers to a key and use it for a grab
#
#      *Example*: grab "W-Return", "urxvt"
#
#   2. *Escape way*: Define an escape grab that needs to be pressed before
#      *any* other grab can be used like in screen/tmux.
#
#      *Example*: grab "C-y", :SubtleEscape
#                 grab "Return", "urxvt"
#
# ==== Mouse buttons
#
# [*B1*] = Button1 (Left mouse button)
# [*B2*] = Button2 (Middle mouse button)
# [*B3*] = Button3 (Right mouse button)
# [*B4*] = Button4 (Mouse wheel up)
# [*B5*] = Button5 (Mouse wheel down)
#
# ==== Modifiers
#
# [*A*] = Alt key
# [*C*] = Control key
# [*M*] = Meta key
# [*S*] = Shift key
# [*W*] = Super (Windows) key
#
# === Action
#
# An action is something that happens when a grab is activated, this can be one
# of the following:
#
# [*symbol*] Run a subtle action
# [*string*] Start a certain program
# [*array*]  Cycle through gravities
# [*lambda*] Run a Ruby proc
#
# === Example
#
# This will create a grab that starts a urxvt when Alt+Enter are pressed:
#
#   grab "A-Return", "urxvt"
#
# === Link
#
# http://subforge.org/projects/subtle/wiki/Grabs
# }}}
# Subtle {{{
# Escape grab
#  grab "C-y", :SubtleEscape

# Jump to view1, view2, ... (multi-head setups only...see wiki)
grab "W-S-1", :ViewJump1
grab "W-S-2", :ViewJump2
grab "W-S-3", :ViewJump3
grab "W-S-4", :ViewJump4

# Switch current view
grab "W-1", :ViewSwitch1
grab "W-2", :ViewSwitch2
grab "W-3", :ViewSwitch3
grab "W-4", :ViewSwitch4
grab "W-5", :ViewSwitch5
grab "W-6", :ViewSwitch6
grab "W-7", :ViewSwitch7
grab "W-8", :ViewSwitch8
grab "W-9", :ViewSwitch9

# Select next and prev view */
grab "W-Right", :ViewNext
grab "W-Left",  :ViewPrev

# Move mouse to screen1, screen2, ... (multi-head setups)
grab "W-A-1", :ScreenJump1
grab "W-A-2", :ScreenJump2
grab "W-A-3", :ScreenJump3
grab "W-A-4", :ScreenJump4

# Force reload of config and sublets
grab "W-C-r", :SubtleReload

# Force restart of subtle
grab "W-C-S-r", :SubtleRestart

# Quit subtle
grab "W-C-q", :SubtleQuit

# Move current window
grab "W-B1", :WindowMove

# Resize current window
grab "W-B3", :WindowResize

# Toggle floating mode of window
grab "W-space", :WindowFloat

# Toggle fullscreen mode of window
grab "W-f", :WindowFull

# Toggle sticky mode of window (will be visible on all views)
grab "W-S-s", :WindowStick

# Raise window
grab "W-S-r", :WindowRaise

# Lower window
grab "W-S-l", :WindowLower

# Select next windows
grab "W-h",   :WindowLeft
grab "W-j",   :WindowDown
grab "W-k",   :WindowUp
grab "W-l",   :WindowRight

# Kill current window
grab "W-S-c", :WindowKill

# Cycle between given gravities
grab "W-q", [ :top_left,     :top_left66,     :top_left33     ]
grab "W-w", [ :top,          :top66,          :top33          ]
grab "W-e", [ :top_right,    :top_right66,    :top_right33    ]
grab "W-a", [ :left,         :left66,         :left33         ]
grab "W-s", [ :center,       :center66,       :center33       ]
grab "W-d", [ :right,        :right66,        :right33        ]
grab "W-z", [ :bottom_left,  :bottom_left66,  :bottom_left33  ]
grab "W-x", [ :bottom,       :bottom66,       :bottom33       ]
grab "W-c", [ :bottom_right, :bottom_right66, :bottom_right33 ]
# }}}
# Exec programs {{{
grab "W-Return", "urxvt"
grab "W-u", "urxvt -T term2"
grab "W-i", "urxvt -T term3"
grab "W-S-w", "firefox"
grab "W-t", "thunar"
# MPD
grab "W-n", "mpd_cmd next"
grab "W-v", "mpd_cmd prev"
grab "W-p", "mpd_cmd toggle"
grab "W-S-p", "mpd_cmd toggle 1100"
grab "W-m", "mpd_status"
grab "F21", "softer"
grab "F22", "louder"
# system
grab "W-r", "bashrun"
grab "W-S-r", "bashrun2"
grab "W-o", "selector.rb"

grab "A-Tab" do |c|
   sel     = 0
   clients = Subtlext::Client.visible

   clients.each_index do |idx|
     if(clients[idx].id == c.id)
       sel = idx + 1 if(idx < clients.size - 1)
     end
   end

  clients[sel].focus
end

# Run Ruby lambdas
grab "S-F2" do |c|
  puts c.name
end

grab "S-F3" do
  puts Subtlext::VERSION
end
# }}}
# }}}

# == Tags {{{
# Notes {{{
# Tags are generally used in subtle for placement of windows. This placement is
# strict, that means that - aside from other tiling window managers - windows 
# must have a matching tag to be on a certain view. This also includes that
# windows that are started on a certain view will not automatically be placed
# there.
#
# There are to ways to define a tag:
#
# === Simple
#
# The simple way just needs a name and a regular expression to just handle the
# placement:
#
# Example:
#
#  tag "terms", "terms"
#
# === Extended
#
# Additionally tags can do a lot more then just control the placement - they
# also have properties than can define and control some aspects of a window
# like the default gravity or the default screen per view.
#
# Example:
#
#  tag "terms" do
#    match   "xterm|[u]?rxvt"
#    gravity :center
#  end
#
# === Default
#
# Whenever a window has no tag it will get the default tag and be placed on the
# default view. The default view can either be set by the user with adding the
# default tag to a view by choice or otherwise the first defined view will be
# chosen automatically.
#
# === Properties
#
# [*float*]     This property either sets the tagged client floating or prevents
#               it from being floating depending on the value.
#
#               Example: float true
#
# [*full*]      This property either sets the tagged client to fullscreen or
#               prevents it from being set to fullscreen depending on the value.
#
#               Example: full true
#
# [*geometry*]  This property sets a certain geometry as well as floating mode
#               to the tagged client, but only on views that have this tag too.
#               It expects an array with x, y, width and height values whereas
#               width and height must be >0.
#
#               Example: geometry [100, 100, 50, 50]
#
# [*gravity*]   This property sets a certain to gravity to the tagged client,
#               but only on views that have this tag too.
#
#              Example: gravity :center
#
# [*match*]    This property adds matching patterns to a tag, a tag can have
#              more than one. Matching works either via plaintext, regex
#              (see man regex(7)) or window id. Per default tags will only
#              match the WM_NAME and the WM_CLASS portion of a client, this
#              can be changed with following possible values:
#
#              [*:name*]      Match the WM_NAME
#              [*:instance*]  Match the first (instance) part from WM_CLASS
#              [*:class*]     Match the second (class) part from WM_CLASS
#              [*:role*]      Match the window role
#
#              Example: match :instance => "urxvt"
#                       match [:role, :class] => "test"
#                       match "[xa]+term"
#
# [*exclude*]  This property works exactly the same way as *match*, but it
#              excludes clients that match from this tag. That can be helpful
#              with catch-all tags e.g. for console apps.
#
#              Example: exclude :instance => "irssi"
#
# [*resize*]   This property either enables or disables honoring of client
#              resize hints and is independent of the global option.
#
#              Example: resize true
#
# [*stick*]    This property either sets the tagged client to stick or prevents
#              it from being set to stick depending on the value. Stick clients
#              are visible on every view.
#
#              Example: stick true
#
# [*type*]     This property sets the [[Tagging|tagged]] client to be treated
#              as a specific window type though as the window sets the type
#              itself. Following types are possible:
#
#              [*:desktop*]  Treat as desktop window (_NET_WM_WINDOW_TYPE_DESKTOP)
#              [*:dock*]     Treat as dock window (_NET_WM_WINDOW_TYPE_DOCK)
#              [*:toolbar*]  Treat as toolbar windows (_NET_WM_WINDOW_TYPE_TOOLBAR)
#              [*:splash*]   Treat as splash window (_NET_WM_WINDOW_TYPE_SPLASH)
#              [*:dialog*]   Treat as dialog window (_NET_WM_WINDOW_TYPE_DIALOG)
#
#              Example: type :desktop
#
# [*urgent*]   This property either sets the tagged client to be urgent or
#              prevents it from being urgent depending on the value. Urgent
#              clients will get keyboard and mouse focus automatically.
#
#              Example: urgent true
#
# === Link
#
# http://subforge.org/projects/subtle/wiki/Tagging
# }}}

# Simple tags
tag "terms" do
  match :name => "xterm|[u]?rxvt"
end

tag "terms2" do
  match :name => "term2"
end

tag "terms3" do
  match :name => "term3"
end

tag "browser", "uzbl|opera|firefox|navigator"

tag "filemanagers", "pcmanfm|thunar|ranger|vifm|feh"

tag "media", "vlc|mplayer|amarok"

tag "pdf", "apvlv|evince|acroread"

tag "bashrun" do
  match  "bashrun" 
  geometry   [ 800, 600, 200, 18 ] ### err..this doesn't seem to do a thing, always in the same place no matter what is here..
  stick  true
  float  true
  urgent true
  #screen 0
end

# Placement
tag "editor" do
  match  "[g]?vim"
  resize true
end

tag "fixed" do
  geometry [ 10, 10, 100, 100 ]
  stick    true
end

tag "resize" do
  match  "sakura|gvim"
  resize true
end

tag "gravity" do
  gravity :center
end

# Modes
tag "stick" do
  match "mplayer|vlc"
  float true
  stick true
end

tag "float" do
  match "display"
  float true
end

# Gimp
tag "gimp_image" do
  match   :role => "gimp-image-window"
  gravity :gimp_image
end

tag "gimp_toolbox" do
  match   :role => "gimp-toolbox$"
  gravity :gimp_toolbox
end

tag "gimp_dock" do
  match   :role => "gimp-dock"
  gravity :gimp_dock
end

# Hacks
tag "flash" do
  match "<unknown>|exe|operapluginwrapper|npviewer.bin"  # <unkown> = firefox ---- http://subforge.org/ezfaq/show/subtle?faq_id=20
  stick true
end

# }}}

# == Views {{{
# Notes {{{
# Views are the virtual desktops in subtle, they show all windows that share a
# tag with them. Windows that have no tag will be visible on the default view
# which is the view with the default tag or the first defined view when this
# tag isn't set.
#
# Like tags views can be defined in two ways:
#
# === Simple
#
# The simple way is exactly the same as for tags:
#
# Example:
#
#   view "terms", "terms"
#
# === Extended
#
# The extended way for views is also similar to the tags, but with fewer
# properties.
#
# Example:
#
#  view "terms" do
#    match "terms"
#    icon  "/usr/share/icons/icon.xbm"
#  end
#
# === Properties
#
# [*match*]      This property adds a matching pattern to a view. Matching
#                works either via plaintext or regex (see man regex(7)) and
#                applies to names of tags.
#
#                Example: match "terms"
#
# [*dynamic*]    This property hides unoccupied views, views that display no
#                windows.
#
#                Example: dynamic true
#
# [*icon*]       This property adds an icon in front of the view name. The
#                icon can either be path to an icon or an instance of
#                Subtlext::Icon.
#
#                Example: icon "/usr/share/icons/icon.xbm"
#                         icon Subtlext::Icon.new("/usr/share/icons/icon.xbm")
#
# [*icon_only*]  This property hides the view name from the view buttons, just
#                the icon will be visible.
#
#                Example: icon_only true
#
#
# === Link
#
# http://subforge.org/projects/subtle/wiki/Tagging
# }}}

view "terms",  "^terms$"
view "terms2", "^terms2$"
view "terms3", "^terms3$"
view "www",    "^browser$"
view "other",  "default"
view "files",  "filemanagers"
view "media",  "media"
view "pdf",    "pdf"
# }}}

# == Sublets {{{
# Notes {{{
# Sublets are Ruby scripts that provide data for the panel and can be managed
# with the sur script that comes with subtle.
#
# === Example
#
#  sur install clock
#  sur uninstall clock
#  sur list
#
# === Configuration
#
# All sublets have a set of configuration values that can be changed directly
# from the config of subtle.
#
# There are three default properties, that can be be changed for every sublet:
#
# [*interval*]    Update interval of the sublet
# [*foreground*]  Default foreground color
# [*background*]  Default background color
#
# sur can also give a brief overview about properties:
#
# === Example
#
#   sur config clock
#
# The syntax of the sublet configuration is similar to other configuration
# options in subtle:
#
# === Example
#
#  sublet :clock do
#    interval      30
#    foreground    "#eeeeee"
#    background    "#000000"
#    format_string "%H:%M:%S"
#  end
#
#  === Link
#
# http://subforge.org/projects/subtle/wiki/Sublets
#
# }}}
#sublet :gmail do
#  interval  360
##  user      'jon.i.austin'
##  pass      'auximinus'
#  urgent    '#ffc400'
#  normal    '#eeeeee'
#end

# }}}

# == Hooks {{{
# Notes {{{
# And finally hooks are a way to bind Ruby scripts to a certain event.
#
# Following hooks exist so far:
#
# [*:client_create*]    Called whenever a window is created
# [*:client_configure*] Called whenever a window is configured
# [*:client_focus*]     Called whenever a window gets focus
# [*:client_kill*]      Called whenever a window is killed
#
# [*:tag_create*]       Called whenever a tag is created
# [*:tag_kill*]         Called whenever a tag is killed
#
# [*:view_create*]      Called whenever a view is created
# [*:view_configure*]   Called whenever a view is configured
# [*:view_jump*]        Called whenever the view is switched
# [*:view_kill*]        Called whenever a view is killed
#
# [*:tile*]             Called on whenever tiling would be needed
# [*:reload*]           Called on reload
# [*:start*]            Called on start
# [*:exit*]             Called on exit
#
# === Example
#
# This hook will print the name of the window that gets the focus:
#
#   on :client_focus do |c|
#     puts c.name
#   end
#
# === Link
#
# http://subforge.org/projects/subtle/wiki/Hooks
# }}}

# On subtle startup (or restart)
on :start do
  #  c = Subtlext::Subtle.spawn("urxvt -name quaketerm")
end

# IMPORTANT -- this is dangerous -- i.e. any fullscreen app (i.e. flash/vlc/etc) needs to be in the explicit list below, otherwise it'll be all screwed up
# always switch to client upon creation
on :client_create do |c|
  c.views.first.jump unless (c.name == 'bashrun' or c.name == 'Dialog' or c.name == '<unknown>' or c.name == 'vlc')
end

# }}}

# == Hacks {{{

# Quake-style 'drop-down' console
grab "W-grave" do
  if((c = Subtlext::Client["quaketerm"]))
    c.gravity = :top33
    c.toggle_stick
    c.focus
  elsif((c = Subtlext::Subtle.spawn("urxvt -name quaketerm")))
    # setting gravity here doesn't do anything...wtf.
    c.gravity = :top33 # [:top33] works even less well (i.e. still no gravity change and doesn't hide the window anymore)
    c.tags  = [] 
    c.flags = [ :stick ]
  end
end

# Move windows
# This snippet adds nine grabs to move windows on the fly to nine defined views. It uses tagging for this, creates tags based on the view names and applies them when needed.
on :start do
  # Create missing tags
  views = Subtlext::View.all.map { |v| v.name }
  tags  = Subtlext::Tag.all.map { |t| t.name }

  views.each do |v|
    unless(tags.include?(v))
      t = Subtlext::Tag.new(v)
      t.save
    end
  end
end

# Add nine S-C-< number> grabs
(1..9).each do |i|
  grab "W-C-%d" % [ i ] do |c|
    views = Subtlext::View.all
    names = views.map { |v| v.name }

    # Sanity check
    if(i <= views.size)
      # Tag client
      tags = c.tags.reject { |t| names.include?(t.name) or "default" == t.name }
      tags << names[i - 1]

      c.tags = tags

      # Tag view
      views[i - 1].tag(names[i - 1])
    end
  end
end



# vim:ts=2:bs=2:sw=2:et:fdm=marker
