# Mpd specification file
# Created with sur-0.1
Sur::Specification.new do |s|
  # Sublet information
  s.name        = "Mpd"
  s.version     = "0.46"
  s.tags        = [ "Socket", "Icon", "Config", "Format" ]
  s.files       = [ "mpd.rb" ]
  s.icons       = [
    "icons/play.xbm",
    "icons/pause.xbm",
    "icons/stop.xbm",
    "icons/prev.xbm",
    "icons/next.xbm",
    "icons/note.xbm",
    "icons/repeat.xbm",
    "icons/shuffle.xbm",
    "icons/diskette.xbm"
  ]

  # Sublet authors
  s.authors     = [ "unexist" ]
  s.contact     = "unexist@dorfelite.net"
  s.date        = "Tue Jan 18 15:05 CET 2011"

  # Sublet description
  s.description = "Show the current track of mpd and some controls"
  s.notes       = <<NOTES
This sublet displays the current track playing in mpd along with control
buttons like play and stop and random and shuffle mode.

Dependent on the mpd version (>= 0.16), a running database update will be
indicated as well.

The output of the sublet can be customized by a format_string. Following
fields are allowed alongside all kind of text, icons and colors:

%note%   - Predefined icon
%artist% - Name of the artist
%album%  - Name of the album
%title%  - Name of the track
%track%  - Track number of current album
%id%     - mpd playlist id

Examples:

%note%%artist% - %album
%artist% :: %album% :: %title%

Empty fields are just replaced by 'n/a'.

[1] http://www.musicpd.org/doc/protocol/ch02.html#id555938
NOTES

  # Sublet config
  s.config      = [
    {
      :name        => "host",
      :type        => "string",
      :description => "Hostname to connect to (host@password)",
      :def_value   => "$MPD_HOST, localhost"
    },
    {
      :name        => "port",
      :type        => "integer",
      :description => "Port number",
      :def_value   => "$MPD_PORT, 6600"
    },
    {
      :name        => "def_action",
      :type        => "string",
      :description => "Default action on click",
      :def_value   => "next"
    },
    {
      :name        => "wheel_up",
      :type        => "string",
      :description => "Default action on wheel up",
      :def_value   => "next"
    },
    {
      :name        => "wheel_down",
      :type        => "string",
      :description => "Default action on wheel down",
      :def_value   => "previous"
    },
    {
      :name        => "format_string",
      :type        => "string",
      :description => "Output format string",
      :def_value   => "%note%%artist% - %album%"
    }
  ]
end
