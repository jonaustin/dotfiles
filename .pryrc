# vim: ft=ruby

Pry.config.prompt = Pry::NAV_PROMPT

# http://cubiclemuses.com/cm/articles/2011/12/19/pry-ruby-open/
# reload <file>
Pry.config.commands.import(Pry::CommandSet.new do
  ## For quick loading.
  $wl__reload_file = nil

  command "reload", "Reload specified source file or previously reloaded file by default." do |file|
    unless file
      if $wl__reload_file
        load($wl__reload_file)
        next
      end

      files = Dir["*.rb"]
      case files.size
      when 0
        output.puts "No Ruby files in #{Dir.pwd}"
        next
      when 1
        file = files.first
      else
        output.puts "Many Ruby files in #{Dir.pwd}:"
        output.puts files.map{|f| "\t#{f}"}
        next
      end
    end

    file = file.sub(/(\.rb)?$/, '.rb')
    $wl__reload_file = file
    load(file)
  end

  alias_command "r", "reload"
end)


## Benchmarking
# Inspired by <http://stackoverflow.com/questions/123494/whats-your-favourite-irb-trick/123834#123834>.
def time(repetitions = 100, &block)
  require 'benchmark'
  Benchmark.bm{|b| b.report{repetitions.times(&block)}}
end

class Object
  # list methods which aren't in superclass
  def local_methods(obj = self)
    (obj.methods - obj.class.superclass.instance_methods).sort
  end
end

# eh..I don't recall what this does..
#begin
  #require 'awesome_print'
  #Pry.config.print = proc {|output, value| Pry::Helpers::BaseHelpers.stagger_output("=> #{value.ai}", output)}
  ##Pry.config.print = proc { |output, value| output.send(:ap, value) }

  #require 'hirb'
  ## Dirty hack to support in-session Hirb.disable/enable
  #Hirb::View.instance_eval do
    #def enable_output_method
      #@output_method = true
      #Pry.config.print = proc do |output, value|
        #Hirb::View.view_or_page_output(value) || output.send(:ap, value)
      #end
    #end

    #def disable_output_method
      #Pry.config.print = proc { |output, value| output.send(:ap, value) }
      #@output_method = nil
    #end
  #end
  #Hirb.enable
#rescue LoadError
  ## Missing some goodies, bummer
#end

# for pry-debugger
Pry.commands.alias_command 'c', 'continue'
Pry.commands.alias_command 's', 'step'
Pry.commands.alias_command 'n', 'next'
Pry.commands.alias_command 'f', 'finish' # byebug only (I think)

# Hit Enter to repeat last command
Pry::Commands.command /^$/, "repeat last command" do
  _pry_.run_command Pry.history.to_a.last
end

# Use pry-editline (tpope) if available (this'll make it work even if its not in Gemfile)
Gem.path.each do |gemset|
  $:.concat(Dir.glob("#{gemset}/gems/pry-*/lib"))
  $:.concat(Dir.glob("#{gemset}/gems/yard-*/lib"))
end if defined?(Bundler)
$:.uniq!
#require 'pry-editline'

require 'ostruct' # must be required before awesome_print, otherwise openstruct support will not be loaded
require "awesome_print"
AwesomePrint.pry!

pryrc_local = "#{ENV['HOME']}/.pryrc.local"
load pryrc_local if File.exist?(pryrc_local)

require 'looksee' # irb> [].ls
