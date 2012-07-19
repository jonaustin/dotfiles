# vim: ft=ruby
# Load plugins (only those I whitelist)
#Pry.config.should_load_plugins = false
#Pry.plugins['nav'].activate!

#Pry.commands.alias_command 'c', 'continue'
#Pry.commands.alias_command 's', 'step'
#Pry.commands.alias_command 'n', 'next'



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


# Launch Pry with access to the entire Rails stack.
# If you have Pry in your Gemfile, you can pass: ./script/console --irb=pry instead.
# If you don't, you can load it through the lines below :)
rails = File.join Dir.getwd, 'config', 'environment.rb'

if File.exist?(rails) && ENV['SKIP_RAILS'].nil?
  require rails

  if Rails.version[0..0] == "2"
    require 'console_app'
    require 'console_with_helpers'
  elsif Rails.version[0..0] == "3"
    require 'rails/console/app'
    require 'rails/console/helpers'
  else
    warn "[WARN] cannot load Rails console commands (Not on Rails2 or Rails3?)"
  end

  # always display activerecord sql on stdout
  ActiveRecord::Base.logger = Logger.new(STDOUT)
end

class Object
  # list methods which aren't in superclass
  def local_methods(obj = self)
    (obj.methods - obj.class.superclass.instance_methods).sort
  end
end

begin
  require 'awesome_print'
  Pry.config.print = proc {|output, value| Pry::Helpers::BaseHelpers.stagger_output("=> #{value.ai}", output)}
  #Pry.config.print = proc { |output, value| output.send(:ap, value) }

  require 'hirb'
  # Dirty hack to support in-session Hirb.disable/enable
  Hirb::View.instance_eval do
    def enable_output_method
      @output_method = true
      Pry.config.print = proc do |output, value|
        Hirb::View.view_or_page_output(value) || output.send(:ap, value)
      end
    end

    def disable_output_method
      Pry.config.print = proc { |output, value| output.send(:ap, value) }
      @output_method = nil
    end
  end
  Hirb.enable
rescue LoadError
  # Missing some goodies, bummer
end
