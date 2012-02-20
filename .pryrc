# vim: ft=ruby
# Load plugins (only those I whitelist)
#Pry.config.should_load_plugins = false
#Pry.plugins["doc"].activate!

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
