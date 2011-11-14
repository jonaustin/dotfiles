# vim: ft=ruby

# always display activerecord sql on stdout
ActiveRecord::Base.logger = Logger.new(STDOUT)

def sql(a)
  ActiveRecord::Base.connection.execute(a).fetch_hash
end

begin
  require 'awesome_print'
  Pry.config.print = proc { |output, value| output.send(:ap, value) }

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

#begin
  #require 'awesome_print'
  #Pry.config.print = proc { |output, value| Pry::Helpers::BaseHelpers.stagger_output("=> #{value.ai}", output) }
#rescue LoadError => err
  #puts "no awesome_print :("
#end
