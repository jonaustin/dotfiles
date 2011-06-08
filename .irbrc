#!/usr/bin/ruby
require 'wirble'
Wirble.init
Wirble.colorize
require 'awesome_print'
require 'interactive_editor'

# i.e. rh 'collect'
def rh(*args); system('ri', *args); end

def show_log
  change_log(STDOUT)
end

def hide_log
  change_log(nil)
end

def change_log(stream, colorize=true)
  ActiveRecord::Base.logger = ::Logger.new(stream)
  ActiveRecord::Base.clear_all_connections!
  ActiveRecord::Base.colorize_logging = colorize
end

def execute sql
  ActiveRecord::Base.connection.execute(sql)
end


def clear
  for n in 0..65 do
    puts
  end
end

class Object
  # list methods which aren't in superclass
  def local_methods(obj = self)
    (obj.methods - obj.class.superclass.instance_methods).sort
  end

  # print documentation
  #
  #   ri 'Array#pop'
  #   Array.ri
  #   Array.ri :pop
  #   arr.ri :pop
  def ri(method = nil)
    unless method && method =~ /^[A-Z]/ # if class isn't specified
      klass = self.kind_of?(Class) ? name : self.class.name
      method = [klass, method].compact.join('#')
    end
    puts `ri '#{method}'`
  end
end

# Get SQL output in console
if ENV['RAILS_ENV']
  # Called after the irb session is initialized and Rails has been loaded
  IRB.conf[:IRB_RC] = Proc.new do
    logger = Logger.new(STDOUT)
    ActiveRecord::Base.logger = logger
    ActiveResource::Base.logger = logger
  end
end

#require 'irbtools'
#require 'irbtools/more' # the /more just provides bond and drx (which doesn't work so far)
# bond/yard looks cool, but doesn't seem capable of much yet:
# http://tagaholic.me/2010/05/19/documentation-generated-irb-autocompletions-with-bond-and-yard.html
#require 'bond/yard'
#Bond.load_yard_gems 'yard'
