#!/usr/bin/ruby
require 'wirble'
Wirble.init
Wirble.colorize
require 'awesome_print'
require 'interactive_editor'

# i.e. rh 'collect'
def rh(*args); system('ri', *args); end

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


#require 'irbtools'
#require 'irbtools/more' # the /more just provides bond and drx (which doesn't work so far)
# bond/yard looks cool, but doesn't seem capable of much yet:
# http://tagaholic.me/2010/05/19/documentation-generated-irb-autocompletions-with-bond-and-yard.html
#require 'bond/yard'
#Bond.load_yard_gems 'yard'
