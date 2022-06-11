#!/usr/bin/ruby
# require 'looksee' # irb> [].ls
#
# def unbundled_require(gem)
#   loaded = false
#   if defined?(::Bundler)
#     Gem.path.each do |gems_path|
#       gem_path = Dir.glob("#{gems_path}/gems/#{gem}*").last
#       unless gem_path.nil?
#         $LOAD_PATH << "#{gem_path}/lib"
#         require gem
#         loaded = true
#       end
#     end
#   else
#     require gem
#     loaded = true
#   end
#   raise(LoadError, "couldn't find #{gem}") unless loaded
#   loaded
# end
#
# def load_gem(gem, &block)
#   begin
#     if unbundled_require gem
#       yield if block_given?
#     end
#   rescue Exception => err
#     warn "Couldn't load #{gem}: #{err}"
#   end
#
# end
#
# # Highlighting and other features
# #load_gem 'wirble' do
#   #Wirble.init
#   #Wirble.colorize
# #end
#
# # Improved formatting for objects
# #load_gem 'awesome_print'
# require 'ostruct' # must be required before awesome_print, otherwise openstruct support will not be loaded
# require "awesome_print"
# AwesomePrint.irb!
#
# # i.e. rh 'collect'
# def rh(*args); system('ri', *args); end
#
# def show_log
#   change_log(STDOUT)
# end
#
# def hide_log
#   change_log(nil)
# end
#
# def change_log(stream, colorize=true)
#   ActiveRecord::Base.logger = ::Logger.new(stream)
#   ActiveRecord::Base.clear_all_connections!
#   ActiveRecord::Base.colorize_logging = colorize
# end
#
# def execute sql
#   ActiveRecord::Base.connection.execute(sql)
# end
#
#
# def clear
#   for n in 0..65 do
#     puts
#   end
# end
#
# class Object
#   # list methods which aren't in superclass
#   def local_methods(obj = self)
#     (obj.methods - obj.class.superclass.instance_methods).sort
#   end
#
#   # print documentation
#   #
#   #   ri 'Array#pop'
#   #   Array.ri
#   #   Array.ri :pop
#   #   arr.ri :pop
#   def ri(method = nil)
#     unless method && method =~ /^[A-Z]/ # if class isn't specified
#       klass = self.kind_of?(Class) ? name : self.class.name
#       method = [klass, method].compact.join('#')
#     end
#     puts `ri '#{method}'`
#   end
# end
#
# # Get SQL output in console
# #if ENV['RAILS_ENV']
#   ## Called after the irb session is initialized and Rails has been loaded
#   #IRB.conf[:IRB_RC] = Proc.new do
#     #logger = Logger.new(STDOUT)
#     #ActiveRecord::Base.logger = logger
#     #ActiveResource::Base.logger = logger
#   #end
# #end
