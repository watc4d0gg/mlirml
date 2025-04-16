module Types = Types

module Bindings (F : Cstubs.FOREIGN) = struct
  include Support.Bindings (F)
  include Ir.Bindings (F)
end