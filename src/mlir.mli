type context
type mlir_module
type mlir_type
type operation

module Context : sig

    val create : unit -> context

    val equal : context -> context -> bool

    val destroy : context -> unit

end

module Module : sig
  
    val parse : context -> string -> mlir_module

    val destroy : mlir_module -> unit

end

val with_context : context -> (context -> 'a) -> 'a