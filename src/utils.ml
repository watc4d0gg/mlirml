exception OutOfViewBounds of string

let ( >> ) f g x = f x |> g

module type ListView = sig
  type t
  type elt

  val size : t -> int
  val get : t -> int -> elt
end

module type ArrayView = sig
  include ListView

  val set : t -> int -> elt -> unit
  val replace : t -> elt list -> unit
end

module type DequeView = sig
  include ArrayView

  val append : t -> elt -> unit
  val prepend : t -> elt -> unit
end

module type MapView = sig
  type t
  type value

  val contains : t -> string -> bool
  val lookup : t -> string -> value
  val put : t -> string -> value -> unit
  val remove : t -> string -> bool
end
