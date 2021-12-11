module type Element = sig
  type t
  val id_add : t
  val (+) : t -> t -> t
  val id_prod : t
  val ( * ) : t -> t -> t
end

module Make : functor (E : Element) -> sig
  include Semigroup.S with type t = E.t array array
  val zero : int -> int -> t
  val id : int -> int -> t
  val of_array : [< `Column | `Row] -> E.t array -> t
end
