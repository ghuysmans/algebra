module type Element = sig
  type t
  val id_add : t
  val (+) : t -> t -> t
  val id_prod : t
  val ( * ) : t -> t -> t
end

module type S = sig
  type element
  include Semigroup.S with type t = element array array
  val zero : int -> int -> t
  val id : int -> t
  val of_array : [< `Column | `Row] -> element array -> t
end

module Make : functor (E : Element) -> S with type element := E.t

module I : S with type element := int
module F : S with type element := float
module C : S with type element := Complex.t
