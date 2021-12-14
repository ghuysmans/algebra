module type Element = sig
  type t
  val id_add : t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val id_prod : t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val abs : t -> float
end

module type S = sig
  type element
  include Semigroup.S with type t = element array array
  val zero : int -> int -> t
  val id : int -> t
  val of_array : [< `Column | `Row] -> element array -> t
end

module type Invertible = sig
  include S
  val inv : t -> t
end

module Make : functor (E : Element) -> Invertible with type element := E.t

module I : S with type element := int
module F : Invertible with type element := float
module C : Invertible with type element := Complex.t
