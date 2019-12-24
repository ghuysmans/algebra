module type S = sig
  type t
  val equal: t -> t -> bool

  (** {@} must be associative. *)
  val (@): t -> t -> t
end

(** Imperative signature *)
module type I = sig
  include S

  val copy: t -> t
end

module Direct_product = struct
  module S (A: S) (B: S) : S with type t = A.t * B.t = struct
    type t = A.t * B.t
    let equal (a, b) (a', b') = A.equal a a' && B.equal b b'
    let (@) (a, b) (a', b') = A.(a @ a'), B.(b @ b')
  end

  module I (A: I) (B: I) : I with type t = A.t * B.t = struct
    include S (A) (B)
    let copy (a, b) = A.copy a, B.copy b
  end
end
