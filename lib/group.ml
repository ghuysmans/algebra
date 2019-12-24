module type S = sig
  include Monoid.S
  val inv: t -> t
end

module type I = sig
  include Monoid.I
  val inv: t -> t
end

module Direct_product = struct
  module S (A: S) (B: S) : S with type t = A.t * B.t = struct
    include Monoid.Direct_product.S (A) (B)
    let inv (a, b) = A.inv a, B.inv b
  end

  module I (A: I) (B: I) : I with type t = A.t * B.t = struct
    include Monoid.Direct_product.I (A) (B)
    let inv (a, b) = A.inv a, B.inv b
  end
end
