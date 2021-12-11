module type S = sig
  include Semigroup.S

  (** {id} is the (unique) identity element. *)
  val id: t
end

module type I = sig
  include Semigroup.I

  (** {make ()} creates an identity value. *)
  val make: unit -> t
end

module Direct_product = struct
  module S (A: S) (B: S) : S with type t = A.t * B.t = struct
    include Semigroup.Direct_product.S (A) (B)
    let id = A.id, B.id
  end

  module I (A: I) (B: I) : I with type t = A.t * B.t = struct
    include Semigroup.Direct_product.I (A) (B)
    let make () = A.make (), B.make ()
  end
end

module Tools = struct
  module S (M : S) = struct
    module T = Semigroup.Tools.S (M)
    open M

    let fastpow a ~b =
      if b = 0 then
        id
      else
        T.fastpow a ~b

    let order t =
      let rec f u n =
        if equal u id then
          n
        else
          f (t @ u) (n + 1)
      in
      f t 0
  end

  module I (M : I) = struct
    module T = Semigroup.Tools.I (M)
    open M

    let fastpow a ~b =
      if b = 0 then
        make ()
      else
        T.fastpow a ~b

    let order t =
      let id = make () in
      let rec f u n =
        if equal u id then
          n
        else
          f (t @ u) (n + 1)
      in
      f t 0
  end
end
