let rec pow f x = function
  | 0 -> x
  | n -> pow f (f x) (n - 1)
