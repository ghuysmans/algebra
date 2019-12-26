let rec gcd a b =
  if b = 0 then
    a
  else
    gcd b (a mod b)

let lcm a b = a * b / gcd a b
