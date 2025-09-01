let rec f x = if x > 0 then g (x - 1) else 0
and g x = if x > 0 then f (x - 1) else 1

let () = print_int (f 5)