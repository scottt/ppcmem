type num = int
let (<) = (<)
let (<=) = (<=)
let (>) = (>)
let (>=) = (>=)
let (+) = (+)
let (-) x y =
  let d = x - y in
    if d < 0 then
      0
    else
      d
let ( * ) = ( * )
let (/) = (/)

