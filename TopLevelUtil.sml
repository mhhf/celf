datatype ('a, 'b) sum = INL of 'a | INR of 'b
infixr 1 $
fun f $ x = f x
(* val map1 : ('a -> 'c) -> 'a * 'b -> 'c * 'b
 * val map2 : ('b -> 'd) -> 'a * 'b -> 'a * 'd
 * val map12 : ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd
 *)
fun map1 f (a, b) = (f a, b)
fun map2 f (a, b) = (a, f b)
fun map12 f g (a, b) = (f a, g b)

signature TOP_LEVEL_UTIL = sig end
