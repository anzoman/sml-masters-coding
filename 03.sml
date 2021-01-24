(* 

val zip = fn : 'a list * 'b list -> ('a * 'b) list
val unzip = fn : ('a * 'b) list -> 'a list * 'b list
val subtract = fn : natural * natural -> natural
val any = fn : ('a -> bool) * 'a list -> bool
val map = fn : ('a -> 'b) * 'a list -> 'b list
val filter = fn : ('a -> bool) * 'a list -> 'a list
val fold = fn : ('a * 'b -> 'a) * 'a * 'b list -> 'a
val rotate = fn : 'a bstree * direction -> 'a bstree
val rebalance = fn : 'a bstree -> 'a bstree
val avl = fn : ('a * 'a -> order) * 'a bstree * 'a -> 'a bstree 

*)

datatype natural = Succ of natural | One;
exception NotNaturalNumber;

datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

fun zip (x: 'a list, y: 'b list) : ('a * 'b) list = 
  let
    fun helper(x: 'a list, y: 'b list, acc : ('a * 'b) list) =
      if null x orelse null y
      then acc
      else helper(tl x, tl y, acc @ [(hd x, hd y)])
  in
    helper(x, y, [])
  end;

fun unzip (xy : ('a * 'b) list) : 'a list * 'b list =
  let
    fun helper(xy : ('a * 'b) list, acc : 'a list * 'b list) =
      if null xy
      then acc
      else helper(tl xy, (#1 acc @ [#1 (hd xy)], #2 acc @ [#2 (hd xy)]))
  in
    helper(xy, ([], []))
  end;

fun subtract (a : natural, b : natural) : natural =
  case (a, b) of
    (One, One) => raise NotNaturalNumber
  | (One, Succ y) => raise NotNaturalNumber
  | (Succ x, One) => x
  | (Succ x, Succ y) => subtract (x, y);

fun any (f : ('a -> bool), s : 'a list) : bool =
  let
    fun helper (f : ('a -> bool), s : 'a list, acc : bool) : bool =
      case s of
        [] => acc
      | glava::rep => (if f glava = true
                      then helper (f, rep, true)
                      else false);
  in
    helper (f, s, false)
  end;

(* fun map (f : ('a -> 'b), s : 'a list) : 'b list =
  let
    fun helper (f : ('a -> 'b), s : 'a list, acc : 'b list) : 'b list =
      case s of
        [] => acc
      | glava::rep => helper (f, rep, acc @ [(f glava)])
  in
    helper (f, s, [])
  end;

fun filter (f : ('a -> bool), s : 'a list) : 'a list  =
  let
    fun helper (f : ('a -> bool), s : 'a list, acc : 'a list) : 'a list =
      case s of
        [] => acc
      | glava::rep => (if f glava = true
                      then helper (f, rep, acc @ [glava])
                      else helper (f, rep, acc))
  in
    helper (f, s, [])
  end; *)

fun fold (f : ('a * 'b -> 'a), z : 'a, s: 'b list) : 'a  =
  case s of
    [] => z
  | glava::rep => fold (f, f (z, glava), rep)

fun foldr (f : ('a * 'b -> 'a), z : 'a, s: 'b list) : 'a  =
  fold (f, z, fold (fn (z, x) => x :: z, [], s))

fun map (f : ('a -> 'b), s : 'a list) : 'b list =
  foldr (fn (z, x) => f x :: z, [], s);

fun filter (f : ('a -> bool), s : 'a list) : 'a list  =
  foldr (fn (z, x) => if f x then x :: z else z, [], s)

fun height lf = 1 | height (br (l, _, r)) = 1 + Int.max (height l, height r);

fun imbalance lf = 0 | imbalance (br (l, _, r)) = height l - height r;

fun rotateLeft (drevo : 'a bstree) : 'a bstree =
  case drevo of 
    lf => lf
  | br (l, e, r) => (case r of
                      lf => drevo
                    | br (lr, er, rr) => br (br (l, e, lr), er, rr));

fun rotateRight (drevo : 'a bstree) : 'a bstree =
  case drevo of 
    lf => lf
  | br (l, e, r) => (case l of
                      lf => drevo
                    | br (ll, el, rl) => br (ll, el, br (rl, e, r)));

fun rotate (drevo : 'a bstree, smer : direction) : 'a bstree =
  if abs(imbalance drevo) = 0
  then drevo
  else (case (smer) of 
          L => if imbalance drevo < 0 then rotateLeft drevo else drevo
        | R => if imbalance drevo > 0 then rotateRight drevo else drevo);

fun rebalance (drevo : 'a bstree) : 'a bstree  =
  case drevo of
    lf => lf
  | br (l, el, r) => (if imbalance drevo < ~1
                      then (if imbalance l = ~1
                            then rotate (drevo, R)
                            else rotate (rotate (drevo, L), R))
                      else if imbalance drevo > 1
                      then (if imbalance r = 1
                            then rotate (drevo, L)
                            else rotate (rotate (drevo, R), L))
                      else drevo);

fun avl (c : ('a * 'a -> order), drevo : 'a bstree,  e : 'a) : 'a bstree  =
  case drevo of
    lf => br (lf, e, lf)
  | br (l, el, r) => (case c (e, el) of
                       LESS => rebalance (br (avl (c, l, e), el, r))
                     | EQUAL => drevo
                     | GREATER => rebalance (br (l, el, avl (c, r, e))));
