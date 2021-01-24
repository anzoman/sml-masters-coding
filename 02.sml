datatype number = Zero | Succ of number | Pred of number;

(* Število pretvori v najbolj ekonomično. *)

fun simp (a : number) : number = 
  case a of
    Zero => Zero
  | Succ b => (case simp b of
                Pred c => c
              | c => Succ c)
  | Pred b => (case simp b of
                Succ c => c
              | c => Pred c);

(* Negira število a. Pretvorba v int ni dovoljena! *)
fun neg (a : number) : number =
  case a of 
    Zero => a
  | Succ i => simp (Pred (neg i))
  | Pred j => simp (Succ (neg j));

(* Vrne vsoto števil a in b. Pretvorba v int ni dovoljena! *)
fun add (a : number, b : number) : number =
  case (simp a, simp b) of 
    (Zero, Zero) => Zero
  | (Zero, Succ i) => Succ i
  | (Zero, Pred i) => Pred i
  | (Succ i, Zero) => Succ i
  | (Pred i, Zero) => Pred i
  | (Succ i, Succ j) => simp (Succ (Succ (add (i, j))))
  | (Succ i, Pred j) => add (i, j)
  | (Pred i, Succ j) => add (i, j)
  | (Pred i, Pred j) => simp (Pred (Pred (add (i, j))));

(* Vrne rezultat primerjave števil a in b. Pretvorba v int ter uporaba funkcij `add` in `neg` ni dovoljena!
    namig: uporabi funkcijo simp *)
fun comp (a : number, b : number) : order =
  case (simp(a), simp(b)) of 
    (Zero, Zero) => EQUAL
  | (Zero, Succ i) => LESS
  | (Zero, Pred i) => GREATER
  | (Succ i, Zero) => GREATER
  | (Pred i, Zero) => LESS
  | (Succ i, Succ j) =>	comp (i, j)
  | (Succ i, Pred j) => GREATER
  | (Pred i, Succ j) => LESS
  | (Pred i, Pred j) => comp(i, j);

datatype tree = Node of int * tree * tree | Leaf of int;

(* Vrne true, če drevo vsebuje element x. *)
fun contains (tree : tree, x : int) : bool =
  case tree of
    Leaf l => l = x
  | Node (r, t1, t2) => if r = x
                        then true
                        else contains (t1, x) orelse contains (t2, x);

(* Vrne število listov v drevesu. *)
fun countLeaves (tree : tree) : int =
  case tree of
    Leaf l => 1
  | Node (r, t1, t2) => countLeaves t1 + countLeaves t2;

(* Vrne število število vej v drevesu. *)
fun countBranches (tree : tree) : int =
  case tree of
    Leaf l => 0
  | Node (r, t1, t2) => 2 + countBranches t1 + countBranches t2;

(* Vrne višino drevesa. Višina lista je 1. *)
fun height (tree : tree) : int =
  case tree of
    Leaf l => 1
  | Node (r, t1, t2) => Int.max(1 + height t1, 1 + height t2);

(* Pretvori drevo v seznam z vmesnim prehodom (in-order traversal). *)
fun toList (tree : tree) : int list =
  case tree of
    Leaf l => l::nil
  | Node (r, t1, t2) => toList(t1) @ [r] @ toList(t2);

(* Vrne true, če je drevo uravnoteženo:
 * - Obe poddrevesi sta uravnoteženi.
 * - Višini poddreves se razlikujeta kvečjemu za 1.
 * - Listi so uravnoteženi po definiciji.
 *)
fun isBalanced (tree : tree) : bool =
  case tree of
    Leaf l => true
  | Node (r, t1, t2) => abs (height t1 - height t2) <= 1 andalso (isBalanced t1 andalso isBalanced t2);

fun min (tree : tree) : int =
  case tree of
    Leaf n => n
  | Node (n, t1, t2) => Int.min(n, Int.min(min t1, min t2));
  
fun max (tree : tree) : int =
  case tree of
    Leaf n => n
  | Node (n, t1, t2) => Int.max(n, Int.max(min t1, min t2));

(* Vrne true, če je drevo binarno iskalno drevo:
 * - Vrednosti levega poddrevesa so strogo manjši od vrednosti vozlišča.
 * - Vrednosti desnega poddrevesa so strogo večji od vrednosti vozlišča.
 * - Obe poddrevesi sta binarni iskalni drevesi.
 * - Listi so binarna iskalna drevesa po definiciji.
 *)
fun isBST (tree : tree) : bool =
  case tree of
    Leaf l => true
  | Node (r, t1, t2) => max t1 < r andalso min t2 > r andalso (isBST t1 andalso isBST t2);
