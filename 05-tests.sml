Control.Print.printDepth := 100;
Control.Print.printLength := 1000;
Control.Print.stringDepth := 1000;
val _ = Control.polyEqWarn := false;

open Rational;

val _ = print "------- makeRational -------";
val _ : int * int -> rational = makeRational;
makeRational (0, 0) = Celo 0;
makeRational (0, 71) = Celo 0;
(makeRational (15, 0) handle BadRational => Celo 0) = Celo 0;
makeRational (5, 5) = Celo 1;
makeRational (15, 5) = Celo 3;
makeRational (15, ~5) = Celo (~3);
makeRational (5, 2) = Ulomek (5, 2);
makeRational (18, 4) = Ulomek (9, 2);
makeRational (18, ~4) = Ulomek (~9, 2);
makeRational (~18, 4) = Ulomek (~9, 2);
makeRational (~18, ~4) = Ulomek (9, 2);

val _ = print "------- neg -------";
val _ : rational -> rational = neg;
neg (Celo 0) = Celo 0;
neg (Celo 4) = Celo (~4);
neg (Celo (~2)) = Celo (2);
neg (Ulomek (5, 2)) = Ulomek (~5, 2);
neg (Ulomek (~2, 6)) = Ulomek (1, 3);

val _ = print "------- inv -------";
val _ : rational -> rational = inv;
(inv (Celo 0) handle BadRational => Celo 0) = Celo 0;
inv (Celo 4) = Ulomek (1, 4);
inv (Celo ~4) = Ulomek (~1, 4);
inv (Ulomek (5, 2)) = Ulomek (2, 5);
inv (Ulomek (2, 6)) = Celo 3;

val _ = print "------- add -------";
val _ : rational * rational -> rational = add;
add (Celo 4, Celo 4) = Celo 8;
add (Celo 1, Celo ~4) = Celo (~3);
add (Celo 1, Ulomek (1, 2)) = Ulomek (3, 2);
add (Ulomek (~1, 2), Celo 2) = Ulomek (3, 2);
add (Ulomek (1, 2), Ulomek (1, 4)) = Ulomek (3, 4);

val _ = print "------- mul -------";
val _ : rational * rational -> rational = add;
mul (Celo 4, Celo 4) = Celo 16;
mul (Celo 1, Celo ~4) = Celo (~4);
mul (Celo 2, Ulomek (1, 2)) = Celo 1;
mul (Ulomek (~5, 4), Celo 2) = Ulomek (~5, 2);
mul (Ulomek (3, 2), Ulomek (1, 4)) = Ulomek (3, 8);

val _ = print "------- toString -------";
val _ : rational -> string = toString;
toString (Celo 0) = "0";
toString (Celo ~4) = "~4";
toString (Ulomek (5, 2)) = "5/2";
toString (Ulomek (2, ~6)) = "~1/3";

val _ = print "------- testFunctor -------";
structure S = SetFn (type t = int fun eq x y = x = y);
val s1 = S.singleton 1;
val s2 = S.singleton 2;
val s3 = S.singleton 3;
val s4 = S.singleton 4;
S.subset s4 (S.union s3 s4) = true;
S.subset s3 (S.union s3 s4) = true;
S.subset s1 (S.union s3 s4) = false;
S.subset s1 (S.difference (S.union s2 s3) ((S.union (S.union s1 s3) s4))) = false;
S.subset s2 (S.difference (S.union s2 s3) ((S.union (S.union s1 s3) s4))) = true;
S.subset s3 (S.difference (S.union s2 s3) ((S.union (S.union s1 s3) s4))) = false;
S.subset s4 (S.difference (S.union s2 s3) ((S.union (S.union s1 s3) s4))) = false;