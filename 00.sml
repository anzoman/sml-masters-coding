(* Vrne naslednika števila n. *)
fun next (n : int) : int = n + 1;

(* Vrne vsoto števil a in b. *)
fun add (a : int, b : int) : int = a + b;

(* Vrne true, če sta vsaj dva argumenta true, drugače vrne false *)
fun majority (a : bool, b : bool, c : bool) : bool =
    if a = true then (b orelse c) else (b andalso c);

(* Vrne mediano argumentov - števila tipa real brez (inf : real), (~inf : real), (nan : real) in (~0.0 : real) *)
fun median (a : real, b : real, c : real) : real =
    Real.max(Real.min(a, b), Real.max(Real.min(a, c), Real.min(b, c)));

(* Preveri ali so argumenti veljavne dolžine stranic nekega trikotnika - trikotnik ni izrojen *)
fun triangle (a : int, b : int, c : int)  : bool =
    if a + b > c andalso a + c > b andalso b + c > a
    then true
    else false;
