structure Rational = 
struct
    datatype rational = Celo of int | Ulomek of (int * int);

    exception BadRational;

    fun gcd (a : int, b : int) : int =
        if b = 0
        then a
        else if a = b
        then a
        else gcd (b, a mod b);

    fun okrajsaj (Ulomek (s, i)) : rational =
      if i = 0
      then raise BadRational
      else if i = 1
      then Celo s
      else if s = 0
      then Celo 0
      else if (s < 0 andalso i < 0) orelse (s > 0 andalso i < 0)
      then okrajsaj (Ulomek (~s, ~i))
      else let val g = gcd (s, i)
           in if g = 1 
              then Ulomek (s, i)
              else okrajsaj (Ulomek (s div g, i div g))
           end;

    fun makeRational (a : int, b : int) : rational =
      if a = 0
      then Celo 0
      else if b = 0 
      then raise BadRational
      else if gcd (a, b) = b
      then Celo (a div b)
      else okrajsaj (Ulomek (a, b));

    fun neg (r : rational) : rational =
      case r of
        Celo c => Celo (~c)
      | Ulomek (s, i) => okrajsaj (Ulomek (~s, i));

    fun inv (r : rational) : rational =
      case r of
        Celo c => okrajsaj (Ulomek (1, c))
      | Ulomek (s, i) => okrajsaj (Ulomek (i, s));

    fun add (r1 : rational, r2 : rational) : rational =
      case (r1, r2) of
        (Celo c1, Celo c2) => Celo (c1 + c2)
      | (Ulomek (s1, i1), Ulomek (s2, i2)) => okrajsaj (Ulomek ((s1 * i2) + (s2 * i1), i1 * i2))
      | (Celo c, Ulomek (s, i)) => okrajsaj (Ulomek ((c * i) + s, i))
      | (Ulomek (s, i), Celo c) => okrajsaj (Ulomek (s + (c * i), i));

    fun mul (r1 : rational, r2 : rational) : rational =
      case (r1, r2) of
        (Celo c1, Celo c2) => Celo (c1 * c2)
      | (Ulomek (s1, i1), Ulomek (s2, i2)) => okrajsaj (Ulomek (s1 * s2, i1 * i2))
      | (Celo c, Ulomek (s, i)) => okrajsaj (Ulomek (c * s, i))
      | (Ulomek (s, i), Celo c) => okrajsaj (Ulomek (s * c, i));

    fun toString (r : rational) : string =
      case r of
        Celo c => Int.toString c
      | Ulomek (s, i) => let val okrajsan = okrajsaj (Ulomek (s, i))
                         in case okrajsan of
                              Celo c => Int.toString c
                            | Ulomek (s, i) => (Int.toString s) ^  "/" ^ (Int.toString i)
                         end;
end

signature EQ =
sig
    type t
    val eq : t -> t -> bool
end

signature SET =
sig
    (* podatkovni tip za elemente množice *)
    type item

    (* podatkovni tip množico *)
    type set

    (* prazna množica *)
    val empty : set

    (* vrne množico s samo podanim elementom *)
    val singleton : item -> set

    (* unija množic *)
    val union : set -> set -> set

    (* razlika množic (prva - druga) *)
    val difference : set -> set -> set

    (* a je prva množica podmnožica druge *)
    val subset : set -> set -> bool
end

functor SetFn (Eq : EQ) :> SET where type item = Eq.t =
struct
    type item = Eq.t;
    type set = item list ref;

    val empty : set = ref [];
    
    fun singleton (x : item) : set = ref [x];

    fun union (s1 : set) (s2 : set) : set = ref (List.foldl (fn (i, acc) => if (List.exists (fn x => Eq.eq x i) acc) then acc else acc @ [i]) [] ((!s1) @ (!s2)));
    
    fun difference (s1 : set) (s2 : set) : set = ref (List.foldl (fn (i, acc) => if (List.exists (fn x => Eq.eq x i) (!s2)) then acc else acc @ [i]) [] (!s1));
    
    fun subset (s1 : set) (s2 : set) : bool = List.foldl (fn (i, acc) => if acc = false
                                                                            then false
                                                                            else if (List.exists (fn x => Eq.eq x i) (!s2)) 
                                                                            then true 
                                                                            else false) true (!s1);
end
