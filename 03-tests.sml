val _ = print "------- zip -------";
val _ : 'a list * 'b list -> ('a * 'b) list = zip;
val test_zip_1 = zip ([], []) = [];
val test_zip_2 = zip ([], [1, 3, 5]) = [];
val test_zip_3 = zip ([1], []) = [];
val test_zip_4 = zip ([1, 3, 5], [2, 4, 6]) = [(1, 2), (3, 4), (5, 6)];
val test_zip_5 = zip ([1, 1, 1], [2, 3]) = [(1, 2), (1, 3)];
val test_zip_6 = zip ([1], [2, 2, 2]) = [(1, 2)];
val test_zip_7 = zip ([1, 1, 1, 1], [2, 2, 2, 2, 2, 2, 2, 2, 2, 2]) = [(1, 2), (1, 2), (1, 2), (1, 2)];

val _ = print "------- unzip -------";
val _ : ('a * 'b) list -> 'a list * 'b list = unzip;
val test_unzip_1 = unzip [] = ([], []);
val test_unzip_2 = unzip [(1, 2)] = ([1], [2]);
val test_unzip_3 = unzip [(1, 2), (3, 4), (5, 6)] = ([1, 3, 5], [2, 4, 6]);
val test_unzip_4 = unzip [(1, 2), (1, 3)] = ([1, 1], [2, 3]);
val test_unzip_5 = unzip [(1, 2), (1, 2), (1, 2), (1, 2)] = ([1, 1, 1, 1], [2, 2, 2, 2]);

val _ = print "------- subtract  -------";
val _ : natural * natural -> natural = subtract ;
(* val test_subtract_1 = subtract (One, One) = NotNaturalNumber;
val test_subtract_2 = subtract (One, Succ One) = NotNaturalNumber;
val test_subtract_3 = subtract (Succ (Succ One), Succ (Succ One)) = NotNaturalNumber; *)
val test_subtract_4 = subtract (Succ One, One) = One;
val test_subtract_5 = subtract (Succ (Succ One), Succ One) = One; 
val test_subtract_6 = subtract (Succ (Succ (Succ One)), One) = Succ (Succ One);

val _ = print "------- any -------";
val _ : ('a -> bool) * 'a list -> bool = any;
val test_any_1 = any ((fn a => true), []) = false;
val test_any_2 = any ((fn a => true), [1]) = true;
val test_any_3 = any ((fn a => true), [3, 5]) = true;
val test_any_4 = any ((fn a => false), [3, 5]) = false;
val test_any_5 = any ((fn a => if a = true then true else false), [true, true, true, true]) = true;
val test_any_6 = any ((fn a => if a = true then true else false), [true, true, false, true]) = false;
val test_any_7 = any ((fn a => if a mod 2 = 0 then true else false), [2, 4, 56, 1000]) = true;
val test_any_8 = any ((fn a => if a mod 2 = 0 then true else false), [1, 2, 3, 4]) = false;

val _ = print "------- map -------";
val _ : ('a -> 'b) * 'a list -> 'b list = map;
val test_map_1 = map ((fn a => a), []) = [];
val test_map_2 = map ((fn a => a), [1]) = [1];
val test_map_3 = map ((fn a => a), [3, 5, 7]) = [3, 5, 7];
val test_map_4 = map ((fn a => 5), [1, 2]) = [5, 5];
val test_map_5 = map ((fn a => if a = true then false else true), [true, false, true, false]) = [false, true, false, true];
val test_map_6 = map ((fn a => if a mod 2 = 0 then 0 else 1), [2, 4, 56, 1000]) = [0, 0, 0, 0];
val test_map_7 = map ((fn a => if a mod 2 = 0 then 0 else 1), [1, 2, 3, 4]) = [1, 0, 1, 0];
val test_map_8 = map ((fn a => if a mod 2 = 0 then true else false), [1, 2, 3, 4]) = [false, true, false, true];

val _ = print "------- filter -------";
val _ : ('a -> bool) * 'a list -> 'a list = filter;
val test_filter_1 = filter ((fn a => true), []) = [];
val test_filter_2 = filter ((fn a => true), [1]) = [1];
val test_filter_3 = filter ((fn a => true), [3, 5, 7]) = [3, 5, 7];
val test_filter_4 = filter ((fn a => false), [1, 2]) = [];
val test_filter_5 = filter ((fn a => if a = true then true else false), [true, false, true, false]) = [true, true];
val test_filter_6 = filter ((fn a => if a mod 2 = 0 then true else false), [11, 22, 33, 44]) = [22, 44];
val test_filter_7 = filter ((fn a => if a = "a" then false else true), ["a", "b", "c", "d"]) = ["b", "c", "d"];

val _ = print "------- fold -------";
val _ : ('a * 'b -> 'a) * 'a * 'b list -> 'a = fold;
val test_fold_1 = fold ((fn (a, b) => a), 5, []) = 5;
val test_fold_2 = fold ((fn (a, b) => a), "abcd", []) = "abcd";
val test_fold_3 = fold ((fn (a, b) => a + b), 1, [1]) = 2;
val test_fold_4 = fold ((fn (a, b) => a + b), 1, [1, 2]) = 4;
val test_fold_5 = fold ((fn (a, b) => a + b), 1, [1, 2, 3]) = 7;
val test_fold_6 = fold ((fn (a, b) => a + b), 1, [1, 1, 1, 1, 1, 1, 1, 1, 1]) = 10;
val test_fold_7 = fold ((fn (a, b) => a ^ b), "a", ["b", "c", "d"]) = "abcd";

val _ = print "------- rotate -------";
val _ : 'a bstree * direction -> 'a bstree = rotate;
val test_rotate_1 = rotate (lf, L) = lf;
val test_rotate_2 = rotate (lf, R) = lf;
val test_rotate_3 = rotate (br (lf, 10, lf), L) = br (lf, 10, lf);
val test_rotate_4 = rotate (br (lf, 15, lf), R) = br (lf, 15, lf);
val test_rotate_5 = rotate (br (br (lf, 5, lf), 15, lf), R) = br (lf, 5, br (lf, 15, lf));
val test_rotate_6 = rotate (br (br (lf, 5, lf), 15, lf), L) = br (br (lf, 5, lf), 15, lf);
val test_rotate_7 = rotate (br (lf, 15, br (lf, 25, lf)), L) = br (br (lf, 15, lf), 25, lf);
val test_rotate_8 = rotate (br (lf, 15, br (lf, 25, lf)), R) = br (lf, 15, br (lf, 25, lf));
val test_rotate_9 = rotate (br (br (br (lf, 1, lf), 2, br (lf, 3, lf)), 4, br (lf, 5, lf)), R) = br (br (lf, 1, lf), 2, br (br (lf, 3, lf), 4, br (lf, 5, lf)));
val test_rotate_10 = rotate (br (br (br (lf, 1, lf), 2, br (lf, 3, lf)), 4, br (lf, 5, lf)), L) = br (br (br (lf, 1, lf), 2, br (lf, 3, lf)), 4, br (lf, 5, lf));
val test_rotate_11 = rotate (br (br (lf, 1, lf), 2, br (br (lf, 3, lf), 4, br (lf, 5, lf))), L) = br (br (br (lf, 1, lf), 2, br (lf, 3, lf)), 4, br (lf, 5, lf));
val test_rotate_12 = rotate (br (br (lf, 1, lf), 2, br (br (lf, 3, lf), 4, br (lf, 5, lf))), R) = br (br (lf, 1, lf), 2, br (br (lf, 3, lf), 4, br (lf, 5, lf)));

(* izpis daljših izrazov v interpreterju *)
val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;

(* izpis drevesa po nivojih *)
fun showTree (toString : 'a -> string, t : 'a bstree) =
let fun strign_of_avltree_level (lvl, t) = case t of  
        lf => if lvl = 0 then "nil" else "   "
    |   br (l, n, r) =>
        let val make_space = String.map (fn _ => #" ")
            val sn = toString n
            val sl = strign_of_avltree_level (lvl, l)
            val sr = strign_of_avltree_level (lvl, r)
        in if height t = lvl
            then make_space sl ^ sn ^ make_space sr
            else sl ^ make_space sn ^ sr
        end
    fun print_levels lvl =
        if lvl >= 0
        then (print (Int.toString lvl ^ ": " ^ strign_of_avltree_level (lvl, t) ^ "\n");
                    print_levels (lvl - 1))
        else ()
  in  print_levels (height t)
end;

(* primeri vstavljanja elementov v AVL drevo *)
fun avlInt (t, i) = avl (Int.compare, t, i);
fun showTreeInt t = showTree(Int.toString, t);

val tr = lf : int bstree;
val _ = showTreeInt tr;
val tr = avlInt (tr, 1);
val _ = showTreeInt tr;
val tr = avlInt (tr, 2);
val _ = showTreeInt tr;
val tr = avlInt (tr, 3);
val _ = showTreeInt tr;
val tr = avlInt (tr, 4);
val _ = showTreeInt tr;
val tr = avlInt (tr, 5);
val _ = showTreeInt tr;
val tr = avlInt (tr, 6);
val _ = showTreeInt tr;
val tr = avlInt (tr, 7);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~4);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~3);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~2);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~1);
val _ = showTreeInt tr;
val tr = avlInt (tr, 0);
val _ = showTreeInt tr;

val from0to13 = fold (fn (z, x) => avl (Int.compare, z, x), lf, List.tabulate (14, fn i => i));
