val _ = print "------- next -------";
val _ : int -> int = next;
val test1_1 = next 2 = 3;
val test1_2 = next 0 = 1;
val test1_3 = next 1000 = 1001;
val test1_4 = next ~15 = ~14;

val _ = print "------- add -------";
val _ : int * int -> int = add;
val test2_1 = add(2, 1) = 3;
val test2_2 = add(0, 0) = 0;
val test2_3 = add(~15, 4) = ~11;
val test2_4 = add(~8, ~2) = ~10;

val _ = print "------- majority -------";
val _ : bool * bool * bool -> bool = majority;
val test3_1 = majority(true, true, true) = true;
val test3_2 = majority(true, false, true) = true;
val test3_3 = majority(true, false, false) = false;
val test3_4 = majority(false, false, false) = false;

val _ = print "------- median -------";
val _ : real * real * real -> real = median;
val test4_1 = Real.== (median(5.0, 6.0, 7.0), 6.0);
val test4_2 = Real.== (median(25.0, 22.5, 115.0), 25.0);
val test4_3 = Real.== (median(~15.0, ~6.0, ~7.0), ~7.0);
val test4_4 = Real.== (median(0.0, 0.0, 0.0), 0.0);

val _ = print "------- triangle -------";
val _ : int * int * int -> bool = triangle;
val test5_1 = triangle(1, 2, 3) = false;
val test5_2 = triangle(15, 15, 15) = true;
val test5_3 = triangle(5, 4, 3) = true;
val test5_4 = triangle(100, 400, 300) = false;
val test5_5 = triangle(0, 0, 0) = false;