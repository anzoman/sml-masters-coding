val _ = print "------- factorial -------";
val _ : int -> int = factorial;
val test1_1 = factorial 0 = 1;
val test1_2 = factorial 1 = 1;
val test1_3 = factorial 2 = 2;
val test1_4 = factorial 3 = 6;
val test1_5 = factorial 5 = 120;
val test1_6 = factorial 10 = 3628800;

val _ = print "------- power -------";
val _ : int * int -> int = power;
val test2_1 = power (0, 0) = 1;
val test2_2 = power (0, 5) = 0;
val test2_3 = power (3, 0) = 1;
val test2_4 = power (1, 1) = 1;
val test2_5 = power (3, 2) = 9;
val test2_6 = power (5, 3) = 125;
val test2_7 = power (2, 10) = 1024;

val _ = print "------- gcd -------";
val _ : int * int -> int = gcd;
val test3_1 = gcd (0, 0) = 0;
val test3_2 = gcd (10, 0) = 10;
val test3_3 = gcd (5, 5) = 5;
val test3_4 = gcd (12, 8) = 4;
val test3_5 = gcd (54, 24) = 6;
val test3_6 = gcd (180, 150) = 30;
val test3_7 = gcd (58961, 1259) = 1;

val _ = print "------- len -------";
val _ : int list -> int = len;
val test4_1 = len [] = 0;
val test4_2 = len nil = 0;
val test4_3 = len [1] = 1;
val test4_4 = len [1, 5] = 2;
val test4_5 = len [10, 2, 50, 88] = 4;
val test4_6 = len [13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1] = 13;

val _ = print "------- nth -------";
val _ : int list * int -> int option = nth;
val test5_1 = nth ([], 0) = NONE;
val test5_2 = nth (nil, 10) = NONE;
val test5_3 = nth ([1, 5], ~1) = NONE;
val test5_4 = nth ([1, 2], 3) = NONE;
val test5_5 = nth ([10, 2, 50, 88], 0) = SOME 10;
val test5_6 = nth ([13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1], 5) = SOME 8;

val _ = print "------- last -------";
val _ : int list -> int option = last;
val test6_1 = last [] = NONE;
val test6_2 = last nil = NONE;
val test6_3 = last [1] = SOME 1;
val test6_4 = last [1, 2] = SOME 2;
val test6_5 = last [10, 2, 50, 88] = SOME 88;
val test6_6 = last [13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1] = SOME 1;

val _ = print "------- insert -------";
val _ : int list * int * int -> int list = insert;
val test7_1 = insert ([], 0, 10) = [10];
val test7_2 = insert ([1], 0, 5) = [5, 1];
val test7_3 = insert ([1], 2, 5) = [1, 5];
val test7_4 = insert ([1, 3], 1, 2) = [1, 2, 3];
val test7_5 = insert ([10, 2, 50, 88], 4, 42) = [10, 2, 50, 88, 42];
val test7_6 = insert ([13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1], 5, ~1) = [13, 12, 11, 10, 9, ~1, 8, 7, 6, 5, 4, 3, 2, 1];

val _ = print "------- delete -------";
val _ : int list * int -> int list = delete;
val test8_1 = delete ([], 5) = [];
val test8_2 = delete ([1], 1) = [];
val test8_3 = delete ([1, 5], 1) = [5];
val test8_4 = delete ([1, 3, 6], 5) = [1, 3, 6];
val test8_5 = delete ([10, 2, 10, 3, 10, 4], 10) = [2, 3, 4];
val test8_6 = delete ([1, 1, 1, 10, 9, 1, 8, 7, 6, 1, 5, 1, 1, 1, 4, 3, 2, 1, 1], 1) = [10, 9, 8, 7, 6, 5, 4, 3, 2];

val _ = print "------- reverse -------";
val _ : int list -> int list = reverse;
val test9_1 = reverse [] = [];
val test9_2 = reverse nil = nil;
val test9_3 = reverse [1] = [1];
val test9_4 = reverse [1, 2] = [2, 1];
val test9_5 = reverse [10, 2, 50, 88] = [88, 50, 2, 10];
val test9_6 = reverse [13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13];

val _ = print "------- palindrome -------";
val _ : int list -> int list = reverse;
val test10_1 = palindrome [] = true;
val test10_2 = palindrome nil = true;
val test10_3 = palindrome [1] = true;
val test10_4 = palindrome [1, 2] = false;
val test10_5 = palindrome [1, 2, 1] = true;
val test10_6 = palindrome [1, 2, 3, 2, 1] = true;
val test10_7 = palindrome [13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1] = false;
