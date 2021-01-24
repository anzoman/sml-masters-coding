val _ = print "------- Running tests... -------";

val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;
val _ = Control.polyEqWarn := false;

val all_tests : bool list ref = ref [];

val _ = print "------- Variables -------";
val exp1 = (Eq [True, Eq [False, Var 3, And [And [],  Or [Var 1, Not (Eq [Var 4, False, True])], Imp (True, Var 2)]]]);
val exp2 = And [Not (Var 3), Not (Var 2)];
val exp3 = And [Not (Var 1), Not (Var 2)];
val exp4 = Eq [True, Eq [False, Var 3, And [And [Var 2], Eq [Var 0], Or [Var 6, Var 3], Or [Var 1, Not (Eq [Var 4, False, True])], Imp (Eq [Or []], Var 8)]]];
val exp5 = Eq [True, Eq [False, Var 2, And [And [Var 2], Eq [Var 0], Or [Var 6, Var 3], Or [Var 1, Not (Eq [Var 2, False, True])], Imp (Eq [Or []], Var 2)]]];
val exp6 = (And [Or [Not (Var 1), Not (Var 5), Var 7, Var 7], Or [Var 1, Var 4, Not (Var 7)], Or [Not (Var 1), Var 6, Not (Var 7), Var 7, Var 7], Or [Var 2, Var 5, Not (Var 7), Var 7], Or [Not (Var 4), Var 7, Var 7, Var 7], Or [Var 7, Var 7, Var 7, Not (Var 7)], Or [Var 2, Not (Var 7), Var 7], Or [Not (Var 3), Not (Var 1), Var 7, Var 7], Or [Var 5, Var 4, Not (Var 7)], Or [Var 3, Var 6, Not (Var 7), Var 7], Or [Var 3, Var 1, Var 5, Not (Var 7)], Or [Var 2, Var 5, Not (Var 7)], Or [Not (Var 1), Not (Var 4), Var 7], Or [Not (Var 2), Not (Var 4), Var 7, Var 7], Or [Not (Var 2), Not (Var 5), Not (Var 7), Var 7], Or [Not (Var 3), Not (Var 6), Not (Var 7), Not (Var 7)], Or [Var 3, Var 2, Not (Var 7)], Or [Var 5, Var 6], Or [Not (Var 2), Not (Var 5), Var 7, Var 7, Var 7], Or [Var 3, Var 6], Or [Var 1, Var 5, Var 4, Not (Var 7)], Or [Not (Var 3), Not (Var 5), Var 7, Not (Var 7), Not (Var 7)], Or [Not (Var 3), Var 1, Not (Var 5), Not (Var 7), Not (Var 7)], Or [Var 2, Var 6], Or [Var 1, Not (Var 7)], Or [Not (Var 6), Not (Var 5), Not (Var 7), Not (Var 7)], Or [Var 5, Not (Var 7), Var 7], Or [Not (Var 2), Var 5, Var 7, Not (Var 7)], Or [Not (Var 6), Not (Var 4), Var 7, Not (Var 7)], Or [Var 3, Var 2, Var 1, Not (Var 7)], Or [Not (Var 3), Var 1, Not (Var 6), Var 4, Not (Var 7), Not (Var 7)], Or [Var 6, Not (Var 7)], Or [Var 3, Not (Var 7)], Or [Var 2, Var 3], Or [Not (Var 2), Not (Var 1), Not (Var 6), Not (Var 5), Not (Var 7), Not (Var 7)], Or [Not (Var 2), Var 5, Var 7], Or [Var 3, Var 5], Or [Not (Var 3), Not (Var 2), Not (Var 7), Not (Var 7)], Or [Not (Var 2), Not (Var 5), Var 7, Not (Var 7), Var 7, Not (Var 7)], Or [Var 2, Not (Var 5), Var 7], Or [Var 2, Var 6, Var 4, Not (Var 7)], Or [Var 2, Not (Var 5), Var 7, Not (Var 7)], Or [Var 6, Var 5, Not (Var 7)], Or [Var 4, Not (Var 7)], Or [Var 3, Var 6, Not (Var 7), Not (Var 7)]]);

(* ==================== PART 1 ==================== *)

val _ = print "------- getVars -------";
val _ : ''a expression -> ''a list = getVars;
val getVars_1 = getVars True = [];
val getVars_2 = getVars False = [];
val getVars_3 = getVars (And []) = [];
val getVars_4 = getVars (Or [And [Eq []]]) = [];
val getVars_5 = getVars (Imp (False, True)) = [];
val getVars_6 = getVars (Var 5) = [5];
val getVars_7 = getVars (And [Var true, Var false, Var true]) = [true, false];
val getVars_8 = getVars (And [Var false, Var true, Var true]) = [false, true];
val getVars_9 = getVars (Eq [Var 5, Imp (False, Var 2), True, Var 3]) = [5, 2, 3];
val getVars_10 = getVars (Eq [Var "A", Var "B", Imp (Var "D", Not (Var "Q")), Var "D", Var "B"]) = ["A", "B", "D", "Q"];
val getVars_11 = getVars (Imp (Imp (True, Var 1), Imp (Var 0, Not (Var 4)))) = [1, 0, 4];
val getVars_12 = getVars exp1 = [3, 1, 4, 2];
val getVars_13 = getVars exp2 = [3, 2];
val getVars_14 = getVars exp3 = [1, 2];
val getVars_15 = getVars exp4 = [3, 2, 0, 6, 1, 4, 8];
val getVars_16 = getVars exp5 = [2, 0, 6, 3, 1];
val getVars_17 = getVars exp6 = [1, 5, 7, 4, 6, 2, 3];
val _ = (all_tests := !all_tests @ [getVars_1 , getVars_2 , getVars_3 , getVars_4 , getVars_5 , getVars_6 , getVars_7 , getVars_8 , getVars_9 , getVars_10, getVars_11, getVars_12, getVars_13, getVars_14, getVars_15, getVars_16, getVars_17]);

val _ = print "------- eval -------";
val _ : ''a list -> ''a expression -> bool = eval;
val eval_1 = eval [] True = true;
val eval_2 = eval [] False = false;
val eval_3 = eval [] (Not False) = true;
val eval_4 = eval [] (Not True) = false;
val eval_5 = eval [] (Not (Not False)) = false;
val eval_6 = eval [] (Not (Not True)) = true;
val eval_7 = eval [] (Var "A") = false;
val eval_8 = eval ["B"] (Var "A") = false;
val eval_9 = eval ["A"] (Var "A") = true;
val eval_10 = eval ["A"] (Not (Var "A")) = false;
val eval_11 = eval [] (Or []) = false;
val eval_12 = eval [] (And []) = true;
val eval_13 = eval [] (Eq []) = true;
val eval_14 = eval [] (Or [True]) = true;
val eval_15 = eval [] (Or [False]) = false;
val eval_16 = eval [] (And [True]) = true;
val eval_17 = eval [] (And [False]) = false;
val eval_18 = eval [] (Eq [True]) = true;
val eval_19 = eval [] (Eq [False]) = true;
val eval_20 = eval [5] (Or [Var 5]) = true;
val eval_21 = eval [true] (And [Var true]) = true;
val eval_22 = eval ["bla"] (Eq [Var "bla"]) = true;
val eval_23 = eval [] (Or [False, False]) = false;
val eval_24 = eval [] (Or [False, True]) = true;
val eval_25 = eval [] (Or [True, False]) = true;
val eval_26 = eval [] (Or [True, True]) = true;
val eval_27 = eval [5, 6] (Or [Var 5, Var 6]) = true;
val eval_28 = eval [4, 7] (Or [Var 5, Var 6]) = false;
val eval_29 = eval [] (And [False, False]) = false;
val eval_30 = eval [] (And [False, True]) = false;
val eval_31 = eval [] (And [True, False]) = false;
val eval_32 = eval [] (And [True, True]) = true;
val eval_33 = eval [5, 6] (And [Var 5, Var 6]) = true;
val eval_34 = eval [5, 7] (And [Var 5, Var 6]) = false;
val eval_35 = eval [] (Eq [False, False]) = true;
val eval_36 = eval [] (Eq [False, True]) = false;
val eval_37 = eval [] (Eq [True, False]) = false;
val eval_38 = eval [] (Eq [True, True]) = true;
val eval_39 = eval [5, 6] (Eq [Var 5, Var 6]) = true;
val eval_40 = eval [5, 7] (Eq [Var 5, Var 6]) = false;
val eval_41 = eval [] (Imp (False, False)) = true;
val eval_42 = eval [] (Imp (False, True)) = true;
val eval_43 = eval [] (Imp (True, False)) = false;
val eval_44 = eval [] (Imp (True, True)) = true;
val eval_45 = eval [5, 6] (Imp (Var 5, Var 6)) = true;
val eval_46 = eval [5, 7] (Imp (Var 5, Var 6)) = false;
val eval_47 = eval [] (Or [True, True, True]) = true;
val eval_48 = eval [] (Or [True, False, True]) = true;
val eval_49 = eval [] (Or [False, False, True]) = true;
val eval_50 = eval [] (Or [False, False, False]) = false;
val eval_51 = eval [] (And [True, True, True]) = true;
val eval_52 = eval [] (And [True, False, True]) = false;
val eval_53 = eval [] (And [False, False, True]) = false;
val eval_54 = eval [] (And [False, False, False]) = false;
val eval_55 = eval [] (Eq [True, True, True]) = true;
val eval_56 = eval [] (Eq [True, False, True]) = false;
val eval_57 = eval [] (Eq [False, False, True]) = false;
val eval_58 = eval [] (Eq [False, False, False]) = true;
val eval_59 = eval [2, 3] (And [True, Or [Var 1, Not (Not (Var 2))], Imp (Var 1, Var 2)]) = true;
val eval_60 = eval [] (And [True, Or [Var 1, Not (Not (Var 2))], Imp (Var 1, Var 2)]) = false;
val eval_61 = eval ["A", "B", "D", "Q"] (Eq [Var "A", Var "B", Imp (Var "D", Not (Var "Q")), Var "D", Var "B"]) = false;
val eval_62 = eval [2, 3] exp1 = false;
val eval_63 = eval [] exp1 = true;
val eval_64 = eval [0, 2, 4, 6] exp4 = true;
val eval_65 = eval [0, 2, 4, 6] exp5 = false;
val _ = (all_tests := !all_tests @ [eval_1 , eval_2 , eval_3 , eval_4 , eval_5 , eval_6 , eval_7 , eval_8 , eval_9 , eval_10, eval_11, eval_12, eval_13, eval_14, eval_15, eval_16, eval_17, eval_18, eval_19, eval_20, eval_21, eval_22, eval_23, eval_24, eval_25, eval_26, eval_27, eval_28, eval_29, eval_30, eval_31, eval_32, eval_33, eval_34, eval_35, eval_36, eval_37, eval_38, eval_39, eval_40, eval_41, eval_42, eval_43, eval_44, eval_45, eval_46, eval_47, eval_48, eval_49, eval_50, eval_51, eval_52, eval_53, eval_54, eval_55, eval_56, eval_57, eval_58, eval_59, eval_60, eval_61, eval_62, eval_63, eval_64, eval_65]);

val _ = print "------- rmEmpty -------";
val _ : 'a expression -> 'a expression = rmEmpty;
val rmEmpty_1 = rmEmpty (Or []) = False;
val rmEmpty_2 = rmEmpty (And []) = True;
val rmEmpty_3 = rmEmpty (Eq []) = True;
val rmEmpty_4 = rmEmpty (Or [Var "A"]) = Var "A";
val rmEmpty_5 = rmEmpty (And [Var "A"]) = Var "A";
val rmEmpty_6 = rmEmpty (Eq [Var "A"]) = True;
val rmEmpty_7 = rmEmpty (Eq [False]) = True;
val rmEmpty_7 = rmEmpty (Eq [Not (Var 100)]) = True;
val rmEmpty_8 = rmEmpty (Or [And [Or [Eq [Not (Var 0)]]], True]) = Or [True, True];
val rmEmpty_9 = rmEmpty (Eq [True, Eq [False, Var 3, And [And [], Eq [], Or [], Or [Var 1, Not (Eq [Var 4, False, True])], Imp (Eq [Or []], Var 2)]]]) = Eq [True, Eq [False, Var 3, And [True, True, False, Or [Var 1, Not (Eq [Var 4, False, True])], Imp (True, Var 2)]]];
val _ = (all_tests := !all_tests @ [rmEmpty_1 , rmEmpty_2 , rmEmpty_3 , rmEmpty_4 , rmEmpty_5 , rmEmpty_6 , rmEmpty_7 , rmEmpty_7 , rmEmpty_8 , rmEmpty_9]);

val _ = print "------- beautify -------";
val _ : 'a expression -> 'a expr = beautify;
val beautify1 = beautify True = T;
val beautify2 = beautify False = F;
val beautify3 = beautify (Var "lala") = V "lala";
val beautify4 = beautify (Not True) = !! T;
val beautify5 = beautify (Not False) = !! F;
val beautify6 = beautify (Not (Var true)) = !! (V true);
val beautify7 = beautify (Or [Var 5, True]) = V 5 \/ T;
val beautify8 = beautify (And [Var 5, True]) = V 5 /\ T;
val beautify9 = beautify (Eq [Var 5, True]) = V 5 <=> T;
val beautify10 = beautify (Imp (Var 5, True)) = V 5 ==> T;
val beautify11 = beautify (Or [Var 1, Var 2, Var 3]) = V 1 \/ V 2 \/ V 3;
val beautify12 = beautify (And [Var 1, Var 2, Var 3]) = V 1 /\ V 2 /\ V 3;
val beautify13 = beautify (Eq [Var 1, Var 2, Var 3]) = (V 1 <=> V 2) /\ (V 2 <=> V 3);
val beautify14 = beautify (Eq [Var 1, Var 2, Var 3, Var 4]) = (V 1 <=> V 2) /\ (V 2 <=> V 3) /\ (V 3 <=> V 4);
val beautify15 = beautify (Eq [Eq [Var 1, Var 2], Var 3]) = V 1 <=> V 2 <=> V 3;
val beautify16 = beautify (Eq [Var 1, Eq [Var 2, Var 3]]) = V 1 <=> (V 2 <=> V 3);
val beautify17 = beautify (Eq [Var 1, Eq [Var 2, Var 3, And [Or [], Or [Var 1, Not (Not (Var 2))], Imp (Var 1, Var 2)]]]) = V 1 <=> (V 2 <=> V 3) /\ (V 3 <=> F /\ (V 1 \/ !! (!! (V 2))) /\ (V 1 ==> V 2));
val beautify18 = beautify (Eq [True, Eq [False, Var 3, And [And [], Eq [], Or [], Or [Var 1, Not (Eq [Var 4, False, True])], Imp (Eq [Or []], Var 2)]]]) = T <=> (F <=> V 3) /\ (V 3 <=> T /\ T /\ F /\ (V 1 \/ !! ((V 4 <=> F) /\ (F <=> T))) /\ (T ==> V 2));
val _ = (all_tests := !all_tests @ [beautify1 , beautify2 , beautify3 , beautify4 , beautify5 , beautify6 , beautify7 , beautify8 , beautify9 , beautify10, beautify11, beautify12, beautify13, beautify14, beautify15, beautify16, beautify17, beautify18]);

val _ = print "------- pushNegations -------";
val _ : 'a expression -> 'a expression = pushNegations;
val pushNegations1 = pushNegations True = True;
val pushNegations2 = pushNegations False = False;
val pushNegations3 = pushNegations (Var "ABC") = Var "ABC";
val pushNegations4 = pushNegations (Or [True, Var ~5, Var 5]) = Or [True, Var ~5, Var 5];
val pushNegations5 = pushNegations (And [True, Var ~5, Var 5]) = And [True, Var ~5, Var 5];
val pushNegations6 = pushNegations (Eq [True, Var ~5, Var 5]) = Eq [True, Var ~5, Var 5];
val pushNegations7 = pushNegations (Imp (False, Var false)) = Imp (False, Var false);
val pushNegations8 = pushNegations (Not True) = Not True;
val pushNegations9 = pushNegations (Not False) = Not False;
val pushNegations10 = pushNegations (Not (Var "ABC")) = (Not (Var "ABC"));
val pushNegations11 = pushNegations (Not (Not True)) = True;
val pushNegations12 = pushNegations (Not (Not False)) = False;
val pushNegations13 = pushNegations (Not (Not (Not True))) = Not True;
val pushNegations14 = pushNegations (Not (Not (Not False))) = Not False;
val pushNegations15 = pushNegations (Not (Not (Not (Not True)))) = True;
val pushNegations16 = pushNegations (Not (Not (Not (Not False)))) = False;
val pushNegations17 = pushNegations (Not (Or [True, False])) = And [Not True, Not False];
val pushNegations18 = pushNegations (Not (Or [Var "A", Var "B"])) = And [Not (Var "A"), Not (Var "B")];
val pushNegations19 = pushNegations (Not (Or [Var "A", Var "B", Var "C", Var "D"])) = And [Not (Var "A"), Not (Var "B"), Not (Var "C"), Not (Var "D")];
val pushNegations20 = pushNegations (Not (Not (Or [Var "A", Var "B"]))) = Or [Var "A", Var "B"];
val pushNegations21 = pushNegations (Not (And [True, False])) = Or [Not True, Not False];
val pushNegations22 = pushNegations (Not (And [Var "A", Var "B"])) = Or [Not (Var "A"), Not (Var "B")];
val pushNegations23 = pushNegations (Not (And [Var "A", Var "B", Var "C", Var "D"])) = Or [Not (Var "A"), Not (Var "B"), Not (Var "C"), Not (Var "D")];
val pushNegations24 = pushNegations (Not (Not (And [Var "A", Var "B"]))) = And [Var "A", Var "B"];
val pushNegations25 = pushNegations (Not (Imp (False, True))) = And [False, Not True];
val pushNegations26 = pushNegations (Not (Imp (Var "A", Var "B"))) = And [Var "A", Not (Var "B")];
val pushNegations27 = pushNegations (Not (Not (Imp (False, True)))) = Imp (False, True);
val pushNegations28 = pushNegations (Not (Eq [Var "A", Var "B"])) = And [Or [Not (Var "A"), Not (Var "B")], Or [Var "A", Var "B"]];
val pushNegations29 = pushNegations (Not (Eq [Var "A", Var "B", Var "C", Var "D"])) = And [Or [Not (Var "A"), Not (Var "B"), Not (Var "C"), Not (Var "D")], Or [Var "A", Var "B", Var "C", Var "D"]];
val pushNegations30 = pushNegations (Not (Imp (Not (Not (Var "a")), True))) = And [Var "a", Not True];
val pushNegations31 = pushNegations (Not (Eq [False, Var 3, Not (And [True, False])])) = And [Or [Not False, Not (Var 3), And [True, False]], Or [False, Var 3, Or [Not True, Not False]]];
val pushNegations32 = pushNegations (Not (Eq [False, Var 3, Not (And [And [], Or [Var 1, Not (Eq [])], Imp (True, Var 2)])])) = And [Or [Not False, Not (Var 3), And [True, Or [Var 1, Not True], Imp (True, Var 2)]], Or [False, Var 3,Or [Not True, And [Not (Var 1), True], And [True, Not (Var 2)]]]];
val _ = (all_tests := !all_tests @ [pushNegations1, pushNegations2, pushNegations3, pushNegations4, pushNegations5, pushNegations6, pushNegations7, pushNegations8, pushNegations9, pushNegations10, pushNegations11, pushNegations12, pushNegations13, pushNegations14, pushNegations15, pushNegations16, pushNegations17, pushNegations18, pushNegations19, pushNegations20, pushNegations21, pushNegations22, pushNegations23, pushNegations24, pushNegations25, pushNegations26, pushNegations27, pushNegations28, pushNegations29, pushNegations30, pushNegations31, pushNegations32]);

val _ = print "------- rmConstants -------";
val _ : ''a expression -> ''a expression = rmConstants;
val rmConstants1 = rmConstants True = True;
val rmConstants2 = rmConstants False = False;
val rmConstants3 = rmConstants (Var "A") = Var "A";
val rmConstants4 = rmConstants (Not True) = False;
val rmConstants5 = rmConstants (Not False) = True;
val rmConstants6 = rmConstants (Not (Not True)) = True;
val rmConstants7 = rmConstants (Not (Not False)) = False;
val rmConstants8 = rmConstants (Not (Not (Not True))) = False;
val rmConstants9 = rmConstants (Not (Not (Not False))) = True;
val rmConstants10 = rmConstants (Not (Not (Not (Not True)))) = True;
val rmConstants11 = rmConstants (Not (Not (Not (Not False)))) = False;
val rmConstants12 = rmConstants (Not (Var "B")) = Not (Var "B");
val rmConstants13 = rmConstants (Or [True]) = True;
val rmConstants14 = rmConstants (Or [False]) = False;
val rmConstants15 = rmConstants (Or [True, False]) = True;
val rmConstants16 = rmConstants (Or [False, False]) = False;
val rmConstants17 = rmConstants (Or [True, False, False]) = True;
val rmConstants18 = rmConstants (Or [False, False, False]) = False;
val rmConstants19 = rmConstants (Or [False, False, False, True]) = True;
val rmConstants20 = rmConstants (Or [False, False, False, False]) = False;
val rmConstants21 = rmConstants (Or [Var false]) = Var false;
val rmConstants22 = rmConstants (Or [Var 20, Var 10]) = Or [Var 20, Var 10];
val rmConstants23 = rmConstants (Or [Var 20, True, Var 10]) = True;
val rmConstants24 = rmConstants (Or [Var 20, False, Var 10]) = Or [Var 20, Var 10];
val rmConstants25 = rmConstants (Or [Var 20, False, Var 10, True]) = True;
val rmConstants26 = rmConstants (Or [False, Var "A", False, Var "B", False]) = Or [Var "A", Var "B"];
val rmConstants27 = rmConstants (And [True]) = True;
val rmConstants28 = rmConstants (And [False]) = False;
val rmConstants29 = rmConstants (And [True, False]) = False;
val rmConstants30 = rmConstants (And [True, True]) = True;
val rmConstants31 = rmConstants (And [False, False]) = False;
val rmConstants32 = rmConstants (And [True, False, False]) = False;
val rmConstants33 = rmConstants (And [False, False, False]) = False;
val rmConstants34 = rmConstants (And [True, True, True]) = True;
val rmConstants35 = rmConstants (And [False, False, False, True]) = False;
val rmConstants36 = rmConstants (And [False, False, False, False]) = False;
val rmConstants37 = rmConstants (And [True, True, True, True]) = True;
val rmConstants38 = rmConstants (And [Var true]) = Var true;
val rmConstants39 = rmConstants (And [Var 20, Var 10]) = And [Var 20, Var 10];
val rmConstants40 = rmConstants (And [Var 20, True, Var 10]) = And [Var 20, Var 10];
val rmConstants41 = rmConstants (And [Var 20, False, Var 10]) = False;
val rmConstants42 = rmConstants (And [Var 20, False, Var 10, True]) = False;
val rmConstants43 = rmConstants (And [True, Var "A", True, Var "B", True]) = And [Var "A", Var "B"];
val rmConstants44 = rmConstants (Eq [True]) = True;
val rmConstants45 = rmConstants (Eq [False]) = True;
val rmConstants46 = rmConstants (Eq [True, False]) = False;
val rmConstants47 = rmConstants (Eq [False, True]) = False;
val rmConstants48 = rmConstants (Eq [True, True]) = True;
val rmConstants49 = rmConstants (Eq [False, False]) = True;
val rmConstants50 = rmConstants (Eq [True, False, False]) = False;
val rmConstants51 = rmConstants (Eq [False, False, False]) = True;
val rmConstants52 = rmConstants (Eq [True, True, True]) = True;
val rmConstants53 = rmConstants (Eq [False, False, False, True]) = False;
val rmConstants54 = rmConstants (Eq [False, False, False, False]) = True;
val rmConstants55 = rmConstants (Eq [True, True, True, True]) = True;
val rmConstants56 = rmConstants (Eq [Var true]) = True;
val rmConstants57 = rmConstants (Eq [True, Var 5]) = Var 5;
val rmConstants58 = rmConstants (Eq [False, Var "D"]) = Not (Var "D");
val rmConstants59 = rmConstants (Eq [Var 20, Var 10]) = Eq [Var 20, Var 10];
val rmConstants60 = rmConstants (Eq [Var 20, True, Var 10]) = And [Var 20, Var 10];
val rmConstants61 = rmConstants (Eq [Var 20, False, Var 10]) = And [Not (Var 20), Not (Var 10)];
val rmConstants62 = rmConstants (Eq [Var 20, False, Var 10, True]) = False;
val rmConstants63 = rmConstants (Eq [True, Var "A", True, Var "B", True, Var "C"]) = And [Var "A", Var "B", Var "C"];
val rmConstants64 = rmConstants (Eq [False, Var "A", False, Var "B", False, Var "C", False]) = And [Not (Var "A"), Not (Var "B"), Not (Var "C")];
val rmConstants65 = rmConstants (Imp (True, True)) = True;
val rmConstants66 = rmConstants (Imp (True, False)) = False;
val rmConstants67 = rmConstants (Imp (False, True)) = True;
val rmConstants68 = rmConstants (Imp (True, True)) = True;
val rmConstants69 = rmConstants (Imp (True, Var "C")) = Var "C";
val rmConstants70 = rmConstants (Imp (False, Var "C")) = True;
val rmConstants71 = rmConstants (Imp (Var "C", True)) = True;
val rmConstants72 = rmConstants (Imp (Var "C", False)) = Not (Var "C");
val rmConstants73 = rmConstants (Eq [True, Var 1, Var 2]) = And [Var 1,Var 2];
val rmConstants74 = rmConstants (Eq [Var 1, False, Var 2]) = And [Not (Var 1), Not (Var 2)];
val rmConstants75 = rmConstants (Not (Eq [Var 4, False, True])) = True;
val rmConstants76 = rmConstants (Or [Or [False, Var 1, Var 2], Var 1, Var 2]) = Or [Or [Var 1, Var 2], Var 1, Var 2];
val rmConstants77 = rmConstants (Or [Or [True, True, True], Var 1, Var 2]) = True;
val rmConstants78 = rmConstants (Or [Or [False, False, False], Var 1, Var 2]) = Or [Var 1, Var 2];
val rmConstants79 = rmConstants (Or [Or [Or [False, False, False], False, False], Var 1, Var 2]) = Or [Var 1, Var 2];
val rmConstants80 = rmConstants (Not (Or [Or [True, True, True], Var 1, Var 2])) = False;
val rmConstants81 = rmConstants (And [And [True, Var 1, Var 2], Var 1, Var 2]) = And [And [Var 1, Var 2], Var 1, Var 2];
val rmConstants82 = rmConstants (And [And [True, True, True], Var 1, Var 2]) = And [Var 1, Var 2];
val rmConstants83 = rmConstants (And [And [False, False, False], Var 1, Var 2]) = False;
val rmConstants84 = rmConstants (And [And [And [True, True, True], True, True], Var 1, Var 2]) = And [Var 1, Var 2];
val rmConstants85 = rmConstants (Not (And [And [False, False, False], Var 1, Var 2])) = True;
val rmConstants86 = rmConstants (Eq [Eq [True, Var 1, Var 2], Var 1, Var 2]) = Eq [And [Var 1,Var 2], Var 1,Var 2];
val rmConstants87 = rmConstants (Eq [Eq [True, True, True], Var 1, Var 2]) = And [Var 1, Var 2];
val rmConstants88 = rmConstants (Eq [Eq [False, False, False], Var 1, Var 2]) = And [Var 1, Var 2];
val rmConstants89 = rmConstants (Eq [Eq [Eq [False, False, False], True, True], Var 1, Var 2]) = And [Var 1, Var 2];
val rmConstants90 = rmConstants (Eq [Eq [Eq [False, False, False], True, True], True, True]) = True;
val rmConstants91 = rmConstants (Not (Eq [Eq [Eq [False, False, False], True, True], True, True])) = False;
val rmConstants92 = rmConstants (Imp (Imp (Var 1, Var 2), Var 3)) = Imp (Imp (Var 1, Var 2), Var 3);
val rmConstants93 = rmConstants (Imp (Imp (True, Var 2), Var 3)) = Imp (Var 2, Var 3);
val rmConstants94 = rmConstants (Imp (Imp (False, Var 2), Var 3)) = Var 3;
val rmConstants95 = rmConstants (Imp (Imp (Var 1, True), Var 3)) = Var 3;
val rmConstants96 = rmConstants (Imp (Imp (Var 1, False), Var 3)) = Imp (Not (Var 1), Var 3);
val rmConstants97 = rmConstants (Imp (Imp (True, True), True)) = True;
val rmConstants98 = rmConstants (Imp (Imp (True, True), False)) = False;
val rmConstants99 = rmConstants (Not (Imp (Imp (True, True), True))) = False;
val rmConstants100 = rmConstants (Not (Imp (Imp (True, True), False))) = True;
val rmConstants101 = rmConstants (Imp (Imp (True, Imp (Var 1, Var 2)), Var 3)) = Imp (Imp (Var 1, Var 2), Var 3);
val rmConstants102 = rmConstants (Or [Var 1, Not (Eq [Var 4, False, True])]) = True;
val rmConstants103 = rmConstants (And [And [], Or [Var 1, Not (Eq [Var 4, False, True])], Imp (True, Var 2)]) = Var 2;
val rmConstants104 = rmConstants (Eq [Var 1, Eq [False, Var 3, And [And [], Or [Var 1, Not (Eq [Var 4, False, True])], Imp (True, Var 2)]]]) = Eq [Var 1, And [Not (Var 3), Not (Var 2)]];
val _ = (all_tests := !all_tests @ [rmConstants1 , rmConstants2 , rmConstants3 , rmConstants4 , rmConstants5 , rmConstants6 , rmConstants7 , rmConstants8 , rmConstants9 , rmConstants10, rmConstants11, rmConstants12, rmConstants13, rmConstants14, rmConstants15, rmConstants16, rmConstants17, rmConstants18, rmConstants19, rmConstants20, rmConstants21, rmConstants22, rmConstants23, rmConstants24, rmConstants25, rmConstants26, rmConstants27, rmConstants28, rmConstants29, rmConstants30, rmConstants31, rmConstants32, rmConstants33, rmConstants34, rmConstants35, rmConstants36, rmConstants37, rmConstants38, rmConstants39, rmConstants40, rmConstants41, rmConstants42, rmConstants43, rmConstants44, rmConstants45, rmConstants46, rmConstants47, rmConstants48, rmConstants49, rmConstants50, rmConstants51, rmConstants52, rmConstants53, rmConstants54, rmConstants55, rmConstants56, rmConstants57, rmConstants58, rmConstants59, rmConstants60, rmConstants61, rmConstants62, rmConstants63, rmConstants64, rmConstants65, rmConstants66, rmConstants67, rmConstants68, rmConstants69, rmConstants70, rmConstants71, rmConstants72, rmConstants73, rmConstants74, rmConstants75, rmConstants76 , rmConstants77 , rmConstants78 , rmConstants79 , rmConstants80 , rmConstants81 , rmConstants82 , rmConstants83 , rmConstants84 , rmConstants85 , rmConstants86 , rmConstants87 , rmConstants88 , rmConstants89 , rmConstants90 , rmConstants91 , rmConstants92 , rmConstants93 , rmConstants94 , rmConstants95 , rmConstants96 , rmConstants97 , rmConstants98 , rmConstants99 , rmConstants100, rmConstants101, rmConstants102, rmConstants103, rmConstants104]);

val _ = print "------- rmVars -------";
val _ : ''a expression -> ''a expression = rmVars;
val rmVars1 = rmVars (True) = True;
val rmVars2 = rmVars (False) = False;
val rmVars3 = rmVars (Var 6) = Var 6;
val rmVars4 = rmVars (Not (Var true)) = Not (Var true);
val rmVars5 = rmVars (Or [Var "a", Var "a"]) = Var "a";
val rmVars6 = rmVars (Or [Var "a", Var "b", Var "c"]) = Or [Var "a", Var "b", Var "c"];
val rmVars7 = rmVars (Or [Var "a", Var "a", Var "b", Var "a", Var "b", Var "a", Var "c"]) = Or [Var "a", Var "b", Var "c"];
val rmVars8 = rmVars (Or [Or [Var "a", Var "a", Var "b"], Var "b"]) = Or [Or [Var "a", Var "b"], Var "b"];
val rmVars9 = rmVars (Or [Or [Var "a", Var "a", Var "a"], Var "b"]) = Or [Var "a", Var "b"];
val rmVars10 = rmVars (Or [Or [Var "a", Or [Var "a", Var "a"]], Var "b", Or [Var "a", Var "a"]]) = Or [Var "a", Var "b"];
val rmVars11 = rmVars (And [Var "a", Var "a"]) = Var "a";
val rmVars12 = rmVars (And [Var "a", Var "b", Var "c"]) = And [Var "a", Var "b", Var "c"];
val rmVars13 = rmVars (And [Var "a", Var "a", Var "b", Var "a", Var "b", Var "a", Var "c"]) = And [Var "a", Var "b", Var "c"];
val rmVars14 = rmVars (And [And [Var "a", Var "a", Var "b"], Var "b"]) = And [And [Var "a", Var "b"], Var "b"];
val rmVars15 = rmVars (And [And [Var "a", Var "a", Var "a"], Var "b"]) = And [Var "a", Var "b"];
val rmVars16 = rmVars (And [And [Var "a", And [Var "a", Var "a"]], Var "b", And [Var "a", Var "a"]]) = And [Var "a", Var "b"];
val rmVars17 = rmVars (Eq [Var "a", Var "a"]) = True;
val rmVars18 = rmVars (Eq [Var "a", Var "a", Var "b"]) = Eq [Var "a", Var "b"];
val rmVars19 = rmVars (Eq [Var "a", Var "b", Var "c"]) = Eq [Var "a", Var "b", Var "c"];
val rmVars20 = rmVars (Eq [Var "a", Var "a", Var "b", Var "a", Var "b", Var "a", Var "c"]) = Eq [Var "a", Var "b", Var "c"];
val rmVars21 = rmVars (Eq [Eq [Var "a", Var "a", Var "b"], Var "b"]) = Eq [Eq [Var "a", Var "b"], Var "b"];
val rmVars22 = rmVars (Eq [Eq [Var "a", Var "a", Var "a"], Var "b"]) = Eq [True, Var "b"];
val rmVars23 = rmVars (Eq [Eq [Var "a", Eq [Var "a", Var "a"]], Var "b", Eq [Var "a", Var "a"]]) = Eq [Eq [Var "a", True], Var "b", True];
val rmVars24 = rmVars (Imp (Var "a", Var "b")) = Imp (Var "a", Var "b");
val rmVars25 = rmVars (Imp (Var "a", Var "a")) = True;
val rmVars26 = rmVars (Imp (Imp (Var "a", Var "a"), Var "b")) = (Imp (True, Var "b"));
val rmVars27 = rmVars (Imp (Imp (Var "a", Var "b"), Imp (Var "a", Var "b"))) = True;
val rmVars28 = rmVars (Imp (Imp (Var "a", Imp (Var "a", Var "a")), Var "b")) = Imp (Imp (Var "a", True), Var "b");
val rmVars29 = rmVars (Or [Var "a", Var "a", Not (Var "b"), Not (Var "b")]) = Or [Var "a", Not (Var "b")];
val rmVars30 = rmVars (Imp (And [Var 0, Var 0] , Or [Var 0, Var 0])) = True;
val rmVars31 = rmVars (Imp (And [Var 0, Var 1] , And [Var 1, Var 0])) = Imp (And [Var 0, Var 1], And [Var 1, Var 0]);
val rmVars32 = rmVars (Imp (And [Eq [Var 1, Var 0], Eq [Var 1, Var 0], Or [Var 1, Var 1]], And [Eq [Var 1, Var 0], Var 1])) = True;
val _ = (all_tests := !all_tests @ [rmVars1 , rmVars2 , rmVars3 , rmVars4 , rmVars5 , rmVars6 , rmVars7 , rmVars8 , rmVars9 , rmVars10, rmVars11, rmVars12, rmVars13, rmVars14, rmVars15, rmVars16, rmVars17, rmVars18, rmVars19, rmVars20, rmVars21, rmVars22, rmVars23, rmVars24, rmVars25, rmVars26, rmVars27, rmVars28, rmVars29, rmVars30, rmVars31, rmVars32]);

val _ = print "------- simplify -------";
val _ : ''a expression -> ''a expression = simplify;
val simplify1 = simplify (True) = True;
val simplify2 = simplify (False) = False;
val simplify3 = simplify (Var 5) = Var 5;
val simplify4 = simplify (Not (Var 5)) = Not (Var 5);
val simplify5 = simplify (Or [Var "A", Var "B"]) = Or [Var "A", Var "B"];
val simplify6 = simplify (And [Var "A", Var "B"]) = And [Var "A", Var "B"];
val simplify7 = simplify (Eq [Var "A", Var "B"]) = Eq [Var "A", Var "B"];
val simplify8 = simplify (Imp (Var "A", Var "B")) = Imp (Var "A", Var "B");
val simplify9 = simplify (Eq [True, Eq [False, Var 3, And [And [], Or [Var 1, Not (Eq [Var 4, False, True])], Imp (True, Var 2)]], True,Eq [Not (Var 3), Var 2]]) = And [And [Not (Var 3), Not (Var 2)], Eq [Not (Var 3), Var 2]];
val simplify10 = simplify (Not (Eq [False, Var 3, Not (And [And [], Or [Var 1, Not (Eq [])], Imp (True, Var 2)])])) = Or [Var 3, Or [Not (Var 1), Not (Var 2)]];
val _ = (all_tests := !all_tests @ [simplify1, simplify2 , simplify3 , simplify4 , simplify5 , simplify6 , simplify7 , simplify8 , simplify9 , simplify10]);

val _ = print "------- prTestEq -------";
val _ : int -> ''a expression -> ''a expression -> bool = prTestEq;
val prTestEq1 = List.tabulate (20, (fn i => (i, prTestEq i exp1 exp2))) = [(0,true), (1,true), (2,true), (3,true), (4,true), (5,true), (6,true), (7,true), (8,true), (9,true), (10,true), (11,true), (12,true), (13,true), (14,true), (15,true), (16,true), (17,true), (18,true), (19,true)];
val prTestEq2 = List.tabulate (20, (fn i => (i, prTestEq i exp1 exp3))) = [(0,false), (1,true), (2,true), (3,true),( 4,false), (5,false), (6,true), (7,true), (8,true), (9,true), (10,true), (11,true), (12,false), (13,false), (14,true), (15,true), (16,true), (17,true), (18,true), (19,true)];
val prTestEq3 = List.tabulate (20, (fn i => (i, prTestEq i exp4 exp5))) = [(0,false), (1,false), (2,true), (3,true), (4,false), (5,false), (6,true), (7,true), (8,false), (9,false), (10,true), (11,true), (12,false), (13,false), (14,true), (15,true), (16,false), (17,false), (18,true), (19,true)];
val _ = (all_tests := !all_tests @ [prTestEq1, prTestEq2, prTestEq3]);

val _ = print "------- isCNF -------";
val _ : ''a expression -> bool = isCNF;
val isCNF1 = isCNF True = true;
val isCNF2 = isCNF False = true;
val isCNF3 = isCNF (Var 2) = true;
val isCNF4 = isCNF (Not (Var 3)) = true;
val isCNF5 = isCNF (Not (Not (Var 4))) = false;
val isCNF6 = isCNF (Or []) = true;
val isCNF7 = isCNF (Or [True]) = false;
val isCNF8 = isCNF (Or [False, True, True, False]) = false;
val isCNF9 = isCNF (Or [Var 1]) = true;
val isCNF10 = isCNF (Or [Var 1, Var 2, Var 3]) = true;
val isCNF11 = isCNF (Or [Var 1, Not (Var 2), Not (Var 3)]) = true;
val isCNF12 = isCNF (Not (Or [Var 1, Not (Var 2), Not (Var 3)])) = false;
val isCNF13 = isCNF (And []) = true;
val isCNF14 = isCNF (And [True]) = true;
val isCNF15 = isCNF (And [False, True, True, False]) = true;
val isCNF16 = isCNF (And [Var 1]) = true;
val isCNF17 = isCNF (And [Var 1, Var 2, Var 3]) = true;
val isCNF18 = isCNF (And [Var 1, Not (Var 2), Not (Var 3)]) = true;
val isCNF19 = isCNF (Not (And [Var 1, Not (Var 2), Not (Var 3)])) = false;
val isCNF20 = isCNF (And [Eq [], Or [Or []], Not False]) = false;
val isCNF21 = isCNF (And [Or [Var 1, Var 2], Or [Var 3, Not (Var 4)], Var 5, Not (Var 6), Or []]) = true;
val isCNF22 = isCNF (Or [Or [Var 1, Var 2], Or [Var 3, Not (Var 4)], Var 5, Not (Var 6), Or []]) = false;
val isCNF23 = isCNF (And [And [Var 1, Var 2], Or [Var 3, Not (Var 4)], Var 5, Not (Var 6), Or []]) = false;
val isCNF24 = isCNF (Or [And [Var 1, Var 2], Or [Var 3, Not (Var 4)], Var 5, Not (Var 6), Or []]) = false;
val isCNF25 = isCNF (Not (And [Or [Var 1, Var 2], Or [Var 3, Not (Var 4)], Var 5, Not (Var 6), Or []])) = false;
val isCNF26 = isCNF (And [Or [Var 1, Var 2], Or [Var 3, Not (Var 4)], Var 5, Not (Var 6), Eq []]) = false;
val isCNF27 = isCNF (And [Or [Var 1, Var 2], Or [Var 3, Not (Var 4)], Var 5, Not (Var 6), Imp (Var 7, Var 8)]) = false;
val isCNF28 = isCNF (And [Or [Var 1, Var 2], Or [Var 3, Not (Var 4)], Var 5, Not (Var 6), False]) = true;
val isCNF29 = isCNF (And [Or [Var 1, Var 2], Or [Var 3, Not (Var 4)], Var 5, Not (Var 6), True]) = true;
val isCNF30 = isCNF (And [Or [Var 1, Var 2], Or [Var 3, Not (Var 4)], Var 5, Not (Var 6), Not (Not (Var 5))]) = false;
val isCNF31 = isCNF (Eq []) = false;
val isCNF32 = isCNF (Eq [Var 1, False, Or []]) = false;
val isCNF33 = isCNF (Imp (Var 1, False)) = false;
val isCNF34 = isCNF (And [Var 1, Not (Var 0), Or [], Or [Var 1], Or [Var 1, Not (Var 2)]]) = true;
val _ = (all_tests := !all_tests @ [isCNF1 , isCNF2 , isCNF3 , isCNF4 , isCNF5 , isCNF6 , isCNF7 , isCNF8 , isCNF9 , isCNF10, isCNF11, isCNF12, isCNF13, isCNF14, isCNF15, isCNF16, isCNF17, isCNF18, isCNF19, isCNF20, isCNF21, isCNF22, isCNF23, isCNF24, isCNF25, isCNF26, isCNF27, isCNF28, isCNF29, isCNF30, isCNF31, isCNF32, isCNF33, isCNF34]);

val _ = print "------- toWolframLang -------";
val _ : ('a -> string) -> 'a expression -> string = toWolframLang;
val toWolframLang1 = toWolframLang Int.toString True = "True";
val toWolframLang2 = toWolframLang Int.toString False = "False";
val toWolframLang3 = toWolframLang Int.toString (Var 5) = "Var[\"5\"]";
val toWolframLang4 = toWolframLang (fn x => x) (Var "A") = "Var[\"A\"]";
val toWolframLang5 = toWolframLang (fn x => x) (Not (Var "A")) = "Not[Var[\"A\"]]";
val toWolframLang6 = toWolframLang Int.toString (Not (Var ~5)) = "Not[Var[\"~5\"]]";
val toWolframLang7 = toWolframLang Int.toString (And []) = "And[]";
val toWolframLang8 = toWolframLang Int.toString (And [Var 10]) = "And[Var[\"10\"]]";
val toWolframLang9 = toWolframLang Int.toString (And [False, Var 10, True, Not (Var 9)]) = "And[False, Var[\"10\"], True, Not[Var[\"9\"]]]";
val toWolframLang10 = toWolframLang (fn x => x) (And [False, Var "a", True, Not (Var "b")]) = "And[False, Var[\"a\"], True, Not[Var[\"b\"]]]";
val toWolframLang11 = toWolframLang Int.toString (Or []) = "Or[]";
val toWolframLang12 = toWolframLang Int.toString (Or [Var 10]) = "Or[Var[\"10\"]]";
val toWolframLang13 = toWolframLang Int.toString (Or [False, Var 10, True, Not (Var 9)]) = "Or[False, Var[\"10\"], True, Not[Var[\"9\"]]]";
val toWolframLang14 = toWolframLang (fn x => x) (Or [False, Var "a", True, Not (Var "b")]) = "Or[False, Var[\"a\"], True, Not[Var[\"b\"]]]";
val toWolframLang15 = toWolframLang Int.toString (Eq []) = "Equivalent[]";
val toWolframLang16 = toWolframLang Int.toString (Eq [Var 10]) = "Equivalent[Var[\"10\"]]";
val toWolframLang17 = toWolframLang Int.toString (Eq [False, Var 10, True, Not (Var 9)]) = "Equivalent[False, Var[\"10\"], True, Not[Var[\"9\"]]]";
val toWolframLang18 = toWolframLang (fn x => x) (Eq [False, Var "a", True, Not (Var "b")]) = "Equivalent[False, Var[\"a\"], True, Not[Var[\"b\"]]]";
val toWolframLang19 = toWolframLang Int.toString (Imp (True, Var 10)) = "Implies[True, Var[\"10\"]]";
val toWolframLang20 = toWolframLang Int.toString (Imp (Imp (False, Var 5), Not (Var 10))) = "Implies[Implies[False, Var[\"5\"]], Not[Var[\"10\"]]]";
val toWolframLang21 = toWolframLang (fn x => x) (Imp (Imp (False, Var "a"), Not (Var "b"))) = "Implies[Implies[False, Var[\"a\"]], Not[Var[\"b\"]]]";
val toWolframLang22 = toWolframLang Int.toString (Eq [True, Eq [False, Var 3, And [And [], Or [Var 1, Not (Eq [Var 4, False, True])], Imp (True, Var 2)]]]) = "Equivalent[True, Equivalent[False, Var[\"3\"], And[And[], Or[Var[\"1\"], Not[Equivalent[Var[\"4\"], False, True]]], Implies[True, Var[\"2\"]]]]]";
val toWolframLang23 = toWolframLang (fn x => x) (Eq [Eq [False, Var "a", And [And [], Or [Var "b", Not (Eq [Var "test"])], Imp (True, Var "2")]]]) = "Equivalent[Equivalent[False, Var[\"a\"], And[And[], Or[Var[\"b\"], Not[Equivalent[Var[\"test\"]]]], Implies[True, Var[\"2\"]]]]]";
val _ = (all_tests := !all_tests @ [toWolframLang1 , toWolframLang2 , toWolframLang3 , toWolframLang4 , toWolframLang5 , toWolframLang6 , toWolframLang7 , toWolframLang8 , toWolframLang9 , toWolframLang10, toWolframLang11, toWolframLang12, toWolframLang13, toWolframLang14, toWolframLang15, toWolframLang16, toWolframLang17, toWolframLang18, toWolframLang19, toWolframLang20, toWolframLang21, toWolframLang22, toWolframLang23]);

val _ = print "------- satSolver -------";
val _ : ''a expression -> ''a list option = satSolver;
val satSolver1 = satSolver (True : int expression) = SOME [];
val satSolver2 = satSolver (False : int expression) = NONE;
val satSolver3 = satSolver (And [] : int expression) = SOME [];
val satSolver4 = satSolver (And [Or []] : int expression) = NONE;
val satSolver5 = satSolver (And [Or [Var 1]]) = SOME [1];
val satSolver6 = let val sol = satSolver (And [Or [Var 1, Not (Var 2)]]) in sol = SOME [1] orelse sol = SOME [] end;
val satSolver7 = let val sol = satSolver (And [Or [Var 1, Not (Var 1)]]) in sol = SOME [] orelse sol = SOME [1] end;
val satSolver8 = satSolver (And [Or [Var 1, Var 3], Or [Not (Var 1), Not (Var 3)], Or[Not (Var 3), Var 1]]) = SOME [1];
val satSolver9 = satSolver (And [Or [Var 1, Var 3], Or [Not (Var 1), Not (Var 3)], Or[Not (Var 3), Var 1], Or[Not (Var 1), Var 3]]) = NONE;
val satSolver10 = let val sol = satSolver exp6 in sol = SOME [3, 6] orelse sol = SOME [6, 3] end;
val _ = (all_tests := !all_tests @ [satSolver1, satSolver2, satSolver3, satSolver4, satSolver5, satSolver6, satSolver6, satSolver7, satSolver8, satSolver9, satSolver10]);

val _ = print "------- bruteforce -------";
val _ : ''a expression -> ''a list option = bruteforce;
val bruteforce1 = bruteforce (True : int expression) = SOME [];
val bruteforce2 = bruteforce (False : int expression) = NONE;
val bruteforce3 = bruteforce (Or [] : int expression) = NONE;
val bruteforce4 = bruteforce (And [] : int expression) = SOME [];
val bruteforce5 = bruteforce (And [Or []] : int expression) = NONE;
val bruteforce6 = bruteforce (And [Or [Var 1]]) = SOME [1];
val bruteforce7 = let val sol = bruteforce (And [Or [Var 1, Var 3], Or [Not (Var 1), Not (Var 3)], Eq [Not (Var 3), Var 1]]) in sol = SOME [1] orelse sol = SOME [3] end;
val bruteforce8 = let val sol = bruteforce exp6 in sol = SOME [3, 6] orelse sol = SOME [6, 3] end;
val bruteforce9 = let val sol = bruteforce (And [Or [Var 1, Not (Var 2)]]) in sol = SOME [1] orelse sol = SOME [] end;
val bruteforce10 = let val sol = bruteforce (And [Or [Var 1, Not (Var 1)]]) in sol = SOME [] orelse sol = SOME [1] end;
val bruteforce11 = bruteforce (And [Or [Var 1, Var 3], Or [Not (Var 1), Not (Var 3)], Or[Not (Var 3), Var 1]]) = SOME [1];
val bruteforce12 = bruteforce (And [Or [Var 1, Var 3], Or [Not (Var 1), Not (Var 3)], Or[Not (Var 3), Var 1], Or[Not (Var 1), Var 3]]) = NONE;

val _ = (all_tests := !all_tests @ [bruteforce1, bruteforce2, bruteforce3, bruteforce4, bruteforce5, bruteforce6, bruteforce7, bruteforce8, bruteforce9, bruteforce10, bruteforce11, bruteforce12]);

(* ==================== PART 2 ==================== *)

type timetable = {day : string, time: int, course: string} list
type student = {studentID : int, curriculum : string list}

val students1 =
    [{studentID = 55, curriculum = ["DS", "P2", "RK"]},
    {studentID = 99, curriculum = ["P2", "RK", "ARS","MAFI"]}] : student list;

val students2 =
    [{studentID = 55, curriculum = ["DS", "P2", "RK"]},
    {studentID = 99, curriculum = ["P2", "RK", "ARS"]}] : student list;

val timetable =
    [{day = "cet", time = 8, course = "RK"},
    {day = "pet", time = 7, course = "DS"},
    {day = "sre", time = 10, course = "DS"},
    {day = "pet", time = 14, course = "DS"},
    {day = "sre", time = 10, course = "ARS"},
    {day = "pet", time = 14, course = "ARS"},
    {day = "tor", time = 7, course = "P2"},
    {day = "pon", time = 12, course = "P2"}] : timetable;

val _ = print "------- problemReduction + satSolver + solutionRepresentation -------";
fun solve i tt sl = solutionRepresentation (satSolver (problemReduction i tt sl));
val _ : int -> timetable -> student list -> (student * timetable) list option = solve;

val test1 = solve 1 timetable students2 = NONE;
val test2 = solve 2 timetable students1 = NONE;
val test3 = solve 3 timetable students1 = NONE;
(*  for a feasible solution (in order to verify it) you need to check (by hand)
    if the following statemetns hold:
        1. for each student A: A's timetable is a subset of the global timetable
        2. no student is missing in the output
        3. for each student A: A's timetable covers his curriculum
        4. for each student A: A's timetable has no overlaps
        5. for each cycle C: #students on a cycle C  < room capacity *)
val test4 = solve 2 timetable students2;
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "------- Test results -------";
val nr_passes_tests = foldl (fn (true, acc) => acc + 1 | (false, acc) => acc) 0 (!all_tests);
val nr_all_tests = length (!all_tests);
