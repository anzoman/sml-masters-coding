(* 1. seminarska naloga pri FP *)
(* Avtor: Anže Luzar *)
(* Naslov: FRI in izgubljeni urniki *)

Control.Print.printDepth := 100;
Control.Print.printLength := 1000;
Control.Print.stringDepth := 1000;
val _ = Control.polyEqWarn := false;

(* ---------------------------------------------------------------------------- *)
(* Prvi del — logične formule in algoritem DPLL *)

datatype 'a expr = !! of 'a expr
                | \/ of 'a expr * 'a expr
                | /\ of 'a expr * 'a expr
                | <=> of 'a expr * 'a expr
                | ==> of 'a expr * 'a expr
                | V of 'a
                | T | F;
infix 5 <=>;
infixr 6 ==>;
infix 7 \/;
infix 8 /\;

datatype 'a expression = Not of 'a expression
                    | Or of 'a expression list
                    | And of 'a expression list
                    | Eq of 'a expression list
                    | Imp of 'a expression * 'a expression
                    | Var of 'a
                    | True | False;

datatype 'a stream = Next of 'a * (unit -> 'a stream);
fun lcg seed =
    let fun lcg seed =
        Next (seed, fn () =>
            lcg (LargeInt.mod (1103515245 * seed + 12345, 0x7FFFFFFF)))
    in lcg (LargeInt.fromInt seed) end;

fun int2bool i = LargeInt.mod (i, 2) = 1;

exception InvalidCNF;
exception NotImplemented;

(* ---------------------------------------------------------------------------- *)
(* getVars *)

fun deleteElementHelper (vars : ''a list, x : ''a) : ''a list =
  if null vars
  then nil
  else if (hd vars) = x
  then deleteElementHelper (tl vars, x) 
  else (hd vars) :: deleteElementHelper (tl vars, x)

fun deleteDuplicatesHelper (vars : ''a list) : ''a list =
  if null vars then nil
  else (hd vars) :: deleteElementHelper (deleteDuplicatesHelper (tl vars), hd vars)

fun getVarsHelper (exp : ''a expression) : ''a list =
  case exp of
    True => nil
  | False => nil
  | Var v => v::nil
  | Not e => getVarsHelper e
  | Or el => if null el
             then nil
             else if null (tl el)
             then getVarsHelper (hd el)
             else getVarsHelper (hd el) @ getVarsHelper (Or (tl el))
  | And el => if null el
              then nil
              else if null (tl el)
              then getVarsHelper (hd el)
              else getVarsHelper (hd el) @ getVarsHelper (And (tl el))
  | Eq el => if null el
             then nil
             else if null (tl el)
             then getVarsHelper (hd el)
             else getVarsHelper (hd el) @ getVarsHelper (Eq (tl el))
  | Imp et => getVarsHelper (#1 et) @ getVarsHelper (#2 et)

fun getVars (exp : ''a expression) : ''a list =
  deleteDuplicatesHelper (getVarsHelper exp)

(* ---------------------------------------------------------------------------- *)
(* eval *)

fun isElementInListHelper (vars : ''a list, x : ''a) : bool =
  if null vars
  then false
  else if (hd vars) = x
  then true 
  else isElementInListHelper (tl vars, x);

fun allTheSameHelper (lst: bool list) = 
  let fun helper (lst: bool list, first : bool) = 
        case lst of
          [] => true
        | glava::rep => (if (hd lst) = first then helper (tl lst, first) else false)
  in if (length lst) < 2 then true else helper (tl lst, hd lst)
  end;

fun eval (vars : ''a list) (exp : ''a expression) : bool =
  case exp of
    True => true
  | False => false
  | Var v => isElementInListHelper (vars, v)
  | Not e => not (eval vars e)
  | Or el => (case el of 
                [] => false
              | glava::rep => (eval vars glava) orelse (eval vars (Or rep)))
  | And el => (case el of 
                [] => true
              | glava::rep => (eval vars glava) andalso (eval vars (And rep)))
  | Eq el => (case el of 
                [] => true
              | glava::rep => allTheSameHelper (List.map (fn (x : ''a expression) => eval vars x) el))
  | Imp et => (not (eval vars (#1 et))) orelse (eval vars (#2 et));

(* ---------------------------------------------------------------------------- *)
(* rmEmpty *)

fun rmEmpty (exp : 'a expression) : 'a expression =
  case exp of
    True => True
  | False => False
  | Var v => Var v
  | Not e => Not (rmEmpty e)
  | Or el => if null el
             then False
             else if null (tl el)
             then rmEmpty (hd el)
             else Or ([rmEmpty (hd el)] @ (map(rmEmpty) (tl el)))
  | And el => if null el
              then True
              else if null (tl el)
              then rmEmpty (hd el)
              else And ([rmEmpty (hd el)] @ (map(rmEmpty) (tl el)))
  | Eq el => if null el
             then True
             else if null (tl el)
             then True
             else Eq ([rmEmpty (hd el)] @ (map(rmEmpty) (tl el)))
  | Imp et => Imp (rmEmpty (#1 et), rmEmpty (#2 et));

(* ---------------------------------------------------------------------------- *)
(* beautify *)

fun beautifyEquivalenceHelper (lst : 'a expr list, acc : 'a expr) : 'a expr =
  if null lst orelse null (tl lst)
  then acc
  else if List.length lst = 2
  then acc /\ ((hd lst) <=> (hd (tl lst)))
  else beautifyEquivalenceHelper (tl lst, acc /\ ((hd lst) <=> (hd (tl lst))));

fun beautifyHelper (exp : 'a expression) : 'a expr =
  case exp of
    True => T
  | False => F
  | Var v => V v
  | Not e => !! (beautifyHelper e)
  | Or el => if null (tl el)
             then beautifyHelper (hd el)
             else List.foldl (fn (x : 'a expression, acc : 'a expr) :'a expression * 'a expr => acc \/ (beautifyHelper x)) (beautifyHelper (hd el)) (tl el)
  | And el => if null (tl el)
             then beautifyHelper (hd el)
             else List.foldl (fn (x : 'a expression, acc : 'a expr) :'a expression * 'a expr => acc /\ (beautifyHelper x)) (beautifyHelper (hd el)) (tl el)
  | Eq el => if null (tl el)
             then beautifyHelper (hd el)
             else if null (tl (tl el))
             then beautifyHelper (hd el) <=> beautifyHelper (hd (tl el))
             else List.foldl (fn (x : 'a expression * 'a expression, acc : 'a expr) : ('a expression * 'a expression) * 'a expr => acc /\ (beautifyHelper (#1 x) <=> beautifyHelper (#2 x))) (beautifyHelper (hd el) <=> beautifyHelper (hd (tl el))) (tl (ListPair.zip (List.take (el, List.length el), tl el)))
  | Imp et => beautifyHelper (#1 et) ==> beautifyHelper (#2 et);

fun beautify (exp : 'a expression) : 'a expr =
  beautifyHelper (rmEmpty exp);

(* ---------------------------------------------------------------------------- *)
(* pushNegations *)

fun negations (exp : 'a expression) : 'a expression =
  case exp of
    True => True
  | False => False
  | Var v => Var v
  | Not e => (case e of
                Not ee => negations ee
              | And ee => Or (List.map (fn x => negations (Not x)) ee)
              | Or ee => And (List.map (fn x => negations (Not x)) ee)
              | Eq ee => And [Or (List.map (fn x => negations (Not x)) ee), Or (List.map (fn x => negations x) ee)]
              | Imp (ee1, ee2) => And [negations ee1, negations (Not ee2)]
              | _ => Not (negations e))
  | Or el => Or (List.map (fn x => negations x) el)
  | And el => And (List.map (fn x => negations x) el)
  | Eq el => Eq (List.map (fn x => negations x) el)
  | Imp (e1, e2) => Imp (negations e1, negations e2);

fun pushNegations (exp : 'a expression) : 'a expression =
  negations (rmEmpty exp);

(* ---------------------------------------------------------------------------- *)
(* rmConstants *)

fun rmConstantsHelper (exp : ''a expression) : ''a expression =
  case exp of
    True => True
  | False => False
  | Var v => Var v
  | Not e => (case rmConstantsHelper e of
                False => True
              | True => False
              | ee => Not ee)
  | Or el => (let fun rmConstantsDisjunction (el : ''a expression list) : ''a expression =
                    if (List.exists (fn x => x = True) el)
                    then True
                    else (let val lst = List.filter (fn x => (rmConstantsHelper x) <> False) el
                              val lstLength = List.length (lst)
                          in if lstLength = 0 then False else if lstLength = 1 then hd lst else Or (List.map (fn x => rmConstantsHelper x) lst)
                          end)
                  val result = rmConstantsDisjunction el
               in if result = (Or el) then result else rmConstantsHelper result
               end)
  | And el => (let fun rmConstantsConjunction (el : ''a expression list) : ''a expression =
                     if (List.exists (fn x => x = False) el)
                     then False
                     else (let val lst = List.filter (fn x => (rmConstantsHelper x) <> True) el
                               val lstLength = List.length (lst)
                           in if lstLength = 0 then True else if lstLength = 1 then hd lst else And (List.map (fn x => rmConstantsHelper x) lst)
                           end)
                   val result = rmConstantsConjunction el
               in if result = (And el) then result else rmConstantsHelper result
               end)
  | Eq el => (let fun rmConstantsEquivalence (el : ''a expression list) : ''a expression = 
                    let val trueExists = List.exists (fn x => x = True) el
                        val falseExists = List.exists (fn x => x = False) el
                    in case (trueExists, falseExists) of
                         (true, true) => False
                       | (true, false) => (let val lst = List.filter (fn x => (rmConstantsHelper x) <> True) el
                                               val lstLength = List.length (lst)
                                           in if lstLength = 0 then True else if lstLength = 1 then hd lst else And lst
                                           end)
                       | (false, true) => (let val lst = List.filter (fn x => (rmConstantsHelper x) <> False) el
                                               val lstLength = List.length (lst)
                                           in if lstLength = 0 then True else if lstLength = 1 then Not (hd lst) else And (List.map (fn x => Not x) lst) 
                                           end)
                       | (false, false) => Eq (List.map (fn x => rmConstantsHelper x) el)
                    end
                val result = rmConstantsEquivalence el
              in if result = (Eq el) then result else rmConstantsHelper result
              end)
  | Imp et => (let fun rmImplicationConstants (et : ''a expression * ''a expression) : ''a expression =
                     case et of
                       (e1, False) => rmConstantsHelper (Not e1)
                     | (e1, True) => True
                     | (False, e2) => True
                     | (True, e2) => e2
                     | (e1, e2) => Imp (rmConstantsHelper e1, rmConstantsHelper e2)
                   val result = rmImplicationConstants et
               in if result = (Imp et) then result else rmConstantsHelper result
               end);

fun rmConstants (exp : ''a expression) : ''a expression =
  rmConstantsHelper (rmEmpty exp);

(* ---------------------------------------------------------------------------- *)
(* rmVars *)

fun rmVarsHelper (exp : ''a expression) : ''a expression = 
  case exp of
    True => True
  | False => False
  | Var v => Var v
  | Not e => Not (rmVarsHelper e)
  | Or el => (let fun rmDisjunctionVars (el : ''a expression list) : ''a expression =
                    let val lst = (List.map (fn x => rmVarsHelper x) (deleteDuplicatesHelper el))
                        val lstLength = List.length (lst)
                    in if lstLength = 0 then True else if lstLength = 1 then hd lst else Or lst 
                    end
                  val result = rmDisjunctionVars el
              in if result = (Or el) then result else rmVarsHelper result
              end)
  | And el => (let fun rmConjunctionVars (el : ''a expression list) : ''a expression =
                     let val lst = (List.map (fn x => rmVarsHelper x) (deleteDuplicatesHelper el))
                         val lstLength = List.length (lst)
                     in if lstLength = 0 then True else if lstLength = 1 then hd lst else And lst 
                     end
                   val result = rmConjunctionVars el
               in if result = (And el) then result else rmVarsHelper result
               end)
  | Eq el => (let fun rmEquivalenceVars (el : ''a expression list) : ''a expression =
                    let val allTheSame = false
                        val lst = (List.map (fn x => rmVarsHelper x) (deleteDuplicatesHelper el))
                        val lstLength = List.length (lst)
                    in if lstLength = 0 then True 
                       else if lstLength = 1 
                       then (if List.length (el) >= 2 then True else hd lst) 
                       else Eq lst 
                    end
                  val result = rmEquivalenceVars el
              in if result = (Eq el) then result else rmVarsHelper result
              end)
  | Imp (e1, e2) => if e1 = e2
                    then True
                    else (let val result = Imp (rmVarsHelper e1, rmVarsHelper e2)
                          in if result = (Imp (e1, e2)) then result else rmVarsHelper result
                          end);

fun rmVars (exp : ''a expression) : ''a expression =
  rmVarsHelper (rmEmpty exp);

(* ---------------------------------------------------------------------------- *)
(* simplify *)

fun simplify (exp : ''a expression) : ''a expression =
  let val result = rmVars (pushNegations (rmConstants exp))
  in if result = exp then result else simplify result
  end;

(* ---------------------------------------------------------------------------- *)
(* simplify2 *)

fun simplify2 (exp : ''a expression) : ''a expression =
  raise NotImplemented;

(* ---------------------------------------------------------------------------- *)
(* prTestEq *)

fun prTestEq (seed : int) (exp1 : ''a expression) (exp2 : ''a expression) : bool =
  let fun chooseBoolsForVars (len : int, stream : IntInf.int stream, result : bool list) : bool list =
        if len = 0
        then result
        else case stream of Next (e1, e2) => chooseBoolsForVars (len - 1, e2 (), result @ [int2bool e1])
      val vars1 = getVars exp1
      val vars2 = getVars exp2
      val vars = deleteDuplicatesHelper (vars1 @ vars2)
      val chosenVars = List.foldl (fn ((v, b), acc) => if b = true then acc @ [v] else acc) [] (ListPair.zip (vars, chooseBoolsForVars (List.length vars, lcg seed, [])))
  in eval chosenVars exp1 = eval chosenVars exp2
  end;

(* ---------------------------------------------------------------------------- *)
(* isCNF *)

fun isCNF (exp : ''a expression) : bool =
  let fun checkOr (el : ''a expression list) : bool = List.foldl (fn (x, acc) => if acc = false then acc else (case x of Var v => true | Not (Var v) => true | _ => false)) true el
      fun checkAnd (el : ''a expression list) : bool = List.foldl (fn (x, acc) => if acc = false then acc else (case x of True => true | False => true | Var v => true | Not (Var v) => true | Or l => (checkOr l) | _ => false)) true el
  in case exp of
       True => true
     | False => true
     | Var v => true
     | Not e => (case e of Var ee => true | _ => false)
     | Or el => checkOr el
     | And el => checkAnd el
     | _ => false
  end;

(* ---------------------------------------------------------------------------- *)
(* toWolframLang *)

fun toWolframLang (f : 'a -> string) (exp : 'a expression) : string =
  case exp of
    True => "True"
  | False => "False"
  | Var v => "Var[\"" ^ (f v) ^ "\"]"
  | Not e => "Not[" ^ (toWolframLang f e) ^ "]"
  | Or el => "Or[" ^ (List.foldl (fn (x, acc) => if acc = "" then x else acc ^ ", " ^ x) "" (List.map (fn x => toWolframLang f x) el)) ^ "]"
  | And el => "And[" ^ (List.foldl (fn (x, acc) => if acc = "" then x else acc ^ ", " ^ x) "" (List.map (fn x => toWolframLang f x) el)) ^ "]"
  | Eq el => "Equivalent[" ^ (List.foldl (fn (x, acc) => if acc = "" then x else acc ^ ", " ^ x) "" (List.map (fn x => toWolframLang f x) el)) ^ "]"
  | Imp (e1, e2) => "Implies[" ^ (toWolframLang f e1) ^ ", " ^ (toWolframLang f e2)  ^ "]";

(* ---------------------------------------------------------------------------- *)
(* tseytinTransformation *)

fun tseytinTransformation (l : ''a list) (exp : ''a expression) : ''a expression =
  raise NotImplemented;

(* ---------------------------------------------------------------------------- *)
(* satSolver *)

fun dpll (exp : ''a expression) =
  let val removedVars = ref ([] : ''a list)
      
      fun addRemovedVar (removedVar : ''a) = 
        if List.exists (fn x => x = removedVar) (!removedVars)
        then ()
        else removedVars := (!removedVars) @ [removedVar]

      fun deleteRemovedVar (removedVar : ''a) =
        removedVars := List.filter (fn x => x <> removedVar) (!removedVars)

      fun setVarToConstant (exp : ''a expression) (selectedVar : ''a, value : ''a expression) : ''a expression = 
        case exp of
          Var v => if v = selectedVar 
                   then ( if value = True then addRemovedVar v else deleteRemovedVar v ; value ) 
                   else Var v
        | Not (Var v) => if v = selectedVar 
                         then ( if value = True then addRemovedVar v else deleteRemovedVar v ; Not (value) )
                         else Not (Var v)
        | Or el => Or (List.map (fn x => setVarToConstant x (selectedVar, value)) el)
        | And el => And (List.map (fn x => setVarToConstant x (selectedVar, value)) el)
        | _ => exp

      fun findIsolatedVariable (exp : ''a expression) : (''a * ''a expression) option = 
        case exp of
          Var v => SOME (v, True)
        | Not (Var v) => SOME (v, False)
        | Or [Var v] => SOME (v, True)
        | Or [Not (Var v)] => SOME (v, False)
        | And el => List.foldl (fn (x, acc) => findIsolatedVariable x) NONE el
        | _ => NONE

      fun step1 (exp : ''a expression) : ''a expression = 
        let val selectedIsolatedVar = findIsolatedVariable exp
        in if selectedIsolatedVar = NONE
           then rmConstants (exp)
           else step1 (setVarToConstant (exp) (valOf (selectedIsolatedVar)))
        end

      fun dpll_helper (exp : ''a expression) : ''a list option =
        let val expAfterStep1 = step1 exp
        in if expAfterStep1 = (And []) orelse expAfterStep1 = True 
           then SOME (!removedVars) 
           else if expAfterStep1 = (Or []) orelse expAfterStep1 = False
           then NONE
           else (let val recursion_result1 = dpll_helper (rmConstants (setVarToConstant (expAfterStep1) (hd (getVars expAfterStep1), True)))
                 in if recursion_result1 = NONE 
                    then dpll_helper (rmConstants (setVarToConstant (expAfterStep1) (hd (getVars expAfterStep1), False)))
                    else recursion_result1
                 end)
        end
  in dpll_helper exp
  end;

fun satSolver (exp : ''a expression) : ''a list option =
  if not (isCNF (exp)) 
  then raise InvalidCNF
  else (case exp of
          True => SOME []
        | False => NONE
        | And [] => SOME []
        | Or [] => NONE
        | _ => dpll exp)

(* ---------------------------------------------------------------------------- *)
(* bruteforce *)

fun bruteforce (exp : ''a expression) : ''a list option =
  let fun generate lst head =
        case lst of
          [] => []
        | h::t => (let val start = (head @ [h]) 
                   in let fun append lst =
                            case lst of
                              [] => []
                            | hh::tt => [start @ [hh]] @ append tt 
                      in append t @ (generate t start)
                      end
                   end)

      fun generateSublists lst =
          case lst of
            [] => [[]]
          | h::t => [[h]] @ (generate lst []) @ (generateSublists t)

      val vars = getVars exp
      val sublists = generateSublists vars

      fun bruteForce (exp : ''a expression, sublistLen : int) : ''a list option =
        let val sublists = List.filter (fn x => List.length x = sublistLen) sublists
            val result = List.foldl (fn (sublist, acc) => if acc <> NONE then acc else (if eval sublist exp = true then SOME sublist else NONE)) NONE sublists
        in if result <> NONE
           then result
           else if sublistLen = List.length vars
           then NONE
           else bruteForce (exp, sublistLen + 1)
        end
  in
    bruteForce (exp, 0)
  end;

(* ---------------------------------------------------------------------------- *)
(* eqExpressions *)

fun eqExpressions (exp1 : ''a expression) (exp2 : ''a expression) : bool =
  raise NotImplemented;

(* ---------------------------------------------------------------------------- *)
(* Drugi del — reševanje problema urnikov *)

type timetable = {day : string, time : int, course : string} list
type student = {studentID : int, curriculum : string list}

(* ---------------------------------------------------------------------------- *)
(* problemReduction *)

fun problemReduction (steviloProstihMest : int) (urnik : timetable) (studenti : student list) : (int * string * (string * int) * int) expression =
  let fun varsBuilderHelper (s : student, t : {day : string, time : int, course : string}, ponovitev : int, acc : (int * string * (string * int) * int) expression list) : (int * string * (string * int) * int) expression list =
        if ponovitev = steviloProstihMest 
        then acc
        else varsBuilderHelper (s, t, ponovitev + 1, acc @ [Var (#studentID s, #course t, ((#day t), (#time t)), ponovitev + 1)])

      fun kombinacijeBrezPonavljanjaHelper (red, lst) =
        case (red, lst) of 
          (0, _) => [[]]
        | (_, []) => []
        | (m, x::xs) => map (fn y => x::y) (kombinacijeBrezPonavljanjaHelper (m - 1, xs)) @ kombinacijeBrezPonavljanjaHelper (m, xs)

      fun varsBuilderHelperNegations (s : student, t : {day : string, time : int, course : string}, ponovitev : int, acc : (int * string * (string * int) * int) expression list) : (int * string * (string * int) * int) expression list =
          if ponovitev = steviloProstihMest 
          then [Or acc]
          else (if steviloProstihMest <= 2
                then varsBuilderHelperNegations (s, t, ponovitev + 1, acc @ [Not (Var (#studentID s, #course t, ((#day t), (#time t)), ponovitev + 1))])
                else List.foldl (fn (p, acc) => acc @ [Or [Not (Var (#studentID s, #course t, ((#day t), (#time t)), hd p)),
                                                           Not (Var (#studentID s, #course t, ((#day t), (#time t)), hd (tl p)))]]) 
                                [] (kombinacijeBrezPonavljanjaHelper (2, List.tabulate (steviloProstihMest, (fn x => x + 1)))))

      fun variacijeSPonavljanjemHelper spaces lst =
        case (spaces, lst) of
          (spaces, []) => [[]]
        | (0, _)  => [[]]
        | _ => List.foldr (fn (lst', result) => List.foldr (fn (x, acc) => (lst'::x)::acc) result (variacijeSPonavljanjemHelper (spaces - 1) lst)) [] lst

      fun poisciVariacijeSPonavljanjemHelper (prekrivajoci : timetable, steviloProstihMest : int) : int list list =
        variacijeSPonavljanjemHelper (List.length prekrivajoci) (List.tabulate (steviloProstihMest, (fn x => x + 1)))

      val prekrivajociPoiskani = ref ([] : string list)

      fun poisciPrekrivajoceHelper (s : student, urnik : timetable, t : {day : string, time : int, course : string}) : timetable =
        let val prekrivajoci = List.filter (fn x => (
                               List.exists (fn c => c = (#course x)) (#curriculum s)) andalso not (List.exists (fn p => p = (#course x)) (!prekrivajociPoiskani)) 
                                            andalso (#day x) = (#day t) andalso (#time x) = (#time t) andalso (#course x) <> (#course t)) urnik
        in if List.length prekrivajoci > 0 
           then (prekrivajociPoiskani := (!prekrivajociPoiskani) @ [#course t] ; [t] @ prekrivajoci)
           else (prekrivajociPoiskani := (!prekrivajociPoiskani) @ [#course t] ; [] )
        end

      val prekrivajociPoiskani = ref ([] : int list)

      fun poisciPrekrivajoceHelper (s : student, urnik : timetable, t : {day : string, time : int, course : string}) : timetable =
        let val prekrivajoci = List.filter (fn x => (
                               List.exists (fn c => c = (#course x)) (#curriculum s)) andalso not (List.exists (fn p => p = (#studentID s)) (!prekrivajociPoiskani)) 
                                            andalso (#day x) = (#day t) andalso (#time x) = (#time t) andalso (#course x) <> (#course t)) urnik
        in if List.length prekrivajoci > 0 
           then (prekrivajociPoiskani := (!prekrivajociPoiskani) @ [#studentID s] ; [t] @ prekrivajoci)
           else (prekrivajociPoiskani := (!prekrivajociPoiskani) @ [#studentID s] ; [] )
        end

      fun obdelajPrekrivajoceHelper (s : student, prekrivajoci : timetable, steviloProstihMest : int) : (int * string * (string * int) * int) expression list =
        if List.length prekrivajoci > 1
        then List.foldl (fn (variacija, acc) => acc @
             List.map (fn lp => (let val pair1 = hd lp
                                     val pair2 = hd (tl lp)
                                 in Or [Not (Var (#studentID s, #course (#1 pair1), (#day (#1 pair1), #time (#1 pair1)), #2 pair1)),
                                        Not (Var (#studentID s, #course (#1 pair2), (#day (#1 pair2), #time (#1 pair2)), #2 pair2))]
                                 end)) (kombinacijeBrezPonavljanjaHelper (2, ListPair.zip (prekrivajoci, variacija)))) [] (poisciVariacijeSPonavljanjemHelper (prekrivajoci, steviloProstihMest))
        else []

      val skupniPoiskani = ref ([] : int list)

      fun poisciSkupneHelper (s : student, studenti : student list) =
        let fun presekSeznamov (l1, l2) = List.filter (fn x => List.exists (fn y => x = y) l2) l1
            val skupni = List.foldl (fn (e, acc) => let val presek = presekSeznamov (#curriculum s, #curriculum e)
                                                    in if null presek orelse e = s orelse List.exists (fn p => p = (#studentID e)) (!skupniPoiskani)
                                                       then acc 
                                                       else acc @ [(#studentID e, presek)]
                                                    end) [] studenti
            val skupniRazvrsceni = List.foldl (fn (c, result) => let val tuple = ((List.foldl (fn (sk, acc) => if List.exists (fn e => e = c) (#2 sk)
                                                                                                               then acc @ [(#1 sk)]
                                                                                                               else acc) [] skupni), c)
                                                                 in if null (#1 tuple)
                                                                    then result
                                                                    else result @ [tuple]
                                                                 end) [] (#curriculum s) 
        in if List.length skupniRazvrsceni > 0 
           then (skupniPoiskani := (!skupniPoiskani) @ [#studentID s] ; skupniRazvrsceni )
           else (skupniPoiskani := (!skupniPoiskani) @ [#studentID s] ; [] )
        end

      fun varsSkupniBuilderHelper (studentList : int list, t : {day : string, time : int, course : string}, ponovitev : int, acc : (int * string * (string * int) * int) expression list) : (int * string * (string * int) * int) expression list =
        if ponovitev = steviloProstihMest 
        then acc
        else varsSkupniBuilderHelper (studentList, t, ponovitev + 1 , acc @ 
             List.foldl (fn (s, accc) => accc @ [Or [Not (Var (hd s, #course t, ((#day t), (#time t)), ponovitev + 1)),
                                                     Not (Var (hd (tl s), #course t, ((#day t), (#time t)), ponovitev + 1))]]) 
                                                                        [] (kombinacijeBrezPonavljanjaHelper (2, studentList)))

      fun obdelajSkupneHelper (s : student, skupni : (int list * string) list, urnik : timetable, steviloProstihMest : int) : (int * string * (string * int) * int) expression list =
        if List.length skupni > 0
        then List.foldl (fn (sk, result) => result @ 
            (List.foldl (fn (t, acc) => if (#course t) = (#2 sk)
                                        then acc @ varsSkupniBuilderHelper ([#studentID s] @ (#1 sk), t, 0, []) 
                                        else acc) [] urnik)) [] skupni
        else []

  in if steviloProstihMest = 0 orelse List.length studenti = 0 orelse List.length urnik = 0
     then False
     else let val result =
         (List.foldl (fn (s, acc1) => acc1 @ (
          List.foldl (fn (c, acc2) => acc2 @ [Or (
          List.foldl (fn (t, acc3) => if c = (#course t) 
                                      then acc3 @ varsBuilderHelper (s, t, 0, []) 
                                      else acc3) [] urnik)]) [] (#curriculum s))) [] studenti) @
         (List.foldl (fn (s, acc1) => acc1 @ (
          List.foldl (fn (c, acc2) => acc2 @ (
          List.foldl (fn (t, acc3) => if c = (#course t)
                                      then if steviloProstihMest > 1 
                                           then acc3 @ (varsBuilderHelperNegations (s, t, 0, [])) @ obdelajPrekrivajoceHelper (s, poisciPrekrivajoceHelper (s, urnik, t), steviloProstihMest)
                                           else acc3 @ obdelajPrekrivajoceHelper (s, poisciPrekrivajoceHelper (s, urnik, t), steviloProstihMest)
                                      else acc3) [] urnik)) [] (#curriculum s))) [] studenti)
              val skupni = List.foldl (fn (e, acc) => acc @ obdelajSkupneHelper (e, poisciSkupneHelper (e, studenti), urnik, steviloProstihMest)) [] studenti
          in if null skupni then And result else And (result @ skupni)
          end                                                                                       
  end;

(* ---------------------------------------------------------------------------- *)
(* solutionRepresentation *)

fun solutionRepresentation (satSolverResult : (int * string * (string * int) * int) list option) : (student * timetable) list option =
  let fun poisciStudente (lst : (int * string * (string * int) * int) list) : int list =
        List.foldl (fn (x, acc) => if (List.exists (fn e => e = (#1 x)) acc) 
                                   then acc 
                                   else acc @ [#1 x]) [] lst

      fun poisciPredmeteZaStudenta (s : int, lst : (int * string * (string * int) * int) list) : string list =
        List.foldl (fn (x, acc) => if (List.exists (fn e => e = (#2 x)) acc) orelse not ((#1 x) = s)
                                   then acc 
                                   else acc @ [#2 x]) [] lst

      fun sestaviUrnikZaStudenta (s : int, lst : (int * string * (string * int) * int) list) : timetable =
        List.foldl (fn (x, acc) => if (#1 x) = s
                                   then acc @ [{course = (#2 x), day = (#1 (#3 x)), time = (#2 (#3 x))}]
                                   else acc) [] lst
  in if satSolverResult = NONE
     then NONE
     else SOME (List.map (
                fn s => ({curriculum = poisciPredmeteZaStudenta (s, valOf satSolverResult), studentID = s}, deleteDuplicatesHelper (sestaviUrnikZaStudenta (s, valOf satSolverResult)))) 
                (poisciStudente (valOf satSolverResult)))
  end;
