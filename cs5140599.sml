(* bigint is a list of integers and a sign bit *)
datatype bigint = BIGINT of int list * bool;

(* utility functions *)
fun getIntList(BIGINT(a, _)) = a;
fun getSignBit(BIGINT(_, b)) = b;

fun getbigint(n:int) = n::[]; 

fun bi2str(b:bigint) = "winter is coming";

fun str2bi(s:string) = 10::[];

fun lt(a:bigint, b:bigint) = true;
fun leq(a:bigint, b:bigint) = true;
fun le(a:bigint, b:bigint) = true;
fun geq(a:bigint, b:bigint) = true;
fun eq(a:bigint, b:bigint) = 
let
  val aInts = getIntList(a);
  val bInts = getIntList(b);
  val aBit = getSignBit(a);
  val bBit =  getSignBit(b);
in 
  if((List.length(aInts) = List.length(bInts)) andalso (aBit = bBit)) then
    (aInts = bInts)
  else false
end;

fun neq(a:bigint, b:bigint) = not (eq(a, b));

fun add(a:bigint, b:bigint) = a;
fun sub(a:bigint, b:bigint) = a;
fun mul(a:bigint, b:bigint) = a;
(* fun div(a:bigint, b:bigint) = a; *)
(* fun mod(a:bigint, b:bigint) = a; *)
fun unminus(BIGINT(a, b)) = BIGINT(a, not b);



(* testing *)
val a = BIGINT(10::[], true);
val c = BIGINT(10::20::[], true);
val d = BIGINT(10::20::[], true);
(* unimus *)
val b = unminus(a);
(* eq *)
val testeq = eq(a, b);
val testeq = eq(c, d);
val testeq = eq(c, c);
val testeq = eq(c, a);
(* noteq *)
val testneq = neq(a, b);
val testneq = neq(c, d);
val testneq = neq(c, c);
val testneq = neq(c, a);

val _ = OS.Process.exit(OS.Process.success);



