(* it was my birthday on 24th march 2016, I turned 20 *)
(* and I am doing my PL assignment *weeps* *)

(* bigint is a list of bits and a sign bit *)
(* LSB is the leftmost bit, RSB is the rightmost bit *)
(* integers are stored in two's complement form *)

datatype bigint = BIGINT of bool list * bool;

(* -------------------------- utility functions --------------------------*)
fun getBits(BIGINT(a, _)) = a;
fun getSign(BIGINT(_, b)) = b;
fun bitmake(i:int) = if(i = 0) then false else true;

(* converts a positive number to its bool list form *)
fun bintegerize(n) = if(n = 0) then [] 
                     else (bitmake(n mod 2)) :: bintegerize(n div 2); 

(* creates a list of matching bits, useful for sign extension *)
fun extend(n, bit) = if(n = 0) then [] else bit :: extend(n - 1, bit);

(* extends a blist by n digits *)
fun signextend(bint, sign, n) = bint @ extend(n, sign);

(* takes two bigints and equalizes their lengths, by sign extension *)
fun equalizer(BIGINT(abits, asign), BIGINT(bbits, bsign)) = 
let
  val n:int = List.length(abits);
  val m:int = List.length(bbits);
  val s:int = if(n >= m) then n else m;
  val aext:bool list = signextend(abits, asign, s - n);
  val bext:bool list = signextend(bbits, bsign, s - m);
in (BIGINT(aext, asign), BIGINT(bext, bsign)) end;

fun boolLT(a, b) = b andalso not a;
fun boolGT(a, b) = a andalso not b;
fun boolLTE(a, b) = (a = b) orelse boolLT(a, b);
fun boolGTE(a, b) = (a = b) orelse boolGT(a, b);

fun xor(a, b) = (a andalso (not b)) orelse (b andalso (not a));

(* full adder circuit : returns sum and carry *)
fun fulladder(a, b, c) = 
let
  val carry = (a andalso b) orelse (b andalso c) orelse (c andalso a);
  val sum = xor(a, xor(b, c));
in (sum, carry) end;

(* adder circuit : adds 2 bit lists and returns carry *)
(* assert |as| = |bs| *)
fun adder([], [], incarry) = ([], incarry)
  | adder(x::xs, y::ys, incarry) = 
      let val (scurr, ccurr) = fulladder(x, y, incarry);
          val (snext, cnext) = adder(xs, ys, ccurr);
      in (scurr :: snext, cnext) end
  | adder(x, y, c) = ([], false); (* this should never run *)

fun frombitstring(bstr) = 
let
  fun fromstring(#"1"::str) = true::fromstring(str)
    | fromstring(#"0"::str) = false::fromstring(str)
    | fromstring([]) = []; 
in BIGINT(fromstring(String.explode(bstr)), false) end;

(* prints a bit representation of the number *)
fun bitstring(b) = 
let 
  fun tostring(true::x) = "1" ^ tostring(x)
    | tostring(false::x) = "0" ^ tostring(x)
    | tostring([]) = "";
in tostring(getBits(b)) end;
(* --------------------end of utility functions --------------------------*)


(* converts a int to bigint *)
fun getbigint(n) = 
let 
  val sign = if(n >= 0) then false else true;
  val abso = if(n >= 0) then n else ~n;
  val bits = bintegerize(abso);
  (* TODO : twos complement for negative numbers *)
in BIGINT(bits, sign) end;

fun bi2str(b:bigint) = "winter is coming";

fun str2bi(s:string) = 10::[];

fun lt(a:bigint, b:bigint) = true;
fun leq(a:bigint, b:bigint) = true;
fun le(a:bigint, b:bigint) = true;
fun geq(a:bigint, b:bigint) = true;

(* tests for equality *)
fun eq(a, b) = 
  let val (a, b) = equalizer(a, b);
  in getBits(a) = getBits(b) end;
  
fun neq(a, b) = not (eq(a, b))

fun add(a:bigint, b:bigint) = a;
fun sub(a:bigint, b:bigint) = a;
fun mul(a:bigint, b:bigint) = a;
(* fun div(a:bigint, b:bigint) = a; *)
(* fun mod(a:bigint, b:bigint) = a; *)
fun unminus(BIGINT(a, b)) = BIGINT(a, not b);

val x = getbigint(3);
val y = getbigint(9);
val z = frombitstring("11000");

val t = eq(y, z);

val (a, b) = equalizer(x, y);
val z = bitstring(BIGINT(adder(getBits(a), getBits(b), false)));

val _ = OS.Process.exit(OS.Process.success);
