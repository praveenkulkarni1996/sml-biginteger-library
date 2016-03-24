(* TODO : remove the sign bit *)
(* it was my birthday on 24th march 2016, I turned 20 *)
(* and I am doing my PL assignment *weeps* *)

(* bigint is a list of bits and a sign bit *)
(* LSB is the leftmost bit, RSB is the rightmost bit *)
(* integers are stored in two's complement form *)

datatype bigint = BIGINT of bool list;

(* -------------------------- utility functions --------------------------*)
fun getBits(BIGINT(a)) = a;
fun getSign(BIGINT(a)) = List.last(a);
fun bitmake(i:int) = if(i = 0) then false else true;

(* converts a positive number to its bool list form *)
fun bintegerize(n) = if(n = 0) then false::[] 
                     else (bitmake(n mod 2)) :: bintegerize(n div 2); 

(* creates a list of matching bits, useful for sign extension *)
fun extend(n, bit) = if(n = 0) then [] else bit :: extend(n - 1, bit);

(* extends a blist by n digits *)
fun signextend(bint, sign, n) = bint @ extend(n, sign);

(* takes a pair of bit sequences, and does signextension until they are both of
 * equal length, this works in linear time.
 * Named after the greek monster who did this sort of stuff.
 *)

fun greek(BIGINT(x), BIGINT(y)) = 
let
  fun equator([], [], xp, yp) = ([], [])
    | equator(x::xs, [], xp, yp) = 
      let val (a, b) = equator(xs, [], x, yp) in (x::a, yp::b) end
    | equator([], y::ys, xp, yp) = 
      let val (a, b) = equator([], ys, xp, y) in (xp::a, y::b) end
    | equator(x::xs, y::ys, xp, yp) = 
      let val (a, b) = equator(xs, ys, x, y) in (x::a, y::b) end;
  val (xx, yy) = equator(x, y, false, false)
in (BIGINT(xx), BIGINT(yy)) end;

(* takes two bigints and equalizes their lengths, by sign extension *)
fun equalizer(BIGINT(abits), BIGINT(bbits)) = 
let
  val n:int = List.length(abits);
  val m:int = List.length(bbits);
  val asign = List.last(abits);  
  val bsign = List.last(bbits);  
  val s:int = if(n >= m) then n else m;
  val aext:bool list = signextend(abits, asign, s - n);
  val bext:bool list = signextend(bbits, bsign, s - m);
in (BIGINT(aext), BIGINT(bext)) end;

fun xor(a, b) = (a andalso (not b)) orelse (b andalso (not a));

(* full adder circuit : returns sum and carry *)
fun fulladder(a, b, c) = 
let
  val carry = (a andalso b) orelse (b andalso c) orelse (c andalso a);
  val sum = xor(a, xor(b, c));
in (sum, carry) end;

(* adder circuit : adds 2 bit lists and returns carry *)
(* assert |as| = |bs| *)
fun adder(x::xs, y::ys, cin) = 
      let val (s, cout) = fulladder(x, y, cin);
      in s :: adder(xs, ys, cout) end
  | adder(x, y, c) = []; 


(* creates a bigint from a bitstring of '0's and '1's *)
fun frombitstring(bstr) = 
let
  fun fromstring(#"1"::str) = true::fromstring(str)
    | fromstring(#"0"::str) = false::fromstring(str)
    | fromstring(s) = []; 
in BIGINT(fromstring(String.explode(bstr))) end;

(* prints a bit representation of the number *)
fun bitstring(b) = 
let 
  fun tostring(true::x) = "1" ^ tostring(x)
    | tostring(false::x) = "0" ^ tostring(x)
    | tostring([]) = ""
in tostring(getBits(b)) end;

(* complements a bool list *)
fun complement(b::bs) = not b :: complement(bs)
  | complement([]) = [];

(* creates a twos complements of a bool list *)
fun complement2(b) =
let
  val a = frombitstring("0");
  val bbitscomp = complement(getBits(b)); 
  val sign = getSign(b);
  val bcomp = BIGINT(bbitscomp);
  val (aa, bb) = equalizer(a, bcomp);
  val d = adder(getBits(aa), getBits(bb), true);
 in BIGINT(d) end;

fun abs(x) = 
let
  val y = List.last(getBits(x));
in 
  if(y = false) then x else complement2(x) end;


(* --------------------end of utility functions --------------------------*)

(* converts a int to bigint *)
fun getbigint(n) = 
let 
  val sign = if(n >= 0) then false else true;
  val abso = if(n >= 0) then n else ~n;
  val bits = bintegerize(abso);
in 
  if(sign = false) then BIGINT(bits) 
  else complement2(BIGINT(bits))
end;

(* returns the negative of a bigint *)
fun unminus(a) = complement2(a);

fun add(a, b) = 
let
  val (aa, bb) = equalizer(a, b);
in BIGINT(adder(getBits(aa), getBits(bb), false)) end;

fun sub(a, b) = 
let
  val b = complement(getBits(b));
  val (aa, bb) = equalizer(a, BIGINT(b));
in BIGINT(adder(getBits(aa), getBits(bb), true)) end;

(* test suite *)
val x = getbigint(9);
val y = getbigint(~3);
val w = bitstring(x);
val w = bitstring(y);
val w = bitstring(sub(x, y));
val w = bitstring(abs(y));
val w = bitstring(abs(x));
val (x, y) = greek(x, y);
val x = bitstring(x);
val y = bitstring(y);

val _ = OS.Process.exit(OS.Process.success);
