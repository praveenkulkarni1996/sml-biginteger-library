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

(* converts 0/1 to true/false *)
fun bitmake(i:int) = if(i = 0) then false else true;

(* converts a positive number to its bool list form *)
fun bintegerize(n) = if(n = 0) then false::false::[] 
                     else (bitmake(n mod 2)) :: bintegerize(n div 2); 

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

(* converts a bool list into normal form *)
(* a normal form has at least 3 bits *)
(* if it has more than 3 bits then it is of the form x::x::y::zs *)
fun normal(x) = 
let
  fun norm([]) = false::false::false::[]
    | norm(x::[]) = x::x::x::[]
    | norm(x::y::[]) = x::x::y::[]
    | norm(x::y::z::s) = if(x <> y) then x::x::y::z::s 
                         else if (x = y andalso y <> z) then x::y::z::s
                         else if(x = y andalso y = z) then norm(y::z::s)
                         else [];
  val normalform = List.rev(norm(List.rev x));
in normalform end;

(* takes a pair of bit sequences, and does signextension until they are both of
 * equal length, this works in linear time.
 * Named after the greek monster who did this sort of stuff.
 *)

fun greek(x, y) = 
let
  fun equator([], [], xp, yp) = ([], [])
    | equator(x::xs, [], xp, yp) = 
      let val (a, b) = equator(xs, [], x, yp) in (x::a, yp::b) end
    | equator([], y::ys, xp, yp) = 
      let val (a, b) = equator([], ys, xp, y) in (xp::a, y::b) end
    | equator(x::xs, y::ys, xp, yp) = 
      let val (a, b) = equator(xs, ys, x, y) in (x::a, y::b) end;
  val (xx, yy) = equator(x, y, false, false)
in (xx, yy) end;

fun xor(a, b) = (a andalso (not b)) orelse (b andalso (not a));

(* full adder circuit : returns sum and carry *)
fun fulladder(a, b, c) = 
let
  val carry = (a andalso b) orelse (b andalso c) orelse (c andalso a);
  val sum = xor(a, xor(b, c));
in (sum, carry) end;

(* performs addition or subtraction of bigints depending upon the
 * value of c. False => addition, True => subtraction.
 * There is no overflow. Assert that the numbers are normalized
 *)
fun addsub(x, y, c) =
let
  fun adder(x::xs, y::ys, cin) = 
        let val (s, cout) = fulladder(x, y, cin);
        in s :: adder(xs, ys, cout) end
    | adder([], [], c) = []
    | adder(x, y, c) = [];  (* this should not run *)
  val (xx, yy) = greek(normal(x), normal(y));
in adder(xx, yy, c) end;


(* complements a bool list *)
fun negate(b::bs) = not b :: negate(bs)
  | negate([]) = [];

(* creates a twos complements of a bool list *)
(* assert that it is in the normal form *)
fun comp2(b) =
let
  fun add1(x::xs, bit) = xor(x, bit)::add1(xs, x andalso bit)
    | add1([], bit) = [];
  val d = add1(negate(getBits(b)), true)
 in BIGINT(d) end;

fun abs(x) = 
let
  val y = List.last(getBits(x));
in 
  if(y = false) then x else comp2(x) end;


fun andlist(x::xs, y) = (x andalso y)::andlist(xs, y)
  | andlist([], y) = [];


(* multiply *)
fun multiply(x, BIGINT([])) = BIGINT(false::[]) (* ZERO *)
  | multiply(x, y) = 
    let 
      val a = BIGINT(andlist(getBits(x), List.hd(getBits(y))));
      val b = multiply(x, BIGINT(List.tl(getBits(y))));
      val c = BIGINT(addsub(getBits(a), false::getBits(b), false));
    in c end;


(* --------------------end of utility functions --------------------------*)

(* converts a int to bigint *)
fun getbigint(n) = 
let 
  val sign = if(n >= 0) then false else true;
  val abso = if(n >= 0) then n else ~n;
  val bits = bintegerize(abso);
in 
  if(sign = false) then BIGINT(bits) 
  else comp2(BIGINT(bits))
end;

(* returns the negative of a bigint *)
fun unminus(a) = comp2(a);

(* adds two bigintegers *)
fun add(a, b) = 
let
  val aa = getBits(a);
  val bb = getBits(b);
  val ans = normal(addsub(aa, bb, false));
in BIGINT(ans) end;
  

(* subtracts two bigintegers *)
fun sub(a, b) = 
let
  val aa = getBits(a);
  val bb = getBits(b);
  val ans = normal(addsub(aa, negate(bb), true));
in BIGINT(ans) end;

(* multplies two bigintegers *)
fun mul(a, b) = 
let
  val sign = not(getSign(a) = getSign(b));
  val magnitude = multiply(abs(a), abs(b));
in 
  if(sign = false) then magnitude else comp2(magnitude)
end;

val a = getbigint(~15);
val b = getbigint(~15);
val aa = getBits(a);
val bb = getBits(b);
val aa = bitstring(BIGINT(normal(aa)));
val bb = bitstring(BIGINT(normal(bb)));
val cc = bitstring(mul(a, b));
val _ = OS.Process.exit(OS.Process.success);
