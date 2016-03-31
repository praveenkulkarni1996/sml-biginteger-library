(* it was my birthday on 24th march 2016, I turned 20 *)
(* and I am doing my PL assignment *weeps* *)

(* bigint is a list of bits and a sign bit *)
(* LSB is the leftmost bit, RSB is the rightmost bit *)
(* integers are stored in two's complement form *)

signature BigInt =
sig
  type bigint
  val getbigint: int -> bigint
  val bi2str : bigint -> string
  val str2bi : string -> bigint
  val lt : bigint * bigint -> bool 
  val leq : bigint * bigint -> bool 
  val gt : bigint * bigint -> bool 
  val geq : bigint * bigint -> bool 
  val eq : bigint * bigint -> bool 
  val neq : bigint * bigint -> bool
  val add : bigint * bigint -> bigint 
  val sub : bigint * bigint -> bigint 
  val mul : bigint * bigint -> bigint 
  val div4bigint : bigint * bigint -> bigint 
  val mod4bigint : bigint * bigint -> bigint 
  val unminus : bigint -> bigint
end;

structure BigInt:BigInt = 
struct
  datatype bigint = BIGINT of bool list;
  type bigint = bigint;

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

  (* helper xor function *)
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

  (* returns the absolute value of a biginteger *)
  fun abs(x) = 
  let val y = List.last(getBits(x));
  in if(y = false) then x else comp2(x) end;

  (* takes the and of a bool list with a bool*)
  (* used as a subroutine in the multiply() function *)
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

  (* checks for less than and less than equal to *)
  fun checkless(x::xs, y::ys, prev) = 
    if(x = true andalso y = false) then checkless(xs, ys, false)
    else if(x = false andalso y = true) then checkless(xs, ys, true)
    else checkless(xs, ys, prev)
    | checkless([], y::ys, prev) = 
    if(y = true) then true else checkless([], ys, prev)
    | checkless(x::xs, [], prev) = 
    if(x = true) then false else checkless(xs, [], prev)
    | checkless([], [], prev) = prev;

  (* --------------------end of utility functions --------------------------*)

  (* getbigint : int -> bigint *)
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
  (* bigint -> bigint *)
  fun unminus(a) = comp2(a);

  (* tests for equality *)
  fun eq(a, b) =
  let
    val aa = normal(getBits(a));
    val bb = normal(getBits(b));
  in (aa = bb) end;

  (* tests for inequality *)
  fun neq(a, b) = not (eq(a, b));

  (* tests if a < b *)
  fun lt(a, b) = 
  let
    val aa = getBits(abs(a));
    val bb = getBits(abs(b));
    val sigA = getSign(a);
    val sigB = getSign(b);
    val res = checkless(aa, bb, false);
  in 
    if(res andalso sigB) then false
    else if(not(res) andalso sigA andalso not(sigB)) then true
    else res 
  end;

  (* tests if a <= b*)
  fun leq(a, b) = eq(a, b) orelse lt(a, b);

  (* tests if a > b *)
  fun gt(a, b) = not(leq(a, b));

  (* tests if a >= b *)
  fun geq(a, b) = not(lt(a, b));

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

  (*----------------- for conversions -----------------*)

  (* converts a string to a list of intergers *)
  fun str2intlist(str) = 
  let 
    fun sometoint(x) = Option.getOpt(x, 0);
    val strlist = List.map (Char.toString) (String.explode(str));
    val optlist = List.map (Int.fromString) (strlist);
    val intlist = List.map (sometoint) (optlist);
  in intlist end;

  (* converts a string into a bigint *)
  fun str2bi(str) =
  let
    (* converts a list of integers to a biginteger *)
    fun mul10add(x, []) = x
      | mul10add(x, y::ys) = 
      mul10add(add(mul(x, getbigint(10)), getbigint(y)), ys);
    val sign = List.hd(String.explode(str)) = #"~";
    val intlist = str2intlist(str);
    val magnitude = mul10add(getbigint(0), intlist);
  in 
    if(sign) then comp2(magnitude) else magnitude
  end;


  (*----------------- to string -----------------------*)

  type NIBBLE = bool list;
  (* shifts a nibble to the left *)

  (* converts a bool to a nibble *)
  fun boolToNibble(c):NIBBLE = c::false::false::false::[];

  (* shifts a nibble *)
  fun shiftNibbleLeft(nibble:NIBBLE, c) = 
    ((c :: List.take(nibble, 3)):NIBBLE, (List.last nibble));

  val x = boolToNibble(false);
  val y = shiftNibbleLeft(x, true);

  (* greater than 5 *)
  fun greaterThanFive(n:NIBBLE) = 
    List.nth(n, 3) orelse ((List.nth(n, 2) andalso (List.nth(n, 1) orelse List.nth(n, 0))));

  (* converts a nibble =  4 bits to a int *)
  fun nibbleToInt(n:NIBBLE) =
  let 
    fun boolToInt(true) = 1
      | boolToInt(false) = 0;
    fun boolListToInt((n::ns):bool list) = boolToInt(n) + 2 * boolListToInt(ns)
      | boolListToInt([]) = 0;
  in boolListToInt(n) end;

  (* converts an integer to a nibble *)
  fun intToNibble(i:int) = 
  let
    fun helper(0) = []
      | helper(n) = if(n mod 2 = 1) then true::helper(n div 2) else false::helper(n div 2);
    val ans = helper(i);
    val n = List.length ans;
  in 
    if(n = 0) then false::false::false::false::[]
    else if(n = 1) then ans @ false::false::false::[]
    else if(n = 2) then ans @ false::false::[]
    else if(n = 3) then ans @ false::[]
    else ans 
  end;

  (* adds three to a nibble *)
  fun addThree(n:NIBBLE) =
  let
    val intn = nibbleToInt(n);
    val nn = intToNibble(intn + 3);
  in nn end;

  (* shifts a nibble vector and inserts stuff*)
  fun shiftLeft(nibvec, enter) = 
  let
    fun fixer(nv, c) = 
      let fun shifter([], c) = ([], c)
            | shifter(n::nv, c) = 
              let val (newn, newc) = shiftNibbleLeft(n, c);
                  val (newnv, newnewc) = shifter(nv, newc);
              in (newn::newnv, newnewc) end;
          val (shifted, overflow) = shifter(nv, c);
          val answer = if(overflow) then shifted @ [boolToNibble(true)] else shifted; 
    in answer end;
  in fixer(nibvec, enter) end;

  (* runs the bool list *)
  fun runBoolList(bitseq) = 
  let 
    (* performs add3 if greater than 5 throuh out the nibble vector *)
    fun resolve([]) = []
      | resolve(n::nv) = 
        let val fixed = if(greaterThanFive(n)) then addThree(n) else n;
        in fixed::resolve(nv) end;
    (* actually does the running around *)
    fun helper(nv, b::c::bs) = helper(resolve(shiftLeft(nv, b)), c::bs)
      | helper(nv, b::[]) = shiftLeft(nv, b)
      | helper(nv, []) = nv;
  in helper(boolToNibble(false)::[], bitseq) end;

  fun bi2str(b) = 
  let
    val mag = getBits(abs(b));
    val sign = getSign(b);
    fun prefix(false) = ""
      | prefix(true) = "~";

    fun nv2iv([]) = []
      | nv2iv(n::nv) = nibbleToInt(n)::nv2iv(nv);

    fun iv2str([]) = ""
      | iv2str(h::t) = Int.toString(h) ^ iv2str(t);
  in prefix(sign) ^ iv2str(List.rev (nv2iv(runBoolList(List.rev mag)))) end;

  (*------ for divmod -----*)
  fun cansub(a, b) = geq(BIGINT(List.rev a @ [false]), BIGINT(List.rev b @ [false]));
  fun subtract(a, b) = List.rev (getBits(sub(BIGINT(List.rev a @ [false]), BIGINT(List.rev b @ [false]))));

  fun divmod(lt, [], divizor) = 
    if(cansub(lt, divizor)) then ([true], subtract(lt, divizor)) 
    else  ([false], lt) 
    | divmod(lt, r::rs, divizor) = 
      if(cansub(lt, divizor)) then
        let val (q, r) = divmod(subtract(lt, divizor) @ [r], rs, divizor);
        in (true::q, r) end
      else 
        let val (q, r) = divmod(lt @ [r], rs, divizor);
        in (false::q, r) end;

  exception dividebyzero;

  (* division *)
  fun div4bigint(a, b) = 
    if(eq(b, getbigint(0))) then raise dividebyzero
    else 
      let
        val aa = abs a;
        val bb = abs b;
        val cc = BIGINT(normal(List.rev (#1(divmod([], List.rev(getBits(aa)), List.rev(getBits(bb)))))))
        val sig_a = getSign a;
        val sig_b = getSign b;
      in 
        if(sig_a = sig_b) then cc
        else unminus(cc)
      end;

  (* modulus *)
  fun mod4bigint(a, b) = 
    if(eq(b, getbigint(0))) then raise dividebyzero
    else 
      let
        val aa = abs a;
        val bb = abs b;
        val cc = BIGINT(normal(List.rev (#2(divmod([], List.rev(getBits(aa)), List.rev(getBits(bb)))))));
        val sig_a = getSign a;
        val sig_b = getSign b;
      in 
        if(sig_a) then unminus(cc) else cc
      end;
end;

