datatype bigint = BIGINT of int list ;

fun getbigint(n:int) = n::[]; 

fun bi2str(b:bigint) = "winter is coming";

fun str2bi(s:string) = 10::[];

fun lt(a:bigint, b:bigint) = true;
fun leq(a:bigint, b:bigint) = true;
fun le(a:bigint, b:bigint) = true;
fun geq(a:bigint, b:bigint) = true;
fun eq(a:bigint, b:bigint) = true;
fun neq(a:bigint, b:bigint) = true;

fun add(a:bigint, b:bigint) = a;
fun sub(a:bigint, b:bigint) = a;
fun mul(a:bigint, b:bigint) = a;
(* fun div(a:bigint, b:bigint) = a; *)
(* fun mod(a:bigint, b:bigint) = a; *)
fun unminus(a:bigint) = a;

val _ = OS.Process.exit(OS.Process.success);


