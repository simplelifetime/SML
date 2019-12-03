fun printInt (a:int) =
    print(Int.toString(a)^" ");

fun printIntInf (a:IntInf.int) =
    print(IntInf.toString(a)^" ");


fun printReal (a:real) =
    print(Real.toString(a)^" ");

fun printString (a:string) =
    print(a^" ");

fun getInt () =
    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);

fun getIntInf () =
    Option.valOf (TextIO.scanStream (IntInf.scan StringCvt.DEC) TextIO.stdIn);

fun getReal () =
    Option.valOf (TextIO.scanStream (Real.scan) TextIO.stdIn);

fun printEndOfLine () =
    print("\n");

fun printIntTable ( [] ) = ()
  | printIntTable ( x::xs ) = 
    let
	val tmp = printInt(x)
    in
	printIntTable(xs)
    end;

fun printIntInfTable ( [] ) = ()
  | printIntInfTable ( x::xs ) = 
    let
	val tmp = printIntInf(x)
    in
	printIntInfTable(xs)
    end;

fun getIntTable ( 0 ) = []
  | getIntTable ( N:int) = getInt()::getIntTable(N-1);

fun getIntInfTable ( 0 ) = []
  | getIntInfTable ( N:int) = getIntInf()::getIntInfTable(N-1);

fun getIntVector ( 0 ) =  Vector.fromList []
  | getIntVector ( N:int) = Vector.fromList(getIntTable(N));

fun getIntInfVector ( 0 ) = Vector.fromList []
  | getIntInfVector ( N:int) = Vector.fromList(getIntInfTable(N));


(*****Begin*****)
val n=getIntInf();
val primes:(IntInf.int)list=[2,3,5,7,11,13,17,19,23,29,31,37,41];
val len=List.length(primes);
val primes:(IntInf.int)array=Array.fromList(primes);
fun divide1(p:IntInf.int,power:IntInf.int):IntInf.int=
if p mod 2 = 0 then divide1(p div 2,power+1)
else
power;

fun divide2(p:IntInf.int,power:IntInf.int):IntInf.int=
if p mod 2 = 0 then divide2(p div 2,power+1)
else
p;

val s=divide1(n-1,0);
val d=divide2(n-1,0);

fun square(x:IntInf.int):IntInf.int=x*x;

fun powermod(a:IntInf.int,b:IntInf.int,m:IntInf.int):IntInf.int=
if b=1 then a mod m
else if b mod 2 = 0 then powermod(a*a mod m,b div 2,m) mod m
else a mod m * powermod(a*a mod m,b div 2,m) mod m;

fun decide(a:IntInf.int,power:IntInf.int,times:IntInf.int,ori:IntInf.int,sum:IntInf.int)=
if sum=power+1 then 0
else 
let 
val p=powermod(a,ori*times,n)
in
if p=1 orelse p=n-1 then 1
else decide(a,power,times*2,ori,sum+1)
end;

fun ulti(n:IntInf.int,i:int)=
if i=len-1 then 1
else
let 
val k=decide(Array.sub(primes,i),s,1,d,0)
in
if k=1 then ulti(n,i+1)
else 0
end;

val pk=if n=2 then 1
else if n mod 2 =0 then 0
else ulti(n,0);
if pk=1 then print("True ")
else print("False ");
(* a为底，power为2的总阶乘数，times为2的n次方，ori为最初的原值，sum为乘的次数 *)


(*****End*****)