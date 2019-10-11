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
val n:int=getInt();
val m:int=getInt();
val x:int=getInt();
val y:int=getInt();
fun f(xx:int,yy:int)=
if xx=0 andalso yy=0 then 1
else if xx<0 orelse yy<0 then 0
else if xx=x andalso yy=y then 0
else if xx-2=x andalso yy-1=y then 0
else if xx-2=x andalso yy+1=y then 0
else if xx+2=x andalso yy-1=y then 0
else if xx+2=x andalso yy+1=y then 0
else if xx-1=x andalso yy-2=y then 0
else if xx-1=x andalso yy+2=y then 0
else if xx+1=x andalso yy-2=y then 0
else if xx+1=x andalso yy+2=y then 0
else f(xx-1,yy)+f(xx,yy-1);
printInt(f(n,m));
(*****End*****)
