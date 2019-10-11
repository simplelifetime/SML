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

(*****Begin*****)
val m:int=getInt();
val s:(int)list=getIntTable(m);

fun maxx(a:int,b:int):int=
if a>b then a
else b

fun maxt(p::sx:(int)list,maxp:int):int=
if null(sx)=true then maxx(p,maxp)
else if p>maxp then maxt(sx,p)
else maxt(sx,maxp)

fun maxs(t:(int)list):int=
let
val pop:int=if null(t)=true then ~1
else maxt(t,0)
in pop
end;

val s:int=maxs(s);
if s <> ~1 then printInt(s)
else print("Empty Table");

(*****End*****)