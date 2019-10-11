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
val q:int=getInt();
val vec=getIntVector(n);
val p:int=Vector.length(vec);
fun findposi(t:int,vec:int vector,start:int,endl:int)=
if start = endl then 
if Vector.sub(vec,endl)=t then endl
else ~1
else if Vector.sub(vec,(start+endl) div 2)=t then (start+endl) div 2
else if Vector.sub(vec,(start+endl) div 2)>t then findposi(t,vec,start,(start+endl) div 2)
else findposi(t,vec,(start+endl) div 2+1,endl);
fun printout(q:int,s:(int)list)=
if q<>0 then printout(q-1,findposi(getInt(),vec,0,p-1)::s)
else s;
val t:(int)list=printout(q,nil);
printIntTable(List.rev(t));
(*****End*****)