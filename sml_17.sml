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
val n:int=getInt();
val s:(int)list=getIntTable(n);
fun findnext(xlist:(int)list,y::ys:(int)list)=
if hd(xlist)<=y then findnext(y::xlist,ys)
else
let fun get_order [x] = y::x::ys
    (* |get_order(x::xk)=
             *)
    |get_order(x::xs::xk)=
        if xs>y then x::get_order(xs::xk)  
        else (y::xs::xk)@(x::ys)
in get_order(xlist) 
end;
fun find(x::xs:(int)list)=findnext([x],xs);
printIntTable(find(s));

(*****End*****)