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
val sbbbsa=ref 2;
val m=getInt();
val n=getInt();
val startpoint=getInt()-1;
val array:(int)Array2.array=Array2.array(m,m,10000000);
fun init(array:(int)Array2.array,p:int)=
if p=0 then array
else
let
val fir=getInt()-1
val sec=getInt()-1
val dis=getInt()
in
if dis < Array2.sub(array,fir,sec)
then (Array2.update(array,fir,sec,dis);Array2.update(array,sec,fir,dis))
else sbbbsa := !sbbbsa;
init(array,p-1)
end;
val jjj = ref 0;
val realarray=init(array,n);
while !jjj < m do (Array2.update(realarray,!jjj,!jjj,0);jjj := !jjj + 1);
val result=Array.array(m,10000000);
val visit=Array.array(m,~1);
val i=ref m;
Array.update(result,startpoint,0);
val s=ref startpoint;
while !i>0 do
(
  let
    val jj=ref 0
    val j=ref 0
    val choose=ref ~1
    val min=ref 100000000
  in
    while !j<m do
      (
        if Array2.sub(realarray,startpoint,!j)< !min andalso Array.sub(visit,!j)= ~1
        then (choose := !j; min := Array2.sub(realarray,startpoint,!j))
        else (choose := !choose);
        j := !j+1
      );
    Array.update(result,!choose,!min);
    Array.update(visit,!choose,0);
    s := !choose;
    i := !i - 1;
    while !jj<m do
      (
        if Array.sub(result,!s) + Array2.sub(realarray,!s,!jj) < Array2.sub(realarray,startpoint,!jj)
        then Array2.update(realarray,startpoint,!jj,Array.sub(result,!s) + Array2.sub(realarray,!s,!jj))
        else jj := !jj;
        jj := !jj + 1
      )
  end
);
val t=ref 0;
while !t < m do(
  if Array.sub(result,!t) <10000000
  then printInt(Array.sub(result,!t))
  else printInt(~1);
  t := !t + 1);
(*****End*****)