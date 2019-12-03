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
val n=getInt();
val Li:(int)list=getIntTable(n);
fun stack(L,[],right,max)=
if right=n then max
else
  if List.hd(L)=0 then 
  stack(List.tl(L),[right],right+1,max)
  else
  stack(List.tl(L),[],right+1,max)

    |stack(L,xs,right,max)=
if right=n then max
else
  if List.hd(L)=1 then 
    if right-List.hd(xs)+1>max then stack(List.tl(L),List.tl(xs),right+1,right-List.hd(xs)+1)
    else stack(List.tl(L),List.tl(xs),right+1,max)
  else
    stack(List.tl(L),right::xs,right+1,max);
printInt(stack(Li,nil,0,0))

(*****End*****)