fun printInt (a:int) =
    print(Int.toString(a)^" ");

fun getInt () =
    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);

fun printIntTable ( [] ) = ()
  | printIntTable ( x::xs ) = 
    let
	val tmp = printInt(x)
    in
	printIntTable(xs)
    end;

fun getIntTable ( 0 ) = []
  | getIntTable ( N:int) = getInt()::getIntTable(N-1);

(*****Begin*****)			 
val n:int=getInt();
val list:(int)list=getIntTable(n);
fun mergesort [] = []
    |mergesort [Li] = [Li]
    |mergesort(Li:(int)list)=
let 
val mid:int = length(Li) div 2
fun merge([],y)=y     (*从小到大依次进行merge*)
    |merge(x,[])=x
    |merge(x::xs,y::ys)=
    if x<y then x::merge(xs,y::ys)
    else y::merge(x::xs,ys)
in merge(mergesort(List.take(Li,mid)),mergesort(List.drop(Li,mid)))
end;
val result:(int)list=mergesort(list);
printIntTable(result);

(*****End*****)