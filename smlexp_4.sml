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

fun printEndOfLine () =
    print("\n");

fun getIntTable ( 0 ) = []
  | getIntTable ( N:int) = getInt()::getIntTable(N-1);

fun printArray ( Arr ) =
    let
	val cur = ref 0
	val len = Array.length(Arr)
    in
	while !cur < len
	do
	(
	  printInt(Array.sub(Arr,!cur));
	  cur := !cur + 1)
    end;

fun printString( s ) = print(s ^ " ");

(*****Begin*****)
val n=ref (getInt());	
val s=ref [(0,0)];
while !n>0 do(
let
    val l1=getInt();
    val h1=getInt();
    val r1=getInt();
in
    s := (l1,~h1)::(!s);
    s := (r1,h1)::(!s);
    n := !n -1
end
);

fun dropele(t,[s])= []
|dropele(t,[])=[]
|dropele(t,[s1,s2])=
if s1=t then [s2]
else [s1]
|dropele(t,Li)=
let 
val mid=List.length(Li) div 2
val left=List.take(Li,mid)
val right=List.drop(Li,mid+1)
val midnum=List.nth(Li,mid)
in
if midnum=t then left@right
else if midnum>t then left@[midnum]@dropele(t,right)
else dropele(t,left)@[midnum]@right
end;

fun mergesortsingle [] = []
    |mergesortsingle [Li] = [Li]
    |mergesortsingle(Li:(int)list)=
let 
val mid:int = length(Li) div 2
fun merge([],y)=y     (*从小到大依次进行merge*)
    |merge(x,[])=x
    |merge(x::xs,y::ys)=
    if x>y then x::merge(xs,y::ys)
    else y::merge(x::xs,ys)
in merge(mergesortsingle(List.take(Li,mid)),mergesortsingle(List.drop(Li,mid)))
end;

fun mergesort [] = []
    |mergesort [Li:int*int] = [Li]
    |mergesort(Li:(int*int)list)=
let 
val mid:int = length(Li) div 2
fun merge([],y)=y     (*从小到大依次进行merge*)
    |merge(x,[])=x
    |merge(x::xs:(int*int)list,y::ys:(int*int)list)=
    if #1 x < #1 y then x::merge(xs,y::ys)
    else y::merge(x::xs,ys)
in merge(mergesort(List.take(Li,mid)),mergesort(List.drop(Li,mid)))
end;

val ss=ref [0];
val result=ref [(0,0)];

s := mergesort(!s);
val arr=Array.fromList(!s);
val len=Array.length(arr);
val i=ref 1;

fun abs(sss)= if sss>0 then sss
else ~sss;

while !i < len
do(
let
    val cord = #1 (Array.sub(arr,!i));
    val height = #2 (Array.sub(arr,!i));
    val maxnow=List.hd(!ss);
in
if height<0 then ss := mergesortsingle((~height)::(!ss))
else ss:= dropele(height,!ss);
if maxnow=List.hd(!ss) then result := !result
else result := (cord,abs(List.hd(!ss)))::(!result)
end;
i := !i+1
);
val mate=List.drop(List.rev(!result),1);
val ARR=Array.fromList(mate);
val lenn=Array.length(ARR);
val count=ref 0;
while !count<lenn do
(
    printInt(#1(Array.sub(ARR,!count)));
    printInt(#2(Array.sub(ARR,!count)));
    printEndOfLine();
    count := !count + 1
);
(*****End*****)