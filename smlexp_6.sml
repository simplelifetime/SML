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
val n1=getInt();
val n1L=getIntTable(n1);
val n2=getInt();
val n2L=getIntTable(n2);
val n1a=Array.fromList(n1L);
val n2a=Array.fromList(n2L);
val n1L1=List.rev(n1L);
val n2L2=List.rev(n2L);
val n1a1=Array.fromList(n1L1);
val n2a2=Array.fromList(n2L2);
val maxlen=if n1>n2 then n1
                    else n2;

fun plus(a1:(int)Array.array,a2:(int)Array.array,len:int,res,A)=
if len=maxlen then res::A
else 
if len>n2-1 then plus(a1,a2,len+1,0,Array.sub(a1,n1-len-1)::A)
else if len>n1-1 then plus(a1,a2,len+1,0,Array.sub(a2,n2-len-1)::A)
else 
let 
val p=Array.sub(a1,n1-len-1)+Array.sub(a2,n2-len-1)+res
val re=p mod 10
val im=p div 10
in
plus(a1,a2,len+1,im,re::A)
end;

fun reduce(a1:(int)Array.array,a2:(int)Array.array,len:int,res,A:(int)list)=
if len=maxlen then A
else 
if len>n2-1 then reduce(a1,a2,len+1,0,(Array.sub(a1,n1-len-1)+res)::A)
else 
let 
val p=Array.sub(a1,n1-len-1)-Array.sub(a2,n2-len-1)+res
val re=p mod 10
val im=p div 10
in
reduce(a1,a2,len+1,im,re::A)
end;

val s=Array.array(n1+n2+1,0)

fun multi(a1,a2,len1,len2,res,res1,A)=
if len1=n1 then 
let val p=res1
in
Array.update(A,n1+n2-1,res1);
A
end

else if len2=n2-1 then 
let 
val k=len1+len2
val p=Array.sub(a1,len1)*Array.sub(a2,len2)+Array.sub(A,len1+len2)+res+res1
val s1=p div 10
val s2=p mod 10
in
Array.update(A,len1+len2,s2);
multi(a1,a2,len1+1,0,0,s1,A)
end

else 
let 
val k=len1+len2
val p=Array.sub(a1,len1)*Array.sub(a2,len2)+Array.sub(A,len1+len2)+res
val s1=p div 10
val s2=p mod 10
in
Array.update(A,len1+len2,s2);
multi(a1,a2,len1,len2+1,s1,res1,A)
end;

val so=ref 10;

val result1=ref (plus(n1a,n2a,0,0,nil));
val result2=ref (reduce(n1a,n2a,0,0,nil));
if List.hd(!result1)=0 andalso List.length(!result1)>1 then result1 := List.tl(!result1)
else so := !so;
printIntTable(!result1);
printEndOfLine();

if n1=0 andalso n2=0 then printInt(0)
else
while List.hd(!result2)=0 andalso List.length(!result2)>1 do 
(result2 := List.tl(!result2));
printIntTable(!result2);

printEndOfLine();

if n1=0 andalso n2=0 then printInt(0)
else
let
val result3=multi(n1a1,n2a2,0,0,0,0,s);
val k=ref (n1+n2);
val p=ref 0;
in
while !k > ~1 do
(
  if Array.sub(result3,!k)=0 andalso !p=0 then so := !so
  else 
  (p := 1;
  printInt(Array.sub(result3,!k)));
  k := !k - 1
)
end;
(*****End*****)
