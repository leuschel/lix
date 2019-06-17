#!/bin/bash

mkdir -p tests

sicstus -l make_selfann.pl --goal "go,halt." &> /dev/null

## append test

testLix() {
    rm tests/$File*
    echo $Ans > tests/$File.ans
    sicstus -l lix_main.pl --goal "Call = $Call,lix(Call, R1), portray_clause(':-'(Call,R1)),halt." 2> /dev/null > tests/$File
    sicstus -l tests/$File --goal "findall($CallOut,$Call,Bag), print(Bag),nl,halt." 2> /dev/null > tests/$File.out
	
    if ( ! diff -q tests/$File.ans tests/$File.out )
    then
	echo "######Failed on test Case $Call"
    else
	echo "OK"
    fi
}


testLixGX() {

    LCall=lix\($Call,Res\)
    rm tests/$File*
    echo $Ans > tests/$File.ans

    sicstus -l lix_main.pl --goal "Call = $LCall,lix(Call, R1), portray_clause(':-'(Call,R1)),halt." 2> /dev/null > tests/$File
    sicstus -l tests/$File --goal "$LCall, portray_clause(':-'($Call,Res)),halt." 2> /dev/null > tests/$File.spec    
    sicstus -l tests/$File.spec --goal "findall($CallOut,$Call,Bag),print(Bag),nl,halt." 2> /dev/null > tests/$File.out
        	
    if ( ! diff -q tests/$File.ans tests/$File.out )
    then
	echo "######Failed on test Case $Call"
    else
	echo "OK"
    fi
}

testLixCogen() {
    LCall=lix\($Call,Res\)
    CogCall=lix\($LCall,Res1\)


    rm tests/$File*
    echo $Ans > tests/$File.ans    
   #sicstus -l lix_main.pl --goal "Call = $CogCall,lix(Call, R1), portray_clause(':-'(Call,R1)),halt." 2> /dev/null > tests/$File.cog
    
    #sicstus -l tests/$File.cog --goal "Call = $LCall,lix(Call, R1), portray_clause(':-'(Call,R1)),halt." 2> /dev/null > tests/$File.gx

    sicstus -l tests/$LixCogen --goal "cogen($Call,R,R1), portray_clause(':-'(gx($Call,R),R1)),halt." 2> /dev/null > tests/$File.gx

    sicstus -l tests/$File.gx --goal "gx($Call,R1), portray_clause(':-'($Call,R1)),halt." 2> /dev/null > tests/$File.spec    
    #sicstus -l tests/$File.gx --goal "$LCall, portray_clause(':-'($Call,Res)),halt." 2> /dev/null > tests/$File.spec    
    sicstus -l tests/$File.spec --goal "findall($CallOut,$Call,Bag),print(Bag),nl,halt." 2> /dev/null > tests/$File.out
        	
    if ( ! diff -q tests/$File.ans tests/$File.out )
    then
	echo "Failed"
	echo "Error on test case $Call"
    else
	echo "OK"
    fi
}

makeLixCogen() {

    sicstus -l lix_main.pl --goal "lix(lix(lix(A,R1),R2),R3), portray_clause(':-'(cogen(A,R1,R2),R3)),halt." 2> /dev/null > tests/$LixCogen
    
}


Indent="                   "

echo "Testing Lix mode-->"

# Append Test
echo -n "$Indent app/3..."
File=app.pl
Call=app\([a,b,c,d],[b],C\)    
CallOut=C
Ans=[[a,b,c,d,b]]
testLix

# Append Test
echo -n "$Indent fib/2..."
File=fib.pl
Call=fib\(6,X\)    
CallOut=X
Ans=[13]
testLix


echo "Testing GX mode-->"
echo -n "$Indent app/3..."
#app_gx
File=appgx.pl
Call=app\([a,b,c,d],[b],C\)
CallOut=C
Ans=[[a,b,c,d,b]]
testLixGX

echo -n "$Indent fib/2..."
#app_gx
File=benchgx.pl
Call=fib\(5,X\)
CallOut=X
Ans=[8]
testLixGX



echo "Testing Cogen mode->"
LixCogen='lix-cogen.pl'
makeLixCogen

echo -n "$Indent app/3..."
File=appcog.pl
Call=app\([a,b,c,d],[b],C\)
CallOut=C
Ans=[[a,b,c,d,b]]
testLixCogen

echo -n "$Indent test/1..."
File=testcog.pl
Call=test\(A\)
CallOut=A
Ans=[a,b]
testLixCogen

echo -n "$Indent fib/2..."
File=fibcog.pl
Call=fib\(5,X\)
CallOut=X
Ans=[8]
testLixCogen





