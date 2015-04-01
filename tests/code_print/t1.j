; Produced by NeoJasminVisitor (tinapoc)
; http://tinapoc.sourceforge.net
; The original JasminVisitor is part of the BCEL
; http://jakarta.apache.org/bcel/
; Wed Apr 01 12:37:05 EDT 2015

.bytecode 51.0
.source t1.java
.class public t1
.super java/lang/Object




.method public <init>()V
    .limit stack 1
    .limit locals 1
    .var 0 is this Lt1; from Label0 to Label1

    
    Label0:
.line 1
       0: aload_0
       1: invokespecial java/lang/Object/<init>()V

    Label1:
       4: return

.end method



.method public static main([Ljava/lang/String;)V
    .limit stack 2
    .limit locals 1
    .var 0 is arg0 [Ljava/lang/String; from Label0 to Label1

    
    Label0:
.line 6
       0: getstatic java.lang.System.out Ljava/io/PrintStream;
       3: invokestatic t1/foo()I
       6: invokevirtual java/io/PrintStream/println(I)V

    Label1:
.line 8
       9: return

.end method



.method public static foo()I
    .limit stack 1
    .limit locals 0

    
    .line 12
       0: bipush 6
       2: ireturn

.end method
