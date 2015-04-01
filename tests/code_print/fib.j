; Produced by NeoJasminVisitor (tinapoc)
; http://tinapoc.sourceforge.net
; The original JasminVisitor is part of the BCEL
; http://jakarta.apache.org/bcel/
; Tue Mar 31 23:44:34 EDT 2015

.bytecode 51.0
.source fib.java
.class public fib
.super java/lang/Object




.method public <init>()V
    .limit stack 1
    .limit locals 1
    .var 0 is this Lfib; from Label0 to Label1

    
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
       3: bipush 45
       5: invokestatic fib/fib(I)I
       8: invokevirtual java/io/PrintStream/println(I)V

    Label1:
.line 8
      11: return

.end method



.method public static fib(I)I
    .limit stack 3
    .limit locals 1
    .var 0 is arg0 I from Label1 to Label2

    
    Label1:
.line 11
       0: iload_0
       1: iconst_2
       2: if_icmpge Label0

    .line 12
       5: iload_0
       6: ireturn

    Label0:
.line 14
       7: iload_0
       8: iconst_1
       9: isub
      10: invokestatic fib/fib(I)I
      13: iload_0
      14: iconst_2
      15: isub
      16: invokestatic fib/fib(I)I
      19: iadd

    Label2:
      20: ireturn

.end method
