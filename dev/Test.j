; Produced by NeoJasminVisitor (tinapoc)
; http://tinapoc.sourceforge.net
; The original JasminVisitor is part of the BCEL
; http://jakarta.apache.org/bcel/
; Sat Mar 28 11:27:42 EDT 2015

.bytecode 51.0
.source Test.java
.class public Test
.super java/lang/Object

.field public static hello Ljava/lang/String;
.field public static int_array [I



.method public <init>()V
    .limit stack 1
    .limit locals 1
    .var 0 is this LTest; from Label0 to Label1

    
    Label0:
.line 1
       0: aload_0
       1: invokespecial java/lang/Object/<init>()V

    Label1:
       4: return

.end method



.method public static greetings(Ljava/lang/String;)Ljava/lang/String;
    .limit stack 2
    .limit locals 1
    .var 0 is arg0 Ljava/lang/String; from Label0 to Label1

    
    Label0:
.line 8
       0: new java/lang/StringBuilder
       3: dup
       4: invokespecial java/lang/StringBuilder/<init>()V
       7: getstatic Test.hello Ljava/lang/String;
      10: invokevirtual java/lang/StringBuilder/append(Ljava/lang/String;)Ljava/lang/StringBuilder;
      13: ldc " "
      15: invokevirtual java/lang/StringBuilder/append(Ljava/lang/String;)Ljava/lang/StringBuilder;
      18: aload_0
      19: invokevirtual java/lang/StringBuilder/append(Ljava/lang/String;)Ljava/lang/StringBuilder;
      22: invokevirtual java/lang/StringBuilder/toString()Ljava/lang/String;

    Label1:
      25: areturn

.end method



.method public static main([Ljava/lang/String;)V
    .limit stack 3
    .limit locals 2
    .var 0 is arg0 [Ljava/lang/String; from Label2 to Label3

    
    Label2:
.line 12
       0: getstatic Test.int_array [I
       3: iconst_0
       4: bipush 42
       6: iastore

    .line 13
       7: getstatic Test.int_array [I
      10: iconst_1
      11: bipush 10
      13: iastore

    .line 15
      14: iconst_1
      15: istore_1

    Label1:
      16: iload_1
      17: iflt Label0

    .line 16
      20: getstatic java.lang.System.out Ljava/io/PrintStream;
      23: getstatic Test.int_array [I
      26: iload_1
      27: iaload
      28: invokevirtual java/io/PrintStream/println(I)V

    .line 15
      31: iinc 1 -1
      34: goto Label1

    Label0:
.line 19
      37: getstatic java.lang.System.out Ljava/io/PrintStream;
      40: sipush 1036
      43: invokevirtual java/io/PrintStream/println(I)V

    .line 20
      46: getstatic java.lang.System.out Ljava/io/PrintStream;
      49: ldc "world!"
      51: invokestatic Test/greetings(Ljava/lang/String;)Ljava/lang/String;
      54: invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V

    Label3:
.line 21
      57: return

.end method



.method static <clinit>()V
    .limit stack 1
    .limit locals 0

    
    .line 3
       0: ldc "hello"
       2: putstatic Test.hello Ljava/lang/String;

    .line 4
       5: iconst_2
       6: newarray int
       8: putstatic Test.int_array [I
      11: return

.end method
