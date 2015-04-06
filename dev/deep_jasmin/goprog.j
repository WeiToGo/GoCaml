.class public genstructs/II[L
.super java/lang/Object

.field public static hell Ljava/lang/String;
.field public static int_array [I
;
; standard initializer (calls java.lang.Object's initializer)
;
.method public <init>()V
  aload_0
  invokenonvirtual java/lang/Object/<init>()V
  return
.end method


.method public static greetings(Ljava/lang/String;)Ljava/lang/String;
  .limit stack 2
  .limit locals 1

  ; Adding the strings
  new java/lang/StringBuilder
  dup
  invokespecial java/lang/StringBuilder/<init>()V
  getstatic GoProg/hell Ljava/lang/String;
  invokevirtual java/lang/StringBuilder/append(Ljava/lang/String;)Ljava/lang/StringBuilder;
  ldc " "
  invokevirtual java/lang/StringBuilder/append(Ljava/lang/String;)Ljava/lang/StringBuilder;
  aload_0
  invokevirtual java/lang/StringBuilder/append(Ljava/lang/String;)Ljava/lang/StringBuilder;
  invokevirtual java/lang/StringBuilder/toString()Ljava/lang/String;
  areturn
.end method

; main() - prints out Hello World
;
.method public static main([Ljava/lang/String;)V
  .limit stack 3
  .limit locals 2
    ; initialize the array
    getstatic GoProg/int_array [I
    iconst_0
    ldc 42
    iastore

    getstatic GoProg/int_array [I
    iconst_1
    ldc 10
    iastore

  ; Loop 1 init
    bipush 1
    istore_1

  Loop_1_Check:
    iload_1
    bipush 0
    if_icmpge True_0
  False_0:
    iconst_0
    ; ; Uncomment following for testing if_icmpge
    ; getstatic java/lang/System/out Ljava/io/PrintStream;
    ; ldc "False!"
    ; invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
    goto EndBoolExp_0
  True_0:
    iconst_1
    ; ; Uncomment following for testing if_icmpge
    ; getstatic java/lang/System/out Ljava/io/PrintStream;
    ; ldc "True!"
    ; invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
  EndBoolExp_0:
    ifeq Loop_1_End
  Loop_1_Begin:
    getstatic java/lang/System/out Ljava/io/PrintStream;
    getstatic GoProg/int_array [I
    iload_1
    iaload
    invokevirtual java/io/PrintStream/println(I)V

    iinc 1 -1
    goto Loop_1_Check
  Loop_1_End: 
  ; ; push System.out onto the stack
  getstatic java/lang/System/out Ljava/io/PrintStream;

  ; Calculate the value of the expression
  ldc 45
  ldc 23
  imul
  ldc 1
  iadd

  ; call the PrintStream.println() method.
  invokevirtual java/io/PrintStream/println(I)V

  getstatic java/lang/System/out Ljava/io/PrintStream;
  ldc "world!"
  invokestatic GoProg/greetings(Ljava/lang/String;)Ljava/lang/String;
  invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V

  getstatic java/lang/System/out Ljava/io/PrintStream;
  ldc 2.1
  f2d
  invokevirtual java/io/PrintStream/println(D)V

  

  ; done
  return
.end method

.method static <clinit>()V
  .limit stack 1
  .limit locals 0

  ldc "hello"
  putstatic GoProg/hell Ljava/lang/String;

  ldc 2
  newarray int
  putstatic GoProg/int_array [I
  return 

.end method

.method static dummy()V
  .limit stack 0
  .limit locals 0
  return
.end method
