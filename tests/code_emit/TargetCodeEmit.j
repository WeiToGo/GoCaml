.source code_emitter_test.java
.class public CodeEmitTest
.super java/lang/Object

.field public static str_field Ljava/lang/String;
.field public static int_field I

.method public <init>()V
  aload_0
  invokespecial java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
  .limit locals 25
  .limit stack 25
  getstatic java/lang/System/out Ljava/io/PrintStream;
  bipush 42
SillyLabel:
  invokevirtual java/io/PrintStream/println(I)V
  return
.end method

.method static <clinit>()V
  .limit locals 25
  .limit stack 25
  ldc "hello world!"
  putstatic CodeEmitTest/str_field Ljava/lang/String;
  bipush 0
  putstatic CodeEmitTest/int_field I
  return
.end method

