.source helloworld.go
.class public GeneratedBytecode
.super java/lang/Object


.method public <init>()V
  aload_0
  invokespecial java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
  .limit locals 25
  .limit stack 25
  getstatic java/lang/System/out Ljava/io/PrintStream;
  ldc "Hello world!"
  invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
  return
.end method

.method static <clinit>()V
  .limit locals 25
  .limit stack 25
  return
.end method

