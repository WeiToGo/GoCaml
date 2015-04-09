.source binary_expressions.go
.class public RuntimeSupport
.super java/lang/Object


.method public <init>()V
  aload_0
  invokespecial java/lang/Object/<init>()V
  return
.end method


.method public static booltostring(I)Ljava/lang/String;
  .limit stack 1
  .limit locals 1
  iload_0
  ifeq False_0
True_0:
  ldc "true"
  goto EndExp_0
False_0:
  ldc "false"
EndExp_0:
  areturn

.end method

