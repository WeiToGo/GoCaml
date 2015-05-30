;This file is part of GoCaml, a compiler that compiles Go-Lite (a subset of Go) to Java bytecode. 

;Copyright (C) 2015 by Deepanjan Roy, Wei Gao, Omar Gonzalez 


;GoCaml is free software: you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation, either version 3 of the License, or
;(at your option) any later version.

;GoCaml is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.

;You should have received a copy of the GNU General Public License
;along with Foobar.  If not, see <http://www.gnu.org/licenses/>. 

;This code originated as a project for the COMP 520 class at McGill University
;in Winter 2015. Any subsequent COMP 520 student who is viewing this code must 
;follow the course rules and report any viewing and/or use of the code.



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

