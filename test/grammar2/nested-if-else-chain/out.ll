; ModuleID = 'spl'
source_filename = "spl"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define i64 @main() {
entry:
  %1 = alloca i64, align 8
  store i64 0, ptr %1, align 8
  %2 = icmp ne i64 0, 0
  br i1 %2, label %if.then.1, label %if.else.3
if.then.1:
  store i64 1, ptr %1, align 8
  br label %if.end.2
if.else.3:
  %3 = icmp ne i64 1, 0
  br i1 %3, label %if.then.4, label %if.else.6
if.then.4:
  store i64 2, ptr %1, align 8
  br label %if.end.5
if.else.6:
  store i64 3, ptr %1, align 8
  br label %if.end.5
if.end.5:
  br label %if.end.2
if.end.2:
  %4 = load i64, ptr %1, align 8
  ret i64 %4
}
