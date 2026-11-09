; ModuleID = 'spl'
source_filename = "spl"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define i64 @main() {
entry:
  %1 = alloca i64, align 8
  store i64 0, ptr %1, align 8
  br label %while.cond.1
while.cond.1:
  %2 = load i64, ptr %1, align 8
  %3 = icmp ne i64 %2, 0
  br i1 %3, label %while.body.2, label %while.end.3
while.body.2:
  br label %while.cond.1
while.end.3:
  ret i64 0
}
