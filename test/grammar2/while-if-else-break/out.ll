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
  %3 = icmp slt i64 %2, 10
  %4 = zext i1 %3 to i64
  %5 = icmp ne i64 %4, 0
  br i1 %5, label %while.body.2, label %while.end.3
while.body.2:
  %6 = load i64, ptr %1, align 8
  %7 = icmp slt i64 %6, 5
  %8 = zext i1 %7 to i64
  %9 = icmp ne i64 %8, 0
  br i1 %9, label %if.then.4, label %if.else.6
if.then.4:
  %10 = load i64, ptr %1, align 8
  %11 = add i64 %10, 1
  store i64 %11, ptr %1, align 8
  br label %if.end.5
if.else.6:
  br label %while.end.3
if.end.5:
  br label %while.cond.1
while.end.3:
  %12 = load i64, ptr %1, align 8
  ret i64 %12
}
