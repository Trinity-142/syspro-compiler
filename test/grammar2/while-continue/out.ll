; ModuleID = 'spl'
source_filename = "spl"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define i64 @main() {
entry:
  %1 = alloca i64, align 8
  store i64 0, ptr %1, align 8
  %2 = alloca i64, align 8
  store i64 0, ptr %2, align 8
  br label %while.cond.1
while.cond.1:
  %3 = load i64, ptr %1, align 8
  %4 = icmp slt i64 %3, 5
  %5 = zext i1 %4 to i64
  %6 = icmp ne i64 %5, 0
  br i1 %6, label %while.body.2, label %while.end.3
while.body.2:
  %7 = load i64, ptr %1, align 8
  %8 = add i64 %7, 1
  store i64 %8, ptr %1, align 8
  %9 = load i64, ptr %1, align 8
  %10 = icmp sgt i64 %9, 3
  %11 = zext i1 %10 to i64
  %12 = icmp ne i64 %11, 0
  br i1 %12, label %if.then.4, label %if.end.5
if.then.4:
  br label %while.cond.1
if.end.5:
  %13 = load i64, ptr %2, align 8
  %14 = add i64 %13, 1
  store i64 %14, ptr %2, align 8
  br label %while.cond.1
while.end.3:
  %15 = load i64, ptr %2, align 8
  ret i64 %15
}
