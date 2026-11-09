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
  %6 = alloca i64, align 8
  store i64 0, ptr %6, align 8
  br label %while.cond.4
while.cond.4:
  %7 = load i64, ptr %6, align 8
  %8 = icmp slt i64 %7, 10
  %9 = zext i1 %8 to i64
  %10 = icmp ne i64 %9, 0
  br i1 %10, label %while.body.5, label %while.end.6
while.body.5:
  %11 = load i64, ptr %6, align 8
  %12 = add i64 %11, 1
  store i64 %12, ptr %6, align 8
  br label %while.cond.4
while.end.6:
  %13 = load i64, ptr %1, align 8
  %14 = add i64 %13, 1
  store i64 %14, ptr %1, align 8
  br label %while.cond.1
while.end.3:
  %15 = load i64, ptr %1, align 8
  ret i64 %15
}
