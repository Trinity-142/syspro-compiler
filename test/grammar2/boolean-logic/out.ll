; ModuleID = 'spl'
source_filename = "spl"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define i64 @main() {
entry:
  %1 = alloca i64, align 8
  store i64 1, ptr %1, align 8
  %2 = alloca i64, align 8
  store i64 0, ptr %2, align 8
  %3 = alloca i1
  %4 = alloca i1
  %5 = load i64, ptr %1, align 8
  %6 = icmp ne i64 %5, 0
  br i1 %6, label %and.rhs.1, label %and.false.2
and.false.2:
  store i1 false, ptr %4
  br label %and.end.3
and.rhs.1:
  %7 = load i64, ptr %2, align 8
  %8 = icmp ne i64 %7, 0
  store i1 %8, ptr %4
  br label %and.end.3
and.end.3:
  %9 = load i1, ptr %4
  %10 = zext i1 %9 to i64
  %11 = icmp ne i64 %10, 0
  br i1 %11, label %or.true.5, label %or.rhs.4
or.true.5:
  store i1 true, ptr %3
  br label %or.end.6
or.rhs.4:
  %12 = load i64, ptr %1, align 8
  %13 = icmp eq i64 %12, 0
  %14 = zext i1 %13 to i64
  %15 = icmp ne i64 %14, 0
  store i1 %15, ptr %3
  br label %or.end.6
or.end.6:
  %16 = load i1, ptr %3
  %17 = zext i1 %16 to i64
  %18 = alloca i64, align 8
  store i64 %17, ptr %18, align 8
  %19 = load i64, ptr %18, align 8
  ret i64 %19
}
