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
  %3 = alloca i64, align 8
  store i64 0, ptr %3, align 8
  %4 = alloca i1
  %5 = load i64, ptr %1, align 8
  %6 = icmp eq i64 %5, 0
  %7 = zext i1 %6 to i64
  %8 = icmp ne i64 %7, 0
  br i1 %8, label %and.rhs.1, label %and.false.2
and.false.2:
  store i1 false, ptr %4
  br label %and.end.3
and.rhs.1:
  %9 = load i64, ptr %2, align 8
  %10 = icmp eq i64 %9, 0
  %11 = zext i1 %10 to i64
  %12 = icmp ne i64 %11, 0
  store i1 %12, ptr %4
  br label %and.end.3
and.end.3:
  %13 = load i1, ptr %4
  %14 = zext i1 %13 to i64
  %15 = icmp ne i64 %14, 0
  br i1 %15, label %if.then.4, label %if.end.5
if.then.4:
  store i64 1, ptr %3, align 8
  br label %if.end.5
if.end.5:
  %16 = load i64, ptr %3, align 8
  ret i64 %16
}
