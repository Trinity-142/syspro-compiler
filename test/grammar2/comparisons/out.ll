; ModuleID = 'spl'
source_filename = "spl"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define i64 @main() {
entry:
  %1 = alloca i64, align 8
  store i64 0, ptr %1, align 8
  %2 = alloca i64, align 8
  store i64 1, ptr %2, align 8
  %3 = alloca i64, align 8
  store i64 1, ptr %3, align 8
  %4 = alloca i64, align 8
  store i64 0, ptr %4, align 8
  %5 = alloca i64, align 8
  store i64 1, ptr %5, align 8
  %6 = alloca i64, align 8
  store i64 0, ptr %6, align 8
  %7 = load i64, ptr %1, align 8
  %8 = load i64, ptr %2, align 8
  %9 = add i64 %7, %8
  %10 = load i64, ptr %3, align 8
  %11 = add i64 %9, %10
  %12 = load i64, ptr %4, align 8
  %13 = add i64 %11, %12
  %14 = load i64, ptr %5, align 8
  %15 = add i64 %13, %14
  %16 = load i64, ptr %6, align 8
  %17 = add i64 %15, %16
  ret i64 %17
}
