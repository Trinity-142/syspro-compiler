; ModuleID = 'spl'
source_filename = "spl"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define i64 @main() {
entry:
  %1 = alloca i64, align 8
  store i64 1, ptr %1, align 8
  %2 = alloca i64, align 8
  store i64 2, ptr %2, align 8
  %3 = alloca i64, align 8
  store i64 3, ptr %3, align 8
  %4 = load i64, ptr %1, align 8
  %5 = load i64, ptr %2, align 8
  %6 = add i64 %4, %5
  %7 = load i64, ptr %3, align 8
  %8 = add i64 %6, %7
  ret i64 %8
}
