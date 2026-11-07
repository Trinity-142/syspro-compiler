; ModuleID = 'spl'
source_filename = "spl"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define i64 @main() {
entry:
  %0 = alloca i64, align 8
  store i64 10, ptr %0, align 8
  store i64 20, ptr %0, align 8
  %1 = load i64, ptr %0, align 8
  %2 = add i64 %1, 1
  store i64 %2, ptr %0, align 8
  %3 = load i64, ptr %0, align 8
  ret i64 %3
}
