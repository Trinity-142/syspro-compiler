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
  %3 = load i64, ptr %1, align 8
  %4 = icmp eq i64 %3, 1
  %5 = zext i1 %4 to i64
  %6 = icmp ne i64 %5, 0
  br i1 %6, label %if.then.1, label %if.else.3
if.then.1:
  store i64 2, ptr %2, align 8
  br label %if.end.2
if.else.3:
  store i64 3, ptr %2, align 8
  br label %if.end.2
if.end.2:
  %7 = load i64, ptr %2, align 8
  ret i64 %7
}
