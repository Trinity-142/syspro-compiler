; ModuleID = 'spl'
source_filename = "spl"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define i64 @main() {
entry:
  %1 = icmp ne i64 1, 0
  br i1 %1, label %if.then.1, label %if.end.2
if.then.1:
  %2 = icmp ne i64 0, 0
  br i1 %2, label %if.then.3, label %if.else.5
if.then.3:
  ret i64 1
if.else.5:
  ret i64 2
if.end.4:
  br label %if.end.2
if.end.2:
  ret i64 0
}
