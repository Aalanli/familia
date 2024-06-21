target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%footy = type { fp128, i32 }

define %footy @foo(%footy %0) {
entry:
  %s = alloca %footy, align 16
  store %footy %0, ptr %s, align 16
  %gep = getelementptr inbounds %footy, ptr %s, i64 0, i32 1
  store i32 1, ptr %gep, align 4
  %load = load %footy, ptr %s, align 16
  ret %footy %load
}

define %footy @bar() {
entry:
    %a = alloca %footy, align 16
    %gep = getelementptr inbounds %footy, ptr %a, i64 0, i32 0
    store i128 1, ptr %gep, align 16
    %gep1 = getelementptr inbounds %footy, ptr %a, i64 0, i32 1
    store i32 1, ptr %gep1, align 4
    %res = load %footy, ptr %a, align 16
    %y = call %footy @foo(%footy %res)
    ret %footy %y
}

define %footy @baz(fp128 %0, i32 %1) {
entry:
  %a = alloca %footy, align 16
  %gep = getelementptr inbounds %footy, ptr %a, i64 0, i32 0
  store fp128 %0, ptr %gep, align 16
  %gep1 = getelementptr inbounds %footy, ptr %a, i64 0, i32 1
  store i32 %1, ptr %gep1, align 4
  %res = load %footy, ptr %a, align 16
  %y = call %footy @foo(%footy %res)
  ret %footy %y
}


; ../thirdparty/bin/opt -O1 test1.ll -S -o test1_O1.ll
