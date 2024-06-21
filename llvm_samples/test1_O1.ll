; ModuleID = 'test1.ll'
source_filename = "test1.ll"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%footy = type { fp128, i32 }

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define %footy @foo(%footy %0) local_unnamed_addr #0 {
entry:
  %load.fca.1.insert = insertvalue %footy %0, i32 1, 1
  ret %footy %load.fca.1.insert
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define %footy @bar() local_unnamed_addr #0 {
entry:
  ret %footy { fp128 0xL00000000000000010000000000000000, i32 1 }
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define %footy @baz(fp128 %0, i32 %1) local_unnamed_addr #0 {
entry:
  %res.fca.0.insert = insertvalue %footy poison, fp128 %0, 0
  %load.fca.1.insert.i = insertvalue %footy %res.fca.0.insert, i32 1, 1
  ret %footy %load.fca.1.insert.i
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) }
