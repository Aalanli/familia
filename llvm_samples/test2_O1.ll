; ModuleID = 'test2.ll'
source_filename = "test1"

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(argmem: write)
define void @foo(ptr nocapture writeonly %0) local_unnamed_addr #0 {
entry:
  store i32 1, ptr %0, align 4
  %i2 = getelementptr inbounds { i32, i32 }, ptr %0, i64 0, i32 1
  store i32 2, ptr %i2, align 4
  ret void
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define { i32, i32 } @bar() local_unnamed_addr #1 {
entry:
  ret { i32, i32 } { i32 1, i32 2 }
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(argmem: write) }
attributes #1 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) }
