; ModuleID = 'call_by_val.ll'
source_filename = "call_by_val.ll"

%t0 = type { i32, i32 }

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @f0(%t0 %"5", i32 %"1") local_unnamed_addr #0 {
entry:
  %"5.fca.0.extract" = extractvalue %t0 %"5", 0
  %0 = add i32 %"5.fca.0.extract", %"1"
  ret i32 %0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define noundef i32 @f1(i32 %a0) local_unnamed_addr #0 {
entry:
  ret i32 3
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) }
