; ModuleID = 'codegen_test3.ll'
source_filename = "main"

%t1 = type { i32, i32 }

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @f0(i32 returned %"0", i32 %"7") local_unnamed_addr #0 {
entry:
  ret i32 %"0"
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @f1(%t1 %"11") local_unnamed_addr #0 {
entry:
  %"11.fca.0.extract" = extractvalue %t1 %"11", 0
  ret i32 %"11.fca.0.extract"
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define void @f2() local_unnamed_addr #0 {
entry:
  ret void
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) }
