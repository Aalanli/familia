; ModuleID = 'main'
source_filename = "main"

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @bar(i32 %a, i32 %b) local_unnamed_addr #0 {
entry:
  %0 = add i32 %a, 2
  %1 = add i32 %0, %b
  ret i32 %1
}

define void @main() local_unnamed_addr {
entry:
  tail call void @__rts_gc_init()
  %call = tail call ptr @__rts_int_to_string(i32 1)
  tail call void @__rts_string_print(ptr %call)
  tail call void @__rts_gc_destroy()
  ret void
}

declare void @__rts_gc_init() local_unnamed_addr

declare ptr @__rts_int_to_string(i32) local_unnamed_addr

declare void @__rts_string_print(ptr) local_unnamed_addr

declare void @__rts_gc_destroy() local_unnamed_addr

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) }
