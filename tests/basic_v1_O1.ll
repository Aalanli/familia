; ModuleID = 'main'
source_filename = "main"

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(read, inaccessiblemem: none)
define i32 @foo(ptr nocapture readonly %a, ptr nocapture readonly %b) local_unnamed_addr #0 {
entry:
  %0 = load ptr, ptr %b, align 8
  %1 = getelementptr inbounds { i32, i32 }, ptr %0, i64 0, i32 1
  %2 = load i32, ptr %a, align 4
  %3 = load i32, ptr %1, align 4
  %4 = add i32 %3, %2
  ret i32 %4
}

define void @main() local_unnamed_addr {
entry:
  tail call void @__rts_gc_init()
  %alloc = tail call ptr @__rts_gc_alloc(ptr null, i32 8)
  store i32 1, ptr %alloc, align 4
  %0 = getelementptr inbounds { i32, i32 }, ptr %alloc, i64 0, i32 1
  store i32 2, ptr %0, align 4
  %alloc1 = tail call ptr @__rts_gc_alloc(ptr null, i32 8)
  store i32 3, ptr %alloc1, align 4
  %1 = getelementptr inbounds { i32, i32 }, ptr %alloc1, i64 0, i32 1
  store i32 4, ptr %1, align 4
  %alloc2 = tail call ptr @__rts_gc_alloc(ptr null, i32 16)
  store ptr %alloc1, ptr %alloc2, align 8
  %2 = getelementptr inbounds { ptr, i32 }, ptr %alloc2, i64 0, i32 1
  store i32 5, ptr %2, align 4
  tail call void @__rts_gc_destroy()
  ret void
}

declare void @__rts_gc_init() local_unnamed_addr

declare ptr @__rts_gc_alloc(ptr, i32) local_unnamed_addr

declare void @__rts_gc_destroy() local_unnamed_addr

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(read, inaccessiblemem: none) }
