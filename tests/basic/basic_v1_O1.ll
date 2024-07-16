; ModuleID = 'main'
source_filename = "main"

define i32 @foo(ptr %a, ptr %b) local_unnamed_addr {
entry:
  %get_data = tail call ptr @__rts_get_data(ptr %a)
  %get_data3 = tail call ptr @__rts_get_data(ptr %b)
  %0 = load ptr, ptr %get_data3, align 8
  %get_data4 = tail call ptr @__rts_get_data(ptr %0)
  %1 = getelementptr inbounds { i32, i32 }, ptr %get_data4, i64 0, i32 1
  %2 = load i32, ptr %get_data, align 4
  %3 = load i32, ptr %1, align 4
  %4 = add i32 %3, %2
  ret i32 %4
}

declare ptr @__rts_get_data(ptr) local_unnamed_addr

define void @main() local_unnamed_addr {
entry:
  tail call void @__rts_gc_init()
  %alloc = tail call ptr @__rts_gc_alloc(ptr null, i32 8)
  %get_data = tail call ptr @__rts_get_data(ptr %alloc)
  store i32 1, ptr %get_data, align 4
  %0 = getelementptr inbounds { i32, i32 }, ptr %get_data, i64 0, i32 1
  store i32 2, ptr %0, align 4
  %alloc1 = tail call ptr @__rts_gc_alloc(ptr null, i32 8)
  %get_data2 = tail call ptr @__rts_get_data(ptr %alloc1)
  store i32 3, ptr %get_data2, align 4
  %1 = getelementptr inbounds { i32, i32 }, ptr %get_data2, i64 0, i32 1
  store i32 4, ptr %1, align 4
  %alloc3 = tail call ptr @__rts_gc_alloc(ptr null, i32 16)
  %get_data4 = tail call ptr @__rts_get_data(ptr %alloc3)
  store ptr %alloc1, ptr %get_data4, align 8
  %2 = getelementptr inbounds { ptr, i32 }, ptr %get_data4, i64 0, i32 1
  store i32 5, ptr %2, align 4
  %get_data.i = tail call ptr @__rts_get_data(ptr %alloc)
  %get_data3.i = tail call ptr @__rts_get_data(ptr %alloc3)
  %3 = load ptr, ptr %get_data3.i, align 8
  %get_data4.i = tail call ptr @__rts_get_data(ptr %3)
  tail call void @__rts_gc_destroy()
  ret void
}

declare void @__rts_gc_init() local_unnamed_addr

declare ptr @__rts_gc_alloc(ptr, i32) local_unnamed_addr

declare void @__rts_gc_destroy() local_unnamed_addr
