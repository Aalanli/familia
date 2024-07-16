; ModuleID = 'main'
source_filename = "main"

define i32 @foo(ptr %a, ptr %b) {
entry:
  %a1 = alloca ptr, align 8
  store ptr %a, ptr %a1, align 8
  %b2 = alloca ptr, align 8
  store ptr %b, ptr %b2, align 8
  %0 = load ptr, ptr %a1, align 8
  %get_data = call ptr @__rts_get_data(ptr %0)
  %1 = getelementptr inbounds { i32, i32 }, ptr %get_data, i32 0, i32 0
  %2 = load ptr, ptr %b2, align 8
  %get_data3 = call ptr @__rts_get_data(ptr %2)
  %3 = getelementptr inbounds { ptr, i32 }, ptr %get_data3, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %get_data4 = call ptr @__rts_get_data(ptr %4)
  %5 = getelementptr inbounds { i32, i32 }, ptr %get_data4, i32 0, i32 1
  %6 = load i32, ptr %1, align 4
  %7 = load i32, ptr %5, align 4
  %"0" = alloca i32, align 4
  %8 = add i32 %6, %7
  store i32 %8, ptr %"0", align 4
  %9 = load i32, ptr %"0", align 4
  ret i32 %9
}

declare ptr @__rts_get_data(ptr)

define void @main() {
entry:
  call void @__rts_gc_init()
  %a1 = alloca i32, align 4
  store i32 1, ptr %a1, align 4
  %a2 = alloca i32, align 4
  store i32 2, ptr %a2, align 4
  %alloc = call ptr @__rts_gc_alloc(ptr null, i32 ptrtoint (ptr getelementptr ({ i32, i32 }, ptr null, i32 1) to i32))
  %get_data = call ptr @__rts_get_data(ptr %alloc)
  %0 = load i32, ptr %a1, align 4
  %1 = getelementptr inbounds { i32, i32 }, ptr %get_data, i32 0, i32 0
  store i32 %0, ptr %1, align 4
  %2 = load i32, ptr %a2, align 4
  %3 = getelementptr inbounds { i32, i32 }, ptr %get_data, i32 0, i32 1
  store i32 %2, ptr %3, align 4
  %a3 = alloca ptr, align 8
  store ptr %alloc, ptr %a3, align 8
  %4 = load ptr, ptr %a3, align 8
  %a4 = alloca ptr, align 8
  store ptr %4, ptr %a4, align 8
  %b1 = alloca i32, align 4
  store i32 3, ptr %b1, align 4
  %b2 = alloca i32, align 4
  store i32 4, ptr %b2, align 4
  %alloc1 = call ptr @__rts_gc_alloc(ptr null, i32 ptrtoint (ptr getelementptr ({ i32, i32 }, ptr null, i32 1) to i32))
  %get_data2 = call ptr @__rts_get_data(ptr %alloc1)
  %5 = load i32, ptr %b1, align 4
  %6 = getelementptr inbounds { i32, i32 }, ptr %get_data2, i32 0, i32 0
  store i32 %5, ptr %6, align 4
  %7 = load i32, ptr %b2, align 4
  %8 = getelementptr inbounds { i32, i32 }, ptr %get_data2, i32 0, i32 1
  store i32 %7, ptr %8, align 4
  %b3 = alloca ptr, align 8
  store ptr %alloc1, ptr %b3, align 8
  %b4 = alloca i32, align 4
  store i32 5, ptr %b4, align 4
  %alloc3 = call ptr @__rts_gc_alloc(ptr null, i32 ptrtoint (ptr getelementptr ({ ptr, i32 }, ptr null, i32 1) to i32))
  %get_data4 = call ptr @__rts_get_data(ptr %alloc3)
  %9 = load ptr, ptr %b3, align 8
  %10 = getelementptr inbounds { ptr, i32 }, ptr %get_data4, i32 0, i32 0
  store ptr %9, ptr %10, align 8
  %11 = load i32, ptr %b4, align 4
  %12 = getelementptr inbounds { ptr, i32 }, ptr %get_data4, i32 0, i32 1
  store i32 %11, ptr %12, align 4
  %b5 = alloca ptr, align 8
  store ptr %alloc3, ptr %b5, align 8
  %13 = load ptr, ptr %b5, align 8
  %b6 = alloca ptr, align 8
  store ptr %13, ptr %b6, align 8
  %14 = load ptr, ptr %a4, align 8
  %15 = load ptr, ptr %b6, align 8
  %call = call i32 @foo(ptr %14, ptr %15)
  %c = alloca i32, align 4
  store i32 %call, ptr %c, align 4
  %16 = load i32, ptr %c, align 4
  %c1 = alloca i32, align 4
  store i32 %16, ptr %c1, align 4
  call void @__rts_gc_destroy()
  ret void
}

declare void @__rts_gc_init()

declare ptr @__rts_gc_alloc(ptr, i32)

declare void @__rts_gc_destroy()
