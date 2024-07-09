; ModuleID = 'main'
source_filename = "main"

@Baz = global { ptr } { ptr @baz }
@Foo = global { ptr } { ptr @foo }

define { ptr, ptr } @foo(ptr %this, i32 %a) {
entry:
  %this1 = alloca ptr, align 8
  store ptr %this, ptr %this1, align 8
  %a2 = alloca i32, align 4
  store i32 %a, ptr %a2, align 4
  %0 = load i32, ptr %a2, align 4
  %call = call ptr @__rts_int_to_string(i32 %0)
  %"0" = alloca ptr, align 8
  store ptr %call, ptr %"0", align 8
  %1 = load ptr, ptr %"0", align 8
  call void @__rts_string_print(ptr %1)
  %2 = load ptr, ptr %this1, align 8
  %3 = getelementptr inbounds { i32 }, ptr %2, i32 0, i32 0
  %"1" = alloca i32, align 4
  store i32 1, ptr %"1", align 4
  %4 = load i32, ptr %a2, align 4
  %5 = load i32, ptr %"1", align 4
  %"2" = alloca i32, align 4
  %6 = add i32 %4, %5
  store i32 %6, ptr %"2", align 4
  %7 = load i32, ptr %"2", align 4
  store i32 %7, ptr %3, align 4
  %8 = load ptr, ptr %this1, align 8
  ret ptr %8
}

declare ptr @__rts_int_to_string(i32)

declare void @__rts_string_print(ptr)

define { ptr, ptr } @baz(ptr %this1, { ptr, ptr } %a1) {
entry:
  %this11 = alloca ptr, align 8
  store ptr %this1, ptr %this11, align 8
  %a12 = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %a1, ptr %a12, align 8
  %0 = load ptr, ptr %this11, align 8
  %1 = getelementptr inbounds { i32 }, ptr %0, i32 0, i32 0
  %2 = load ptr, ptr %this11, align 8
  %3 = getelementptr inbounds { i32 }, ptr %2, i32 0, i32 0
  %"3" = alloca i32, align 4
  store i32 1, ptr %"3", align 4
  %4 = load i32, ptr %3, align 4
  %5 = load i32, ptr %"3", align 4
  %"4" = alloca i32, align 4
  %6 = add i32 %4, %5
  store i32 %6, ptr %"4", align 4
  %7 = load i32, ptr %"4", align 4
  store i32 %7, ptr %1, align 4
  %8 = load ptr, ptr %this11, align 8
  %data = insertvalue { ptr, ptr } { ptr @Baz, ptr poison }, ptr %8, 1
  %"5" = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %data, ptr %"5", align 8
  %9 = load { ptr, ptr }, ptr %a12, align 8
  %10 = extractvalue { ptr, ptr } %9, 0
  %11 = extractvalue { ptr, ptr } %9, 1
  %12 = getelementptr inbounds { ptr }, ptr %10, i32 0, i32 0
  %13 = load { ptr, ptr }, ptr %"5", align 8
  %meth = call { ptr, ptr } %12(ptr %11, { ptr, ptr } %13)
  %"6" = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %meth, ptr %"6", align 8
  %14 = load { ptr, ptr }, ptr %"6", align 8
  ret { ptr, ptr } %14
}

define void @main() {
entry:
  call void @__rts_gc_init()
  %b = alloca i32, align 4
  store i32 3, ptr %b, align 4
  %alloc = call ptr @__rts_gc_alloc(ptr null, i32 ptrtoint (ptr getelementptr ({ i32 }, ptr null, i32 1) to i32))
  %0 = load i32, ptr %b, align 4
  %1 = getelementptr inbounds { i32 }, ptr %alloc, i32 0, i32 0
  store i32 %0, ptr %1, align 4
  %b1 = alloca ptr, align 8
  store ptr %alloc, ptr %b1, align 8
  %2 = load ptr, ptr %b1, align 8
  %data = insertvalue { ptr, ptr } { ptr @Foo, ptr poison }, ptr %2, 1
  %b2 = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %data, ptr %b2, align 8
  %3 = load { ptr, ptr }, ptr %b2, align 8
  %b3 = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %3, ptr %b3, align 8
  %"7" = alloca i32, align 4
  store i32 1, ptr %"7", align 4
  %4 = load { ptr, ptr }, ptr %b3, align 8
  %5 = extractvalue { ptr, ptr } %4, 0
  %6 = extractvalue { ptr, ptr } %4, 1
  %7 = getelementptr inbounds { ptr }, ptr %5, i32 0, i32 0
  %8 = load i32, ptr %"7", align 4
  %meth = call { ptr, ptr } %7(ptr %6, i32 %8)
  %"8" = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %meth, ptr %"8", align 8
  call void @__rts_gc_destroy()
  ret void
}

declare void @__rts_gc_init()

declare ptr @__rts_gc_alloc(ptr, i32)

declare void @__rts_gc_destroy()
