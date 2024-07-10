; ModuleID = 'main'
source_filename = "main"

@str = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@str.1 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@Baz = global { ptr } { ptr @baz }
@str.2 = private unnamed_addr constant [13 x i8] c"hello world\0A\00", align 1
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
  %"1" = alloca ptr, align 8
  %1 = call ptr @__rts_new_string(i32 1, ptr @str)
  store ptr %1, ptr %"1", align 8
  %2 = load ptr, ptr %"0", align 8
  %3 = load ptr, ptr %"1", align 8
  %call3 = call ptr @__rts_string_concat(ptr %2, ptr %3)
  %"2" = alloca ptr, align 8
  store ptr %call3, ptr %"2", align 8
  %4 = load ptr, ptr %"2", align 8
  call void @__rts_string_print(ptr %4)
  %5 = load ptr, ptr %this1, align 8
  %get_data = call ptr @__rts_get_data(ptr %5)
  %6 = getelementptr inbounds { i32 }, ptr %get_data, i32 0, i32 0
  %7 = load i32, ptr %6, align 4
  %call4 = call ptr @__rts_int_to_string(i32 %7)
  %"3" = alloca ptr, align 8
  store ptr %call4, ptr %"3", align 8
  %"4" = alloca ptr, align 8
  %8 = call ptr @__rts_new_string(i32 1, ptr @str.1)
  store ptr %8, ptr %"4", align 8
  %9 = load ptr, ptr %"3", align 8
  %10 = load ptr, ptr %"4", align 8
  %call5 = call ptr @__rts_string_concat(ptr %9, ptr %10)
  %"5" = alloca ptr, align 8
  store ptr %call5, ptr %"5", align 8
  %11 = load ptr, ptr %"5", align 8
  call void @__rts_string_print(ptr %11)
  %12 = load ptr, ptr %this1, align 8
  %get_data6 = call ptr @__rts_get_data(ptr %12)
  %13 = getelementptr inbounds { i32 }, ptr %get_data6, i32 0, i32 0
  %"6" = alloca i32, align 4
  store i32 1, ptr %"6", align 4
  %14 = load i32, ptr %a2, align 4
  %15 = load i32, ptr %"6", align 4
  %"7" = alloca i32, align 4
  %16 = add i32 %14, %15
  store i32 %16, ptr %"7", align 4
  %17 = load i32, ptr %"7", align 4
  store i32 %17, ptr %13, align 4
  %18 = load ptr, ptr %this1, align 8
  ret ptr %18
}

declare ptr @__rts_int_to_string(i32)

declare ptr @__rts_new_string(i32, ptr)

declare ptr @__rts_string_concat(ptr, ptr)

declare void @__rts_string_print(ptr)

declare ptr @__rts_get_data(ptr)

define { ptr, ptr } @baz(ptr %this1, { ptr, ptr } %a1) {
entry:
  %this11 = alloca ptr, align 8
  store ptr %this1, ptr %this11, align 8
  %a12 = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %a1, ptr %a12, align 8
  %0 = load ptr, ptr %this11, align 8
  %get_data = call ptr @__rts_get_data(ptr %0)
  %1 = getelementptr inbounds { i32 }, ptr %get_data, i32 0, i32 0
  %2 = load ptr, ptr %this11, align 8
  %get_data3 = call ptr @__rts_get_data(ptr %2)
  %3 = getelementptr inbounds { i32 }, ptr %get_data3, i32 0, i32 0
  %"8" = alloca i32, align 4
  store i32 1, ptr %"8", align 4
  %4 = load i32, ptr %3, align 4
  %5 = load i32, ptr %"8", align 4
  %"9" = alloca i32, align 4
  %6 = add i32 %4, %5
  store i32 %6, ptr %"9", align 4
  %7 = load i32, ptr %"9", align 4
  store i32 %7, ptr %1, align 4
  %8 = load ptr, ptr %this11, align 8
  %data = insertvalue { ptr, ptr } { ptr @Baz, ptr poison }, ptr %8, 1
  %"10" = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %data, ptr %"10", align 8
  %9 = load { ptr, ptr }, ptr %a12, align 8
  %10 = extractvalue { ptr, ptr } %9, 0
  %vtable_ptr = load ptr, ptr %10, align 8
  %11 = extractvalue { ptr, ptr } %9, 1
  %12 = getelementptr inbounds { ptr }, ptr %vtable_ptr, i32 0, i32 0
  %13 = load { ptr, ptr }, ptr %"10", align 8
  %meth = call { ptr, ptr } %12(ptr %11, { ptr, ptr } %13)
  %"11" = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %meth, ptr %"11", align 8
  %14 = load { ptr, ptr }, ptr %"11", align 8
  ret { ptr, ptr } %14
}

define void @main() {
entry:
  call void @__rts_gc_init()
  %"12" = alloca ptr, align 8
  %0 = call ptr @__rts_new_string(i32 12, ptr @str.2)
  store ptr %0, ptr %"12", align 8
  %1 = load ptr, ptr %"12", align 8
  call void @__rts_string_print(ptr %1)
  %b = alloca i32, align 4
  store i32 3, ptr %b, align 4
  %alloc = call ptr @__rts_gc_alloc(ptr null, i32 ptrtoint (ptr getelementptr ({ i32 }, ptr null, i32 1) to i32))
  %get_data = call ptr @__rts_get_data(ptr %alloc)
  %2 = load i32, ptr %b, align 4
  %3 = getelementptr inbounds { i32 }, ptr %get_data, i32 0, i32 0
  store i32 %2, ptr %3, align 4
  %b1 = alloca ptr, align 8
  store ptr %alloc, ptr %b1, align 8
  %4 = load ptr, ptr %b1, align 8
  %data = insertvalue { ptr, ptr } { ptr @Foo, ptr poison }, ptr %4, 1
  %b2 = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %data, ptr %b2, align 8
  %5 = load { ptr, ptr }, ptr %b2, align 8
  %b3 = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %5, ptr %b3, align 8
  %"13" = alloca i32, align 4
  store i32 1, ptr %"13", align 4
  %6 = load { ptr, ptr }, ptr %b3, align 8
  %7 = extractvalue { ptr, ptr } %6, 0
  %vtable_ptr = load ptr, ptr %7, align 8
  %8 = extractvalue { ptr, ptr } %6, 1
  %9 = getelementptr inbounds { ptr }, ptr %vtable_ptr, i32 0, i32 0
  %10 = load i32, ptr %"13", align 4
  %meth = call { ptr, ptr } %9(ptr %8, i32 %10)
  %"14" = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %meth, ptr %"14", align 8
  %"15" = alloca i32, align 4
  store i32 2, ptr %"15", align 4
  %11 = load { ptr, ptr }, ptr %b3, align 8
  %12 = extractvalue { ptr, ptr } %11, 0
  %vtable_ptr1 = load ptr, ptr %12, align 8
  %13 = extractvalue { ptr, ptr } %11, 1
  %14 = getelementptr inbounds { ptr }, ptr %vtable_ptr1, i32 0, i32 0
  %15 = load i32, ptr %"15", align 4
  %meth2 = call { ptr, ptr } %14(ptr %13, i32 %15)
  %"16" = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %meth2, ptr %"16", align 8
  call void @__rts_gc_destroy()
  ret void
}

declare void @__rts_gc_init()

declare ptr @__rts_gc_alloc(ptr, i32)

declare void @__rts_gc_destroy()
