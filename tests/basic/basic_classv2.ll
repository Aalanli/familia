; ModuleID = 'main'
source_filename = "main"

@str = private unnamed_addr constant [15 x i8] c"Hello, world!\0A\00", align 1
@str.1 = private unnamed_addr constant [14 x i8] c"Hello again.\0A\00", align 1
@Foo = global { ptr } { ptr @say_hello }
@Foo.2 = global { ptr } { ptr @say_hello1 }

define void @say_hello(ptr %this) {
entry:
  %this1 = alloca ptr, align 8
  store ptr %this, ptr %this1, align 8
  %"0" = alloca ptr, align 8
  %0 = call ptr @__rts_new_string(i32 14, ptr @str)
  store ptr %0, ptr %"0", align 8
  %1 = load ptr, ptr %"0", align 8
  call void @__rts_string_print(ptr %1)
  ret void
}

declare ptr @__rts_new_string(i32, ptr)

declare void @__rts_string_print(ptr)

define void @say_hello1(ptr %this1) {
entry:
  %this11 = alloca ptr, align 8
  store ptr %this1, ptr %this11, align 8
  %"1" = alloca ptr, align 8
  %0 = call ptr @__rts_new_string(i32 13, ptr @str.1)
  store ptr %0, ptr %"1", align 8
  %1 = load ptr, ptr %"1", align 8
  call void @__rts_string_print(ptr %1)
  ret void
}

define void @foo({ ptr, ptr } %a) {
entry:
  %a1 = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %a, ptr %a1, align 8
  %0 = load { ptr, ptr }, ptr %a1, align 8
  %1 = extractvalue { ptr, ptr } %0, 0
  %vtable_ptr = load ptr, ptr %1, align 8
  %2 = extractvalue { ptr, ptr } %0, 1
  %3 = getelementptr inbounds { ptr }, ptr %vtable_ptr, i32 0, i32 0
  %meth = call i8 %3(ptr %2)
  %"2" = alloca i8, align 1
  store i8 %meth, ptr %"2", align 1
  ret void
}

define void @main() {
entry:
  call void @__rts_gc_init()
  %b = alloca i8, align 1
  %alloc = call ptr @__rts_gc_alloc(ptr null, i32 ptrtoint (ptr getelementptr ({ i8 }, ptr null, i32 1) to i32))
  %get_data = call ptr @__rts_get_data(ptr %alloc)
  %0 = load i8, ptr %b, align 1
  %1 = getelementptr inbounds { i8 }, ptr %get_data, i32 0, i32 0
  store i8 %0, ptr %1, align 1
  %b1 = alloca ptr, align 8
  store ptr %alloc, ptr %b1, align 8
  %2 = load ptr, ptr %b1, align 8
  %data = insertvalue { ptr, ptr } { ptr @Foo, ptr poison }, ptr %2, 1
  %b2 = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %data, ptr %b2, align 8
  %3 = load { ptr, ptr }, ptr %b2, align 8
  %b3 = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %3, ptr %b3, align 8
  %4 = load { ptr, ptr }, ptr %b3, align 8
  call void @foo({ ptr, ptr } %4)
  %c = alloca i8, align 1
  %alloc1 = call ptr @__rts_gc_alloc(ptr null, i32 ptrtoint (ptr getelementptr ({ i8 }, ptr null, i32 1) to i32))
  %get_data2 = call ptr @__rts_get_data(ptr %alloc1)
  %5 = load i8, ptr %c, align 1
  %6 = getelementptr inbounds { i8 }, ptr %get_data2, i32 0, i32 0
  store i8 %5, ptr %6, align 1
  %c1 = alloca ptr, align 8
  store ptr %alloc1, ptr %c1, align 8
  %7 = load ptr, ptr %c1, align 8
  %data3 = insertvalue { ptr, ptr } { ptr @Foo.2, ptr poison }, ptr %7, 1
  %c2 = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %data3, ptr %c2, align 8
  %8 = load { ptr, ptr }, ptr %c2, align 8
  %c3 = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %8, ptr %c3, align 8
  %9 = load { ptr, ptr }, ptr %c3, align 8
  call void @foo({ ptr, ptr } %9)
  call void @__rts_gc_destroy()
  ret void
}

declare void @__rts_gc_init()

declare ptr @__rts_gc_alloc(ptr, i32)

declare ptr @__rts_get_data(ptr)

declare void @__rts_gc_destroy()
