; ModuleID = 'main'
source_filename = "main"

@str.1 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@Baz = global { ptr } { ptr @baz }
@str.2 = private unnamed_addr constant [13 x i8] c"hello world\0A\00", align 1
@Foo = local_unnamed_addr global { ptr } { ptr @foo }

define { ptr, ptr } @foo(ptr %this, i32 %a) {
entry:
  %call = tail call ptr @__rts_int_to_string(i32 %a)
  %0 = tail call ptr @__rts_new_string(i32 1, ptr nonnull @str.1)
  %call3 = tail call ptr @__rts_string_concat(ptr %call, ptr %0)
  tail call void @__rts_string_print(ptr %call3)
  %get_data = tail call ptr @__rts_get_data(ptr %this)
  %1 = load i32, ptr %get_data, align 4
  %call4 = tail call ptr @__rts_int_to_string(i32 %1)
  %2 = tail call ptr @__rts_new_string(i32 1, ptr nonnull @str.1)
  %call5 = tail call ptr @__rts_string_concat(ptr %call4, ptr %2)
  tail call void @__rts_string_print(ptr %call5)
  %get_data6 = tail call ptr @__rts_get_data(ptr %this)
  %3 = add i32 %a, 1
  store i32 %3, ptr %get_data6, align 4
  ret ptr %this
}

declare ptr @__rts_int_to_string(i32) local_unnamed_addr

declare ptr @__rts_new_string(i32, ptr) local_unnamed_addr

declare ptr @__rts_string_concat(ptr, ptr) local_unnamed_addr

declare void @__rts_string_print(ptr) local_unnamed_addr

declare ptr @__rts_get_data(ptr) local_unnamed_addr

define { ptr, ptr } @baz(ptr %this1, { ptr, ptr } %a1) {
entry:
  %a1.fca.0.extract = extractvalue { ptr, ptr } %a1, 0
  %a1.fca.1.extract = extractvalue { ptr, ptr } %a1, 1
  %get_data = tail call ptr @__rts_get_data(ptr %this1)
  %get_data3 = tail call ptr @__rts_get_data(ptr %this1)
  %0 = load i32, ptr %get_data3, align 4
  %1 = add i32 %0, 1
  store i32 %1, ptr %get_data, align 4
  %vtable_ptr = load ptr, ptr %a1.fca.0.extract, align 8
  %.fca.1.insert5 = insertvalue { ptr, ptr } { ptr @Baz, ptr poison }, ptr %this1, 1
  %meth = tail call { ptr, ptr } %vtable_ptr(ptr %a1.fca.1.extract, { ptr, ptr } %.fca.1.insert5)
  ret { ptr, ptr } %meth
}

define void @main() local_unnamed_addr {
entry:
  tail call void @__rts_gc_init()
  %0 = tail call ptr @__rts_new_string(i32 12, ptr nonnull @str.2)
  tail call void @__rts_string_print(ptr %0)
  %alloc = tail call ptr @__rts_gc_alloc(ptr null, i32 4)
  %get_data = tail call ptr @__rts_get_data(ptr %alloc)
  store i32 3, ptr %get_data, align 4
  %vtable_ptr = load ptr, ptr @Foo, align 8
  %meth = tail call { ptr, ptr } %vtable_ptr(ptr %alloc, i32 1)
  %vtable_ptr1 = load ptr, ptr @Foo, align 8
  %meth2 = tail call { ptr, ptr } %vtable_ptr1(ptr %alloc, i32 2)
  tail call void @__rts_gc_destroy()
  ret void
}

declare void @__rts_gc_init() local_unnamed_addr

declare ptr @__rts_gc_alloc(ptr, i32) local_unnamed_addr

declare void @__rts_gc_destroy() local_unnamed_addr
