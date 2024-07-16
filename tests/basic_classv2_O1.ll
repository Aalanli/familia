; ModuleID = 'main'
source_filename = "main"

@str = private unnamed_addr constant [15 x i8] c"Hello, world!\0A\00", align 1
@Foo = local_unnamed_addr global { ptr } { ptr @say_hello }

define void @say_hello(ptr nocapture readnone %this) {
entry:
  %0 = tail call ptr @__rts_new_string(i32 14, ptr nonnull @str)
  tail call void @__rts_string_print(ptr %0)
  ret void
}

declare ptr @__rts_new_string(i32, ptr) local_unnamed_addr

declare void @__rts_string_print(ptr) local_unnamed_addr

define void @foo({ ptr, ptr } %a) local_unnamed_addr {
entry:
  %a.fca.0.extract = extractvalue { ptr, ptr } %a, 0
  %a.fca.1.extract = extractvalue { ptr, ptr } %a, 1
  %vtable_ptr = load ptr, ptr %a.fca.0.extract, align 8
  %meth = tail call i8 %vtable_ptr(ptr %a.fca.1.extract)
  ret void
}

define void @main() local_unnamed_addr {
entry:
  tail call void @__rts_gc_init()
  %alloc = tail call ptr @__rts_gc_alloc(ptr null, i32 1)
  %get_data = tail call ptr @__rts_get_data(ptr %alloc)
  %vtable_ptr.i = load ptr, ptr @Foo, align 8
  %meth.i = tail call i8 %vtable_ptr.i(ptr %alloc)
  tail call void @__rts_gc_destroy()
  ret void
}

declare void @__rts_gc_init() local_unnamed_addr

declare ptr @__rts_gc_alloc(ptr, i32) local_unnamed_addr

declare ptr @__rts_get_data(ptr) local_unnamed_addr

declare void @__rts_gc_destroy() local_unnamed_addr
