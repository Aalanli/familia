; ModuleID = 'main'
source_filename = "main"

@Baz = global { ptr } { ptr @baz }
@Foo = local_unnamed_addr global { ptr } { ptr @foo }

define { ptr, ptr } @foo(ptr writeonly %this, i32 %a) {
entry:
  %call = tail call ptr @__rts_int_to_string(i32 %a)
  tail call void @__rts_string_print(ptr %call)
  %0 = add i32 %a, 1
  store i32 %0, ptr %this, align 4
  ret ptr %this
}

declare ptr @__rts_int_to_string(i32) local_unnamed_addr

declare void @__rts_string_print(ptr) local_unnamed_addr

define { ptr, ptr } @baz(ptr %this1, { ptr, ptr } %a1) {
entry:
  %a1.fca.0.extract = extractvalue { ptr, ptr } %a1, 0
  %a1.fca.1.extract = extractvalue { ptr, ptr } %a1, 1
  %0 = load i32, ptr %this1, align 4
  %1 = add i32 %0, 1
  store i32 %1, ptr %this1, align 4
  %.fca.1.insert4 = insertvalue { ptr, ptr } { ptr @Baz, ptr poison }, ptr %this1, 1
  %meth = tail call { ptr, ptr } %a1.fca.0.extract(ptr %a1.fca.1.extract, { ptr, ptr } %.fca.1.insert4)
  ret { ptr, ptr } %meth
}

define void @main() local_unnamed_addr {
entry:
  tail call void @__rts_gc_init()
  %alloc = tail call ptr @__rts_gc_alloc(ptr null, i32 4)
  store i32 3, ptr %alloc, align 4
  %meth = tail call { ptr, ptr } @Foo(ptr nonnull %alloc, i32 1)
  tail call void @__rts_gc_destroy()
  ret void
}

declare void @__rts_gc_init() local_unnamed_addr

declare ptr @__rts_gc_alloc(ptr, i32) local_unnamed_addr

declare void @__rts_gc_destroy() local_unnamed_addr
