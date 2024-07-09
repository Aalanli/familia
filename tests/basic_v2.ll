; ModuleID = 'main'
source_filename = "main"

define i32 @bar(i32 %a, i32 %b) {
entry:
  %a1 = alloca i32, align 4
  store i32 %a, ptr %a1, align 4
  %b2 = alloca i32, align 4
  store i32 %b, ptr %b2, align 4
  %c = alloca i32, align 4
  store i32 1, ptr %c, align 4
  %0 = load i32, ptr %a1, align 4
  %1 = load i32, ptr %c, align 4
  %c1 = alloca i32, align 4
  %2 = add i32 %0, %1
  store i32 %2, ptr %c1, align 4
  %3 = load i32, ptr %c1, align 4
  %c2 = alloca i32, align 4
  store i32 %3, ptr %c2, align 4
  %d = alloca i32, align 4
  store i32 1, ptr %d, align 4
  %4 = load i32, ptr %b2, align 4
  %5 = load i32, ptr %d, align 4
  %d1 = alloca i32, align 4
  %6 = add i32 %4, %5
  store i32 %6, ptr %d1, align 4
  %7 = load i32, ptr %d1, align 4
  %d2 = alloca i32, align 4
  store i32 %7, ptr %d2, align 4
  %8 = load i32, ptr %c2, align 4
  %9 = load i32, ptr %d2, align 4
  %"0" = alloca i32, align 4
  %10 = add i32 %8, %9
  store i32 %10, ptr %"0", align 4
  %11 = load i32, ptr %"0", align 4
  ret i32 %11
}

define void @main() {
entry:
  call void @__rts_gc_init()
  %"1" = alloca i32, align 4
  store i32 1, ptr %"1", align 4
  %0 = load i32, ptr %"1", align 4
  %call = call ptr @__rts_int_to_string(i32 %0)
  %"2" = alloca ptr, align 8
  store ptr %call, ptr %"2", align 8
  call void @__rts_gc_destroy()
  ret void
}

declare void @__rts_gc_init()

declare ptr @__rts_int_to_string(i32)

declare void @__rts_gc_destroy()
