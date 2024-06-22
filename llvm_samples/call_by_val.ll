%t0 = type { i32, i32 }

define i32 @f0(%t0 %"5", i32 %"1") {
entry:
  %0 = alloca %t0, align 8
  store %t0 %"5", ptr %0, align 4
  %1 = alloca i32, align 4
  store i32 %"1", ptr %1, align 4
  %2 = getelementptr inbounds %t0, ptr %0, i32 0, i32 0
  %3 = load i32, ptr %2, align 4
  %4 = alloca i32, align 4
  store i32 %3, ptr %4, align 4
  %5 = load i32, ptr %4, align 4
  %6 = load i32, ptr %1, align 4
  %7 = alloca i32, align 4
  %8 = add i32 %5, %6
  store i32 %8, ptr %7, align 4
  %9 = load i32, ptr %7, align 4
  ret i32 %9
}

define i32 @f1(i32 %a0) {
entry:
  %0 = alloca i32, align 4
  store i32 %a0, ptr %0, align 4
  %1 = alloca i32, align 4
  store i32 2, ptr %1, align 4
  %2 = alloca { i32, i32 }, align 8
  %3 = load i32, ptr %0, align 4
  %4 = getelementptr inbounds { i32, i32 }, ptr %2, i32 0, i32 0
  store i32 %3, ptr %4, align 4
  %5 = load i32, ptr %1, align 4
  %6 = getelementptr inbounds { i32, i32 }, ptr %2, i32 0, i32 1
  store i32 %5, ptr %6, align 4
  %7 = load %t0, ptr %1, align 8
  %8 = call i32 @f0(%t0 %7, i32 1)
  ret i32 %8
}
