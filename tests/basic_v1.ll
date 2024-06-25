; ModuleID = 'main'
source_filename = "main"

%t0 = type { i32, i32 }
%t1 = type { %t0, i32 }

define i32 @f0(%t0 %"0", %t1 %"1") {
entry:
  %0 = alloca %t0, align 8
  store %t0 %"0", ptr %0, align 4
  %1 = alloca %t1, align 8
  store %t1 %"1", ptr %1, align 4
  %2 = getelementptr inbounds %t0, ptr %0, i32 0, i32 0
  %3 = load i32, ptr %2, align 4
  %4 = alloca i32, align 4
  store i32 %3, ptr %4, align 4
  %5 = getelementptr inbounds %t1, ptr %1, i32 0, i32 0
  %6 = load %t0, ptr %5, align 4
  %7 = alloca %t0, align 8
  store %t0 %6, ptr %7, align 4
  %8 = getelementptr inbounds %t0, ptr %7, i32 0, i32 1
  %9 = load i32, ptr %8, align 4
  %10 = alloca i32, align 4
  store i32 %9, ptr %10, align 4
  %11 = load i32, ptr %4, align 4
  %12 = load i32, ptr %10, align 4
  %13 = alloca i32, align 4
  %14 = add i32 %11, %12
  store i32 %14, ptr %13, align 4
  %15 = load i32, ptr %13, align 4
  ret i32 %15
}

define void @f1() {
entry:
  %0 = alloca i32, align 4
  store i32 1, ptr %0, align 4
  %1 = alloca i32, align 4
  store i32 2, ptr %1, align 4
  %2 = alloca { i32, i32 }, align 8
  %3 = load i32, ptr %0, align 4
  %4 = getelementptr inbounds { i32, i32 }, ptr %2, i32 0, i32 0
  store i32 %3, ptr %4, align 4
  %5 = load i32, ptr %1, align 4
  %6 = getelementptr inbounds { i32, i32 }, ptr %2, i32 0, i32 1
  store i32 %5, ptr %6, align 4
  %7 = alloca i32, align 4
  store i32 3, ptr %7, align 4
  %8 = alloca i32, align 4
  store i32 4, ptr %8, align 4
  %9 = alloca { i32, i32 }, align 8
  %10 = load i32, ptr %7, align 4
  %11 = getelementptr inbounds { i32, i32 }, ptr %9, i32 0, i32 0
  store i32 %10, ptr %11, align 4
  %12 = load i32, ptr %8, align 4
  %13 = getelementptr inbounds { i32, i32 }, ptr %9, i32 0, i32 1
  store i32 %12, ptr %13, align 4
  %14 = alloca i32, align 4
  store i32 5, ptr %14, align 4
  %15 = alloca { { i32, i32 }, i32 }, align 8
  %16 = load { i32, i32 }, ptr %9, align 4
  %17 = getelementptr inbounds { { i32, i32 }, i32 }, ptr %15, i32 0, i32 0
  store { i32, i32 } %16, ptr %17, align 4
  %18 = load i32, ptr %14, align 4
  %19 = getelementptr inbounds { { i32, i32 }, i32 }, ptr %15, i32 0, i32 1
  store i32 %18, ptr %19, align 4
  %20 = load %t0, ptr %2, align 4
  %21 = load %t1, ptr %15, align 4
  %22 = alloca i32, align 4
  %call = call i32 @f0(%t0 %20, %t1 %21)
  store i32 %call, ptr %22, align 4
  ret void
}
