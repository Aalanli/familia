; ModuleID = 'main'
source_filename = "main"

%t1 = type { i32, i32 }

define i32 @f0(i32 %"0", i32 %"7") {
entry:
  %0 = alloca i32, align 4
  store i32 %"0", ptr %0, align 4
  %1 = alloca i32, align 4
  store i32 %"7", ptr %1, align 4
  %2 = load i32, ptr %0, align 4
  ret i32 %2
}

define i32 @f1(%t1 %"11") {
entry:
  %0 = alloca %t1, align 8
  store %t1 %"11", ptr %0, align 4
  %1 = getelementptr inbounds %t1, ptr %0, i32 0, i32 0
  %2 = load i32, ptr %1, align 4
  %3 = alloca i32, align 4
  store i32 %2, ptr %3, align 4
  %4 = load i32, ptr %3, align 4
  ret i32 %4
}

define void @f2() {
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
  %7 = alloca { { i32, i32 } }, align 8
  %8 = load { i32, i32 }, ptr %2, align 4
  %9 = getelementptr inbounds { { i32, i32 } }, ptr %7, i32 0, i32 0
  store { i32, i32 } %8, ptr %9, align 4
  %10 = alloca i32, align 4
  store i32 3, ptr %10, align 4
  %11 = getelementptr inbounds { i32, i32 }, ptr %2, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = alloca i32, align 4
  store i32 %12, ptr %13, align 4
  %14 = load i32, ptr %10, align 4
  %15 = load i32, ptr %13, align 4
  %16 = alloca i32, align 4
  %call = call i32 @f0(i32 %14, i32 %15)
  store i32 %call, ptr %16, align 4
  %17 = load %t1, ptr %2, align 4
  %18 = alloca i32, align 4
  %call1 = call i32 @f1(%t1 %17)
  store i32 %call1, ptr %18, align 4
  ret void
}
