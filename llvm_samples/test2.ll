; ModuleID = 'test1'
source_filename = "test1"

define void @foo(ptr %0) {
entry:
  %i1 = getelementptr inbounds { i32, i32 }, ptr %0, i32 0, i32 0
  store i32 1, ptr %i1, align 4
  %i2 = getelementptr inbounds { i32, i32 }, ptr %0, i32 0, i32 1
  store i32 2, ptr %i2, align 4
  ret void
}

define {i32, i32} @bar() {
entry:
  %1 = alloca { i32, i32 }
  call void @foo({ i32, i32 }* %1)
  %2 = load { i32, i32 }, { i32, i32 }* %1
  ret { i32, i32 } %2
}
