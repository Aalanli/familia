; ModuleID = 'test'
source_filename = "test"

declare void @print(ptr)
@str = private unnamed_addr constant [6 x i8] c"hello\00", align 1

define void @foo() {
entry:
  %str = alloca [5 x i8], align 1
  store [5 x i8] c"hello", ptr %str, align 1
  call void @print(ptr %str)
  ret void
}

define void @bar() {
entry:
  call void @print(ptr @str)
  ret void
}

