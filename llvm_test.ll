; ModuleID = 'llvm_test.cpp'
source_filename = "llvm_test.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%class.Foo = type { i32 (...)** }

@t = dso_local global [5 x i32] [i32 1, i32 2, i32 3, i32 4, i32 5], align 16

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local void @_Z3fooP3Foo(%class.Foo* noundef %0) #0 {
  %2 = alloca %class.Foo*, align 8
  store %class.Foo* %0, %class.Foo** %2, align 8
  %3 = load %class.Foo*, %class.Foo** %2, align 8
  %4 = bitcast %class.Foo* %3 to void (%class.Foo*)***
  %5 = load void (%class.Foo*)**, void (%class.Foo*)*** %4, align 8
  %6 = getelementptr inbounds void (%class.Foo*)*, void (%class.Foo*)** %5, i64 0
  %7 = load void (%class.Foo*)*, void (%class.Foo*)** %6, align 8
  call void %7(%class.Foo* noundef nonnull align 8 dereferenceable(8) %3)
  %8 = load %class.Foo*, %class.Foo** %2, align 8
  %9 = bitcast %class.Foo* %8 to void (%class.Foo*)***
  %10 = load void (%class.Foo*)**, void (%class.Foo*)*** %9, align 8
  %11 = getelementptr inbounds void (%class.Foo*)*, void (%class.Foo*)** %10, i64 1
  %12 = load void (%class.Foo*)*, void (%class.Foo*)** %11, align 8
  call void %12(%class.Foo* noundef nonnull align 8 dereferenceable(8) %8)
  ret void
}

attributes #0 = { mustprogress noinline optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 1}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"Ubuntu clang version 14.0.0-1ubuntu1.1"}
