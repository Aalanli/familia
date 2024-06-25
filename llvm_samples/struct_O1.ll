; ModuleID = 'struct.ll'
source_filename = "struct.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.T = type { i32, float, i32, i32 }
%struct.R = type { %struct.T, ptr, i32 }

; Function Attrs: noinline nounwind optnone uwtable
define dso_local { i64, i64 } @foo(i64 %0, i64 %1) local_unnamed_addr #0 {
  %3 = alloca %struct.T, align 4
  %4 = alloca %struct.T, align 4
  %5 = bitcast ptr %4 to ptr
  %6 = getelementptr inbounds { i64, i64 }, ptr %5, i32 0, i32 0
  store i64 %0, ptr %6, align 4
  %7 = getelementptr inbounds { i64, i64 }, ptr %5, i32 0, i32 1
  store i64 %1, ptr %7, align 4
  %8 = getelementptr inbounds %struct.T, ptr %4, i32 0, i32 0
  store i32 1, ptr %8, align 4
  %9 = getelementptr inbounds %struct.T, ptr %4, i32 0, i32 1
  store float 2.000000e+00, ptr %9, align 4
  %10 = bitcast ptr %3 to ptr
  %11 = bitcast ptr %4 to ptr
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %10, ptr align 4 %11, i64 16, i1 false)
  %12 = bitcast ptr %3 to ptr
  %13 = load { i64, i64 }, ptr %12, align 4
  ret { i64, i64 } %13
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @bar(ptr noundef byval(%struct.R) align 8 %0) local_unnamed_addr #0 {
  %2 = getelementptr inbounds %struct.R, ptr %0, i32 0, i32 0
  %3 = getelementptr inbounds %struct.T, ptr %2, i32 0, i32 0
  %4 = load i32, ptr %3, align 8
  %5 = add nsw i32 %4, 1
  ret i32 %5
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @baz(ptr noundef %0) local_unnamed_addr #0 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr inbounds %struct.R, ptr %3, i32 0, i32 0
  %5 = getelementptr inbounds %struct.T, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 8
  %7 = add nsw i32 %6, 1
  ret i32 %7
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() local_unnamed_addr #0 {
  %1 = alloca %struct.T, align 4
  %2 = alloca %struct.T, align 4
  %3 = getelementptr inbounds %struct.T, ptr %1, i32 0, i32 0
  store i32 1, ptr %3, align 4
  %4 = getelementptr inbounds %struct.T, ptr %1, i32 0, i32 1
  store float 2.000000e+00, ptr %4, align 4
  %5 = bitcast ptr %1 to ptr
  %6 = getelementptr inbounds { i64, i64 }, ptr %5, i32 0, i32 0
  %7 = load i64, ptr %6, align 4
  %8 = getelementptr inbounds { i64, i64 }, ptr %5, i32 0, i32 1
  %9 = load i64, ptr %8, align 4
  %10 = call { i64, i64 } @foo(i64 %7, i64 %9)
  %11 = bitcast ptr %2 to ptr
  %12 = getelementptr inbounds { i64, i64 }, ptr %11, i32 0, i32 0
  %13 = extractvalue { i64, i64 } %10, 0
  store i64 %13, ptr %12, align 4
  %14 = getelementptr inbounds { i64, i64 }, ptr %11, i32 0, i32 1
  %15 = extractvalue { i64, i64 } %10, 1
  store i64 %15, ptr %14, align 4
  ret i32 0
}

; Function Attrs: mustprogress nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #1

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { mustprogress nocallback nofree nounwind willreturn memory(argmem: readwrite) }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 1}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"Ubuntu clang version 14.0.0-1ubuntu1.1"}
