define i32 @test(i32 %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = load i32, i32* %2, align 4
  %4 = add nsw i32 %3, 5
  ret i32 %4
}

define i32 @main() #0 {
  %1 = call i32 @test(i32 5)
  ret i32 %1
}
