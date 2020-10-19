(module $awesome
  (func $factorial (param $n i32) (result i32)
    (if (result i32) (i32.eq (local.get $n) (i32.const 1))
      (then (i32.const 1))
    (else
      (i32.mul
       (local.get $n)
       (call $factorial (i32.sub (local.get $n) (i32.const 1)))
      )
     ))
   )
  (func $main (result i32) (call $factorial (i32.const 20))) (export \"main\" (func $main)))
