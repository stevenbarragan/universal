(module
  (func $factorial (param $n i32) (result i32)
	(if (result i32)
	  (i32.eq
	    (get_local $n)
	    (i32.const 1)
	    )
	  (then
	    (i32.const 1)
	    )
	  (else
	    (i32.mul
	      (get_local $n)
	      (call
		$factorial
		(i32.sub
		  (get_local $n)
		  (i32.const 1)
		  )
		)
	      )
	    )
	  )
  )
  (func (export "main") (result i32 i32)
    (local $x i32)
    (local $s i32)

    (local.set $x (i32.const 2))

    (local.set $s (if (result i32) (i32.eq (local.get $x) (i32.const 2))
      (then
	(call $factorial (local.get $x)) 
      )
      (else (call $factorial (i32.const 0)))
    ))

    (i32.add (i32.const 5) (i32.const 2))
    (i32.add (i32.const 5) (i32.const 2))
  )
)
