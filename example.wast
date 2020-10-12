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
	(func $main (result i32) (i32.const 42))
)
