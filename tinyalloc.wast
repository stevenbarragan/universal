(module
  (type $t0 (func (result i32)))
  (type $t1 (func (param i32) (result i32)))
  (type $t2 (func))
  (type $t3 (func (param i32 i32)))
  (type $t4 (func (param i32)))
  (type $t5 (func (param i32 i32) (result i32)))
  (type $t6 (func (param i32 i32 i32 i32 i32) (result i32)))
  (import "env" "__memory_base" (global $env.__memory_base i32))
  (import "env" "memory" (memory $env.memory 1))
  (func $__post_instantiate (type $t2)
    nop)
  (func $ta_init (type $t6) (param $p0 i32) (param $p1 i32) (param $p2 i32) (param $p3 i32) (param $p4 i32) (result i32)
    (local $l5 i32)
    global.get $env.__memory_base
    local.tee $l5
    local.get $p1
    i32.store offset=4
    local.get $l5
    local.get $p0
    i32.store
    local.get $l5
    local.get $p3
    i32.store offset=8
    local.get $l5
    local.get $p4
    i32.store offset=12
    local.get $l5
    local.get $p2
    i32.store offset=16
    local.get $p0
    local.get $p0
    i32.const 16
    i32.add
    local.tee $p1
    local.get $p2
    i32.const 12
    i32.mul
    i32.add
    i32.store offset=12
    local.get $p0
    local.get $p1
    i32.store offset=8
    local.get $p0
    i64.const 0
    i64.store align=4
    loop $L0
      local.get $p2
      i32.const 1
      i32.sub
      local.tee $p2
      if $I1
        local.get $p1
        local.get $p1
        i32.const 12
        i32.add
        local.tee $p1
        i32.store offset=4
        br $L0
      end
    end
    local.get $p1
    i32.const 0
    i32.store offset=4
    i32.const 1)
  (func $ta_free (type $t1) (param $p0 i32) (result i32)
    (local $l1 i32) (local $l2 i32) (local $l3 i32) (local $l4 i32)
    global.get $env.__memory_base
    i32.load
    i32.const 4
    i32.add
    local.tee $l4
    local.set $l3
    loop $L0
      block $B1
        local.get $l3
        i32.load
        local.tee $l1
        if $I2
          local.get $l1
          i32.load
          local.get $p0
          i32.ne
          br_if $B1
          local.get $l2
          i32.const 4
          i32.add
          local.get $l4
          local.get $l2
          select
          local.get $l1
          i32.load offset=4
          i32.store
          local.get $l1
          call $f3
          call $f4
        end
        local.get $l1
        i32.const 0
        i32.ne
        return
      end
      local.get $l1
      i32.const 4
      i32.add
      local.set $l3
      local.get $l1
      local.set $l2
      br $L0
    end
    unreachable)
  (func $f3 (type $t4) (param $p0 i32)
    (local $l1 i32) (local $l2 i32) (local $l3 i32) (local $l4 i32)
    global.get $env.__memory_base
    i32.load
    local.tee $l4
    local.set $l3
    loop $L0
      block $B1
        local.get $l3
        i32.load
        local.tee $l1
        i32.eqz
        br_if $B1
        local.get $p0
        i32.load
        local.get $l1
        i32.load
        i32.le_u
        br_if $B1
        local.get $l1
        i32.const 4
        i32.add
        local.set $l3
        local.get $l1
        local.set $l2
        br $L0
      end
    end
    local.get $l2
    i32.const 4
    i32.add
    local.get $l4
    local.get $l2
    select
    local.get $p0
    i32.store
    local.get $p0
    local.get $l1
    i32.store offset=4)
  (func $f4 (type $t2)
    (local $l0 i32) (local $l1 i32) (local $l2 i32) (local $l3 i32)
    global.get $env.__memory_base
    i32.load
    local.set $l3
    loop $L0
      local.get $l3
      i32.load
      local.tee $l0
      if $I1
        local.get $l0
        i32.const 4
        i32.add
        local.set $l3
        local.get $l0
        local.set $l2
        loop $L2
          local.get $l2
          local.tee $l1
          i32.load offset=4
          local.tee $l2
          if $I3
            local.get $l2
            i32.load
            local.get $l1
            i32.load offset=8
            local.get $l1
            i32.load
            i32.add
            i32.eq
            br_if $L2
          end
        end
        local.get $l0
        local.get $l1
        i32.eq
        br_if $L0
        local.get $l0
        local.get $l1
        i32.load offset=8
        local.get $l1
        i32.load
        local.get $l0
        i32.load
        i32.sub
        i32.add
        i32.store offset=8
        local.get $l0
        i32.load offset=4
        local.get $l1
        i32.load offset=4
        local.tee $l2
        call $f14
        local.get $l0
        local.get $l2
        i32.store offset=4
        br $L0
      end
    end)
  (func $ta_alloc (type $t1) (param $p0 i32) (result i32)
    local.get $p0
    call $f6
    local.tee $p0
    i32.eqz
    if $I0
      i32.const 0
      return
    end
    local.get $p0
    i32.load)
  (func $f6 (type $t1) (param $p0 i32) (result i32)
    (local $l1 i32) (local $l2 i32) (local $l3 i32) (local $l4 i32) (local $l5 i32) (local $l6 i32) (local $l7 i32) (local $l8 i32)
    local.get $p0
    global.get $env.__memory_base
    local.tee $l2
    i32.load offset=12
    local.tee $l1
    i32.add
    i32.const 1
    i32.sub
    i32.const 0
    local.get $l1
    i32.sub
    i32.and
    local.set $l4
    local.get $l2
    i32.load offset=4
    local.set $l6
    local.get $l2
    i32.load
    local.tee $l2
    i32.load offset=12
    local.set $l5
    local.get $l2
    local.set $l1
    block $B0
      loop $L1
        local.get $l1
        i32.load
        local.tee $p0
        if $I2
          local.get $p0
          i32.load offset=8
          local.tee $l7
          local.get $p0
          i32.load
          local.tee $l1
          i32.add
          local.get $l5
          i32.ge_u
          local.get $l1
          local.get $l4
          i32.add
          local.tee $l8
          local.get $l6
          i32.le_u
          i32.and
          local.set $l1
          i32.const 0
          local.get $l4
          local.get $l7
          i32.gt_u
          local.get $l1
          select
          if $I3
            local.get $p0
            i32.const 4
            i32.add
            local.set $l1
            local.get $p0
            local.set $l3
            br $L1
          else
            local.get $l3
            i32.const 4
            i32.add
            local.get $l2
            local.get $l3
            select
            local.get $p0
            i32.load offset=4
            i32.store
            local.get $p0
            local.get $l2
            i32.load offset=4
            i32.store offset=4
            local.get $l2
            local.get $p0
            i32.store offset=4
            local.get $l1
            if $I4
              local.get $p0
              local.get $l4
              i32.store offset=8
              local.get $l2
              local.get $l8
              i32.store offset=12
              local.get $p0
              return
            end
            local.get $l2
            i32.load offset=8
            local.tee $l1
            i32.eqz
            br_if $B0
            local.get $l7
            local.get $l4
            i32.sub
            local.tee $l3
            global.get $env.__memory_base
            i32.load offset=8
            i32.lt_u
            br_if $B0
            local.get $p0
            local.get $l4
            i32.store offset=8
            local.get $l2
            local.get $l1
            i32.load offset=4
            i32.store offset=8
            local.get $l1
            local.get $l3
            i32.store offset=8
            local.get $l1
            local.get $l8
            i32.store
            local.get $l1
            call $f3
            call $f4
            local.get $p0
            return
          end
          unreachable
        end
      end
      i32.const 0
      local.set $p0
      local.get $l2
      i32.load offset=8
      local.tee $l3
      i32.eqz
      br_if $B0
      local.get $l4
      local.get $l5
      i32.add
      local.tee $l1
      local.get $l6
      i32.gt_u
      br_if $B0
      local.get $l2
      local.get $l3
      i32.load offset=4
      i32.store offset=8
      local.get $l3
      local.get $l5
      i32.store
      local.get $l2
      i32.load offset=4
      local.set $p0
      local.get $l3
      local.get $l4
      i32.store offset=8
      local.get $l3
      local.get $p0
      i32.store offset=4
      local.get $l2
      local.get $l1
      i32.store offset=12
      local.get $l2
      local.get $l3
      i32.store offset=4
      local.get $l3
      local.set $p0
    end
    local.get $p0)
  (func $ta_calloc (type $t5) (param $p0 i32) (param $p1 i32) (result i32)
    local.get $p0
    local.get $p1
    i32.mul
    local.tee $p1
    call $f6
    local.tee $p0
    i32.eqz
    if $I0
      i32.const 0
      return
    end
    local.get $p0
    i32.load
    local.get $p1
    call $f8
    local.get $p0
    i32.load)
  (func $f8 (type $t3) (param $p0 i32) (param $p1 i32)
    (local $l2 i32)
    local.get $p1
    i32.const 2
    i32.shr_u
    local.set $l2
    loop $L0
      local.get $l2
      if $I1
        local.get $p0
        i32.const 0
        i32.store
        local.get $p0
        i32.const 4
        i32.add
        local.set $p0
        local.get $l2
        i32.const 1
        i32.sub
        local.set $l2
        br $L0
      end
    end
    local.get $p1
    i32.const 3
    i32.and
    local.set $l2
    loop $L2
      local.get $l2
      if $I3
        local.get $p0
        i32.const 0
        i32.store8
        local.get $p0
        i32.const 1
        i32.add
        local.set $p0
        local.get $l2
        i32.const 1
        i32.sub
        local.set $l2
        br $L2
      end
    end)
  (func $ta_num_free (type $t0) (result i32)
    global.get $env.__memory_base
    i32.load
    i32.load
    call $f10)
  (func $f10 (type $t1) (param $p0 i32) (result i32)
    (local $l1 i32)
    loop $L0
      local.get $p0
      if $I1
        local.get $l1
        i32.const 1
        i32.add
        local.set $l1
        local.get $p0
        i32.load offset=4
        local.set $p0
        br $L0
      end
    end
    local.get $l1)
  (func $ta_num_used (type $t0) (result i32)
    global.get $env.__memory_base
    i32.load
    i32.load offset=4
    call $f10)
  (func $ta_num_fresh (type $t0) (result i32)
    global.get $env.__memory_base
    i32.load
    i32.load offset=8
    call $f10)
  (func $ta_check (type $t0) (result i32)
    global.get $env.__memory_base
    i32.load offset=16
    call $ta_num_free
    call $ta_num_used
    i32.add
    call $ta_num_fresh
    i32.add
    i32.eq)
  (func $f14 (type $t3) (param $p0 i32) (param $p1 i32)
    (local $l2 i32) (local $l3 i32)
    global.get $env.__memory_base
    i32.load
    local.set $l2
    loop $L0
      local.get $p0
      local.get $p1
      i32.eq
      i32.eqz
      if $I1
        local.get $p0
        i32.load offset=4
        local.set $l3
        local.get $p0
        local.get $l2
        i32.load offset=8
        i32.store offset=4
        local.get $l2
        local.get $p0
        i32.store offset=8
        local.get $p0
        i32.const 0
        i32.store offset=8
        local.get $p0
        i32.const 0
        i32.store
        local.get $l3
        local.set $p0
        br $L0
      end
    end)
  (global $__dso_handle i32 (i32.const 0))
  (export "__post_instantiate" (func $__post_instantiate))
  (export "ta_init" (func $ta_init))
  (export "ta_free" (func $ta_free))
  (export "ta_alloc" (func $ta_alloc))
  (export "ta_calloc" (func $ta_calloc))
  (export "ta_num_free" (func $ta_num_free))
  (export "ta_num_used" (func $ta_num_used))
  (export "ta_num_fresh" (func $ta_num_fresh))
  (export "ta_check" (func $ta_check))
  (export "__dso_handle" (global 1))
  (export "__wasm_apply_data_relocs" (func $__post_instantiate))
  (data $d0 (global.get $env.__memory_base) "\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00"))
