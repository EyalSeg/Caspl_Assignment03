section .bss
    align 16
    extern drones_count
    extern drone_coroutines
    extern COROUTINE_SIZE

section .txt
    align 16
    global schedualer_coroutine
    extern start_coroutine
    extern end_co

schedualer_coroutine:
    xor ecx, 0
    
    mov ebx, [drone_coroutines]
    iterate_over_coroutines:
        cmp ecx, [drones_count]
            je iterate_over_coroutines_end

        call start_coroutine

        add ebx, COROUTINE_SIZE
        inc ecx

    iterate_over_coroutines_end:
    
    call end_co

