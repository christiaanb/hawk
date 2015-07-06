.global _main
_main:
       ;; Initialize Stack Pointer
        add r14,r0,r0
        lhi r14, ((memSize-4)>>16)&0xffff
        addui r14, r14, ((memSize-4)&0xffff)
        ;; Save the old frame pointer
        sw -4(r14),r30
        ;; Save the return address
        sw -8(r14),r31
        ;; Establish new frame pointer
        add r30,r0,r14
        ;; Adjust Stack Pointer
        add r14,r14,#-32
        ;; Save Registers
        sw 0(r14),r3
        sw 4(r14),r4
        sw 8(r14),r5
        addi r5,r0,#2
        sw -12(r30),r5
        nop

        sw -12(r30),#0
L2:     
	lw r3,-12(r30)    
	add r3,r3,#1              ;;336
	sw -12(r30),r3            ;;340
	j L2                      ;;344
