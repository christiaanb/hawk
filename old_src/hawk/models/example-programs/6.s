.global _exit
.global _open
.global _close
.global _read
.global _write
.global _printf

	.align 4
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
L2:
	lw r3,-12(r30)
	addi r4,r0,#9
		;cmpsi	r3,r4
	sgt	r1,r3,r4
	bnez	r1,L3
	nop
	lw r3,-12(r30)
	lw r3,-12(r30)
	add r3,r3,#1
	sw -12(r30),r3
	j L2
	nop
L3:
	sub r14,r14,#8
	sw 0(r14),r0
	addi r5,r0,#9999
	sw 4(r14),r5
	jal _printf
	nop
	add r14,r14,#8
L1:
	;; Restore the saved registers
	lw r3,-32(r30)
	nop
	lw r4,-28(r30)
	nop
	lw r5,-24(r30)
	nop
	;; Restore return address
	lw r31,-8(r30)
	nop
	;; Restore stack pointer
	add r14,r0,r30
	;; Restore frame pointer
	lw r30,-4(r30)
	nop
	;; HALT
	jal _exit
	nop

_exit:
	trap #0
	jr r31
	nop
_open:
	trap #1
	jr r31
	nop
_close:
	trap #2
	jr r31
	nop
_read:
	trap #3
	jr r31
	nop
_write:
	trap #4
	jr r31
	nop
_printf:
	trap #5
	jr r31
	nop
