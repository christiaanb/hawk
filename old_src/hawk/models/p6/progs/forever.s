L1:     add r1,r0,1
        add r2,r0,10
L2:     beqz r2,L3
        mult f1,f1,f2
        sub r2,r2,1
        j L2
L3:     add r3,r1,0
        j L1
        nop
        nop
        nop
        nop
        nop
