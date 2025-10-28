/* timer.c — 8051 timer test program */

#include <8051.h>  /* gives you SFR definitions like P1 */

volatile int interrupt_flag = 0;

void timer0_isr(void) __interrupt (1)
{
    /* Toggle port 1 and stop timer 0 */
    TR0 = 0;
    P1 = ~P1;
    interrupt_flag = 1;
}

void main(void)
{
    unsigned char i, sum = 0;
    __xdata unsigned char *scratch = (__xdata unsigned char *)0x8000;

    TMOD = 5; // 0b0000_0101: timer 0: mode 1 counter, 16-bit timer, auto-reload
    TH0 = 0xff;
    TL0 = 0xf4;
    ET0 = 1; // enable timer 0 interrupt
    EA = 1; // enable global interrupts
    TR0 = 1; // start timer 0

    while (!interrupt_flag) {
    }

    interrupt_flag = 0;

    TMOD = 1; // 0b0000_0001: timer 0: mode 1 timer, 16-bit timer, auto-reload
    TH0 = 0xff;
    TL0 = 0xf0;
    TR0 = 1; // start timer 0

    while (!interrupt_flag) {
    }

    /* Stop here (endless loop) */
    while (1) {
        /* idle */
    }
}
