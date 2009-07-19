/*
 * This is a RANDOMLY GENERATED PROGRAM.
 *
 * Generator: randprog 1.0.0
 * Options:   (none)
 * Seed:      2173174124
 */

#include <stdint.h>
uint16_t context = 0;

#if defined(__AVR_ARCH__)
#  include "platform_avr.h"
#elif defined (__MSP430__)
#  include "platform_msp430.h"
#else
#  include "platform_generic.h"
#endif

#include "random_runtime.h"

/* --- GLOBAL VARIABLES --- */
uint16_t g_3 = 0x53AAL;


/* --- FORWARD DECLARATIONS --- */
int32_t func_1(void);


/* --- FUNCTIONS --- */
/* ------------------------------------------ */
/*
 * reads : g_3
 * writes:
 */
int32_t func_1(void)
{
{
uint32_t l_2 = 0x73838A1FL;
(l_2 * g_3);
return l_2;
}
}




/* ---------------------------------------- */
int main(int argc, char *argv[])
{
    __initialize__();
    platform_main_begin();
    /* Call the first function */
    func_1();
crcBytes(g_3);
    platform_main_end(context);
    return 0;
}
