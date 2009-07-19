/*
 * This is a RANDOMLY GENERATED PROGRAM.
 *
 * Generator: randprog 1.0.0
 * Options:   (none)
 * Seed:      1103563019
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


/* --- FORWARD DECLARATIONS --- */
int32_t func_1(void);


/* --- FUNCTIONS --- */
/* ------------------------------------------ */
/*
 * reads :
 * writes:
 */
int32_t func_1(void)
{
{
uint32_t l_2 = 0x2C213532L;
int8_t l_3 = -2L;
return l_2;
return l_3;
}
}




/* ---------------------------------------- */
int main(int argc, char *argv[])
{
    __initialize__();
    platform_main_begin();
    /* Call the first function */
    func_1();
    platform_main_end(context);
    return 0;
}
