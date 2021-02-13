/*
Compute PI with the Leibnitz formula:
    PI = 4 - 4/3 + 4/5 - 4/7 + 4/9
Simple, yet converges very slowly

Copyright (c) 2021, Paulo Custodio
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>

#define NUM_FRAC_SIZE 4096
#include "bignum.h"

void compute_pi(Num* pi, int digits) {
    UT_string* out;
    utstring_new(out);

    num_init_zero(pi);
    Num n2;					// 2
    num_init_int(&n2, 2);
    Num n4;					// 4
    num_init_int(&n4, 4);
    Num series;				// has 1+2*i
    num_init_int(&series, 1);
    Num term;				// 4/(1+2*i)

    int i = 0;
    int old_exp = INT_MAX;
    bool negative = true;				// for second term
    do {
        num_div(&term, &n4, &series);	// compute each term
        num_add(pi, pi, &term);			// accumulate

        series.negative = false;		// make positive
        num_add(&series, &series, &n2);	// add 2
        series.negative = negative;		// and apply correct sign

        if (old_exp != term.exp) {
            num_to_str(out, pi);
            if (term.exp < 0 && (int)utstring_len(out) > -term.exp)
                utstring_body(out)[-term.exp + 1] = '\0';		// truncate significant digits
            printf("pi = %s\n", utstring_body(out));

            old_exp = term.exp;
        }

        i++;							// next term
        negative = !negative;			// swap sign
    } while (-term.exp + 1 < digits);

    utstring_free(out);
}

#ifndef TEST
int main(int argc, char* argv[]) {
    if (argc == 2) {
        Num pi;
        compute_pi(&pi, atoi(argv[1]));
        num_print(&pi);
        printf("\n");
    }
}
#endif
