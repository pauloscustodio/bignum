/*
Toy-bignum package, developed just for the fun of it

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

#pragma once

#include <stdbool.h>
#include <stdint.h>
#include "utstring.h"

#ifndef NUM_FRAC_SIZE
#  ifdef TEST
#    define NUM_FRAC_SIZE 16
#  else
#    define NUM_FRAC_SIZE 1024
#  endif
#endif

typedef uint8_t digit_t;
#define NUM_BASE 10

typedef struct Num {
    bool    negative;               // true if negative
    int     exp;                    // power of 10 to multiply first digit
    int     n, size;                // number of digits in frac, total size of frac
    digit_t frac[NUM_FRAC_SIZE];    // digits 0-9
} Num;

void num_init_zero(Num* num);
bool num_is_zero(const Num* num);
void num_to_str(UT_string* out, const Num* num);
void num_print(const Num* num);
void num_normalize(Num* num);
void num_init_int(Num* num, long long value);
void num_init_exp(Num* num, long long value, int exp);
void num_init_double(Num* num, long double value);
long double num_to_double(const Num* num);
int num_compare(const Num* a, const Num* b);
void num_add(Num* res, const Num* a, const Num* b);
void num_sub(Num* res, const Num* a, const Num* b);
void num_mult(Num* res, const Num* a, const Num* b);
void num_div(Num* res, const Num* a, const Num* b);
