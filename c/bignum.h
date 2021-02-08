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
#include "utstring.h"

extern int div_max_digits;  // maximum decimals for division

typedef struct Num {
    bool negative;          // true if negative
    int  pow10;             // power of 10 to multiply first digit
    UT_string* digits;      // ASCII digits 0-9
} Num;

Num* num_new();
void num_set_zero(Num* num);
void num_copy(Num* dst, const Num* src);
void num_free(Num* num);
void num_normalize(Num* num);
void num_init_int(Num* num, int value);
void num_init_str(Num* num, const char* value);
void num_init_double(Num* num, double value);
double num_to_double(const Num* num);
void num_to_string(UT_string* dest, const Num* num);
void num_print(const Num* num);
bool num_is_zero(Num* num);
int num_compare(Num* a, Num* b);
int num_digits(Num* num);
void num_add(Num* res, Num* a, Num* b);
void num_sub(Num* res, Num* a, Num* b);
void num_mult(Num* res, Num* a, Num* b);
void num_div(Num* quotient, Num* a, Num* b);
