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

#include "bignum.h"
#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIN(a,b)            ((a)<(b)?(a):(b))
#define MAX(a,b)            ((a)>(b)?(a):(b))

// maximum decimals for division
#ifdef TEST
int div_max_digits = 32;
#else
int div_max_digits = 2048;
#endif

#define DIGITS(num)         ((int)utstring_len((num)->digits))
#define DIGIT(num,i)        utstring_body((num)->digits)[i]

static void* check_mem(void* p) {
    if (!p) {
        fputs("out of memory", stderr);
        exit(EXIT_FAILURE);
    }
    return p;
}

Num* num_new() {
    Num* num = check_mem(calloc(1, sizeof(Num)));
    utstring_new(num->digits);
    num_set_zero(num);
    return num;
}

void num_set_zero(Num* num) {
    utstring_clear(num->digits);
    utstring_printf(num->digits, "0");
    num->pow10 = 0;
    num->negative = false;
}

void num_copy(Num* dst, const Num* src) {
    dst->negative = src->negative;
    dst->pow10 = src->pow10;
    utstring_clear(dst->digits); utstring_concat(dst->digits, src->digits);
}

void num_free(Num* num) {
    utstring_free(num->digits);
    free(num);
}

static void insert_zeros(Num* num, int zeros) {
    if (zeros > 0) {
        UT_string* temp; utstring_new(temp);
        utstring_printf(temp, "%0*d%s", zeros, 0, utstring_body(num->digits));
        utstring_clear(num->digits); utstring_concat(num->digits, temp);
        utstring_free(temp);
        num->pow10 += zeros;
    }
}

static void append_zeros(Num* num, int zeros) {
    if (zeros > 0)
        utstring_printf(num->digits, "%0*d", zeros, 0);
}

void num_normalize(Num* num) {
    if (utstring_len(num->digits) == 0 || num_is_zero(num))
        num_set_zero(num);
    else {
        // skip initial zeros
        int i;
        for (i = 0; i < DIGITS(num) && DIGIT(num, i) == '0'; i++) {
            num->pow10--;
        }
        if (i == DIGITS(num))
            num_set_zero(num);
        else if (i != 0) {
            memmove(&DIGIT(num, 0), &DIGIT(num, i), DIGITS(num) - i + 1);
            utstring_len(num->digits) = strlen(utstring_body(num->digits));
        }

        // cut end zeros
        for (i = DIGITS(num) - 1; i > 0 && DIGIT(num, i) == '0'; i--) {
        }
        DIGIT(num, i + 1) = '\0';
        utstring_len(num->digits) = strlen(utstring_body(num->digits));
    }
}

void num_init_int(Num* num, int value) {
    if (value < 0) {
        num->negative = true;
        value = -value;
    }
    else
        num->negative = false;
    utstring_clear(num->digits);
    utstring_printf(num->digits, "%d", value);
    num->pow10 = utstring_len(num->digits) - 1;
    num_normalize(num);
}

void num_init_str(Num* num, const char* value) {
    num->negative = false;
    while (isspace(*value) || *value == '+')
value++;
if (*value == '-') {
    num->negative = true;
    value++;
}
while (isspace(*value))
value++;
utstring_clear(num->digits);
bool seen_dot = false, seen_digit = false;
num->pow10 = 0;
while (*value) {
    if (isdigit(*value)) {
        utstring_printf(num->digits, "%c", *value++);
        if (!seen_dot && seen_digit)
            num->pow10++;
        seen_digit = true;
    }
    else if (!seen_dot && *value == '.') {
        seen_dot = true;
        if (!seen_digit) {
            utstring_printf(num->digits, "0");
            seen_digit = true;
        }
        value++;
    }
    else
        break;
}
num_normalize(num);
}

void num_init_double(Num* num, double value) {
    if (value == 0.0) {
        num_set_zero(num);
    }
    else {
        // sign
        if (value < 0) {
            num->negative = true;
            value = -value;
        }
        else
            num->negative = false;

        // convert to a number less than 10.0
        num->pow10 = 0;
        while (value >= 10.0) {
            value /= 10.0;
            num->pow10++;
        }

        // convert to X.xxx, X!=0
        while (value < 1.0 && value != 0.0) {
            value *= 10.0;
            num->pow10--;
        }

        // convert each digit
        utstring_clear(num->digits);
        do {
            double digit = floor(value);
            value -= digit;
            utstring_printf(num->digits, "%c", (int)digit + '0');
            value *= 10.0;
        } while (value != 0.0);

        num_normalize(num);
    }
}

double num_to_double(const Num* num) {
    double value = 0.0;
    double factor = 1.0;
    for (int i = 0; i < DIGITS(num); i++) {
        value = value * 10.0 + DIGIT(num, i) - '0';
        factor /= 10.0;
    }
    value *= factor;
    value *= pow(10.0, (double)num->pow10 + 1);
    if (num->negative)
        value = -value;

    return value;
}

void num_to_string(UT_string* dest, const Num* num) {
    utstring_clear(dest);

    utstring_printf(dest, (num->negative) ? "-" : " ");

    if (num->pow10 < 0) {
        utstring_printf(dest, "0.");
        int top_zeros = -num->pow10 - 1;
        if (top_zeros > 0)
            utstring_printf(dest, "%0*d", top_zeros, 0);
        utstring_printf(dest, "%s", utstring_body(num->digits));
    }
    else {
        for (int i = 0; i < DIGITS(num); i++) {
            utstring_printf(dest, "%c", DIGIT(num, i));
            if (num->pow10 - i == 0 && i + 1 != DIGITS(num))
                utstring_printf(dest, ".");
        }
        int bottom_zeros = num->pow10 - (DIGITS(num) - 1);
        if (bottom_zeros > 0)
            utstring_printf(dest, "%0*d", bottom_zeros, 0);
    }
}

void num_print(const Num* num) {
    UT_string* str; utstring_new(str);
    num_to_string(str, num);
    printf("%s", utstring_body(str));
    utstring_free(str);
}

static void align_addition(Num* res, Num* a, Num* b) {
    // insert zeros
    if (a->pow10 < b->pow10)
        insert_zeros(a, b->pow10 - a->pow10);
    else if (a->pow10 > b->pow10)
        insert_zeros(b, a->pow10 - b->pow10);

    // append zeros
    if (DIGITS(a) < DIGITS(b))
        append_zeros(a, DIGITS(b) - DIGITS(a));
    else if (DIGITS(a) > DIGITS(b))
        append_zeros(b, DIGITS(a) - DIGITS(b));

    // define result with zeros
    if (res) {
        res->negative = false;
        res->pow10 = a->pow10;
        utstring_clear(res->digits);
        append_zeros(res, DIGITS(a));
    }
}

bool num_is_zero(Num* num) {
    for (int i = 0; i < DIGITS(num); i++) {
        if (DIGIT(num, i) != '0')
            return false;
    }
    return true;
}

static int compare_1(Num* a, Num* b) {
    if (!a->negative) {                     // A >= 0
        if (!b->negative)                   // A >= 0 && B >= 0
            return strcmp(&DIGIT(a, 0), &DIGIT(b, 0));
        else                                // A >= 0 && B < 0
            return 1;
    }
    else {                                  // A < 0
        if (!b->negative)                   // A < 0 && B >= 0
            return -1;
        else {                              // A < 0 && B < 0
            a->negative = !a->negative;     // invert sign
            b->negative = !b->negative;
            int cmp = -compare_1(a, b);     // invert result
            a->negative = !a->negative;     // revert sign
            b->negative = !b->negative;
            return cmp;
        }
    }
}

int num_compare(Num* a, Num* b) {
    align_addition(NULL, a, b);
    int cmp = compare_1(a, b);
    num_normalize(a);
    num_normalize(b);
    return cmp;
}

int num_digits(Num* num) {
    return DIGITS(num);
}

static void add_positive(Num* res, Num* a, Num* b) {
    align_addition(res, a, b);

    // add digits
    int carry = 0;
    for (int i = DIGITS(res) - 1; i >= 0; i--) {
        int temp = (DIGIT(a, i) - '0') + (DIGIT(b, i) - '0') + carry;
        DIGIT(res, i) = temp % 10 + '0';
        carry = temp / 10;
    }
    if (carry != 0) {
        insert_zeros(res, 1);
        DIGIT(res, 0) = carry + '0';
    }
    num_normalize(a);
    num_normalize(b);
    num_normalize(res);
}

static void sub_positive_1(Num* res, Num* a, Num* b) {
    // subtract digits
    align_addition(res, a, b);
    int borrow = 0;
    for (int i = DIGITS(res) - 1; i >= 0; i--) {
        int temp = (DIGIT(a, i) - '0') - (DIGIT(b, i) - '0') - borrow;
        if (temp < 0) {
            temp += 10;
            borrow = 1;
        }
        else
            borrow = 0;
        DIGIT(res, i) = temp + '0';
    }
    assert(borrow == 0);    // fails if a<b

    num_normalize(a);
    num_normalize(b);
    num_normalize(res);
}

static void sub_positive(Num* res, Num* a, Num* b) {
    if (num_compare(a, b) < 0) {
        sub_positive_1(res, b, a);
        res->negative = !res->negative;
    }
    else
        sub_positive_1(res, a, b);
}

void num_add(Num* res, Num* a, Num* b) {
    if (!a->negative) {                     // A >= 0
        if (!b->negative)                   // A >= 0 && B >= 0 => +A + +B = A+B
            add_positive(res, a, b);
        else {                              // A >= 0 && B < 0  => +A + -B = A-B
            b->negative = !b->negative;
            sub_positive(res, a, b);
            b->negative = !b->negative;
        }
    }
    else {                                  // A < 0
        if (!b->negative) {                 // A < 0 && B >= 0  => -A + +B = B-A
            a->negative = !a->negative;
            sub_positive(res, b, a);
            a->negative = !a->negative;
        }
        else {                              // A < 0 && B < 0   => -A + -B = -(A+B)
            a->negative = !a->negative;
            b->negative = !b->negative;
            add_positive(res, a, b);
            a->negative = !a->negative;
            b->negative = !b->negative;
            res->negative = !res->negative;
        }
    }
}

void num_sub(Num* res, Num* a, Num* b) {
    if (!a->negative) {                     // A >= 0
        if (!b->negative)                   // A >= 0 && B >= 0 => +A - +B = A-B
            sub_positive(res, a, b);
        else {                              // A >= 0 && B < 0  => +A - -B = A+B
            b->negative = !b->negative;
            add_positive(res, a, b);
            b->negative = !b->negative;
        }
    }
    else {                                  // A < 0
        if (!b->negative) {                 // A < 0 && B >= 0  => -A - +B = -(A+B)
            a->negative = !a->negative;
            add_positive(res, a, b);
            a->negative = !a->negative;
            res->negative = !res->negative;
        }
        else {                              // A < 0 && B < 0   => -A - -B = B-A
            a->negative = !a->negative;
            b->negative = !b->negative;
            sub_positive(res, b, a);
            a->negative = !a->negative;
            b->negative = !b->negative;
        }
    }
}

void num_mult(Num* res, Num* a, Num* b) {
    num_normalize(a);
    num_normalize(b);
    utstring_clear(res->digits);
    append_zeros(res, DIGITS(a) + DIGITS(b));
    res->negative = (a->negative == b->negative) ? false : true;
    res->pow10 = a->pow10 + b->pow10 + 1;

    for (int di = 0; di < DIGITS(a); di++) {
        int i = DIGITS(a) - 1 - di;
        if (DIGIT(a, i) != '0') {
            for (int dj = 0; dj < DIGITS(b); dj++) {
                int j = DIGITS(b) - 1 - dj;
                if (DIGIT(b, j) != '0') {
                    int dk = di + dj;
                    int k = DIGITS(res) - 1 - dk;

                    // multiply and add
                    int carry = (DIGIT(a, i) - '0') * (DIGIT(b, j) - '0');
                    do {
                        assert(k >= 0);     // detect overflow
                        int temp = (DIGIT(res, k) - '0') + carry;
                        DIGIT(res, k) = temp % 10 + '0';
                        carry = temp / 10;
                        k--;
                    } while (carry != 0);
                }
            }
        }
    }
    num_normalize(res);
}

void num_div(Num* quotient, Num* a, Num* b) {
    assert(!num_is_zero(b));        // trap division by zero

    num_normalize(a);
    num_normalize(b);

    // make dividend
    Num* dividend = num_new();
    num_copy(dividend, a);

    Num* save_dividend = num_new();

    // make divisor with one extra left zero, to allow for partial products to overflow
    Num* divisor = num_new();
    num_copy(divisor, b);
    insert_zeros(divisor, 1);

    // make dividend larger than divisor
    if (DIGITS(dividend) <= DIGITS(divisor)) {
        int zeros = DIGITS(divisor) - DIGITS(dividend) + 1;
        append_zeros(dividend, zeros);
    }

    // make result quotient
    utstring_clear(quotient->digits);
    insert_zeros(quotient, DIGITS(divisor));        // to match divisor's extra zero
    quotient->negative = (a->negative == b->negative) ? false : true;
    quotient->pow10 = dividend->pow10 - divisor->pow10 + 2;

    // make partial product
    Num* product = num_new();

    // enter a loop for each partial division
    int width = DIGITS(divisor);
    for (int i = 0; i + width < DIGITS(dividend); i++) {
        // find left digit(s) of dividend
        int dividend_digit = DIGIT(dividend, i) - '0';
        if (i > 0)
            dividend_digit += 10 * (DIGIT(dividend, i - 1) - '0');

        // divide by first significant digit of divisor to estimate quotient
        int quotient_digit = MIN(9, dividend_digit / (DIGIT(divisor, 1) - '0'));

        // loop if estimate was wrong
        quotient_digit++;
        int cmp;
        do {
        retry:
            quotient_digit--;               // decrement on each pass

            // multipy digit by divisor

            // make partial product the same size as divisor
            utstring_clear(product->digits);
            insert_zeros(product, DIGITS(divisor));

            if (quotient_digit == 1) {
                utstring_clear(product->digits);
                utstring_concat(product->digits, divisor->digits);
            }
            else if (quotient_digit > 1) {
                int carry = 0;
                for (int j = DIGITS(divisor) - 1; j >= 0; j--) {
                    int prod = (DIGIT(divisor, j) - '0') * quotient_digit + carry;
                    DIGIT(product, j) = prod % 10 + '0';
                    carry = prod / 10;
                }
                assert(carry == 0);         // because we added an extra zero
            }

            // repeat decrementing q_digit while there was overflow in the product
            // or product > part of dividend
            if (i == 0)
                cmp = memcmp(&DIGIT(product, 1), &DIGIT(dividend, i), width - 1);
            else
                cmp = memcmp(&DIGIT(product, 0), &DIGIT(dividend, i - 1), width);
        } while (cmp > 0);

        // append quotient digit
        if (i + 1 < DIGITS(quotient))
            DIGIT(quotient, i + 1) = quotient_digit + '0';
        else
            utstring_printf(quotient->digits, "%c", quotient_digit + '0');

        // subtract product from dividend
        if (!num_is_zero(product)) {
            num_copy(save_dividend, dividend);          // make a copy in case we need to revert

            int borrow = 0;
            for (int j = DIGITS(product) - 1; j >= 0; j--) {
                int di = i + j - 1;
                int sub = (di >= 0 ? (DIGIT(dividend, di) - '0') : 0)
                    - (DIGIT(product, j) - '0') - borrow;
                if (sub < 0) {
                    sub += 10;
                    borrow = 1;
                }
                else
                    borrow = 0;
                if (di >= 0)
                    DIGIT(dividend, di) = sub + '0';
                else if (sub != 0) {                    // estimate failed, retry
                    num_copy(dividend, save_dividend);  // revert dividend
                    goto retry;
                }
            }
            assert(borrow == 0);
        }

        // loop back if dividend not zero, append zero if needed
        if (num_is_zero(dividend))
            break;
        else if (i + 1 + width >= DIGITS(dividend)) {   // last pass through loop
            int min_digits = MAX(div_max_digits, 2 * MAX(DIGITS(a), DIGITS(b)));
            if (DIGITS(dividend) < min_digits) {
                int zeros = min_digits - DIGITS(dividend);
                append_zeros(dividend, zeros);
            }
        }
    }

    // normalize
    num_normalize(quotient);

    // cleanup
    num_free(save_dividend);
    num_free(dividend);
    num_free(divisor);
    num_free(product);
}

#ifdef TEST
#define IS(num, str)    do{ num_to_string(text_, num); \
                            if (0 != strcmp(str, utstring_body(text_))) { \
                                printf("Got %s, expected %s\n", \
                                        utstring_body(text_), str); \
                                assert(0); \
                            } \
                        }while(0)

#define APROX(num, exp) do{ double got = num_to_double(num); \
                            double err = fabs(got - (exp)); \
                            if ( (exp) != 0.0 ) err /= (exp); \
                            if (err >= 0.01) { \
                                printf("got %g, expected %g, err %g\n", \
                                       got, (double)(exp), err); assert(0); \
                            } \
                        }while (0)

int main() {
    UT_string* text; utstring_new(text);
    UT_string* text_; utstring_new(text_);
    Num* a = num_new();
    Num* b = num_new();
    Num* res = num_new();
    Num* tst = num_new();

    // init int
    num_init_int(a, 0); IS(a, " 0"); assert(a->pow10 == 0); assert(num_is_zero(a));
    num_init_int(a, 1); IS(a, " 1"); assert(a->pow10 == 0); assert(!num_is_zero(a));
    num_init_int(a, -1); IS(a, "-1"); assert(a->pow10 == 0); assert(!num_is_zero(a));
    num_init_int(a, 12345678); IS(a, " 12345678"); assert(a->pow10 == 7); assert(!num_is_zero(a));

    // copy
    num_copy(b, a); IS(b, " 12345678"); assert(b->pow10 == 7); assert(!num_is_zero(b));

    // num_to_string
    num_init_int(a, 1); IS(a, " 1");
    a->pow10++; IS(a, " 10");
    a->pow10++; IS(a, " 100");
    a->pow10++; IS(a, " 1000");
    num_init_int(a, 1); IS(a, " 1");
    a->pow10--; IS(a, " 0.1");
    a->pow10--; IS(a, " 0.01");
    a->pow10--; IS(a, " 0.001");
    a->pow10--; IS(a, " 0.0001");

    // init string
    num_init_str(a, "no number"); IS(a, " 0"); assert(a->pow10 == 0); assert(num_is_zero(a));
    num_init_str(a, "123no number"); IS(a, " 123"); assert(a->pow10 == 2); assert(!num_is_zero(a));
    num_init_str(a, "0"); IS(a, " 0"); assert(a->pow10 == 0); assert(num_is_zero(a));
    num_init_str(a, "123.456"); IS(a, " 123.456"); assert(a->pow10 == 2); assert(!num_is_zero(a));
    num_init_str(a, "0000123.4560000"); IS(a, " 123.456"); assert(a->pow10 == 2); assert(!num_is_zero(a));
    num_init_str(a, "00001234560000"); IS(a, " 1234560000"); assert(a->pow10 == 9); assert(!num_is_zero(a));
    num_init_str(a, "10"); IS(a, " 10"); assert(a->pow10 == 1); assert(!num_is_zero(a));
    num_init_str(a, "1.0"); IS(a, " 1"); assert(a->pow10 == 0); assert(!num_is_zero(a));
    num_init_str(a, ".10"); IS(a, " 0.1"); assert(a->pow10 == -1); assert(!num_is_zero(a));
    num_init_str(a, "+ + -.10"); IS(a, "-0.1"); assert(a->pow10 == -1); assert(!num_is_zero(a));

    // num_init_double
    num_init_double(a, 0.0); IS(a, " 0");
    num_init_double(a, 2.0); APROX(a, 2);
    num_init_double(a, 2e10); APROX(a, 20000000000);
    num_init_double(a, 2e-10); APROX(a, 0.0000000002);
    num_init_double(a, 256.0); APROX(a, 256);
    num_init_double(a, -256.0); APROX(a, -256);
    num_init_double(a, 0.2); APROX(a, 0.2);
    num_init_double(a, 0.256); APROX(a, 0.256);
    num_init_double(a, 0.256e2); APROX(a, 25.6);
    num_init_double(a, 123.456); APROX(a, 123.456);
    num_init_double(a, -123.456); APROX(a, -123.456);
    num_set_zero(a); IS(a, " 0");

    // align for addition
    num_init_str(a, "123456"); IS(a, " 123456");
    num_init_str(b, ".123456"); IS(b, " 0.123456");
    align_addition(res, a, b);
    IS(a,   " 123456.000000");
    IS(b,   " 000000.123456");
    IS(res, " 000000.000000");

    num_init_str(a, ".123456"); IS(a, " 0.123456");
    num_init_str(b, "123456"); IS(b, " 123456");
    align_addition(res, a, b);
    IS(a,   " 000000.123456");
    IS(b,   " 123456.000000");
    IS(res, " 000000.000000");

    // compare
    num_init_int(a, 10); IS(a, " 10"); num_init_int(b, 1); IS(b, " 1"); assert(num_compare(a, b) == 1);
    num_init_int(a, 10); IS(a, " 10"); num_init_int(b, 10); IS(b, " 10"); assert(num_compare(a, b) == 0);
    num_init_int(a, 10); IS(a, " 10"); num_init_int(b, 100); IS(b, " 100"); assert(num_compare(a, b) == -1);

    for (int i = -1; i <= 1; i++) {
        for (int j = -1; j <= 1; j++) {
            num_init_int(a, i);
            num_init_int(b, j);
            if (i < j) assert(num_compare(a, b) == -1);
            else if (i == j) assert(num_compare(a, b) == 0);
            else assert(num_compare(a, b) == 1);
        }
    }

    // add positive
    num_init_str(a, ".123456"); IS(a, " 0.123456");
    num_init_str(b, "123456"); IS(b, " 123456");
    add_positive(res, a, b);
    IS(a, " 0.123456");
    IS(b, " 123456");
    IS(res, " 123456.123456");

    num_init_str(a, "99999999"); IS(a, " 99999999");
    num_init_str(b, "99999999"); IS(b, " 99999999");
    add_positive(res, a, b);
    IS(a, " 99999999");
    IS(b, " 99999999");
    IS(res, " 199999998");

    // sub positive
    num_init_str(a, "199999998"); IS(a, " 199999998");
    num_init_str(b, "99999999"); IS(b, " 99999999");
    sub_positive(res, a, b);
    IS(a, " 199999998");
    IS(b, " 99999999");
    IS(res, " 99999999");

    num_init_str(a, "99999999"); IS(a, " 99999999");
    num_init_str(b, "199999998"); IS(b, " 199999998");
    sub_positive(res, a, b);
    IS(a, " 99999999");
    IS(b, " 199999998");
    IS(res, "-99999999");

    // multiplication
    num_init_str(a, "123.456"); IS(a, " 123.456");
    num_init_str(b, "999.9"); IS(b, " 999.9");
    num_mult(res, a, b);
    IS(a, " 123.456");
    IS(b, " 999.9");
    IS(res, " 123443.6544");

    // division
    num_init_str(a, "123443.6544"); IS(a, " 123443.6544");
    num_init_str(b, "999.9"); IS(b, " 999.9");
    num_div(res, a, b);
    IS(a, " 123443.6544");
    IS(b, " 999.9");
    IS(res, " 123.456");

    num_init_str(a, "10"); IS(a, " 10");
    num_init_str(b, "3"); IS(b, " 3");
    num_div(res, a, b);
    IS(a, " 10");
    IS(b, " 3");
    APROX(res, 10.0 / 3.0);

    num_init_str(a, "10"); IS(a, " 10");
    num_init_str(b, "1"); IS(b, " 1");
    num_div(res, a, b);
    IS(a, " 10");
    IS(b, " 1");
    IS(res, " 10");

    num_init_str(a, "4"); IS(a, " 4");
    num_init_str(b, "1"); IS(b, " 1");
    num_div(res, a, b);
    IS(a, " 4");
    IS(b, " 1");
    IS(res, " 4");

    num_init_str(a, "4"); IS(a, " 4");
    num_init_str(b, "997"); IS(b, " 997");
    num_div(res, a, b);
    IS(a, " 4");
    IS(b, " 997");
    APROX(res, 4.0 / 997.0);

    num_init_str(a, "0"); IS(a, " 0");
    num_init_str(b, "1"); IS(b, " 1");
    num_div(res, a, b);
    IS(a, " 0");
    IS(b, " 1");
    IS(res, " 0");

    num_init_str(a, "1"); IS(a, " 1");
    num_init_str(b, "1"); IS(b, " 1");
    num_div(res, a, b);
    IS(a, " 1");
    IS(b, " 1");
    IS(res, " 1");

    num_init_str(a, "1"); IS(a, " 1");
    num_init_str(b, "1"); IS(b, " 1");
    num_div(res, a, b);
    IS(a, " 1");
    IS(b, " 1");
    IS(res, " 1");

    for (double i = -10.0; i <= 10.0; i += 0.1) {
        for (double j = -10.0; j <= 10.0; j += 0.1) {
            for (double k = 0.01; k <= 10; k *= 10) {
                num_init_double(a, i*k);
                num_init_double(b, j);

                // addition
                num_add(res, a, b); APROX(res, i*k + j);

                // subtraction
                num_sub(res, a, b); APROX(res, i*k - j);

                // multiplication
                num_mult(res, a, b); APROX(res, i*k * j);

                // division
                if (j != 0) {
                    num_div(res, a, b); APROX(res, i * k / j);
                }
            }
        }
    }

    num_free(a);
    num_free(b);
    num_free(res);
    num_free(tst);
    utstring_free(text);
    utstring_free(text_);
    printf("All tests passed\n");
}
#endif

