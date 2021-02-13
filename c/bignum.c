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

#define DBL_FRAC_SIZE       (2 * NUM_FRAC_SIZE)

// structure for intermediate computations
// overlaps Num but with double digits
typedef struct Dbl {
    Num     num;
    digit_t frac2[NUM_FRAC_SIZE];   // follows immediately num.frac[]
} Dbl;

void num_init_zero(Num* num) {
    num->negative = false;
    num->exp = 0;
    num->n = 1;
    num->size = NUM_FRAC_SIZE;
    num->frac[0] = 0;
}

static void dbl_init_zero(Dbl* dbl) {
    num_init_zero(&dbl->num);
    dbl->num.size = DBL_FRAC_SIZE;
}

bool num_is_zero(const Num* num) {
    for (int i = 0; i < num->n; i++)
        if (num->frac[i] != 0)
            return false;
    return true;
}

static bool num_check_zero(Num* num) {
    if (num_is_zero(num)) {
        num_init_zero(num);
        return true;
    }
    else
        return false;
}

static bool dbl_check_zero(Dbl* dbl) {
    if (num_is_zero(&dbl->num)) {
        dbl_init_zero(dbl);
        return true;
    }
    else
        return false;
}

void num_to_str(UT_string* out, const Num* num) {
    utstring_clear(out);
    if (num_is_zero(num))
        utstring_printf(out, "0");
    else {
        if (num->negative)
            utstring_printf(out, "-");
        if (num->exp < 0) {             // 0.nnn
            int zeros = -(num->exp) - 1;
            utstring_printf(out, "0.");
            for (int i = 0; i < zeros; i++)
                utstring_printf(out, "0");
            for (int i = 0; i < num->n; i++) {
                utstring_printf(out, "%c", num->frac[i] + '0');
            }
        }
        else {                      // n.nnn
            int num_ints = num->exp + 1;
            for (int i = 0; i < num->n && i < num->n; i++) {
                if (num_ints-- == 0)
                    utstring_printf(out, ".");
                utstring_printf(out, "%c", num->frac[i] + '0');
            }
            for (int i = 0; i < num_ints; i++)
                utstring_printf(out, "0");
        }
    }
}

void num_print(const Num* num) {
    UT_string* str;
    utstring_new(str);
    num_to_str(str, num);
    printf("%s", utstring_body(str));
    utstring_free(str);
}

#ifdef TEST
static void dbl_to_str(UT_string* out, const Dbl* dbl) {
    utstring_clear(out);
    if (num_is_zero(&dbl->num))
        utstring_printf(out, "0");
    else
        num_to_str(out, &dbl->num);
}
#endif

static void ripple_add(Num* num, int idx, int value);

static void insert_digit(Num* num, int value) {
    int num_move = MIN(num->n, num->size - 1);

    bool round_up = false;
    if (num_move == num->size - 1 && num->frac[num->size - 1] >= NUM_BASE / 2)
        round_up = true;

    memmove(num->frac + 1, num->frac, num_move * sizeof(digit_t));
    num->frac[0] = value % NUM_BASE;
    num->n = num_move + 1;
    num->exp++;

    if (round_up)
        ripple_add(num, num->n - 1, 1);
}

static void insert_digits(Num* num, int value) {
    assert(value >= 0);
    while (value != 0) {
        insert_digit(num, value);
        value /= NUM_BASE;
    }
}

static void ripple_add(Num* num, int idx, int value) {
    assert(idx >= 0 && idx < num->n);
    int carry = value;
    for (int i = idx; i >= 0; i--) {
        int add = num->frac[i] + carry;
        num->frac[i] = add % NUM_BASE;
        carry = add / NUM_BASE;
        if (carry == 0)
            break;
    }
    if (carry != 0)
        insert_digits(num, carry);
}

static void normalize(Num* num, int max_size) {
    // skip initial zeros
    int i = 0;
    while (i < max_size && num->frac[i] == 0)
        i++;
    assert(i < max_size);
    if (i != 0) {
        memmove(num->frac, num->frac + i, num->n - i);
        num->exp -= i;
        num->n -= i;
    }

    // cut end zeros
    while (num->n > 1 && num->frac[num->n - 1] == 0)
        num->n--;
}

static void dbl_normalize(Dbl* dbl) {
    if (!dbl_check_zero(dbl))
        normalize(&dbl->num, DBL_FRAC_SIZE);
}

void num_normalize(Num* num) {
    if (!num_check_zero(num))
        normalize(num, NUM_FRAC_SIZE);
}

static void num_to_dbl(Dbl* dst, const Num* src) {
    memcpy(dst, src, sizeof(Num));
    dst->num.size = DBL_FRAC_SIZE;
    dbl_normalize(dst);
}

static void dbl_to_num(Num* dst, Dbl* src) {
    dbl_normalize(src);
    // round if necessary
    if (src->num.n >= NUM_FRAC_SIZE && src->num.frac[NUM_FRAC_SIZE] >= NUM_BASE / 2)
        ripple_add(&src->num, NUM_FRAC_SIZE - 1, 1);
    memcpy(dst, &src->num, sizeof(Num));
    dst->size = NUM_FRAC_SIZE;
    dst->n = MIN(dst->n, NUM_FRAC_SIZE);
    num_normalize(dst);
}

static void init_int(Num* num, long long value) {
    // sign
    if (value < 0) {
        num->negative = true;
        value = -value;
    }
    else
        num->negative = false;

    memset(num->frac, 0, num->size * sizeof(digit_t));
    num->n = num->size;
    num->exp = num->size - 1;
    for (int i = num->size - 1; i >= 0 && value != 0; i--) {
        num->frac[i] = (digit_t)(value % NUM_BASE);
        value /= NUM_BASE;
    }
    assert(value < INT_MAX);
    if (value != 0)
        insert_digits(num, (int)value);
}

#ifdef TEST
static void dbl_init_int(Dbl* dbl, long long value) {
    dbl_init_zero(dbl);
    init_int(&dbl->num, value);
    dbl_normalize(dbl);
}
#endif

void num_init_int(Num* num, long long value) {
    num_init_zero(num);
    init_int(num, value);
    num_normalize(num);
}

static void init_double(Num* num, long double value) {
    // sign
    if (value < 0) {
        num->negative = true;
        value = -value;
    }
    else
        num->negative = false;

    // convert to a number less than NUM_BASE
    num->exp = 0;
    while (value >= NUM_BASE) {
        value /= NUM_BASE;
        num->exp++;
    }

    // convert to X.xxx, X!=0
    while (value < 1.0 && value != 0.0) {
        value *= NUM_BASE;
        num->exp--;
    }

    // convert each digit
    num->n = 0;
    do {
        double digit = floor(value);
        num->frac[num->n++] = (digit_t)digit;
        value = (value - digit)*NUM_BASE;
    } while (value != 0.0 && num->n < num->size);

    // round if needed
    if (floor(value) >= NUM_BASE / 2)
        ripple_add(num, num->n - 1, 1);
}

#ifdef TEST
static void dbl_init_double(Dbl* dbl, long double value) {
    dbl_init_zero(dbl);
    if (value != 0.0)
        init_double(&dbl->num, value);
    dbl_normalize(dbl);
}
#endif

void num_init_double(Num* num, long double value) {
    num_init_zero(num);
    if (value != 0.0)
        init_double(num, value);
    num_normalize(num);
}

long double num_to_double(const Num* num) {
    long double value = 0.0;
    long double factor = 1.0;
    for (int i = 0; i < num->n; i++) {
        value = value * NUM_BASE + num->frac[i];
        factor /= NUM_BASE;
    }
    value *= factor;
    value *= pow(NUM_BASE, (double)num->exp + 1);
    if (num->negative)
        value = -value;

    return value;
}

#ifdef TEST
static long double dbl_to_double(const Dbl* dbl) {
    return num_to_double(&dbl->num);
}
#endif

static int compare(Num* a, Num* b) {
    if (num_is_zero(a)) {                   // A == 0
        if (num_is_zero(b))                 // B == 0
            return 0;
        else if (b->negative)               // A == 0 && B < 0
            return 1;
        else                                // A == 0 && B > 0
            return -1;
    }
    else if (num_is_zero(b))
        return -compare(b, a);
    else if (!a->negative) {                // A >= 0
        if (!b->negative) {                 // A >= 0 && B >= 0
            if (a->exp > b->exp)
                return 1;
            else if (a->exp < b->exp)
                return -1;
            else {
                int d = MAX(a->n, b->n);
                for (int i = 0; i < d; i++) {
                    if (i >= a->n)
                        return -1;
                    else if (i >= b->n)
                        return 1;
                    else if (a->frac[i] > b->frac[i])
                        return 1;
                    else if (a->frac[i] < b->frac[i])
                        return -1;
                }
                return 0;
            }
        }
        else                                // A >= 0 && B < 0
            return 1;
    }
    else {                                  // A < 0
        if (!b->negative)                   // A < 0 && B >= 0
            return -1;
        else {                              // A < 0 && B < 0
            a->negative = !a->negative;     // invert sign
            b->negative = !b->negative;
            int cmp = -compare(a, b);       // invert result
            a->negative = !a->negative;     // revert sign
            b->negative = !b->negative;
            return cmp;
        }
    }
}

int num_compare(const Num* a, const Num* b) {
    Num a1 = *a;
    Num b1 = *b;
    num_normalize(&a1);
    num_normalize(&b1);
    return compare(&a1, &b1);
}

static int get_digit(const Num* num, int exp) {
    int i = num->exp - exp;
    if (i >= 0 && i < num->n)
        return num->frac[i];
    else
        return 0;
}

static void set_digit(Num* num, int exp, int value) {
    int i = num->exp - exp;
    assert(i >= 0 && i < num->n);
    assert(value < NUM_BASE);
    num->frac[i] = value;
}

struct addition_params_t {
    Dbl dres;
    int a_min_exp, a_max_exp;
    int b_min_exp, b_max_exp;
    int min_exp, max_exp;
    int dres_min_exp, dres_max_exp;
};

static void prepare_addition(struct addition_params_t* params, const Num* a, const Num* b) {
    dbl_init_zero(&params->dres);

    params->a_min_exp = a->exp - (a->n - 1);
    params->a_max_exp = a->exp;

    params->b_min_exp = b->exp - (b->n - 1);
    params->b_max_exp = b->exp;

    params->min_exp = MIN(params->a_min_exp, params->b_min_exp);
    params->max_exp = MAX(params->a_max_exp, params->b_max_exp);

    params->dres_min_exp = MIN(params->min_exp, params->max_exp + params->dres.num.size - 1);
    params->dres_max_exp = params->max_exp;

    params->dres.num.exp = params->dres_max_exp;
    params->dres.num.n = params->dres.num.exp - (params->dres_min_exp - 1);
}

static void add_positive(Num* res, const Num* a, const Num* b) {
    struct addition_params_t params;
    prepare_addition(&params, a, b);

    int carry = 0;
    int last_digit = 0;
    for (int exp = params.min_exp; exp <= params.max_exp; exp++) {
        int add = get_digit(a, exp) + get_digit(b, exp) + carry;
        if (exp >= params.dres_min_exp) {
            if (exp == params.dres_min_exp && last_digit >= NUM_BASE / 2)   // round up
                add++;
            set_digit(&params.dres.num, exp, add % NUM_BASE);
        }
        last_digit = add % NUM_BASE;
        carry = add / NUM_BASE;
    }
    if (carry != 0)
        insert_digits(&params.dres.num, carry);
    dbl_to_num(res, &params.dres);
}

static void sub_positive_1(Num* res, const Num* a, const Num* b) {
    struct addition_params_t params;
    prepare_addition(&params, a, b);

    int borrow = 0;
    int last_digit = 0;
    for (int exp = params.min_exp; exp <= params.max_exp; exp++) {
        int sub = get_digit(a, exp) - get_digit(b, exp) - borrow;
        if (exp >= params.dres_min_exp) {
            if (exp == params.dres_min_exp && last_digit >= NUM_BASE / 2)   // round up
                sub++;
            set_digit(&params.dres.num, exp, (sub + NUM_BASE) % NUM_BASE);
        }
        last_digit = (sub + NUM_BASE) % NUM_BASE;
        borrow = (sub < 0) ? 1 : 0;
    }
    assert(borrow == 0);    // fails if a<b
    dbl_to_num(res, &params.dres);
}

static void sub_positive(Num* res, const Num* a, const Num* b) {
    if (num_compare(a, b) < 0) {
        sub_positive_1(res, b, a);
        res->negative = !res->negative;
    }
    else
        sub_positive_1(res, a, b);
}

void num_add(Num* res, const Num* a, const Num* b) {
    Num a1 = *a, b1 = *b;
    if (!a1.negative) {                     // A >= 0
        if (!b1.negative)                   // A >= 0 && B >= 0 => +A + +B = A+B
            add_positive(res, &a1, &b1);
        else {                              // A >= 0 && B < 0  => +A + -B = A-B
            b1.negative = !b1.negative;
            sub_positive(res, &a1, &b1);
            b1.negative = !b1.negative;
        }
    }
    else {                                  // A < 0
        if (!b1.negative) {                 // A < 0 && B >= 0  => -A + +B = B-A
            a1.negative = !a1.negative;
            sub_positive(res, &b1, &a1);
            a1.negative = !a1.negative;
        }
        else {                              // A < 0 && B < 0   => -A + -B = -(A+B)
            a1.negative = !a1.negative;
            b1.negative = !b1.negative;
            add_positive(res, &a1, &b1);
            a1.negative = !a1.negative;
            b1.negative = !b1.negative;
            res->negative = !res->negative;
        }
    }
}

void num_sub(Num* res, const Num* a, const Num* b) {
    Num a1 = *a, b1 = *b;
    if (!a1.negative) {                     // A >= 0
        if (!b1.negative)                   // A >= 0 && B >= 0 => +A - +B = A-B
            sub_positive(res, &a1, &b1);
        else {                              // A >= 0 && B < 0  => +A - -B = A+B
            b1.negative = !b1.negative;
            add_positive(res, &a1, &b1);
            b1.negative = !b1.negative;
        }
    }
    else {                                  // A < 0
        if (!b1.negative) {                 // A < 0 && B >= 0  => -A - +B = -(A+B)
            a1.negative = !a1.negative;
            add_positive(res, &a1, &b1);
            a1.negative = !a1.negative;
            res->negative = !res->negative;
        }
        else {                              // A < 0 && B < 0   => -A - -B = B-A
            a1.negative = !a1.negative;
            b1.negative = !b1.negative;
            sub_positive(res, &b1, &a1);
            a1.negative = !a1.negative;
            b1.negative = !b1.negative;
        }
    }
}

void num_mult(Num* res, const Num* a, const Num* b) {
    Dbl dres;
    dbl_init_zero(&dres);
    memset(dres.num.frac, 0, dres.num.size * sizeof(digit_t));
    dres.num.n=a->n+b->n;
    dres.num.negative = (a->negative == b->negative) ? false : true;
    dres.num.exp = a->exp + b->exp + 1;

    for (int di = 0; di < a->n; di++) {
        int i = a->n - 1 - di;
        if (a->frac[i] != 0) {
            for (int dj = 0; dj < b->n; dj++) {
                int j = b->n - 1 - dj;
                if (b->frac[j] != 0) {
                    int dk = di + dj;
                    int k = dres.num.n - 1 - dk;

                    // multiply and add
                    int carry = a->frac[i] * b->frac[j];
                    do {
                        assert(k >= 0);     // detect overflow
                        int mul = dres.num.frac[k] + carry;
                        dres.num.frac[k] = mul % NUM_BASE;
                        carry = mul / NUM_BASE;
                        k--;
                    } while (carry != 0);
                }
            }
        }
    }
    dbl_to_num(res, &dres);
}

void num_div(Num* res, const Num* a, const Num* b) {
    assert(!num_is_zero(b));        // trap division by zero

    // make dividend max width
    Dbl dividend, save_dividend;
    num_to_dbl(&dividend, a);
    memset(dividend.num.frac + dividend.num.n, 0, (DBL_FRAC_SIZE - dividend.num.n) * sizeof(digit_t));   // append zeros
    dividend.num.n = DBL_FRAC_SIZE;

    // make divisor with one extra left zero, to allow for partial products to overflow
    Dbl divisor;
    num_to_dbl(&divisor, b);
    insert_digit(&divisor.num, 0);

    // make result quotient
    Dbl quotient;
    memset(quotient.num.frac, 0, DBL_FRAC_SIZE * sizeof(digit_t));  // reset to zeros
    quotient.num.n = divisor.num.n;
    quotient.num.negative = (a->negative == b->negative) ? false : true;
    quotient.num.exp = dividend.num.exp - divisor.num.exp + 2;

    // make partial product
    Dbl product;

    // enter a loop for each partial division
    int width = divisor.num.n;
    for (int i = 0; i + width < dividend.num.n; i++) {

        // find left digit(s) of dividend
        int dividend_digit = dividend.num.frac[i];
        if (i > 0)
            dividend_digit += NUM_BASE * dividend.num.frac[i - 1];

        // divide by first significant digit of divisor to estimate quotient
        int quotient_digit = MIN(9, dividend_digit / divisor.num.frac[1]);

        // loop if estimate was wrong
        quotient_digit++;
        int cmp;
retry:
        do {
            quotient_digit--;               // decrement on each pass

            // multipy digit by divisor
            product = divisor;
            if (quotient_digit == 0)
                memset(product.num.frac, 0, divisor.num.n * sizeof(digit_t));  // reset to zeros
            else if (quotient_digit > 1) {
                int carry = 0;
                for (int j = divisor.num.n - 1; j >= 0; j--) {
                    int prod = divisor.num.frac[j] * quotient_digit + carry;
                    product.num.frac[j] = prod % NUM_BASE;
                    carry = prod / NUM_BASE;
                }
                assert(carry == 0);         // because we added an extra zero
            }

            // repeat decrementing q_digit while there was overflow in the product
            // or product > part of dividend
            if (i == 0)
                cmp = memcmp(product.num.frac + 1, dividend.num.frac + i, width - 1);
            else
                cmp = memcmp(product.num.frac, dividend.num.frac + i - 1, width);
        } while (cmp > 0);

        // subtract product from dividend and retry if overflow
        if (!num_is_zero(&product.num)) {
            save_dividend = dividend;       // make a copy in case we need to revert

            int borrow = 0;
            for (int j = product.num.n - 1; j >= 0; j--) {
                int di = i + j - 1;
                int sub = (di >= 0 ? dividend.num.frac[di] : 0) - product.num.frac[j] - borrow;
                if (di >= 0)
                    dividend.num.frac[di] = (sub + NUM_BASE) % NUM_BASE;
                else if (sub != 0) {            // estimate failed, retry with smaller quotient_digit
                    dividend = save_dividend;   // revert dividend
                    goto retry;
                }
                borrow = (sub < 0) ? 1 : 0;
            }
            assert(borrow == 0);
        }

        // append quotient digit
        quotient.num.n = MAX(quotient.num.n, i + 2);
        quotient.num.frac[i + 1] = quotient_digit;

        // loop back if dividend not zero, append zero if needed
        if (num_is_zero(&dividend.num))
            break;
    }

    dbl_to_num(res, &quotient);
}



#ifdef TEST
#include <time.h>

int test_num = 0;
int test_failed = 0;
UT_string* test_out;

#define OK()			printf("Ok %d\n", ++test_num)
#define CHECK(f)		check((f), __LINE__)

void check(bool f, int line) {
    if (f)
        OK();
    else {
        printf("Nok %d at line %d\n", ++test_num, line);
        test_failed++;
    }
}

#define IS_(num, str, func) \
                        do{ func(test_out, num); \
                            if (0 != strcmp(str, utstring_body(test_out))) { \
                                printf("Got %s, expected %s at line %d\n", \
                                        utstring_body(test_out), str, __LINE__); \
                                CHECK(0); \
                            } else OK(); \
                        }while(0)

#define IS(num, str)    IS_(num, str, num_to_str)
#define D_IS(dbl, str)  IS_(dbl, str, dbl_to_str)

#define APPROX_(num, exp, func) \
                        do{ long double got = func(num); \
                            long double err = fabsl(got - (exp)); \
                            if ( (exp) >= 1.0 ) err /= (exp); \
                            if (err >= 0.01) { \
                                printf("got %g, expected %g, err %g at line %d\n", \
                                       (double)got, (double)(exp), (double)err, __LINE__); CHECK(0); \
                            } else OK(); \
                        }while (0)

#define APROX(num, exp)     APPROX_(num, exp, num_to_double)
#define D_APROX(dbl, exp)   APPROX_(dbl, exp, dbl_to_double)

int main() {
    time_t t0 = time(NULL);
    utstring_new(test_out);

    // num_to_str
    {
        Num a;
        num_init_zero(&a);
        IS(&a, "0");

        a.negative = true;
        a.exp = 23;
        IS(&a, "0");

        a.negative = true;
        a.exp = -23;
        IS(&a, "0");

        a.negative = false;
        memcpy(a.frac, "\x01\x02\x03\x04\x05\x06", 6);
        a.n = 6;
        a.exp = 0;
        APROX(&a, 1.23456);

        a.negative = true;
        APROX(&a, -1.23456);

        a.exp = -1;
        APROX(&a, -0.123456);

        a.negative = false;
        a.exp = -2;
        APROX(&a, 0.0123456);

        a.exp = -3;
        APROX(&a, 0.00123456);

        a.negative = true;
        a.exp = 1;
        APROX(&a, -12.3456);

        a.negative = false;
        a.exp = 2;
        APROX(&a, 123.456);

        a.exp = 4;
        APROX(&a, 12345.6);

        a.exp = 5;
        APROX(&a, 123456);

        a.exp = 6;
        APROX(&a, 1234560);

        a.exp = 8;
        APROX(&a, 123456000);

        Dbl d;
        dbl_init_zero(&d);
        D_IS(&d, "0");

        memcpy(d.num.frac,
               "\x01\x02\x03\x04\x05\x06\x07\x08\x09\x00"
               "\x01\x02\x03\x04\x05\x06\x07\x08\x09\x00", 20);
        d.num.n = 20;
        d.num.exp = 19;
        D_IS(&d, "12345678901234567890");
    }

    // num <-> dbl
    {
        Num n;
        Dbl d;

        num_init_zero(&n);
        memcpy(n.frac,
               "\x01\x02\x03\x04\x05\x06\x07\x08\x09\x00"
               "\x01\x02\x03\x04\x05\x06", 16);
        n.n = 16;
        n.exp = 15;
        IS(&n, "1234567890123456");

        num_to_dbl(&d, &n);
        D_IS(&d, "1234567890123456");

        // test rounding
        d.num.n = DBL_FRAC_SIZE;
        d.num.exp = -1;
        memset(d.num.frac, 1, DBL_FRAC_SIZE * sizeof(digit_t));
        dbl_to_num(&n, &d);
        IS(&n, "0.1111111111111111");

        memset(d.num.frac, 4, DBL_FRAC_SIZE * sizeof(digit_t));
        dbl_to_num(&n, &d);
        IS(&n, "0.4444444444444444");

        memset(d.num.frac, 5, DBL_FRAC_SIZE * sizeof(digit_t));
        dbl_to_num(&n, &d);
        IS(&n, "0.5555555555555556");

        memset(d.num.frac, 8, DBL_FRAC_SIZE * sizeof(digit_t));
        dbl_to_num(&n, &d);
        IS(&n, "0.8888888888888889");

        memset(d.num.frac, 9, DBL_FRAC_SIZE * sizeof(digit_t));
        dbl_to_num(&n, &d);
        IS(&n, "1");
    }

    // normalize
    {
        Num n;
        num_init_zero(&n);
        n.n = NUM_FRAC_SIZE;
        n.exp = 20;
        memset(n.frac, 0, NUM_FRAC_SIZE);
        num_normalize(&n);
        IS(&n, "0");

        n.n = NUM_FRAC_SIZE;
        n.exp = 0;
        memset(n.frac, 0, NUM_FRAC_SIZE);
        memset(n.frac + 4, 5, NUM_FRAC_SIZE - 8);
        num_normalize(&n);
        IS(&n, "0.00055555555");

        Dbl d;
        dbl_init_zero(&d);
        d.num.n = DBL_FRAC_SIZE;
        d.num.exp = 20;
        memset(d.num.frac, 0, DBL_FRAC_SIZE);
        dbl_normalize(&d);
        D_IS(&d, "0");

        d.num.n = DBL_FRAC_SIZE;
        d.num.exp = 0;
        memset(d.num.frac, 0, DBL_FRAC_SIZE);
        memset(d.num.frac + 4, 5, DBL_FRAC_SIZE - 8);
        dbl_normalize(&d);
        D_IS(&d, "0.000555555555555555555555555");
    }

    // init_int, check zero
    {
        Num n;
        num_init_int(&n, 0);
        IS(&n, "0");
        CHECK(num_is_zero(&n));

        num_init_int(&n, 1234);
        IS(&n, "1234");
        CHECK(!num_is_zero(&n));

        num_init_int(&n, 9223372036854775807LL);     // 2^63-1
        IS(&n, "9223372036854776000");
        CHECK(!num_is_zero(&n));

        num_init_int(&n, -9223372036854775807LL);    // -2^63-1
        IS(&n, "-9223372036854776000");
        CHECK(!num_is_zero(&n));

        Dbl d;
        dbl_init_int(&d, 0);
        D_IS(&d, "0");

        dbl_init_int(&d, 1234);
        D_IS(&d, "1234");

        dbl_init_int(&d, 9223372036854775807LL);     // 2^63-1
        D_IS(&d, "9223372036854775807");

        dbl_init_int(&d, -9223372036854775807LL);    // 2^63-1
        D_IS(&d, "-9223372036854775807");
    }

    // init_double
    {
        Num n;
        num_init_double(&n, 0);
        IS(&n, "0");

        num_init_double(&n, 123.456);
        APROX(&n, 123.456);

        num_init_double(&n, -123.456);
        APROX(&n, -123.456);

        num_init_double(&n, 9.223372036854775807L);     // 2^63-1
        IS(&n, "9.223372036854776");

        num_init_double(&n, -9.223372036854775807L);    // -2^63-1
        IS(&n, "-9.223372036854776");

        Dbl d;
        dbl_init_double(&d, 0);
        D_IS(&d, "0");

        dbl_init_double(&d, 123.456);
        D_APROX(&d, 123.456);

        dbl_init_double(&d, -123.456);
        D_APROX(&d, -123.456);

        dbl_init_double(&d, 9.223372036854775807L);     // 2^63-1
        D_APROX(&d, 9.2233720368547764678623934742063L);

        dbl_init_double(&d, -9.223372036854775807L);    // 2^63-1
        D_APROX(&d, -9.2233720368547764678623934742063L);
    }

    // compare
    {
        Num a, b;
        num_init_int(&a, 10);
        num_init_int(&b, 1);
        CHECK(num_compare(&a, &b) == 1);

        num_init_int(&a, 10);
        num_init_int(&b, 10);
        CHECK(num_compare(&a, &b) == 0);

        num_init_int(&a, 101);
        num_init_int(&b, 102);
        CHECK(num_compare(&a, &b) == -1);

        num_init_int(&a, 103);
        num_init_int(&b, 102);
        CHECK(num_compare(&a, &b) == 1);

        num_init_int(&a, 10);
        num_init_int(&b, 100);
        CHECK(num_compare(&a, &b) == -1);

        for (int i = -1; i <= 1; i++) {
            for (int j = -1; j <= 1; j++) {
                num_init_int(&a, i);
                num_init_int(&b, j);
                if (i < j) CHECK(num_compare(&a, &b) == -1);
                else if (i == j) CHECK(num_compare(&a, &b) == 0);
                else CHECK(num_compare(&a, &b) == 1);
            }
        }
    }

    // add positive
    {
        Num a, b, res;
        num_init_double(&a, .123456);
        APROX(&a, 0.123456);
        num_init_double(&b, 123456);
        APROX(&b, 123456);
        add_positive(&res, &a, &b);
        APROX(&res, 123456.123456);

        // fill max 9999 number
        num_init_zero(&a);
        a.n = NUM_FRAC_SIZE;
        memset(a.frac, 9, NUM_FRAC_SIZE * sizeof(digit_t));
        APROX(&a, 9.999999999999999L);
        b = a;
        APROX(&b, 9.999999999999999L);
        add_positive(&res, &a, &b);
        APROX(&res, 19.999999999999998L);
    }

    // subtract positive
    {
        Num a, b, res;

        num_init_double(&a, 199999998.0L);
        APROX(&a, 199999998.0L);
        num_init_double(&b, 99999999.0L);
        APROX(&b, 99999999.0L);
        sub_positive(&res, &a, &b);
        APROX(&a, 199999998.0L);
        APROX(&b, 99999999.0L);
        APROX(&res, 99999999.0L);

        num_init_double(&a, 99999999.0L);
        APROX(&a, 99999999.0L);
        num_init_double(&b, 199999998.0L);
        APROX(&b, 199999998.0L);
        sub_positive(&res, &a, &b);
        APROX(&res, -99999999.0L);
    }

    // multiplication
    {
        Num a, b, res;
        num_init_double(&a, 123.456);
        APROX(&a, 123.456);
        num_init_double(&b, 999.9);
        APROX(&b, 999.9);
        num_mult(&res, &a, &b);
        APROX(&res, 123443.6544);
    }

    // division
    {
        Num a, b, res;

        num_init_double(&a, 123443.6544);
        APROX(&a,  123443.6544);
        num_init_double(&b, 999.9);
        APROX(&b,  999.9);
        num_div(&res, &a, &b);
        APROX(&res,  123.456);

        num_init_double(&a, 10);
        APROX(&a,  10);
        num_init_double(&b, 3);
        APROX(&b,  3);
        num_div(&res, &a, &b);
        APROX(&res, 10.0 / 3.0);

        num_init_double(&a, 10);
        APROX(&a,  10);
        num_init_double(&b, 1);
        APROX(&b,  1);
        num_div(&res, &a, &b);
        APROX(&res,  10);

        num_init_double(&a, 4);
        APROX(&a,  4);
        num_init_double(&b, 1);
        APROX(&b,  1);
        num_div(&res, &a, &b);
        APROX(&res,  4);

        num_init_double(&a, 4);
        APROX(&a,  4);
        num_init_double(&b, 997);
        APROX(&b,  997);
        num_div(&res, &a, &b);
        APROX(&res, 4.0 / 997.0);

        num_init_double(&a, 0);
        IS(&a, "0");
        num_init_double(&b, 1);
        APROX(&b,  1);
        num_div(&res, &a, &b);
        IS(&res, "0");

        num_init_double(&a, 1);
        APROX(&a,  1);
        num_init_double(&b, 1);
        APROX(&b,  1);
        num_div(&res, &a, &b);
        APROX(&res,  1);

        num_init_double(&a, 1);
        APROX(&a,  1);
        num_init_double(&b, 1);
        APROX(&b,  1);
        num_div(&res, &a, &b);
        APROX(&res,  1);
    }

    // add, subtract
    {
        Num a, b, res;
        double aa, bb, rr;
        for (int i = -100; i <= 100; i++) {
            Num ii;
            num_init_int(&ii, i);
            ii.exp--;

            for (int k = -2; k <= 2; k++) {
                Num ik = ii;
                ik.exp += k;

                for (int j = -100; j <= 100; j++) {
                    Num jj;
                    num_init_int(&jj, j);
                    jj.exp--;

                    a = ik;
                    aa = (double)i / 10.0 * pow(10.0, (double)k);
                    APROX(&a, aa);

                    b = jj;
                    bb= (double)j / 10.0;
                    APROX(&b, bb);

                    num_add(&res, &a, &b);
                    rr = aa + bb;
                    APROX(&res, rr);

                    num_sub(&res, &a, &b);
                    rr = aa - bb;
                    APROX(&res, rr);

                    num_mult(&res, &a, &b);
                    rr = aa * bb;
                    APROX(&res, rr);

                    if (fabsl(bb) >= 0.001) {
                        num_div(&res, &a, &b);
                        rr = aa / bb;
                        APROX(&res, rr);
                    }
                }
            }
        }
    }

    utstring_free(test_out);

    if (test_failed != 0) {
        printf("%d tests of %d FAILED in %ld seconds\n",
               test_failed, test_num, (long)(time(NULL) - t0));
        return EXIT_FAILURE;
    }
    else {
        printf("%d tests PASSED in %ld seconds\n",
               test_num, (long)(time(NULL) - t0));
        return EXIT_SUCCESS;
    }
}
#endif
