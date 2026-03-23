/*
 * Copyright (C) 2025 Igalia, S.L. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. AND ITS CONTRIBUTORS ``AS IS''
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL APPLE INC. OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "config.h"
#include "FractionToDouble.h"

#include "MathCommon.h"

// The calculations here are based on algorithms from two sources. The second
// one builds on the first.
//
// Shewchuk (1997). Adaptive precision floating-point arithmetic and fast robust
//   geometric predicates. Discrete & Computational Geometry 18(3), pp. 305–363.
//   https://doi.org/10.1007/PL00009321
//
// Hida, Li, Bailey (2008). Library for double-double and quad-double
//   arithmetic. Manuscript. https://www.davidhbailey.com/dhbpapers/qd.pdf
//   and the accompanying QD library https://github.com/BL-highprecision/QD,
//   which is BSD-licensed.

namespace JSC {

// Double-double precision floating point number, represented as the unevaluated
// sum of two doubles. In other words, dd[0] is the double approximation term
// and dd[1] is the error term.
//
// There are many such representations, but only one is 'normalized' meaning the
// dd[0] term is the most accurate possible double-precision approximation of
// the double-double value.
using DD = std::array<double, 2>;

// Conversion of Int128 to double-double precision floating point. The
// calculations follow from the definition of hi and lo: hi is the closest
// double-precision approximation to the exact value (which itself will be a
// safe integer) and lo is the error term.
static DD int128ToDD(const Int128& value)
{
    double hi = static_cast<double>(value);
    double lo = static_cast<double>(value - static_cast<Int128>(hi));
    return { hi, lo };
}

static DD ddNegate(DD a)
{
    return { -a[0], -a[1] };
}

/*
// Computes double-double precision a + b of two doubles a and b. This is the
// Two-Sum algorithm in theorem 7 of the Shewchuk paper.
static DD ddSum(double a, double b)
{
    // First compute the double-precision approximation of the sum by regular
    // double addition.
    double sum = a + b;

    // Compute the error term.
    double bVirtual = sum - a;
    double aVirtual = sum - bVirtual;
    double bRoundoff = b - bVirtual;
    double aRoundoff = a - aVirtual;
    double error = aRoundoff + bRoundoff;

    return { sum, error };
}
*/

static double twoSum(double a, double b, double &err)
{
    double s = a + b;
    double bb = s - a;
    err = (a - (s - bb)) + (b - bb);
    return s;
}

static DD twoDDSum(DD a, DD b)
{
    double s0, s1;
    double t0, t1;
    
    s0 = twoSum(a[0], b[0], t0);
    s1 = twoSum(a[1], b[1], t1);

    s1 = twoSum(s1, t0, t0);
    return { s0, s1 };
}

static void renorm(double& c0, double& c1, double& c2)
{
    double s0, s1, s2 = 0.0;

    if (!std::isfinite(c0))
        return;

    s0 = c2;
    s0 = twoSum(c1, s0, c2);
    c0 = twoSum(c0, s0, c1);

    s0 = c0;
    s1 = c1;
    if (s1 != 0.0)
        s1 = twoSum(s1, c2, s2);
    else
        s0 = twoSum(s0, c2, s1);

    c0 = s0;
    c1 = s1;
    c2 = s2;
}

static double twoProd(double a, double b, double& err)
{
    double p = a * b;
    err = std::fma(a, b, p);
    return p;
}

static DD ddProductSlow(double a, DD b)
{
    double p0, p1;
    double q0, q1;
    double s0, s1, s2;

    p0 = twoProd(b[0], a, q0);
    p1 = twoProd(b[1], a, q1);
    s0 = p0;
    s1 = q0 + p1; // ignore error term
    renorm(s0, s1, s2);
    return { s0, s1 };
}

// Computes double-double precision a * b of two doubles a and b. The
// optimization using std::fma() is suggested in section 2 of the Hida-Li-Bailey
// paper.
static DD ddProduct(double a, DD b)
{
    if (!b[1]) {
        // First compute the double-precision approximation of the product by
        // regular double multiplication.
        double product = a * b[0]; 

        // On armv8, this emits the fnmsub instruction.
        // On x86_64, this emits the vfmsub213sd instruction if compiled with SSE
        // instructions. If not, it calls libm's fma(), which is comparably fast to
        // using the Two-Product algorithm in theorem 18 of the Shewchuk paper.
        double error = std::fma(a, b[0], -product);

        return { product, error };
    }
    return ddProductSlow(a, b);
}

// Computes double-double precision numerator / denominator, where divisor is a
// double, and rounds the result to double precision. This is described in
// section 3.5 of the Hida-Li-Bailey paper.
static double fractionToDoubleSlow(const Int128& numerator, const Int128& denominator)
{
    DD ddNumerator = int128ToDD(numerator);
    DD ddDenominator = int128ToDD(denominator);

    double quotient0 = ddNumerator[0] / ddDenominator[0];
    DD product1 = ddProduct(quotient0, ddDenominator);
    DD remainder = twoDDSum(ddNumerator, ddNegate(product1));
                  
    double quotient1 = remainder[0] / ddDenominator[0];
    DD product2 = ddProduct(quotient1, ddDenominator);
    remainder = twoDDSum(remainder, ddNegate(product2));

    double quotient2 = remainder[0] / ddDenominator[0];
    DD product3 = ddProduct(quotient2, ddDenominator);
    remainder = twoDDSum(remainder, ddNegate(product3));

    renorm(quotient0, quotient1, quotient2);
    double result = quotient0 + quotient1 + quotient2;
    return result;
/*
    // Compute a first approximation of the quotient by regular double division.
    double quotient0 = ddNumerator[0] / ddDenominator[0];

    // Compute remainder, ddNumerator - quotient0 * denominator.
    DD product1 = ddProduct(quotient0, ddDenominator);
    DD remainder = twoDDSum(ddNumerator, ddNegate(product1));

    // Compute the next approximation term.
    double error1 = remainder[1] + ddNumerator[1];
    double quotient1 = (remainder[0] + error1) / ddDenominator[0];

    // Compute remainder
    DD product2 = ddProduct(quotient1, ddDenominator);
    remainder = twoDDSum(remainder, ddNegate(product2));

    // Compute the next approximation term.
    double error2 = remainder[1] + ddNumerator[1];
    double quotient2 = (remainder[0] + error2) / ddDenominator[0];

    renorm(quotient0, quotient1, quotient2);

    double result = quotient0 + quotient1 + quotient2;

    // The result is DD { quotient0, quotient1 }. If we wanted double-double
    // precision here, we would have to use the Fast-Two-Sum algorithm from
    // theorem 6 of the Shewchuk paper to renormalize the two terms, but since
    // we only need double precision we can discard the error term.
    return result;
*/
}

double fractionToDouble(const Int128& numerator, const Int128& denominator)
{
    ASSERT(denominator > 0);

    if (!numerator)
        return 0;

    // When the denominator is 1, we are just calculating the double
    // approximation of the numerator.
    if (denominator == 1)
        return static_cast<double>(numerator);

    // When the numerator can be represented exactly as a double the algorithm
    // collapses to a simple double division.
    if (isSafeInteger(static_cast<double>(numerator)) && isSafeInteger(static_cast<double>(denominator))) [[likely]]
        return static_cast<double>(numerator) / static_cast<double>(denominator);

    // Otherwise use double-double precision to compute the result.
    return fractionToDoubleSlow(numerator, denominator);
}

} // namespace JSC
