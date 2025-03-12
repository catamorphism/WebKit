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

#pragma once

#include <wtf/Int128.h>

namespace JSC {

static constexpr Int128 absInt128(const Int128& value)
{
    if (value < 0)
        return -value;
    return value;
}

// Double-double precision floating point number, represented as the unevaluated
// sum of two doubles. In other words, dd[0] is the double approximation term
// and dd[1] is the error term.
//
// There are many such representations, but only one is 'normalized' meaning the
// dd[0] term is the most accurate possible double-precision approximation of
// the double-double value.
using DD = std::array<double, 2>;

double fractionToDouble(const Int128& numerator, const Int128& denominator);

unsigned ddCmp(const DD&, const DD&);

DD ddDiv(const Int128& numerator, const Int128& denominator);

bool ddIsZero(const DD&);

bool ddEq(const DD&, const DD&);

DD ddFadd(double, const DD&);

DD ddFmul(double, const DD&);

DD int128Fdiv(const Int128& n, const Int128& d);
} // namespace JSC
