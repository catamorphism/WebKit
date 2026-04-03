/*
 * Copyright (C) 2021 Sony Interactive Entertainment Inc.
 * Copyright (C) 2022-2023 Apple Inc. All rights reserved.
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

#include "ISO8601.h"

namespace JSC {

class NudgeResult final {
public:
    ISO8601::InternalDuration m_duration;
    Int128 m_nudgedEpochNs;
    bool m_didExpandCalendarUnit;
    NudgeResult() { }
    NudgeResult(ISO8601::InternalDuration&& d, Int128 ns, bool expanded)
        : m_duration(WTF::move(d)), m_nudgedEpochNs(ns), m_didExpandCalendarUnit(expanded) { }
};

class Nudged final {
public:
    NudgeResult m_nudgeResult;
    double m_total;
    Nudged() { }
    Nudged(NudgeResult&& n, double t)
        : m_nudgeResult(WTF::move(n)), m_total(t) { }
};

class NudgeWindow final {
public:
    double m_r1;
    double m_r2;
    Int128 m_startEpochNs;
    Int128 m_endEpochNs;
    ISO8601::InternalDuration m_startDuration;
    ISO8601::InternalDuration m_endDuration;
    NudgeWindow() { };
    NudgeWindow(double r1, double r2, Int128 startEpochNs, Int128 endEpochNs, ISO8601::InternalDuration&& startDuration, ISO8601::InternalDuration&& endDuration)
        : m_r1(r1), m_r2(r2), m_startEpochNs(startEpochNs), m_endEpochNs(endEpochNs), m_startDuration(WTF::move(startDuration)), m_endDuration(WTF::move(endDuration)) { }
};

class TemporalDuration final : public JSNonFinalObject {
public:
    using Base = JSNonFinalObject;

    template<typename CellType, SubspaceAccess mode>
    static GCClient::IsoSubspace* subspaceFor(VM& vm)
    {
        return vm.temporalDurationSpace<mode>();
    }

    static TemporalDuration* create(VM&, Structure*, ISO8601::Duration&&);
    static TemporalDuration* tryCreateIfValid(JSGlobalObject*, ISO8601::Duration&&, Structure* = nullptr);
    static Structure* createStructure(VM&, JSGlobalObject*, JSValue);

    DECLARE_INFO;

    static TemporalDuration* toTemporalDuration(JSGlobalObject*, JSValue);
    static TemporalDuration* from(JSGlobalObject*, JSValue);
    static JSValue compare(JSGlobalObject*, JSValue, JSValue, JSValue);

#define JSC_DEFINE_TEMPORAL_DURATION_FIELD(name, capitalizedName) \
    double name##s() const { return m_duration.name##s(); } \
    void set##capitalizedName##s(double value) { m_duration.set##capitalizedName##s(value); }
    JSC_TEMPORAL_UNITS(JSC_DEFINE_TEMPORAL_DURATION_FIELD);
#undef JSC_DEFINE_TEMPORAL_DURATION_FIELD

    int sign() const { return m_duration.sign(); }
    const ISO8601::Duration& iso8601Duration() const { return m_duration; }

    ISO8601::Duration with(JSGlobalObject*, JSObject* durationLike) const;
    ISO8601::Duration negated() const;
    ISO8601::Duration abs() const;
    ISO8601::Duration add(JSGlobalObject*, JSValue) const;
    ISO8601::Duration subtract(JSGlobalObject*, JSValue) const;
    ISO8601::Duration round(JSGlobalObject*, JSValue options) const;
    double total(JSGlobalObject*, JSValue options) const;
    String toString(JSGlobalObject*, JSValue options) const;
    String toString(JSGlobalObject* globalObject, std::tuple<Precision, unsigned> precision = { Precision::Auto, 0 }) const { return toString(globalObject, m_duration, precision); }

    static ISO8601::InternalDuration toInternalDuration(ISO8601::Duration);
    static ISO8601::InternalDuration toInternalDurationRecordWith24HourDays(JSGlobalObject*, ISO8601::Duration);
    ISO8601::Duration addDurations(JSGlobalObject*, AddOrSubtract, ISO8601::Duration, TemporalUnit) const;
    static ISO8601::Duration temporalDurationFromInternal(JSGlobalObject*, ISO8601::InternalDuration, TemporalUnit);
    static Int128 timeDurationFromComponents(double, double, double, double, double, double);
    static double totalRelativeDuration(JSGlobalObject*, const ISO8601::InternalDuration&,
        Int128, Int128, const ISO8601::PlainDateTime&, std::optional<ISO8601::TimeZone>,
        TemporalUnit);

    static ISO8601::Duration fromDurationLike(JSGlobalObject*, JSObject*);
    static ISO8601::Duration toISO8601Duration(JSGlobalObject*, JSValue);

    static ISO8601::InternalDuration round(JSGlobalObject*, ISO8601::InternalDuration, double increment, TemporalUnit, RoundingMode);
    static std::optional<ISO8601::PlainDate> regulateISODate(double, double, double, TemporalOverflow);
    static std::tuple<ISO8601::PlainDate, ISO8601::PlainTime> combineISODateAndTimeRecord(ISO8601::PlainDate, ISO8601::PlainTime);
    static ISO8601::InternalDuration roundRelativeDuration(JSGlobalObject*, ISO8601::InternalDuration&, Int128, Int128, ISO8601::PlainDateTime, std::optional<ISO8601::TimeZone>, TemporalUnit, unsigned, TemporalUnit, RoundingMode);
    static double totalTimeDuration(Int128, TemporalUnit);
    static ISO8601::Duration toDateDurationRecordWithoutTime(JSGlobalObject*, const ISO8601::Duration&);
    static ISO8601::Duration adjustDateDurationRecord(JSGlobalObject*, const ISO8601::Duration&, int64_t, std::optional<int32_t>, std::optional<int32_t>);
    static std::optional<double> balance(ISO8601::Duration&, TemporalUnit largestUnit);
    static ISO8601::Duration toDateDurationWithoutTime(ISO8601::Duration);
    static NudgeWindow computeNudgeWindow(JSGlobalObject*, int32_t, const ISO8601::InternalDuration&, Int128, ISO8601::PlainDate, ISO8601::PlainTime, std::optional<ISO8601::TimeZone>, unsigned, TemporalUnit, TemporalNudgeWindowShift);
    static Nudged nudgeToCalendarUnit(JSGlobalObject*, int32_t, const ISO8601::InternalDuration&, Int128, Int128, ISO8601::PlainDate, ISO8601::PlainTime, std::optional<ISO8601::TimeZone>, unsigned, TemporalUnit, RoundingMode);
    static ISO8601::InternalDuration bubbleRelativeDuration(JSGlobalObject*, int32_t, ISO8601::InternalDuration, Int128, ISO8601::PlainDate, ISO8601::PlainTime, std::optional<ISO8601::TimeZone>, TemporalUnit, TemporalUnit);
    static Int128 timeDurationFromEpochNanosecondsDifference(ISO8601::ExactTime, ISO8601::ExactTime);
    static int32_t timeDurationSign(Int128);
    static Int128 add24HourDaysToTimeDuration(JSGlobalObject*, Int128, double);
    static TemporalUnit largestSubduration(const ISO8601::Duration&);

private:
    TemporalDuration(VM&, Structure*, ISO8601::Duration&&);
    DECLARE_DEFAULT_FINISH_CREATION;

    template<typename CharacterType>
    static std::optional<ISO8601::Duration> parse(StringParsingBuffer<CharacterType>&);

    static String toString(JSGlobalObject*, const ISO8601::Duration&, std::tuple<Precision, unsigned>);

    static NudgeResult nudgeToZonedTime(JSGlobalObject*, int32_t, ISO8601::InternalDuration, ISO8601::PlainDate, ISO8601::PlainTime, ISO8601::TimeZone, unsigned, TemporalUnit, RoundingMode);
    ISO8601::Duration m_duration;
};

Int128 getUTCEpochNanoseconds(std::tuple<ISO8601::PlainDate, ISO8601::PlainTime>);

} // namespace JSC
