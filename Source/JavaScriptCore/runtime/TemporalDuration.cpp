/*
 * Copyright (C) 2021 Sony Interactive Entertainment Inc.
 * Copyright (C) 2021-2023 Apple Inc. All rights reserved.
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
#include "TemporalDuration.h"

#include "IntlObjectInlines.h"
#include "JSCInlines.h"
#include "TemporalObject.h"
#include <wtf/text/MakeString.h>
#include <wtf/text/StringBuilder.h>

namespace JSC {

static constexpr double nanosecondsPerDay = 24.0 * 60 * 60 * 1000 * 1000 * 1000;

const ClassInfo TemporalDuration::s_info = { "Object"_s, &Base::s_info, nullptr, nullptr, CREATE_METHOD_TABLE(TemporalDuration) };

TemporalDuration* TemporalDuration::create(VM& vm, Structure* structure, ISO8601::Duration&& duration)
{
    auto* object = new (NotNull, allocateCell<TemporalDuration>(vm)) TemporalDuration(vm, structure, WTFMove(duration));
    object->finishCreation(vm);
    return object;
}

Structure* TemporalDuration::createStructure(VM& vm, JSGlobalObject* globalObject, JSValue prototype)
{
    return Structure::create(vm, globalObject, prototype, TypeInfo(ObjectType, StructureFlags), info());
}

TemporalDuration::TemporalDuration(VM& vm, Structure* structure, ISO8601::Duration&& duration)
    : Base(vm, structure)
    , m_duration(WTFMove(duration))
{
}

// CreateTemporalDuration ( years, months, weeks, days, hours, minutes, seconds, milliseconds, microseconds, nanoseconds [ , newTarget ] )
// https://tc39.es/proposal-temporal/#sec-temporal-createtemporalduration
TemporalDuration* TemporalDuration::tryCreateIfValid(JSGlobalObject* globalObject, ISO8601::Duration&& duration, Structure* structure)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (!ISO8601::isValidDuration(duration)) {
        throwRangeError(globalObject, scope, "Temporal.Duration properties must be finite and of consistent sign"_s);
        return { };
    }

    return TemporalDuration::create(vm, structure ? structure : globalObject->durationStructure(), WTFMove(duration));
}

// ToTemporalDurationRecord ( temporalDurationLike )
// https://tc39.es/proposal-temporal/#sec-temporal-totemporaldurationrecord
ISO8601::Duration TemporalDuration::fromDurationLike(JSGlobalObject* globalObject, JSObject* durationLike)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (durationLike->inherits<TemporalDuration>())
        return jsCast<TemporalDuration*>(durationLike)->m_duration;

    ISO8601::Duration result;
    auto hasRelevantProperty = false;
    for (TemporalUnit unit : temporalUnitsInTableOrder) {
        JSValue value = durationLike->get(globalObject, temporalUnitPluralPropertyName(vm, unit));
        RETURN_IF_EXCEPTION(scope, { });

        if (value.isUndefined())
            continue;

        hasRelevantProperty = true;
        result[unit] = value.toNumber(globalObject) + 0.0;
        RETURN_IF_EXCEPTION(scope, { });

        if (!isInteger(result[unit])) {
            throwRangeError(globalObject, scope, "Temporal.Duration properties must be integers"_s);
            return { };
        }
    }

    if (!hasRelevantProperty) {
        throwTypeError(globalObject, scope, "Object must contain at least one Temporal.Duration property"_s);
        return { };
    }

    return result;
}

/*
DateDuration TemporalDuration::toDateDuration(ISO8601::Duration d) {
    return DateDuration(d.years(), d.months(), d.weeks(), d.days());
}

ISO8601::Duration TemporalDuration::fromDateDuration(const DateDuration& d) {
    return ISO8601::Duration { d.m_years, d.m_months, d.m_weeks, d.m_days, 0, 0, 0, 0, 0, 0 };
}
*/

// ToLimitedTemporalDuration ( temporalDurationLike, disallowedFields )
// https://tc39.es/proposal-temporal/#sec-temporal-tolimitedtemporalduration
ISO8601::Duration TemporalDuration::toISO8601Duration(JSGlobalObject* globalObject, JSValue itemValue)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    ISO8601::Duration duration;
    if (itemValue.isObject()) {
        duration = fromDurationLike(globalObject, asObject(itemValue));
        RETURN_IF_EXCEPTION(scope, { });
    } else {
        if (!itemValue.isString()) {
            throwTypeError(globalObject, scope, "can only convert to Duration from object or string values"_s);
            return { };
        }

        String string = itemValue.toWTFString(globalObject);
        RETURN_IF_EXCEPTION(scope, { });

        auto parsedDuration = ISO8601::parseDuration(string);
        if (!parsedDuration) {
            // 3090: 308 digits * 10 fields + 10 designators
            throwRangeError(globalObject, scope, makeString("'"_s, ellipsizeAt(3090, string), "' is not a valid Duration string"_s));
            return { };
        }

        duration = parsedDuration.value();
    }

    if (!ISO8601::isValidDuration(duration)) {
        throwRangeError(globalObject, scope, "Temporal.Duration properties must be finite and of consistent sign"_s);
        return { };
    }

    return duration;
}

// ToTemporalDuration ( item )
// https://tc39.es/proposal-temporal/#sec-temporal-totemporalduration
TemporalDuration* TemporalDuration::toTemporalDuration(JSGlobalObject* globalObject, JSValue itemValue)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (itemValue.inherits<TemporalDuration>())
        return jsCast<TemporalDuration*>(itemValue);

    auto result = toISO8601Duration(globalObject, itemValue);
    RETURN_IF_EXCEPTION(scope, nullptr);

    return TemporalDuration::create(vm, globalObject->durationStructure(), WTFMove(result));
}

// ToLimitedTemporalDuration ( temporalDurationLike, disallowedFields )
// https://tc39.es/proposal-temporal/#sec-temporal-tolimitedtemporalduration
ISO8601::Duration TemporalDuration::toLimitedDuration(JSGlobalObject* globalObject, JSValue itemValue, std::initializer_list<TemporalUnit> disallowedUnits)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    ISO8601::Duration duration = toISO8601Duration(globalObject, itemValue);
    RETURN_IF_EXCEPTION(scope, { });

    if (!isValidDuration(duration)) {
        throwRangeError(globalObject, scope, "Temporal.Duration properties must be finite and of consistent sign"_s);
        return { };
    }

    for (TemporalUnit unit : disallowedUnits) {
        if (duration[unit]) {
            throwRangeError(globalObject, scope, makeString("Adding "_s, temporalUnitPluralPropertyName(vm, unit).publicName(), " not supported by Temporal.Instant. Try Temporal.ZonedDateTime instead"_s));
            return { };
        }
    }

    return duration;
}

TemporalDuration* TemporalDuration::from(JSGlobalObject* globalObject, JSValue itemValue)
{
    VM& vm = globalObject->vm();

    if (itemValue.inherits<TemporalDuration>()) {
        ISO8601::Duration cloned = jsCast<TemporalDuration*>(itemValue)->m_duration;
        return TemporalDuration::create(vm, globalObject->durationStructure(), WTFMove(cloned));
    }

    return toTemporalDuration(globalObject, itemValue);
}

static double totalSeconds(ISO8601::Duration& duration)
{
    auto hours = 24 * duration.days() + duration.hours();
    auto minutes = 60 * hours + duration.minutes();
    return 60 * minutes + duration.seconds();
}

static double totalSubseconds(ISO8601::Duration& duration)
{
    auto milliseconds = duration.milliseconds();
    auto microseconds = 1000 * milliseconds + duration.microseconds();
    return 1000 * microseconds + duration.nanoseconds();
}

static double totalNanoseconds(ISO8601::Duration& duration)
{
    auto hours = 24 * duration.days() + duration.hours();
    auto minutes = 60 * hours + duration.minutes();
    auto seconds = 60 * minutes + duration.seconds();
    auto milliseconds = 1000 * seconds + duration.milliseconds();
    auto microseconds = 1000 * milliseconds + duration.microseconds();
    return 1000 * microseconds + duration.nanoseconds();
}

JSValue TemporalDuration::compare(JSGlobalObject* globalObject, JSValue valueOne, JSValue valueTwo)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto* one = toTemporalDuration(globalObject, valueOne);
    RETURN_IF_EXCEPTION(scope, { });

    auto* two = toTemporalDuration(globalObject, valueTwo);
    RETURN_IF_EXCEPTION(scope, { });

    // FIXME: Implement relativeTo parameter after PlainDateTime / ZonedDateTime.
    if (one->years() || two->years() || one->months() || two->months() || one->weeks() || two->weeks()) {
        throwRangeError(globalObject, scope, "Cannot compare a duration of years, months, or weeks without a relativeTo option"_s);
        return { };
    }

    auto nsOne = totalNanoseconds(one->m_duration);
    auto nsTwo = totalNanoseconds(two->m_duration);
    return jsNumber(nsOne > nsTwo ? 1 : nsOne < nsTwo ? -1 : 0);
/*
    auto secOne = totalSeconds(one->m_duration);
    auto secTwo = totalSeconds(two->m_duration);
    auto nsOne = totalSubseconds(one->m_duration);
    auto nsTwo = totalSubseconds(two->m_duration);
    if (secOne > secTwo) {
        if (nsOne > nsTwo)
            return jsNumber(1);
        if (nsOne < nsTwo)
            return jsNumber(-1);
        return jsNumber(0);
    }
    if (secOne < secTwo)
        return jsNumber(-1);
    // secOne == secTwo
    if (nsOne > nsTwo)
        return jsNumber(1);
    if (nsOne < nsTwo)
        return jsNumber(-1);
    return jsNumber(0);
*/
}

int TemporalDuration::sign(const ISO8601::Duration& duration)
{
    for (auto value : duration) {
        if (value < 0)
            return -1;

        if (value > 0)
            return 1;
    }

    return 0;
}

ISO8601::Duration TemporalDuration::with(JSGlobalObject* globalObject, JSObject* durationLike) const
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    ISO8601::Duration result;
    auto hasRelevantProperty = false;
    for (TemporalUnit unit : temporalUnitsInTableOrder) {
        JSValue value = durationLike->get(globalObject, temporalUnitPluralPropertyName(vm, unit));
        RETURN_IF_EXCEPTION(scope, { });

        if (value.isUndefined()) {
            result[unit] = m_duration[unit];
            continue;
        }

        hasRelevantProperty = true;
        result[unit] = value.toNumber(globalObject);
        RETURN_IF_EXCEPTION(scope, { });

        if (!isInteger(result[unit])) {
            throwRangeError(globalObject, scope, "Temporal.Duration properties must be integers"_s);
            return { };
        }
    }

    if (!hasRelevantProperty) {
        throwTypeError(globalObject, scope, "Object must contain at least one Temporal.Duration property"_s);
        return { };
    }

    return result;
}

ISO8601::Duration TemporalDuration::negated() const
{
    return -m_duration;
}

ISO8601::Duration TemporalDuration::abs() const
{
    ISO8601::Duration result;
    for (size_t i = 0; i < numberOfTemporalUnits; i++)
        result[i] = std::abs(m_duration[i]);
    return result;
}

// DefaultTemporalLargestUnit ( years, months, weeks, days, hours, minutes, seconds, milliseconds, microseconds )
// https://tc39.es/proposal-temporal/#sec-temporal-defaulttemporallargestunit
static TemporalUnit largestSubduration(const ISO8601::Duration& duration)
{
    uint8_t index = 0;
    while (index < numberOfTemporalUnits - 1 && !duration[index])
        index++;
    return static_cast<TemporalUnit>(index);
}

// BalanceDuration ( days, hours, minutes, seconds, milliseconds, microseconds, nanoseconds, largestUnit [ , relativeTo ] )
// https://tc39.es/proposal-temporal/#sec-temporal-balanceduration
std::optional<double> TemporalDuration::balance(ISO8601::Duration& duration, TemporalUnit largestUnit)
{

    auto nanoseconds = totalSubseconds(duration);
    auto seconds = totalSeconds(duration);

    if (!std::isfinite(nanoseconds))
        return nanoseconds;
    duration.clear();

//    double days, hours, minutes, seconds, milliseconds, microseconds = 0;

    if (largestUnit <= TemporalUnit::Day) {
        duration.setDays(std::trunc(seconds / secondsPerDay));
        seconds = std::fmod(seconds, secondsPerDay);
    }

    double microseconds = std::trunc(nanoseconds / 1000);
    double milliseconds = std::trunc(microseconds / 1000);
    double minutes = std::trunc(seconds / 60);
    if (largestUnit <= TemporalUnit::Hour) {
        duration.setNanoseconds(std::fmod(nanoseconds, 1000));
        duration.setMicroseconds(std::fmod(microseconds, 1000));
        duration.setMilliseconds(std::fmod(milliseconds, 1000));
        duration.setSeconds(std::fmod(seconds, 60));
        duration.setMinutes(std::fmod(minutes, 60));
        duration.setHours(std::trunc(minutes / 60));
    } else if (largestUnit == TemporalUnit::Minute) {
        duration.setNanoseconds(std::fmod(nanoseconds, 1000));
        duration.setMicroseconds(std::fmod(microseconds, 1000));
        duration.setMilliseconds(std::fmod(milliseconds, 1000));
        duration.setSeconds(std::fmod(seconds, 60));
        duration.setMinutes(minutes);
    } else if (largestUnit == TemporalUnit::Second) {
        duration.setNanoseconds(std::fmod(nanoseconds, 1000));
        duration.setMicroseconds(std::fmod(microseconds, 1000));
        duration.setMilliseconds(std::fmod(milliseconds, 1000));
        duration.setSeconds(seconds);
    } else if (largestUnit == TemporalUnit::Millisecond) {
        duration.setNanoseconds(std::fmod(nanoseconds, 1000));
        duration.setMicroseconds(std::fmod(microseconds, 1000));
        milliseconds += seconds * 1000;
        duration.setMilliseconds(milliseconds);
    } else if (largestUnit == TemporalUnit::Microsecond) {
        duration.setNanoseconds(std::fmod(nanoseconds, 1000));
        microseconds += seconds * 1000 * 1000;
        duration.setMicroseconds(microseconds);
    } else {
        nanoseconds += seconds * 1000 * 1000 * 1000;
        duration.setNanoseconds(nanoseconds);
    }

    return std::nullopt;
}

int32_t dateDurationSign(const ISO8601::Duration& d) {
    if (d.years() > 0) {
        return 1;
    }
    if (d.years() < 0) {
        return -1;
    }
    if (d.months() > 0) {
        return 1;
    }
    if (d.months() < 0) {
        return -1;
    }
    if (d.weeks() > 0) {
        return 1;
    }
    if (d.weeks() < 0) {
        return -1;
    }
    if (d.days() > 0) {
        return 1;
    }
    if (d.days() < 0) {
        return -1;
    }
    return 0;
}

// TODO: don't duplicate this
static constexpr Int128 absInt128(const Int128& value)
{
    if (value < 0)
        return -value;
    return value;
}

ISO8601::Duration TemporalDuration::add(JSGlobalObject* globalObject, JSValue otherValue) const
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto other = toISO8601Duration(globalObject, otherValue);
    RETURN_IF_EXCEPTION(scope, { });

    // FIXME: Implement relativeTo parameter after PlainDateTime / ZonedDateTime.
    auto largestUnit = std::min(largestSubduration(m_duration), largestSubduration(other));
    if (largestUnit <= TemporalUnit::Week) {
        throwRangeError(globalObject, scope, "Cannot add a duration of years, months, or weeks without a relativeTo option"_s);
        return { };
    }

    auto result = addDurations(true, other, largestUnit);
    if (!result) {
        // TODO: other range errors are possible
        throwRangeError(globalObject, scope, "Times sum to more than the maximum time duration"_s);
        return { };
    }
/*
    ISO8601::Duration result {
        0, 0, 0, days() + other.days(),
        hours() + other.hours(), minutes() + other.minutes(), seconds() + other.seconds(),
        milliseconds() + other.milliseconds(), microseconds() + other.microseconds(), nanoseconds() + other.nanoseconds()
    };

    balance(result, largestUnit);
    return result;
*/

    return result.value();
}

/*
std::optional<Int128> addTimeDuration(Int128 one, Int128 two) {
    Int128 result = one + two;
    if (int128abs(result) > maxTimeDuration) {
        // Step 2. If abs(result) > maxTimeDuration, throw a RangeError exception.
        return std::nullopt;
    }
    return result;
}
*/

std::optional<InternalDuration> TemporalDuration::combineDateAndTimeDuration(ISO8601::Duration dateDuration, Int128 timeDuration) {
    int32_t dateSign = dateDurationSign(dateDuration);
    int32_t timeSign = timeDuration < 0 ? -1 : timeDuration == 0 ? 0 : 1;
    if (dateSign != 0 && timeSign != 0 && dateSign != timeSign) {
        return std::nullopt; // RangeError: date and time have different signs
    }
    return InternalDuration { WTFMove(dateDuration), timeDuration };
}

/* static */ std::optional<ISO8601::Duration> TemporalDuration::addDurations(bool isAdd, ISO8601::Duration other, TemporalUnit largestUnit) const {
// 7.5.41 AddDurations
    if (!isAdd) {
        // Step 2: If operation is subtract, set other to CreateNegatedTemporalDuration(other). 
        other = -other;
        // Steps 3-6 already done
    }

    // 7. Let d1 be ToInternalDurationRecordWith24HourDays(duration).
    auto d1 = toInternalDurationRecordWith24HourDays(m_duration);
    // 8. Let d2 be ToInternalDurationRecordWith24HourDays(other).
    auto d2 = toInternalDurationRecordWith24HourDays(other);
    if (!d1 || !d2)
        return std::nullopt;
    // 9. Let timeResult be ? AddTimeDuration(d1.[[Time]], d2.[[Time]]).
    auto timeResult = d1->m_time + d2->m_time;
    // TODO: overflow check?
    // if (absInt128(timeResult) > maxTimeDuration)
    //    return std::nullopt;
    // 10. Let result be ! CombineDateAndTimeDuration(ZeroDateDuration(), timeResult).
    auto result = combineDateAndTimeDuration(ISO8601::Duration(), timeResult);
    if (!result)
        return std::nullopt;
    // 11. Return ? TemporalDurationFromInternal(result, largestUnit).
    return temporalDurationFromInternal(result.value(), largestUnit);
}

Int128 TemporalDuration::timeDurationFromComponents(double hours, double minutes, double seconds, double milliseconds, double microseconds, double nanoseconds) {
    Int128 min = minutes + hours * 60;
    Int128 sec = seconds + min * 60;
    Int128 millis = milliseconds + sec * 1000;
    Int128 micros = microseconds + millis * 1000;
    Int128 nanos = nanoseconds + micros * 1000;
    return nanos; // TODO: overflow check?
}

Int128 add24HourDaysToTimeDuration(Int128 d, double days) {
    Int128 result = d + days * nanosecondsPerDay;
    // TODO: overflow check?
    return result;
}

std::optional<InternalDuration> TemporalDuration::toInternalDurationRecordWith24HourDays(ISO8601::Duration d) {
    // 1. Let timeDuration be TimeDurationFromComponents(duration.[[Hours]], duration.[[Minutes]], duration.[[Seconds]], duration.[[Milliseconds]], duration.[[Microseconds]], duration.[[Nanoseconds]]).
    Int128 timeDuration = timeDurationFromComponents(d.hours(), d.minutes(), d.seconds(), d.milliseconds(), d.microseconds(), d.nanoseconds());
    // 2. Set timeDuration to ! Add24HourDaysToTimeDuration(timeDuration, duration.[[Days]]).
    timeDuration = add24HourDaysToTimeDuration(timeDuration, d.days());
    // 3. Let dateDuration be ! CreateDateDurationRecord(duration.[[Years]], duration.[[Months]], duration.[[Weeks]], 0).
    // (Skipped; assumed to be valid TODO)
    // 4. Return ! CombineDateAndTimeDuration(dateDuration, timeDuration).
    return combineDateAndTimeDuration(d, timeDuration);
}

std::optional<InternalDuration> TemporalDuration::toInternalDuration(ISO8601::Duration d) {
    auto timeDuration = timeDurationFromComponents(d.hours(), d.minutes(), d.seconds(), d.milliseconds(), d.microseconds(), d.nanoseconds());
    return combineDateAndTimeDuration(d, timeDuration);
}

/* static */ ISO8601::Duration TemporalDuration::temporalDurationFromInternal(InternalDuration internalDuration,
                                                                              TemporalUnit largestUnit) {
    // 1. Let days, hours, minutes, seconds, milliseconds, and microseconds be 0.
    double days = 0;
    double hours = 0;
    double minutes = 0;
    double seconds = 0;
    double milliseconds = 0;
    double microseconds = 0;

    // 2. Let sign be TimeDurationSign(internalDuration.[[Time]]).
    int32_t sign = internalDuration.timeDurationSign();

    // 3. Let nanoseconds be abs(internalDuration.[[Time]])
    Int128 nanoseconds = absInt128(internalDuration.time());

    // 4. If TemporalUnitCategory(largestUnit) is DATE, then
    if (largestUnit <= TemporalUnit::Day) {
        microseconds = std::trunc((double) nanoseconds / 1000);
        nanoseconds = nanoseconds % 1000;
        milliseconds = std::trunc(microseconds / 1000);
        microseconds = std::fmod(microseconds, 1000);
        seconds = std::trunc(milliseconds / 1000);
        milliseconds = std::fmod(milliseconds, 1000);
        minutes = std::trunc(seconds / 60);
        seconds = std::fmod(seconds, 60);
        hours = std::trunc(minutes / 60);
        minutes = std::fmod(minutes, 60);
        days = std::trunc(hours / 24);
        hours = std::fmod(hours, 24);
    } else if (largestUnit <= TemporalUnit::Hour) {
        microseconds = std::trunc((double) nanoseconds / 1000);
        nanoseconds = nanoseconds % 1000;
        milliseconds = std::trunc(microseconds / 1000);
        microseconds = std::fmod(microseconds, 1000);
        seconds = std::trunc(milliseconds / 1000);
        milliseconds = std::fmod(milliseconds, 1000);
        minutes = std::trunc(seconds / 60);
        seconds = std::fmod(seconds, 60);
        hours = std::trunc(minutes / 60);
        minutes = std::fmod(minutes, 60);
    } else if (largestUnit <= TemporalUnit::Minute) {
        microseconds = std::trunc((double) nanoseconds / 1000);
        nanoseconds = nanoseconds % 1000;
        milliseconds =- std::trunc(microseconds / 1000);
        microseconds = std::fmod(microseconds, 1000);
        seconds = std::trunc(milliseconds / 1000);
        milliseconds = std::fmod(milliseconds, 1000);
        minutes = std::trunc(seconds / 60);
        seconds = std::fmod(seconds, 60);
    } else if (largestUnit <= TemporalUnit::Second) {
        microseconds = std::trunc((double) nanoseconds / 1000);
        nanoseconds = nanoseconds % 1000;
        milliseconds =- std::trunc(microseconds / 1000);
        microseconds = std::fmod(microseconds, 1000);
        seconds = std::trunc(milliseconds / 1000);
        milliseconds = std::fmod(milliseconds, 1000);
    } else if (largestUnit <= TemporalUnit::Millisecond) {
        microseconds = std::trunc((double) nanoseconds / 1000);
        nanoseconds = nanoseconds % 1000;
        milliseconds = std::fma(seconds, 3, std::trunc(microseconds / 1000));
        microseconds = std::fmod(microseconds, 1000);
    } else if (largestUnit <= TemporalUnit::Microsecond) {
        microseconds = std::fma(seconds, 6, std::trunc((double) nanoseconds / 1000));
        nanoseconds = nanoseconds % 1000;
    } else { // Nanoseconds
        nanoseconds = std::fma(seconds, 9, nanoseconds);
    }

    return ISO8601::Duration { internalDuration.m_date_duration.years(), internalDuration.m_date_duration.months(), internalDuration.m_date_duration.weeks(), internalDuration.m_date_duration.days() + days * sign, hours * sign, minutes * sign, seconds * sign, milliseconds * sign, microseconds * sign, (double) nanoseconds * sign };
}


ISO8601::Duration TemporalDuration::subtract(JSGlobalObject* globalObject, JSValue otherValue) const
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto other = toISO8601Duration(globalObject, otherValue);
    RETURN_IF_EXCEPTION(scope, { });

    // FIXME: Implement relativeTo parameter after PlainDateTime / ZonedDateTime.
    auto largestUnit = std::min(largestSubduration(m_duration), largestSubduration(other));
    if (largestUnit <= TemporalUnit::Week) {
        throwRangeError(globalObject, scope, "Cannot subtract a duration of years, months, or weeks without a relativeTo option"_s);
        return { };
    }

    ISO8601::Duration result {
        0, 0, 0, days() - other.days(),
        hours() - other.hours(), minutes() - other.minutes(), seconds() - other.seconds(),
        milliseconds() - other.milliseconds(), microseconds() - other.microseconds(), nanoseconds() - other.nanoseconds()
    };

    balance(result, largestUnit);
    return result;
}

static Int128 totalTimeDuration(Int128 timeDuration, TemporalUnit unit) {
    Int128 divisor = 1;
    if (unit >= TemporalUnit::Microsecond) {
        divisor *= 1000;  
    }
    if (unit >= TemporalUnit::Millisecond) {
        divisor *= 1000;
    }
    if (unit >= TemporalUnit::Second) {
        divisor *= 1000;
    }
    if (unit >= TemporalUnit::Minute) {
        divisor *= 60;
    }
    if (unit >= TemporalUnit::Hour) {
        divisor *= 60;
    }
    if (unit >= TemporalUnit::Day) {
        divisor *= 24;
    }
    return timeDuration / divisor;
}

Int128 TemporalDuration::roundTimeDuration(Int128 timeDuration, double increment, TemporalUnit unit, RoundingMode mode) {
    // TODO: refactor
    Int128 divisor = 1;
    if (unit >= TemporalUnit::Microsecond) {
        divisor *= 1000;  
    }
    if (unit >= TemporalUnit::Millisecond) {
        divisor *= 1000;
    }
    if (unit >= TemporalUnit::Second) {
        divisor *= 1000;
    }
    if (unit >= TemporalUnit::Minute) {
        divisor *= 60;
    }
    if (unit >= TemporalUnit::Hour) {
        divisor *= 60;
    }
    if (unit >= TemporalUnit::Day) {
        divisor *= 24;
    }
    return roundNumberToIncrement(timeDuration, divisor * ((Int128) (std::trunc(increment))), mode);
}

// RoundRelativeDuration ( duration, destEpochNs, isoDateTime, timeZone, calendar, largestUnit, increment, smallestUnit, roundingMode )
// https://tc39.es/proposal-temporal/#sec-temporal-roundrelativeduration
// TODO: calendar and time zone
 static void roundRelativeDuration(InternalDuration& duration, double destEpochNs, ISO8601::PlainDate isoDate, TemporalUnit largestUnit, double increment, TemporalUnit smallestUnit, RoundingMode roundingMode) {
     // 1, 2
     bool irregularLengthUnit = smallestUnit <= TemporalUnit::Day;
     // 3 elided (no time zones)
     // 4
     int32_t sign = duration.sign() < 0 ? -1 : 1;
     if (irregularLengthUnit) {
         auto record = nudgeToCalendarUnit(sign, duration, destEpochNs, isoDate, increment, smallestUnit, roundingMode);
         nudgeResult = record.nudgeResult;
     } else {
         // 7
         nudgeResult = nudgeToDayOrTime(duration, destEpochNs, largestUnit, increment, smallestUnit, roundingMode);
     }
     // 8.
     duration = nudgeResult.duration;
     // 9.
     if (nudgeResult.didExpandCalendarUnit && smallestUnit != TemporalUnit::Week) {
         auto startUnit = smallestUnit <= TemporalUnit::Day ? smallestUnit : TemporalUnit::Day;
         duration = bubbleRelativeDuration(sign, duration, nudgeResult.nudgedEpochNs, isoDate, largestUnit, startUnit);
     }
     // 10.
     return duration;
}

// RoundDuration ( years, months, weeks, days, hours, minutes, seconds, milliseconds, microseconds, nanoseconds, increment, unit, roundingMode [ , relativeTo ] )
// https://tc39.es/proposal-temporal/#sec-temporal-roundduration
std::optional<InternalDuration> TemporalDuration::round(InternalDuration internalDuration, double increment, TemporalUnit unit, RoundingMode mode)
{
    ASSERT(unit >= TemporalUnit::Day);
 
    // 31.
    if (unit == TemporalUnit::Day) {
        // 31a.
        Int128 fractionalDays = totalTimeDuration(internalDuration.time(), TemporalUnit::Day);
        // 31b.
        Int128 days = roundNumberToIncrement(fractionalDays, (Int128) increment, mode);
        // 31c.
        // 31d.
        return combineDateAndTimeDuration(ISO8601::Duration { 0, 0, 0, (double) days, 0, 0, 0, 0, 0, 0 },
                                          0);
    } else  {
        // 32a.
        Int128 timeDuration = roundTimeDuration(internalDuration.time(), increment, unit, mode);
        // 32b.
        return combineDateAndTimeDuration(ISO8601::Duration(), timeDuration);
    }
/*

if (unit == TemporalUnit::Hour) {
        auto fractionalSeconds = duration.seconds() + duration.milliseconds() * 1e-3 + duration.microseconds() * 1e-6 + duration.nanoseconds() * 1e-9;
        auto fractionalHours = duration.hours() + (duration.minutes() + fractionalSeconds / 60) / 60;
        auto newHours = roundNumberToIncrement(fractionalHours, increment, mode);
        remainder = fractionalHours - newHours;
        duration.setHours(newHours);
    } else if (unit == TemporalUnit::Minute) {
        auto fractionalSeconds = duration.seconds() + duration.milliseconds() * 1e-3 + duration.microseconds() * 1e-6 + duration.nanoseconds() * 1e-9;
        auto fractionalMinutes = duration.minutes() + fractionalSeconds / 60;
        auto newMinutes = roundNumberToIncrement(fractionalMinutes, increment, mode);
        remainder = fractionalMinutes - newMinutes;
        duration.setMinutes(newMinutes);
    } else if (unit == TemporalUnit::Second) {
        auto fractionalSeconds = duration.seconds() + duration.milliseconds() * 1e-3 + duration.microseconds() * 1e-6 + duration.nanoseconds() * 1e-9;
        auto newSeconds = roundNumberToIncrement(fractionalSeconds, increment, mode);
        remainder = fractionalSeconds - newSeconds;
        duration.setSeconds(newSeconds);
    } else if (unit == TemporalUnit::Millisecond) {
        auto fractionalMilliseconds = duration.milliseconds() + duration.microseconds() * 1e-3 + duration.nanoseconds() * 1e-6;
        auto newMilliseconds = roundNumberToIncrement(fractionalMilliseconds, increment, mode);
        remainder = fractionalMilliseconds - newMilliseconds;
        duration.setMilliseconds(newMilliseconds);
    } else if (unit == TemporalUnit::Microsecond) {
        auto fractionalMicroseconds = duration.microseconds() + duration.nanoseconds() * 1e-3;
        auto newMicroseconds = roundNumberToIncrement(fractionalMicroseconds, increment, mode);
        remainder = fractionalMicroseconds - newMicroseconds;
        duration.setMicroseconds(newMicroseconds);
    } else {
        auto newNanoseconds = roundNumberToIncrement(duration.nanoseconds(), increment, mode);
        remainder = duration.nanoseconds() - newNanoseconds;
        duration.setNanoseconds(newNanoseconds);
    }

    for (auto i = static_cast<uint8_t>(unit) + 1u; i < numberOfTemporalUnits; i++)
        duration[i] = 0;

    return remainder;
*/
}

ISO8601::Duration TemporalDuration::round(JSGlobalObject* globalObject, JSValue optionsValue) const
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    JSObject* options = nullptr;
    std::optional<TemporalUnit> smallest;
    std::optional<TemporalUnit> largest;
    TemporalUnit defaultLargestUnit = largestSubduration(m_duration);
    if (optionsValue.isString()) {
        auto string = optionsValue.toWTFString(globalObject);
        RETURN_IF_EXCEPTION(scope, { });

        smallest = temporalUnitType(string);
        if (!smallest) {
            throwRangeError(globalObject, scope, "smallestUnit is an invalid Temporal unit"_s);
            return { };
        }
    } else {
        options = intlGetOptionsObject(globalObject, optionsValue);
        RETURN_IF_EXCEPTION(scope, { });

        smallest = temporalSmallestUnit(globalObject, options, { });
        RETURN_IF_EXCEPTION(scope, { });

        largest = temporalLargestUnit(globalObject, options, { }, defaultLargestUnit);
        RETURN_IF_EXCEPTION(scope, { });

        if (!smallest && !largest) {
            throwRangeError(globalObject, scope, "Cannot round without a smallestUnit or largestUnit option"_s);
            return { };
        }

        if (smallest && largest && smallest.value() < largest.value()) {
            throwRangeError(globalObject, scope, "smallestUnit must be smaller than largestUnit"_s);
            return { };
        }
    }
    TemporalUnit smallestUnit = smallest.value_or(TemporalUnit::Nanosecond);
    TemporalUnit largestUnit = largest.value_or(std::min(defaultLargestUnit, smallestUnit));

    auto roundingMode = temporalRoundingMode(globalObject, options, RoundingMode::HalfExpand);
    RETURN_IF_EXCEPTION(scope, { });

    auto increment = temporalRoundingIncrement(globalObject, options, maximumRoundingIncrement(smallestUnit), false);
    RETURN_IF_EXCEPTION(scope, { });

    // FIXME: Implement relativeTo parameter after PlainDateTime / ZonedDateTime.
    if (largestUnit > TemporalUnit::Year && (years() || months() || weeks() || (days() && largestUnit < TemporalUnit::Day))) {
        throwRangeError(globalObject, scope, "Cannot round a duration of years, months, or weeks without a relativeTo option"_s);
        return { };
    }
    if (largestUnit <= TemporalUnit::Week) {
        throwVMError(globalObject, scope, "FIXME: years, months, or weeks rounding with relativeTo not implemented yet"_s);
        return { };
    }

    // 30. Let internalDuration be ToInternalDurationRecordWith24HourDays(duration).
    std::optional<InternalDuration> internalDuration = toInternalDurationRecordWith24HourDays(m_duration);
    if (!internalDuration)
        throwRangeError(globalObject, scope, "date and time have different signs"_s); // TODO: ??
    auto result = round(internalDuration.value(), increment, smallestUnit, roundingMode);
    if (!result)
        throwRangeError(globalObject, scope, "date and time have different signs"_s);
    return temporalDurationFromInternal(result.value(), largestUnit);
  //  balance(newDuration, largestUnit);
}

double TemporalDuration::total(JSGlobalObject* globalObject, JSValue optionsValue) const
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    String unitString;
    if (optionsValue.isString()) {
        unitString = optionsValue.toWTFString(globalObject);
        RETURN_IF_EXCEPTION(scope, 0);
    } else {
        JSObject* options = intlGetOptionsObject(globalObject, optionsValue);
        RETURN_IF_EXCEPTION(scope, 0);

        unitString = intlStringOption(globalObject, options, vm.propertyNames->unit, { }, { }, { });
        RETURN_IF_EXCEPTION(scope, 0);
    }

    auto unitType = temporalUnitType(unitString);
    if (!unitType) {
        throwRangeError(globalObject, scope, "unit is an invalid Temporal unit"_s);
        return 0;
    }
    TemporalUnit unit = unitType.value();

    // FIXME: Implement relativeTo parameter after PlainDateTime / ZonedDateTime.
    if (unit > TemporalUnit::Year && (years() || months() || weeks() || (days() && unit < TemporalUnit::Day))) {
        throwRangeError(globalObject, scope, "Cannot total a duration of years, months, or weeks without a relativeTo option"_s);
        return { };
    }

    auto internalDuration = toInternalDurationRecordWith24HourDays(m_duration);
    if (!internalDuration)
        throwRangeError(globalObject, scope, "date and time have different signs"_s); // FIXME: ??
    // FIXME: shouldn't cast from Int128 to double here, probably
    return ((double) totalTimeDuration(internalDuration->m_time, unit));
/*
    ISO8601::Duration newDuration = m_duration;
    auto infiniteResult = balance(newDuration, unit);
    if (infiniteResult)
        return infiniteResult.value();
    double remainder = round(newDuration, 1, unit, RoundingMode::Trunc);
    return newDuration[static_cast<uint8_t>(unit)] + remainder;
*/
}

String TemporalDuration::toString(JSGlobalObject* globalObject, JSValue optionsValue) const
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    JSObject* options = intlGetOptionsObject(globalObject, optionsValue);
    RETURN_IF_EXCEPTION(scope, { });

    if (!options)
        RELEASE_AND_RETURN(scope, toString(globalObject));

    PrecisionData data = secondsStringPrecision(globalObject, options);
    RETURN_IF_EXCEPTION(scope, { });
    if (data.unit < TemporalUnit::Second) {
        throwRangeError(globalObject, scope, "smallestUnit must not be \"minute\""_s);
        return { };
    }

    auto roundingMode = temporalRoundingMode(globalObject, options, RoundingMode::Trunc);
    RETURN_IF_EXCEPTION(scope, { });

    // No need to make a new object if we were given explicit defaults.
    if (std::get<0>(data.precision) == Precision::Auto && roundingMode == RoundingMode::Trunc)
        RELEASE_AND_RETURN(scope, toString(globalObject));

    auto maybeInternalDuration = toInternalDuration(m_duration);
    if (!maybeInternalDuration)
        throwRangeError(globalObject, scope, "date and time signs don't match"_s); // FIXME: ??
    auto internalDuration = maybeInternalDuration.value();
    auto timeDuration = roundTimeDuration(internalDuration.m_time, data.increment, data.unit, roundingMode);
    internalDuration.m_time = timeDuration;
    auto roundedLargestUnit = data.unit;
    auto roundedDuration = temporalDurationFromInternal(internalDuration, roundedLargestUnit);
    RELEASE_AND_RETURN(scope, toString(globalObject, roundedDuration, data.precision));
}

static void appendInteger(JSGlobalObject* globalObject, StringBuilder& builder, double value)
{
    ASSERT(std::isfinite(value));

    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    double absValue = std::abs(value);
    if (LIKELY(absValue <= maxSafeInteger())) {
        builder.append(absValue);
        return;
    }

    auto* bigint = JSBigInt::createFrom(globalObject, absValue);
    RETURN_IF_EXCEPTION(scope, void());

    String string = bigint->toString(globalObject, 10);
    RETURN_IF_EXCEPTION(scope, void());

    builder.append(string);
}

// TemporalDurationToString ( years, months, weeks, days, hours, minutes, seconds, milliseconds, microseconds, nanoseconds, precision )
// https://tc39.es/proposal-temporal/#sec-temporal-temporaldurationtostring
String TemporalDuration::toString(JSGlobalObject* globalObject, const ISO8601::Duration& duration, std::tuple<Precision, unsigned> precision)
{
    ASSERT(std::get<0>(precision) == Precision::Auto || std::get<1>(precision) < 10);

    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto balancedMicroseconds = duration.microseconds() + std::trunc(duration.nanoseconds() / 1000);
    auto balancedNanoseconds = std::fmod(duration.nanoseconds(), 1000);
    auto balancedMilliseconds = duration.milliseconds() + std::trunc(balancedMicroseconds / 1000);
    balancedMicroseconds = std::fmod(balancedMicroseconds, 1000);
    auto balancedSeconds = duration.seconds() + std::trunc(balancedMilliseconds / 1000);
    balancedMilliseconds = std::fmod(balancedMilliseconds, 1000);

    StringBuilder builder;

    auto sign = TemporalDuration::sign(duration);
    if (sign < 0)
        builder.append('-');

    builder.append('P');
    if (duration.years()) {
        appendInteger(globalObject, builder, duration.years());
        RETURN_IF_EXCEPTION(scope, { });
        builder.append('Y');
    }
    if (duration.months()) {
        appendInteger(globalObject, builder, duration.months());
        RETURN_IF_EXCEPTION(scope, { });
        builder.append('M');
    }
    if (duration.weeks()) {
        appendInteger(globalObject, builder, duration.weeks());
        RETURN_IF_EXCEPTION(scope, { });
        builder.append('W');
    }
    if (duration.days()) {
        appendInteger(globalObject, builder, duration.days());
        RETURN_IF_EXCEPTION(scope, { });
        builder.append('D');
    }

    // The zero value is displayed in seconds.
    auto usesSeconds = balancedSeconds || balancedMilliseconds || balancedMicroseconds || balancedNanoseconds || !sign || std::get<0>(precision) != Precision::Auto;
    if (!duration.hours() && !duration.minutes() && !usesSeconds)
        return builder.toString();

    builder.append('T');
    if (duration.hours()) {
        appendInteger(globalObject, builder, duration.hours());
        RETURN_IF_EXCEPTION(scope, { });
        builder.append('H');
    }
    if (duration.minutes()) {
        appendInteger(globalObject, builder, duration.minutes());
        RETURN_IF_EXCEPTION(scope, { });
        builder.append('M');
    }
    if (usesSeconds) {
        // TEMPORARY! (pending spec discussion about rebalancing @ https://github.com/tc39/proposal-temporal/issues/2195)
        // Although we must be able to display Number values beyond MAX_SAFE_INTEGER, it does not seem reasonable
        // to require that calculations be performed outside of double space purely to support a case like
        // `Temporal.Duration.from({ microseconds: Number.MAX_VALUE, nanoseconds: Number.MAX_VALUE }).toString()`.
        if (UNLIKELY(!std::isfinite(balancedSeconds))) {
            throwRangeError(globalObject, scope, "Cannot display infinite seconds!"_s);
            return { };
        }

        appendInteger(globalObject, builder, balancedSeconds);
        RETURN_IF_EXCEPTION(scope, { });

        auto fraction = std::abs(balancedMilliseconds) * 1e6 + std::abs(balancedMicroseconds) * 1e3 + std::abs(balancedNanoseconds);
        formatSecondsStringFraction(builder, fraction, precision);

        builder.append('S');
    }

    return builder.toString();
}

} // namespace JSC
