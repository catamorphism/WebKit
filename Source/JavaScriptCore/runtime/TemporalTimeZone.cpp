/*
 * Copyright (C) 2021 Apple Inc. All rights reserved.
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
#include "TemporalTimeZone.h"

#include "FractionToDouble.h"
#include "ISO8601.h"
#include "JSObjectInlines.h"
#include "TemporalZonedDateTime.h"
#include <unicode/ucal.h>
#include <wtf/text/StringParsingBuffer.h>
#include <wtf/unicode/icu/ICUHelpers.h>

namespace JSC {

const ClassInfo TemporalTimeZone::s_info = { "Object"_s, &Base::s_info, nullptr, nullptr, CREATE_METHOD_TABLE(TemporalTimeZone) };

TemporalTimeZone* TemporalTimeZone::createFromID(VM& vm, Structure* structure, TimeZoneID identifier, std::optional<String> original)
{
    TemporalTimeZone* format = new (NotNull, allocateCell<TemporalTimeZone>(vm)) TemporalTimeZone(vm, structure, TimeZone::named(identifier, original));
    format->finishCreation(vm);
    return format;
}

TemporalTimeZone* TemporalTimeZone::createFromUTCOffset(VM& vm, Structure* structure, int64_t utcOffset)
{
    TemporalTimeZone* format = new (NotNull, allocateCell<TemporalTimeZone>(vm)) TemporalTimeZone(vm, structure, TimeZone::offset(utcOffset));
    format->finishCreation(vm);
    return format;
}

TemporalTimeZone* TemporalTimeZone::createFromTimeZone(VM& vm, Structure* structure, ISO8601::TimeZone tz)
{
    TemporalTimeZone* format = new (NotNull, allocateCell<TemporalTimeZone>(vm)) TemporalTimeZone(vm, structure, tz);
    format->finishCreation(vm);
    return format;
}

Structure* TemporalTimeZone::createStructure(VM& vm, JSGlobalObject* globalObject, JSValue prototype)
{
    return Structure::create(vm, globalObject, prototype, TypeInfo(ObjectType, StructureFlags), info());
}

TemporalTimeZone::TemporalTimeZone(VM& vm, Structure* structure, TimeZone timeZone)
    : Base(vm, structure)
    , m_timeZone(timeZone)
{
}

// https://tc39.es/proposal-temporal/#sec-temporal-getoffsetnanosecondsfor
Int128 TemporalTimeZone::getOffsetNanosecondsFor(JSGlobalObject* globalObject,
    ISO8601::TimeZone timeZone, Int128 epochNs)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (timeZone.isOffset())
        return timeZone.offsetNanoseconds();
    RELEASE_AND_RETURN(scope, ISO8601::getNamedTimeZoneOffsetNanoseconds(
        globalObject, timeZone.asID(), ISO8601::ExactTime(epochNs)));
}

// https://tc39.es/proposal-temporal/#sec-temporal-getisodatetimefor
ISO8601::PlainDateTime TemporalTimeZone::getISODateTimeFor(JSGlobalObject* globalObject, ISO8601::TimeZone timeZone, ISO8601::ExactTime epochNs)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto offsetNanoseconds = getOffsetNanosecondsFor(globalObject, timeZone, epochNs.epochNanoseconds());
    RETURN_IF_EXCEPTION(scope, { });
    auto result = TemporalCalendar::getISOPartsFromEpoch(epochNs);
    auto date = result.date();
    auto time = result.time();
    // time.nanosecond() + offsetNanoseconds can overflow int32_t, hence
    // why the arguments to balanceISODateTime are Int128s.
    return TemporalPlainDateTime::balanceISODateTime(globalObject, date.year(), date.month(), date.day(), time.hour(), time.minute(), time.second(), time.millisecond(), time.microsecond(), time.nanosecond() + offsetNanoseconds);
}

static UCalendarMonths toICUMonth(double month)
{
    if (month == 1)
        return UCAL_JANUARY;
    if (month == 2)
        return UCAL_FEBRUARY;
    if (month == 3)
        return UCAL_MARCH;
    if (month == 4)
        return UCAL_APRIL;
    if (month == 5)
        return UCAL_MAY;
    if (month == 6)
        return UCAL_JUNE;
    if (month == 7)
        return UCAL_JULY;
    if (month == 8)
        return UCAL_AUGUST;
    if (month == 9)
        return UCAL_SEPTEMBER;
    if (month == 10)
        return UCAL_OCTOBER;
    if (month == 11)
        return UCAL_NOVEMBER;
    return UCAL_DECEMBER;
}

// https://tc39.es/proposal-temporal/#sec-getnamedtimezoneepochnanoseconds
static Vector<Int128> getNamedTimeZoneEpochNanoseconds(JSGlobalObject* globalObject,
    TimeZoneID timeZoneIdentifier, ISO8601::PlainDateTime isoDateTime)
{
    // Comments based on polyfill

    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    // Get the offset of one day before and after the requested calendar date and
    // clock time, avoiding overflows if near the edge of the Instant range.
    Int128 ns = ISO8601::getUTCEpochNanoseconds(isoDateTime);
    if (timeZoneIdentifier == utcTimeZoneID())
        return Vector<Int128> { ns };

    Int128 nsEarlier = ns - ISO8601::ExactTime::nsPerDay;
    if (nsEarlier < ISO8601::ExactTime::minValue)
        nsEarlier = ISO8601::ExactTime::minValue;

    Int128 nsLater = ns + ISO8601::ExactTime::nsPerDay;
    if (nsLater > ISO8601::ExactTime::maxValue)
        nsLater = ISO8601::ExactTime::maxValue;

    auto earlierOffsetNs = ISO8601::getNamedTimeZoneOffsetNanoseconds(globalObject, timeZoneIdentifier, ISO8601::ExactTime(nsEarlier));
    RETURN_IF_EXCEPTION(scope, { });
    auto laterOffsetNs = ISO8601::getNamedTimeZoneOffsetNanoseconds(globalObject, timeZoneIdentifier, ISO8601::ExactTime(nsLater));
    RETURN_IF_EXCEPTION(scope, { });

    // If before and after offsets are the same, then we assume there was no
    // offset transition in between, and therefore only one exact time can
    // correspond to the provided calendar date and clock time. But if they're
    // different, then there was an offset transition in between, so test both
    // offsets to see which one(s) will yield a matching exact time.

    auto found = earlierOffsetNs == laterOffsetNs ? Vector<Int128> { earlierOffsetNs } : Vector<Int128> { earlierOffsetNs, laterOffsetNs };

    std::optional<String> timeZoneString = vm.timeZoneCache.getTimeZoneNameFromID(timeZoneIdentifier);
    if (!timeZoneString) {
        throwRangeError(globalObject, scope, "bad time zone ID in getNamedTimeZoneOffsetNanoseconds"_s);
        return { };
    }
    // TODO: cache this
    // copied from JSDateMath.cpp
    UErrorCode status = U_ZERO_ERROR;
    auto timeZoneName = timeZoneString->charactersWithNullTermination();
    if (!timeZoneName) {
        throwRangeError(globalObject, scope, "internal error getting time zone data"_s);
        return { };
    }
    Vector<UChar, 32> buffer;
    UBool isSystemID = false;
    status = callBufferProducingFunction(ucal_getCanonicalTimeZoneID, timeZoneName->span().data(), -1, buffer, &isSystemID);
    ASSERT_UNUSED(isSystemID, isSystemID);
    ASSERT_UNUSED(status, U_SUCCESS(status));
    UCalendar* calendar = ucal_open(buffer.span().data(), buffer.size(), "", UCAL_GREGORIAN, &status);
    ASSERT_UNUSED(status, U_SUCCESS(status));

    // From IntlDateTimeFormat::initializeDateTimeFormat():
    // "Gregorian calendar should be used from the beginning of ECMAScript time."
    ucal_setGregorianChange(calendar, minECMAScriptTime, &status);
    ASSERT_UNUSED(status, U_SUCCESS(status));

    Vector<Int128> result;

    for (size_t i = 0; i < found.size(); i++) {
        Int128 offsetNanoseconds = ns - found[i];

    // TODO: refactor w/ getNamedTimeZoneOffsetNanoseconds

        // https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.epochmilliseconds
        // Let ms be floor(ℝ(ns) / 10**6).
        Int128 offsetMillis = ISO8601::ExactTime(offsetNanoseconds).floorEpochMilliseconds();

        ucal_setMillis(calendar, static_cast<double>(offsetMillis), &status);
        int32_t year = ucal_get(calendar, UCAL_YEAR, &status);
        int32_t month = ucal_get(calendar, UCAL_MONTH, &status);
        int32_t day = ucal_get(calendar, UCAL_DATE, &status);
        int32_t hour = ucal_get(calendar, UCAL_HOUR_OF_DAY, &status);
        int32_t minute = ucal_get(calendar, UCAL_MINUTE, &status);
        int32_t second = ucal_get(calendar, UCAL_SECOND, &status);
        int32_t millisecond = ucal_get(calendar, UCAL_MILLISECOND, &status);
        int32_t era = ucal_get(calendar, UCAL_ERA, &status);
        ASSERT_UNUSED(status, U_SUCCESS(status));
        if (!era) // BC = 0
            year = (-year) + 1;

        int32_t expectedYear = static_cast<int32_t>(isoDateTime.date().year());
        int32_t expectedMonth = toICUMonth(static_cast<int32_t>(isoDateTime.date().month()));
        int32_t expectedDay = static_cast<int32_t>(isoDateTime.date().day());
        int32_t expectedHour = static_cast<int32_t>(isoDateTime.time().hour());
        int32_t expectedMinute = static_cast<int32_t>(isoDateTime.time().minute());
        int32_t expectedSecond = static_cast<int32_t>(isoDateTime.time().second());
        int32_t expectedMillisecond = static_cast<int32_t>(isoDateTime.time().millisecond());

        if ((year == expectedYear) && (month == expectedMonth) && (day == expectedDay)
            && (hour == expectedHour) && (minute == expectedMinute) && (second == expectedSecond)
            && (millisecond == expectedMillisecond)) {
            result.append(offsetNanoseconds);
            if (!ISO8601::ExactTime(offsetNanoseconds).isValid()) {
                throwRangeError(globalObject, scope, "time is invalid in getNamedTimeZoneEpochNanoseconds()"_s);
                return { };
            }
            RETURN_IF_EXCEPTION(scope, { });
        }
    }

    ASSERT_UNUSED(status, U_SUCCESS(status));
    ucal_close(calendar);

    return result;
}

// https://tc39.es/proposal-temporal/#sec-temporal-getpossibleepochnanoseconds
Vector<Int128> TemporalTimeZone::getPossibleEpochNanoseconds(JSGlobalObject* globalObject, ISO8601::TimeZone timeZone, ISO8601::PlainDateTime isoDateTime)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto isoDate = isoDateTime.date();
    auto isoTime = isoDateTime.time();

    Vector<Int128> possibleEpochNanoseconds;
    if (timeZone.isOffset()) {
        auto balanced = TemporalPlainDateTime::balanceISODateTime(globalObject, static_cast<Int128>(isoDate.year()), static_cast<Int128>(isoDate.month()), static_cast<Int128>(isoDate.day()), static_cast<Int128>(isoTime.hour()), static_cast<Int128>(isoTime.minute()) - timeZone.offsetMinutes(), static_cast<Int128>(isoTime.second()), static_cast<Int128>(isoTime.millisecond()), static_cast<Int128>(isoTime.microsecond()), static_cast<Int128>(isoTime.nanosecond()));
        RETURN_IF_EXCEPTION(scope, { });
        ISO8601::checkISODaysRange(globalObject, balanced.date());
        RETURN_IF_EXCEPTION(scope, { });
        Int128 epochNanoseconds = ISO8601::getUTCEpochNanoseconds(balanced);
        possibleEpochNanoseconds = Vector<Int128> { epochNanoseconds };
    } else {
        possibleEpochNanoseconds = getNamedTimeZoneEpochNanoseconds(globalObject, timeZone.asID(), isoDateTime);
        RETURN_IF_EXCEPTION(scope, { });
    }
    for (auto epochNanoseconds : possibleEpochNanoseconds) {
        if (!ISO8601::ExactTime(epochNanoseconds).isValid()) {
            throwRangeError(globalObject, scope, "invalid epochNanoseconds result in getPossibleEpochNanoseconds()"_s);
            return { };
        }
    }
    return possibleEpochNanoseconds;
}

// https://tc39.es/proposal-temporal/#sec-temporal-disambiguatepossibleepochnanoseconds
ISO8601::ExactTime TemporalTimeZone::disambiguatePossibleEpochNanoseconds(JSGlobalObject* globalObject,
    Vector<Int128> possibleEpochNs, ISO8601::TimeZone timeZone, ISO8601::PlainDateTime isoDateTime,
    TemporalDisambiguation disambiguation)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto n = possibleEpochNs.size();
    if (n == 1)
        return ISO8601::ExactTime(possibleEpochNs[0]);
    if (n) {
        if (disambiguation == TemporalDisambiguation::Earlier
            || disambiguation == TemporalDisambiguation::Compatible)
            return ISO8601::ExactTime(possibleEpochNs[0]);
        if (disambiguation == TemporalDisambiguation::Later)
            return ISO8601::ExactTime(possibleEpochNs[n - 1]);
        throwRangeError(globalObject, scope, "disambiguation is Reject and multiple instants found in disambiguatePossibleEpochNanoseconds()"_s);
        return { };
    }
    // n == 0
    if (disambiguation == TemporalDisambiguation::Reject) {
        throwRangeError(globalObject, scope, "disambiguation is Reject in disambiguatePossibleEpochNanoseconds() and no possible instants"_s);
        return { };
    }
    auto utcNs = ISO8601::getUTCEpochNanoseconds(isoDateTime);
    auto dayBefore = utcNs - ISO8601::ExactTime::nsPerDay;
    if (!ISO8601::ExactTime(dayBefore).isValid()) {
        throwRangeError(globalObject, scope, "day before is not a valid instant in disambiguatePossibleEpochNanoseconds()"_s);
        return { };
    }
    auto offsetBefore = TemporalTimeZone::getOffsetNanosecondsFor(globalObject, timeZone, dayBefore);
    RETURN_IF_EXCEPTION(scope, { });
    auto dayAfter = utcNs + ISO8601::ExactTime::nsPerDay;
    auto offsetAfter = TemporalTimeZone::getOffsetNanosecondsFor(globalObject, timeZone, dayAfter);
    RETURN_IF_EXCEPTION(scope, { });
    auto nanoseconds = offsetAfter - offsetBefore;
    ASSERT(absInt128(nanoseconds) <= ISO8601::ExactTime::nsPerDay);

    auto isoDate = isoDateTime.date();

    if (disambiguation == TemporalDisambiguation::Earlier) {
        auto earlierTime = TemporalPlainTime::addTime(isoDateTime.time(), -nanoseconds);
        auto earlierDate = TemporalCalendar::addDaysToISODate(isoDate, earlierTime.days());
        auto earlierDateTime = TemporalPlainDateTime::combineISODateAndTimeRecord(earlierDate, ISO8601::PlainTime(earlierTime.hours(), earlierTime.minutes(), earlierTime.seconds(), earlierTime.milliseconds(), earlierTime.microseconds(), earlierTime.nanoseconds()));
        possibleEpochNs = getPossibleEpochNanoseconds(globalObject, timeZone, earlierDateTime);
        RETURN_IF_EXCEPTION(scope, { });
        ASSERT(possibleEpochNs.size() > 0);
        return ISO8601::ExactTime(possibleEpochNs[0]);
    }
    auto laterTime = TemporalPlainTime::addTime(isoDateTime.time(), nanoseconds);
    auto laterDate = TemporalCalendar::addDaysToISODate(isoDate, laterTime.days());
    RETURN_IF_EXCEPTION(scope, { });
    auto laterDateTime = TemporalPlainDateTime::combineISODateAndTimeRecord(laterDate, ISO8601::PlainTime(laterTime.hours(), laterTime.minutes(), laterTime.seconds(), laterTime.milliseconds(), laterTime.microseconds(), laterTime.nanoseconds()));
    possibleEpochNs = getPossibleEpochNanoseconds(globalObject, timeZone, laterDateTime);
    RETURN_IF_EXCEPTION(scope, { });
    n = possibleEpochNs.size();
    ASSERT(n);
    return ISO8601::ExactTime(possibleEpochNs[n - 1]);
}

// https://tc39.es/proposal-temporal/#sec-temporal-getnamedtimezoneprevioustransition
std::optional<ISO8601::ExactTime> TemporalTimeZone::getNamedTimeZonePreviousTransition(JSGlobalObject* globalObject,
    TimeZoneID timeZoneIdentifier, Int128 epochNanoseconds)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    std::optional<String> timeZoneString = vm.timeZoneCache.getTimeZoneNameFromID(timeZoneIdentifier);
    if (!timeZoneString) {
        throwRangeError(globalObject, scope, "bad time zone ID in getNamedTimeZonePreviousTransition"_s);
        return { };
    }
    // copied from JSDateMath.cpp
    UErrorCode status = U_ZERO_ERROR;
    auto timeZoneName = timeZoneString->charactersWithNullTermination();
    if (!timeZoneName) {
        throwRangeError(globalObject, scope, "internal error getting time zone data"_s);
        return { };
    }
    UCalendar* calendar = ucal_open(timeZoneName->span().data(), -1, "", UCAL_DEFAULT, &status);
    ASSERT_UNUSED(status, U_SUCCESS(status));
    double millis = ISO8601::ExactTime(epochNanoseconds).epochMilliseconds();
    // Round up to the nearest millisecond so that we get the correct result
    // for a time 1 nanosecond after the previous transition
    if (epochNanoseconds % 1'000'000 != 0)
        millis++;
    ucal_setMillis(calendar, millis, &status);
    ASSERT_UNUSED(status, U_SUCCESS(status));

    UDate transitionDate = 0;
    bool isValid = ucal_getTimeZoneTransitionDate(calendar, UCAL_TZ_TRANSITION_PREVIOUS,
        &transitionDate, &status);
    ASSERT_UNUSED(status, U_SUCCESS(status));

    ucal_close(calendar);

    if (!isValid)
        return std::nullopt;

    // Also check the offset on and before the transition date, so we don't return
    // transitions that don't change the offset
    Int128 transitionNs = static_cast<Int128>(std::trunc(transitionDate)) * 1'000'000;
    Int128 transitionOffset = ISO8601::getNamedTimeZoneOffsetNanoseconds(globalObject, timeZoneIdentifier,
        ISO8601::ExactTime(transitionNs));
    RETURN_IF_EXCEPTION(scope, { });

    Int128 beforeTransitionNs = static_cast<Int128>(std::trunc(transitionDate) - 1) * 1'000'000;
    Int128 beforeTransitionOffset = ISO8601::getNamedTimeZoneOffsetNanoseconds(globalObject, timeZoneIdentifier,
        ISO8601::ExactTime(beforeTransitionNs));
    RETURN_IF_EXCEPTION(scope, { });

    if (transitionOffset == beforeTransitionOffset) {
        // This transition didn't change the offset, so return the transition
        // before this one
        ASSERT(beforeTransitionNs < epochNanoseconds);
        RELEASE_AND_RETURN(scope, getNamedTimeZonePreviousTransition(globalObject,
            timeZoneIdentifier, beforeTransitionNs));
    }

    if (isValid && transitionNs >= ISO8601::ExactTime::minValue)
        return ISO8601::ExactTime(transitionNs);  
    return std::nullopt;

}

// FIXME refactor
// https://tc39.es/proposal-temporal/#sec-temporal-getnamedtimezonenexttransition
std::optional<ISO8601::ExactTime> TemporalTimeZone::getNamedTimeZoneNextTransition(JSGlobalObject* globalObject, TimeZoneID timeZoneIdentifier, Int128 epochNanoseconds)
{
     VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    std::optional<String> timeZoneString = vm.timeZoneCache.getTimeZoneNameFromID(timeZoneIdentifier);
    if (!timeZoneString) {
        throwRangeError(globalObject, scope, "bad time zone ID in getNamedTimeZonePreviousTransition"_s);
        return { };
    }
    // copied from JSDateMath.cpp
    UErrorCode status = U_ZERO_ERROR;
    auto timeZoneName = timeZoneString->charactersWithNullTermination();
    if (!timeZoneName) {
        throwRangeError(globalObject, scope, "internal error getting time zone data"_s);
        return { };
    }
    UCalendar* calendar = ucal_open(timeZoneName->span().data(), -1, "", UCAL_DEFAULT, &status);
    ASSERT_UNUSED(status, U_SUCCESS(status));
    double millis = ISO8601::ExactTime(epochNanoseconds).floorEpochMilliseconds();
    ucal_setMillis(calendar, millis, &status);
    ASSERT_UNUSED(status, U_SUCCESS(status));

    UDate transitionDate = 0;
    bool isValid = ucal_getTimeZoneTransitionDate(calendar, UCAL_TZ_TRANSITION_NEXT,
        &transitionDate, &status);
    ASSERT_UNUSED(status, U_SUCCESS(status));
    ucal_close(calendar);

    if (!isValid)
        return std::nullopt;

    // Also check the offset before and on the transition date, so we don't return
    // transitions that don't change the offset
    Int128 beforeTransitionNs = static_cast<Int128>(std::trunc(transitionDate) - 1) * 1'000'000;
    Int128 beforeTransitionOffset = ISO8601::getNamedTimeZoneOffsetNanoseconds(globalObject, timeZoneIdentifier,
        ISO8601::ExactTime(beforeTransitionNs));
    RETURN_IF_EXCEPTION(scope, { });

    Int128 transitionNs = static_cast<Int128>(std::trunc(transitionDate)) * 1'000'000;
    Int128 transitionOffset = ISO8601::getNamedTimeZoneOffsetNanoseconds(globalObject, timeZoneIdentifier,
        ISO8601::ExactTime(transitionNs));
    RETURN_IF_EXCEPTION(scope, { });

    if (beforeTransitionOffset == transitionOffset) {
        // This transition didn't change the offset, so return the transition
        // after this one
        ASSERT(transitionNs > epochNanoseconds);
        RELEASE_AND_RETURN(scope, getNamedTimeZoneNextTransition(globalObject,
            timeZoneIdentifier, transitionNs));
    }

    if (isValid && transitionNs <= ISO8601::ExactTime::maxValue)
        return ISO8601::ExactTime(transitionNs);
    return std::nullopt;
}

// TODO: is this necessary?
/*
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto epochMilliseconds = epochNsToMs(epochNanoseconds);
    Int128 bfd = beforeFirstDST() / 1'000'000;
    if (epochMilliseconds < bfd)
        RELEASE_AND_RETURN(scope, getNamedTimeZoneNextTransition(globalObject,
            timeZoneIdentifier, bfd));

    auto now = ISO8601::ExactTime::now();
    auto base = std::max(epochMilliseconds, now.epochNanoseconds() / 1000000);
    auto dayMs = ISO8601::ExactTime::nsPerDay / 1000000;
    auto uppercap = base + dayMs * 366 * 3;
    auto leftMs = epochMilliseconds;
    auto leftOffsetNs = ISO8601::getNamedTimeZoneOffsetNanoseconds(globalObject, timeZoneIdentifier, ISO8601::ExactTime(leftMs * 1'000'000));
    RETURN_IF_EXCEPTION(scope, { });
    auto rightMs = leftMs;
    auto rightOffsetNs = leftOffsetNs;
    while (leftOffsetNs == rightOffsetNs && leftMs < uppercap) {
        rightMs = leftMs + dayMs * 2 * 7;
        if (rightMs > (ISO8601::ExactTime::maxValue / 1000000))
            return std::nullopt;
        rightOffsetNs = ISO8601::getNamedTimeZoneOffsetNanoseconds(globalObject, timeZoneIdentifier, ISO8601::ExactTime(rightMs * 1'000'000));
        RETURN_IF_EXCEPTION(scope, { });
        if (leftOffsetNs == rightOffsetNs)
            leftMs = rightMs;
    }
    if (leftOffsetNs == rightOffsetNs)
        return std::nullopt;
    auto result = bisect([timeZoneIdentifier, globalObject](Int128 epochMs) mutable
        {
            VM& vm = globalObject->vm();
            auto scope = DECLARE_THROW_SCOPE(vm);

            RELEASE_AND_RETURN(scope, ISO8601::getNamedTimeZoneOffsetNanoseconds(globalObject, timeZoneIdentifier, ISO8601::ExactTime(epochMs * 1'000'000)));
        },
        leftMs, rightMs, leftOffsetNs, rightOffsetNs);
    return ISO8601::ExactTime(result * 1000000);
}
*/

// https://tc39.es/proposal-temporal/#sec-temporal-getstartofday
ISO8601::ExactTime TemporalTimeZone::getStartOfDay(JSGlobalObject* globalObject, ISO8601::TimeZone timeZone,
    ISO8601::PlainDate isoDate)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto isoDateTime = TemporalPlainDateTime::combineISODateAndTimeRecord(isoDate, ISO8601::PlainTime());
    auto possibleEpochNs = getPossibleEpochNanoseconds(globalObject, timeZone, isoDateTime);
    RETURN_IF_EXCEPTION(scope, { });
    if (possibleEpochNs.size() > 0)
        return ISO8601::ExactTime(possibleEpochNs[0]);
    ASSERT(!timeZone.isOffset());

    auto utcNs = ISO8601::getUTCEpochNanoseconds(isoDateTime);
    ISO8601::ExactTime dayBefore = ISO8601::ExactTime(utcNs - ISO8601::ExactTime::nsPerDay);
    if (!dayBefore.isValid()) {
        throwRangeError(globalObject, scope, "day before is not valid in getStartOfDay()"_s);
        return { };
    }
    auto result = getNamedTimeZoneNextTransition(globalObject, timeZone.asID(),
        dayBefore.epochNanoseconds());
    RETURN_IF_EXCEPTION(scope, { });
    if (!result) {
        throwRangeError(globalObject, scope, "unable to get next transition in getStartOfDay()"_s);
        return { };
    }
    return result.value();
}

// https://tc39.es/proposal-temporal/#sec-temporal-getepochnanosecondsfor
ISO8601::ExactTime TemporalTimeZone::getEpochNanosecondsFor(JSGlobalObject* globalObject,
    ISO8601::TimeZone timeZone, ISO8601::PlainDateTime isoDateTime, TemporalDisambiguation disambiguation)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto possibleEpochNs = getPossibleEpochNanoseconds(globalObject, timeZone, isoDateTime);
    RETURN_IF_EXCEPTION(scope, { });
    RELEASE_AND_RETURN(scope, disambiguatePossibleEpochNanoseconds(globalObject,
        possibleEpochNs, timeZone, isoDateTime, disambiguation));
}

/*
// https://tc39.es/proposal-temporal/#sec-getavailablenamedtimezoneidentifier
std::optional<ISO8601::TimeZone> TemporalTimeZone::getAvailableNamedTimeZoneIdentifier(JSGlobalObject* globalObject, const Vector<Latin1Character>& chars)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (isUTCTimeZoneString(StringView(chars)))
        return ISO8601::TimeZone::offset(0);
    throwRangeError(globalObject, scope, "getAvailableNamedTimeZoneIdentifier() not yet implemented"_s);
    return { };
}
*/

// https://tc39.es/proposal-temporal/#sec-temporal-totemporaltimezoneidentifier
ISO8601::TimeZone TemporalTimeZone::toTemporalTimeZoneIdentifier(JSGlobalObject* globalObject,
    JSValue temporalTimeZoneLike)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (temporalTimeZoneLike.isObject()) {
        if (temporalTimeZoneLike.inherits<TemporalZonedDateTime>())
            return jsCast<TemporalZonedDateTime*>(temporalTimeZoneLike)->timeZone();
    }
    if (!temporalTimeZoneLike.isString()) {
        throwTypeError(globalObject, scope, "time zone must be ZonedDateTime or string"_s);
        return { };
    }
    auto toParse = temporalTimeZoneLike.toWTFString(globalObject);
    RETURN_IF_EXCEPTION(scope, { });
    auto parseResultOptional = TemporalTimeZone::parseTemporalTimeZoneString(globalObject, toParse);
    RETURN_IF_EXCEPTION(scope, { });
    if (!parseResultOptional) {
        throwRangeError(globalObject, scope, makeString("error parsing time zone from string "_s, toParse));
        return { };
    }
    auto timeZoneIdentifierRecord = parseResultOptional.value();
    return timeZoneIdentifierRecord;
}

// https://tc39.es/proposal-temporal/#sec-temporal-formatoffsettimezoneidentifier
String TemporalTimeZone::formatOffsetTimeZoneIdentifier(int64_t offsetMinutes, std::optional<bool> isSeparated)
{
    auto sign = offsetMinutes >= 0 ? '+' : '-';
    auto absoluteMinutes = std::abs(offsetMinutes);
    auto hour = std::floor(absoluteMinutes / 60);
    auto minute = std::fmod(absoluteMinutes, 60);
    return ISO8601::formatTimeString(sign, hour, minute, 0, 0, std::nullopt, isSeparated);
}

// https://tc39.es/proposal-temporal/#sec-temporal-formatdatetimeutcoffsetrounded
String TemporalTimeZone::formatDateTimeUTCOffsetRounded(Int128 offsetNanoseconds)
{
    Int128 divisor = 60000000000ll;
    offsetNanoseconds = roundNumberToIncrementInt128(offsetNanoseconds, divisor, RoundingMode::HalfExpand);
    ASSERT(!(offsetNanoseconds % divisor));
    Int128 offsetMinutes = offsetNanoseconds / divisor;
    return formatOffsetTimeZoneIdentifier((int64_t) offsetMinutes, std::nullopt);
}


// https://tc39.es/proposal-temporal/#prod-TimeZoneIANAName
template<typename CharacterType>
static std::optional<String> parseTimeZoneIANANameComponent(StringParsingBuffer<CharacterType>& buffer)
{
    //  TimeZoneIANANameComponent :::
    //     TZLeadingChar
    //     TimeZoneIANANameComponent TZChar
    //
    //  TZLeadingChar :::
    //      Alpha
    //      .
    //      _
    //
    //  TZChar :::
    //      TZLeadingChar
    //      DecimalDigit
    //      -
    //      +

    if (buffer.atEnd())
        return std::nullopt;

    auto character = buffer[0];
    if (!(isASCIIAlpha(character) || character == '.' || character == '_'))
        return std::nullopt;

    unsigned index = 0;
    for (; index < buffer.lengthRemaining(); ++index) {
        auto character = buffer[index];
        if (character == '/')
            break;
        if (!(isASCIIAlpha(character) || isASCIIDigit(character)
              || character == '.' || character == '_' || character == '-'
              || character == '+'))
            return std::nullopt;
    }
    if (index == 1)
        return std::nullopt;

    return buffer.consume(index).subspan(0, index);
}

// https://tc39.es/proposal-temporal/#prod-TimeZoneIANAName
template<typename CharacterType>
static bool parseTimeZoneIANAName(StringParsingBuffer<CharacterType>& buffer)
{
    //  TimeZoneIANAName :::
    //      TimeZoneIANANameComponent
    //      TimeZoneIANAName / TimeZoneIANANameComponent
    //

    bool empty = true;

    while (!buffer.atEnd()) {
        std::optional<String> component = parseTimeZoneIANANameComponent(buffer);
        if (!component)
            return false;
        if (buffer.atEnd())
            return true;
        if (*buffer != '/')
            return false;
        buffer.advance();
    }
    return !empty;
}

// https://tc39.es/proposal-temporal/#prod-TimeZoneIANAName
static bool parseTimeZoneIANAName(StringView string)
{
    return readCharactersForParsing(string, [](auto buffer) -> bool {
        auto result = parseTimeZoneIANAName(buffer);
        if (!buffer.atEnd())
            return false;
        return result;
    });
}

// https://tc39.es/proposal-temporal/#sec-parsetimezoneidentifier
std::optional<ISO8601::TimeZone> TemporalTimeZone::parseTimeZoneIdentifier(JSGlobalObject* globalObject, StringView identifier)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (isUTCTimeZoneString(identifier))
        return ISO8601::TimeZone::offset(0);

    auto parseResult = ISO8601::parseUTCOffset(identifier, false); // Don't accept sub-minute precision
    bool isIANAName = false;
    if (!parseResult)
        isIANAName = parseTimeZoneIANAName(identifier);
    if (isIANAName)
        RELEASE_AND_RETURN(scope, ISO8601::parseTimeZoneName(globalObject, identifier));

    if (!parseResult) [[unlikely]]
        return std::nullopt;

    int64_t offsetNanoseconds = parseResult.value();
    ASSERT(!(offsetNanoseconds % ISO8601::ExactTime::nsPerMinute));
    return ISO8601::TimeZone::offset(offsetNanoseconds);
}

static std::optional<ISO8601::TimeZone> parseTimeZoneFromAnnotation(JSGlobalObject* globalObject,
    const ISO8601::TimeZoneAnnotation& annotation)
{
    if (annotation.m_offset) {
        auto offsetNanoseconds = annotation.m_offset.value();
        ASSERT(!(offsetNanoseconds % ISO8601::ExactTime::nsPerMinute));
        return ISO8601::TimeZone::offset(offsetNanoseconds);
    }

    return TemporalTimeZone::parseTimeZoneIdentifier(globalObject, WTF::String(annotation.m_annotation));
}

// https://tc39.es/proposal-temporal/#prod-TimeZoneIdentifier
static bool canBeTimeZoneIdentifier(StringView string)
{
    //  TimeZoneIdentifier :::
    //      UTCOffset[~SubMinutePrecision]
    //      TimeZoneIANAName
    //
    //  UTCOffset[SubMinutePrecision] :::
    //      ASCIISign Hour
    //      ASCIISign Hour TimeSeparator[+Extended] MinuteSecond
    //      ASCIISign Hour TimeSeparator[~Extended] MinuteSecond
    //      [+SubMinutePrecision] ASCIISign Hour TimeSeparator[+Extended] MinuteSecond TimeSeparator[+Extended] MinuteSecond TemporalDecimalFractionopt
    //      [+SubMinutePrecision] ASCIISign Hour TimeSeparator[~Extended] MinuteSecond TimeSeparator[~Extended] MinuteSecond TemporalDecimalFractionopt
    //
    //  TimeZoneIANAName :::
    //      TimeZoneIANANameComponent
    //      TimeZoneIANAName / TimeZoneIANANameComponent
    //
    //  TimeZoneIANANameComponent :::
    //      TZLeadingChar
    //      TimeZoneIANANameComponent TZChar
    //
    //  TZLeadingChar :::
    //      Alpha
    //      .
    //      _
    //
    if (string.isEmpty())
        return false;
    if (string[0] == '+' || string[0] == '-')
        return true;
    if (isASCIIAlpha(string[0]) || string[0] == '.' || string[0] == '_')
        return true;
    return false;
}

// https://tc39.es/proposal-temporal/#sec-temporal-parsetemporaltimezonestring
std::optional<ISO8601::TimeZone> TemporalTimeZone::parseTemporalTimeZoneString(JSGlobalObject* globalObject,
    StringView timeZoneString)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (canBeTimeZoneIdentifier(timeZoneString))
        RELEASE_AND_RETURN(scope, parseTimeZoneIdentifier(globalObject, timeZoneString));
    ISO8601::TimeZoneRecord timeZoneResult;
    auto asDateTime = ISO8601::parseCalendarDateTime(timeZoneString, TemporalDateFormat::Date);
    if (asDateTime) {
        auto [date, optionalTime, optionalTimeZoneRecord, optionalCalendarRecord] = asDateTime.value();
        if (optionalTimeZoneRecord)
            timeZoneResult = optionalTimeZoneRecord.value();
        else
            return std::nullopt;
    } else {
        auto asExactTime = ISO8601::parseInstant(timeZoneString);
        if (asExactTime) {
            // FIXME: support parsing time zone annotation from Instant
            return std::nullopt;
        }
        auto asTime = ISO8601::parseCalendarTime(timeZoneString);
        if (asTime) {
            auto [time, optionalTimeZoneRecord, optionalCalendarRecord] = asTime.value();
            if (optionalTimeZoneRecord)
                timeZoneResult = optionalTimeZoneRecord.value();
            else
                return std::nullopt;
        } else {
            auto asMonthDay = ISO8601::parseCalendarDateTime(timeZoneString, TemporalDateFormat::MonthDay);
            if (asMonthDay) {
                auto [date, optionalTime, optionalTimeZoneRecord, optionalCalendarRecord] = asMonthDay.value();
                if (optionalTimeZoneRecord)
                    timeZoneResult = optionalTimeZoneRecord.value();
                else
                    return std::nullopt;
            } else {
                auto asYearMonth = ISO8601::parseCalendarDateTime(timeZoneString, TemporalDateFormat::YearMonth);
                if (asYearMonth) [[likely]] {
                    auto [date, optionalTime, optionalTimeZoneRecord, optionalCalendarRecord] = asYearMonth.value();
                    if (optionalTimeZoneRecord)
                        timeZoneResult = optionalTimeZoneRecord.value();
                    else
                        return std::nullopt;
                } else
                    return std::nullopt;
            }
        }
    }
    if (timeZoneResult.m_annotation)
        RELEASE_AND_RETURN(scope,
            parseTimeZoneFromAnnotation(globalObject, timeZoneResult.m_annotation.value()));
    if (timeZoneResult.m_z)
        return ISO8601::TimeZone::utc();
    if (timeZoneResult.m_offset) {
        // Check for sub-minute precision in offset string
        auto result = ISO8601::parseUTCOffset(WTF::String(timeZoneResult.m_offset->m_offsetString), false);
        if (!result) [[unlikely]]
            return std::nullopt;
        return ISO8601::TimeZone::offset(timeZoneResult.m_offset->m_offset);
    }
    return std::nullopt;
}

TemporalTimeZone* TemporalTimeZone::from(JSGlobalObject* globalObject, JSValue timeZoneLike, bool parseSubMinutePrecision)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (timeZoneLike.isObject()) {
        if (timeZoneLike.inherits<TemporalZonedDateTime>()) {
            TemporalZonedDateTime* zonedDateTime = jsCast<TemporalZonedDateTime*>(timeZoneLike);
            return TemporalTimeZone::createFromTimeZone(vm, globalObject->timeZoneStructure(), zonedDateTime->timeZone());
        }
    }

    auto timeZoneString = timeZoneLike.toWTFString(globalObject);
    RETURN_IF_EXCEPTION(scope, { });

    std::optional<int64_t> utcOffset = ISO8601::parseUTCOffset(timeZoneString, parseSubMinutePrecision);
    if (utcOffset)
        return TemporalTimeZone::createFromUTCOffset(vm, globalObject->timeZoneStructure(), utcOffset.value());

    std::optional<TimeZone> tz = ISO8601::parseTimeZoneName(globalObject, timeZoneString);
    RETURN_IF_EXCEPTION(scope, { });
    if (tz)
        return TemporalTimeZone::createFromTimeZone(vm, globalObject->timeZoneStructure(), tz.value());

    std::optional<ISO8601::TimeZone> utcOffsetFromInstant = TemporalTimeZone::parseTemporalTimeZoneString(globalObject, timeZoneString);
    RETURN_IF_EXCEPTION(scope, { });
    if (utcOffsetFromInstant) {
        if (utcOffsetFromInstant->isOffset())
            return TemporalTimeZone::createFromUTCOffset(vm, globalObject->timeZoneStructure(), utcOffsetFromInstant->offsetNanoseconds());
        return TemporalTimeZone::createFromID(vm, globalObject->timeZoneStructure(), utcOffsetFromInstant->asID(), timeZoneString);
    }

    if (timeZoneLike.isString())
        throwRangeError(globalObject, scope, "argument needs to be UTC offset string, TimeZone identifier, or temporal Instant string"_s);
    else
        throwTypeError(globalObject, scope, "argument needs to be UTC offset string, TimeZone identifier, or temporal Instant string"_s);
    return { };
}

} // namespace JSC
