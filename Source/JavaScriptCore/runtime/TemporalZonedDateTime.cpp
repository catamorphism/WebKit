/*
 * Copyright (C) 2026 Igalia, S.L. All rights reserved.
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
#include "TemporalZonedDateTime.h"

#include "IntlObjectInlines.h"
#include "ISO8601.h"
#include "JSCInlines.h"
#include "LazyPropertyInlines.h"
#include "ParseInt.h"
#include "TemporalDuration.h"
#include "TemporalInstant.h"
#include "TemporalPlainDate.h"
#include "TemporalPlainDateTime.h"
#include "TemporalPlainMonthDay.h"
#include "TemporalPlainTime.h"
#include "TemporalPlainYearMonth.h"
#include "TemporalTimeZone.h"
#include "VMTrapsInlines.h"

namespace JSC {

const ClassInfo TemporalZonedDateTime::s_info = { "Object"_s, &Base::s_info, nullptr, nullptr, CREATE_METHOD_TABLE(TemporalZonedDateTime) };

TemporalZonedDateTime* TemporalZonedDateTime::create(VM& vm, Structure* structure, ISO8601::ExactTime&& exactTime, ISO8601::TimeZone&& timeZone)
{
    auto* object = new (NotNull, allocateCell<TemporalZonedDateTime>(vm)) TemporalZonedDateTime(vm, structure, WTF::move(exactTime), WTF::move(timeZone));
    object->finishCreation(vm);
    return object;
}

Structure* TemporalZonedDateTime::createStructure(VM& vm, JSGlobalObject* globalObject, JSValue prototype)
{
    return Structure::create(vm, globalObject, prototype, TypeInfo(ObjectType, StructureFlags), info());
}

TemporalZonedDateTime::TemporalZonedDateTime(VM& vm, Structure* structure, ISO8601::ExactTime&& exactTime, ISO8601::TimeZone&& timeZone)
    : Base(vm, structure)
    , m_exactTime(WTF::move(exactTime))
    , m_timeZone(WTF::move(timeZone))
{
}

void TemporalZonedDateTime::finishCreation(VM& vm)
{
    Base::finishCreation(vm);
    ASSERT(inherits(info()));
    m_calendar.initLater(
        [] (const auto& init) {
            VM& vm = init.vm;
            auto* zonedDateTime = jsCast<TemporalZonedDateTime*>(init.owner);
            auto* globalObject = zonedDateTime->globalObject();
            auto* calendar = TemporalCalendar::create(vm, globalObject->calendarStructure(), iso8601CalendarID());
            init.set(calendar);
        });
}

template<typename Visitor>
void TemporalZonedDateTime::visitChildrenImpl(JSCell* cell, Visitor& visitor)
{
    Base::visitChildren(cell, visitor);

    auto* thisObject = jsCast<TemporalZonedDateTime*>(cell);
    thisObject->m_calendar.visit(visitor);
}

DEFINE_VISIT_CHILDREN(TemporalZonedDateTime);

// https://tc39.es/proposal-temporal/#sec-temporal-createtemporalzoneddatetime
TemporalZonedDateTime* TemporalZonedDateTime::tryCreateIfValid(JSGlobalObject* globalObject, Structure* structure, ISO8601::ExactTime&& epochNanoseconds, ISO8601::TimeZone&& timeZone)
{
    VM& vm = globalObject->vm();

    ASSERT(epochNanoseconds.isValid());

    return TemporalZonedDateTime::create(vm, structure, WTF::move(epochNanoseconds), WTF::move(timeZone));
}


// https://tc39.es/proposal-temporal/#sec-temporal-interpretisodatetimeoffset
static ISO8601::ExactTime interpretISODateTimeOffset(JSGlobalObject* globalObject,
    ISO8601::PlainDate isoDate, ISO8601::PlainTime time,
    TemporalOffsetBehavior offsetBehavior, int64_t offsetNanoseconds, ISO8601::TimeZone timeZone,
    TemporalDisambiguation disambiguation, TemporalOffset offsetOption,
    TemporalMatchBehavior matchBehavior)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto isoDateTime = TemporalPlainDateTime::combineISODateAndTimeRecord(isoDate, time);

    if (offsetBehavior == TemporalOffsetBehavior::Wall
        || (offsetBehavior == TemporalOffsetBehavior::Option && offsetOption == TemporalOffset::Ignore)) {
        RELEASE_AND_RETURN(scope, ISO8601::ExactTime(TemporalTimeZone::getEpochNanosecondsFor(
            globalObject, timeZone, isoDateTime, disambiguation)));
    }

    if (offsetBehavior == TemporalOffsetBehavior::Exact
        || (offsetBehavior == TemporalOffsetBehavior::Option && offsetOption == TemporalOffset::Use)) {
        auto balanced = TemporalPlainDateTime::balanceISODateTime(globalObject, static_cast<Int128>(isoDate.year()), static_cast<Int128>(isoDate.month()), static_cast<Int128>(isoDate.day()), static_cast<Int128>(time.hour()), static_cast<Int128>(time.minute()), static_cast<Int128>(time.second()), static_cast<Int128>(time.millisecond()), static_cast<Int128>(time.microsecond()), static_cast<Int128>(time.nanosecond()) - offsetNanoseconds);
        RETURN_IF_EXCEPTION(scope, { });
        checkISODaysRange(globalObject, balanced.date());
        RETURN_IF_EXCEPTION(scope, { });
        auto epochNanoseconds = ISO8601::ExactTime(ISO8601::getUTCEpochNanoseconds(balanced));
        if (!epochNanoseconds.isValid()) {
            throwRangeError(globalObject, scope, "invalid epochNanoseconds result in interpretISODateTimeOffset()"_s);
            return { };
        }
        return epochNanoseconds;
    }

    ASSERT(offsetBehavior == TemporalOffsetBehavior::Option);
    ASSERT(offsetOption == TemporalOffset::Prefer || offsetOption == TemporalOffset::Reject);

    checkISODaysRange(globalObject, isoDate);
    RETURN_IF_EXCEPTION(scope, { });
    auto utcEpochNanoseconds = ISO8601::getUTCEpochNanoseconds(isoDateTime);
    auto possibleEpochNs = TemporalTimeZone::getPossibleEpochNanoseconds(globalObject, timeZone, isoDateTime);
    RETURN_IF_EXCEPTION(scope, { });
    for (auto candidate : possibleEpochNs) {
        auto candidateOffset = utcEpochNanoseconds - candidate;
        if (candidateOffset == offsetNanoseconds)
            return ISO8601::ExactTime(candidate);
        if (matchBehavior == TemporalMatchBehavior::Minutes) {
            Int128 increment = 60;
            increment *= 1000000000;
            auto roundedCandidateNanoseconds = roundNumberToIncrementInt128(candidateOffset, increment, RoundingMode::HalfExpand);
            if (roundedCandidateNanoseconds == offsetNanoseconds)
                return ISO8601::ExactTime(candidate);
        }
    }

    if (offsetOption == TemporalOffset::Reject) {
        throwRangeError(globalObject, scope, "User-provided offset doesn't match any instants for this time zone and date/time"_s);
        return { };
    }

    RELEASE_AND_RETURN(scope, TemporalTimeZone::disambiguatePossibleEpochNanoseconds(globalObject,
        possibleEpochNs, timeZone, ISO8601::PlainDateTime(isoDate, time), disambiguation));
}

TemporalZonedDateTime* TemporalZonedDateTime::with(JSGlobalObject* globalObject, JSObject* temporalZonedDateTimeLike, JSValue options)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (!isPartialTemporalObject(globalObject, temporalZonedDateTimeLike)) {
        RETURN_IF_EXCEPTION(scope, { });
        throwTypeError(globalObject, scope, "argument to with() must be an object, must not be an instance of a time-related or date-related Temporal type, and must not have a calendar or time zone property"_s);
        return { };
    }
    RETURN_IF_EXCEPTION(scope, { });

    auto epochNs = exactTime();
    auto thisTimeZone = timeZone();
    auto thisCalendar = calendar();
    auto offsetNanoseconds = TemporalTimeZone::getOffsetNanosecondsFor(thisTimeZone, epochNs.epochNanoseconds());
    auto isoDateTime = TemporalTimeZone::getISODateTimeFor(globalObject, thisTimeZone, epochNs);
    RETURN_IF_EXCEPTION(scope, { });
    auto isoDate = isoDateTime.date();
    auto isoTime = isoDateTime.time();
    int32_t year = isoDate.year();
    unsigned month = isoDate.month();
    std::optional<WTF::String> monthCode = std::nullopt;
    unsigned day = isoDate.day();
    int32_t hour = isoTime.hour();
    int32_t minute = isoTime.minute();
    int32_t second = isoTime.second();
    int32_t millisecond = isoTime.millisecond();
    int32_t microsecond = isoTime.microsecond();
    int32_t nanosecond = isoTime.nanosecond();

    auto fields =  Vector { FieldName::Day, FieldName::Hour, FieldName::Microsecond, FieldName::Millisecond,
        FieldName::Minute, FieldName::Month, FieldName::MonthCode, FieldName::Nanosecond, FieldName::Offset,
        FieldName::Second, FieldName::Year };
    auto [optionalYear, optionalMonth, optionalMonthCode, optionalDay, optionalHour, optionalMinute, optionalSecond, optionalMillisecond, optionalMicrosecond, optionalNanosecond, optionalOffset, timeZoneOptional] = TemporalCalendar::prepareCalendarFields(globalObject, thisCalendar->identifier(), temporalZonedDateTimeLike, fields, std::nullopt);
    RETURN_IF_EXCEPTION(scope, { });
    year = optionalYear.value_or(year);
    month = optionalMonth.value_or(month);
    monthCode = optionalMonthCode;
    day = optionalDay.value_or(day);
    hour = optionalHour.value_or(hour);
    minute = optionalMinute.value_or(minute);
    second = optionalSecond.value_or(second);
    millisecond = optionalMillisecond.value_or(millisecond);
    microsecond = optionalMicrosecond.value_or(microsecond);
    nanosecond = optionalNanosecond.value_or(nanosecond);
    if (optionalOffset) {
        auto offsetNanosecondsOptional = ISO8601::parseUTCOffset(optionalOffset.value(), false);
        if (!offsetNanosecondsOptional) {
            throwRangeError(globalObject, scope, "invalid offset string in Temporal.ZonedDateTime.with"_s);
            return { };
        }
        offsetNanoseconds = offsetNanosecondsOptional.value();
    }
    auto resolvedOptions = intlGetOptionsObject(globalObject, options);
    RETURN_IF_EXCEPTION(scope, { });
    auto disambiguation = getTemporalDisambiguationOption(globalObject, resolvedOptions);
    RETURN_IF_EXCEPTION(scope, { });
    auto offset = getTemporalOffsetOption(globalObject, resolvedOptions, TemporalOffset::Prefer);
    RETURN_IF_EXCEPTION(scope, { });
    auto overflow = toTemporalOverflow(globalObject, resolvedOptions);
    RETURN_IF_EXCEPTION(scope, { });
    auto dateTimeResult = TemporalCalendar::interpretTemporalDateTimeFields(globalObject,
        thisCalendar->identifier(), year, month, monthCode, day, hour, minute, second, millisecond,
        microsecond, nanosecond, overflow);
    RETURN_IF_EXCEPTION(scope, { });
    auto epochNanoseconds = interpretISODateTimeOffset(globalObject, dateTimeResult.date(),
        dateTimeResult.time(), TemporalOffsetBehavior::Option, static_cast<int64_t>(offsetNanoseconds),
        thisTimeZone, disambiguation, offset, TemporalMatchBehavior::Exactly);
    RETURN_IF_EXCEPTION(scope, { });
    RELEASE_AND_RETURN(scope, TemporalZonedDateTime::tryCreateIfValid(globalObject,
        globalObject->zonedDateTimeStructure(), WTF::move(epochNanoseconds), WTF::move(thisTimeZone)));
}

// https://tc39.es/proposal-temporal/#sec-temporal-temporalzoneddatetimetostring
String TemporalZonedDateTime::temporalZonedDateTimeToString(JSGlobalObject* globalObject, ISO8601::ExactTime exactTime,
    ISO8601::TimeZone timeZone, PrecisionData precision, TemporalShowCalendar showCalendar,
    TemporalShowTimeZone showTimeZone, TemporalShowOffset showOffset, unsigned increment,
    TemporalUnit unit, RoundingMode roundingMode)
{
    Int128 epochNs = TemporalInstant::roundTemporalInstant(exactTime.epochNanoseconds(), increment, unit, roundingMode);
    auto offsetNanoseconds = TemporalTimeZone::getOffsetNanosecondsFor(timeZone, epochNs);
    auto isoDateTime = TemporalTimeZone::getISODateTimeFor(globalObject, timeZone, ISO8601::ExactTime(epochNs));
    auto dateTimeString = ISO8601::temporalDateTimeToString(isoDateTime.date(), isoDateTime.time(), precision.precision);
    String offsetString;
    if (showOffset != TemporalShowOffset::Never)
        offsetString = TemporalTimeZone::formatDateTimeUTCOffsetRounded(offsetNanoseconds);
    String timeZoneString;
    if (showTimeZone != TemporalShowTimeZone::Never) {
        String flag;
        if (showTimeZone == TemporalShowTimeZone::Critical)
            flag = "!"_s;
        timeZoneString = makeString('[', flag, formatTimeZone(timeZone), ']');
    }
    auto calendarString = TemporalCalendar::formatCalendarAnnotation(showCalendar);
    return makeString(dateTimeString, offsetString, timeZoneString, calendarString);
}

// https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.tostring
String TemporalZonedDateTime::toString(JSGlobalObject* globalObject, JSValue optionsValue) const
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    JSObject* options = intlGetOptionsObject(globalObject, optionsValue);
    RETURN_IF_EXCEPTION(scope, { });

    if (!options)
        RELEASE_AND_RETURN(scope, toString(globalObject));

    auto showCalendar = getTemporalShowCalendarNameOption(globalObject, options);
    RETURN_IF_EXCEPTION(scope, { });
    TemporalFractionalSecondDigits digits =
        temporalFractionalSecondDigits(globalObject, options);
    RETURN_IF_EXCEPTION(scope, { });
    auto showOffset = getTemporalShowOffsetOption(globalObject, options);
    RETURN_IF_EXCEPTION(scope, { });
    auto roundingMode = temporalRoundingMode(globalObject, options, RoundingMode::Trunc);
    RETURN_IF_EXCEPTION(scope, { });
    std::optional<String> smallestUnitString = temporalSmallestUnit(globalObject, options);
    RETURN_IF_EXCEPTION(scope, { });
    auto showTimeZone = getTemporalShowTimeZoneNameOption(globalObject, options);
    RETURN_IF_EXCEPTION(scope, { });
    auto smallestUnit = validateSmallestUnit(globalObject, smallestUnitString,
        { TemporalUnit::Year, TemporalUnit::Month, TemporalUnit::Week, TemporalUnit::Day });

    if (smallestUnit == TemporalUnit::Hour) {
        throwRangeError(globalObject, scope, "smallestUnit cannot be hour"_s);
        return { };
    }

    PrecisionData precision = secondsStringPrecision(globalObject, smallestUnit, digits);
    RETURN_IF_EXCEPTION(scope, { });

    RELEASE_AND_RETURN(scope, temporalZonedDateTimeToString(globalObject, m_exactTime.get(), m_timeZone, precision, showCalendar, showTimeZone,
        showOffset, precision.increment, precision.unit, roundingMode));
}

static bool isUTCTimeZoneAnnotation(std::optional<ISO8601::TimeZoneAnnotation>& annotation)
{
    if (!annotation)
        return false;
    return isUTCTimeZoneString(WTF::String(annotation.value().m_annotation));
}

// https://tc39.es/proposal-temporal/#sec-temporal-totemporalzoneddatetime
TemporalZonedDateTime* TemporalZonedDateTime::from(JSGlobalObject* globalObject, JSValue itemValue, std::optional<JSValue> optionsValue)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto offsetBehavior = TemporalOffsetBehavior::Option;
    auto matchBehavior = TemporalMatchBehavior::Exactly;
    auto disambiguation = TemporalDisambiguation::Compatible;
    TemporalOffset offsetOption = TemporalOffset::Reject;
    auto overflow = TemporalOverflow::Constrain;
    std::optional<String> offsetString;
    TimeZone timeZone;

    ISO8601::PlainDate isoDate;
    ISO8601::PlainTime time;

    if (itemValue.isObject()) {
        std::optional<JSObject*> options = std::nullopt;
        if (optionsValue) {
            options = intlGetOptionsObject(globalObject, optionsValue.value());
            RETURN_IF_EXCEPTION(scope, { });
        }

        if (itemValue.inherits<TemporalZonedDateTime>()) {
            if (options) {
                getTemporalDisambiguationOption(globalObject, options.value());
                RETURN_IF_EXCEPTION(scope, { });
                getTemporalOffsetOption(globalObject, options.value(), TemporalOffset::Reject);
                RETURN_IF_EXCEPTION(scope, { });
                toTemporalOverflow(globalObject, options.value());
                RETURN_IF_EXCEPTION(scope, { });
            }
            auto zdt = jsCast<TemporalZonedDateTime*>(itemValue);
            RELEASE_AND_RETURN(scope, TemporalZonedDateTime::tryCreateIfValid(globalObject,
                globalObject->zonedDateTimeStructure(), zdt->exactTime(), zdt->timeZone()));
        }

        auto item = jsCast<JSObject*>(itemValue);
        CalendarID calendar = TemporalCalendar::getTemporalCalendarIdentifierWithISODefault(globalObject, item);
        RETURN_IF_EXCEPTION(scope, { });
        auto [optionalYear, optionalMonth, optionalMonthCode, optionalDay, optionalHour, optionalMinute,
            optionalSecond, optionalMillisecond, optionalMicrosecond, optionalNanosecond, optionalOffset,
            timeZoneOptional] = TemporalCalendar::prepareCalendarFields(globalObject, calendar, item,
            Vector { FieldName::Day, FieldName::Hour, FieldName::Microsecond, FieldName::Millisecond, FieldName::Minute, FieldName::Month, FieldName::MonthCode, FieldName::Nanosecond, FieldName::Offset, FieldName::Second, FieldName::TimeZone, FieldName::Year }, Vector { FieldName::TimeZone });
        RETURN_IF_EXCEPTION(scope, { });
        ASSERT(timeZoneOptional);
        timeZone = timeZoneOptional.value();
        offsetString = optionalOffset;
        if (!optionalOffset)
            offsetBehavior = TemporalOffsetBehavior::Wall;
        if (options) {
            disambiguation = getTemporalDisambiguationOption(globalObject, options.value());
            RETURN_IF_EXCEPTION(scope, { });
            offsetOption = getTemporalOffsetOption(globalObject, options.value(), TemporalOffset::Reject);
            RETURN_IF_EXCEPTION(scope, { });
            overflow = toTemporalOverflow(globalObject, options.value());
            RETURN_IF_EXCEPTION(scope, { });
        }
        auto result = TemporalCalendar::interpretTemporalDateTimeFields(globalObject, calendar, optionalYear,
            optionalMonth, optionalMonthCode, optionalDay, optionalHour.value_or(0),
            optionalMinute.value_or(0), optionalSecond.value_or(0), optionalMillisecond.value_or(0),
            optionalMicrosecond.value_or(0), optionalNanosecond.value_or(0), overflow);
        RETURN_IF_EXCEPTION(scope, { });
        isoDate = result.date();
        time = result.time();
    } else {
        if (!itemValue.isString()) {
            throwTypeError(globalObject, scope, "can only convert to ZonedDateTime from object or string values"_s);
            return { };
        }

        auto string = itemValue.toWTFString(globalObject);
        RETURN_IF_EXCEPTION(scope, { });

        auto dateTime = ISO8601::parseTemporalDateTimeString(string);
        if (!dateTime) {
            throwRangeError(globalObject, scope, makeString("in Temporal.ZonedDateTime.from, error parsing "_s, string));
            return { };
        }

        auto [plainDate, plainTimeOptional, timeZoneOptional, calendarOptional] = WTF::move(dateTime.value());
        if (!timeZoneOptional) {
            throwRangeError(globalObject, scope, "string must have a time zone annotation to convert to ZonedDateTime"_s);
            return { };
        }
        if (!(timeZoneOptional->m_z || (timeZoneOptional->m_annotation
            && (timeZoneOptional->m_annotation->m_offset
                || isUTCTimeZoneAnnotation(timeZoneOptional->m_annotation))))) {
            throwRangeError(globalObject, scope, "in Temporal.ZonedDateTime, parsing strings with named time zones not implemented yet"_s);
            return { };
        }

        auto annotation = timeZoneOptional->m_annotation;
        if (!annotation) {
            throwRangeError(globalObject, scope, "Temporal.ZonedDateTime requires a time zone ID in brackets"_s);
            return { };
        }
        timeZone = TemporalTimeZone::toTemporalTimeZoneIdentifier(globalObject,
            jsString(vm, WTF::String(annotation->m_annotation)));
        RETURN_IF_EXCEPTION(scope, { });
        if (timeZoneOptional->m_offset)
            offsetString = WTF::String(timeZoneOptional->m_offset->m_offsetString);
        if (timeZoneOptional->m_z)
            offsetBehavior = TemporalOffsetBehavior::Exact;
        else if (!offsetString)
            offsetBehavior = TemporalOffsetBehavior::Wall;
        matchBehavior = TemporalMatchBehavior::Minutes;
        if (optionsValue) {
            JSObject* options = intlGetOptionsObject(globalObject, optionsValue.value());
            RETURN_IF_EXCEPTION(scope, { });
            disambiguation = getTemporalDisambiguationOption(globalObject, options);
            RETURN_IF_EXCEPTION(scope, { });
            offsetOption = getTemporalOffsetOption(globalObject, options, TemporalOffset::Reject);
            RETURN_IF_EXCEPTION(scope, { });
            toTemporalOverflow(globalObject, options);
            RETURN_IF_EXCEPTION(scope, { });
        }
        isoDate = plainDate;
        time = plainTimeOptional.value_or(ISO8601::PlainTime());
    }
    int64_t offsetNanoseconds = 0;
    if (offsetBehavior == TemporalOffsetBehavior::Option) {
        if (!offsetString) {
            throwRangeError(globalObject, scope, "missing offset in ZonedDateTime.from"_s);
            return { };
        }
        std::optional<int64_t> offsetNanosecondsOptional = ISO8601::parseUTCOffset(offsetString.value(), true);
        if (!offsetNanosecondsOptional) {
            throwRangeError(globalObject, scope, "error parsing offset in ZonedDateTime.from"_s);
            return { };
        }
        offsetNanoseconds = offsetNanosecondsOptional.value();
    }
    auto epochNanoseconds = interpretISODateTimeOffset(globalObject, isoDate, time, offsetBehavior, offsetNanoseconds, timeZone, disambiguation, offsetOption, matchBehavior);
    RETURN_IF_EXCEPTION(scope, { });
    return TemporalZonedDateTime::tryCreateIfValid(globalObject, globalObject->zonedDateTimeStructure(),
        WTF::move(epochNanoseconds), WTF::move(timeZone));
}

} // namespace JSC
