/*
 * Copyright (C) 2022 Sony Interactive Entertainment Inc.
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
#include "TemporalPlainDateTime.h"

#include "IntlObjectInlines.h"
#include "JSCInlines.h"
#include "LazyPropertyInlines.h"
#include "TemporalPlainDate.h"
#include "TemporalPlainTime.h"
#include "VMTrapsInlines.h"

namespace JSC {

const ClassInfo TemporalPlainDateTime::s_info = { "Object"_s, &Base::s_info, nullptr, nullptr, CREATE_METHOD_TABLE(TemporalPlainDateTime) };

TemporalPlainDateTime* TemporalPlainDateTime::create(VM& vm, Structure* structure, ISO8601::PlainDate&& plainDate, ISO8601::PlainTime&& plainTime)
{
    auto* object = new (NotNull, allocateCell<TemporalPlainDateTime>(vm)) TemporalPlainDateTime(vm, structure, WTF::move(plainDate), WTF::move(plainTime));
    object->finishCreation(vm);
    return object;
}

TemporalPlainDateTime* TemporalPlainDateTime::create(VM& vm, Structure* structure, ISO8601::PlainDate&& plainDate, ISO8601::PlainTime&& plainTime, TemporalCalendar* calendar)
{
    auto* object = new (NotNull, allocateCell<TemporalPlainDate>(vm)) TemporalPlainDateTime(vm, structure, WTF::move(plainDate), WTF::move(plainTime));
    object->finishCreation(vm);
    object->m_calendar.set(vm, object, calendar);
    return object;
}

Structure* TemporalPlainDateTime::createStructure(VM& vm, JSGlobalObject* globalObject, JSValue prototype)
{
    return Structure::create(vm, globalObject, prototype, TypeInfo(ObjectType, StructureFlags), info());
}

TemporalPlainDateTime::TemporalPlainDateTime(VM& vm, Structure* structure, ISO8601::PlainDate&& plainDate, ISO8601::PlainTime&& plainTime)
    : Base(vm, structure)
    , m_plainDate(WTF::move(plainDate))
    , m_plainTime(WTF::move(plainTime))
{
}

void TemporalPlainDateTime::finishCreation(VM& vm)
{
    Base::finishCreation(vm);
    ASSERT(inherits(info()));
    m_calendar.initLater(
        [] (const auto& init) {
            VM& vm = init.vm;
            auto* globalObject = jsCast<TemporalPlainDateTime*>(init.owner)->globalObject();
            auto* calendar = TemporalCalendar::create(vm, globalObject->calendarStructure(), iso8601CalendarID());
            init.set(calendar);
        });
}

template<typename Visitor>
void TemporalPlainDateTime::visitChildrenImpl(JSCell* cell, Visitor& visitor)
{
    Base::visitChildren(cell, visitor);

    auto* thisObject = jsCast<TemporalPlainDateTime*>(cell);
    thisObject->m_calendar.visit(visitor);
}

DEFINE_VISIT_CHILDREN(TemporalPlainDateTime);

// https://tc39.es/proposal-temporal/#sec-temporal-createtemporaldatetime
TemporalPlainDateTime* TemporalPlainDateTime::tryCreateIfValid(JSGlobalObject* globalObject, Structure* structure, ISO8601::PlainDate&& plainDate, ISO8601::PlainTime&& plainTime, std::optional<TemporalCalendar*> calendar)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (!ISO8601::isDateTimeWithinLimits(plainDate.year(), plainDate.month(), plainDate.day(), plainTime.hour(), plainTime.minute(), plainTime.second(), plainTime.millisecond(), plainTime.microsecond(), plainTime.nanosecond())) {
        throwRangeError(globalObject, scope, "date time is out of range of ECMAScript representation"_s);
        return { };
    }

    if (calendar)
        return TemporalPlainDateTime::create(vm, structure, WTF::move(plainDate), WTF::move(plainTime), calendar.value());
    return TemporalPlainDateTime::create(vm, structure, WTF::move(plainDate), WTF::move(plainTime));
}

TemporalPlainDateTime* TemporalPlainDateTime::tryCreateIfValid(JSGlobalObject* globalObject, Structure* structure, ISO8601::Duration&& duration, std::optional<TemporalCalendar*> calendar)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto plainDate = TemporalPlainDate::toPlainDate(globalObject, duration);
    RETURN_IF_EXCEPTION(scope, { });

    auto plainTime = TemporalPlainTime::toPlainTime(globalObject, duration);
    RETURN_IF_EXCEPTION(scope, { });

    RELEASE_AND_RETURN(scope, TemporalPlainDateTime::tryCreateIfValid(globalObject, structure, WTF::move(plainDate), WTF::move(plainTime), calendar));
}

// https://tc39.es/proposal-temporal/#sec-temporal-combineisodateandtimerecord
ISO8601::PlainDateTime TemporalPlainDateTime::combineISODateAndTimeRecord(ISO8601::PlainDate isoDate, ISO8601::PlainTime isoTime)
{
    return ISO8601::PlainDateTime(isoDate, isoTime);
}

// https://tc39.es/proposal-temporal/#sec-temporal-combineisodateandtimerecord
ISO8601::PlainDateTime TemporalPlainDateTime::combineISODateAndTimeRecord(ISO8601::PlainDate isoDate, ISO8601::Duration time)
{
    // NOTE: time.[[Days]] is ignored.
    return ISO8601::PlainDateTime(isoDate, ISO8601::PlainTime(time.hours(), time.minutes(), time.seconds(),
        time.milliseconds(), time.microseconds(), time.nanoseconds()));
}

// https://tc39.es/proposal-temporal/#sec-temporal-totemporaldatetime
TemporalPlainDateTime* TemporalPlainDateTime::from(JSGlobalObject* globalObject, JSValue itemValue, JSValue optionsValue)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (itemValue.isObject()) {
        if (itemValue.inherits<TemporalPlainDateTime>())
            return jsCast<TemporalPlainDateTime*>(itemValue);

        if (itemValue.inherits<TemporalPlainDate>()) {
            toTemporalOverflow(globalObject, optionsValue);
            RETURN_IF_EXCEPTION(scope, { });

            return TemporalPlainDateTime::create(vm, globalObject->plainDateTimeStructure(), jsCast<TemporalPlainDate*>(itemValue)->plainDate(), { });
        }

        JSObject* calendarObject = TemporalCalendar::getTemporalCalendarWithISODefault(globalObject, itemValue);
        RETURN_IF_EXCEPTION(scope, { });

        if (!calendarObject->inherits<TemporalCalendar>()) {
            throwRangeError(globalObject, scope, "bad calendar object in Temporal.PlainDateTime.from"_s);
            return { };
        }
        TemporalCalendar* calendar = jsCast<TemporalCalendar*>(calendarObject);
        if (!calendar->isISO8601()) {
            throwRangeError(globalObject, scope, "unimplemented: from non-ISO8601 calendar"_s);
            return { };
        }

        auto fields =  Vector { FieldName::Day, FieldName::Hour, FieldName::Microsecond, FieldName::Millisecond,
            FieldName::Minute, FieldName::Month, FieldName::MonthCode, FieldName::Nanosecond, FieldName::Second,
            FieldName::Year };
        auto [optionalYear, optionalMonth, optionalMonthCode, optionalDay, optionalHour, optionalMinute,
            optionalSecond, optionalMillisecond, optionalMicrosecond, optionalNanosecond, optionalOffset,
            timeZoneOptional] = TemporalCalendar::prepareCalendarFields(globalObject, calendar->identifier(),
                asObject(itemValue), fields, std::nullopt);
        RETURN_IF_EXCEPTION(scope, { });

        auto hour = optionalHour.value_or(0);
        auto minute = optionalMinute.value_or(0);
        auto second = optionalSecond.value_or(0);
        auto millisecond = optionalMillisecond.value_or(0);
        auto microsecond = optionalMicrosecond.value_or(0);
        auto nanosecond = optionalNanosecond.value_or(0);

        auto overflow = TemporalOverflow::Constrain;
        overflow = toTemporalOverflow(globalObject, optionsValue);
        RETURN_IF_EXCEPTION(scope, { });

        auto result = TemporalCalendar::interpretTemporalDateTimeFields(globalObject, calendar->identifier(),
            optionalYear, optionalMonth, optionalMonthCode, optionalDay, hour, minute, second,
            millisecond, microsecond, nanosecond, overflow);
        RETURN_IF_EXCEPTION(scope, { });

        if (calendar->identifier() != iso8601CalendarID())
            RELEASE_AND_RETURN(scope, TemporalPlainDateTime::tryCreateIfValid(globalObject, globalObject->plainDateTimeStructure(), result.date(), result.time(), calendar));
        RELEASE_AND_RETURN(scope, TemporalPlainDateTime::tryCreateIfValid(globalObject, globalObject->plainDateTimeStructure(), result.date(), result.time(), std::nullopt));
    }

    if (!itemValue.isString()) {
        throwTypeError(globalObject, scope, "can only convert to PlainDateTime from object or string values"_s);
        return { };
    }

    auto string = itemValue.toWTFString(globalObject);
    RETURN_IF_EXCEPTION(scope, { });

    // https://tc39.es/proposal-temporal/#sec-temporal-parsetemporaldatetimestring
    // TemporalDateString :
    //     CalendarDateTime
    auto dateTime = ISO8601::parseCalendarDateTime(string, TemporalDateFormat::Date);
    if (dateTime) {
        auto [plainDate, plainTimeOptional, timeZoneOptional, calendarOptional] = WTF::move(dateTime.value());
        if (!(timeZoneOptional && timeZoneOptional->m_z)) {
            JSObject* options = intlGetOptionsObject(globalObject, optionsValue);
            RETURN_IF_EXCEPTION(scope, { });
            toTemporalOverflow(globalObject, options); // Validate overflow
            RETURN_IF_EXCEPTION(scope, { });

            if (calendarOptional) {
                auto calendarID = TemporalCalendar::parseTemporalCalendarString(globalObject, StringView(calendarOptional.value()));
                RETURN_IF_EXCEPTION(scope, { });
                if (!calendarID) {
                    throwRangeError(globalObject, scope, "bad calendarID parsing date/time string in TemporalPlainDateTime.from"_s);
                    return { };
                }
                TemporalCalendar* calendar = TemporalCalendar::create(vm, globalObject->calendarStructure(), calendarID.value());
                RELEASE_AND_RETURN(scope, TemporalPlainDateTime::tryCreateIfValid(globalObject, globalObject->plainDateTimeStructure(), WTF::move(plainDate), plainTimeOptional.value_or(ISO8601::PlainTime()), calendar));
            }
            RELEASE_AND_RETURN(scope, TemporalPlainDateTime::tryCreateIfValid(globalObject, globalObject->plainDateTimeStructure(), WTF::move(plainDate), plainTimeOptional.value_or(ISO8601::PlainTime()), std::nullopt));
        }
    }

    throwRangeError(globalObject, scope, "invalid date string"_s);
    return { };
}

// https://tc39.es/proposal-temporal/#sec-temporal-compareisodatetime
int32_t TemporalPlainDateTime::compare(TemporalPlainDateTime* plainDateTime1, TemporalPlainDateTime* plainDateTime2)
{
    if (auto dateResult = TemporalCalendar::isoDateCompare(plainDateTime1->plainDate(), plainDateTime2->plainDate()))
        return dateResult;

    return TemporalPlainTime::compare(plainDateTime1->plainTime(), plainDateTime2->plainTime());
}

static void incrementDay(ISO8601::Duration& duration)
{
    double year = duration.years();
    double month = duration.months();
    double day = duration.days();

    double daysInMonth = ISO8601::daysInMonth(year, month);
    if (day < daysInMonth) {
        duration.setDays(day + 1);
        return;
    }

    duration.setDays(1);
    if (month < 12) {
        duration.setMonths(month + 1);
        return;
    }

    duration.setMonths(1);
    duration.setYears(year + 1);
}

String TemporalPlainDateTime::toString(JSGlobalObject* globalObject, JSValue optionsValue)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    JSObject* options = intlGetOptionsObject(globalObject, optionsValue);
    RETURN_IF_EXCEPTION(scope, { });

    if (!options)
        return toString(""_s);

    TemporalShowCalendar showCalendar = getTemporalShowCalendarNameOption(globalObject, options);
    RETURN_IF_EXCEPTION(scope, { });

    auto precision = temporalFractionalSecondDigits(globalObject, options);
    RETURN_IF_EXCEPTION(scope, { });
    auto roundingMode = temporalRoundingMode(globalObject, options, RoundingMode::Trunc);
    RETURN_IF_EXCEPTION(scope, { });

    auto smallestUnitString = temporalSmallestUnit(globalObject, options);
    RETURN_IF_EXCEPTION(scope, { });
    auto smallestUnit = validateSmallestUnit(globalObject, smallestUnitString, { TemporalUnit::Year, TemporalUnit::Month, TemporalUnit::Week, TemporalUnit::Day });
    RETURN_IF_EXCEPTION(scope, { });
    if (smallestUnit == TemporalUnit::Hour) {
        throwRangeError(globalObject, scope, "smallestUnit cannot be hour in Temporal.PlainTime.toString"_s);
        return { };
    }

    PrecisionData data = secondsStringPrecision(globalObject, smallestUnit, precision);
    RETURN_IF_EXCEPTION(scope, { });

    // No need to make a new object if we were given explicit defaults.
    if (std::get<0>(data.precision) == Precision::Auto && roundingMode == RoundingMode::Trunc)
        return toString(""_s);

    auto duration = TemporalPlainTime::roundTime(m_plainTime, data.increment, data.unit, roundingMode, std::nullopt);
    auto plainTime = TemporalPlainTime::toPlainTime(globalObject, duration);
    RETURN_IF_EXCEPTION(scope, { });

    double extraDays = duration.days();
    duration.setYears(year());
    duration.setMonths(month());
    duration.setDays(day());
    if (extraDays) {
        ASSERT(extraDays == 1);
        incrementDay(duration);
    }

    auto plainDate = TemporalPlainDate::toPlainDate(globalObject, duration);
    RETURN_IF_EXCEPTION(scope, { });

    if (!isoDateTimeWithinLimits(combineISODateAndTimeRecord(plainDate, plainTime))) {
        throwRangeError(globalObject, scope, "Duration out of range after rounding"_s);
        return { };
    }

    String calendarString = calendar()->formatCalendarAnnotation(showCalendar);

    return ISO8601::temporalDateTimeToString(plainDate, plainTime, data.precision, calendarString);
}

String TemporalPlainDateTime::monthCode() const
{
    return ISO8601::monthCode(m_plainDate.month());
}

uint8_t TemporalPlainDateTime::dayOfWeek() const
{
    return ISO8601::dayOfWeek(m_plainDate);
}

uint16_t TemporalPlainDateTime::dayOfYear() const
{
    return ISO8601::dayOfYear(m_plainDate);
}

uint8_t TemporalPlainDateTime::weekOfYear() const
{
    return ISO8601::weekOfYear(m_plainDate);
}

TemporalPlainDateTime* TemporalPlainDateTime::addDurationToDateTime(JSGlobalObject* globalObject,
    bool isAdd, ISO8601::Duration duration, JSObject* options) {
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (!isAdd)
        duration = -duration;
    TemporalOverflow overflow = toTemporalOverflow(globalObject, options);
    RETURN_IF_EXCEPTION(scope, { });
    auto internalDuration = TemporalDuration::toInternalDurationRecordWith24HourDays(globalObject, duration);
    RETURN_IF_EXCEPTION(scope, { });
    auto timeResult = TemporalPlainTime::addTime(m_plainTime, internalDuration.time());
    auto dateDuration = TemporalDuration::adjustDateDurationRecord(globalObject, internalDuration.dateDuration(),
        timeResult.days(), std::nullopt, std::nullopt);
    RETURN_IF_EXCEPTION(scope, { });
    auto addedDate = TemporalCalendar::isoDateAdd(globalObject, m_plainDate, dateDuration, overflow);
    RETURN_IF_EXCEPTION(scope, { });
    auto result = combineISODateAndTimeRecord(addedDate,
        ISO8601::PlainTime(timeResult.hours(), timeResult.minutes(), timeResult.seconds(),
            timeResult.milliseconds(), timeResult.microseconds(), timeResult.nanoseconds()));

    if (calendar()->identifier() != iso8601CalendarID()) {
        TemporalCalendar* cal = TemporalCalendar::create(vm, globalObject->calendarStructure(), calendar()->identifier());
        RELEASE_AND_RETURN(scope, TemporalPlainDateTime::tryCreateIfValid(globalObject, globalObject->plainDateTimeStructure(), result.date(), result.time(), cal));
    }
    RELEASE_AND_RETURN(scope, TemporalPlainDateTime::tryCreateIfValid(globalObject, globalObject->plainDateTimeStructure(), result.date(), result.time(), std::nullopt));
}

TemporalPlainDateTime* TemporalPlainDateTime::with(JSGlobalObject* globalObject, JSObject* temporalDateTimeLike, JSValue optionsValue)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    rejectObjectWithCalendarOrTimeZone(globalObject, temporalDateTimeLike);
    RETURN_IF_EXCEPTION(scope, { });

    if (!calendar()->isISO8601()) {
        throwRangeError(globalObject, scope, "unimplemented: from non-ISO8601 calendar"_s);
        return { };
    }

    auto [y, m, d, optionalMonthCode, overflow, optionalHour, optionalMinute, optionalSecond, optionalMillisecond, optionalMicrosecond, optionalNanosecond, any] = TemporalPlainDate::mergeDateTimeFields(globalObject, temporalDateTimeLike, optionsValue, year(), month(), day(), UnitGroup::DateTime);
    RETURN_IF_EXCEPTION(scope, { });

    if (any == TemporalAnyProperties::None) {
        throwTypeError(globalObject, scope, "Object must contain at least one Temporal date or time property"_s);
        return { };
    }

    ASSERT(y);
    ASSERT(m);
    ASSERT(d);

    auto plainDate = TemporalCalendar::isoDateFromFields(globalObject, TemporalDateFormat::Date, y.value(), m.value(), d.value(), optionalMonthCode, overflow);
    RETURN_IF_EXCEPTION(scope, { });

    ISO8601::Duration duration { };
    duration.setHours(optionalHour.value_or(hour()));
    duration.setMinutes(optionalMinute.value_or(minute()));
    duration.setSeconds(optionalSecond.value_or(second()));
    duration.setMilliseconds(optionalMillisecond.value_or(millisecond()));
    duration.setMicroseconds(optionalMicrosecond.value_or(microsecond()));
    duration.setNanoseconds(optionalNanosecond.value_or(nanosecond()));
    auto plainTime = TemporalPlainTime::regulateTime(globalObject, static_cast<Int128>(optionalHour.value_or(hour())), static_cast<Int128>(optionalMinute.value_or(minute())), static_cast<Int128>(optionalSecond.value_or(second())), static_cast<Int128>(optionalMillisecond.value_or(millisecond())), static_cast<Int128>(optionalMicrosecond.value_or(microsecond())), static_cast<Int128>(optionalNanosecond.value_or(nanosecond())), overflow);
    RETURN_IF_EXCEPTION(scope, { });

    if (calendar()->identifier() != iso8601CalendarID()) {
        TemporalCalendar* cal = TemporalCalendar::create(vm, globalObject->calendarStructure(), calendar()->identifier());
        RELEASE_AND_RETURN(scope, TemporalPlainDateTime::tryCreateIfValid(globalObject, globalObject->plainDateTimeStructure(), WTF::move(plainDate), WTF::move(plainTime), cal));
    }
    RELEASE_AND_RETURN(scope, TemporalPlainDateTime::tryCreateIfValid(globalObject, globalObject->plainDateTimeStructure(), WTF::move(plainDate), WTF::move(plainTime), std::nullopt));
}

TemporalPlainDateTime* TemporalPlainDateTime::round(JSGlobalObject* globalObject, JSValue optionsValue)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    JSObject* options = nullptr;
    std::optional<TemporalUnit> smallest;
    if (optionsValue.isString()) {
        auto string = optionsValue.toWTFString(globalObject);
        RETURN_IF_EXCEPTION(scope, { });

        smallest = temporalUnitType(string);
        if (!smallest) {
            throwRangeError(globalObject, scope, "smallestUnit is an invalid Temporal unit"_s);
            return { };
        }

        if (smallest.value() <= TemporalUnit::Week) {
            throwRangeError(globalObject, scope, "smallestUnit is a disallowed unit"_s);
            return { };
        }
    } else {
        options = intlGetOptionsObject(globalObject, optionsValue);
        RETURN_IF_EXCEPTION(scope, { });
    }

    auto roundingIncrement = temporalRoundingIncrement(globalObject, options);
    RETURN_IF_EXCEPTION(scope, { });

    auto roundingMode = temporalRoundingMode(globalObject, options, RoundingMode::HalfExpand);
    RETURN_IF_EXCEPTION(scope, { });

    if (!smallest) {
        std::optional<String> smallestUnitString = temporalSmallestUnit(globalObject, options);
        RETURN_IF_EXCEPTION(scope, { });
        smallest = validateSmallestUnit(globalObject, smallestUnitString, { TemporalUnit::Year, TemporalUnit::Month, TemporalUnit::Week });
        if (!smallest) {
            throwRangeError(globalObject, scope, "Cannot round without a smallestUnit option"_s);
            return { };
        }
    }

    auto smallestUnit = smallest.value();
    unsigned maximum = 1;
    Inclusivity isInclusive = Inclusivity::Inclusive;
    if (smallestUnit != TemporalUnit::Day) {
        auto maximumOptional = maximumRoundingIncrement(smallestUnit);
        ASSERT(maximumOptional);
        maximum = maximumOptional.value();
        isInclusive = Inclusivity::Exclusive;
    }
    validateTemporalRoundingIncrement(globalObject, roundingIncrement, maximum, isInclusive);
    RETURN_IF_EXCEPTION(scope, { });

    auto duration = TemporalPlainTime::roundTime(m_plainTime, roundingIncrement, smallestUnit, roundingMode, std::nullopt);
    auto plainTime = TemporalPlainTime::toPlainTime(globalObject, duration);
    RETURN_IF_EXCEPTION(scope, { });

    double extraDays = duration.days();
    duration.setYears(year());
    duration.setMonths(month());
    duration.setDays(day());
    if (extraDays) {
        ASSERT(extraDays == 1);
        incrementDay(duration);
    }

    auto plainDate = TemporalPlainDate::toPlainDate(globalObject, duration);
    RETURN_IF_EXCEPTION(scope, { });

    if (calendar()->identifier() != iso8601CalendarID()) {
        TemporalCalendar* cal = TemporalCalendar::create(vm, globalObject->calendarStructure(), calendar()->identifier());
        RELEASE_AND_RETURN(scope, TemporalPlainDateTime::tryCreateIfValid(globalObject, globalObject->plainDateTimeStructure(), WTF::move(plainDate), WTF::move(plainTime), cal));
    }
    RELEASE_AND_RETURN(scope, TemporalPlainDateTime::tryCreateIfValid(globalObject, globalObject->plainDateTimeStructure(), WTF::move(plainDate), WTF::move(plainTime), std::nullopt));
}

// https://tc39.es/proposal-temporal/#sec-temporal-roundisodatetime
ISO8601::PlainDateTime TemporalPlainDateTime::roundISODateTime(JSGlobalObject* globalObject,
    ISO8601::PlainDateTime isoDateTime, unsigned increment, TemporalUnit unit, RoundingMode roundingMode)
{
    auto isoDate = isoDateTime.date();
    auto isoTime = isoDateTime.time();

    ASSERT(ISO8601::isDateTimeWithinLimits(isoDate.year(), isoDate.month(), isoDate.day(),
        isoTime.hour(), isoTime.minute(), isoTime.second(), isoTime.millisecond(),
        isoTime.microsecond(), isoTime.nanosecond()));
    auto roundedTime = TemporalPlainTime::roundTime(isoTime, increment, unit, roundingMode, std::nullopt);

    auto balanceResult = TemporalCalendar::balanceISODate(globalObject,
        isoDate.year(), isoDate.month(), isoDate.day() + roundedTime.days());
    return combineISODateAndTimeRecord(balanceResult,
        ISO8601::PlainTime(roundedTime.hours(), roundedTime.minutes(), roundedTime.seconds(),
            roundedTime.milliseconds(), roundedTime.microseconds(), roundedTime.nanoseconds()));
}

// https://tc39.es/proposal-temporal/#sec-temporal-balanceisodatetime
// The way this is currently called, only `nanosecond` needs to be an Int128; everything
// else can be an int32_t. But for consistency, everything is an Int128.
ISO8601::PlainDateTime TemporalPlainDateTime::balanceISODateTime(JSGlobalObject* globalObject, Int128 year, Int128 month, Int128 day, Int128 hour, Int128 minute, Int128 second, Int128 millisecond, Int128 microsecond, Int128 nanosecond)
{
    auto balancedTime = TemporalPlainTime::balanceTime(hour, minute, second, millisecond, microsecond, nanosecond);
    auto balancedDate = TemporalCalendar::balanceISODate(globalObject, static_cast<double>(year), static_cast<double>(month), static_cast<double>(day) + balancedTime.days());
    return ISO8601::PlainDateTime(WTF::move(balancedDate),
        ISO8601::PlainTime(static_cast<unsigned>(balancedTime.hours()), static_cast<unsigned>(balancedTime.minutes()), static_cast<unsigned>(balancedTime.seconds()), static_cast<unsigned>(balancedTime.milliseconds()), static_cast<unsigned>(balancedTime.microseconds()), static_cast<unsigned>(balancedTime.nanoseconds())));
}

static ISO8601::InternalDuration differenceISODateTime(JSGlobalObject* globalObject,
    const ISO8601::PlainDateTime& isoDateTime1, const ISO8601::PlainDateTime& isoDateTime2,
    TemporalUnit largestUnit)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    ASSERT(isoDateTimeWithinLimits(isoDateTime1));
    ASSERT(isoDateTimeWithinLimits(isoDateTime2));

    auto timeDuration = TemporalPlainTime::differenceTime(isoDateTime1.time(), isoDateTime2.time());
    auto timeSign = TemporalDuration::timeDurationSign(timeDuration);
    auto dateSign = TemporalCalendar::isoDateCompare(isoDateTime1.date(), isoDateTime2.date());
    auto adjustedDate = isoDateTime2.date();
    if (timeSign == dateSign) {
        adjustedDate = TemporalCalendar::balanceISODate(globalObject, static_cast<Int128>(adjustedDate.year()), static_cast<Int128>(adjustedDate.month()), static_cast<Int128>(adjustedDate.day()) + timeSign);
        RETURN_IF_EXCEPTION(scope, { });
        timeDuration = TemporalDuration::add24HourDaysToTimeDuration(globalObject, timeDuration, -timeSign);
        RETURN_IF_EXCEPTION(scope, { });
    }
    auto dateLargestUnit = largestUnit < TemporalUnit::Day ? largestUnit : TemporalUnit::Day;
    auto dateDifference = TemporalCalendar::calendarDateUntil(isoDateTime1.date(),
        adjustedDate, dateLargestUnit);
    if (largestUnit != dateLargestUnit) {
        timeDuration = TemporalDuration::add24HourDaysToTimeDuration(globalObject, timeDuration,
            dateDifference.days());
        RETURN_IF_EXCEPTION(scope, { });
        dateDifference.setDays(0);
    }
    return ISO8601::InternalDuration::combineDateAndTimeDuration(dateDifference, timeDuration);
}

// https://tc39.es/proposal-temporal/#sec-temporal-differenceplaindatetimewithtotal
double TemporalPlainDateTime::differencePlainDateTimeWithTotal(
    JSGlobalObject* globalObject, const ISO8601::PlainDateTime& isoDateTime1,
    const ISO8601::PlainDateTime& isoDateTime2, TemporalUnit unit)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (!TemporalCalendar::isoDateTimeCompare(isoDateTime1, isoDateTime2))
        return 0;

    if (!isoDateTimeWithinLimits(isoDateTime1) || !isoDateTimeWithinLimits(isoDateTime2)) {
        throwRangeError(globalObject, scope, "Date/time out of range in differencePlainDateTimeWithTotal"_s);
        return 0;
    }
    auto diff = differenceISODateTime(globalObject, isoDateTime1, isoDateTime2, unit);
    RETURN_IF_EXCEPTION(scope, 0);
    if (unit == TemporalUnit::Nanosecond)
        return diff.time();
    auto originEpochNs = getUTCEpochNanoseconds(isoDateTime1);
    auto destEpochNs = getUTCEpochNanoseconds(isoDateTime2);
    RELEASE_AND_RETURN(scope, TemporalDuration::totalRelativeDuration(globalObject,
        diff, originEpochNs, destEpochNs, isoDateTime1, std::nullopt, unit));
}

// https://tc39.es/proposal-temporal/#sec-temporal-differenceplaindatetimewithrounding
ISO8601::InternalDuration TemporalPlainDateTime::differencePlainDateTimeWithRounding(
    JSGlobalObject* globalObject, const ISO8601::PlainDateTime& isoDateTime1,
    const ISO8601::PlainDateTime& isoDateTime2, TemporalUnit largestUnit,
    double roundingIncrement, TemporalUnit smallestUnit, RoundingMode roundingMode)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (!TemporalCalendar::isoDateTimeCompare(isoDateTime1, isoDateTime2))
        return ISO8601::InternalDuration::combineDateAndTimeDuration(ISO8601::Duration(), 0);
    if (!isoDateTimeWithinLimits(isoDateTime1) || !isoDateTimeWithinLimits(isoDateTime2)) {
        throwRangeError(globalObject, scope, "Date/time out of range in differencePlainDateTimeWithRounding"_s);
        return { };
    }
    auto diff = differenceISODateTime(globalObject, isoDateTime1, isoDateTime2, largestUnit);
    RETURN_IF_EXCEPTION(scope, { });
    if (smallestUnit == TemporalUnit::Nanosecond && roundingIncrement == 1)
        return diff;
    auto originEpochNs = getUTCEpochNanoseconds(isoDateTime1);
    auto destEpochNs = getUTCEpochNanoseconds(isoDateTime2);
    RELEASE_AND_RETURN(scope, TemporalDuration::roundRelativeDuration(globalObject,
        diff, originEpochNs, destEpochNs, isoDateTime1, std::nullopt,
        largestUnit, roundingIncrement, smallestUnit, roundingMode));
}

// https://tc39.es/proposal-temporal/#sec-temporal-differencetemporalplaindatetime
ISO8601::Duration TemporalPlainDateTime::differenceTemporalPlainDateTime(JSGlobalObject* globalObject,
    DifferenceOperation op, TemporalPlainDateTime* other, TemporalUnit smallestUnit, TemporalUnit largestUnit,
    RoundingMode roundingMode, double increment)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto plainDateTimeThis = ISO8601::PlainDateTime(plainDate(), plainTime());
    auto plainDateTimeOther = ISO8601::PlainDateTime(other->plainDate(), other->plainTime());

    if (!TemporalCalendar::isoDateTimeCompare(plainDateTimeThis, plainDateTimeOther))
        return ISO8601::Duration();

    auto internalDuration = differencePlainDateTimeWithRounding(globalObject, plainDateTimeThis,
        plainDateTimeOther, largestUnit, increment, smallestUnit, roundingMode);
    RETURN_IF_EXCEPTION(scope, { });

    auto result = TemporalDuration::temporalDurationFromInternal(globalObject, internalDuration, largestUnit);
    RETURN_IF_EXCEPTION(scope, { });
    if (op == DifferenceOperation::Since)
        result = -result;
    return result;
}

} // namespace JSC
