/*
 * Copyright (C) 2022 Apple Inc. All rights reserved.
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
#include "TemporalPlainDate.h"

#include "DateConstructor.h"
#include "IntlObjectInlines.h"
#include "JSCInlines.h"
#include "LazyPropertyInlines.h"
#include "TemporalDuration.h"
#include "TemporalObjectInlines.h"
#include "TemporalPlainDateTime.h"
#include "TemporalTimeZone.h"
#include "TemporalZonedDateTime.h"
#include "VMTrapsInlines.h"

namespace JSC {

const ClassInfo TemporalPlainDate::s_info = { "Object"_s, &Base::s_info, nullptr, nullptr, CREATE_METHOD_TABLE(TemporalPlainDate) };

TemporalPlainDate* TemporalPlainDate::create(VM& vm, Structure* structure, ISO8601::PlainDate&& plainDate, TemporalCalendar* calendar, std::optional<String> era, std::optional<double> eraYear)
{
    auto* object = new (NotNull, allocateCell<TemporalPlainDate>(vm)) TemporalPlainDate(vm, structure, WTFMove(plainDate), calendar, era, eraYear);
    object->finishCreation(vm, true);
    return object;
}

TemporalPlainDate* TemporalPlainDate::create(VM& vm, Structure* structure, ISO8601::PlainDate&& plainDate, CalendarID calendar, std::optional<String> era, std::optional<double> eraYear)
{
    auto* object = new (NotNull, allocateCell<TemporalPlainDate>(vm)) TemporalPlainDate(vm, structure, WTFMove(plainDate), calendar, era, eraYear);
    object->finishCreation(vm, false);
    return object;
}

Structure* TemporalPlainDate::createStructure(VM& vm, JSGlobalObject* globalObject, JSValue prototype)
{
    return Structure::create(vm, globalObject, prototype, TypeInfo(ObjectType, StructureFlags), info());
}

TemporalPlainDate::TemporalPlainDate(VM& vm, Structure* structure, ISO8601::PlainDate&& plainDate, TemporalCalendar* calendar, std::optional<String> era, std::optional<double> eraYear)
    : Base(vm, structure)
    , m_plainDate(WTFMove(plainDate))
    , m_era(era)
    , m_eraYear(eraYear)
{
    ASSERT(calendar);
    m_calendarId = calendar->identifier();
    m_calendar.set(vm, this, calendar);

    if (era || eraYear)
        ASSERT(calendar->hasEras());
}

TemporalPlainDate::TemporalPlainDate(VM& vm, Structure* structure, ISO8601::PlainDate&& plainDate, CalendarID calendarId, std::optional<String> era, std::optional<double> eraYear)
    : Base(vm, structure)
    , m_plainDate(WTFMove(plainDate))
    , m_calendarId(calendarId)
    , m_era(era)
    , m_eraYear(eraYear)
{
    if (era || eraYear)
        ASSERT(TemporalCalendar::hasEras(m_calendarId));
}

void TemporalPlainDate::finishCreation(VM& vm, bool calendarAlreadyInitialized)
{
    Base::finishCreation(vm);
    ASSERT(inherits(info()));
    if (!calendarAlreadyInitialized) {
        m_calendar.initLater(
            [] (const auto& init) {
                VM& vm = init.vm;
                auto* plainDate = jsCast<TemporalPlainDate*>(init.owner);
                auto* globalObject = plainDate->globalObject();
                auto* calendar = TemporalCalendar::create(vm, globalObject->calendarStructure(), plainDate->m_calendarId);
                init.set(calendar);
            });
    }
}

template<typename Visitor>
void TemporalPlainDate::visitChildrenImpl(JSCell* cell, Visitor& visitor)
{
    Base::visitChildren(cell, visitor);

    auto* thisObject = jsCast<TemporalPlainDate*>(cell);
    thisObject->m_calendar.visit(visitor);
}

DEFINE_VISIT_CHILDREN(TemporalPlainDate);

ISO8601::PlainDate TemporalPlainDate::toPlainDate(JSGlobalObject* globalObject,
    const ISO8601::Duration& duration)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    double yearDouble = duration.years();
    double monthDouble = duration.months();
    double dayDouble = duration.days();

    if (!ISO8601::isYearWithinLimits(yearDouble)) {
        throwRangeError(globalObject, scope, "year is out of range"_s);
        return { };
    }
    int32_t year = static_cast<int32_t>(yearDouble);

    if (!(monthDouble >= 1 && monthDouble <= 12)) {
        throwRangeError(globalObject, scope, "month is out of range"_s);
        return { };
    }
    unsigned month = static_cast<unsigned>(monthDouble);

    double daysInMonth = ISO8601::daysInMonth(year, month);
    if (!(dayDouble >= 1 && dayDouble <= daysInMonth)) {
        throwRangeError(globalObject, scope, "day is out of range"_s);
        return { };
    }
    unsigned day = static_cast<unsigned>(dayDouble);

    return ISO8601::PlainDate {
        year,
        month,
        day
    };
}

// CreateTemporalDate ( years, months, days )
// https://tc39.es/proposal-temporal/#sec-temporal-createtemporaldate
TemporalPlainDate* TemporalPlainDate::tryCreateIfValid(JSGlobalObject* globalObject, Structure* structure, ISO8601::PlainDate&& plainDate, CalendarID calendarId, std::optional<String> era, std::optional<double> eraYear)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (!ISO8601::isDateTimeWithinLimits(plainDate.year(), plainDate.month(), plainDate.day(), 12, 0, 0, 0, 0, 0)) {
        throwRangeError(globalObject, scope, "date time is out of range of ECMAScript representation"_s);
        return { };
    }

    return TemporalPlainDate::create(vm, structure, WTFMove(plainDate), calendarId, era, eraYear);
}

// CreateTemporalDate ( years, months, days )
// https://tc39.es/proposal-temporal/#sec-temporal-createtemporaldate
TemporalPlainDate* TemporalPlainDate::tryCreateIfValid(JSGlobalObject* globalObject, Structure* structure, ISO8601::PlainDate&& plainDate, TemporalCalendar* calendar, std::optional<String> era, std::optional<double> eraYear)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (!ISO8601::isDateTimeWithinLimits(plainDate.year(), plainDate.month(), plainDate.day(), 12, 0, 0, 0, 0, 0)) {
        throwRangeError(globalObject, scope, "date time is out of range of ECMAScript representation"_s);
        return { };
    }

    return TemporalPlainDate::create(vm, structure, WTFMove(plainDate), calendar, era, eraYear);
}

TemporalPlainDate* TemporalPlainDate::tryCreateIfValid(JSGlobalObject* globalObject, Structure* structure, ISO8601::Duration&& duration, TemporalCalendar* calendar, std::optional<String> era, std::optional<double> eraYear)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto plainDate = toPlainDate(globalObject, duration);
    RETURN_IF_EXCEPTION(scope, { });

    RELEASE_AND_RETURN(scope, TemporalPlainDate::tryCreateIfValid(globalObject, structure,  WTFMove(plainDate), calendar, era, eraYear));
}

TemporalPlainDate* TemporalPlainDate::tryCreateIfValid(JSGlobalObject* globalObject, Structure* structure, ISO8601::Duration&& duration, CalendarID calendar, std::optional<String> era, std::optional<double> eraYear)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto plainDate = toPlainDate(globalObject, duration);
    RETURN_IF_EXCEPTION(scope, { });

    RELEASE_AND_RETURN(scope, TemporalPlainDate::tryCreateIfValid(globalObject, structure,  WTFMove(plainDate), calendar, era, eraYear));
}

static String temporalDateToString(JSGlobalObject* globalObject, ISO8601::PlainDate plainDate,
    JSObject* calendar, TemporalShowCalendar showCalendar)
{
    WTF::String calendarString = TemporalCalendar::formatCalendarAnnotation(globalObject, calendar,
        showCalendar);
    auto dateString = temporalDateToString(plainDate);
    return makeString(dateString, calendarString);
}

String TemporalPlainDate::toString(JSGlobalObject* globalObject, JSValue optionsValue) const
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto showCalendar = getTemporalShowCalendarNameOption(globalObject, optionsValue);
    RETURN_IF_EXCEPTION(scope, { });

    RELEASE_AND_RETURN(scope, temporalDateToString(globalObject, m_plainDate,
        calendar(), showCalendar));
}

// https://tc39.es/proposal-temporal/#sec-temporal-totemporaldate
TemporalPlainDate* TemporalPlainDate::from(JSGlobalObject* globalObject, JSValue itemValue, std::optional<JSValue> optionsValue)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (itemValue.isObject()) {
        if (itemValue.inherits<TemporalPlainDate>()) {
            if (optionsValue) {
                // Validate overflow
                toTemporalOverflow(globalObject, optionsValue.value());
                RETURN_IF_EXCEPTION(scope, { });
            }
            return jsCast<TemporalPlainDate*>(itemValue);
        }

        if (itemValue.inherits<TemporalZonedDateTime>()) {
            auto zdt = jsCast<TemporalZonedDateTime*>(itemValue);
            auto isoDateTime = TemporalTimeZone::getISODateTimeFor(globalObject,
                zdt->timeZone(), zdt->exactTime());
            RETURN_IF_EXCEPTION(scope, { });
            if (optionsValue) {
                toTemporalOverflow(globalObject, optionsValue.value());
                RETURN_IF_EXCEPTION(scope, { });
            }
            return TemporalPlainDate::create(vm, globalObject->plainDateStructure(), isoDateTime.date(),
                zdt->calendar(), std::nullopt, std::nullopt);
        }

        if (itemValue.inherits<TemporalPlainDateTime>()) {
            if (optionsValue) {
                toTemporalOverflow(globalObject, optionsValue.value());
                RETURN_IF_EXCEPTION(scope, { });
            }
            auto pdt = jsCast<TemporalPlainDateTime*>(itemValue);
            return TemporalPlainDate::create(vm, globalObject->plainDateStructure(), pdt->plainDate(),
                pdt->calendar(), std::nullopt, std::nullopt);
        }

        auto calendar = TemporalCalendar::getTemporalCalendarWithISODefault(globalObject, itemValue);
        RETURN_IF_EXCEPTION(scope, { });
        Vector<FieldName> fieldList({ FieldName::Day, FieldName::Month, FieldName::MonthCode, FieldName::Year });
        auto calendarID = std::holds_alternative<TemporalCalendar*>(calendar)
            ? std::get<TemporalCalendar*>(calendar)->identifier()
            : std::get<CalendarID>(calendar);
        auto fields = TemporalCalendar::prepareCalendarFields(globalObject, calendarID, asObject(itemValue),
            fieldList, { });
        RETURN_IF_EXCEPTION(scope, { });

        auto overflow = TemporalOverflow::Constrain;
        if (optionsValue) {
            auto options = intlGetOptionsObject(globalObject, optionsValue.value());
            RETURN_IF_EXCEPTION(scope, { });
            overflow = toTemporalOverflow(globalObject, options);
            RETURN_IF_EXCEPTION(scope, { });
        }

        auto isoDate = TemporalCalendar::calendarDateFromFields(globalObject, calendarID, fields, overflow);
        RETURN_IF_EXCEPTION(scope, { });

        if (std::holds_alternative<TemporalCalendar*>(calendar)) {
            return TemporalPlainDate::create(vm, globalObject->plainDateStructure(), WTFMove(isoDate),
                 std::get<TemporalCalendar*>(calendar), fields.era, fields.eraYear);
        }
        return TemporalPlainDate::create(vm, globalObject->plainDateStructure(), WTFMove(isoDate),
            calendarID, fields.era, fields.eraYear);
    }

    if (!itemValue.isString()) {
        throwTypeError(globalObject, scope, "can only convert to PlainDate from object or string values"_s);
        return { };
    }

    auto string = itemValue.toWTFString(globalObject);
    RETURN_IF_EXCEPTION(scope, { });

    // https://tc39.es/proposal-temporal/#sec-temporal-parsetemporaldatestring
    // TemporalDateString :
    //     CalendarDateTime
    auto dateTime = ISO8601::parseCalendarDateTime(string, TemporalDateFormat::Date);
    if (dateTime) {
        if (optionsValue) {
            toTemporalOverflow(globalObject, optionsValue.value());
            RETURN_IF_EXCEPTION(scope, { });
        }
        auto [plainDate, plainTimeOptional, timeZoneOptional, calendarOptional] = WTFMove(dateTime.value());
        auto calendar = iso8601CalendarID();
        if (calendarOptional) {
            calendar = TemporalCalendar::canonicalizeCalendar(globalObject, StringView(calendarOptional.value()));
            RETURN_IF_EXCEPTION(scope, { });
        }
        if (!(timeZoneOptional && timeZoneOptional->m_z)) {
                RELEASE_AND_RETURN(scope, TemporalPlainDate::tryCreateIfValid(globalObject,
                    globalObject->plainDateStructure(), WTFMove(plainDate), calendar, std::nullopt, std::nullopt));
        }
    }

    throwRangeError(globalObject, scope, "invalid date string"_s);
    return { };
}

std::array<std::optional<double>, numberOfTemporalPlainYearMonthUnits> TemporalPlainDate::toYearMonth(JSGlobalObject* globalObject, JSObject* temporalDateLike)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    std::optional<double> month;
    JSValue monthProperty = temporalDateLike->get(globalObject, vm.propertyNames->month);
    RETURN_IF_EXCEPTION(scope, { });
    if (!monthProperty.isUndefined()) {
        month = monthProperty.toIntegerOrInfinity(globalObject);
        RETURN_IF_EXCEPTION(scope, { });

        if (month.value() <= 0 || !std::isfinite(month.value())) {
            throwRangeError(globalObject, scope, "month property must be positive and finite"_s);
            return { };
        }
    }

    JSValue monthCodeProperty = temporalDateLike->get(globalObject, vm.propertyNames->monthCode);
    RETURN_IF_EXCEPTION(scope, { });
    if (!monthCodeProperty.isUndefined()) {
        auto monthCode = monthCodeProperty.toWTFString(globalObject);
        RETURN_IF_EXCEPTION(scope, { });

        auto otherMonth = ISO8601::monthFromCode(monthCode);
        if (!otherMonth) {
            throwRangeError(globalObject, scope, "Invalid monthCode property"_s);
            return { };
        }

        if (!month)
            month = otherMonth;
        else if (month.value() != otherMonth) {
            throwRangeError(globalObject, scope, "month and monthCode properties must match if both are provided"_s);
            return { };
        }
    }

    std::optional<double> year;
    JSValue yearProperty = temporalDateLike->get(globalObject, vm.propertyNames->year);
    RETURN_IF_EXCEPTION(scope, { });
    if (!yearProperty.isUndefined()) {
        year = yearProperty.toIntegerOrInfinity(globalObject);
        RETURN_IF_EXCEPTION(scope, { });

        if (!std::isfinite(year.value())) {
            throwRangeError(globalObject, scope, "year property must be finite"_s);
            return { };
        }
    }

    return { year, month };
}

std::array<std::optional<double>, numberOfTemporalPlainDateUnits> TemporalPlainDate::toPartialDate(JSGlobalObject* globalObject, JSObject* temporalDateLike)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    std::optional<double> day;
    JSValue dayProperty = temporalDateLike->get(globalObject, vm.propertyNames->day);
    RETURN_IF_EXCEPTION(scope, { });
    if (!dayProperty.isUndefined()) {
        day = dayProperty.toIntegerOrInfinity(globalObject);
        RETURN_IF_EXCEPTION(scope, { });

        if (day.value() <= 0 || !std::isfinite(day.value())) {
            throwRangeError(globalObject, scope, "day property must be positive and finite"_s);
            return { };
        }
    }

    auto [year, month] = toYearMonth(globalObject, temporalDateLike);
    RETURN_IF_EXCEPTION(scope, { });

    return { year, month, day };
}

std::tuple<ISO8601::PlainDate, std::optional<String>, std::optional<double>>
TemporalPlainDate::with(JSGlobalObject* globalObject, JSObject* temporalDateLike, JSValue optionsValue)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto thisCalendar = calendar();

    if (!TemporalCalendar::isISO8601(thisCalendar) && (thisCalendar->identifier() != gregoryCalendarID())) {
        throwRangeError(globalObject, scope, "calendar not implemented yet in TemporalPlainDate.with"_s);
        return { };
    }

    if (!isPartialTemporalObject(globalObject, temporalDateLike)) {
        RETURN_IF_EXCEPTION(scope, { });
        throwTypeError(globalObject, scope, "bad argument in TemporalPlainDate.with"_s);
        return { };
    }
    RETURN_IF_EXCEPTION(scope, { });

    auto calendarID = thisCalendar->identifier();
    auto fields = thisCalendar->isoDateToFields(globalObject, plainDate(),
        TemporalDateFormat::Date);
    RETURN_IF_EXCEPTION(scope, { });
    Vector<FieldName> fieldList({ FieldName::Day, FieldName::Month, FieldName::MonthCode, FieldName::Year });
    auto partialDate = TemporalCalendar::prepareCalendarFields(globalObject, calendarID,
        temporalDateLike, fieldList, { });
    RETURN_IF_EXCEPTION(scope, { });

    fields = TemporalCalendar::calendarMergeFields(calendarID, fields, partialDate);

    TemporalOverflow overflow = toTemporalOverflow(globalObject, optionsValue);
    RETURN_IF_EXCEPTION(scope, { });

    auto isoDate = TemporalCalendar::calendarDateFromFields(globalObject, calendarID, fields, overflow);
    RETURN_IF_EXCEPTION(scope, { });

    RELEASE_AND_RETURN(scope, std::tuple(isoDate, fields.era, fields.eraYear));
}

// https://tc39.es/proposal-temporal/#sec-temporal-differencetemporalplaindate
ISO8601::Duration TemporalPlainDate::differenceTemporalPlainDate(JSGlobalObject* globalObject, bool isSince, TemporalPlainDate* other, TemporalUnit smallestUnit, TemporalUnit largestUnit, RoundingMode roundingMode, double increment)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (!TemporalCalendar::isoDateCompare(plainDate(), other->plainDate()))
        return ISO8601::Duration();
    ISO8601::Duration dateDifference = TemporalCalendar::calendarDateUntil(plainDate(), other->plainDate(), largestUnit);
    ISO8601::InternalDuration duration = ISO8601::InternalDuration::combineDateAndTimeDuration(dateDifference, 0);
    if (smallestUnit != TemporalUnit::Day || increment != 1) {
        auto isoDateTime = TemporalPlainDateTime::combineISODateAndTimeRecord(plainDate(), ISO8601::PlainTime());
        auto isoDateTimeOther = TemporalPlainDateTime::combineISODateAndTimeRecord(other->plainDate(), ISO8601::PlainTime());
        Int128 destEpochNs = ISO8601::getUTCEpochNanoseconds(isoDateTimeOther);
        TemporalDuration::roundRelativeDuration(
            globalObject, duration, destEpochNs, isoDateTime, std::nullopt, largestUnit,
            increment, smallestUnit, roundingMode);
        RETURN_IF_EXCEPTION(scope, { });
    }
    auto result = TemporalDuration::temporalDurationFromInternal(globalObject, duration, TemporalUnit::Day);
    RETURN_IF_EXCEPTION(scope, { });
    if (isSince)
        result = -result;
    return result;
}

ISO8601::Duration TemporalPlainDate::until(JSGlobalObject* globalObject, TemporalPlainDate* other, JSValue optionsValue)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    bool calendarsMatch = TemporalCalendar::equals(globalObject, calendar(), other->calendar());
    RETURN_IF_EXCEPTION(scope, { });
    if (!calendarsMatch) {
        throwRangeError(globalObject, scope, "calendars must match"_s);
        return { };
    }

    if (!TemporalCalendar::isISO8601(calendar())) {
        throwRangeError(globalObject, scope, "unimplemented: with non-ISO8601 calendar"_s);
        return { };
    }

    auto [smallestUnit, largestUnit, roundingMode, increment] = extractDifferenceOptions(globalObject, optionsValue, UnitGroup::Date, TemporalUnit::Day, TemporalUnit::Day);
    RETURN_IF_EXCEPTION(scope, { });

    RELEASE_AND_RETURN(scope, differenceTemporalPlainDate(globalObject, false, other, smallestUnit, largestUnit, roundingMode, increment));
}

ISO8601::Duration TemporalPlainDate::since(JSGlobalObject* globalObject, TemporalPlainDate* other, JSValue optionsValue)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    bool calendarsMatch = TemporalCalendar::equals(globalObject, calendar(), other->calendar());
    RETURN_IF_EXCEPTION(scope, { });
    if (!calendarsMatch) {
        throwRangeError(globalObject, scope, "calendars must match"_s);
        return { };
    }

    if (!TemporalCalendar::isISO8601(calendar())) {
        throwRangeError(globalObject, scope, "unimplemented: with non-ISO8601 calendar"_s);
        return { };
    }

    auto [smallestUnit, largestUnit, roundingMode, increment] = extractDifferenceOptions(globalObject, optionsValue, UnitGroup::Date, TemporalUnit::Day, TemporalUnit::Day);
    RETURN_IF_EXCEPTION(scope, { });
    roundingMode = negateTemporalRoundingMode(roundingMode);

    RELEASE_AND_RETURN(scope, differenceTemporalPlainDate(globalObject, true, other, smallestUnit, largestUnit, roundingMode, increment));
}

// https://tc39.es/proposal-temporal/#sec-temporal-create-iso-date-record
ISO8601::PlainDate TemporalPlainDate::createISODateRecord(double year, double month, double day)
{
    ASSERT(isValidISODate(year, month, day));
    return ISO8601::PlainDate(year, month, day);
}

// https://tc39.es/proposal-temporal/#sec-temporal-isvalidisodate
bool TemporalPlainDate::isValidISODate(double year, double month, double day)
{
    if (month < 1 || month > 12)
        return false;
    auto daysInMonth1 = ISO8601::daysInMonth(year, month);
    if (day < 1 || day > daysInMonth1)
        return false;
    return true;
}

// https://tc39.es/proposal-temporal/#sec-temporal-regulateisodate
std::optional<ISO8601::PlainDate> TemporalPlainDate::regulateISODate(double year, double month, double day,
    TemporalOverflow overflow)
{
    if (overflow == TemporalOverflow::Constrain) {
        if (month < 1)
            month = 1;
        if (month > 12)
            month = 12;
        auto daysInMonth = ISO8601::daysInMonth(year, month);
        if (day < 1)
            day = 1;
        if (day > daysInMonth)
            day = daysInMonth;
    } else if (!isValidISODate(year, month, day))
        return std::nullopt;
    return createISODateRecord(year, month, day);
}

String TemporalPlainDate::monthCode() const
{
    return ISO8601::monthCode(m_plainDate.month());
}

uint8_t TemporalPlainDate::dayOfWeek() const
{
    return ISO8601::dayOfWeek(m_plainDate);
}

uint16_t TemporalPlainDate::dayOfYear() const
{
    return ISO8601::dayOfYear(m_plainDate);
}

uint8_t TemporalPlainDate::weekOfYear() const
{
    return ISO8601::weekOfYear(m_plainDate);
}

} // namespace JSC
