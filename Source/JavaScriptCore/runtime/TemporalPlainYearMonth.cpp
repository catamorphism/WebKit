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
#include "TemporalPlainYearMonth.h"

#include "IntlObjectInlines.h"
#include "JSCInlines.h"
#include "LazyPropertyInlines.h"
#include "TemporalDuration.h"
#include "TemporalPlainDateTime.h"
#include "VMTrapsInlines.h"

namespace JSC {

const ClassInfo TemporalPlainYearMonth::s_info = { "Object"_s, &Base::s_info, nullptr, nullptr, CREATE_METHOD_TABLE(TemporalPlainYearMonth) };

TemporalPlainYearMonth* TemporalPlainYearMonth::create(VM& vm, Structure* structure, ISO8601::PlainYearMonth&& plainYearMonth, JSObject* calendar)
{
    auto* object = new (NotNull, allocateCell<TemporalPlainYearMonth>(vm)) TemporalPlainYearMonth(vm, structure, WTFMove(plainYearMonth), calendar);
    object->finishCreation(vm);
    return object;
}

TemporalPlainYearMonth* TemporalPlainYearMonth::create(VM& vm, Structure* structure, ISO8601::PlainYearMonth&& plainYearMonth, CalendarID calendarId)
{
    auto* object = new (NotNull, allocateCell<TemporalPlainYearMonth>(vm)) TemporalPlainYearMonth(vm, structure, WTFMove(plainYearMonth), calendarId);
    object->finishCreation(vm);
    return object;
}

Structure* TemporalPlainYearMonth::createStructure(VM& vm, JSGlobalObject* globalObject, JSValue prototype)
{
    return Structure::create(vm, globalObject, prototype, TypeInfo(ObjectType, StructureFlags), info());
}

TemporalPlainYearMonth::TemporalPlainYearMonth(VM& vm, Structure* structure, ISO8601::PlainYearMonth&& plainYearMonth, JSObject* calendar)
    : Base(vm, structure)
    , m_plainYearMonth(WTFMove(plainYearMonth))
    , m_customCalendar(calendar)
{
}

TemporalPlainYearMonth::TemporalPlainYearMonth(VM& vm, Structure* structure, ISO8601::PlainYearMonth&& plainYearMonth, CalendarID calendarId)
    : Base(vm, structure)
    , m_plainYearMonth(WTFMove(plainYearMonth))
    , m_builtInCalendarId(calendarId)
{
}

void TemporalPlainYearMonth::finishCreation(VM& vm)
{
    Base::finishCreation(vm);
    ASSERT(inherits(info()));
    if (m_builtInCalendarId) {
        m_builtInCalendar.initLater(
            [] (const auto& init) {
                VM& vm = init.vm;
                auto* plainYearMonth = jsCast<TemporalPlainYearMonth*>(init.owner);
                auto* globalObject = plainYearMonth->globalObject();
                ASSERT(plainYearMonth->m_builtInCalendarId);
                auto* calendar = TemporalCalendar::create(vm, globalObject->calendarStructure(), plainYearMonth->m_builtInCalendarId.value());
                init.set(calendar);
        });
    }
}

template<typename Visitor>
void TemporalPlainYearMonth::visitChildrenImpl(JSCell* cell, Visitor& visitor)
{
    Base::visitChildren(cell, visitor);

    auto* thisObject = jsCast<TemporalPlainYearMonth*>(cell);
    if (thisObject->m_customCalendar)
        Base::visitChildren(static_cast<JSCell*>(thisObject->m_customCalendar.value()), visitor);
    thisObject->m_builtInCalendar.visit(visitor);
}

DEFINE_VISIT_CHILDREN(TemporalPlainYearMonth);

// CreateTemporalYearMonth ( isoDate, calendar [, newTarget ] )
// https://tc39.es/proposal-temporal/#sec-temporal-createtemporalyearmonth
TemporalPlainYearMonth* TemporalPlainYearMonth::tryCreateIfValid(JSGlobalObject* globalObject, Structure* structure, ISO8601::PlainDate&& plainDate, JSObject* calendar)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (!ISO8601::isYearMonthWithinLimits(plainDate.year(), plainDate.month())) {
        throwRangeError(globalObject, scope, "PlainYearMonth is out of range of ECMAScript representation"_s);
        return { };
    }

    return TemporalPlainYearMonth::create(vm, structure, ISO8601::PlainYearMonth(WTFMove(plainDate)), calendar);
}

// CreateTemporalYearMonth ( isoDate, calendar [, newTarget ] )
// https://tc39.es/proposal-temporal/#sec-temporal-createtemporalyearmonth
TemporalPlainYearMonth* TemporalPlainYearMonth::tryCreateIfValid(JSGlobalObject* globalObject, Structure* structure, ISO8601::PlainDate&& plainDate, CalendarID calendarId)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (!ISO8601::isYearMonthWithinLimits(plainDate.year(), plainDate.month())) {
        throwRangeError(globalObject, scope, "PlainYearMonth is out of range of ECMAScript representation"_s);
        return { };
    }

    return TemporalPlainYearMonth::create(vm, structure, ISO8601::PlainYearMonth(WTFMove(plainDate)), calendarId);
}

static String temporalYearMonthToString(JSGlobalObject* globalObject, ISO8601::PlainYearMonth plainYearMonth,
    JSObject* calendar, TemporalShowCalendar showCalendar)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    WTF::String calendarString = TemporalCalendar::formatCalendarAnnotation(globalObject, calendar,
        showCalendar);
    RETURN_IF_EXCEPTION(scope, { });
    String dateString;
    switch (showCalendar) {
    case TemporalShowCalendar::Always:
    case TemporalShowCalendar::Critical:
         dateString = ISO8601::temporalDateToString(plainYearMonth.isoPlainDate());
         break;
    default: {
          JSString* jsString = calendar->toString(globalObject);
          RETURN_IF_EXCEPTION(scope, { });
          StringView calendar = jsString->view(globalObject);
          RETURN_IF_EXCEPTION(scope, { });
          if (calendar != "iso8601"_s)
              dateString = ISO8601::temporalDateToString(plainYearMonth.isoPlainDate());
          else
              dateString = ISO8601::temporalYearMonthToString(plainYearMonth);
          break;
    }
    }
    return makeString(dateString, calendarString);
}

String TemporalPlainYearMonth::toString(JSGlobalObject* globalObject, JSValue optionsValue) const
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    JSObject* options = intlGetOptionsObject(globalObject, optionsValue);
    RETURN_IF_EXCEPTION(scope, { });

    if (!options)
        return toString();

    auto showCalendar = getTemporalShowCalendarNameOption(globalObject, options);
    RETURN_IF_EXCEPTION(scope, { });

    RELEASE_AND_RETURN(scope, temporalYearMonthToString(globalObject, m_plainYearMonth,
        calendar(), showCalendar));
}

// https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.from
// https://tc39.es/proposal-temporal/#sec-temporal-totemporalyearmonth
TemporalPlainYearMonth* TemporalPlainYearMonth::from(JSGlobalObject* globalObject, JSValue itemValue, std::optional<JSValue> optionsValue)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    // Handle string case first so that string parsing errors (RangeError)
    // can be thrown before options-related errors (TypeError);
    // see step 4 of ToTemporalYearMonth
    TemporalPlainYearMonth* result;
    bool isString = itemValue.isString();

    if (isString) {
        auto string = itemValue.toWTFString(globalObject);
        RETURN_IF_EXCEPTION(scope, { });
        result = TemporalPlainYearMonth::from(globalObject, string);
        RETURN_IF_EXCEPTION(scope, { });
        // See step 11 of ToTemporalYearMonth
        if (optionsValue)
            toTemporalOverflow(globalObject, optionsValue.value());
        RETURN_IF_EXCEPTION(scope, { });
        RELEASE_AND_RETURN(scope, result);
    }

    std::optional<JSObject*> options;
    if (optionsValue) {
        options = intlGetOptionsObject(globalObject, optionsValue.value());
        RETURN_IF_EXCEPTION(scope, { });
    }

    if (isString) {
        // See step 11 of ToTemporalYearMonth
        // Overflow has to be validated even though it's not used,
        // so that an error can be thrown for a bad overflow option)
        if (options) {
            toTemporalOverflow(globalObject, options.value());
            RETURN_IF_EXCEPTION(scope, { });
        }
        RELEASE_AND_RETURN(scope, { });
    }

    if (itemValue.isObject()) {

        if (itemValue.inherits<TemporalPlainYearMonth>())
            return jsCast<TemporalPlainYearMonth*>(itemValue);

        JSObject* calendar = TemporalCalendar::getTemporalCalendarWithISODefault(globalObject, itemValue); 

        std::variant<JSObject*, TemporalOverflow> optionsOrOverflow = TemporalOverflow::Constrain;
        if (options)
            optionsOrOverflow = options.value();
        auto overflow = TemporalOverflow::Constrain;
        auto plainYearMonth = TemporalCalendar::isoDateFromFields(globalObject, asObject(itemValue), TemporalDateFormat::YearMonth, optionsOrOverflow, overflow);
        RETURN_IF_EXCEPTION(scope, { });

        return TemporalPlainYearMonth::create(vm, globalObject->plainYearMonthStructure(), WTFMove(plainYearMonth), calendar);
    }

    throwTypeError(globalObject, scope, "can only convert to PlainYearMonth from object or string values"_s);
    return { };
}

// https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.from
TemporalPlainYearMonth* TemporalPlainYearMonth::from(JSGlobalObject* globalObject, WTF::String string)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    // https://tc39.es/proposal-temporal/#sec-temporal-parsetemporaldatestring
    // TemporalDateString :
    //     CalendarDateTime
    auto dateTime = ISO8601::parseCalendarDateTime(string, TemporalDateFormat::YearMonth);
    if (dateTime) {
        auto [plainDate, plainTimeOptional, timeZoneOptional, calendarOptional] = WTFMove(dateTime.value());
        if (calendarOptional && StringView(calendarOptional.value()) != String::fromLatin1("iso8601")) {
            throwRangeError(globalObject, scope,
                "YYYY-MM format is only valid with iso8601 calendar"_s);
            return { };
        }
        if (!(timeZoneOptional && timeZoneOptional->m_z))
            RELEASE_AND_RETURN(scope, TemporalPlainYearMonth::tryCreateIfValid(globalObject, globalObject->plainYearMonthStructure(), WTFMove(plainDate), iso8601CalendarID()));
    }

    throwRangeError(globalObject, scope,
        makeString("Temporal.PlainYearMonth.from: invalid date string "_s, string));
    return { };
}

ISO8601::PlainDate TemporalPlainYearMonth::with(JSGlobalObject* globalObject, JSObject* temporalYearMonthLike, JSValue optionsValue)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    rejectObjectWithCalendarOrTimeZone(globalObject, temporalYearMonthLike);
    RETURN_IF_EXCEPTION(scope, { });

    if (!TemporalCalendar::isISO8601(calendar())) {
        throwRangeError(globalObject, scope, "unimplemented: with non-ISO8601 calendar"_s);
        return { };
    }

    auto [optionalYear, optionalMonth] = TemporalPlainDate::toYearMonth(globalObject, temporalYearMonthLike);
    RETURN_IF_EXCEPTION(scope, { });
    if (!optionalYear && !optionalMonth) {
        throwTypeError(globalObject, scope, "Object must contain at least one Temporal date property"_s);
        return { };
    }

    TemporalOverflow overflow = toTemporalOverflow(globalObject, optionsValue);
    RETURN_IF_EXCEPTION(scope, { });

    double y = optionalYear.value_or(year());
    double m = optionalMonth.value_or(month());
    RELEASE_AND_RETURN(scope,
        TemporalCalendar::yearMonthFromFields(globalObject, y, m, overflow));
}

ISO8601::Duration TemporalPlainYearMonth::until(JSGlobalObject* globalObject, TemporalPlainYearMonth* other, JSValue optionsValue)
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

    auto [smallestUnit, largestUnit, roundingMode, increment] = extractDifferenceOptions(globalObject, optionsValue, UnitGroup::Date, TemporalUnit::Month, TemporalUnit::Year);
    RETURN_IF_EXCEPTION(scope, { });

    auto result = TemporalCalendar::differenceTemporalPlainYearMonth(
        globalObject, false, plainYearMonth(), other->plainYearMonth(), increment, smallestUnit, largestUnit, roundingMode);
    RETURN_IF_EXCEPTION(scope, { });

    return result;
}

ISO8601::Duration TemporalPlainYearMonth::since(JSGlobalObject* globalObject, TemporalPlainYearMonth* other, JSValue optionsValue)
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

    auto [smallestUnit, largestUnit, roundingMode, increment] = extractDifferenceOptions(globalObject, optionsValue, UnitGroup::Date, TemporalUnit::Month, TemporalUnit::Year);
    RETURN_IF_EXCEPTION(scope, { });
    roundingMode = negateTemporalRoundingMode(roundingMode);

    auto result = TemporalCalendar::differenceTemporalPlainYearMonth(
        globalObject, true, plainYearMonth(), other->plainYearMonth(), increment, smallestUnit, largestUnit, roundingMode);
    RETURN_IF_EXCEPTION(scope, { });

    return result;
}

String TemporalPlainYearMonth::monthCode() const
{
    return ISO8601::monthCode(m_plainYearMonth.month());
}

// https://tc39.es/proposal-temporal/#sec-temporal-adddurationtoyearmonth
ISO8601::PlainYearMonth TemporalPlainYearMonth::addDurationToYearMonth(JSGlobalObject* globalObject,
    bool isAdd, ISO8601::PlainYearMonth yearMonth, ISO8601::Duration duration, TemporalOverflow overflow)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    if (!isAdd)
        duration = -duration;
    auto sign = TemporalDuration::sign(duration);
    auto year = yearMonth.year();
    auto month = yearMonth.month();
    auto day = 1;
    auto intermediateDate = ISO8601::PlainDate(year, month, day);
    ISO8601::PlainDate date;
    if (sign < 0) {
        auto oneMonthDuration = ISO8601::Duration { 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 };
        auto nextMonth = TemporalCalendar::isoDateAdd(globalObject,
            intermediateDate, oneMonthDuration, TemporalOverflow::Constrain);
        RETURN_IF_EXCEPTION(scope, { });
        double y = nextMonth.year();
        double m = nextMonth.month();
        double d = nextMonth.day() - 1;
        date = TemporalCalendar::balanceISODate(y, m, d);
    } else
        date = intermediateDate;
    auto durationToAdd = TemporalDuration::toDateDurationRecordWithoutTime(globalObject, duration);
    RETURN_IF_EXCEPTION(scope, { });
    auto addedDate = TemporalCalendar::isoDateAdd(globalObject, date, durationToAdd, overflow);
    RETURN_IF_EXCEPTION(scope, { });
    return ISO8601::PlainYearMonth(addedDate.year(), addedDate.month());
}

} // namespace JSC
