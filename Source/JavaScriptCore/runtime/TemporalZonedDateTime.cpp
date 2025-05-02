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

} // namespace JSC
