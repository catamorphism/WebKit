/*
 * Copyright (C) 2022 Apple Inc.
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
#include "LazyProperty.h"
#include "TemporalCalendar.h"
#include "TemporalPlainDateTime.h"

namespace JSC {

class TemporalPlainDate final : public JSNonFinalObject {
public:
    using Base = JSNonFinalObject;

    template<typename CellType, SubspaceAccess mode>
    static GCClient::IsoSubspace* subspaceFor(VM& vm)
    {
        return vm.temporalPlainDateSpace<mode>();
    }

    static TemporalPlainDate* create(VM&, Structure*, ISO8601::PlainDate&&, CalendarID);
    static TemporalPlainDate* create(VM&, Structure*, ISO8601::PlainDate&&, JSObject*);
    static TemporalPlainDate* tryCreateIfValid(JSGlobalObject*, Structure*, ISO8601::PlainDate&&, JSObject*);
    static TemporalPlainDate* tryCreateIfValid(JSGlobalObject*, Structure*, ISO8601::PlainDate&&, CalendarID);
    static TemporalPlainDate* tryCreateIfValid(JSGlobalObject*, Structure*, ISO8601::Duration&&, JSObject*);
    static TemporalPlainDate* tryCreateIfValid(JSGlobalObject*, Structure*, ISO8601::Duration&&, CalendarID);
    static Structure* createStructure(VM&, JSGlobalObject*, JSValue);

    DECLARE_INFO;

    static ISO8601::PlainDate toPlainDate(JSGlobalObject*, const ISO8601::Duration&);
    static std::array<std::optional<double>, numberOfTemporalPlainDateUnits> toPartialDate(JSGlobalObject*, JSObject*);
    static std::array<std::optional<double>, numberOfTemporalPlainYearMonthUnits> toYearMonth(JSGlobalObject*, JSObject*);

    static TemporalPlainDate* from(JSGlobalObject*, JSValue, std::optional<JSValue>);

    JSObject* calendar() const {
        ASSERT(m_builtInCalendarId || m_customCalendar);
        if (m_builtInCalendarId)
            return m_builtInCalendar.get(this);
        return m_customCalendar.value();
    }
    ISO8601::PlainDate plainDate() const { return m_plainDate; }

#define JSC_DEFINE_TEMPORAL_PLAIN_DATE_FIELD(name, capitalizedName) \
    decltype(auto) name() const { return m_plainDate.name(); }
    JSC_TEMPORAL_PLAIN_DATE_UNITS(JSC_DEFINE_TEMPORAL_PLAIN_DATE_FIELD);
#undef JSC_DEFINE_TEMPORAL_PLAIN_DATE_FIELD

    ISO8601::PlainDate with(JSGlobalObject*, JSObject* temporalDateLike, JSValue options);

    String monthCode() const;
    uint8_t dayOfWeek() const;
    uint16_t dayOfYear() const;
    uint8_t weekOfYear() const;

    String toString(JSGlobalObject*, JSValue options) const;
    String toString() const
    {
        return ISO8601::temporalDateToString(m_plainDate);
    }

    ISO8601::Duration until(JSGlobalObject*, TemporalPlainDate*, JSValue options);
    ISO8601::Duration since(JSGlobalObject*, TemporalPlainDate*, JSValue options);

    static bool isValidISODate(double, double, double);
    static ISO8601::PlainDate createISODateRecord(double, double, double);
    static std::optional<ISO8601::PlainDate> regulateISODate(double, double, double, TemporalOverflow);

    DECLARE_VISIT_CHILDREN;

private:
    TemporalPlainDate(VM&, Structure*, ISO8601::PlainDate&&, CalendarID);
    TemporalPlainDate(VM&, Structure*, ISO8601::PlainDate&&, JSObject*);
    void finishCreation(VM&);

    template<typename CharacterType>
    static std::optional<ISO8601::PlainDate> parse(StringParsingBuffer<CharacterType>&);
    static ISO8601::PlainDate fromObject(JSGlobalObject*, JSObject*);

    ISO8601::Duration differenceTemporalPlainDate(JSGlobalObject*, bool, TemporalPlainDate*, TemporalUnit, TemporalUnit, RoundingMode, double);

    ISO8601::PlainDate m_plainDate;
    std::optional<CalendarID> m_builtInCalendarId;
    std::optional<JSObject*> m_customCalendar;
    // Should not be accessed if !m_builtInCalendarID
    LazyProperty<TemporalPlainDate, TemporalCalendar> m_builtInCalendar;
};

// https://tc39.es/proposal-temporal/#sec-temporal-isodatewithinlimits
constexpr bool isoDateWithinLimits(ISO8601::PlainDate isoDate)
{
    return isoDateTimeWithinLimits(ISO8601::PlainDateTime(isoDate,
        ISO8601::PlainTime(12, 0, 0, 0, 0, 0)));
}

} // namespace JSC
