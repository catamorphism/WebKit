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

namespace JSC {

class TemporalPlainYearMonth final : public JSNonFinalObject {
public:
    using Base = JSNonFinalObject;

    template<typename CellType, SubspaceAccess mode>
    static GCClient::IsoSubspace* subspaceFor(VM& vm)
    {
        return vm.temporalPlainYearMonthSpace<mode>();
    }

    static TemporalPlainYearMonth* create(VM&, Structure*, ISO8601::PlainYearMonth&&, JSObject*);
    static TemporalPlainYearMonth* create(VM&, Structure*, ISO8601::PlainYearMonth&&, CalendarID);
    static TemporalPlainYearMonth* tryCreateIfValid(JSGlobalObject*, Structure*, ISO8601::PlainDate&&, JSObject*);
    static TemporalPlainYearMonth* tryCreateIfValid(JSGlobalObject*, Structure*, ISO8601::PlainDate&&, CalendarID);
    static Structure* createStructure(VM&, JSGlobalObject*, JSValue);

    DECLARE_INFO;

    static ISO8601::PlainYearMonth toPlainYearMonth(JSGlobalObject*, const ISO8601::Duration&);
    static std::array<std::optional<double>, 2> toPartialDate(JSGlobalObject*, JSObject*);
    static ISO8601::PlainYearMonth addDurationToYearMonth(
        JSGlobalObject*, bool, ISO8601::PlainYearMonth, ISO8601::Duration, TemporalOverflow);

    static TemporalPlainYearMonth* from(JSGlobalObject*, JSValue, std::optional<JSValue>);
    static TemporalPlainYearMonth* from(JSGlobalObject*, WTF::String);

   JSObject* calendar() const {
        ASSERT(m_builtInCalendarId || m_customCalendar);
        if (m_builtInCalendarId)
            return m_builtInCalendar.get(this);
        return m_customCalendar.value();
    }
    ISO8601::PlainYearMonth plainYearMonth() const { return m_plainYearMonth; }

#define JSC_DEFINE_TEMPORAL_PLAIN_YEAR_MONTH_FIELD(name, capitalizedName) \
    decltype(auto) name() const { return m_plainYearMonth.name(); }
    JSC_TEMPORAL_PLAIN_YEAR_MONTH_UNITS(JSC_DEFINE_TEMPORAL_PLAIN_YEAR_MONTH_FIELD);
#undef JSC_DEFINE_TEMPORAL_PLAIN_YEAR_MONTH_FIELD

    ISO8601::PlainDate with(JSGlobalObject*, JSObject*, JSValue);

    String monthCode() const;

    String toString(JSGlobalObject*, JSValue options) const;
    String toString() const
    {
        return ISO8601::temporalYearMonthToString(m_plainYearMonth, ""_s);
    }

    ISO8601::Duration until(JSGlobalObject*, TemporalPlainYearMonth*, JSValue options);
    ISO8601::Duration since(JSGlobalObject*, TemporalPlainYearMonth*, JSValue options);

    DECLARE_VISIT_CHILDREN;

private:
    TemporalPlainYearMonth(VM&, Structure*, ISO8601::PlainYearMonth&&, JSObject*);
    TemporalPlainYearMonth(VM&, Structure*, ISO8601::PlainYearMonth&&, CalendarID);
    void finishCreation(VM&);

    template<typename CharacterType>
    static std::optional<ISO8601::PlainYearMonth> parse(StringParsingBuffer<CharacterType>&);
    static ISO8601::PlainYearMonth fromObject(JSGlobalObject*, JSObject*);

    ISO8601::PlainYearMonth m_plainYearMonth;
    std::optional<CalendarID> m_builtInCalendarId;
    std::optional<JSObject*> m_customCalendar;
    // Should not be accessed if !m_builtInCalendarID
    LazyProperty<TemporalPlainYearMonth, TemporalCalendar> m_builtInCalendar;
};

} // namespace JSC
