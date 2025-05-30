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

class TemporalPlainMonthDay final : public JSNonFinalObject {
public:
    using Base = JSNonFinalObject;

    template<typename CellType, SubspaceAccess mode>
    static GCClient::IsoSubspace* subspaceFor(VM& vm)
    {
        return vm.temporalPlainMonthDaySpace<mode>();
    }

    static TemporalPlainMonthDay* create(VM&, Structure*, ISO8601::PlainMonthDay&&, JSObject*);
    static TemporalPlainMonthDay* create(VM&, Structure*, ISO8601::PlainMonthDay&&, CalendarID);
    static TemporalPlainMonthDay* tryCreateIfValid(JSGlobalObject*, Structure*, ISO8601::PlainDate&&, JSObject*);
    static TemporalPlainMonthDay* tryCreateIfValid(JSGlobalObject*, Structure*, ISO8601::PlainDate&&, CalendarID);
    static Structure* createStructure(VM&, JSGlobalObject*, JSValue);

    DECLARE_INFO;

    static ISO8601::PlainMonthDay toPlainMonthDay(JSGlobalObject*, const ISO8601::Duration&);

    static TemporalPlainMonthDay* from(JSGlobalObject*, JSValue, std::optional<JSValue>);
    static TemporalPlainMonthDay* from(JSGlobalObject*, WTF::String);

    JSObject* calendar() const {
        ASSERT(m_builtInCalendarId || m_customCalendar);
        if (m_builtInCalendarId)
            return m_builtInCalendar.get(this);
        return m_customCalendar.value();
    }
    ISO8601::PlainMonthDay plainMonthDay() const { return m_plainMonthDay; }

#define JSC_DEFINE_TEMPORAL_PLAIN_MONTH_DAY_FIELD(name, capitalizedName) \
    decltype(auto) name() const { return m_plainMonthDay.name(); }
    JSC_TEMPORAL_PLAIN_MONTH_DAY_UNITS(JSC_DEFINE_TEMPORAL_PLAIN_MONTH_DAY_FIELD);
#undef JSC_DEFINE_TEMPORAL_PLAIN_MONTH_DAY_FIELD

    ISO8601::PlainDate with(JSGlobalObject*, JSObject*, JSValue);

    String monthCode() const;

    String toString(JSGlobalObject*, JSValue options) const;
    String toString() const
    {
        return ISO8601::temporalMonthDayToString(m_plainMonthDay);
    }

    DECLARE_VISIT_CHILDREN;

private:
    TemporalPlainMonthDay(VM&, Structure*, ISO8601::PlainMonthDay&&, JSObject*);
    TemporalPlainMonthDay(VM&, Structure*, ISO8601::PlainMonthDay&&, CalendarID);
    void finishCreation(VM&);

    template<typename CharacterType>
    static std::optional<ISO8601::PlainMonthDay> parse(StringParsingBuffer<CharacterType>&);
    static ISO8601::PlainMonthDay fromObject(JSGlobalObject*, JSObject*);

    ISO8601::PlainMonthDay m_plainMonthDay;
    std::optional<CalendarID> m_builtInCalendarId;
    std::optional<JSObject*> m_customCalendar;
    // Should not be accessed if !m_builtInCalendarID
    LazyProperty<TemporalPlainMonthDay, TemporalCalendar> m_builtInCalendar;
};

} // namespace JSC
