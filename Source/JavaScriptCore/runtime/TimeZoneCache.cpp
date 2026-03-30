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

#pragma once

#include "TimeZoneCache.h"

#include "ISO8601.h"
#include "TemporalCalendar.h"

namespace JSC {

    namespace TimeZoneCacheInternal {
        static constexpr bool verbose = false;
    }

    static std::optional<String> getTimeZoneNameFromIDSlow(TimeZoneID id)
    {
        const auto& timeZones = intlAvailableTimeZones(TimeZoneKind::All);
        if (id >= timeZones.size())
            return std::nullopt;
        return timeZones[id];
    }

    std::optional<String> TimeZoneCache::getTimeZoneNameFromID(TimeZoneID id)
    {
        if (m_timeZoneIDToStringCache.contains(id))
            return m_timeZoneIDToStringCache[id];
        std::optional<String> name = getTimeZoneNameFromIDSlow(id);
        if (name)
            m_timeZoneIDToStringCache[id] = name.value();
        return name;
    }

    static std::optional<Int128> epochMsToOffsetSlowWithCalendar(UCalendar* calendar, Int128 epochMilliseconds)
    {
        UErrorCode status = U_ZERO_ERROR;

        ucal_setMillis(calendar, epochMilliseconds, &status);
        ASSERT_UNUSED(status, U_SUCCESS(status));

        int32_t dstOffset = 0;

        int32_t rawOffset = ucal_get(calendar, UCAL_ZONE_OFFSET, &status);
        if (U_FAILURE(status))
            return std::nullopt;
        dstOffset = ucal_get(calendar, UCAL_DST_OFFSET, &status);
        if (U_FAILURE(status))
            return std::nullopt;
        return static_cast<Int128>(rawOffset + dstOffset);
    }

    std::unique_ptr<UCalendar, ICUDeleter<ucal_close>> TimeZoneCache::timeZoneToCalendarSlow(TimeZoneID timeZoneIdentifier)
    {
        std::optional<String> timeZoneString = getTimeZoneNameFromID(timeZoneIdentifier);
        if (!timeZoneString)
            return nullptr;

        UErrorCode status = U_ZERO_ERROR;
        auto timeZoneName = timeZoneString->charactersWithNullTermination();
        if (!timeZoneName)
            return nullptr;
        UCalendar* calendar = ucal_open(timeZoneName->span().data(), -1, "", UCAL_DEFAULT, &status);
        ASSERT_UNUSED(status, U_SUCCESS(status));

        return std::unique_ptr<UCalendar, ICUDeleter<ucal_close>>(calendar);
    }
  
    Int128 TimeZoneCache::getNamedTimeZoneOffsetMilliseconds(JSGlobalObject* globalObject, TimeZoneID timeZoneIdentifier, Int128 epochMilliseconds)
    {
        VM& vm = globalObject->vm();
        auto scope = DECLARE_THROW_SCOPE(vm);

        if (m_timeZoneIDToOffsetCache.contains(timeZoneIdentifier)) {
            dataLogLnIf(TimeZoneCacheInternal::verbose, "Cache hit for ", timeZoneIdentifier);
            std::map<Int128, Int128> epochMsToOffset = m_timeZoneIDToOffsetCache[timeZoneIdentifier];
            if (epochMsToOffset.contains(epochMilliseconds)) {
                dataLogLnIf(TimeZoneCacheInternal::verbose, "Cache hit (id and epoch ms) for ", timeZoneIdentifier, " and ", epochMilliseconds);
                return epochMsToOffset[epochMilliseconds];
            }
            else
                dataLogLnIf(TimeZoneCacheInternal::verbose, "Cache miss (id and epoch ms) for ", timeZoneIdentifier, " and ", epochMilliseconds);
        } else
            m_timeZoneIDToOffsetCache[timeZoneIdentifier] = std::map<Int128, Int128>();

        if (!m_timeZoneIDToCalendar.contains(timeZoneIdentifier)) {
            dataLogLnIf(TimeZoneCacheInternal::verbose, "Cache miss (calendar) for ", timeZoneIdentifier);
            std::unique_ptr<UCalendar, ICUDeleter<ucal_close>> calendar = timeZoneToCalendarSlow(timeZoneIdentifier);
            if (!calendar) {
                throwRangeError(globalObject, scope, "bad time zone ID in getNamedTimeZoneOffsetNanoseconds"_s);
                return 0;
            }
            m_timeZoneIDToCalendar[timeZoneIdentifier] = WTF::move(calendar);
        }

        std::optional<Int128> offset = epochMsToOffsetSlowWithCalendar(m_timeZoneIDToCalendar[timeZoneIdentifier].get(), epochMilliseconds);
        if (!offset) {
            throwRangeError(globalObject, scope, "error looking up time zone data"_s);
            return 0;
        }

        std::map<Int128, Int128>& epochMsToOffset = m_timeZoneIDToOffsetCache[timeZoneIdentifier];
        epochMsToOffset[epochMilliseconds] = offset.value();
        dataLogLnIf(TimeZoneCacheInternal::verbose, "Updating cache with id ", timeZoneIdentifier, " and ", epochMilliseconds, " and ", offset.value());
        return offset.value();
    }

} // namespace JSC
