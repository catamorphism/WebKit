/*
 * Copyright (C) 2021 Sony Interactive Entertainment Inc.
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
#include "ISO8601.h"

#include "DateConstructor.h"
#include "FractionToDouble.h"
#include "IntlObject.h"
#include "IntlObjectInlines.h"
#include "ParseInt.h"
#include "TemporalObject.h"
#include "TemporalPlainDate.h"
#include <limits>
#include <unicode/ucal.h>
#include <wtf/CheckedArithmetic.h>
#include <wtf/DateMath.h>
#include <wtf/WallTime.h>
#include <wtf/text/MakeString.h>
#include <wtf/text/StringParsingBuffer.h>
#include <wtf/unicode/CharacterNames.h>
#include <wtf/unicode/icu/ICUHelpers.h>

WTF_ALLOW_UNSAFE_BUFFER_USAGE_BEGIN

namespace JSC {
namespace ISO8601 {

static constexpr int64_t nsPerHour = 1000LL * 1000 * 1000 * 60 * 60;
static constexpr int64_t nsPerMinute = 1000LL * 1000 * 1000 * 60;
static constexpr int64_t nsPerSecond = 1000LL * 1000 * 1000;
static constexpr int64_t nsPerMillisecond = 1000LL * 1000;
static constexpr int64_t nsPerMicrosecond = 1000LL;

std::optional<TimeZoneID> parseTimeZoneName(StringView string);

static constexpr int32_t caseNormalizeExceptionsLength = 38;
static const String caseNormalizeExceptions[caseNormalizeExceptionsLength] =
    { "Australia/ACT"_s, "Australia/LHI"_s, "Australia/NSW"_s, "Africa/Dar_es_Salaam"_s,
      "America/Port_of_Spain"_s, "Europe/Isle_of_Man"_s, "America/Argentina/ComodRivadavia"_s,
      "America/Knox_IN"_s, "Antarctica/DumontDUrville"_s, "Antarctica/McMurdo"_s,
      "Brazil/DeNoronha"_s, "Chile/EasterIsland"_s, "Mexico/BajaNorte"_s, "Mexico/BajaSur"_s,
      "America/Port-au-Prince"_s, "US/Alaska"_s, "US/Aleutian"_s, "US/Arizona"_s,
      "US/Central"_s, "US/East-Indiana"_s, "US/Eastern"_s, "US/Hawaii"_s,
      "US/Indiana-Starke"_s, "US/Michigan"_s, "US/Mountain"_s, "US/Pacific"_s,
      "US/Pacific-New"_s, "US/Samoa"_s, "GB-Eire"_s, "NZ-CHAT"_s, "W-SU"_s,
      "EST5EDT"_s, "CST6CDT"_s, "MST7MDT"_s, "PST8PDT"_s, "Etc/UCT"_s, "Etc/UTC"_s};

static String caseNormalize(StringView string)
{
    // Capitalize the first letter in the string,
    // the first letter after the '/' if present,
    // and the first letter after any '_'s if present.

    // Anything <= 3 characters should be in all caps.

    if (string.length() <= 3)
        return string.convertToASCIIUppercase();
    
    // Special cases
    for (unsigned i = 0; i < caseNormalizeExceptionsLength; i++) {
        if (equalIgnoringASCIICase(string, caseNormalizeExceptions[i]))
            return caseNormalizeExceptions[i];
    }

    if (string.length() >= 3
        && toASCIILower(string[0]) == 'g'
        && toASCIILower(string[1]) == 'm'
        && toASCIILower(string[2]) == 't') {
        return string.convertToASCIIUppercase();
    }

    StringBuilder result;
    for (unsigned i = 0; i < string.length(); i++) {
        if (i == 0)
            result.append(toASCIIUpper(string[0]));
        else if (string[i - 1] == '/' && i < string.length() - 2
                 && toASCIILower(string[i]) == 'g'
                 && toASCIILower(string[i + 1]) == 'm'
                 && toASCIILower(string[i + 2]) == 't') {
            result.append("GMT"_s);
            i += 2;
        }
        else if (string[i - 1] == '/' || string[i - 1] == '_' || string[i - 1] == '-')
            result.append(toASCIIUpper(string[i]));
        else
            result.append(toASCIILower(string[i]));
    }

    return result.toString();
}

// TODO move this into a separate file
std::optional<TimeZone>
parseTimeZoneName(JSGlobalObject* globalObject, StringView string)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    // Need the canonical name so that ZonedDateTime comparisons
    // work correctly

    if (isNonIANALinkName(string))
        return std::nullopt;

    // TODO: cache this and refactor with getNamedTimeZoneEpochNanoseconds()
    UErrorCode status = U_ZERO_ERROR;
    // ucal_getCanonicalTimeZoneID() is case-sensitive, so we have to case-normalize
    // the string first.
    String caseNormalized = caseNormalize(string);
    auto timeZoneName = caseNormalized.charactersWithNullTermination();
    if (!timeZoneName) {
        throwRangeError(globalObject, scope, "internal error getting time zone data"_s);
        return { };
    }
    Vector<UChar, 32> buffer;
    UBool isSystemID = false;
    status = callBufferProducingFunction(ucal_getCanonicalTimeZoneID, timeZoneName->span().data(), -1, buffer, &isSystemID);
    // ILLEGAL_ARGUMENT_ERROR means this isn't a known time zone, which is OK
    // because there are more arguments to check
    ASSERT_UNUSED(status, U_SUCCESS(status) || status == U_ILLEGAL_ARGUMENT_ERROR);

    StringView canonical(buffer);
    if (caseNormalized != canonical && isSystemID) {
        std::optional<TimeZone> result = parseTimeZoneName(globalObject, canonical);
        RETURN_IF_EXCEPTION(scope, { });
        if (result)
            return result->withOriginal(caseNormalized);
    }

    const auto& timeZones = intlAvailableTimeZones(TimeZoneKind::All);
    for (unsigned index = 0; index < timeZones.size(); ++index) {
        if (equalIgnoringASCIICase(timeZones[index], string))
            return TimeZone::named(index, caseNormalized);
    }

    // Some special cases. Etc/GMT doesn't canonicalize to UTC or GMT for some reason.
    if (equalIgnoringASCIICase(string, "Etc/GMT"))
        return ISO8601::TimeZone::named(utcTimeZoneID(), "Etc/GMT"_s);

    if (equalIgnoringASCIICase(string, "Etc/UTC"))
        return ISO8601::TimeZone::named(utcTimeZoneID(), "Etc/UTC"_s);

    if (equalIgnoringASCIICase(string, "GMT"))
        return ISO8601::TimeZone::named(utcTimeZoneID(), "GMT"_s);

    return std::nullopt;

/* TODO: is any of this necessary now? Was a workaround for 
   intlAvailableTimeZones() filtering out certain things

    // Time zone may be non-canonical, so try canonicalizing it
  
    StringView canonical(buffer);
    if (string != canonical && isSystemID) {
        std::optional<TimeZone> result = parseTimeZoneName(globalObject, canonical);
        if (result)
            return result->withOriginal(caseNormalize(string));
    }

    // These zones canonicalize to themselves for some reason,
    // even though they're shown as aliases in 
    // https://github.com/unicode-org/cldr/blob/main/common/bcp47/timezone.xml .
    // Also, non-uppercase versions of any of these strings are rejected by
    // ucal_getCanonicalTimeZoneID()
    std::optional<TimeZone> result;
    if (equalIgnoringASCIICase(string, "EST5EDT"))
        result = parseTimeZoneName(globalObject, "America/New_York"_s);
    else if (equalIgnoringASCIICase(string, "CST6CDT"))
        result = parseTimeZoneName(globalObject, "America/Chicago"_s);
    else if (equalIgnoringASCIICase(string, "MST7MDT"))
        result = parseTimeZoneName(globalObject, "America/Denver"_s);
    else if (equalIgnoringASCIICase(string, "PST8PDT"))
        result = parseTimeZoneName(globalObject, "America/Los_Angeles"_s);
    else if (equalIgnoringASCIICase(string, "CET") || equalIgnoringASCIICase(string, "MET"))
        result = parseTimeZoneName(globalObject, "Europe/Brussels"_s);
    else if (equalIgnoringASCIICase(string, "EET"))
        result = parseTimeZoneName(globalObject, "Europe/Athens"_s);
    else if (equalIgnoringASCIICase(string, "EST"))
        result = parseTimeZoneName(globalObject, "America/Panama"_s);
    else if (equalIgnoringASCIICase(string, "HST"))
        result = parseTimeZoneName(globalObject, "Pacific/Honolulu"_s);
    else if (equalIgnoringASCIICase(string, "MST"))
        result = parseTimeZoneName(globalObject, "America/Phoenix"_s);
    else if (equalIgnoringASCIICase(string, "WET"))
        result = parseTimeZoneName(globalObject, "Europe/Lisbon"_s);
    else if (equalIgnoringASCIICase(string, "Etc/UTC"))
        result = parseTimeZoneName(globalObject, "UTC"_s);
    else if (equalIgnoringASCIICase(string, "Cuba"))
        result = parseTimeZoneName(globalObject, "Cuba"_s);

    RETURN_IF_EXCEPTION(scope, { });
    if (result)
        return result->withOriginal(caseNormalize(string));

    return std::nullopt;
*/
}

template<typename CharType>
static int32_t parseDecimalInt32(std::span<const CharType> characters)
{
    int32_t result = 0;
    for (auto character : characters) {
        ASSERT(isASCIIDigit(character));
        result = (result * 10) + character - '0';
    }
    return result;
}

// DurationHandleFractions ( fHours, minutes, fMinutes, seconds, fSeconds, milliseconds, fMilliseconds, microseconds, fMicroseconds, nanoseconds, fNanoseconds )
// https://tc39.es/proposal-temporal/#sec-temporal-durationhandlefractions
static void handleFraction(Duration& duration, int factor, StringView fractionString, TemporalUnit fractionType)
{
    auto fractionLength = fractionString.length();
    ASSERT(fractionLength && fractionLength <= 9 && fractionString.containsOnlyASCII());
    ASSERT(fractionType == TemporalUnit::Hour || fractionType == TemporalUnit::Minute || fractionType == TemporalUnit::Second);

    Vector<Latin1Character, 9> padded(9, '0');
    for (unsigned i = 0; i < fractionLength; i++)
        padded[i] = fractionString[i];

    int64_t fraction = static_cast<int64_t>(factor) * parseDecimalInt32(padded.span());
    if (!fraction)
        return;

    static constexpr int64_t divisor = 1'000'000'000LL;
    if (fractionType == TemporalUnit::Hour) {
        fraction *= 60;
        duration.setMinutes(fraction / divisor);
        fraction %= divisor;
        if (!fraction)
            return;
    }

    if (fractionType != TemporalUnit::Second) {
        fraction *= 60;
        duration.setSeconds(fraction / divisor);
        fraction %= divisor;
        if (!fraction)
            return;
    }

    duration.setMilliseconds(fraction / nsPerMillisecond);
    duration.setMicroseconds(fraction % nsPerMillisecond / nsPerMicrosecond);
    duration.setNanoseconds(fraction % nsPerMicrosecond);
}

// ParseTemporalDurationString ( isoString )
// https://tc39.es/proposal-temporal/#sec-temporal-parsetemporaldurationstring
template<typename CharacterType>
static std::optional<Duration> parseDuration(StringParsingBuffer<CharacterType>& buffer)
{
    // ISO 8601 duration strings are like "-P1Y2M3W4DT5H6M7.123456789S". Notes:
    // - case insensitive
    // - sign: + -
    // - separator: . ,
    // - T is present iff there is a time part
    // - integral parts can have any number of digits but fractional parts have at most 9
    // - hours and minutes can have fractional parts too, but only as the LAST part of the string
    if (buffer.lengthRemaining() < 3)
        return std::nullopt;

    Duration result;

    int factor = 1;
    if (*buffer == '+')
        buffer.advance();
    else if (*buffer == '-') {
        factor = -1;
        buffer.advance();
    }

    if (toASCIIUpper(*buffer) != 'P')
        return std::nullopt;

    buffer.advance();
    for (unsigned datePartIndex = 0; datePartIndex < 4 && buffer.hasCharactersRemaining() && isASCIIDigit(*buffer); buffer.advance()) {
        unsigned digits = 1;
        while (digits < buffer.lengthRemaining() && isASCIIDigit(buffer[digits]))
            digits++;

        double integer = factor * parseInt(buffer.span().first(digits), 10);
        buffer.advanceBy(digits);
        if (buffer.atEnd())
            return std::nullopt;

        switch (toASCIIUpper(*buffer)) {
        case 'Y':
            if (datePartIndex)
                return std::nullopt;
            result.setYears(integer);
            datePartIndex = 1;
            break;
        case 'M':
            if (datePartIndex >= 2)
                return std::nullopt;
            result.setMonths(integer);
            datePartIndex = 2;
            break;
        case 'W':
            if (datePartIndex >= 3)
                return std::nullopt;
            result.setWeeks(integer);
            datePartIndex = 3;
            break;
        case 'D':
            result.setDays(integer);
            datePartIndex = 4;
            break;
        default:
            return std::nullopt;
        }
    }

    if (buffer.atEnd())
        return result;

    if (buffer.lengthRemaining() < 3 || toASCIIUpper(*buffer) != 'T')
        return std::nullopt;

    buffer.advance();
    for (unsigned timePartIndex = 0; timePartIndex < 3 && buffer.hasCharactersRemaining() && isASCIIDigit(*buffer); buffer.advance()) {
        unsigned digits = 1;
        while (digits < buffer.lengthRemaining() && isASCIIDigit(buffer[digits]))
            digits++;

        double integer = factor * parseInt(buffer.span().first(digits), 10);
        buffer.advanceBy(digits);
        if (buffer.atEnd())
            return std::nullopt;

        StringView fractionalPart;
        if (*buffer == '.' || *buffer == ',') {
            buffer.advance();
            digits = 0;
            while (digits < buffer.lengthRemaining() && isASCIIDigit(buffer[digits]))
                digits++;
            if (!digits || digits > 9)
                return std::nullopt;

            fractionalPart = buffer.span().first(digits);
            buffer.advanceBy(digits);
            if (buffer.atEnd())
                return std::nullopt;
        }

        switch (toASCIIUpper(*buffer)) {
        case 'H':
            if (timePartIndex)
                return std::nullopt;
            result.setHours(integer);
            if (fractionalPart) {
                handleFraction(result, factor, fractionalPart, TemporalUnit::Hour);
                timePartIndex = 3;
            } else
                timePartIndex = 1;
            break;
        case 'M':
            if (timePartIndex >= 2)
                return std::nullopt;
            result.setMinutes(integer);
            if (fractionalPart) {
                handleFraction(result, factor, fractionalPart, TemporalUnit::Minute);
                timePartIndex = 3;
            } else
                timePartIndex = 2;
            break;
        case 'S':
            result.setSeconds(integer);
            if (fractionalPart)
                handleFraction(result, factor, fractionalPart, TemporalUnit::Second);
            timePartIndex = 3;
            break;
        default:
            return std::nullopt;
        }
    }

    if (buffer.hasCharactersRemaining())
        return std::nullopt;

    return result;
}

std::optional<Duration> parseDuration(StringView string)
{
    return readCharactersForParsing(string, [](auto buffer) -> std::optional<Duration> {
        return parseDuration(buffer);
    });
}


enum class Second60Mode { Accept, Reject };
template<typename CharacterType>
static std::optional<PlainTime> parseTimeSpec(StringParsingBuffer<CharacterType>& buffer, Second60Mode second60Mode, bool parseSubMinutePrecision = true)
{
    // https://tc39.es/proposal-temporal/#prod-TimeSpec
    // TimeSpec :
    //     TimeHour
    //     TimeHour : TimeMinute
    //     TimeHour TimeMinute
    //     TimeHour : TimeMinute : TimeSecond TimeFraction[opt]
    //     TimeHour TimeMinute TimeSecond TimeFraction[opt]
    //
    //  TimeSecond can be 60. And if it is 60, we interpret it as 59.
    //  https://tc39.es/proposal-temporal/#sec-temporal-parseisodatetime

    if (buffer.lengthRemaining() < 2)
        return std::nullopt;

    ASSERT(buffer.lengthRemaining() >= 2);
    auto firstHourCharacter = *buffer;
    if (!(firstHourCharacter >= '0' && firstHourCharacter <= '2'))
        return std::nullopt;

    buffer.advance();
    auto secondHourCharacter = *buffer;
    if (!isASCIIDigit(secondHourCharacter))
        return std::nullopt;
    unsigned hour = (secondHourCharacter - '0') + 10 * (firstHourCharacter - '0');
    if (hour >= 24)
        return std::nullopt;
    buffer.advance();

    if (buffer.atEnd())
        return PlainTime(hour, 0, 0, 0, 0, 0);

    bool splitByColon = false;
    if (*buffer == ':') {
        splitByColon = true;
        buffer.advance();
    } else if (!(*buffer >= '0' && *buffer <= '5'))
        return PlainTime(hour, 0, 0, 0, 0, 0);

    if (buffer.lengthRemaining() < 2)
        return std::nullopt;
    auto firstMinuteCharacter = *buffer;
    if (!(firstMinuteCharacter >= '0' && firstMinuteCharacter <= '5'))
        return std::nullopt;

    buffer.advance();
    auto secondMinuteCharacter = *buffer;
    if (!isASCIIDigit(secondMinuteCharacter))
        return std::nullopt;
    unsigned minute = (secondMinuteCharacter - '0') + 10 * (firstMinuteCharacter - '0');
    ASSERT(minute < 60);
    buffer.advance();

    if (buffer.atEnd())
        return PlainTime(hour, minute, 0, 0, 0, 0);

    if (splitByColon) {
        if (*buffer == ':')
            buffer.advance();
        else
            return PlainTime(hour, minute, 0, 0, 0, 0);
    } else if (!(*buffer >= '0' && (second60Mode == Second60Mode::Accept ? (*buffer <= '6') : (*buffer <= '5'))))
        return PlainTime(hour, minute, 0, 0, 0, 0);

    if (!parseSubMinutePrecision)
        return std::nullopt;

    unsigned second = 0;
    if (buffer.lengthRemaining() < 2)
        return std::nullopt;
    auto firstSecondCharacter = *buffer;
    if (firstSecondCharacter >= '0' && firstSecondCharacter <= '5') {
        buffer.advance();
        auto secondSecondCharacter = *buffer;
        if (!isASCIIDigit(secondSecondCharacter))
            return std::nullopt;
        second = (secondSecondCharacter - '0') + 10 * (firstSecondCharacter - '0');
        ASSERT(second < 60);
        buffer.advance();
    } else if (second60Mode == Second60Mode::Accept && firstSecondCharacter == '6') {
        buffer.advance();
        auto secondSecondCharacter = *buffer;
        if (secondSecondCharacter != '0')
            return std::nullopt;
        second = 59;
        buffer.advance();
    } else
        return std::nullopt;

    if (buffer.atEnd())
        return PlainTime(hour, minute, second, 0, 0, 0);

    if (*buffer != '.' && *buffer != ',')
        return PlainTime(hour, minute, second, 0, 0, 0);
    buffer.advance();

    size_t digits = 0;
    size_t maxCount = std::min<size_t>(buffer.lengthRemaining(), 9);
    for (; digits < maxCount; ++digits) {
        if (!isASCIIDigit(buffer[digits]))
            break;
    }
    if (!digits)
        return std::nullopt;

    Vector<Latin1Character, 9> padded(9, '0');
    for (size_t i = 0; i < digits; ++i)
        padded[i] = buffer[i];
    buffer.advanceBy(digits);

    unsigned millisecond = parseDecimalInt32(padded.span().first(3));
    unsigned microsecond = parseDecimalInt32(padded.subspan(3, 3));
    unsigned nanosecond = parseDecimalInt32(padded.subspan(6, 3));

    return PlainTime(hour, minute, second, millisecond, microsecond, nanosecond);
}

template<typename CharacterType>
static std::optional<int64_t> parseUTCOffset(StringParsingBuffer<CharacterType>& buffer, bool parseSubMinutePrecision = true)
{
    // UTCOffset[SubMinutePrecision] :
    //     ASCIISign Hour
    //     ASCIISign Hour TimeSeparator[+Extended] MinuteSecond
    //     ASCIISign Hour TimeSeparator[~Extended] MinuteSecond
    //     [+SubMinutePrecision] ASCIISign Hour TimeSeparator[+Extended] MinuteSecond TimeSeparator[+Extended] MinuteSecond TemporalDecimalFractionopt
    //     [+SubMinutePrecision] ASCIISign Hour TimeSeparator[~Extended] MinuteSecond TimeSeparator[~Extended] MinuteSecond TemporalDecimalFractionopt
    //
    //  This is the same to
    //     ASCIISign TimeSpec
    //
    //  Maximum and minimum values are ±23:59:59.999999999 = ±86399999999999ns, which can be represented by int64_t / double's integer part.

    // sign and hour.
    if (buffer.lengthRemaining() < 3)
        return std::nullopt;

    int64_t factor = 1;
    if (*buffer == '+')
        buffer.advance();
    else if (*buffer == '-') {
        factor = -1;
        buffer.advance();
    } else
        return std::nullopt;

    auto plainTime = parseTimeSpec(buffer, Second60Mode::Reject, parseSubMinutePrecision);
    if (!plainTime)
        return std::nullopt;

    int64_t hour = plainTime->hour();
    int64_t minute = plainTime->minute();
    int64_t second = plainTime->second();
    int64_t millisecond = plainTime->millisecond();
    int64_t microsecond = plainTime->microsecond();
    int64_t nanosecond = plainTime->nanosecond();

    return (nsPerHour * hour + nsPerMinute * minute + nsPerSecond * second + nsPerMillisecond * millisecond + nsPerMicrosecond * microsecond + nanosecond) * factor;
}

std::optional<int64_t> parseUTCOffset(StringView string, bool parseSubMinutePrecision)
{
    return readCharactersForParsing(string, [parseSubMinutePrecision](auto buffer) -> std::optional<int64_t> {
        auto result = parseUTCOffset(buffer, parseSubMinutePrecision);
        if (!buffer.atEnd())
            return std::nullopt;
        return result;
    });
}

std::optional<int64_t> parseDateTimeUTCOffset(StringView string)
{
    return parseUTCOffset(string, true);
}

template<typename CharacterType>
static std::optional<int64_t> parseUTCOffsetInMinutes(StringParsingBuffer<CharacterType>& buffer)
{
    // UTCOffset :::
    //     TemporalSign Hour
    //     TemporalSign Hour HourSubcomponents[+Extended]
    //     TemporalSign Hour HourSubcomponents[~Extended]
    //
    // TemporalSign :::
    //     ASCIISign
    //     <MINUS>
    //
    // ASCIISign ::: one of
    //     + -
    //
    // Hour :::
    //     0 DecimalDigit
    //     1 DecimalDigit
    //     20
    //     21
    //     22
    //     23
    //
    // HourSubcomponents[Extended] :::
    //     TimeSeparator[?Extended] MinuteSecond
    //
    // TimeSeparator[Extended] :::
    //     [+Extended] :
    //     [~Extended] [empty]
    //
    // MinuteSecond :::
    //     0 DecimalDigit
    //     1 DecimalDigit
    //     2 DecimalDigit
    //     3 DecimalDigit
    //     4 DecimalDigit
    //     5 DecimalDigit

    // sign and hour.
    if (buffer.lengthRemaining() < 3)
        return std::nullopt;

    int64_t factor = 1;
    if (*buffer == '+')
        buffer.advance();
    else if (*buffer == '-') {
        factor = -1;
        buffer.advance();
    } else
        return std::nullopt;

    ASSERT(buffer.lengthRemaining() >= 2);
    auto firstHourCharacter = *buffer;
    if (!(firstHourCharacter >= '0' && firstHourCharacter <= '2'))
        return std::nullopt;

    buffer.advance();
    auto secondHourCharacter = *buffer;
    if (!isASCIIDigit(secondHourCharacter))
        return std::nullopt;
    unsigned hour = (secondHourCharacter - '0') + 10 * (firstHourCharacter - '0');
    if (hour >= 24)
        return std::nullopt;
    buffer.advance();

    if (buffer.atEnd())
        return (hour * 60) * factor;

    if (*buffer == ':')
        buffer.advance();
    else if (!(*buffer >= '0' && *buffer <= '5'))
        return (hour * 60) * factor;

    if (buffer.lengthRemaining() < 2)
        return std::nullopt;
    auto firstMinuteCharacter = *buffer;
    if (!(firstMinuteCharacter >= '0' && firstMinuteCharacter <= '5'))
        return std::nullopt;

    buffer.advance();
    auto secondMinuteCharacter = *buffer;
    if (!isASCIIDigit(secondMinuteCharacter))
        return std::nullopt;
    unsigned minute = (secondMinuteCharacter - '0') + 10 * (firstMinuteCharacter - '0');
    ASSERT(minute < 60);
    buffer.advance();

    return (hour * 60 + minute) * factor;
}

std::optional<int64_t> parseUTCOffsetInMinutes(StringView string)
{
    return readCharactersForParsing(string, [](auto buffer) -> std::optional<int64_t> {
        auto result = parseUTCOffsetInMinutes(buffer);
        if (!buffer.atEnd())
            return std::nullopt;
        return result;
    });
}

template<typename CharacterType>
static bool canBeRFC9557Annotation(const StringParsingBuffer<CharacterType>& buffer)
{
    // https://tc39.es/proposal-temporal/#sec-temporal-parseisodatetime
    // Step 4(a)(ii)(2)(a):
    //  Let key be the source text matched by the AnnotationKey Parse Node contained within annotation
    //
    // https://tc39.es/proposal-temporal/#prod-Annotation
    // Annotation :::
    //     [ AnnotationCriticalFlag[opt] AnnotationKey = AnnotationValue ]
    //
    // AnnotationCriticalFlag :::
    //     !
    //
    // AnnotationKey :::
    //     AKeyLeadingChar
    //     AnnotationKey AKeyChar
    //
    // AKeyLeadingChar :::
    //     LowercaseAlpha
    //     _
    //
    // AKeyChar :::
    //     AKeyLeadingChar
    //     DecimalDigit
    //     -
    //
    // AnnotationValue :::
    //     AnnotationValueComponent
    //     AnnotationValueComponent - AnnotationValue
    //
    // AnnotationValueComponent :::
    //     Alpha AnnotationValueComponent[opt]
    //     DecimalDigit AnnotationValueComponent[opt]

    // This just checks for '[', followed by an optional '!' (critical flag),
    // followed by a valid key, followed by an '='.

    size_t length = buffer.lengthRemaining();
    // Because of `[`, `=`, `]`, `AnnotationKey`, and `AnnotationValue`,
    // the annotation must have length >= 5.
    if (length < 5)
        return false;
    if (*buffer != '[')
        return false;
    size_t index = 1;
    if (buffer[index] == '!')
        ++index;
    if (!isASCIILower(buffer[index]) && buffer[index] != '_')
        return false;
    ++index;
    while (index < length) {
        if (buffer[index] == '=')
            return true;
        if (isASCIILower(buffer[index]) || isASCIIDigit(buffer[index]) || buffer[index] == '-' || buffer[index] == '_')
            ++index;
        else
            return false;
    }
    return false;
}

template<typename CharacterType>
static bool canBeTimeZone(const StringParsingBuffer<CharacterType>& buffer, CharacterType character)
{
    switch (static_cast<char16_t>(character)) {
    // UTCDesignator
    // https://tc39.es/proposal-temporal/#prod-UTCDesignator
    case 'z':
    case 'Z':
    // TimeZoneUTCOffsetSign
    // https://tc39.es/proposal-temporal/#prod-TimeZoneUTCOffsetSign
    case '+':
    case '-':
        return true;
    // TimeZoneBracketedAnnotation
    // https://tc39.es/proposal-temporal/#prod-TimeZoneBracketedAnnotation
    case '[': {
        // We should reject calendar extension case.
        // For BNF, see comment in canBeRFC9557Annotation()
        if (canBeRFC9557Annotation(buffer))
            return false;
        return true;
    }
    default:
        return false;
    }
}

template<typename CharacterType>
static bool canBeTimeZoneAnnotation(const StringParsingBuffer<CharacterType>& buffer, CharacterType character)
{
    switch (static_cast<UChar>(character)) {
    // TimeZoneBracketedAnnotation
    // https://tc39.es/proposal-temporal/#prod-TimeZoneBracketedAnnotation
    case '[': {
        // We should reject calendar extension case.
        // https://tc39.es/proposal-temporal/#prod-Calendar
        // Calendar :
        //     [u-ca= CalendarName]
        if (canBeRFC9557Annotation(buffer))
            return false;
        return true;
    }
    default:
        return false;
    }
}

template<typename CharacterType>
static bool isTZChar(CharacterType character) {
        // TZLeadingChar :
        //     Alpha
        //     .
        //     _
        //
        // TZChar :
        //     TZLeadingChar
        //     DecimalDigit
        //     -
        //     +
        //
    return isASCIIAlpha(character) || isASCIIDigit(character) || character == '.' || character == '_' || character == '+' || character == '-';
}

template<typename CharacterType>
static std::optional<TimeZoneAnnotation> parseTimeZoneAnnotation(StringParsingBuffer<CharacterType>& buffer)
{
    // https://tc39.es/proposal-temporal/#prod-TimeZoneAnnotation
    // TimeZoneAnnotation :
    //     [ AnnotationCriticalFlag_opt TimeZoneIdentifier ]
    // TimeZoneIdentifier :
    //     UTCOffset_[~SubMinutePrecision]
    //     TimeZoneIANAName

    if (buffer.lengthRemaining() < 3)
        return std::nullopt;

    if (*buffer != '[')
        return std::nullopt;
    buffer.advance();

    if (*buffer == '!')
        buffer.advance();

    switch (static_cast<char16_t>(*buffer)) {
    case '+':
    case '-': {
        StringParsingBuffer<CharacterType> bufferCopy = buffer;
        int32_t lengthRemaining = buffer.lengthRemaining();
        auto offset = parseUTCOffset(buffer, false);
        auto numOffsetChars = lengthRemaining - buffer.lengthRemaining();
        Vector<Latin1Character> asString(bufferCopy.consume(numOffsetChars).subspan(0, numOffsetChars));
        if (!offset)
            return std::nullopt;
        if (buffer.atEnd())
            return std::nullopt;
        if (*buffer != ']')
            return std::nullopt;
        buffer.advance();
        return TimeZoneAnnotation { asString, offset.value() };
    }
    case 'E': {
        // "Etc/GMT+20" and "]" => length is 11.
        if (buffer.lengthRemaining() >= 11) {
            if (buffer[0] == 'E' && buffer[1] == 't' && buffer[2] == 'c' && buffer[3] == '/' && buffer[4] == 'G' && buffer[5] == 'M' && buffer[6] == 'T') {
                auto signCharacter = buffer[7];
                // Not including minusSign since it is ASCIISign.
                if (signCharacter == '+' || signCharacter == '-') {
                    // Etc/GMT+01 is UTC-01:00. This sign is intentionally inverted.
                    // https://en.wikipedia.org/wiki/Tz_database#Area
                    int64_t factor = signCharacter == '+' ? -1 : 1;
                    int64_t hour = 0;
                    auto firstHourCharacter = buffer[8];
                    if (firstHourCharacter >= '0' && firstHourCharacter <= '2') {
                        auto secondHourCharacter = buffer[9];
                        if (isASCIIDigit(secondHourCharacter)) {
                            hour = (secondHourCharacter - '0') + 10 * (firstHourCharacter - '0');
                            if (hour < 24 && buffer[10] == ']') {
                                Vector<Latin1Character> asString = buffer.consume(10).subspan(0, 10);
                                buffer.advance(); // consume ']'
                                return TimeZoneAnnotation { asString, nsPerHour * hour * factor };
                            }
                        } else
                            return std::nullopt;
                    } else
                        return std::nullopt;
                }
            }
        }
/*
        if (buffer.lengthRemaining() >= 9) {
            if (buffer[0] == 'E' && buffer[1] == 't' && buffer[2] == 'c' && buffer[3] == '/' && buffer[4] == 'G' && buffer[5] == 'M' && buffer[6] == 'T' && ((buffer[7] != '+' && buffer[7] != '-') || ((buffer[8] == '0') && buffer.lengthRemaining() > 10))) {
                return std::nullopt;
            }
        }
*/
        [[fallthrough]];
    }
    default: {
        // TZLeadingChar :
        //     Alpha
        //     .
        //     _
        //
        // TZChar :
        //     TZLeadingChar
        //     DecimalDigit
        //     -
        //     +
        //
        // TimeZoneIANANameComponent :
        //     TZLeadingChar TZChar[opt] TZChar[opt] TZChar[opt] TZChar[opt] TZChar[opt] TZChar[opt] TZChar[opt] TZChar[opt] TZChar[opt] TZChar[opt] TZChar[opt] TZChar[opt] TZChar[opt] but not one of . or ..
        //
        // TimeZoneIANAName :
        //     TimeZoneIANANameComponent
        //     TimeZoneIANAName / TimeZoneIANANameComponent

        unsigned nameLength = 0;
        {
            unsigned index = 0;
            for (; index < buffer.lengthRemaining(); ++index) {
                auto character = buffer[index];
                if (character == ']')
                    break;
                if (!(isTZChar(character) || character == '/'))
                    return std::nullopt;
            }
            if (!index)
                return std::nullopt;
            nameLength = index;
        }

        auto isValidComponent = [&](unsigned start, unsigned end) {
            unsigned componentLength = end - start;
            if (!componentLength)
                return false;
// TODO: ???
// Check fails for Canada/East-Saskatchewan
/*
            if (componentLength > 14)
                return false;
*/
            if (componentLength == 1 && buffer[start] == '.')
                return false;
            if (componentLength == 2 && buffer[start] == '.' && buffer[start + 1] == '.')
                return false;
            return true;
        };

        unsigned currentNameComponentStartIndex = 0;
        bool isLeadingCharacterInNameComponent = true;
        for (unsigned index = 0; index < nameLength; ++index) {
            auto character = buffer[index];
            if (isLeadingCharacterInNameComponent) {
                if (!(isASCIIAlpha(character) || character == '.' || character == '_'))
                    return std::nullopt;

                currentNameComponentStartIndex = index;
                isLeadingCharacterInNameComponent = false;
                continue;
            }

            if (character == '/') {
                if (!isValidComponent(currentNameComponentStartIndex, index))
                    return std::nullopt;
                isLeadingCharacterInNameComponent = true;
                continue;
            }

            if (!isTZChar(character))
                return std::nullopt;
        }
        if (isLeadingCharacterInNameComponent)
            return std::nullopt;
        if (!isValidComponent(currentNameComponentStartIndex, nameLength))
            return std::nullopt;

        Vector<Latin1Character> result;
        for (uint32_t i = 0; i < nameLength; i++) {
            result.append(*buffer);
            buffer.advance();
        }

        if (buffer.atEnd())
            return std::nullopt;
        if (*buffer != ']')
            return std::nullopt;
        buffer.advance();
        return TimeZoneAnnotation { result, std::nullopt };
    }
    }
}

template<typename CharacterType>
static std::optional<TimeZoneRecord> parseTimeZone(StringParsingBuffer<CharacterType>& buffer, bool requireBrackets)
{
    if (buffer.atEnd())
        return std::nullopt;
    switch (static_cast<char16_t>(*buffer)) {
    // UTCDesignator
    // https://tc39.es/proposal-temporal/#prod-UTCDesignator
    case 'z':
    case 'Z': {
        if (!requireBrackets) {
            buffer.advance();
            if (!buffer.atEnd() && *buffer == '[' && canBeTimeZone(buffer, *buffer)) {
                auto timeZoneAnnotation = parseTimeZoneAnnotation(buffer);
                if (!timeZoneAnnotation)
                    return std::nullopt;
                return TimeZoneRecord { true, std::nullopt, timeZoneAnnotation };
            }
            return TimeZoneRecord { true, std::nullopt, std::nullopt };
        }
        break;
    }
    // TimeZoneUTCOffsetSign
    // https://tc39.es/proposal-temporal/#prod-TimeZoneUTCOffsetSign
    case '+':
    case '-': {
        if (!requireBrackets) {
            // Accept sub-minute precision in offset
            StringParsingBuffer<CharacterType> bufferCopy = buffer;
            int32_t lengthRemaining = buffer.lengthRemaining();
            auto offset = parseUTCOffset(buffer, true);
            auto numOffsetChars = lengthRemaining - buffer.lengthRemaining();
            Vector<Latin1Character> chars;
            for (uint32_t i = 0; i < numOffsetChars; i++) {
                chars.append(*bufferCopy);
                bufferCopy.advance();
            }
            if (!offset)
                return std::nullopt;
            if (!buffer.atEnd() && *buffer == '[' && canBeTimeZone(buffer, *buffer)) {
                auto timeZoneAnnotation = parseTimeZoneAnnotation(buffer);
                if (!timeZoneAnnotation)
                    return std::nullopt;
                return TimeZoneRecord { false, TimeZoneOffset { chars, offset.value() }, timeZoneAnnotation };
            }
            return TimeZoneRecord { false, TimeZoneOffset { chars, offset.value() }, std::nullopt };
        }
        break;
    }
    // TimeZoneBracketedAnnotation
    // https://tc39.es/proposal-temporal/#prod-TimeZoneBracketedAnnotation
    case '[': {
        auto timeZoneAnnotation = parseTimeZoneAnnotation(buffer);
        if (!timeZoneAnnotation) [[unlikely]]
            return std::nullopt;
        return TimeZoneRecord { false, std::nullopt, timeZoneAnnotation };
    }
    default:
        break;
    }
    return std::nullopt;
}

std::optional<TimeZoneRecord> parseTimeZone(StringView string, bool requireBrackets)
{
    return readCharactersForParsing(string, [requireBrackets](auto buffer) -> std::optional<TimeZoneRecord> {
        auto result = parseTimeZone(buffer, requireBrackets);
        if (!buffer.atEnd()) [[unlikely]]
            return std::nullopt;
        return result;
    });
}

template<typename CharacterType>
static std::optional<RFC9557Annotation> parseOneRFC9557Annotation(StringParsingBuffer<CharacterType>& buffer)
{
    // For BNF, see comment in canBeRFC9557Annotation()

    if (!canBeRFC9557Annotation(buffer))
        return std::nullopt;
    RFC9557Flag flag = buffer[1] == '!' ? RFC9557Flag::Critical : RFC9557Flag::None;
    // Skip '[' or '[!'
    buffer.advanceBy(flag == RFC9557Flag::Critical ? 2 : 1);

    // Parse the key
    unsigned keyLength = 0;
    while (buffer[keyLength] != '=')
        keyLength++;
    if (!keyLength)
        return std::nullopt;
    auto key(buffer.span().first(keyLength));
    buffer.advanceBy(keyLength);

    if (buffer.atEnd())
        return std::nullopt;

    // Consume the '='
    buffer.advance();

    unsigned nameLength = 0;
    {
        unsigned index = 0;
        for (; index < buffer.lengthRemaining(); ++index) {
            auto character = buffer[index];
            if (character == ']')
                break;
            if (!isASCIIAlpha(character) && !isASCIIDigit(character) && character != '-')
                return std::nullopt;
        }
        if (!index)
            return std::nullopt;
        nameLength = index;
    }

    // Check if the key is equal to "u-ca"
    if (key.size() != 4
        || key[0] != 'u' || key[1] != '-'
        || key[2] != 'c' || key[3] != 'a') {
        // Annotation is unknown
        // Consume the rest of the annotation
        buffer.advanceBy(nameLength);
        if (buffer.atEnd() || *buffer != ']') {
            // Parse error
            return std::nullopt;
        }
        // Consume the ']'
        buffer.advance();
        return RFC9557Annotation { flag, RFC9557Key::Other, { } };
    }

    auto isValidComponent = [&](unsigned start, unsigned end) {
        unsigned componentLength = end - start;
        if (componentLength < minCalendarLength)
            return false;
        if (componentLength > maxCalendarLength)
            return false;
        return true;
    };

    unsigned currentNameComponentStartIndex = 0;
    bool isLeadingCharacterInNameComponent = true;
    for (unsigned index = 0; index < nameLength; ++index) {
        auto character = buffer[index];
        if (isLeadingCharacterInNameComponent) {
            if (!(isASCIIAlpha(character) || isASCIIDigit(character)))
                return std::nullopt;

            currentNameComponentStartIndex = index;
            isLeadingCharacterInNameComponent = false;
            continue;
        }

        if (character == '-') {
            if (!isValidComponent(currentNameComponentStartIndex, index))
                return std::nullopt;
            isLeadingCharacterInNameComponent = true;
            continue;
        }

        if (!(isASCIIAlpha(character) || isASCIIDigit(character)))
            return std::nullopt;
    }
    if (isLeadingCharacterInNameComponent)
        return std::nullopt;
    if (!isValidComponent(currentNameComponentStartIndex, nameLength))
        return std::nullopt;

    Vector<Latin1Character, maxCalendarLength> result;
    for (uint32_t i = 0; i < nameLength; i++) {
        result.append(*buffer);
        buffer.advance();
    }

    if (buffer.atEnd())
        return std::nullopt;
    if (*buffer != ']')
        return std::nullopt;
    buffer.advance();
    return RFC9557Annotation { flag, RFC9557Key::Calendar, WTF::move(result) };
}

template<typename CharacterType>
static std::optional<Vector<CalendarID, 1>>
parseCalendar(StringParsingBuffer<CharacterType>& buffer)
{
    // https://tc39.es/proposal-temporal/#prod-Annotations
    //  Annotations :::
    //      Annotation Annotations[opt]

    if (!canBeRFC9557Annotation(buffer))
        return std::nullopt;

    Vector<CalendarID, 1> result;
    // https://tc39.es/proposal-temporal/#sec-temporal-parseisodatetime
    bool calendarWasCritical = false;
    while (canBeRFC9557Annotation(buffer)) {
        auto annotation = parseOneRFC9557Annotation(buffer);
        if (!annotation)
            return std::nullopt;
        if (annotation->m_key == RFC9557Key::Calendar)
            result.append(annotation->m_value);
        if (annotation->m_flag == RFC9557Flag::Critical) {
            // Check for unknown annotations with critical flag
            // step 4(a)(ii)(2)(d)(i)
            if (annotation->m_key != RFC9557Key::Calendar)
                return std::nullopt;
            // Check for multiple calendars and critical flag
            // step 4(a)(ii)(2)(c)(ii)
            if (result.size() == 1)
                calendarWasCritical = true;
            else
                return std::nullopt;
        }
        if (calendarWasCritical && result.size() > 1)
            return std::nullopt;
    }
    return result;
}

std::optional<CalendarID> parseCalendar(StringView string)
{
    return readCharactersForParsing(string, [](auto buffer) -> std::optional<CalendarID> {
        auto result = parseCalendar(buffer);
        if (!buffer.atEnd()) [[unlikely]]
            return std::nullopt;
        if (result && result->size() > 1) [[unlikely]]
            return std::nullopt;
        if (result)
            return result.value()[0];
        return std::nullopt;
    });
}

template<typename CharacterType>
static std::optional<std::tuple<PlainTime, std::optional<TimeZoneRecord>>> parseTime(StringParsingBuffer<CharacterType>& buffer)
{
    // https://tc39.es/proposal-temporal/#prod-Time
    // Time :
    //     TimeSpec TimeZone[opt]
    auto plainTime = parseTimeSpec(buffer, Second60Mode::Accept);
    if (!plainTime)
        return std::nullopt;
    if (buffer.atEnd())
        return std::tuple { WTF::move(plainTime.value()), std::nullopt };
    if (canBeTimeZone(buffer, *buffer)) {
        auto timeZone = parseTimeZone(buffer, false);
        if (!timeZone)
            return std::nullopt;
        return std::tuple { WTF::move(plainTime.value()), WTF::move(timeZone) };
    }
    return std::tuple { WTF::move(plainTime.value()), std::nullopt };
}

template<typename CharacterType>
static bool canBeYear(StringParsingBuffer<CharacterType>& buffer)
{
    // 4 characters for year, plus 2 more for month
    if (buffer.lengthRemaining() < 6)
        return false;
    bool hasPrefix = buffer[0] == '+' || buffer[0] == '-';
    if (!isASCIIDigit(buffer[0]) && !hasPrefix)
        return false;
    size_t start = hasPrefix ? 1 : 0;
    for (size_t i = start; i < 4 + start; i++) {
        if (!isASCIIDigit(buffer[i]))
            return false;
    }
    return true;
}

template<typename CharacterType>
static std::optional<PlainDate> parseDate(StringParsingBuffer<CharacterType>& buffer, TemporalDateFormat format)
{
    // https://tc39.es/proposal-temporal/#prod-Date
    // Date :
    //     DateYear - DateMonth - DateDay
    //     DateYear DateMonth DateDay
    //
    // DateYear :
    //     DateFourDigitYear
    //     DateExtendedYear
    //
    // DateFourDigitYear :
    //     Digit Digit Digit Digit
    //
    // DateExtendedYear :
    //     Sign Digit Digit Digit Digit Digit Digit
    //
    // DateMonth :
    //     0 NonzeroDigit
    //     10
    //     11
    //     12
    //
    // DateDay :
    //     0 NonzeroDigit
    //     1 Digit
    //     2 Digit
    //     30
    //     31
    //
    //  DateSpecYearMonth :::
    //      DateYear DateSeparator_[+Extended] DateMonth
    //      DateYear DateSeparator_[~Extended] DateMonth
    //
    //  DateSpecMonthDay :::
    //      --opt DateMonth DateSeparator_[+Extended] DateDay
    //      --opt DateMonth DateSeparator_[~Extended] DateDay

    if (buffer.atEnd())
        return std::nullopt;

    int32_t year = 0;
    bool splitByHyphen = false;

    if (*buffer == '-') {
        if (buffer.lengthRemaining() > 2
            && buffer[1] == '-'
            && format == TemporalDateFormat::MonthDay) {
            buffer.advanceBy(2);
        }
    }

    // Look ahead to distinguish month from year
    if (canBeYear(buffer)) {
        bool sixDigitsYear = false;
        int yearFactor = 1;
        if (*buffer == '+') {
            buffer.advance();
            sixDigitsYear = true;
        } else if (*buffer == '-') {
            yearFactor = -1;
            buffer.advance();
            sixDigitsYear = true;
        } else if (!isASCIIDigit(*buffer))
            return std::nullopt;

        if (sixDigitsYear) {
            if (buffer.lengthRemaining() < 6)
                return std::nullopt;
            for (unsigned index = 0; index < 6; ++index) {
                if (!isASCIIDigit(buffer[index]))
                    return std::nullopt;
            }
            year = parseDecimalInt32(std::span { buffer.position(), 6 }) * yearFactor;
            if (!year && yearFactor < 0)
                return std::nullopt;
            buffer.advanceBy(6);
        } else {
            if (buffer.lengthRemaining() >= 5) {
                for (unsigned index = 0; index < 4; ++index) {
                    if (!isASCIIDigit(buffer[index]))
                        return std::nullopt;
                }
                // A year must be followed by a - or month
                if (buffer[4] == '-' || isASCIIDigit(buffer[4])) {
                    year = parseDecimalInt32(std::span { buffer.position(), 4 });
                    buffer.advanceBy(4);
                }
            }
        }

        if (buffer.atEnd())
            return std::nullopt;

        if (*buffer == '-') {
            splitByHyphen = true;
            buffer.advance();
            if (buffer.lengthRemaining() < 5 && format == TemporalDateFormat::Date)
                return std::nullopt;
        } else {
            if (buffer.lengthRemaining() < 4 && format == TemporalDateFormat::Date)
                return std::nullopt;
        }
    } else if (buffer.lengthRemaining() < 4) // If not enough length for month and day, it's a parse error
        return std::nullopt;

    // We ensured that buffer has enough length for month and day. We do not need to check length.

    unsigned month = 0;
    auto firstMonthCharacter = *buffer;
    if (firstMonthCharacter == '0' || firstMonthCharacter == '1') {
        buffer.advance();
        auto secondMonthCharacter = *buffer;
        if (!isASCIIDigit(secondMonthCharacter))
            return std::nullopt;
        month = (secondMonthCharacter - '0') + 10 * (firstMonthCharacter - '0');
        if (!month || month > 12)
            return std::nullopt;
        buffer.advance();
    } else
        return std::nullopt;

    if (format == TemporalDateFormat::YearMonth && (buffer.atEnd() || canBeRFC9557Annotation(buffer) || canBeTimeZoneAnnotation(buffer, *buffer))) {
        if (!isYearWithinLimits(year)) [[unlikely]]
            year = outOfRangeYear;
        return PlainDate(year, month, 1);
    }

    if (!buffer.atEnd() && *buffer == '-') {
        if (splitByHyphen || format != TemporalDateFormat::Date)
            buffer.advance();
        else
            return std::nullopt;
    } else if (splitByHyphen)
        return std::nullopt;

    unsigned day = 0;
    if (buffer.lengthRemaining() >= 2) {
        auto firstDayCharacter = *buffer;
        if (firstDayCharacter >= '0' && firstDayCharacter <= '3') {
            buffer.advance();
            auto secondDayCharacter = *buffer;
            if (!isASCIIDigit(secondDayCharacter))
                return std::nullopt;
            day = (secondDayCharacter - '0') + 10 * (firstDayCharacter - '0');
            if (!day || day > daysInMonth(year, month))
                return std::nullopt;
            buffer.advance();
        } else if (format != TemporalDateFormat::YearMonth)
            return std::nullopt;
    }

    // PlainDate represents out-of-range years using outOfRangeYear
    if (!isYearWithinLimits(year)) [[unlikely]]
        year = outOfRangeYear;

    switch (format) {
    case TemporalDateFormat::Date:
        return PlainDate(year, month, day);
    case TemporalDateFormat::YearMonth:
        return PlainDate(year, month, 1);
    case TemporalDateFormat::MonthDay:
        return PlainDate(1972, month, day);
    default:
        RELEASE_ASSERT_NOT_REACHED();
    }
}

template<typename CharacterType>
static std::optional<std::tuple<PlainDate, std::optional<PlainTime>, std::optional<TimeZoneRecord>>> parseDateTime(StringParsingBuffer<CharacterType>& buffer, TemporalDateFormat format)
{
    // https://tc39.es/proposal-temporal/#prod-DateTime
    // DateTime :
    //     Date TimeSpecSeparator[opt] TimeZone[opt]
    //
    // TimeSpecSeparator :
    //     DateTimeSeparator TimeSpec
    auto plainDate = parseDate(buffer, format);
    if (!plainDate)
        return std::nullopt;
    if (buffer.atEnd())
        return std::tuple { WTF::move(plainDate.value()), std::nullopt, std::nullopt };

    if (*buffer == ' ' || *buffer == 'T' || *buffer == 't') {
        buffer.advance();
        auto plainTimeAndTimeZone = parseTime(buffer);
        if (!plainTimeAndTimeZone)
            return std::nullopt;
        auto [plainTime, timeZone] = WTF::move(plainTimeAndTimeZone.value());
        return std::tuple { WTF::move(plainDate.value()), WTF::move(plainTime), WTF::move(timeZone) };
    }

    if (canBeTimeZone(buffer, *buffer)) {
        auto timeZone = parseTimeZone(buffer, true);
        return std::tuple { WTF::move(plainDate.value()), std::nullopt, timeZone };
    }

    return std::tuple { WTF::move(plainDate.value()), std::nullopt, std::nullopt };
}

template<typename CharacterType>
static std::optional<std::tuple<PlainDate, std::optional<PlainTime>, std::optional<TimeZoneRecord>, std::optional<CalendarID>>> parseTemporalDateTimeString(StringParsingBuffer<CharacterType>& buffer)
{
    // https://tc39.es/proposal-temporal/#prod-TemporalDateTimeString
    // TemporalDateTimeString[Zoned] :
    //     AnnotatedDateTime[?Zoned, ~TimeRequired]
    //
    //  AnnotatedDateTime[Zoned, TimeRequired] :
    //      [~Zoned] DateTime[~Z, ?TimeRequired] TimeZoneAnnotationopt Annotationsopt
    //      [+Zoned] DateTime[+Z, ?TimeRequired] TimeZoneAnnotation Annotationsopt
    auto plainDate = parseDate(buffer, TemporalDateFormat::Date);
    if (!plainDate)
        return std::nullopt;
    if (buffer.atEnd())
        return std::tuple { WTF::move(plainDate.value()), std::nullopt, std::nullopt, std::nullopt };

    std::optional<PlainTime> plainTimeOptional = std::nullopt;
    std::optional<TimeZoneRecord> timeZoneOptional = std::nullopt;

    if (*buffer == ' ' || *buffer == 'T' || *buffer == 't') {
        buffer.advance();
        auto plainTimeAndTimeZone = parseTime(buffer);
        if (!plainTimeAndTimeZone)
            return std::nullopt;
        auto [plainTime, timeZone] = WTF::move(plainTimeAndTimeZone.value());
        if (buffer.atEnd())
            return std::tuple { WTF::move(plainDate.value()), WTF::move(plainTime), WTF::move(timeZone), std::nullopt };
        plainTimeOptional = plainTime;
        timeZoneOptional = timeZone;
    }

    if (!timeZoneOptional) {
        if (canBeTimeZoneAnnotation(buffer, *buffer))
            timeZoneOptional = parseTimeZone(buffer, false);
    }

    if (buffer.atEnd())
        return std::tuple { WTF::move(plainDate.value()), WTF::move(plainTimeOptional), WTF::move(timeZoneOptional), std::nullopt };

    std::optional<CalendarID> calendarOptional;
    if (canBeRFC9557Annotation(buffer)) {
        auto calendar = parseCalendar(buffer);
        if (!calendar)
            return std::nullopt;
        if (calendar->size() > 0) // Ignore non-calendar annotations
            calendarOptional = WTF::move(calendar.value()[0]);
    }

    return std::tuple { WTF::move(plainDate.value()), WTF::move(plainTimeOptional), WTF::move(timeZoneOptional), WTF::move(calendarOptional) };

}

std::optional<std::tuple<PlainDate, std::optional<PlainTime>, std::optional<TimeZoneRecord>, std::optional<CalendarID>>> parseTemporalDateTimeString(StringView string)
{
    return readCharactersForParsing(string, [](auto buffer) -> std::optional<std::tuple<PlainDate, std::optional<PlainTime>, std::optional<TimeZoneRecord>, std::optional<CalendarID>>> {
        auto result = parseTemporalDateTimeString(buffer);
        if (!buffer.atEnd())
            return std::nullopt;
        return result;
    });
}

template<typename CharacterType>
static std::optional<std::tuple<PlainTime, std::optional<TimeZoneRecord>, std::optional<CalendarID>>> parseCalendarTime(StringParsingBuffer<CharacterType>& buffer)
{
    // https://tc39.es/proposal-temporal/#prod-CalendarTime
    // CalendarTime :
    //     TimeDesignator TimeSpec TimeZone[opt] Calendar[opt]
    //     TimeSpec TimeZone[opt] Calendar
    //     TimeSpecWithOptionalTimeZoneNotAmbiguous

    if (buffer.atEnd())
        return std::nullopt;

    if (*buffer == 'T' || *buffer == 't')
        buffer.advance();

    auto plainTime = parseTimeSpec(buffer, Second60Mode::Accept);
    if (!plainTime)
        return std::nullopt;
    if (buffer.atEnd())
        return std::tuple { WTF::move(plainTime.value()), std::nullopt, std::nullopt };

    std::optional<TimeZoneRecord> timeZoneOptional;
    if (canBeTimeZone(buffer, *buffer)) {
        auto timeZone = parseTimeZone(buffer, false);
        if (!timeZone)
            return std::nullopt;
        timeZoneOptional = WTF::move(timeZone);
    }

    if (buffer.atEnd())
        return std::tuple { WTF::move(plainTime.value()), WTF::move(timeZoneOptional), std::nullopt };

    std::optional<CalendarID> calendarOptional;
    if (canBeRFC9557Annotation(buffer)) {
        auto calendars = parseCalendar(buffer);
        if (!calendars)
            return std::nullopt;
        if (calendars.value().size() > 0)
            calendarOptional = WTF::move(calendars.value()[0]);
    }

    return std::tuple { WTF::move(plainTime.value()), WTF::move(timeZoneOptional), WTF::move(calendarOptional) };
}

template<typename CharacterType>
static std::optional<std::tuple<PlainDate, std::optional<PlainTime>, std::optional<TimeZoneRecord>, std::optional<CalendarID>>> parseCalendarDateTime(StringParsingBuffer<CharacterType>& buffer, TemporalDateFormat format)
{
    // https://tc39.es/proposal-temporal/#prod-DateTime
    // CalendarDateTime :
    //     DateTime CalendarName[opt]
    //
    auto dateTime = parseDateTime(buffer, format);
    if (!dateTime)
        return std::nullopt;

    auto [plainDate, plainTimeOptional, timeZoneOptional] = WTF::move(dateTime.value());

    std::optional<CalendarID> calendarOptional;
    if (!buffer.atEnd() && canBeRFC9557Annotation(buffer)) {
        auto calendars = parseCalendar(buffer);
        if (!calendars)
            return std::nullopt;
        if (calendars.value().size() > 0)
            calendarOptional = WTF::move(calendars.value()[0]);
    }

    return std::tuple { WTF::move(plainDate), WTF::move(plainTimeOptional), WTF::move(timeZoneOptional), WTF::move(calendarOptional) };
}

std::optional<std::tuple<PlainTime, std::optional<TimeZoneRecord>>> parseTime(StringView string)
{
    return readCharactersForParsing(string, [](auto buffer) -> std::optional<std::tuple<PlainTime, std::optional<TimeZoneRecord>>> {
        auto result = parseTime(buffer);
        if (!buffer.atEnd())
            return std::nullopt;
        return result;
    });
}

template<typename CharacterType>
static unsigned lengthRemainingBeforeAnnotation(StringParsingBuffer<CharacterType>& buffer)
{
    unsigned length = 0;
    while (length < buffer.lengthRemaining()) {
        if (buffer[length] == '[')
            break;
        length++;
    }
    return length;
}

template<typename CharacterType>
static bool isAmbiguousCalendarTime(StringParsingBuffer<CharacterType>& buffer)
{
    // Time zone or calendar annotations don't disambiguate,
    // so we consider the length of the buffer without annotations if any are present
    auto length = lengthRemainingBeforeAnnotation(buffer);
    ASSERT(length > 1);

    // There is no ambiguity if we have a TimeDesignator.
    if (toASCIIUpper(*buffer) == 'T')
        return false;

    // The string is known to be valid as `TimeSpec TimeZone[opt]`, so DateExtendedYear and TwoDashes are not possible.
    // Actual possibilities are `DateFourDigitYear -[opt] DateMonth` and `DateMonth -[opt] DateDay`, i.e. YYYY-MM, YYYYMM, MM-DD, MMDD.
    ASSERT(isASCIIDigit(buffer[0]) && isASCIIDigit(buffer[1]));

    unsigned monthPartLength = 2;
    switch (length) {
    case 7:
        if (!isASCIIDigit(buffer[2]) || !isASCIIDigit(buffer[3]) || buffer[4] != '-' || !isASCIIDigit(buffer[5]) || !isASCIIDigit(buffer[6]))
            return false;
        buffer.advanceBy(5);
        break;
    case 6:
        if (!isASCIIDigit(buffer[2]) || !isASCIIDigit(buffer[3]) || !isASCIIDigit(buffer[4]) || !isASCIIDigit(buffer[5]))
            return false;
        buffer.advanceBy(4);
        break;
    case 5:
        if (buffer[2] != '-' || !isASCIIDigit(buffer[3]) || !isASCIIDigit(buffer[4]))
            return false;
        monthPartLength++;
        break;
    case 4:
        if (!isASCIIDigit(buffer[2]) || !isASCIIDigit(buffer[3]))
            return false;
        break;
    default:
        return false;
    }

    // Any YYYY is valid, we just need to check the MM and DD.
    unsigned month = (buffer[0] - '0') * 10 + (buffer[1] - '0');
    if (!month || month > 12)
        return false;

    buffer.advanceBy(monthPartLength);
    if (buffer.hasCharactersRemaining()) {
        if (isASCIIDigit(buffer[0]) && isASCIIDigit(buffer[1])) {
            auto day = (buffer[0] - '0') * 10 + (buffer[1] - '0');
            if (!day || day > daysInMonth(month))
                return false;
        }
    }

    return true;
}

std::optional<std::tuple<PlainTime, std::optional<TimeZoneRecord>, std::optional<CalendarID>>> parseCalendarTime(StringView string)
{
    auto tuple = readCharactersForParsing(string, [](auto buffer) -> std::optional<std::tuple<PlainTime, std::optional<TimeZoneRecord>, std::optional<CalendarID>>> {
        auto result = parseCalendarTime(buffer);
        if (!buffer.atEnd())
            return std::nullopt;
        return result;
    });

    // We need to verify that the parse isn't ambiguous with DateSpecYearMonth or DateSpecMonthDay.
    if (tuple) {
        if (readCharactersForParsing(string, [](auto buffer) -> bool { return isAmbiguousCalendarTime(buffer); }))
            return std::nullopt;
    }

    return tuple;
}

std::optional<std::tuple<PlainDate, std::optional<PlainTime>, std::optional<TimeZoneRecord>>> parseDateTime(StringView string, TemporalDateFormat format)
{
    return readCharactersForParsing(string, [format](auto buffer) -> std::optional<std::tuple<PlainDate, std::optional<PlainTime>, std::optional<TimeZoneRecord>>> {
        auto result = parseDateTime(buffer, format);
        if (!buffer.atEnd())
            return std::nullopt;
        return result;
    });
}

std::optional<std::tuple<PlainDate, std::optional<PlainTime>, std::optional<TimeZoneRecord>, std::optional<CalendarID>>> parseCalendarDateTime(StringView string, TemporalDateFormat format)
{
    return readCharactersForParsing(string, [format](auto buffer) -> std::optional<std::tuple<PlainDate, std::optional<PlainTime>, std::optional<TimeZoneRecord>, std::optional<CalendarID>>> {
        auto result = parseCalendarDateTime(buffer, format);
        if (!buffer.atEnd())
            return std::nullopt;
        return result;
    });
}

std::optional<ExactTime> parseInstant(StringView string)
{
    // https://tc39.es/proposal-temporal/#prod-TemporalInstantString
    // TemporalInstantString :
    //     Date TimeZoneOffsetRequired
    //     Date DateTimeSeparator TimeSpec TimeZoneOffsetRequired

    // https://tc39.es/proposal-temporal/#prod-TimeZoneOffsetRequired
    // TimeZoneOffsetRequired :
    //     TimeZoneUTCOffset TimeZoneBracketedAnnotation_opt

    return readCharactersForParsing(string, [](auto buffer) -> std::optional<ExactTime> {
        auto datetime = parseCalendarDateTime(buffer, TemporalDateFormat::Date);
        if (!datetime)
            return std::nullopt;
        auto [plainDate, plainTimeOptional, timeZoneOptional, calendarOptional] = WTF::move(datetime.value());
        if (!timeZoneOptional || (!timeZoneOptional->m_z && !timeZoneOptional->m_offset))
            return std::nullopt;
        if (!buffer.atEnd())
            return std::nullopt;

        PlainTime plainTime = plainTimeOptional.value_or(PlainTime());

        int64_t offset = 0;
        if (!timeZoneOptional->m_z) {
            if (timeZoneOptional->m_offset)
                offset = timeZoneOptional->m_offset->m_offset;
            else
                return std::nullopt;
        }
        return { ExactTime::fromISOPartsAndOffset(plainDate.year(), plainDate.month(), plainDate.day(), plainTime.hour(), plainTime.minute(), plainTime.second(), plainTime.millisecond(), plainTime.microsecond(), plainTime.nanosecond(), offset) };
    });
}

uint8_t dayOfWeek(PlainDate plainDate)
{
    Int128 dateDays = static_cast<Int128>(dateToDaysFrom1970(plainDate.year(), plainDate.month() - 1, plainDate.day()));
    int weekDay = static_cast<int>((dateDays + 4) % 7);
    if (weekDay < 0)
        weekDay += 7;
    return !weekDay ? 7 : weekDay;
}

uint16_t dayOfYear(PlainDate plainDate)
{
    return dayInYear(plainDate.year(), plainDate.month() - 1, plainDate.day()) + 1; // Always start with 1 (1/1 is 1).
}

uint8_t weekOfYear(PlainDate plainDate)
{
    int32_t dayOfYear = ISO8601::dayOfYear(plainDate);
    int32_t dayOfWeek = ISO8601::dayOfWeek(plainDate);

    // ISO week 1 is the week containing the first Thursday (4) of the year.
    // https://en.wikipedia.org/wiki/ISO_week_date#Algorithms
    int32_t week = (dayOfYear - dayOfWeek + 10) / 7;
    if (week <= 0) {
        // Previous year's last week. Thus, 52 or 53 weeks. Getting weeks in the previous year.
        //
        // https://en.wikipedia.org/wiki/ISO_week_date#Weeks_per_year
        // > The long years, with 53 weeks in them, can be described by any of the following equivalent definitions:
        // >  - any year ending on Thursday (D, ED) and any leap year ending on Friday (DC)

        int32_t dayOfWeekForJanuaryFirst = ISO8601::dayOfWeek(PlainDate { plainDate.year(), 1, 1 });

        // Any year ending on Thursday (D, ED) -> this year's 1/1 is Friday.
        if (dayOfWeekForJanuaryFirst == 5)
            return 53;

        // Any leap year ending on Friday (DC) -> this year's 1/1 is Saturday and previous year is a leap year.
        if (dayOfWeekForJanuaryFirst == 6 && isLeapYear(plainDate.year() - 1))
            return 53;

        return 52;
    }

    if (week == 53) {
        // Check whether this is in next year's week 1.
        if ((daysInYear(plainDate.year()) - dayOfYear) < (4 - dayOfWeek))
            return 1;
    }

    return week;
}

static constexpr uint8_t daysInMonths[2][12] = {
    { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
    { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
};

// https://tc39.es/proposal-temporal/#sec-temporal-isodaysinmonth
uint8_t daysInMonth(int32_t year, uint8_t month)
{
    return daysInMonths[isLeapYear(year)][month - 1];
}

uint8_t daysInMonth(uint8_t month)
{
    constexpr unsigned isLeapYear = 1;
    return daysInMonths[isLeapYear][month - 1];
}

String formatTimeZone(VM& vm, TimeZone tz, bool intlDateTimeFormat)
{
    auto displayName = tz.getDisplayName();
    if (displayName)
        return displayName.value();

    if (tz.isUTC()) {
        if (intlDateTimeFormat && tz.isOffset())
            return "GMT"_s;
        return "UTC"_s;
    }
    if (tz.isOffset())
        return formatUTCOffsetNanoseconds(tz.offsetNanoseconds(), intlDateTimeFormat);
    auto timeZoneName = vm.timeZoneCache.getTimeZoneNameFromID(tz.asID());
    ASSERT(timeZoneName);
    return timeZoneName.value();
}

// https://tc39.es/proposal-temporal/#sec-temporal-formatfractionalseconds
static String formatFractionalSeconds(int64_t subSecondNanoseconds, TemporalFractionalSecondDigits precision)
{
    if (precision == TemporalFractionalSecondDigits::Auto) {
        if (!subSecondNanoseconds)
            return ""_s;
        // Since nsPerSecond is 1000000000, stringified nanoseconds takes at most 9 characters (999999999).
        WTF::Vector<Latin1Character, 9> fraction = numberToStringUnsigned<Vector<Latin1Character, 9>>(subSecondNanoseconds);
        unsigned paddingLength = 9 - fraction.size();
        unsigned index = fraction.size();
        std::optional<unsigned> validLength;
        while (index--) {
            if (fraction[index] != '0') {
                validLength = index + 1;
                break;
            }
        }
        if (validLength)
            fraction.shrink(validLength.value());
        else
            fraction.clear();
        return makeString('.', pad('0', paddingLength, emptyString()), fraction);
    }
    if (precision == TemporalFractionalSecondDigits::Zero)
        return ""_s;
    StringBuilder fractionString;
    fractionString.append("."_s);
    fractionString.append(pad('0', 9, subSecondNanoseconds));
    fractionString.shrink(static_cast<unsigned>(precision) + 1);
    return fractionString.toString();
}

// https://tc39.es/proposal-temporal/#sec-temporal-formattimestring
String formatTimeString(char sign, int64_t hour, int64_t minute, int64_t second, int64_t subSecondNanoseconds,
    std::optional<TemporalFractionalSecondDigits> precision, std::optional<bool> isSeparated)
{
    auto separator = isSeparated && !(isSeparated.value()) ? ""_s : ":"_s;
    auto hh = pad('0', 2, hour);
    auto mm = pad('0', 2, minute);
    if (!precision)
        return makeString(sign, hh, separator, mm);
    auto ss = pad('0', 2, second);
    auto subSecondsPart = formatFractionalSeconds(subSecondNanoseconds, precision.value());
    return makeString(sign, hh, separator, mm, separator, ss, subSecondsPart);
}

// https://tc39.es/proposal-temporal/#sec-temporal-formatutcoffsetnanoseconds
String formatUTCOffsetNanoseconds(int64_t offsetNanoseconds, bool isIntlDateTimeFormat)
{
    auto sign = offsetNanoseconds >= 0 ? '+' : '-';
    int64_t absoluteNanoseconds = std::abs(offsetNanoseconds);
    Int128 divisor = 3600 * 1000000000ll;
    auto hour = absoluteNanoseconds / divisor;
    if (isIntlDateTimeFormat)
        return makeString("GMT"_s, sign, static_cast<int64_t>(hour)); 
    divisor = 60 * 1000000000ll;
    auto minute = (absoluteNanoseconds / divisor) % 60;
    divisor = 1000000000ll;
    auto second = (absoluteNanoseconds / divisor) % 60;
    auto subSecondNanoseconds = absoluteNanoseconds % (static_cast<int64_t>(divisor));
    std::optional<TemporalFractionalSecondDigits> precision = std::nullopt;
    if (!(!second && !subSecondNanoseconds))
        precision = TemporalFractionalSecondDigits::Auto;
    return formatTimeString(sign, static_cast<int64_t>(hour), static_cast<int64_t>(minute), static_cast<int64_t>(second), static_cast<int64_t>(subSecondNanoseconds), precision, std::nullopt);
}

String temporalTimeToString(PlainTime plainTime, std::tuple<Precision, unsigned> precision)
{
    auto [precisionType, precisionValue] = precision;
    ASSERT(precisionType == Precision::Auto || precisionValue < 10);
    if (precisionType == Precision::Minute)
        return makeString(pad('0', 2, plainTime.hour()), ':', pad('0', 2, plainTime.minute()));

    int64_t milliseconds = plainTime.millisecond();
    int64_t microseconds = plainTime.microsecond();
    int64_t nanoseconds = plainTime.nanosecond();
    int64_t fractionNanoseconds = milliseconds * nsPerMillisecond + microseconds * nsPerMicrosecond + nanoseconds;
    if (precisionType == Precision::Auto) {
        if (!fractionNanoseconds)
            return makeString(pad('0', 2, plainTime.hour()), ':', pad('0', 2, plainTime.minute()), ':', pad('0', 2, plainTime.second()));
        auto fraction = numberToStringUnsigned<Vector<Latin1Character, 9>>(fractionNanoseconds);
        unsigned paddingLength = 9 - fraction.size();
        unsigned index = fraction.size();
        std::optional<unsigned> validLength;
        while (index--) {
            if (fraction[index] != '0') {
                validLength = index + 1;
                break;
            }
        }
        if (validLength)
            fraction.shrink(validLength.value());
        else
            fraction.clear();
        return makeString(pad('0', 2, plainTime.hour()), ':', pad('0', 2, plainTime.minute()), ':', pad('0', 2, plainTime.second()), '.', pad('0', paddingLength, emptyString()), fraction);
    }
    if (!precisionValue)
        return makeString(pad('0', 2, plainTime.hour()), ':', pad('0', 2, plainTime.minute()), ':', pad('0', 2, plainTime.second()));
    auto fraction = numberToStringUnsigned<Vector<Latin1Character, 9>>(fractionNanoseconds);
    unsigned paddingLength = 9 - fraction.size();
    paddingLength = std::min(paddingLength, precisionValue);
    precisionValue -= paddingLength;
    fraction.resize(precisionValue);
    return makeString(pad('0', 2, plainTime.hour()), ':', pad('0', 2, plainTime.minute()), ':', pad('0', 2, plainTime.second()), '.', pad('0', paddingLength, emptyString()), fraction);
}

static String temporalDateToString(int32_t year, int32_t month)
{
    // If we're printing a date, it should be within range
    ASSERT(isYearWithinLimits(year));

    String prefix;
    auto yearDigits = 4;
    if (year < 0 || year > 9999) {
        prefix = year < 0 ? "-"_s : "+"_s;
        yearDigits = 6;
        year = std::abs(year);
    }

    return makeString(prefix, pad('0', yearDigits, year), '-', pad('0', 2, month));
}

static String temporalDateToString(int32_t year, int32_t month, int32_t day)
{
    auto first = temporalDateToString(year, month);
    return makeString(first, '-', pad('0', 2, day));
}

String temporalDateTimeToString(PlainDate plainDate, PlainTime plainTime, std::tuple<Precision, unsigned> precision)
{
    return makeString(temporalDateToString(plainDate), 'T', temporalTimeToString(plainTime, precision));
}

String temporalDateToString(PlainDate plainDate)
{
    return temporalDateToString(plainDate.year(), plainDate.month(), plainDate.day());
}

String temporalYearMonthToString(PlainYearMonth plainYearMonth, StringView calendarName)
{
    if (calendarName == "always"_s) {
        // FIXME: Include the correct calendar ID when calendars are fully implemented.
        return makeString(temporalDateToString(plainYearMonth.isoPlainDate()), "[u-ca=iso8601]"_s);
    }
    return temporalDateToString(plainYearMonth.year(), plainYearMonth.month());
}

String temporalMonthDayToString(PlainMonthDay plainMonthDay, StringView calendarName)
{
    if (calendarName == "always"_s) {
        // FIXME: print the correct calendar ID when calendars are fully implemented
        auto first = temporalDateToString(plainMonthDay.isoPlainDate());
        return makeString(first, "[u-ca=iso8601]"_s);
    }

    return makeString(pad('0', 2, plainMonthDay.month()), '-', pad('0', 2, plainMonthDay.day()));
}

String monthCode(uint32_t month)
{
    return makeString('M', pad('0', 2, month));
}

// https://tc39.es/proposal-temporal/#sec-temporal-parsemonthcode
std::optional<ParsedMonthCode> parseMonthCode(StringView monthCode)
{
    // Allow leap month marker (e.g. "M05L"), even though it doesn't apply to ISO8601 calendar
    if (monthCode.length() < 3 || monthCode.length() > 4 || !monthCode.startsWith('M') || !isASCIIDigit(monthCode[2]))
        return { };

    // 4th code unit must be 'L' because the month code is valid
    auto isLeapMonth = monthCode.length() == 4;

    uint8_t monthNumber = monthCode[2] - '0';
    monthNumber += (monthCode[1] - '0') * 10;

    return ParsedMonthCode { monthNumber, isLeapMonth };
}

ExactTime ExactTime::fromISOPartsAndOffset(int32_t year, uint8_t month, uint8_t day, unsigned hour, unsigned minute, unsigned second, unsigned millisecond, unsigned microsecond, unsigned nanosecond, int64_t offset)
{
    ASSERT(month >= 1 && month <= 12);
    ASSERT(day >= 1 && day <= 31);
    ASSERT(hour <= 23);
    ASSERT(minute <= 59);
    ASSERT(second <= 59);
    ASSERT(millisecond <= 999);
    ASSERT(microsecond <= 999);
    ASSERT(nanosecond <= 999);

    Int128 dateDays = ISO8601::dateToDaysFrom1970(year, month - 1, day);
    Int128 utcNanoseconds = dateDays * nsPerDay + hour * nsPerHour + minute * nsPerMinute + second * nsPerSecond + millisecond * nsPerMillisecond + microsecond * nsPerMicrosecond + nanosecond;
    return ExactTime { utcNanoseconds - offset };
}

} // namespace ISO8601

CheckedInt128 checkedCastDoubleToInt128(double n)
{
    // Based on __fixdfti() and __fixunsdfti() from compiler_rt:
    // https://github.com/llvm/llvm-project/blob/f3671de5500ff1f8210419226a9603a7d83b1a31/compiler-rt/lib/builtins/fp_fixint_impl.inc
    // https://github.com/llvm/llvm-project/blob/f3671de5500ff1f8210419226a9603a7d83b1a31/compiler-rt/lib/builtins/fp_fixuint_impl.inc

    static constexpr int significandBits = std::numeric_limits<double>::digits - 1;
    static constexpr int exponentBits = std::numeric_limits<uint64_t>::digits - std::numeric_limits<double>::digits;
    static constexpr int exponentBias = std::numeric_limits<double>::max_exponent - 1;
    static constexpr uint64_t implicitBit = uint64_t { 1 } << significandBits;
    static constexpr uint64_t significandMask = implicitBit - uint64_t { 1 };
    static constexpr uint64_t signMask = uint64_t { 1 } << (significandBits + exponentBits);
    static constexpr uint64_t absMask = signMask - uint64_t { 1 };

    // Break n into sign, exponent, significand parts.
    const uint64_t bits = *reinterpret_cast<uint64_t*>(&n);
    const uint64_t nAbs = bits & absMask;
    const int sign = bits & signMask ? -1 : 1;
    const int exponent = (nAbs >> significandBits) - exponentBias;
    const uint64_t significand = (nAbs & significandMask) | implicitBit;

    // If exponent is negative, the result is zero.
    if (exponent < 0)
        return { 0 };

    // If the value is too large for the integer type, overflow.
    if (exponent >= 128)
        return { WTF::ResultOverflowed };

    // If 0 <= exponent < significandBits, right shift to get the result.
    // Otherwise, shift left.
    Int128 result { significand };
    if (exponent < significandBits)
        result >>= significandBits - exponent;
    else
        result <<= exponent - significandBits;
    result *= sign;
    return { result };
}

namespace ISO8601 {

template<TemporalUnit unit>
std::optional<Int128> Duration::totalNanoseconds() const
{
    ASSERT(unit >= TemporalUnit::Day);

    CheckedInt128 resultNs { 0 };

    if constexpr (unit <= TemporalUnit::Day) {
        CheckedInt128 days = checkedCastDoubleToInt128(this->days());
        resultNs += days * ExactTime::nsPerDay;
    }
    if constexpr (unit <= TemporalUnit::Hour) {
        CheckedInt128 hours = checkedCastDoubleToInt128(this->hours());
        resultNs += hours * ExactTime::nsPerHour;
    }
    if constexpr (unit <= TemporalUnit::Minute) {
        CheckedInt128 minutes = checkedCastDoubleToInt128(this->minutes());
        resultNs += minutes * ExactTime::nsPerMinute;
    }
    if constexpr (unit <= TemporalUnit::Second) {
        CheckedInt128 seconds = checkedCastDoubleToInt128(this->seconds());
        resultNs += seconds * ExactTime::nsPerSecond;
    }
    if constexpr (unit <= TemporalUnit::Millisecond) {
        CheckedInt128 milliseconds = checkedCastDoubleToInt128(this->milliseconds());
        resultNs += milliseconds * ExactTime::nsPerMillisecond;
    }
    if constexpr (unit <= TemporalUnit::Microsecond) {
        CheckedInt128 microseconds = checkedCastDoubleToInt128(this->microseconds());
        resultNs += microseconds * ExactTime::nsPerMicrosecond;
    }
    if constexpr (unit <= TemporalUnit::Nanosecond)
        resultNs += checkedCastDoubleToInt128(this->nanoseconds());

    if (resultNs.hasOverflowed())
        return std::nullopt;

    return resultNs;
}
template std::optional<Int128> Duration::totalNanoseconds<TemporalUnit::Day>() const;
template std::optional<Int128> Duration::totalNanoseconds<TemporalUnit::Second>() const;
template std::optional<Int128> Duration::totalNanoseconds<TemporalUnit::Millisecond>() const;
template std::optional<Int128> Duration::totalNanoseconds<TemporalUnit::Microsecond>() const;

// IsValidDuration ( years, months, weeks, days, hours, minutes, seconds, milliseconds, microseconds, nanoseconds )
// https://tc39.es/proposal-temporal/#sec-temporal-isvalidduration
bool isValidDuration(const Duration& duration)
{
    int sign = 0;
    for (auto value : duration) {
        if (!std::isfinite(value) || (value < 0 && sign > 0) || (value > 0 && sign < 0))
            return false;

        if (!sign && value)
            sign = value > 0 ? 1 : -1;
    }

    // 3. If abs(years) ≥ 2^32, return false.
    // 4. If abs(months) ≥ 2^32, return false.
    // 5. If abs(weeks) ≥ 2^32, return false.
    constexpr double limit = 1ULL << 32;
    if (std::abs(duration[TemporalUnit::Year]) >= limit || std::abs(duration[TemporalUnit::Month]) >= limit || std::abs(duration[TemporalUnit::Week]) >= limit)
        return false;

    // 6. Let normalizedSeconds be days × 86,400 + hours × 3600 + minutes × 60 + seconds + ℝ(𝔽(milliseconds)) × 10^-3 + ℝ(𝔽(microseconds)) × 10^-6 + ℝ(𝔽(nanoseconds)) × 10^-9.
    auto normalizedNanoseconds = duration.totalNanoseconds<TemporalUnit::Day>();
    // 8. If abs(normalizedSeconds) ≥ 2^53, return false.
    constexpr Int128 nanosecondsLimit = (Int128(1) << 53) * 1000000000;
    if (!normalizedNanoseconds || absInt128(normalizedNanoseconds.value()) >= nanosecondsLimit)
        return false;

    return true;
}

std::optional<ExactTime> ExactTime::add(Duration duration) const
{
    ASSERT(!duration.years());
    ASSERT(!duration.months());
    ASSERT(!duration.weeks());
    ASSERT(!duration.days());

    CheckedInt128 resultNs { m_epochNanoseconds };

    // The duration's hours, minutes, seconds, and milliseconds should be
    // able to be cast into a 64-bit int. 2*1e8 24-hour days is the maximum
    // time span for exact time, so if we already know that the duration exceeds
    // that, then we can bail out.

    CheckedInt128 hours = checkedCastDoubleToInt128(duration.hours());
    resultNs += hours * ExactTime::nsPerHour;
    CheckedInt128 minutes = checkedCastDoubleToInt128(duration.minutes());
    resultNs += minutes * ExactTime::nsPerMinute;
    CheckedInt128 seconds = checkedCastDoubleToInt128(duration.seconds());
    resultNs += seconds * ExactTime::nsPerSecond;
    CheckedInt128 milliseconds = checkedCastDoubleToInt128(duration.milliseconds());
    resultNs += milliseconds * ExactTime::nsPerMillisecond;
    CheckedInt128 microseconds = checkedCastDoubleToInt128(duration.microseconds());
    resultNs += microseconds * ExactTime::nsPerMicrosecond;
    resultNs += checkedCastDoubleToInt128(duration.nanoseconds());
    if (resultNs.hasOverflowed())
        return std::nullopt;

    ExactTime result { resultNs.value() };
    if (!result.isValid())
        return std::nullopt;
    return result;
}

// https://tc39.es/proposal-temporal/#sec-temporal-roundtemporalinstant
Int128 roundTemporalInstant(Int128 ns, unsigned increment, TemporalUnit unit, RoundingMode roundingMode)
{
    auto unitLength = lengthInNanoseconds(unit);
    auto incrementNs = increment * unitLength;
    return roundNumberToIncrementAsIfPositive(ns, incrementNs, roundingMode);
}

// https://tc39.es/proposal-temporal/#sec-validatetemporalroundingincrement
static void validateTemporalRoundingIncrement(JSGlobalObject* globalObject, unsigned increment,
    Int128 dividend, Inclusivity inclusive)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    Int128 maximum = 0;
    switch (inclusive) {
    case Inclusivity::Inclusive:
        maximum = dividend;
        break;
    case Inclusivity::Exclusive:
        ASSERT(dividend > 1);
        maximum = dividend - 1;
        break;
    }
    if (increment > maximum)
        throwRangeError(globalObject, scope, "Rounding increment exceeds maximum value"_s);
    else if (dividend % increment)
        throwRangeError(globalObject, scope, "Rounding increment does not divide evenly into maximum value"_s);
}

// https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.round
// (Steps 10-17 only)
ExactTime ExactTime::round(JSGlobalObject* globalObject, unsigned increment,
    TemporalUnit unit, RoundingMode roundingMode) const
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    Int128 maximum = 0;
    switch (unit) {
    case TemporalUnit::Hour: maximum = hoursPerDay; break;
    case TemporalUnit::Minute: maximum = minutesPerHour * hoursPerDay; break;
    case TemporalUnit::Second: maximum = secondsPerMinute * minutesPerHour * hoursPerDay; break;
    case TemporalUnit::Millisecond: maximum = msPerDay; break;
    case TemporalUnit::Microsecond: maximum = msPerDay * 1000; break;
    case TemporalUnit::Nanosecond: maximum = nsPerDay; break;
    default:
        RELEASE_ASSERT_NOT_REACHED();
    }
    validateTemporalRoundingIncrement(globalObject, increment, maximum, Inclusivity::Inclusive);
    RETURN_IF_EXCEPTION(scope, { });
    auto roundedNs = roundTemporalInstant(m_epochNanoseconds, increment, unit, roundingMode);
    return ExactTime { roundedNs };
}

// https://tc39.es/proposal-temporal/#sec-temporal-roundtimedurationtoincrement
Int128 roundTimeDurationToIncrement(JSGlobalObject* globalObject, Int128 d, Int128 increment,
    RoundingMode roundingMode)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    Int128 rounded = roundNumberToIncrementInt128(d, increment, roundingMode);
    if (absInt128(rounded) > InternalDuration::maxTimeDuration) {
        throwRangeError(globalObject, scope, "Rounded time duration exceeds maximum"_s);
        return 0;
    }
    return rounded;
}

// https://tc39.es/proposal-temporal/#sec-temporal-roundtimeduration
Int128 roundTimeDuration(JSGlobalObject* globalObject, Int128 timeDuration, unsigned increment, TemporalUnit unit, RoundingMode roundingMode)
{
    auto divisor = lengthInNanoseconds(unit);

    return roundTimeDurationToIncrement(globalObject, timeDuration,
        (divisor * increment), roundingMode);
}

// https://tc39.es/proposal-temporal/#sec-temporal-datedurationsign
int32_t dateDurationSign(const Duration& d)
{
    if (d.years() > 0)
        return 1;
    if (d.years() < 0)
        return -1;
    if (d.months() > 0)
        return 1;
    if (d.months() < 0)
        return -1;
    if (d.weeks() > 0)
        return 1;
    if (d.weeks() < 0)
        return -1;
    if (d.days() > 0)
        return 1;
    if (d.days() < 0)
        return -1;
    return 0;
}

// https://tc39.es/proposal-temporal/#sec-temporal-internaldurationsign
int32_t ISO8601::InternalDuration::sign() const
{
    int32_t sign = dateDurationSign(m_dateDuration);
    if (sign)
        return sign;
    return timeDurationSign();
}

// https://tc39.es/proposal-temporal/#sec-temporal-combinedateandtimeduration
InternalDuration InternalDuration::combineDateAndTimeDuration(Duration dateDuration, Int128 timeDuration)
{
    int32_t dateSign = dateDurationSign(dateDuration);
    int32_t timeSign = timeDuration < 0 ? -1 : timeDuration > 0 ? 1 : 0;
    bool signsDiffer = dateSign && timeSign && (dateSign != timeSign);
    ASSERT_UNUSED(signsDiffer, !signsDiffer);
    return InternalDuration { WTF::move(dateDuration), timeDuration };
}

// DifferenceInstant ( ns1, ns2, roundingIncrement, smallestUnit, roundingMode )
// https://tc39.es/proposal-temporal/#sec-temporal-differenceinstant
InternalDuration ExactTime::difference(JSGlobalObject* globalObject, ExactTime other, unsigned roundingIncrement, TemporalUnit smallestUnit, RoundingMode roundingMode) const
{
    Int128 timeDuration = other.m_epochNanoseconds - m_epochNanoseconds;
    timeDuration = roundTimeDuration(globalObject, timeDuration, roundingIncrement, smallestUnit, roundingMode);
    return InternalDuration::combineDateAndTimeDuration(ISO8601::Duration(), timeDuration);
}

ExactTime ExactTime::now()
{
    return ExactTime { WTF::currentTimeInNanoseconds() };
}

// https://tc39.es/proposal-temporal/#sec-temporal-isodatetimewithinlimits
bool isDateTimeWithinLimits(int32_t year, uint8_t month, uint8_t day, unsigned hour, unsigned minute, unsigned second, unsigned millisecond, unsigned microsecond, unsigned nanosecond)
{
    Int128 nanoseconds = ExactTime::fromISOPartsAndOffset(year, month, day, hour, minute, second, millisecond, microsecond, nanosecond, 0).epochNanoseconds();
    if (nanoseconds <= (ExactTime::minValue - ExactTime::nsPerDay))
        return false;
    if (nanoseconds >= (ExactTime::maxValue + ExactTime::nsPerDay))
        return false;
    return true;
}

// https://tc39.es/proposal-temporal/#sec-checkisodaysrange
void checkISODaysRange(JSGlobalObject* globalObject, ISO8601::PlainDate isoDate)
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    Int128 val = ISO8601::makeDay(isoDate.year(), isoDate.month() - 1, isoDate.day());
    if (absInt128(val) > 100000000)
        throwRangeError(globalObject, scope, "date/time value is outside the supported range"_s);
}

// https://tc39.es/ecma262/#sec-getnamedtimezoneoffsetnanoseconds
Int128 getNamedTimeZoneOffsetNanoseconds(JSGlobalObject* globalObject,
    TimeZoneID timeZoneIdentifier, ExactTime epochNanoseconds)
{
    VM& vm = globalObject->vm();

    if (timeZoneIdentifier == utcTimeZoneID())
        return 0;
    // https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.epochmilliseconds
    // Let ms be floor(ℝ(ns) / 10**6).
    Int128 epochMilliseconds = epochNanoseconds.floorEpochMilliseconds();
    Int128 offsetMilliseconds = vm.timeZoneCache.getNamedTimeZoneOffsetMilliseconds(globalObject, timeZoneIdentifier, epochMilliseconds);
    return offsetMilliseconds * 1'000'000;
}

// https://tc39.es/proposal-temporal/#sec-getutcepochnanoseconds
Int128 getUTCEpochNanoseconds(PlainDateTime isoDateTime)
{
    auto isoDate = isoDateTime.date();
    auto isoTime = isoDateTime.time();
    Int128 date = makeDay(isoDate.year(), isoDate.month() - 1, isoDate.day());
    Int128 time = makeTime(isoTime.hour(), isoTime.minute(), isoTime.second(), isoTime.millisecond());
    Int128 ms = makeDate(date, time);
    return (ms * 1000000
        + (static_cast<Int128>(isoTime.microsecond())) * 1000
        + (static_cast<Int128>(isoTime.nanosecond())));
}

int32_t compareTimeRecord(const PlainTime& time1, const PlainTime& time2)
{
    if (time1.hour() > time2.hour())
        return 1;
    if (time1.hour() < time2.hour())
        return -1;
    if (time1.minute() > time2.minute())
        return 1;
    if (time1.minute() < time2.minute())
        return -1;
    if (time1.second() > time2.second())
        return 1;
    if (time1.second() < time2.second())
        return -1;
    if (time1.millisecond() > time2.millisecond())
        return 1;
    if (time1.millisecond() < time2.millisecond())
        return -1;
    if (time1.microsecond() > time2.microsecond())
        return 1;
    if (time1.microsecond() < time2.microsecond())
        return -1;
    if (time1.nanosecond() > time2.nanosecond())
        return 1;
    if (time1.nanosecond() < time2.nanosecond())
        return -1;
    return 0;
}

} // namespace ISO8601
} // namespace JSC

WTF_ALLOW_UNSAFE_BUFFER_USAGE_END
