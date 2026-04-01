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
#include "TemporalPlainDateTimeConstructor.h"

#include "IntlObjectInlines.h"
#include "JSCInlines.h"
#include "TemporalPlainDateTime.h"
#include "TemporalPlainDateTimePrototype.h"

namespace JSC {

STATIC_ASSERT_IS_TRIVIALLY_DESTRUCTIBLE(TemporalPlainDateTimeConstructor);

static JSC_DECLARE_HOST_FUNCTION(temporalPlainDateTimeConstructorFuncFrom);
static JSC_DECLARE_HOST_FUNCTION(temporalPlainDateTimeConstructorFuncCompare);

}

#include "TemporalPlainDateTimeConstructor.lut.h"

namespace JSC {

const ClassInfo TemporalPlainDateTimeConstructor::s_info = { "Function"_s, &Base::s_info, &temporalPlainDateTimeConstructorTable, nullptr, CREATE_METHOD_TABLE(TemporalPlainDateTimeConstructor) };

/* Source for TemporalPlainDateTimeConstructor.lut.h
@begin temporalPlainDateTimeConstructorTable
  from             temporalPlainDateTimeConstructorFuncFrom             DontEnum|Function 1
  compare          temporalPlainDateTimeConstructorFuncCompare          DontEnum|Function 2
@end
*/

TemporalPlainDateTimeConstructor* TemporalPlainDateTimeConstructor::create(VM& vm, Structure* structure, TemporalPlainDateTimePrototype* plainDateTimePrototype)
{
    auto* constructor = new (NotNull, allocateCell<TemporalPlainDateTimeConstructor>(vm)) TemporalPlainDateTimeConstructor(vm, structure);
    constructor->finishCreation(vm, plainDateTimePrototype);
    return constructor;
}

Structure* TemporalPlainDateTimeConstructor::createStructure(VM& vm, JSGlobalObject* globalObject, JSValue prototype)
{
    return Structure::create(vm, globalObject, prototype, TypeInfo(InternalFunctionType, StructureFlags), info());
}

static JSC_DECLARE_HOST_FUNCTION(callTemporalPlainDateTime);
static JSC_DECLARE_HOST_FUNCTION(constructTemporalPlainDateTime);

TemporalPlainDateTimeConstructor::TemporalPlainDateTimeConstructor(VM& vm, Structure* structure)
    : Base(vm, structure, callTemporalPlainDateTime, constructTemporalPlainDateTime)
{
}

void TemporalPlainDateTimeConstructor::finishCreation(VM& vm, TemporalPlainDateTimePrototype* plainDateTimePrototype)
{
    Base::finishCreation(vm, 3, "PlainDateTime"_s, PropertyAdditionMode::WithoutStructureTransition);
    putDirectWithoutTransition(vm, vm.propertyNames->prototype, plainDateTimePrototype, PropertyAttribute::DontEnum | PropertyAttribute::DontDelete | PropertyAttribute::ReadOnly);
    plainDateTimePrototype->putDirectWithoutTransition(vm, vm.propertyNames->constructor, this, static_cast<unsigned>(PropertyAttribute::DontEnum));
}

JSC_DEFINE_HOST_FUNCTION(constructTemporalPlainDateTime, (JSGlobalObject* globalObject, CallFrame* callFrame))
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    JSObject* newTarget = asObject(callFrame->newTarget());
    Structure* structure = JSC_GET_DERIVED_STRUCTURE(vm, plainDateTimeStructure, newTarget, callFrame->jsCallee());
    RETURN_IF_EXCEPTION(scope, { });

    int32_t year = 0;
    unsigned month = 1;
    unsigned day = 1;
    uint8_t hour = 0;
    uint8_t minute = 0;
    uint8_t second = 0;
    int32_t millisecond = 0;
    int32_t microsecond = 0;
    int32_t nanosecond = 0;

    if (callFrame->argumentCount() < 1)
        return throwVMRangeError(globalObject, scope, "not enough arguments to Temporal.PlainDateTime constructor"_s);

    JSValue arg = callFrame->uncheckedArgument(0);
    double doubleValue = 0;
    if (arg.isUndefined())
        return throwVMRangeError(globalObject, scope, "in Temporal.PlainDateTime constructor, year must be defined"_s);
    doubleValue = arg.toIntegerWithTruncation(globalObject);
    RETURN_IF_EXCEPTION(scope, { });
    if (!std::isfinite(doubleValue))
        return throwVMRangeError(globalObject, scope, "Temporal.PlainDateTime properties must be finite"_s);
    year = static_cast<int32_t>(doubleValue);

    if (callFrame->argumentCount() < 2)
        return throwVMRangeError(globalObject, scope, "not enough arguments to Temporal.PlainDateTime constructor"_s);
    
    arg = callFrame->uncheckedArgument(1);
    if (arg.isUndefined())
        return throwVMRangeError(globalObject, scope, "in Temporal.PlainDateTime constructor, month must be defined"_s);
    doubleValue = arg.toIntegerWithTruncation(globalObject);
    RETURN_IF_EXCEPTION(scope, { });
    if (!std::isfinite(doubleValue))
        return throwVMRangeError(globalObject, scope, "Temporal.PlainDateTime properties must be finite"_s);
    month = static_cast<unsigned>(doubleValue);

    if (callFrame->argumentCount() < 3)
        return throwVMRangeError(globalObject, scope, "not enough arguments to Temporal.PlainDateTime constructor"_s);

    arg = callFrame->uncheckedArgument(2);
    if (arg.isUndefined())
        return throwVMRangeError(globalObject, scope, "in Temporal.PlainDateTime constructor, day must be defined"_s);
    doubleValue = arg.toIntegerWithTruncation(globalObject);
    RETURN_IF_EXCEPTION(scope, { });
    if (!std::isfinite(doubleValue))
        return throwVMRangeError(globalObject, scope, "Temporal.PlainDateTime properties must be finite"_s);
    day = static_cast<unsigned>(doubleValue);

    arg = callFrame->argument(3);
    if (!arg.isUndefined()) {
        doubleValue = arg.toIntegerWithTruncation(globalObject);
        RETURN_IF_EXCEPTION(scope, { });
        if (!std::isfinite(doubleValue))
            return throwVMRangeError(globalObject, scope, "Temporal.PlainDateTime properties must be finite"_s);
        hour = static_cast<uint8_t>(doubleValue);
    }
    arg = callFrame->argument(4);
    if (!arg.isUndefined()) {
        doubleValue = arg.toIntegerWithTruncation(globalObject);
        RETURN_IF_EXCEPTION(scope, { });
        if (!std::isfinite(doubleValue))
            return throwVMRangeError(globalObject, scope, "Temporal.PlainDateTime properties must be finite"_s);
        minute = static_cast<uint8_t>(doubleValue);
    }
    arg = callFrame->argument(5);
    if (!arg.isUndefined()) {
        doubleValue = arg.toIntegerWithTruncation(globalObject);
        RETURN_IF_EXCEPTION(scope, { });
        if (!std::isfinite(doubleValue))
            return throwVMRangeError(globalObject, scope, "Temporal.PlainDateTime properties must be finite"_s);
        second = static_cast<uint8_t>(doubleValue);
    }
    arg = callFrame->argument(6);
    if (!arg.isUndefined()) {
        doubleValue = arg.toIntegerWithTruncation(globalObject);
        RETURN_IF_EXCEPTION(scope, { });
        if (!std::isfinite(doubleValue))
            return throwVMRangeError(globalObject, scope, "Temporal.PlainDateTime properties must be finite"_s);
        millisecond = static_cast<int32_t>(doubleValue);
    }
    arg = callFrame->argument(7);
    if (!arg.isUndefined()) {
        doubleValue = arg.toIntegerWithTruncation(globalObject);
        RETURN_IF_EXCEPTION(scope, { });
        if (!std::isfinite(doubleValue))
            return throwVMRangeError(globalObject, scope, "Temporal.PlainDateTime properties must be finite"_s);
        microsecond = static_cast<int32_t>(doubleValue);
    }
    arg = callFrame->argument(8);
    if (!arg.isUndefined()) {
        doubleValue = arg.toIntegerWithTruncation(globalObject);
        RETURN_IF_EXCEPTION(scope, { });
        if (!std::isfinite(doubleValue))
            return throwVMRangeError(globalObject, scope, "Temporal.PlainDateTime properties must be finite"_s);
        nanosecond = static_cast<int32_t>(doubleValue);
    }

    std::optional<CalendarID> calendarID = std::nullopt;
    if (callFrame->argumentCount() > 9) {
        auto value = callFrame->uncheckedArgument(9);
        if (!value.isUndefined()) {
            if (!value.isString())
                return throwVMTypeError(globalObject, scope, "Temporal.PlainDateTime calendar must be a string"_s);
            auto calendarString = value.toWTFString(globalObject);
            RETURN_IF_EXCEPTION(scope, { });
            std::optional<ISO8601::CalendarID> parsedCalendarString = ISO8601::parseCalendarIdentifier(calendarString);
            if (!parsedCalendarString)
                return throwVMRangeError(globalObject, scope, "invalid calendar in PlainDateTime"_s);
            calendarID = TemporalCalendar::parseTemporalCalendarString(globalObject, StringView(parsedCalendarString.value()));
            RETURN_IF_EXCEPTION(scope, { });
            if (!calendarID)
                return throwVMRangeError(globalObject, scope, "error parsing calendar ID from PlainDateTime"_s);
        }
    }

    ISO8601::PlainDate plainDate = ISO8601::PlainDate(year, month, day);
    ISO8601::PlainTime plainTime = ISO8601::PlainTime(hour, minute, second, millisecond, microsecond, nanosecond);
    ISO8601::PlainDateTime plainDateTime = ISO8601::PlainDateTime(WTF::move(plainDate), WTF::move(plainTime));

    if (calendarID) {
        TemporalCalendar* calendar = TemporalCalendar::create(vm, globalObject->calendarStructure(), calendarID.value());
        RETURN_IF_EXCEPTION(scope, { });
        RELEASE_AND_RETURN(scope, JSValue::encode(TemporalPlainDateTime::tryCreateIfValid(globalObject, structure, WTF::move(plainDateTime), calendar)));
    }
    RELEASE_AND_RETURN(scope, JSValue::encode(TemporalPlainDateTime::tryCreateIfValid(globalObject, structure, WTF::move(plainDateTime), std::nullopt)));
}

JSC_DEFINE_HOST_FUNCTION(callTemporalPlainDateTime, (JSGlobalObject* globalObject, CallFrame*))
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    return JSValue::encode(throwConstructorCannotBeCalledAsFunctionTypeError(globalObject, scope, "PlainDateTime"_s));
}

// https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.from
JSC_DEFINE_HOST_FUNCTION(temporalPlainDateTimeConstructorFuncFrom, (JSGlobalObject* globalObject, CallFrame* callFrame))
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    JSValue itemValue = callFrame->argument(0);

    if (itemValue.inherits<TemporalPlainDateTime>()) {
        // Validate overflow
        JSObject* options = intlGetOptionsObject(globalObject, callFrame->argument(1));
        RETURN_IF_EXCEPTION(scope, { });
        toTemporalOverflow(globalObject, options);
        RETURN_IF_EXCEPTION(scope, { });

        RELEASE_AND_RETURN(scope, JSValue::encode(TemporalPlainDateTime::create(vm, globalObject->plainDateTimeStructure(), jsCast<TemporalPlainDateTime*>(itemValue)->plainDateTime())));
    }

    RELEASE_AND_RETURN(scope, JSValue::encode(TemporalPlainDateTime::from(globalObject, itemValue, callFrame->argument(1))));
}

// https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.compare
JSC_DEFINE_HOST_FUNCTION(temporalPlainDateTimeConstructorFuncCompare, (JSGlobalObject* globalObject, CallFrame* callFrame))
{
    VM& vm = globalObject->vm();
    auto scope = DECLARE_THROW_SCOPE(vm);

    auto* one = TemporalPlainDateTime::from(globalObject, callFrame->argument(0), jsUndefined());
    RETURN_IF_EXCEPTION(scope, { });

    auto* two = TemporalPlainDateTime::from(globalObject, callFrame->argument(1), jsUndefined());
    RETURN_IF_EXCEPTION(scope, { });

    return JSValue::encode(jsNumber(TemporalPlainDateTime::compare(one, two)));
}

} // namespace JSC
