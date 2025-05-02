//@ requireOptions("--useTemporal=1")

function shouldBe(actual, expected) {
    if (actual !== expected)
        throw new Error(`expected ${expected} but got ${actual}`);
}

function shouldThrow(func, errorType, message) {
    let error;
    try {
        func();
    } catch (e) {
        error = e;
    }

    if (!(error instanceof errorType))
        throw new Error(`Expected ${errorType.name}!`);
    if (message !== undefined)
        shouldBe(String(error), message);
}

shouldBe(Temporal.ZonedDateTime instanceof Function, true);
shouldBe(Temporal.ZonedDateTime.length, 2);
shouldBe(Object.getOwnPropertyDescriptor(Temporal.ZonedDateTime, 'prototype').writable, false);
shouldBe(Object.getOwnPropertyDescriptor(Temporal.ZonedDateTime, 'prototype').enumerable, false);
shouldBe(Object.getOwnPropertyDescriptor(Temporal.ZonedDateTime, 'prototype').configurable, false);
shouldBe(Temporal.ZonedDateTime.prototype.constructor, Temporal.ZonedDateTime);

const zdt = new Temporal.ZonedDateTime(0n, "UTC");

{
    shouldBe(zdt.year, 1970);
    shouldBe(zdt.month, 1);
    shouldBe(zdt.monthCode, "M01");
    shouldBe(zdt.day, 1);
    shouldBe(zdt.hour, 0);
    shouldBe(zdt.minute, 0);
    shouldBe(zdt.second, 0);
    shouldBe(zdt.millisecond, 0);
    shouldBe(zdt.microsecond, 0);
    shouldBe(zdt.nanosecond, 0);
    shouldBe(zdt.epochMilliseconds, 0);
    shouldBe(zdt.epochNanoseconds, 0n);
    shouldBe(zdt.calendarId, "iso8601");

    shouldThrow(() => new Temporal.ZonedDateTime(0n, null), TypeError);
    shouldThrow(() => new Temporal.ZonedDateTime(864n * 10n ** 19n + 1n, "UTC"), RangeError);
}

{
    shouldBe(zdt.toString(), '1970-01-01T00:00:00+00:00[UTC]');
    shouldBe(zdt.toJSON(), zdt.toString());
    shouldBe(zdt.toLocaleString(), zdt.toString());
}
