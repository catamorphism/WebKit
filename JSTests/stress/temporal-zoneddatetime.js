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

{
    const zdt = new Temporal.ZonedDateTime(192_258_181_000_000_000n, "UTC");
    shouldBe(zdt.year, 1976);
    shouldBe(zdt.month, 2);
    shouldBe(zdt.day, 4);
    shouldBe(zdt.hour, 5);
    shouldBe(zdt.minute, 3);
    shouldBe(zdt.second, 1);
    shouldBe(zdt.millisecond, 0);
    shouldBe(zdt.microsecond, 0);
    shouldBe(zdt.nanosecond, 0);
}

{
    const zdt = new Temporal.ZonedDateTime(-13849764_999_999_999n, "UTC");
    shouldBe(zdt.year, 1969);
    shouldBe(zdt.month, 7);
    shouldBe(zdt.day, 24);
    shouldBe(zdt.hour, 16);
    shouldBe(zdt.minute, 50);
    shouldBe(zdt.second, 35);
    shouldBe(zdt.millisecond, 0);
    shouldBe(zdt.microsecond, 0);
    shouldBe(zdt.nanosecond, 1);
}

{
    const zdt = new Temporal.ZonedDateTime(-3217846_616_964_000_000_000n, "UTC");
    shouldBe(zdt.year, -100000);
    shouldBe(zdt.month, 7);
    shouldBe(zdt.day, 1);
    shouldBe(zdt.hour, 21);
    shouldBe(zdt.minute, 30);
    shouldBe(zdt.second, 36);
    shouldBe(zdt.millisecond, 0);
    shouldBe(zdt.microsecond, 0);
    shouldBe(zdt.nanosecond, 0);
}

{
    const zdt = new Temporal.ZonedDateTime(-1n, "UTC");
    shouldBe(zdt.epochMilliseconds, -1);
}

{
    shouldThrow(() => new Temporal.ZonedDateTime(0n, "[UTC]"), RangeError);
    shouldThrow(() => new Temporal.ZonedDateTime({ valueOf() { throw RangeError(1) }}), RangeError);
}

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
    shouldBe(zdt.timeZoneId, "UTC");

    shouldThrow(() => new Temporal.ZonedDateTime(0n, null), TypeError);
    shouldThrow(() => new Temporal.ZonedDateTime(864n * 10n ** 19n + 1n, "UTC"), RangeError);
}

{
    shouldBe(zdt.toString(), '1970-01-01T00:00:00+00:00[UTC]');
    shouldBe(zdt.toJSON(), zdt.toString());
    shouldBe(zdt.toLocaleString(), zdt.toString());
}

shouldBe(Temporal.ZonedDateTime.prototype.with.length, 1);
{
    shouldBe(zdt.with({ year: 2021, month: 3, day: 5 }).toString(), '2021-03-05T00:00:00+00:00[UTC]');
    shouldBe(zdt.with({ year: "2021", month: 3, day: 5 }).toString(), '2021-03-05T00:00:00+00:00[UTC]');
    shouldBe(zdt.with({ offset: "+01:30" }, { offset: "use" }).toString(), '1969-12-31T22:30:00+00:00[UTC]');
    shouldBe(zdt.with({ month: 3, day: 5 }).toString(), '1970-03-05T00:00:00+00:00[UTC]');
    shouldBe(zdt.with({ month: 3 }).toString(), '1970-03-01T00:00:00+00:00[UTC]');

    shouldBe(zdt.with({ month: 4, day: 31 }).toString(), '1970-04-30T00:00:00+00:00[UTC]');
    shouldThrow(() => { zdt.with({ month: 4, day: 31 }, { overflow: 'reject' }); }, RangeError);
}

shouldBe(Temporal.ZonedDateTime.prototype.withPlainTime.length, 0);
{
    shouldBe(zdt.withPlainTime({ hour: 10 }).toString(), '1970-01-01T10:00:00+00:00[UTC]');
    shouldBe(zdt.withPlainTime(new Temporal.PlainTime(11, 22)).toString(), '1970-01-01T11:22:00+00:00[UTC]');
    shouldBe(zdt.withPlainTime("12:34").toString(), '1970-01-01T12:34:00+00:00[UTC]');
}

shouldBe(Temporal.ZonedDateTime.prototype.withTimeZone.length, 1);
{
    shouldBe(zdt.toString(), zdt.withTimeZone("UTC").toString());
    shouldBe(zdt.withTimeZone("+01:30").toString(), '1970-01-01T01:30:00+01:30[+01:30]');
}

shouldBe(Temporal.ZonedDateTime.prototype.withCalendar.length, 1);
{
    shouldBe(zdt.toString(), zdt.withCalendar("iso8601").toString());
    shouldBe(zdt.toString(), zdt.withCalendar("2020-01-01[u-ca=iso8601]").toString());
    shouldBe(zdt.toString(), zdt.withCalendar("01-01[u-ca=iso8601]").toString());
    shouldBe(zdt.toString(), zdt.withCalendar("2020-01[u-ca=iso8601]").toString());
}

shouldBe(Temporal.ZonedDateTime.from.length, 1);
{
    shouldThrow(() => Temporal.ZonedDateTime.from("1970-01-01T00:00"), RangeError);
    shouldThrow(() => Temporal.ZonedDateTime.from("1970-01-01T00:00Z"), RangeError);
    shouldThrow(() => Temporal.ZonedDateTime.from("1970-01-01T00:00+01:00"), RangeError);

    const str = "1970-01-01T00:00Z[+01:00]";
    const zdt1 = Temporal.ZonedDateTime.from(str);
    shouldBe(zdt1.toString(), "1970-01-01T01:00:00+01:00[+01:00]");
    shouldBe(zdt1.epochNanoseconds, 0n);
    shouldBe(zdt1.timeZoneId, "+01:00");

    const zdt2 = Temporal.ZonedDateTime.from({ year: 2000, month: 5, day: 2, timeZone: "UTC" });
    shouldBe(zdt2.timeZoneId, "UTC");
    shouldBe(zdt2.year, 2000);
    shouldBe(zdt2.month, 5);
    shouldBe(zdt2.day, 2);
    shouldBe(zdt2.hour, 0);
}

shouldBe(Temporal.ZonedDateTime.compare.length, 2);
{
    const zdt2 = new Temporal.ZonedDateTime(1n, "UTC");
    shouldBe(Temporal.ZonedDateTime.compare(zdt, zdt), 0);
    shouldBe(Temporal.ZonedDateTime.compare(zdt, zdt2), -1);
    shouldBe(Temporal.ZonedDateTime.compare(zdt2, zdt), 1);
}

shouldBe(Temporal.ZonedDateTime.prototype.equals.length, 1);
{
    const zdt2 = new Temporal.ZonedDateTime(1n, "UTC");
    const zdt3 = new Temporal.ZonedDateTime(0n, "+01:00");

    shouldBe(zdt.equals(zdt), true);
    shouldBe(zdt.equals(zdt2), false);
    shouldBe(zdt.equals(zdt3), false);
}

shouldBe(Temporal.ZonedDateTime.prototype.valueOf.length, 0);
shouldThrow(() => zdt.valueOf(), TypeError);

shouldBe(Temporal.ZonedDateTime.prototype.toInstant.length, 0);
shouldBe(zdt.toInstant().toString(), (new Temporal.Instant(0n)).toString());
shouldBe(Temporal.ZonedDateTime.prototype.toPlainDate.length, 0);
shouldBe(zdt.toPlainDate().toString(), (new Temporal.PlainDate(1970, 1, 1)).toString());
shouldBe(Temporal.ZonedDateTime.prototype.toPlainDateTime.length, 0);
shouldBe(zdt.toPlainDateTime().toString(), (new Temporal.PlainDateTime(1970, 1, 1, 0, 0, 0, 0, 0, 0, 0)).toString());
shouldBe(Temporal.ZonedDateTime.prototype.toPlainTime.length, 0);
shouldBe(zdt.toPlainTime().toString(), (new Temporal.PlainTime(0, 0, 0, 0, 0, 0, 0)).toString());

shouldBe(Temporal.ZonedDateTime.prototype.add.length, 1);
{
    shouldBe(zdt.add(new Temporal.Duration()).toString(), zdt.toString());
    shouldBe(zdt.add(new Temporal.Duration(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)).toString(), '1971-02-09T01:01:01.001001001+00:00[UTC]');
    shouldBe(zdt.add({ hours: 24 }).toString(), '1970-01-02T00:00:00+00:00[UTC]');
    shouldThrow(() => zdt.add({ years: 300000 }), RangeError);
}

shouldBe(Temporal.ZonedDateTime.prototype.subtract.length, 1);
{
    shouldBe(zdt.subtract(new Temporal.Duration()).toString(), zdt.toString());
    shouldBe(zdt.subtract(new Temporal.Duration(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)).toString(), '1968-11-22T22:58:58.998998999+00:00[UTC]');
    shouldBe(zdt.subtract({ hours: 24 }).toString(), '1969-12-31T00:00:00+00:00[UTC]');
    shouldThrow(() => zdt.subtract({ years: 300000 }), RangeError);
}
