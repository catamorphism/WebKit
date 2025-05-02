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
