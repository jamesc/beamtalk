## DateTime & Time

Beamtalk provides `DateTime` for calendar dates and times, and `Time` for
high-resolution timestamps.

## Creating a DateTime

Date only (time defaults to 00:00:00):

```beamtalk
dt := DateTime year: 2026 month: 3 day: 10  // => _
dt year   // => 2026
dt month  // => 3
dt day    // => 10
dt hour   // => 0
dt minute // => 0
dt second // => 0
```

Full date and time:

```beamtalk
dt := DateTime year: 2026 month: 3 day: 10 hour: 14 minute: 30 second: 45  // => _
dt hour   // => 14
dt minute // => 30
dt second // => 45
```

## Parsing and formatting

From an ISO 8601 string:

```beamtalk
dt := DateTime fromString: "2026-02-18T10:30:00Z"  // => _
dt year   // => 2026
dt month  // => 2
dt day    // => 18
dt hour   // => 10
dt minute // => 30
```

From a Unix timestamp (seconds since epoch):

```beamtalk
dt := DateTime fromTimestamp: 0  // => _
dt year  // => 1970
dt month // => 1
dt day   // => 1
```

Convert back to a string or timestamp:

```beamtalk
dt := DateTime year: 2026 month: 2 day: 18 hour: 10 minute: 30 second: 0  // => _
dt asString     // => 2026-02-18T10:30:00Z
dt asTimestamp  // => _
```

## Current time

```beamtalk
now := DateTime now  // => _
now year > 2024      // => true
```

## Arithmetic

Add or subtract time:

```beamtalk
dt := DateTime year: 2026 month: 1 day: 1 hour: 12 minute: 0 second: 0  // => _
later := dt addSeconds: 3600  // => _
later hour                    // => 13
```

```beamtalk
dt := DateTime year: 2026 month: 1 day: 1  // => _
tomorrow := dt addDays: 1                   // => _
tomorrow day                                // => 2
```

Negative values work too:

```beamtalk
dt := DateTime year: 2026 month: 3 day: 15  // => _
earlier := dt addDays: -5                    // => _
earlier day                                  // => 10
```

Month and year boundaries are handled:

```beamtalk
dt := DateTime year: 2026 month: 1 day: 31  // => _
next := dt addDays: 1                        // => _
next month                                   // => 2
next day                                     // => 1
```

## Difference between dates

`diffSeconds:` returns the signed difference in seconds:

```beamtalk
d1 := DateTime year: 2026 month: 1 day: 1  // => _
d2 := DateTime year: 2026 month: 1 day: 2  // => _
d2 diffSeconds: d1                          // => 86400
d1 diffSeconds: d2                          // => -86400
```

## Comparisons

DateTimes support the standard comparison operators:

```beamtalk
d1 := DateTime year: 2026 month: 1 day: 1  // => _
d2 := DateTime year: 2026 month: 6 day: 15 // => _
d1 < d2   // => true
d1 > d2   // => false
d1 <= d1  // => true
d1 /= d2  // => true
```

## High-resolution timestamps

The `Time` class provides millisecond and microsecond precision:

```beamtalk
ms := Time nowMs  // => _
ms > 0            // => true
```

```beamtalk
us := Time nowUs  // => _
us > 0            // => true
```

Microseconds are always larger than milliseconds (1000x more precise):

```beamtalk
ms := Time nowMs  // => _
us := Time nowUs  // => _
us > ms           // => true
```

## Summary

**Construction:**

```text
DateTime year: y month: m day: d
DateTime year: y month: m day: d hour: h minute: min second: s
DateTime fromString: "2026-01-01T00:00:00Z"
DateTime fromTimestamp: unixSeconds
DateTime now
```

**Accessors:** `year`, `month`, `day`, `hour`, `minute`, `second`

**Conversion:** `asString` (ISO 8601), `asTimestamp` (Unix seconds)

**Arithmetic:** `addSeconds:`, `addDays:`, `diffSeconds:`

**Comparisons:** `<`, `>`, `<=`, `>=`, `=:=`, `/=`

**High-resolution:** `Time nowMs` (milliseconds), `Time nowUs` (microseconds)

## Exercises

**1. Days between dates.** Compute the number of days between January 1 and
March 1, 2026. Remember `diffSeconds:` returns seconds.

<details>
<summary>Hint</summary>

```text
d1 := DateTime year: 2026 month: 1 day: 1
d2 := DateTime year: 2026 month: 3 day: 1
seconds := d2 diffSeconds: d1
days := seconds div: 86400    // => 59
```

January has 31 days + February has 28 days = 59 days.
</details>

**2. Add a week.** Create a DateTime for today, add 7 days, and verify the
result is after the original using `>`.

<details>
<summary>Hint</summary>

```text
today := DateTime now
nextWeek := today addDays: 7
nextWeek > today    // => true
```
</details>

**3. ISO round-trip.** Create a DateTime, convert it to an ISO string with
`asString`, parse it back with `fromString:`, and verify the year, month, and
day are preserved.

<details>
<summary>Hint</summary>

```text
dt := DateTime year: 2026 month: 6 day: 15 hour: 10 minute: 30 second: 0
iso := dt asString                    // => "2026-06-15T10:30:00Z"
dt2 := DateTime fromString: iso
dt2 year =:= dt year    // => true
dt2 month =:= dt month  // => true
dt2 day =:= dt day      // => true
```
</details>

Next: Chapter 22 — Workspace & Globals
