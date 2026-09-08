// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

/**
 * Tracks per-key "generations" so an in-flight async fetch can tell, once it
 * resolves, whether its key was invalidated (or everything was) while it was
 * in flight.
 *
 * This is the guard a lazily-populated, per-key cache needs to avoid a
 * stale-write race: `getChildren` reads a cache, finds it empty, and starts
 * an async fetch; before that fetch resolves, a push event (e.g. a class
 * reload) invalidates the same key and refetches fresh data; the original,
 * now-stale fetch then resolves and — without this guard — overwrites the
 * fresh data with its own outdated result. Comparing a token captured before
 * the fetch started against the tracker's current state lets the resolved
 * handler recognize this and discard its result instead of applying it.
 */
export class GenerationTracker {
  private epoch = 0;
  private readonly perKey = new Map<string, number>();

  /** A token capturing the current validity of `key`. Compare with `isCurrent`. */
  token(key: string): { epoch: number; keyGen: number } {
    return { epoch: this.epoch, keyGen: this.perKey.get(key) ?? 0 };
  }

  /** True if nothing invalidated `key` (specifically or globally) since `token` was captured. */
  isCurrent(key: string, token: { epoch: number; keyGen: number }): boolean {
    return this.epoch === token.epoch && (this.perKey.get(key) ?? 0) === token.keyGen;
  }

  /** Invalidate one key — e.g. a targeted cache-delete for a single reloaded class. */
  bump(key: string): void {
    this.perKey.set(key, (this.perKey.get(key) ?? 0) + 1);
  }

  /** Invalidate everything — e.g. a wholesale cache-clear on disconnect/refresh. */
  bumpAll(): void {
    this.epoch++;
  }
}
