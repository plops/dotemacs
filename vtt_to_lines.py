#!/usr/bin/env -S uv run
# /// script
# requires-python = ">=3.13"
# dependencies = [
#   "webvtt-py>=0.5.1",
# ]
# ///

"""
Convert WebVTT (.vtt) into simple "HH:MM:SS Text" lines (one per cue).

Example output:
00:00:05 Good.
00:00:06 Then I'd like to welcome you to this kick...

Requires:
  uv
  webvtt-py  (import name: webvtt)

Usage:
  uv run vtt_to_lines.py kickoff.vtt > kickoff.txt
  uv run vtt_to_lines.py kickoff.vtt --ms
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

import webvtt  # from webvtt-py


_TS_RE = re.compile(r"^(?:(\d+):)?(\d{2}):(\d{2})(?:\.(\d{1,3}))?$")


def _parse_ts(ts: str) -> tuple[int, int, int, int]:
    """
    Parse WebVTT timestamp like:
      HH:MM:SS.mmm or MM:SS.mmm
    Returns (h, m, s, ms)
    """
    m = _TS_RE.match(ts.strip())
    if not m:
        raise ValueError(f"Unrecognised timestamp: {ts!r}")
    h_str, mm, ss, ms = m.groups()
    h = int(h_str) if h_str is not None else 0
    m_ = int(mm)
    s = int(ss)
    ms_ = int(ms) if ms is not None else 0
    # normalise ms to 0..999 even if 1-2 digits
    if ms is not None and len(ms) == 1:
        ms_ *= 100
    elif ms is not None and len(ms) == 2:
        ms_ *= 10
    return h, m_, s, ms_


def _format_ts_seconds(ts: str) -> str:
    h, m, s, _ = _parse_ts(ts)
    return f"{h:02d}:{m:02d}:{s:02d}"


def _format_ts_ms(ts: str) -> str:
    h, m, s, ms = _parse_ts(ts)
    return f"{h:02d}:{m:02d}:{s:02d}.{ms:03d}"


def cue_text_lines(cue) -> list[str]:
    """
    webvtt cue.text can contain newlines; keep them, but drop empty lines.
    """
    lines = [ln.strip() for ln in (cue.text or "").splitlines()]
    return [ln for ln in lines if ln]


def convert(vtt_path: Path, include_ms: bool) -> None:
    v = webvtt.read(str(vtt_path))

    fmt = _format_ts_ms if include_ms else _format_ts_seconds

    for cue in v:
        ts = fmt(cue.start)
        for line in cue_text_lines(cue):
            sys.stdout.write(f"{ts} {line}\n")


def main() -> int:
    p = argparse.ArgumentParser(description="Convert WebVTT to 'timestamp text' lines.")
    p.add_argument("vtt", type=Path, help="Input .vtt file")
    p.add_argument("--ms", action="store_true", help="Keep milliseconds in output")
    args = p.parse_args()

    if not args.vtt.exists():
        print(f"File not found: {args.vtt}", file=sys.stderr)
        return 2

    convert(args.vtt, include_ms=args.ms)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
