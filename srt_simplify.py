#!/usr/bin/env python3
"""
Read a WebVTT file (whisper.cpp -ovtt) and emit lines like:
00:00 HDB Scan
00:02 HDB Scan 是什么东西呢?

Usage:
  python3 srt_simplify.py file.vtt
  cat file.vtt | python3 srt_simplify.py -
Requires: pip install webvtt-py
"""
import sys
import argparse
import tempfile
from pathlib import Path
import webvtt

def parse_vtt_file(path):
    old_text = None
    out_lines = []
    for c in webvtt.read(str(path)):
        # join multi-line captions, skip empty
        txt = " ".join(line.strip() for line in c.text.splitlines() if line.strip())
        if not txt:
            continue
        # simple adjacent-duplicate suppression
        if txt == old_text:
            continue
        old_text = txt
        # drop milliseconds and convert to total seconds
        time_no_ms = c.start.split(".")[0]  # "HH:MM:SS"
        hh, mm, ss = (int(p) for p in time_no_ms.split(":"))
        total = hh * 3600 + mm * 60 + ss
        minutes = total // 60
        seconds = total % 60
        out_lines.append(f"{minutes:02d}:{seconds:02d} {txt}")
    return "\n".join(out_lines)

def main():
    p = argparse.ArgumentParser(description="Simplify VTT (whisper.cpp) to MM:SS lines.")
    p.add_argument("file", nargs="?", help="VTT file path or '-' for stdin")
    args = p.parse_args()

    if not args.file or args.file == "-":
        # read stdin into a temp file and parse
        data = sys.stdin.buffer.read()
        if not data:
            print("No input.", file=sys.stderr)
            sys.exit(1)
        with tempfile.NamedTemporaryFile(suffix=".vtt", delete=False) as tmp:
            tmp.write(data)
            tmp_path = Path(tmp.name)
        try:
            print(parse_vtt_file(tmp_path))
        finally:
            try:
                tmp_path.unlink()
            except Exception:
                pass
    else:
        path = Path(args.file)
        if not path.exists():
            print(f"File not found: {path}", file=sys.stderr)
            sys.exit(2)
        print(parse_vtt_file(path))

if __name__ == "__main__":
    main()
