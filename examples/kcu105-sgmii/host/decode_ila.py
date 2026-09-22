#!/usr/bin/env python3
"""Decode a code group column of an ILA capture (CSV written by run.sh ila).

Uses the former table-based 8b/10b decoder kept as reference in the clash-cores
test suite. Prints frames (runs of RX_DV or TX_EN) and every code group that is
invalid or has a disparity error, with the running disparity tracked from the
first K28.5 seen.

    decode_ila.py ila.csv [rx|tx]
"""
import csv, re, sys, pathlib

root = pathlib.Path(__file__).resolve().parents[3]
table = (root / "test/Test/Cores/LineCoding/Lc8b10b/Reference/Decoder.hs").read_text()
rows = re.findall(r"\((\d), (\d), (\d), (\d), 0b([01]{8})\)", table)
assert len(rows) == 2048
dec = [(int(a), int(b), int(c), int(d), int(e, 2)) for a, b, c, d, e in rows]

names = {0xBC: "K28.5", 0xFB: "/S/", 0xFD: "/T/", 0xF7: "/R/", 0xFE: "/V/"}

def decode(rd, cg):
    rd_er, cg_er, cw, rd_new, w = dec[(rd << 10) | cg]
    return rd_er, cg_er, cw, rd_new, w

def main():
    path = sys.argv[1]
    side = sys.argv[2] if len(sys.argv) > 2 else "rx"
    data = list(csv.reader(open(path)))
    hdr, samples = data[0], data[2:]
    col = lambda n: [i for i, h in enumerate(hdr) if h.endswith(n)][0]
    cg_i = col("ila_rx_cg[9:0]" if side == "rx" else "ila_tx_cg[9:0]")
    dv_i = col("ila_rx_dv" if side == "rx" else "ila_tx_en")
    dw_i = col("ila_rx_dw[7:0]" if side == "rx" else "ila_tx_dw[7:0]")
    cgs = [int(r[cg_i], 16) for r in samples]
    dvs = [r[dv_i] == "1" for r in samples]
    # start the running disparity at the first comma
    rd = None
    problems, symbols = [], []
    for i, cg in enumerate(cgs):
        if rd is None:
            if cg == 0b0101111100: rd = 0
            elif cg == 0b1010000011: rd = 1
            else: symbols.append((i, "?")); continue
        rd_er, cg_er, cw, rd_new, w = decode(rd, cg)
        if cg_er: problems.append((i, cg, "invalid code group")); symbols.append((i, "ERR"))
        elif rd_er: problems.append((i, cg, "disparity error")); symbols.append((i, "RDERR"))
        elif cw: symbols.append((i, names.get(w, f"K{w & 31}.{w >> 5}")))
        else: symbols.append((i, f"{w:02x}"))
        rd = rd_new
    frames, cur = [], None
    for i, d in enumerate(dvs):
        if d and cur is None: cur = i
        if not d and cur is not None: frames.append((cur, i - cur)); cur = None
    print(f"{len(cgs)} samples, {len(frames)} frames ({side}): {frames[:8]}")
    print(f"{len(problems)} problem code groups: {problems[:12]}")
    ctrl = [(i, s) for i, s in symbols if s.startswith(('/', 'K', 'ERR', 'RDERR'))]
    print("control symbols and errors (first 40):", ctrl[:40])
    for start, length in frames[:2]:
        seq = [s for _, s in symbols[start - 2:start + min(length, 24) + 4]]
        print(f"frame at {start} ({length} cycles):", " ".join(seq))

main()
