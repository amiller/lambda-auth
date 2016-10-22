#!/bin/bash
ls data
for x in `seq 4 21`; do
    echo "=== Tree Size: 2^${x} elements ==="
    echo "[Avg. proof size for lookup]: "
    echo -n "Uncompressed (bytes): "
    bc -l <<< "$(cat data/proof_rbp_look_$(printf '%03d' $x).dat | wc -c) / 100000"
    echo -n "Compressed (bytes): "
    bc -l <<< "$(cat data/proof_rbp_look_$(printf '%03d' $x).dat | gzip -c | wc -c) / 100000"
    echo "[Avg. proof size for insert]: "
    echo -n "Uncompressed (bytes): "
    bc -l <<< "$(cat data/proof_rbp_ins_$(printf '%03d' $x).dat | wc -c) / 100000"
    echo -n "Compressed (bytes): "
    bc -l <<< "$(cat data/proof_rbp_ins_$(printf '%03d' $x).dat | gzip -c | wc -c) / 100000"
done
