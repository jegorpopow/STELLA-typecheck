#!/bin/bash

if [ $# -ne 1 ]; then
    echo "Usage: $0 <directory>"
    exit 1
fi

target_dir="$1"
find "$target_dir" -type f -name "*.stella" -print0 | while IFS= read -r -d '' file; do
    echo "$file"
    cat "$file" | ./Check
done
