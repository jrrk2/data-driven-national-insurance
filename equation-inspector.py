# equation_inspector.py
import json
import re
import sys

def analyze_string(s, prefix=""):
    print(f"{prefix}String: {s}")
    print(f"{prefix}Length: {len(s)}")
    print(f"{prefix}Characters and their Unicode values:")
    for i, c in enumerate(s):
        print(f"{prefix}{i}: '{c}' (U+{ord(c):04X})")
    print()

def inspect_equations(obj, path=""):
    if isinstance(obj, dict):
        for k, v in obj.items():
            new_path = f"{path}.{k}" if path else k
            if k == "equation":
                print(f"\nEquation at {new_path}:")
                analyze_string(v)
            inspect_equations(v, new_path)
    elif isinstance(obj, list):
        for i, item in enumerate(obj):
            new_path = f"{path}[{i}]"
            inspect_equations(item, new_path)

def main(filename):
    with open(filename) as f:
        data = json.load(f)
    inspect_equations(data)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python equation_inspector.py <json_file>")
        sys.exit(1)
    main(sys.argv[1])
