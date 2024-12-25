# pattern_tester.py
import json
import re
import sys

def test_pattern(pattern, string):
    try:
        regex = re.compile(pattern)
        match = regex.fullmatch(string)
        print(f"Testing pattern: {pattern}")
        print(f"Against string: {string}")
        print(f"Match: {'Yes' if match else 'No'}")
        if not match:
            # Find what characters might not match
            allowed_chars = set(c for c in string if re.match(f'[{pattern[1:-1]}]', c))
            all_chars = set(string)
            problematic = all_chars - allowed_chars
            if problematic:
                print("Characters not matching pattern:")
                for c in sorted(problematic):
                    print(f"  '{c}' (U+{ord(c):04X})")
    except re.error as e:
        print(f"Pattern error: {e}")

def main(json_file, pattern):
    with open(json_file) as f:
        data = json.load(f)
    
    # Function to find equations
    def find_equations(obj):
        if isinstance(obj, dict):
            for k, v in obj.items():
                if k == 'equation':
                    yield v
                elif isinstance(v, (dict, list)):
                    yield from find_equations(v)
        elif isinstance(obj, list):
            for item in item:
                yield from find_equations(item)

    # Test each equation
    for equation in find_equations(data):
        print("\n=== Testing equation ===")
        test_pattern(pattern, equation)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python pattern_tester.py <json_file>")
        sys.exit(1)
        
    pattern = r'^[()\\w\\s+\\-*/\\u00D7\\?:.,=\\^]+$'
    main(sys.argv[1], pattern)
