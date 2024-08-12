import re

def transform_inline_if_then_else(content):
    # Regular expression to match 'if-then-else' patterns
    # This pattern ensures minimal matching to prevent greedy captures
    pattern = re.compile(r'\bif\s+(.*?)\s+then\s+(.*?)\s+else\s+(.*?)(?=\)|,|\s|$)')

    def replace_match(match):
        condition = match.group(1).strip()
        true_expr = match.group(2).strip()
        false_expr = match.group(3).strip()
        return f'(({condition}) and {true_expr} or {false_expr})'

    # Split the content into lines for individual processing
    lines = content.split('\n')
    transformed_lines = []

    for line in lines:
        stripped_line = line.lstrip()  # Trim leading whitespace to check the starting keyword
        # Check if the line is a block 'if' statement
        if stripped_line.startswith('if '):
            # It's a block statement; leave it unchanged
            transformed_lines.append(line)
        else:
            # Process all inline 'if-then-else' expressions in the line
            # Continuously apply replacements until no more matches are found
            original_line = line
            while True:
                # Search for the pattern in the current line
                match = pattern.search(original_line)
                if not match:
                    break  # Exit if no matches are found
                # Replace the first occurrence
                replacement = replace_match(match)
                start, end = match.span()
                original_line = original_line[:start] + replacement + original_line[end:]
            transformed_lines.append(original_line)

    # Join the transformed lines back into a single string
    return '\n'.join(transformed_lines)

content = """reg_1 = FUNC_LIST[78](rt_add_i32(loc_1, 4), (if loc_3 ~= 0 then rt_load_i32(memory_at_0, loc_0) else loc_0), (if loc_3 ~= 0 then rt_load_i32(memory_at_0, loc_0 + 4) else loc_2))

if i > 31 then
    rem_1 = bit_or(rem_1, bit_extract(num_2, i - 32, 1))
else
    rem_1 = bit_or(rem_1, bit_extract(num_1, i, 1))
end

result = if not len or len == #data then data else string_sub(data, 1, len)"""

transformed_content = transform_inline_if_then_else(content)
print(transformed_content)