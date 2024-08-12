import os
import shutil
import subprocess
import re


def main():
    with open("src\\CMakeLists.txt", "r") as f:
        content = f.read()
        regex = re.compile(r"EXPORTED_FUNCTIONS=\[(.+)\]")
        exported_functions = (
            regex.search(content)
            .group(1)
            .replace("'", "")
            .replace("[", "")
            .replace("]", "")
            .split(" ")[0]
            .split(",")
        )
        exported_functions = [func[1:] for func in exported_functions if func != ""]

    # Remove (build) and dist folders if they exist
    # if os.path.exists("build"):
    # shutil.rmtree("build")
    if os.path.exists("dist"):
        shutil.rmtree("dist")

    # Create (build) and dist folders
    if not os.path.exists("build"):
        os.mkdir("build")
    os.mkdir("dist")

    # Change directory to build
    os.chdir("build")

    # Run cmake
    subprocess.run("emcmake cmake -DCMAKE_BUILD_TYPE=Release ..", shell=True)

    cores = os.cpu_count()

    # Build project
    subprocess.run("cmake --build . -j" + str(cores), shell=True)

    # Change directory to src
    os.chdir("src")

    # Convert wasm to lua
    subprocess.run("wasm2luau RbxCppProject.wasm > ..\\..\\dist\\main.lua", shell=True)

    # Change directory back to build
    os.chdir("..\\..")

    # Remove build folder
    # shutil.rmtree("build")

    # Replace some stuff .-.
    with open("dist\\main.lua", "r") as f:
        content = f.read()

        offsets = {
            "__wasm_call_ctors": re.search(
                r"\[\"__wasm_call_ctors\"\] = FUNC_LIST\[(\d+)\]", content
            ).group(1)
        }

        for func in exported_functions:
            regex = re.compile(r"\[\"" + func + '"\] = FUNC_LIST\[(\d+)\]')
            offsets[func] = regex.search(content).group(1)

        content = re.sub(
            r"local string_byte = string.byte",
            "local string_byte = string.byte\n    local string_char = string.char",
            content,
        )

        content2 = content.split("local memory_at_0")
        content = (
            """--!optimize 2
local lastSleep = 0
function peformSleepCheck()
    if tick() - lastSleep >= 1 then
        lastSleep = tick()
        task.wait()
    end
end
"""
            + content2[0].replace("--!optimize 2", "")
            + """local function load_string(memory, addr, len)
    peformSleepCheck()
    local temp = buffer_create(len)

    buffer_copy(temp, 0, memory.data, addr, len)

    return buffer_to_string(temp)
end

local function store_string(memory, addr, data, len)
    peformSleepCheck()
    local content = if not len or len == #data then data else string_sub(data, 1, len)
    local temp = buffer_from_string(content)

    buffer_copy(memory.data, addr, temp)
end

local function to_signed(num)
    return num >= 0x80000000 and num - 0x100000000 or num
end

local function truncate_f64(num)
	if num >= 0 then
		return (math_floor(num))
	else
		return (math_ceil(num))
	end
end

local function rt_truncate_u32_f32(num)
    return (to_signed(truncate_f64(num)))
end

local memory_at_0"""
            + content2[1]
        )

        function_defs = re.findall(r"local function .*\n", content)
        for func in function_defs:
            content = content.replace(func, func + "    peformSleepCheck()\n")

        ### Start of  Lua compatibility ###

        """
        def transform_inline_if_then_else(content):
            pattern = re.compile(
                r"\bif\s+(.*?)\s+then\s+(.*?)\s+else\s+([^\),\s]+(?:\s*,\s*[^\),\s]+)*)"
            )

            def replace_match(match):
                condition = match.group(1).strip()
                true_expr = match.group(2).strip()
                false_expr = match.group(3).strip()
                return f"(({condition}) and {true_expr} or {false_expr})"

            lines = content.split("\n")
            transformed_lines = []

            for line in lines:
                stripped_line = (
                    line.lstrip()
                )
                if stripped_line.startswith("if "):
                    transformed_lines.append(line)
                else:
                    original_line = line
                    while True:
                        match = pattern.search(original_line)
                        if not match:
                            break
                        replacement = replace_match(match)
                        start, end = match.span()
                        original_line = (
                            original_line[:start] + replacement + original_line[end:]
                        )
                    transformed_lines.append(original_line)

            return "\n".join(transformed_lines)

        content = transform_inline_if_then_else(content)

        content = re.sub(r"0_(\d+)", r"\1", content)
        """

        ### End of Lua compatibility ###

        content = re.sub(r"return function\(wasm\)(.|\n)*end", "", content)

        content = (
            content
            + """

--// Named function dictionary
local NamedFunctionList = {
    --// WebAssembly
    ["__wasm_call_ctors"] = FUNC_LIST["""
            + offsets["__wasm_call_ctors"]
            + """];

    --// Exported functions
    """
            + "\n".join(
                [
                    f'["{func}"] = FUNC_LIST[{offsets[func]}];'
                    for func in exported_functions
                ]
            )
            + """
}

--// Pre-init environment function setup

--// lua_call
FUNC_LIST["""
            + offsets["lua_call"]
            + """] = function (code)
    code = load_string(memory_at_0, code, NamedFunctionList.strlen(code))
    local loaded = loadstring(code)
    local ret = loaded and loaded() or ""
    if type(ret) ~= "string" then
        ret = tostring(ret)
    end
    
    local addr = NamedFunctionList.malloc(#ret + 1)
    store_string(memory_at_0, addr, ret .. "\\0", #ret + 1)
    return addr
end

--// Initialize
run_init_code()
memory_at_0 = MEMORY_LIST[0]
NamedFunctionList.__wasm_call_ctors()

local function convertString(s)
    if type(s) == "string" then
        local addr = NamedFunctionList.malloc(#s + 1)
        store_string(memory_at_0, addr, s .. "\\0", #s + 1)
        return addr
    else
        return load_string(memory_at_0, s, NamedFunctionList.strlen(s))
    end
end

""" + ("main" in exported_functions and "local exitCode = NamedFunctionList.main(..., #{...})" or "") + """

--// Return
return {
    --// Exported functions
    """
            + "\n".join(
                [f"{func} = NamedFunctionList.{func}," for func in exported_functions]
            )
            + """

    --// Wasynth related utilities
    NamedFunctionList = NamedFunctionList;
    convertString = convertString;
    FUNC_LIST = FUNC_LIST;""" + ("main" in exported_functions and "\nexitCode = exitCode;" or "") + """
}"""
        )

        with open("dist\\main.lua", "w") as f:
            f.write(content)


if __name__ == "__main__":
    main()
