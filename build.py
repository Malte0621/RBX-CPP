import os
import shutil
import subprocess
import re


# Lua implementations for known WebAssembly imports emitted by Emscripten.
# Each value is a Lua function literal assigned to FUNC_LIST[idx].
IMPORT_IMPLS = {
    "abort": 'function() error("[RBX-CPP] abort() called", 0) end',
    "_abort_js": 'function() error("[RBX-CPP] abort() called", 0) end',
    "_tzset_js": "function() end",
    "environ_sizes_get": (
        "function(count_ptr, size_ptr)\n"
        "    local count, bufsize = 0, 0\n"
        "    for _, e in ipairs(rbx_env_entries) do\n"
        "        count = count + 1\n"
        "        bufsize = bufsize + #e + 1\n"
        "    end\n"
        "    rt_store_i32(memory_at_0, count_ptr, count)\n"
        "    rt_store_i32(memory_at_0, size_ptr, bufsize)\n"
        "    return 0\n"
        "end"
    ),
    "environ_get": (
        "function(env_ptr, buf_ptr)\n"
        "    local p = buf_ptr\n"
        "    for i, e in ipairs(rbx_env_entries) do\n"
        '        store_string(memory_at_0, p, e .. "\\0", #e + 1)\n'
        "        rt_store_i32(memory_at_0, env_ptr + (i - 1) * 4, p)\n"
        "        p = p + #e + 1\n"
        "    end\n"
        "    return 0\n"
        "end"
    ),
    "fd_read": (
        "function(fd, iov, iovcnt, nread)\n"
        "    if fd ~= 0 then rt_store_i32(memory_at_0, nread, 0) return 0 end\n"
        "    local total = 0\n"
        "    for i = 0, iovcnt - 1 do\n"
        "        if #rbx_stdin_buf == 0 then break end\n"
        "        local ptr = rt_load_i32(memory_at_0, iov + i * 8)\n"
        "        local len = rt_load_i32(memory_at_0, iov + i * 8 + 4)\n"
        "        if len > 0 then\n"
        "            local n = math.min(len, #rbx_stdin_buf)\n"
        "            local chunk = string.sub(rbx_stdin_buf, 1, n)\n"
        "            store_string(memory_at_0, ptr, chunk, n)\n"
        "            rbx_stdin_buf = string.sub(rbx_stdin_buf, n + 1)\n"
        "            total = total + n\n"
        "            if n < len then break end\n"
        "        end\n"
        "    end\n"
        "    rt_store_i32(memory_at_0, nread, total)\n"
        "    return 0\n"
        "end"
    ),
    "__cxa_throw": (
        "function(ptr, tinfo, dtor)\n"
        '    error("[RBX-CPP] uncaught C++ exception", 0)\n'
        "end"
    ),
    "_emscripten_memcpy_js": (
        "function(dest, src, len)\n"
        "    if len > 0 then buffer_copy(memory_at_0.data, dest, memory_at_0.data, src, len) end\n"
        "end"
    ),
    "emscripten_memcpy_big": (
        "function(dest, src, len)\n"
        "    if len > 0 then buffer_copy(memory_at_0.data, dest, memory_at_0.data, src, len) end\n"
        "    return dest\n"
        "end"
    ),
    "emscripten_resize_heap": (
        "function(requested)\n"
        "    local current_pages = rt_allocator_size(memory_at_0)\n"
        "    local needed_pages = math_ceil(requested / 65536) - current_pages\n"
        "    if needed_pages <= 0 then return 1 end\n"
        "    if current_pages + needed_pages > memory_at_0.max then\n"
        "        memory_at_0.max = current_pages + needed_pages\n"
        "    end\n"
        "    return rt_allocator_grow(memory_at_0, needed_pages) ~= 0xFFFFFFFF and 1 or 0\n"
        "end"
    ),
    "fd_close": "function(fd) return 0 end",
    "fd_seek": "function(fd, ol, oh, whence, np) return 70 end",
    "fd_write": (
        "function(fd, iov, iovcnt, nwritten)\n"
        "    local total = 0\n"
        '    local data = ""\n'
        "    for i = 0, iovcnt - 1 do\n"
        "        local ptr = rt_load_i32(memory_at_0, iov + i * 8)\n"
        "        local len = rt_load_i32(memory_at_0, iov + i * 8 + 4)\n"
        "        if len > 0 then\n"
        "            data = data .. load_string(memory_at_0, ptr, len)\n"
        "            total = total + len\n"
        "        end\n"
        "    end\n"
        "    if fd == 2 then\n"
        "        rbx_stderr_buf = rbx_stderr_buf .. data\n"
        '        local nl = string.find(rbx_stderr_buf, "\\n")\n'
        "        while nl do\n"
        "            warn(string.sub(rbx_stderr_buf, 1, nl - 1))\n"
        "            rbx_stderr_buf = string.sub(rbx_stderr_buf, nl + 1)\n"
        '            nl = string.find(rbx_stderr_buf, "\\n")\n'
        "        end\n"
        "    else\n"
        "        rbx_stdout_buf = rbx_stdout_buf .. data\n"
        '        local nl = string.find(rbx_stdout_buf, "\\n")\n'
        "        while nl do\n"
        "            print(string.sub(rbx_stdout_buf, 1, nl - 1))\n"
        "            rbx_stdout_buf = string.sub(rbx_stdout_buf, nl + 1)\n"
        '            nl = string.find(rbx_stdout_buf, "\\n")\n'
        "        end\n"
        "    end\n"
        "    rt_store_i32(memory_at_0, nwritten, total)\n"
        "    return 0\n"
        "end"
    ),
}


def build_import_bindings(imports):
    """Generate Lua source binding each captured wasm import to a Lua handler.

    `imports` maps import-name -> FUNC_LIST index. Unknown imports become a
    runtime-warning stub so missing wires don't silently no-op.
    """
    if not imports:
        return ""
    lines = ["--// WebAssembly import bindings"]
    lines.append('local rbx_stdout_buf = ""')
    lines.append('local rbx_stderr_buf = ""')
    lines.append('local rbx_stdin_buf  = ""')
    lines.append("local rbx_env_entries = {}")
    lines.append("local function rbx_flush_streams()")
    lines.append('    if rbx_stdout_buf ~= "" then print(rbx_stdout_buf); rbx_stdout_buf = "" end')
    lines.append('    if rbx_stderr_buf ~= "" then warn(rbx_stderr_buf); rbx_stderr_buf = "" end')
    lines.append("end")
    lines.append("local function rbx_set_stdin(text)")
    lines.append('    rbx_stdin_buf = type(text) == "string" and text or ""')
    lines.append("end")
    lines.append("local function rbx_append_stdin(text)")
    lines.append('    rbx_stdin_buf = rbx_stdin_buf .. (type(text) == "string" and text or "")')
    lines.append("end")
    lines.append("local function rbx_set_env(tbl)")
    lines.append("    rbx_env_entries = {}")
    lines.append('    if type(tbl) ~= "table" then return end')
    lines.append("    for k, v in pairs(tbl) do")
    lines.append('        table.insert(rbx_env_entries, tostring(k) .. "=" .. tostring(v))')
    lines.append("    end")
    lines.append("end")
    for name, idx in sorted(imports.items(), key=lambda kv: kv[1]):
        impl = IMPORT_IMPLS.get(name)
        if impl is None:
            impl = (
                "function(...)\n"
                f'    warn("[RBX-CPP] unimplemented wasm import: {name}")\n'
                "    return 0\n"
                "end"
            )
        lines.append(f"FUNC_LIST[{idx}] = --[[ {name} ]] {impl}")
    return "\n".join(lines) + "\n"


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

    # Convert wasm to lua (use the bundled wasynth build to ensure compatible output).
    wasm2luau = os.path.join("..", "..", "wasynth-builds", "wasm2luau.exe")
    subprocess.run(
        f"{wasm2luau} RbxCppProject.wasm > ..\\..\\dist\\main.lua", shell=True
    )

    # Change directory back to build
    os.chdir("..\\..")

    # Remove build folder
    # shutil.rmtree("build")

    # Replace some stuff .-.
    with open("dist\\main.lua", "r") as f:
        content = f.read()

        # __wasm_call_ctors is only emitted when there are global ctors to
        # run. Optimized builds without C++ globals can drop it entirely; in
        # that case we just skip the call instead of failing.
        ctors_match = re.search(
            r"\[\"__wasm_call_ctors\"\] = FUNC_LIST\[(\d+)\]", content
        )
        offsets = {}
        if ctors_match:
            offsets["__wasm_call_ctors"] = ctors_match.group(1)

        # Aliases emscripten emits when the signature differs from the
        # default-exported name. e.g. an `int main(int, char**)` is exported
        # as `__main_argc_argv` rather than `main`.
        export_aliases = {
            "main": ["main", "__main_argc_argv", "__main_void"],
        }
        for func in exported_functions:
            candidates = export_aliases.get(func, [func])
            offset = None
            for cand in candidates:
                regex = re.compile(r"\[\"" + cand + r'"\] = FUNC_LIST\[(\d+)\]')
                m = regex.search(content)
                if m:
                    offset = m.group(1)
                    break
            if offset is None:
                raise RuntimeError(
                    f"Could not find FUNC_LIST offset for export '{func}' "
                    f"(tried {candidates})."
                )
            offsets[func] = offset

        # Capture wasm imports from Wasynth's `return function(wasm)` block
        # before it gets stripped. Each entry maps import name -> FUNC_LIST index.
        imports = {}
        import_block = re.search(
            r"return function\(wasm\)\s*\n((?:\s*FUNC_LIST\[\d+\]\s*=\s*wasm\[[^\n]+\n)+)",
            content,
        )
        if import_block:
            for line in import_block.group(1).splitlines():
                m = re.search(
                    r'FUNC_LIST\[(\d+)\]\s*=\s*wasm\["([^"]+)"\]\.func_list\["([^"]+)"\]',
                    line,
                )
                if m:
                    imports[m.group(3)] = int(m.group(1))

        content = re.sub(
            r"local string_byte = string.byte",
            "local string_byte = string.byte\n    local string_char = string.char",
            content,
        )

        # Fallback from Vector3.new to vector.create when Vector3 is unavailable
        # (e.g. in environments without the Roblox API).
        content = re.sub(
            r"local constructor = Vector3\.new",
            "local constructor = (Vector3 and Vector3.new) or (vector and vector.create)",
            content,
        )

        content2 = content.split("local memory_at_0")
        content = (
            """--!optimize 2
local _wait = (task and task.wait) or wait
local lastSleep = 0
function peformSleepCheck()
    if not _wait then return end
    if tick() - lastSleep >= 1 then
        lastSleep = tick()
        _wait()
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

-- Lua numbers are double precision, so f32 saturate/truncate ops are
-- semantically identical to their f64 counterparts. Wasynth normally aliases
-- them in its `rt` table; since we strip that block we re-create the aliases
-- here so generated code that calls the f32 form keeps working.
local rt_saturate_u32_f32 = rt_saturate_u32_f64
local rt_saturate_i32_f32 = rt_saturate_i32_f64
local rt_saturate_u64_f32 = rt_saturate_u64_f64
local rt_saturate_i64_f32 = rt_saturate_i64_f64
local rt_truncate_i32_f32 = rt_truncate_u32_f32
local rt_truncate_u64_f32 = rt_truncate_u32_f32
local rt_truncate_i64_f32 = rt_truncate_u32_f32

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

        import_bindings_lua = build_import_bindings(imports)

        content = (
            content
            + """

--// Named function dictionary
local NamedFunctionList = {
    """
            + (
                f'--// WebAssembly\n    ["__wasm_call_ctors"] = FUNC_LIST[{offsets["__wasm_call_ctors"]}];\n\n    '
                if "__wasm_call_ctors" in offsets else ""
            )
            + """--// Exported functions
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

"""
            + import_bindings_lua
            + """
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
""" + ("NamedFunctionList.__wasm_call_ctors()" if "__wasm_call_ctors" in offsets else "-- (no __wasm_call_ctors export — nothing to init)") + """

local function convertString(s)
    if type(s) == "string" then
        local addr = NamedFunctionList.malloc(#s + 1)
        store_string(memory_at_0, addr, s .. "\\0", #s + 1)
        return addr
    else
        return load_string(memory_at_0, s, NamedFunctionList.strlen(s))
    end
end

-- Pack a Lua list of strings into a C-style argv (array of char* in linear
-- memory) and return (argc, argv_ptr). Memory is leaked (mirrors libc); the
-- WASM module typically lives for the runtime of the script.
local function rbx_pack_argv(args)
    local argc = #args
    local addrs = {}
    for i = 1, argc do
        local s = tostring(args[i])
        local addr = NamedFunctionList.malloc(#s + 1)
        store_string(memory_at_0, addr, s .. "\\0", #s + 1)
        addrs[i] = addr
    end
    -- argv is a NULL-terminated array of pointers (4 bytes each in wasm32).
    local argv = NamedFunctionList.malloc((argc + 1) * 4)
    for i = 1, argc do
        rt_store_i32(memory_at_0, argv + (i - 1) * 4, addrs[i])
    end
    rt_store_i32(memory_at_0, argv + argc * 4, 0)
    return argc, argv
end

""" + ("main" in exported_functions and (
"-- Run main with the given arguments. Call setStdin/setEnv BEFORE this so\n"
"-- stdin reads and getenv() see what you provided.\n"
"local function rbx_run_main(...)\n"
'    local args = {...}\n'
'    if #args == 0 then args = {"rbx-cpp"} end\n'
"    local argc, argv = rbx_pack_argv(args)\n"
"    local code = NamedFunctionList.main(argc, argv)\n"
"    if rbx_flush_streams then rbx_flush_streams() end\n"
"    return code\n"
"end") or "") + """

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
    flushStreams = rbx_flush_streams;
    setStdin = rbx_set_stdin;
    appendStdin = rbx_append_stdin;
    setEnv = rbx_set_env;
    FUNC_LIST = FUNC_LIST;""" + ("main" in exported_functions and "\nrun = rbx_run_main;" or "") + """
}"""
        )

        with open("dist\\main.lua", "w") as f:
            f.write(content)


if __name__ == "__main__":
    main()
