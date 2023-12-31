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

    # Remove build and dist folders if they exist
    if os.path.exists("build"):
        shutil.rmtree("build")
    if os.path.exists("dist"):
        shutil.rmtree("dist")

    # Create build and dist folders
    os.mkdir("build")
    os.mkdir("dist")

    # Change directory to build
    os.chdir("build")

    # Run cmake
    subprocess.run("emcmake cmake -DCMAKE_BUILD_TYPE=Debug ..", shell=True)

    # Build project
    subprocess.run("cmake --build .", shell=True)

    # Change directory to src
    os.chdir("src")

    # Convert wasm to lua
    subprocess.run("wasm2luau RbxCppProject.wasm > ..\\..\\dist\\main.lua", shell=True)

    # Change directory back to build
    os.chdir("..\\..")

    # Remove build folder
    shutil.rmtree("build")

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

        content = content.replace(
            """function load.f64(memory, addr)
		local raw = load_i64(memory, addr)

		return reinterpret_f64_i64(raw)
	end""",
            """function load.f64(memory, addr)
		local raw = load_i64(memory, addr)

		return reinterpret_f64_i64(raw)
	end

    function load.string(memory, addr, len)
        local buffer = table.create(len)
        local data = memory.data

        for i = 1, len do
            local raw = load_byte(data, addr + i - 1)

            buffer[i] = string_char(raw)
        end

        return table.concat(buffer)
    end""",
        )

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
	""" + "\n".join(
                [
                    f'["{func}"] = FUNC_LIST[{offsets[func]}];'
                    for func in exported_functions
                ]
            )
        + """
}

--// Pre-init environment function setup
local load_string = rt.load.string

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
	rt.store.string(memory_at_0, addr, ret .. "\\0", #ret + 1)
	return addr
end

--// Initialize
run_init_code()
memory_at_0 = MEMORY_LIST[0]
NamedFunctionList.__wasm_call_ctors()

local function convertString(s)
	if type(s) == "string" then
		local addr = NamedFunctionList.malloc(#s + 1)
		rt.store.string(memory_at_0, addr, s .. "\\0", #s + 1)
		return addr
	else
		return load_string(memory_at_0, s, NamedFunctionList.strlen(s))
	end
end

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
    rt = rt;
    FUNC_LIST = FUNC_LIST;
}"""
        )

        with open("dist\\main.lua", "w") as f:
            f.write(content)


if __name__ == "__main__":
    main()
