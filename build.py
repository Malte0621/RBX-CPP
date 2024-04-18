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

        content2 = content.split("return module")
        content = """--!optimize 2
--!native
local lastSleep = 0
function peformSleepCheck()
	if tick() - lastSleep >= 1 then
		lastSleep = tick()
		task.wait()
	end
end

local Integer = (function()
local Numeric = {}

local NUM_ZERO = Vector3.zero
local NUM_ONE, NUM_SIX_FOUR

local NUM_BIT_26, NUM_BIT_52

local bit_lshift = bit32.lshift
local bit_rshift = bit32.rshift
local bit_arshift = bit32.arshift

local bit_and = bit32.band
local bit_or = bit32.bor
local bit_xor = bit32.bxor
local bit_not = bit32.bnot

local bit_extract = bit32.extract
local bit_replace = bit32.replace

local from_u32, from_u64, into_u64
local num_subtract, num_divide_unsigned, num_negate
local num_or, num_shift_left, num_shift_right_unsigned
local num_is_less_unsigned

-- X: a[0 ..21]
-- Y: a[22..31]
--  | b[0 ..11]
-- Z: b[12..31]
local constructor = Vector3.new

function Numeric.from_u32(data_1, data_2)
	peformSleepCheck()
	local x = bit_and(data_1, 0x3FFFFF)
	local y = bit_and(data_2, 0x3FFFFF)
	local z = bit_replace(bit_rshift(data_1, 22), bit_rshift(data_2, 22), 10, 10)

	return constructor(x, y, z)
end

local function num_is_zero(value)
	peformSleepCheck()
	return value == NUM_ZERO
end

local function load_d1(value)
	peformSleepCheck()
	return bit_replace(bit_and(value.X, 0x3FFFFF), value.Z, 22, 10)
end

local function load_d2(value)
	peformSleepCheck()
	return bit_replace(bit_and(value.Y, 0x3FFFFF), bit_rshift(value.Z, 10), 22, 10)
end

local function into_u32(value)
	peformSleepCheck()
	local x, y, z = value.X, value.Y, value.Z
	return bit_replace(bit_and(x, 0x3FFFFF), z, 22, 10), bit_replace(bit_and(y, 0x3FFFFF), bit_rshift(z, 10), 22, 10)
end
Numeric.into_u32 = into_u32

function Numeric.from_u64(value)
	peformSleepCheck()
	return from_u32(bit_and(value % 0x100000000), bit_and(value / 0x100000000))
end

function Numeric.into_u64(value)
	peformSleepCheck()
	local value_1, value_2 = into_u32(value)
	return value_1 + value_2 * 0x100000000
end

function Numeric.add(lhs, rhs)
	peformSleepCheck()
	local lhs_1, lhs_2 = into_u32(lhs)
	local rhs_1, rhs_2 = into_u32(rhs)
	local data_1 = lhs_1 + rhs_1
	local data_2 = lhs_2 + rhs_2

	if data_1 >= 0x100000000 then
		data_1 = data_1 - 0x100000000
		data_2 = data_2 + 1
	end

	if data_2 >= 0x100000000 then
		data_2 = data_2 - 0x100000000
	end

	return from_u32(data_1, data_2)
end

function Numeric.subtract(lhs, rhs)
	peformSleepCheck()
	local lhs_1, lhs_2 = into_u32(lhs)
	local rhs_1, rhs_2 = into_u32(rhs)
	local data_1 = lhs_1 - rhs_1
	local data_2 = lhs_2 - rhs_2

	if data_1 < 0 then
		data_1 = data_1 + 0x100000000
		data_2 = data_2 - 1
	end

	if data_2 < 0 then
		data_2 = data_2 + 0x100000000
	end

	return from_u32(data_1, data_2)
end

function Numeric.multiply(lhs, rhs)
	peformSleepCheck()
	if num_is_zero(lhs) or num_is_zero(rhs) then
		return NUM_ZERO
	elseif num_is_less_unsigned(lhs, NUM_BIT_26) and num_is_less_unsigned(rhs, NUM_BIT_26) then
		return from_u64(load_d1(lhs) * load_d1(rhs))
	end

	-- Divide each long into 4 chunks of 16 bits, and then add up 4x4 products.
	-- We can skip products that would overflow.
	local lhs_1, lhs_2 = into_u32(lhs)
	local rhs_1, rhs_2 = into_u32(rhs)

	local a48 = bit_rshift(lhs_2, 16)
	local a32 = bit_and(lhs_2, 0xFFFF)
	local a16 = bit_rshift(lhs_1, 16)
	local a00 = bit_and(lhs_1, 0xFFFF)

	local b48 = bit_rshift(rhs_2, 16)
	local b32 = bit_and(rhs_2, 0xFFFF)
	local b16 = bit_rshift(rhs_1, 16)
	local b00 = bit_and(rhs_1, 0xFFFF)

	local c00 = a00 * b00
	local c16 = bit_rshift(c00, 16)

	c00 = bit_and(c00, 0xFFFF)
	c16 = c16 + a16 * b00

	local c32 = bit_rshift(c16, 16)

	c16 = bit_and(c16, 0xFFFF)
	c16 = c16 + a00 * b16
	c32 = c32 + bit_rshift(c16, 16)
	c16 = bit_and(c16, 0xFFFF)
	c32 = c32 + a32 * b00

	local c48 = bit_rshift(c32, 16)

	c32 = bit_and(c32, 0xFFFF)
	c32 = c32 + a16 * b16
	c48 = c48 + bit_rshift(c32, 16)
	c32 = bit_and(c32, 0xFFFF)
	c32 = c32 + a00 * b32
	c48 = c48 + bit_rshift(c32, 16)
	c32 = bit_and(c32, 0xFFFF)
	c48 = c48 + a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48
	c48 = bit_and(c48, 0xFFFF)

	local data_1 = bit_replace(c00, c16, 16, 16)
	local data_2 = bit_replace(c32, c48, 16, 16)

	return from_u32(data_1, data_2)
end

function Numeric.divide_unsigned(lhs, rhs)
	peformSleepCheck()
	if num_is_zero(rhs) then
		error("division by zero")
	elseif num_is_zero(lhs) then
		return NUM_ZERO, NUM_ZERO
	elseif num_is_less_unsigned(lhs, NUM_BIT_52) and num_is_less_unsigned(rhs, NUM_BIT_52) then
		local lhs_u = into_u64(lhs)
		local rhs_u = into_u64(rhs)

		return from_u64(lhs_u / rhs_u), from_u64(lhs_u % rhs_u)
	end

	local quotient = NUM_ZERO
	local remainder = NUM_ZERO

	local num_1, num_2 = into_u32(lhs)

	for i = 63, 0, -1 do
		local rem_1, rem_2 = into_u32(num_shift_left(remainder, NUM_ONE))

		if i > 31 then
			rem_1 = bit_or(rem_1, bit_extract(num_2, i - 32, 1))
		else
			rem_1 = bit_or(rem_1, bit_extract(num_1, i, 1))
		end

		remainder = from_u32(rem_1, rem_2)

		if not num_is_less_unsigned(remainder, rhs) then
			remainder = num_subtract(remainder, rhs)
			quotient = num_or(quotient, num_shift_left(NUM_ONE, from_u32(i, 0)))
		end
	end

	return quotient, remainder
end

local function num_is_negative(value)
	peformSleepCheck()
	return value.Z >= 0x80000
end

function Numeric.divide_signed(lhs, rhs)
	peformSleepCheck()
	local left_negative = num_is_negative(lhs)
	local right_negative = num_is_negative(rhs)

	if left_negative then
		lhs = num_negate(lhs)
	end

	if right_negative then
		rhs = num_negate(rhs)
	end

	local quotient, remainder = num_divide_unsigned(lhs, rhs)

	if left_negative ~= right_negative then
		quotient = num_negate(quotient)
	end

	if left_negative then
		remainder = num_negate(remainder)
	end

	return quotient, remainder
end

function Numeric.negate(value)
	peformSleepCheck()
	local value_1, value_2 = into_u32(value)
	local data_1 = bit_not(value_1) + 1
	local data_2 = bit_not(value_2)

	if data_1 >= 0x100000000 then
		data_1 = data_1 - 0x100000000
		data_2 = data_2 + 1
	end

	if data_2 >= 0x100000000 then
		data_2 = data_2 - 0x100000000
	end

	return from_u32(data_1, data_2)
end

function Numeric.bit_and(lhs, rhs)
	peformSleepCheck()
	local x = bit_and(lhs.X, rhs.X)
	local y = bit_and(lhs.Y, rhs.Y)
	local z = bit_and(lhs.Z, rhs.Z)

	return constructor(x, y, z)
end

function Numeric.bit_not(value)
	peformSleepCheck()
	local x = bit_and(bit_not(value.X), 0xFFFFFF)
	local y = bit_and(bit_not(value.Y), 0xFFFFFF)
	local z = bit_and(bit_not(value.Z), 0xFFFFFF)

	return constructor(x, y, z)
end

function Numeric.bit_or(lhs, rhs)
	peformSleepCheck()
	local x = bit_or(lhs.X, rhs.X)
	local y = bit_or(lhs.Y, rhs.Y)
	local z = bit_or(lhs.Z, rhs.Z)

	return constructor(x, y, z)
end

function Numeric.bit_xor(lhs, rhs)
	peformSleepCheck()
	local x = bit_xor(lhs.X, rhs.X)
	local y = bit_xor(lhs.Y, rhs.Y)
	local z = bit_xor(lhs.Z, rhs.Z)

	return constructor(x, y, z)
end

function Numeric.shift_left(lhs, rhs)
	peformSleepCheck()
	local count = rhs.X % 64

	if count == 0 then
		return lhs
	elseif count < 32 then
		local pad = 32 - count
		local lhs_1, lhs_2 = into_u32(lhs)

		local data_1 = bit_lshift(lhs_1, count)
		local data_2 = bit_replace(bit_rshift(lhs_1, pad), lhs_2, count, pad)

		return from_u32(data_1, data_2)
	else
		local lhs_1 = load_d1(lhs)

		return from_u32(0, bit_lshift(lhs_1, count - 32))
	end
end

function Numeric.shift_right_unsigned(lhs, rhs)
	peformSleepCheck()
	local count = rhs.X % 64

	if count == 0 then
		return lhs
	elseif count < 32 then
		local lhs_1, lhs_2 = into_u32(lhs)

		local data_1 = bit_replace(bit_rshift(lhs_1, count), lhs_2, 32 - count, count)
		local data_2 = bit_rshift(lhs_2, count)

		return from_u32(data_1, data_2)
	else
		local lhs_2 = load_d2(lhs)

		return from_u32(bit_rshift(lhs_2, count - 32), 0)
	end
end

function Numeric.shift_right_signed(lhs, rhs)
	peformSleepCheck()
	local count = rhs.X % 64

	if count == 0 then
		return lhs
	elseif count < 32 then
		local lhs_1, lhs_2 = into_u32(lhs)

		local data_1 = bit_replace(bit_rshift(lhs_1, count), lhs_2, 32 - count, count)
		local data_2 = bit_arshift(lhs_2, count)

		return from_u32(data_1, data_2)
	else
		local lhs_2 = load_d2(lhs)

		local data_1 = bit_arshift(lhs_2, count - 32)
		local data_2 = lhs_2 >= 0x80000000 and 0xFFFFFFFF or 0

		return from_u32(data_1, data_2)
	end
end

function Numeric.rotate_left(lhs, rhs)
	peformSleepCheck()
	if num_is_zero(rhs) then
		return lhs
	else
		local data_1 = num_shift_left(lhs, rhs)
		local data_2 = num_shift_right_unsigned(lhs, num_subtract(NUM_SIX_FOUR, rhs))

		return num_or(data_1, data_2)
	end
end

function Numeric.rotate_right(lhs, rhs)
	peformSleepCheck()
	if num_is_zero(rhs) then
		return lhs
	else
		local data_1 = num_shift_right_unsigned(lhs, rhs)
		local data_2 = num_shift_left(lhs, num_subtract(NUM_SIX_FOUR, rhs))

		return num_or(data_1, data_2)
	end
end

function Numeric.is_equal(lhs, rhs)
	peformSleepCheck()
	return lhs == rhs
end

function Numeric.is_less_unsigned(lhs, rhs)
	peformSleepCheck()
	local data_l_2 = load_d2(lhs)
	local data_r_2 = load_d2(rhs)

	return data_l_2 < data_r_2 or (data_l_2 == data_r_2 and load_d1(lhs) < load_d1(rhs))
end

function Numeric.is_greater_unsigned(lhs, rhs)
	peformSleepCheck()
	local data_l_2 = load_d2(lhs)
	local data_r_2 = load_d2(rhs)

	return data_l_2 > data_r_2 or (data_l_2 == data_r_2 and load_d1(lhs) > load_d1(rhs))
end

function Numeric.is_less_signed(lhs, rhs)
	peformSleepCheck()
	local neg_a = num_is_negative(lhs)
	local neg_b = num_is_negative(rhs)

	if neg_a and not neg_b then
		return true
	elseif not neg_a and neg_b then
		return false
	else
		return num_is_negative(num_subtract(lhs, rhs))
	end
end

function Numeric.is_greater_signed(lhs, rhs)
	peformSleepCheck()
	local neg_a = num_is_negative(lhs)
	local neg_b = num_is_negative(rhs)

	if neg_a and not neg_b then
		return false
	elseif not neg_a and neg_b then
		return true
	else
		return num_is_negative(num_subtract(rhs, lhs))
	end
end

from_u32 = Numeric.from_u32
from_u64 = Numeric.from_u64
into_u64 = Numeric.into_u64

num_subtract = Numeric.subtract
num_divide_unsigned = Numeric.divide_unsigned
num_negate = Numeric.negate

num_or = Numeric.bit_or
num_shift_left = Numeric.shift_left
num_shift_right_unsigned = Numeric.shift_right_unsigned

Numeric.is_negative = num_is_negative
Numeric.is_zero = num_is_zero
num_is_less_unsigned = Numeric.is_less_unsigned

NUM_ONE = from_u64(1)
NUM_SIX_FOUR = from_u64(64)
NUM_BIT_26 = from_u64(0x4000000)
NUM_BIT_52 = from_u64(0x10000000000000)

Numeric.ZERO = NUM_ZERO
Numeric.ONE = NUM_ONE

return table.freeze(Numeric)

end)()
local rt = (function()
local module = {}

local bit_and = bit32.band
local bit_or = bit32.bor
local bit_xor = bit32.bxor
local bit_lshift = bit32.lshift
local bit_rshift = bit32.rshift

local num_from_u32 = Integer.from_u32
local num_into_u32 = Integer.into_u32

local function to_i32(num)
	peformSleepCheck()
	return bit_xor(num, 0x80000000) - 0x80000000
end

local function no_op(num)
	peformSleepCheck()
	return num
end

module.i64 = Integer

do
	local add = {}
	local sub = {}
	local mul = {}
	local div = {}
	local rem = {}
	local neg = {}
	local min = {}
	local max = {}
	local copysign = {}
	local nearest = {}

	local assert = assert

	local math_abs = math.abs
	local math_fmod = math.fmod
	local math_modf = math.modf
	local math_round = math.round
	local math_sign = math.sign
	local math_min = math.min
	local math_max = math.max

	local num_divide_signed = Integer.divide_signed
	local num_divide_unsigned = Integer.divide_unsigned

	function add.i32(lhs, rhs)
		peformSleepCheck()
		return bit_or(lhs + rhs, 0)
	end

	function sub.i32(lhs, rhs)
		peformSleepCheck()
		return bit_or(lhs - rhs, 0)
	end

	function mul.i32(lhs, rhs)
		peformSleepCheck()
		if (lhs + rhs) < 0x8000000 then
			return bit_or(lhs * rhs, 0)
		else
			local a16 = bit_rshift(lhs, 16)
			local a00 = bit_and(lhs, 0xFFFF)
			local b16 = bit_rshift(rhs, 16)
			local b00 = bit_and(rhs, 0xFFFF)

			local c00 = a00 * b00
			local c16 = a16 * b00 + a00 * b16

			return bit_or(c00 + bit_lshift(c16, 16), 0)
		end
	end

	function div.i32(lhs, rhs)
		assert(rhs ~= 0, "division by zero")
		peformSleepCheck()

		lhs = to_i32(lhs)
		rhs = to_i32(rhs)

		return bit_or(math_modf(lhs / rhs), 0)
	end

	function div.u32(lhs, rhs)
		assert(rhs ~= 0, "division by zero")
		peformSleepCheck()

		return bit_or(math_modf(lhs / rhs), 0)
	end

	function rem.i32(lhs, rhs)
		assert(rhs ~= 0, "division by zero")
		peformSleepCheck()

		lhs = to_i32(lhs)
		rhs = to_i32(rhs)

		return bit_or(math_fmod(lhs, rhs), 0)
	end

	add.i64 = Integer.add
	sub.i64 = Integer.subtract
	mul.i64 = Integer.multiply
	div.i64 = num_divide_signed

	function rem.i64(lhs, rhs)
		peformSleepCheck()
		local _, remainder = num_divide_signed(lhs, rhs)

		return remainder
	end

	div.u64 = num_divide_unsigned

	function rem.u64(lhs, rhs)
		peformSleepCheck()
		local _, remainder = num_divide_unsigned(lhs, rhs)

		return remainder
	end

	function neg.f32(num)
		peformSleepCheck()
		return -num
	end

	function min.f32(lhs, rhs)
		peformSleepCheck()
		if rhs == rhs then
			return math_min(lhs, rhs)
		else
			return rhs
		end
	end

	function max.f32(lhs, rhs)
		peformSleepCheck()
		if rhs == rhs then
			return math_max(lhs, rhs)
		else
			return rhs
		end
	end

	local CP_INSTANCE = buffer.create(8)

	local buffer_write_f64 = buffer.writef64
	local buffer_read_i8 = buffer.readi8

	function copysign.f32(lhs, rhs)
		peformSleepCheck()
		buffer_write_f64(CP_INSTANCE, 0, rhs)

		if buffer_read_i8(CP_INSTANCE, 7) >= 0 then
			return (math_abs(lhs))
		else
			return -math_abs(lhs)
		end
	end

	function nearest.f32(num)
		peformSleepCheck()
		local result = math_round(num)

		if (math_abs(num) + 0.5) % 2 == 1 then
			return result - math_sign(result)
		else
			return result
		end
	end

	neg.f64 = neg.f32
	min.f64 = min.f32
	max.f64 = max.f32
	copysign.f64 = copysign.f32
	nearest.f64 = nearest.f32

	module.add = add
	module.sub = sub
	module.mul = mul
	module.div = div
	module.rem = rem
	module.neg = neg
	module.min = min
	module.max = max
	module.copysign = copysign
	module.nearest = nearest
end

do
	local clz = {}
	local ctz = {}
	local popcnt = {}

	local bit_countlz = bit32.countlz
	local bit_countrz = bit32.countrz

	local function popcnt_i32(num)
		peformSleepCheck()
		num = num - bit_and(bit_rshift(num, 1), 0x55555555)
		num = bit_and(num, 0x33333333) + bit_and(bit_rshift(num, 2), 0x33333333)
		num = bit_and((num + bit_rshift(num, 4)), 0x0F0F0F0F)
		num = num + bit_rshift(num, 8)
		num = num + bit_rshift(num, 16)
		return bit_and(num, 0x0000003F)
	end

	popcnt.i32 = popcnt_i32

	function clz.i64(num)
		peformSleepCheck()
		local data_1, data_2 = num_into_u32(num)
		local temp

		if data_2 == 0 then
			temp = bit_countlz(data_1) + 32
		else
			temp = bit_countlz(data_2)
		end

		return num_from_u32(temp, 0)
	end

	function ctz.i64(num)
		peformSleepCheck()
		local data_1, data_2 = num_into_u32(num)
		local temp

		if data_1 == 0 then
			temp = bit_countrz(data_2) + 32
		else
			temp = bit_countrz(data_1)
		end

		return num_from_u32(temp, 0)
	end

	function popcnt.i64(num)
		peformSleepCheck()
		local data_1, data_2 = num_into_u32(num)
		local temp = popcnt_i32(data_1) + popcnt_i32(data_2)

		return num_from_u32(temp, 0)
	end

	module.clz = clz
	module.ctz = ctz
	module.popcnt = popcnt
end

do
	local eq = {}
	local ne = {}
	local le = {}
	local lt = {}
	local ge = {}
	local gt = {}

	local num_is_equal = Integer.is_equal
	local num_is_less_signed = Integer.is_less_signed
	local num_is_less_unsigned = Integer.is_less_unsigned
	local num_is_greater_signed = Integer.is_greater_signed
	local num_is_greater_unsigned = Integer.is_greater_unsigned

	function le.i32(lhs, rhs)
		peformSleepCheck()
		return to_i32(lhs) <= to_i32(rhs)
	end

	function lt.i32(lhs, rhs)
		peformSleepCheck()
		return to_i32(lhs) < to_i32(rhs)
	end

	function ge.i32(lhs, rhs)
		peformSleepCheck()
		return to_i32(lhs) >= to_i32(rhs)
	end

	function gt.i32(lhs, rhs)
		peformSleepCheck()
		return to_i32(lhs) > to_i32(rhs)
	end

	eq.i64 = num_is_equal

	function ne.i64(lhs, rhs)
		peformSleepCheck()
		return not num_is_equal(lhs, rhs)
	end

	function le.i64(lhs, rhs)
		peformSleepCheck()
		return num_is_less_signed(lhs, rhs) or num_is_equal(lhs, rhs)
	end

	function le.u64(lhs, rhs)
		peformSleepCheck()
		return num_is_less_unsigned(lhs, rhs) or num_is_equal(lhs, rhs)
	end

	lt.i64 = num_is_less_signed
	lt.u64 = num_is_less_unsigned

	function ge.i64(lhs, rhs)
		peformSleepCheck()
		return num_is_greater_signed(lhs, rhs) or num_is_equal(lhs, rhs)
	end

	function ge.u64(lhs, rhs)
		peformSleepCheck()
		return num_is_greater_unsigned(lhs, rhs) or num_is_equal(lhs, rhs)
	end

	gt.i64 = num_is_greater_signed
	gt.u64 = num_is_greater_unsigned

	module.eq = eq
	module.ne = ne
	module.le = le
	module.lt = lt
	module.ge = ge
	module.gt = gt
end

do
	local band = {}
	local bor = {}
	local bxor = {}
	local bnot = {}

	band.i64 = Integer.bit_and
	bor.i64 = Integer.bit_or
	bxor.i64 = Integer.bit_xor
	bnot.i64 = Integer.bit_not

	module.band = band
	module.bor = bor
	module.bxor = bxor
	module.bnot = bnot
end

do
	local shl = {}
	local shr = {}
	local rotl = {}
	local rotr = {}

	local bit_arshift = bit32.arshift
	local bit_lrotate = bit32.lrotate
	local bit_rrotate = bit32.rrotate

	function shl.i32(lhs, rhs)
		peformSleepCheck()
		return bit_lshift(lhs, rhs % 32)
	end

	function shr.u32(lhs, rhs)
		peformSleepCheck()
		return bit_rshift(lhs, rhs % 32)
	end

	function shr.i32(lhs, rhs)
		peformSleepCheck()
		return bit_arshift(lhs, rhs % 32)
	end

	function rotl.i32(lhs, rhs)
		peformSleepCheck()
		return bit_lrotate(lhs, rhs % 32)
	end

	function rotr.i32(lhs, rhs)
		peformSleepCheck()
		return bit_rrotate(lhs, rhs % 32)
	end

	shl.i64 = Integer.shift_left
	shr.i64 = Integer.shift_right_signed
	shr.u64 = Integer.shift_right_unsigned
	rotl.i64 = Integer.rotate_left
	rotr.i64 = Integer.rotate_right

	module.shl = shl
	module.shr = shr
	module.rotl = rotl
	module.rotr = rotr
end

do
	local wrap = {}
	local truncate = {}
	local saturate = {}
	local extend = {}
	local convert = {}
	local demote = {}
	local promote = {}
	local reinterpret = {}

	local math_ceil = math.ceil
	local math_floor = math.floor
	local math_clamp = math.clamp

	local NUM_ZERO = Integer.ZERO
	local NUM_MIN_I64 = num_from_u32(0, 0x80000000)
	local NUM_MAX_I64 = num_from_u32(0xFFFFFFFF, 0x7FFFFFFF)
	local NUM_MAX_U64 = num_from_u32(0xFFFFFFFF, 0xFFFFFFFF)

	local num_from_u64 = Integer.from_u64
	local num_into_u64 = Integer.into_u64

	local num_negate = Integer.negate
	local num_is_negative = Integer.is_negative

	local function truncate_f64(num)
		peformSleepCheck()
		if num >= 0 then
			return math_floor(num)
		else
			return math_ceil(num)
		end
	end

	function wrap.i32_i64(num)
		peformSleepCheck()
		local data_1, _ = num_into_u32(num)

		return data_1
	end

	function truncate.i32_f32(num)
		peformSleepCheck()
		return bit_or(truncate_f64(num), 0)
	end

	truncate.i32_f64 = truncate.i32_f32

	truncate.u32_f32 = truncate_f64
	truncate.u32_f64 = truncate_f64

	function truncate.i64_f32(num)
		peformSleepCheck()
		if num < 0 then
			local temp = num_from_u64(-num)

			return num_negate(temp)
		else
			return num_from_u64(num)
		end
	end

	truncate.i64_f64 = truncate.i64_f32

	function truncate.u64_f32(num)
		peformSleepCheck()
		if num <= 0 then
			return NUM_ZERO
		else
			return num_from_u64(math_floor(num))
		end
	end

	truncate.u64_f64 = truncate.u64_f32

	truncate.f32 = truncate_f64
	truncate.f64 = truncate_f64

	function saturate.i32_f32(num)
		peformSleepCheck()
		local temp = math_clamp(truncate_f64(num), -0x80000000, 0x7FFFFFFF)

		return bit_or(temp, 0)
	end

	saturate.i32_f64 = saturate.i32_f32

	function saturate.u32_f32(num)
		peformSleepCheck()
		local temp = math_clamp(truncate_f64(num), 0, 0xFFFFFFFF)

		return temp
	end

	saturate.u32_f64 = saturate.u32_f32

	local truncate_i64_f64 = truncate.i64_f64

	function saturate.i64_f32(num)
		peformSleepCheck()
		if num >= 2 ^ 63 - 1 then
			return NUM_MAX_I64
		elseif num <= -2 ^ 63 then
			return NUM_MIN_I64
		else
			return truncate_i64_f64(num)
		end
	end

	saturate.i64_f64 = saturate.i64_f32

	function saturate.u64_f32(num)
		peformSleepCheck()
		if num >= 2 ^ 64 then
			return NUM_MAX_U64
		elseif num <= 0 then
			return NUM_ZERO
		else
			return truncate_i64_f64(num)
		end
	end

	saturate.u64_f64 = saturate.u64_f32

	function extend.i32_n8(num)
		peformSleepCheck()
		num = bit_and(num, 0xFF)

		if num >= 0x80 then
			return bit_or(num - 0x100, 0)
		else
			return num
		end
	end

	function extend.i32_n16(num)
		peformSleepCheck()
		num = bit_and(num, 0xFFFF)

		if num >= 0x8000 then
			return bit_or(num - 0x10000, 0)
		else
			return num
		end
	end

	function extend.i64_n8(num)
		peformSleepCheck()
		local data_1, _ = num_into_u32(num)

		data_1 = bit_and(data_1, 0xFF)

		if data_1 >= 0x80 then
			local temp = num_from_u32(-data_1 + 0x100, 0)

			return num_negate(temp)
		else
			return num_from_u32(data_1, 0)
		end
	end

	function extend.i64_n16(num)
		peformSleepCheck()
		local data_1, _ = num_into_u32(num)

		data_1 = bit_and(data_1, 0xFFFF)

		if data_1 >= 0x8000 then
			local temp = num_from_u32(-data_1 + 0x10000, 0)

			return num_negate(temp)
		else
			return num_from_u32(data_1, 0)
		end
	end

	function extend.i64_n32(num)
		peformSleepCheck()
		local data_1, _ = num_into_u32(num)

		if data_1 >= 0x80000000 then
			local temp = num_from_u32(-data_1 + 0x100000000, 0)

			return num_negate(temp)
		else
			return num_from_u32(data_1, 0)
		end
	end

	function extend.i64_i32(num)
		peformSleepCheck()
		if num >= 0x80000000 then
			local temp = num_from_u32(-num + 0x100000000, 0)

			return num_negate(temp)
		else
			return num_from_u32(num, 0)
		end
	end

	function extend.i64_u32(num)
		peformSleepCheck()
		return num_from_u32(num, 0)
	end

	convert.f32_i32 = to_i32
	convert.f32_u32 = no_op

	function convert.f32_i64(num)
		peformSleepCheck()
		if num_is_negative(num) then
			local temp = num_negate(num)

			return -num_into_u64(temp)
		else
			return num_into_u64(num)
		end
	end

	convert.f32_u64 = num_into_u64
	convert.f64_i32 = to_i32
	convert.f64_u32 = no_op
	convert.f64_i64 = convert.f32_i64
	convert.f64_u64 = num_into_u64

	demote.f32_f64 = no_op

	promote.f64_f32 = no_op

	local RE_INSTANCE = buffer.create(8)

	local buffer_read_f32 = buffer.readf32
	local buffer_read_f64 = buffer.readf64
	local buffer_read_u32 = buffer.readu32

	local buffer_write_f32 = buffer.writef32
	local buffer_write_f64 = buffer.writef64
	local buffer_write_u32 = buffer.writeu32

	function reinterpret.i32_f32(num)
		peformSleepCheck()
		buffer_write_f32(RE_INSTANCE, 0, num)

		return buffer_read_u32(RE_INSTANCE, 0)
	end

	function reinterpret.i64_f64(num)
		peformSleepCheck()
		buffer_write_f64(RE_INSTANCE, 0, num)

		local data_1 = buffer_read_u32(RE_INSTANCE, 0)
		local data_2 = buffer_read_u32(RE_INSTANCE, 4)

		return num_from_u32(data_1, data_2)
	end

	function reinterpret.f32_i32(num)
		peformSleepCheck()
		buffer_write_u32(RE_INSTANCE, 0, num)

		return buffer_read_f32(RE_INSTANCE, 0)
	end

	function reinterpret.f64_i64(num)
		peformSleepCheck()
		local data_1, data_2 = num_into_u32(num)

		buffer_write_u32(RE_INSTANCE, 0, data_1)
		buffer_write_u32(RE_INSTANCE, 4, data_2)

		return buffer_read_f64(RE_INSTANCE, 0)
	end

	module.wrap = wrap
	module.truncate = truncate
	module.saturate = saturate
	module.extend = extend
	module.convert = convert
	module.demote = demote
	module.promote = promote
	module.reinterpret = reinterpret
end

do
	local load = {}
	local store = {}
	local allocator = {}

	local string_sub = string.sub

	local buffer_create = buffer.create
	local buffer_to_string = buffer.tostring
	local buffer_from_string = buffer.fromstring

	local buffer_len = buffer.len
	local buffer_copy = buffer.copy
	local buffer_fill = buffer.fill

	local buffer_read_i8 = buffer.readi8
	local buffer_read_u8 = buffer.readu8
	local buffer_read_i16 = buffer.readi16
	local buffer_read_u16 = buffer.readu16
	local buffer_read_i32 = buffer.readi32
	local buffer_read_u32 = buffer.readu32
	local buffer_read_f32 = buffer.readf32
	local buffer_read_f64 = buffer.readf64

	local buffer_write_u8 = buffer.writeu8
	local buffer_write_u16 = buffer.writeu16
	local buffer_write_u32 = buffer.writeu32
	local buffer_write_f32 = buffer.writef32
	local buffer_write_f64 = buffer.writef64

	function load.i32_i8(memory, addr)
		peformSleepCheck()
		return bit_or(buffer_read_i8(memory.data, addr), 0)
	end

	function load.i32_u8(memory, addr)
		peformSleepCheck()
		return buffer_read_u8(memory.data, addr)
	end

	function load.i32_i16(memory, addr)
		peformSleepCheck()
		return bit_or(buffer_read_i16(memory.data, addr), 0)
	end

	function load.i32_u16(memory, addr)
		peformSleepCheck()
		return buffer_read_u16(memory.data, addr)
	end

	function load.i32(memory, addr)
		peformSleepCheck()
		return buffer_read_u32(memory.data, addr)
	end

	function load.i64_i8(memory, addr)
		peformSleepCheck()
		local value = buffer_read_i8(memory.data, addr)

		if value >= 0 then
			return num_from_u32(value, 0)
		else
			return num_from_u32(value + 0x100000000, 0xFFFFFFFF)
		end
	end

	function load.i64_u8(memory, addr)
		peformSleepCheck()
		return num_from_u32(buffer_read_u8(memory.data, addr), 0)
	end

	function load.i64_i16(memory, addr)
		peformSleepCheck()
		local value = buffer_read_i16(memory.data, addr)

		if value >= 0 then
			return num_from_u32(value, 0)
		else
			return num_from_u32(value + 0x100000000, 0xFFFFFFFF)
		end
	end

	function load.i64_u16(memory, addr)
		peformSleepCheck()
		return num_from_u32(buffer_read_u16(memory.data, addr), 0)
	end

	function load.i64_i32(memory, addr)
		peformSleepCheck()
		local value = buffer_read_i32(memory.data, addr)

		if value >= 0 then
			return num_from_u32(value, 0)
		else
			return num_from_u32(value + 0x100000000, 0xFFFFFFFF)
		end
	end

	function load.i64_u32(memory, addr)
		peformSleepCheck()
		return num_from_u32(buffer_read_u32(memory.data, addr), 0)
	end

	function load.i64(memory, addr)
		peformSleepCheck()
		local data = memory.data
		local value_1 = buffer_read_u32(data, addr)
		local value_2 = buffer_read_u32(data, addr + 4)

		return num_from_u32(value_1, value_2)
	end

	function load.f32(memory, addr)
		peformSleepCheck()
		return buffer_read_f32(memory.data, addr)
	end

	function load.f64(memory, addr)
		peformSleepCheck()
		return buffer_read_f64(memory.data, addr)
	end

	function load.string(memory, addr, len)
		peformSleepCheck()
		local temp = buffer_create(len)

		buffer_copy(temp, 0, memory.data, addr, len)

		return buffer_to_string(temp)
	end

	function store.i32_n8(memory, addr, value)
		peformSleepCheck()
		buffer_write_u8(memory.data, addr, value)
	end

	function store.i32_n16(memory, addr, value)
		peformSleepCheck()
		buffer_write_u16(memory.data, addr, value)
	end

	function store.i32(memory, addr, value)
		peformSleepCheck()
		buffer_write_u32(memory.data, addr, value)
	end

	function store.i64_n8(memory, addr, value)
		peformSleepCheck()
		local value_1, _ = num_into_u32(value)

		buffer_write_u8(memory.data, addr, value_1)
	end

	function store.i64_n16(memory, addr, value)
		peformSleepCheck()
		local value_1, _ = num_into_u32(value)

		buffer_write_u16(memory.data, addr, value_1)
	end

	function store.i64_n32(memory, addr, value)
		peformSleepCheck()
		local value_1, _ = num_into_u32(value)

		buffer_write_u32(memory.data, addr, value_1)
	end

	function store.i64(memory, addr, value)
		peformSleepCheck()
		local data = memory.data
		local value_1, value_2 = num_into_u32(value)

		buffer_write_u32(data, addr, value_1)
		buffer_write_u32(data, addr + 4, value_2)
	end

	function store.f32(memory, addr, value)
		peformSleepCheck()
		buffer_write_f32(memory.data, addr, value)
	end

	function store.f64(memory, addr, value)
		peformSleepCheck()
		buffer_write_f64(memory.data, addr, value)
	end

	function store.string(memory, addr, data, len)
		peformSleepCheck()
		local content = if not len or len == #data then data else string_sub(data, 1, len)
		local temp = buffer_from_string(content)

		buffer_copy(memory.data, addr, temp)
	end

	function store.copy(memory_1, addr_1, memory_2, addr_2, len)
		peformSleepCheck()
		buffer_copy(memory_1.data, addr_1, memory_2.data, addr_2, len)
	end

	function store.fill(memory, addr, len, value)
		peformSleepCheck()
		buffer_fill(memory.data, addr, value, len)
	end

	local WASM_PAGE_SIZE = 65536

	function allocator.new(min, max)
		peformSleepCheck()
		return { max = max, data = buffer_create(min * WASM_PAGE_SIZE) }
	end

	function allocator.size(memory)
		peformSleepCheck()
		return buffer_len(memory.data) / WASM_PAGE_SIZE
	end

	function allocator.grow(memory, num)
		peformSleepCheck()
		local old = allocator.size(memory)
		local new = old + num

		if new <= memory.max then
			local reallocated = buffer_create(new * WASM_PAGE_SIZE)

			buffer_copy(reallocated, 0, memory.data)

			memory.data = reallocated

			return old
		else
			return 0xFFFFFFFF
		end
	end

	module.load = load
	module.store = store
	module.allocator = allocator
end

return module""" + content2[1]

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
