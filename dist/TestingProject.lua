local Integer = (function()
local Numeric = {}

local NUM_ZERO, NUM_ONE, NUM_SIX_FOUR
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
local num_is_negative, num_is_zero, num_is_less_unsigned

-- X: a[0 ..21]
-- Y: a[22..31]
--  | b[0 ..11]
-- Z: b[12..31]
local constructor = Vector3.new

function Numeric.from_u32(data_1, data_2)
	local x = bit_and(data_1, 0x3FFFFF)
	local y = bit_and(data_2, 0x3FFFFF)
	local z = bit_replace(bit_rshift(data_1, 22), bit_rshift(data_2, 22), 10, 10)

	return constructor(x, y, z)
end

local function load_d1(value)
	return bit_replace(bit_and(value.X, 0x3FFFFF), value.Z, 22, 10)
end

local function load_d2(value)
	return bit_replace(bit_and(value.Y, 0x3FFFFF), bit_rshift(value.Z, 10), 22, 10)
end

function Numeric.into_u32(value)
	return load_d1(value), load_d2(value)
end

function Numeric.from_u64(value)
	return from_u32(bit_and(value % 0x100000000), bit_and(value / 0x100000000))
end

function Numeric.into_u64(value)
	return load_d1(value) + load_d2(value) * 0x100000000
end

function Numeric.add(lhs, rhs)
	local data_1 = load_d1(lhs) + load_d1(rhs)
	local data_2 = load_d2(lhs) + load_d2(rhs)

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
	local data_1 = load_d1(lhs) - load_d1(rhs)
	local data_2 = load_d2(lhs) - load_d2(rhs)

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
	if num_is_zero(lhs) or num_is_zero(rhs) then
		return NUM_ZERO
	elseif num_is_less_unsigned(lhs, NUM_BIT_26) and num_is_less_unsigned(rhs, NUM_BIT_26) then
		return from_u64(load_d1(lhs) * load_d1(rhs))
	end

	-- Divide each long into 4 chunks of 16 bits, and then add up 4x4 products.
	-- We can skip products that would overflow.
	local lhs_1, lhs_2 = load_d1(lhs), load_d2(lhs)
	local rhs_1, rhs_2 = load_d1(rhs), load_d2(rhs)

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

	local num_1, num_2 = load_d1(lhs), load_d2(lhs)

	for i = 63, 0, -1 do
		local temp = num_shift_left(remainder, NUM_ONE)
		local rem_1, rem_2 = load_d1(temp), load_d2(temp)

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

function Numeric.divide_signed(lhs, rhs)
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
	local data_1 = bit_not(load_d1(value)) + 1
	local data_2 = bit_not(load_d2(value))

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
	local x = bit_and(lhs.X, rhs.X)
	local y = bit_and(lhs.Y, rhs.Y)
	local z = bit_and(lhs.Z, rhs.Z)

	return constructor(x, y, z)
end

function Numeric.bit_not(value)
	local x = bit_and(bit_not(value.X), 0xFFFFFF)
	local y = bit_and(bit_not(value.Y), 0xFFFFFF)
	local z = bit_and(bit_not(value.Z), 0xFFFFFF)

	return constructor(x, y, z)
end

function Numeric.bit_or(lhs, rhs)
	local x = bit_or(lhs.X, rhs.X)
	local y = bit_or(lhs.Y, rhs.Y)
	local z = bit_or(lhs.Z, rhs.Z)

	return constructor(x, y, z)
end

function Numeric.bit_xor(lhs, rhs)
	local x = bit_xor(lhs.X, rhs.X)
	local y = bit_xor(lhs.Y, rhs.Y)
	local z = bit_xor(lhs.Z, rhs.Z)

	return constructor(x, y, z)
end

function Numeric.shift_left(lhs, rhs)
	local count = rhs.X % 64

	if count == 0 then
		return lhs
	elseif count < 32 then
		local pad = 32 - count
		local lhs_1, lhs_2 = load_d1(lhs), load_d2(lhs)

		local data_1 = bit_lshift(lhs_1, count)
		local data_2 = bit_replace(bit_rshift(lhs_1, pad), lhs_2, count, pad)

		return from_u32(data_1, data_2)
	else
		local lhs_1 = load_d1(lhs)

		return from_u32(0, bit_lshift(lhs_1, count - 32))
	end
end

function Numeric.shift_right_unsigned(lhs, rhs)
	local count = rhs.X % 64

	if count == 0 then
		return lhs
	elseif count < 32 then
		local lhs_1, lhs_2 = load_d1(lhs), load_d2(lhs)

		local data_1 = bit_replace(bit_rshift(lhs_1, count), lhs_2, 32 - count, count)
		local data_2 = bit_rshift(lhs_2, count)

		return from_u32(data_1, data_2)
	else
		local lhs_2 = load_d2(lhs)

		return from_u32(bit_rshift(lhs_2, count - 32), 0)
	end
end

function Numeric.shift_right_signed(lhs, rhs)
	local count = rhs.X % 64

	if count == 0 then
		return lhs
	elseif count < 32 then
		local lhs_1, lhs_2 = load_d1(lhs), load_d2(lhs)

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
	if num_is_zero(rhs) then
		return lhs
	else
		local data_1 = num_shift_left(lhs, rhs)
		local data_2 = num_shift_right_unsigned(lhs, num_subtract(NUM_SIX_FOUR, rhs))

		return num_or(data_1, data_2)
	end
end

function Numeric.rotate_right(lhs, rhs)
	if num_is_zero(rhs) then
		return lhs
	else
		local data_1 = num_shift_right_unsigned(lhs, rhs)
		local data_2 = num_shift_left(lhs, num_subtract(NUM_SIX_FOUR, rhs))

		return num_or(data_1, data_2)
	end
end

function Numeric.is_negative(value)
	return value.Z >= 0x80000
end

function Numeric.is_zero(value)
	return value.X == 0 and value.Y == 0 and value.Z == 0
end

function Numeric.is_equal(lhs, rhs)
	return lhs.X == rhs.X and lhs.Y == rhs.Y and lhs.Z == rhs.Z
end

function Numeric.is_less_unsigned(lhs, rhs)
	local data_l_2 = load_d2(lhs)
	local data_r_2 = load_d2(rhs)

	return data_l_2 < data_r_2 or (data_l_2 == data_r_2 and load_d1(lhs) < load_d1(rhs))
end

function Numeric.is_greater_unsigned(lhs, rhs)
	local data_l_2 = load_d2(lhs)
	local data_r_2 = load_d2(rhs)

	return data_l_2 > data_r_2 or (data_l_2 == data_r_2 and load_d1(lhs) > load_d1(rhs))
end

function Numeric.is_less_signed(lhs, rhs)
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

num_is_negative = Numeric.is_negative
num_is_zero = Numeric.is_zero
num_is_less_unsigned = Numeric.is_less_unsigned

NUM_ZERO = from_u64(0)
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

local to_u32 = bit32.band

local bit_or = bit32.bor
local bit_and = bit32.band
local bit_lshift = bit32.lshift
local bit_rshift = bit32.rshift

local num_from_u32 = Integer.from_u32
local num_into_u32 = Integer.into_u32

local function to_i32(num)
	if num >= 0x80000000 then
		return num - 0x100000000
	else
		return num
	end
end

local function no_op(num)
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
	local math_round = math.round
	local math_sign = math.sign
	local math_min = math.min
	local math_max = math.max

	local string_byte = string.byte
    local string_char = string.char
	local string_pack = string.pack

	local num_divide_signed = Integer.divide_signed
	local num_divide_unsigned = Integer.divide_unsigned

	function add.i32(lhs, rhs)
		return to_u32(lhs + rhs)
	end

	function sub.i32(lhs, rhs)
		return to_u32(lhs - rhs)
	end

	function mul.i32(lhs, rhs)
		if (lhs + rhs) < 0x8000000 then
			return to_u32(lhs * rhs)
		else
			local a16 = bit_rshift(lhs, 16)
			local a00 = bit_and(lhs, 0xFFFF)
			local b16 = bit_rshift(rhs, 16)
			local b00 = bit_and(rhs, 0xFFFF)

			local c00 = a00 * b00
			local c16 = a16 * b00 + a00 * b16

			return to_u32(c00 + bit_lshift(c16, 16))
		end
	end

	function div.i32(lhs, rhs)
		assert(rhs ~= 0, "division by zero")

		lhs = to_i32(lhs)
		rhs = to_i32(rhs)

		return to_u32(lhs / rhs)
	end

	function div.u32(lhs, rhs)
		assert(rhs ~= 0, "division by zero")

		return to_u32(lhs / rhs)
	end

	function rem.i32(lhs, rhs)
		assert(rhs ~= 0, "division by zero")

		lhs = to_i32(lhs)
		rhs = to_i32(rhs)

		return to_u32(math_fmod(lhs, rhs))
	end

	add.i64 = Integer.add
	sub.i64 = Integer.subtract
	mul.i64 = Integer.multiply
	div.i64 = num_divide_signed

	function rem.i64(lhs, rhs)
		local _, remainder = num_divide_signed(lhs, rhs)

		return remainder
	end

	div.u64 = num_divide_unsigned

	function rem.u64(lhs, rhs)
		local _, remainder = num_divide_unsigned(lhs, rhs)

		return remainder
	end

	function neg.f32(num)
		return -num
	end

	function min.f32(lhs, rhs)
		if rhs == rhs then
			return math_min(lhs, rhs)
		else
			return rhs
		end
	end

	function max.f32(lhs, rhs)
		if rhs == rhs then
			return math_max(lhs, rhs)
		else
			return rhs
		end
	end

	function copysign.f32(lhs, rhs)
		local packed = string_pack("<d", rhs)
		local sign = string_byte(packed, 8)

		if sign >= 0x80 then
			return -math_abs(lhs)
		else
			return math_abs(lhs)
		end
	end

	function nearest.f32(num)
		local result = math_round(num)

		if (math_abs(num) + 0.5) % 2 == 1 then
			result = result - math_sign(result)
		end

		return result
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
		local count = 0

		while num ~= 0 do
			num = bit_and(num, num - 1)
			count = count + 1
		end

		return count
	end

	popcnt.i32 = popcnt_i32

	function clz.i64(num)
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
		return to_i32(lhs) <= to_i32(rhs)
	end

	function lt.i32(lhs, rhs)
		return to_i32(lhs) < to_i32(rhs)
	end

	function ge.i32(lhs, rhs)
		return to_i32(lhs) >= to_i32(rhs)
	end

	function gt.i32(lhs, rhs)
		return to_i32(lhs) > to_i32(rhs)
	end

	eq.i64 = num_is_equal

	function ne.i64(lhs, rhs)
		return not num_is_equal(lhs, rhs)
	end

	function le.i64(lhs, rhs)
		return num_is_less_signed(lhs, rhs) or num_is_equal(lhs, rhs)
	end

	function le.u64(lhs, rhs)
		return num_is_less_unsigned(lhs, rhs) or num_is_equal(lhs, rhs)
	end

	lt.i64 = num_is_less_signed
	lt.u64 = num_is_less_unsigned

	function ge.i64(lhs, rhs)
		return num_is_greater_signed(lhs, rhs) or num_is_equal(lhs, rhs)
	end

	function ge.u64(lhs, rhs)
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
		return bit_lshift(lhs, rhs % 32)
	end

	function shr.u32(lhs, rhs)
		return bit_rshift(lhs, rhs % 32)
	end

	function shr.i32(lhs, rhs)
		return bit_arshift(lhs, rhs % 32)
	end

	function rotl.i32(lhs, rhs)
		return bit_lrotate(lhs, rhs % 32)
	end

	function rotr.i32(lhs, rhs)
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

	local string_pack = string.pack
	local string_unpack = string.unpack

	local NUM_ZERO = Integer.ZERO
	local NUM_MIN_I64 = num_from_u32(0, 0x80000000)
	local NUM_MAX_I64 = num_from_u32(0xFFFFFFFF, 0x7FFFFFFF)
	local NUM_MAX_U64 = num_from_u32(0xFFFFFFFF, 0xFFFFFFFF)

	local num_from_u64 = Integer.from_u64
	local num_into_u64 = Integer.into_u64

	local num_negate = Integer.negate
	local num_is_negative = Integer.is_negative

	local function truncate_f64(num)
		if num >= 0 then
			return math_floor(num)
		else
			return math_ceil(num)
		end
	end

	function wrap.i32_i64(num)
		local data_1, _ = num_into_u32(num)

		return data_1
	end

	function truncate.i32_f32(num)
		return to_u32(truncate_f64(num))
	end

	truncate.i32_f64 = to_u32
	truncate.u32_f32 = truncate_f64
	truncate.u32_f64 = truncate_f64

	function truncate.i64_f32(num)
		if num < 0 then
			local temp = num_from_u64(-num)

			return num_negate(temp)
		else
			return num_from_u64(num)
		end
	end

	truncate.i64_f64 = truncate.i64_f32

	function truncate.u64_f32(num)
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
		local temp = math_clamp(truncate_f64(num), -0x80000000, 0x7FFFFFFF)

		return to_u32(temp)
	end

	saturate.i32_f64 = saturate.i32_f32

	function saturate.u32_f32(num)
		local temp = math_clamp(truncate_f64(num), 0, 0xFFFFFFFF)

		return to_u32(temp)
	end

	saturate.u32_f64 = saturate.u32_f32

	local truncate_i64_f64 = truncate.i64_f64

	function saturate.i64_f32(num)
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
		num = bit_and(num, 0xFF)

		if num >= 0x80 then
			return to_u32(num - 0x100)
		else
			return num
		end
	end

	function extend.i32_n16(num)
		num = bit_and(num, 0xFFFF)

		if num >= 0x8000 then
			return to_u32(num - 0x10000)
		else
			return num
		end
	end

	function extend.i64_n8(num)
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
		local data_1, _ = num_into_u32(num)

		if data_1 >= 0x80000000 then
			local temp = num_from_u32(-data_1 + 0x100000000, 0)

			return num_negate(temp)
		else
			return num_from_u32(data_1, 0)
		end
	end

	function extend.i64_i32(num)
		if num >= 0x80000000 then
			local temp = num_from_u32(-num + 0x100000000, 0)

			return num_negate(temp)
		else
			return num_from_u32(num, 0)
		end
	end

	function extend.i64_u32(num)
		return num_from_u32(num, 0)
	end

	convert.f32_i32 = to_i32
	convert.f32_u32 = no_op

	function convert.f32_i64(num)
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

	function reinterpret.i32_f32(num)
		local packed = string_pack("f", num)

		return string_unpack("I4", packed)
	end

	function reinterpret.i64_f64(num)
		local packed = string_pack("d", num)
		local data_1, data_2 = string_unpack("I4I4", packed)

		return num_from_u32(data_1, data_2)
	end

	function reinterpret.f32_i32(num)
		local packed = string_pack("I4", num)

		return string_unpack("f", packed)
	end

	function reinterpret.f64_i64(num)
		local data_1, data_2 = num_into_u32(num)
		local packed = string_pack("I4I4", data_1, data_2)

		return string_unpack("d", packed)
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

	local bit_extract = bit32.extract
	local bit_replace = bit32.replace

	local math_floor = math.floor

	local string_byte = string.byte
    local string_char = string.char
	local string_unpack = string.unpack

	local reinterpret_f32_i32 = module.reinterpret.f32_i32
	local reinterpret_f64_i64 = module.reinterpret.f64_i64
	local reinterpret_i32_f32 = module.reinterpret.i32_f32
	local reinterpret_i64_f64 = module.reinterpret.i64_f64

	local function load_byte(data, addr)
		local value = data[math_floor(addr / 4)] or 0

		return bit_extract(value, addr % 4 * 8, 8)
	end

	local function store_byte(data, addr, value)
		local adjust = math_floor(addr / 4)

		data[adjust] = bit_replace(data[adjust] or 0, value, addr % 4 * 8, 8)
	end

	function load.i32_i8(memory, addr)
		local temp = load_byte(memory.data, addr)

		if temp >= 0x80 then
			return to_u32(temp - 0x100)
		else
			return temp
		end
	end

	function load.i32_u8(memory, addr)
		return load_byte(memory.data, addr)
	end

	function load.i32_i16(memory, addr)
		local data = memory.data
		local temp

		if addr % 4 == 0 then
			temp = bit_and(data[addr / 4] or 0, 0xFFFF)
		else
			local b1 = load_byte(data, addr)
			local b2 = bit_lshift(load_byte(data, addr + 1), 8)

			temp = bit_or(b1, b2)
		end

		if temp >= 0x8000 then
			return to_u32(temp - 0x10000)
		else
			return temp
		end
	end

	function load.i32_u16(memory, addr)
		local data = memory.data

		if addr % 4 == 0 then
			return bit_and(data[addr / 4] or 0, 0xFFFF)
		else
			local b1 = load_byte(data, addr)
			local b2 = bit_lshift(load_byte(data, addr + 1), 8)

			return bit_or(b1, b2)
		end
	end

	function load.i32(memory, addr)
		local data = memory.data

		if addr % 4 == 0 then
			-- aligned read
			return data[addr / 4] or 0
		else
			-- unaligned read
			local b1 = load_byte(data, addr)
			local b2 = bit_lshift(load_byte(data, addr + 1), 8)
			local b3 = bit_lshift(load_byte(data, addr + 2), 16)
			local b4 = bit_lshift(load_byte(data, addr + 3), 24)

			return bit_or(b1, b2, b3, b4)
		end
	end

	function load.i64_i8(memory, addr)
		local data_1 = load_byte(memory.data, addr)
		local data_2

		if data_1 >= 0x80 then
			data_1 = to_u32(data_1 - 0x100)
			data_2 = 0xFFFFFFFF
		else
			data_2 = 0
		end

		return num_from_u32(data_1, data_2)
	end

	function load.i64_u8(memory, addr)
		local temp = load_byte(memory.data, addr)

		return num_from_u32(temp, 0)
	end

	function load.i64_i16(memory, addr)
		local data = memory.data
		local data_1, data_2

		if addr % 4 == 0 then
			data_1 = bit_and(data[addr / 4] or 0, 0xFFFF)
		else
			local b1 = load_byte(data, addr)
			local b2 = bit_lshift(load_byte(data, addr + 1), 8)

			data_1 = bit_or(b1, b2)
		end

		if data_1 >= 0x8000 then
			data_1 = to_u32(data_1 - 0x10000)
			data_2 = 0xFFFFFFFF
		else
			data_2 = 0
		end

		return num_from_u32(data_1, data_2)
	end

	function load.i64_u16(memory, addr)
		local data = memory.data
		local temp

		if addr % 4 == 0 then
			temp = bit_and(data[addr / 4] or 0, 0xFFFF)
		else
			local b1 = load_byte(data, addr)
			local b2 = bit_lshift(load_byte(data, addr + 1), 8)

			temp = bit_or(b1, b2)
		end

		return num_from_u32(temp, 0)
	end

	function load.i64_i32(memory, addr)
		local data = memory.data
		local data_1, data_2

		if addr % 4 == 0 then
			data_1 = data[addr / 4] or 0
		else
			local b1 = load_byte(data, addr)
			local b2 = bit_lshift(load_byte(data, addr + 1), 8)
			local b3 = bit_lshift(load_byte(data, addr + 2), 16)
			local b4 = bit_lshift(load_byte(data, addr + 3), 24)

			data_1 = bit_or(b1, b2, b3, b4)
		end

		if data_1 >= 0x80000000 then
			data_1 = to_u32(data_1 - 0x100000000)
			data_2 = 0xFFFFFFFF
		else
			data_2 = 0
		end

		return num_from_u32(data_1, data_2)
	end

	function load.i64_u32(memory, addr)
		local data = memory.data
		local temp

		if addr % 4 == 0 then
			temp = data[addr / 4] or 0
		else
			local b1 = load_byte(data, addr)
			local b2 = bit_lshift(load_byte(data, addr + 1), 8)
			local b3 = bit_lshift(load_byte(data, addr + 2), 16)
			local b4 = bit_lshift(load_byte(data, addr + 3), 24)

			temp = bit_or(b1, b2, b3, b4)
		end

		return num_from_u32(temp, 0)
	end

	local load_i32 = load.i32

	function load.i64(memory, addr)
		local data_1 = load_i32(memory, addr)
		local data_2 = load_i32(memory, addr + 4)

		return num_from_u32(data_1, data_2)
	end

	local load_i64 = load.i64

	function load.f32(memory, addr)
		local raw = load_i32(memory, addr)

		return reinterpret_f32_i32(raw)
	end

	function load.f64(memory, addr)
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
    end

	function store.i32_n8(memory, addr, value)
		store_byte(memory.data, addr, value)
	end

	local store_i8 = store.i32_n8

	function store.i32_n16(memory, addr, value)
		store_byte(memory.data, addr, value)
		store_byte(memory.data, addr + 1, bit_rshift(value, 8))
	end

	function store.i32(memory, addr, value)
		local data = memory.data

		if addr % 4 == 0 then
			-- aligned write
			data[addr / 4] = value
		else
			-- unaligned write
			store_byte(data, addr, value)
			store_byte(data, addr + 1, bit_rshift(value, 8))
			store_byte(data, addr + 2, bit_rshift(value, 16))
			store_byte(data, addr + 3, bit_rshift(value, 24))
		end
	end

	local store_i32 = store.i32
	local store_i32_n8 = store.i32_n8
	local store_i32_n16 = store.i32_n16

	function store.i64_n8(memory, addr, value)
		local data_1, _ = num_into_u32(value)

		store_i32_n8(memory, addr, data_1)
	end

	function store.i64_n16(memory, addr, value)
		local data_1, _ = num_into_u32(value)

		store_i32_n16(memory, addr, data_1)
	end

	function store.i64_n32(memory, addr, value)
		local data_1, _ = num_into_u32(value)

		store_i32(memory, addr, data_1)
	end

	function store.i64(memory, addr, value)
		local data_1, data_2 = num_into_u32(value)

		store_i32(memory, addr, data_1)
		store_i32(memory, addr + 4, data_2)
	end

	local store_i64 = store.i64

	function store.f32(memory, addr, value)
		store_i32(memory, addr, reinterpret_i32_f32(value))
	end

	function store.f64(memory, addr, value)
		store_i64(memory, addr, reinterpret_i64_f64(value))
	end

	function store.string(memory, addr, data, len)
		len = len or #data

		local rem = len % 4

		for i = 1, len - rem, 4 do
			local v = string_unpack("<I4", data, i)

			store_i32(memory, addr + i - 1, v)
		end

		for i = len - rem + 1, len do
			local v = string_byte(data, i)

			store_i8(memory, addr + i - 1, v)
		end
	end

	function allocator.new(min, max)
		return { min = min, max = max, data = {} }
	end

	function allocator.grow(memory, num)
		local old = memory.min
		local new = old + num

		if new > memory.max then
			return to_u32(-1)
		else
			memory.min = new

			return old
		end
	end

	module.load = load
	module.store = store
	module.allocator = allocator
end

return module

end)()
local add_i32 = rt.add.i32 local band_i32 = bit32.band local bor_i32 = bit32.bor local bor_i64 = rt.bor.i64 local bxor_i32 = bit32.bxor local clz_i32 = bit32.countlz local ctz_i32 = bit32.countrz local extend_i64_i32 = rt.extend.i64_i32 local extend_i64_u32 = rt.extend.i64_u32 local gt_i32 = rt.gt.i32 local i64_ZERO = rt.i64.ZERO local i64_from_u32 = rt.i64.from_u32 local load_i32 = rt.load.i32 local load_i32_u8 = rt.load.i32_u8 local load_i64 = rt.load.i64 local lt_i32 = rt.lt.i32 local rotl_i32 = rt.rotl.i32 local shl_i32 = rt.shl.i32 local shl_i64 = rt.shl.i64 local shr_u32 = rt.shr.u32 local shr_u64 = rt.shr.u64 local store_i32 = rt.store.i32 local store_i32_n8 = rt.store.i32_n8 local store_i64 = rt.store.i64 local sub_i32 = rt.sub.i32 local wrap_i32_i64 = rt.wrap.i32_i64 local memory_at_0 local FUNC_LIST = table.create(205)local TABLE_LIST = table.create(0)local MEMORY_LIST = table.create(0)local GLOBAL_LIST = table.create(3)FUNC_LIST[6] =--[[ __wasm_call_ctors ]]function()while true do FUNC_LIST[195]()break end end FUNC_LIST[7] =--[[ lua_call ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = 66926 reg_0 = loc_3 break end return reg_0 end FUNC_LIST[8] =--[[ RBX::print(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>) ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local loc_16 = 0  local loc_17 = 0  local loc_18 = 0  local loc_19 = 0  local loc_20 = 0  local loc_21 = 0  local loc_22 = 0  local loc_23 = 0  local loc_24 = 0  local loc_25 = 0  local loc_26 = 0  local loc_27 = 0  local loc_28 = 0  local loc_29 = 0  local loc_30 = 0  local loc_31 = 0  local loc_32 = 0  local loc_33 = 0  local loc_34 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 48 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 44, param_0 )loc_3 = 8 loc_4 = add_i32(loc_2 , loc_3 )loc_5 = loc_4 loc_6 = 66631 reg_0 = FUNC_LIST[9](loc_5 , loc_6 )loc_7 = 20 loc_8 = add_i32(loc_2 , loc_7 )loc_9 = loc_8 loc_10 = 8 loc_11 = add_i32(loc_2 , loc_10 )loc_12 = loc_11 FUNC_LIST[10](loc_9 , loc_12 , param_0 )loc_13 = 32 loc_14 = add_i32(loc_2 , loc_13 )loc_15 = loc_14 loc_16 = 20 loc_17 = add_i32(loc_2 , loc_16 )loc_18 = loc_17 loc_19 = 66590 FUNC_LIST[11](loc_15 , loc_18 , loc_19 )loc_20 = 32 loc_21 = add_i32(loc_2 , loc_20 )loc_22 = loc_21 reg_0 = FUNC_LIST[12](loc_22 )loc_23 = reg_0 reg_0 = FUNC_LIST[7](loc_23 )loc_24 = 32 loc_25 = add_i32(loc_2 , loc_24 )loc_26 = loc_25 reg_0 = FUNC_LIST[143](loc_26 )loc_27 = 20 loc_28 = add_i32(loc_2 , loc_27 )loc_29 = loc_28 reg_0 = FUNC_LIST[143](loc_29 )loc_30 = 8 loc_31 = add_i32(loc_2 , loc_30 )loc_32 = loc_31 reg_0 = FUNC_LIST[143](loc_32 )loc_33 = 48 loc_34 = add_i32(loc_2 , loc_33 )GLOBAL_LIST[0].value = loc_34 break end end FUNC_LIST[9] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::basic_string[abi:v160004]<std::nullptr_t>(char const*) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = 7 loc_5 = add_i32(loc_2 , loc_4 )loc_6 = loc_5 loc_7 = 6 loc_8 = add_i32(loc_2 , loc_7 )loc_9 = loc_8 reg_0 = FUNC_LIST[15](loc_3 , loc_6 , loc_9 )loc_10 = load_i32(memory_at_0, loc_2 + 8)loc_11 = load_i32(memory_at_0, loc_2 + 8)reg_0 = FUNC_LIST[16](loc_11 )loc_12 = reg_0 FUNC_LIST[146](loc_3 , loc_10 , loc_12 )FUNC_LIST[17](loc_3 )loc_13 = 16 loc_14 = add_i32(loc_2 , loc_13 )GLOBAL_LIST[0].value = loc_14 reg_0 = loc_3 break end return reg_0 end FUNC_LIST[10] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> std::__2::operator+[abi:v160004]<char, std::__2::char_traits<char>, std::__2::allocator<char>>(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>&&, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> const&) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )store_i32(memory_at_0, loc_2 + 4, param_2 )loc_3 = load_i32(memory_at_0, loc_2 + 8)loc_4 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[14](loc_3 , loc_4 )loc_5 = reg_0 reg_0 = FUNC_LIST[13](param_0 , loc_5 )loc_6 = 16 loc_7 = add_i32(loc_2 , loc_6 )GLOBAL_LIST[0].value = loc_7 break end end FUNC_LIST[11] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> std::__2::operator+[abi:v160004]<char, std::__2::char_traits<char>, std::__2::allocator<char>>(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>&&, char const*) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )store_i32(memory_at_0, loc_2 + 4, param_2 )loc_3 = load_i32(memory_at_0, loc_2 + 8)loc_4 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[154](loc_3 , loc_4 )loc_5 = reg_0 reg_0 = FUNC_LIST[13](param_0 , loc_5 )loc_6 = 16 loc_7 = add_i32(loc_2 , loc_6 )GLOBAL_LIST[0].value = loc_7 break end end FUNC_LIST[12] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::c_str[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[18](loc_3 )loc_4 = reg_0 loc_5 = 16 loc_6 = add_i32(loc_2 , loc_5 )GLOBAL_LIST[0].value = loc_6 reg_0 = loc_4 break end return reg_0 end FUNC_LIST[13] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::basic_string[abi:v160004](std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>&&) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = i64_ZERO  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local loc_16 = 0  local loc_17 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 8, param_0 )store_i32(memory_at_0, loc_2 + 4, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 8)store_i32(memory_at_0, loc_2 + 12, loc_3 )loc_4 = load_i32(memory_at_0, loc_2 + 4)loc_5 = load_i64(memory_at_0, loc_4 )store_i64(memory_at_0, loc_3 , loc_5 )loc_6 = 8 loc_7 = add_i32(loc_3 , loc_6 )loc_8 = add_i32(loc_4 , loc_6 )loc_9 = load_i32(memory_at_0, loc_8 )store_i32(memory_at_0, loc_7 , loc_9 )loc_10 = load_i32(memory_at_0, loc_2 + 4)FUNC_LIST[33](loc_10 )FUNC_LIST[17](loc_3 )reg_0 = FUNC_LIST[34](loc_3 )loc_11 = reg_0 loc_12 = 1 loc_13 = band_i32(loc_11 , loc_12 )while true do if loc_13 == 0 then break end loc_14 = load_i32(memory_at_0, loc_2 + 4)FUNC_LIST[35](loc_3 , loc_14 )break end if desired then if desired == 0 then desired = nil end break end loc_15 = load_i32(memory_at_0, loc_2 + 12)loc_16 = 16 loc_17 = add_i32(loc_2 , loc_16 )GLOBAL_LIST[0].value = loc_17 reg_0 = loc_15 break end return reg_0 end FUNC_LIST[14] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::append[abi:v160004](std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> const&) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = load_i32(memory_at_0, loc_2 + 8)reg_0 = FUNC_LIST[18](loc_4 )loc_5 = reg_0 loc_6 = load_i32(memory_at_0, loc_2 + 8)reg_0 = FUNC_LIST[40](loc_6 )loc_7 = reg_0 reg_0 = FUNC_LIST[151](loc_3 , loc_5 , loc_7 )loc_8 = reg_0 loc_9 = 16 loc_10 = add_i32(loc_2 , loc_9 )GLOBAL_LIST[0].value = loc_10 reg_0 = loc_8 break end return reg_0 end FUNC_LIST[15] =--[[ std::__2::__compressed_pair<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, std::__2::allocator<char>>::__compressed_pair[abi:v160004]<std::__2::__default_init_tag, std::__2::__default_init_tag>(std::__2::__default_init_tag&&, std::__2::__default_init_tag&&) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )store_i32(memory_at_0, loc_2 + 4, param_2 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[48](loc_3 )reg_0 = FUNC_LIST[49](loc_3 )loc_4 = 16 loc_5 = add_i32(loc_2 , loc_4 )GLOBAL_LIST[0].value = loc_5 reg_0 = loc_3 break end return reg_0 end FUNC_LIST[16] =--[[ std::__2::char_traits<char>::length(char const*) ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[50](loc_3 )loc_4 = reg_0 loc_5 = 16 loc_6 = add_i32(loc_2 , loc_5 )GLOBAL_LIST[0].value = loc_6 reg_0 = loc_4 break end return reg_0 end FUNC_LIST[17] =--[[ void std::__2::__debug_db_insert_c[abi:v160004]<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>>(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>*) ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )break end end FUNC_LIST[18] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::data[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[41](loc_3 )loc_4 = reg_0 reg_0 = FUNC_LIST[42](loc_4 )loc_5 = reg_0 loc_6 = 16 loc_7 = add_i32(loc_2 , loc_6 )GLOBAL_LIST[0].value = loc_7 reg_0 = loc_5 break end return reg_0 end FUNC_LIST[19] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> std::__2::operator+[abi:v160004]<char, std::__2::char_traits<char>, std::__2::allocator<char>>(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>&&, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>&&) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )store_i32(memory_at_0, loc_2 + 4, param_2 )loc_3 = load_i32(memory_at_0, loc_2 + 8)loc_4 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[14](loc_3 , loc_4 )loc_5 = reg_0 reg_0 = FUNC_LIST[13](param_0 , loc_5 )loc_6 = 16 loc_7 = add_i32(loc_2 , loc_6 )GLOBAL_LIST[0].value = loc_7 break end end FUNC_LIST[20] =--[[ __original_main ]]function()local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local loc_16 = 0  local loc_17 = 0  local loc_18 = 0  local loc_19 = 0  local loc_20 = 0  local loc_21 = 0  local loc_22 = 0  local loc_23 = 0  local loc_24 = 0  local loc_25 = 0  local loc_26 = 0  local loc_27 = 0  local loc_28 = 0  local loc_29 = 0  local loc_30 = 0  local loc_31 = 0  local loc_32 = 0  local loc_33 = 0  local loc_34 = 0  local loc_35 = 0  local loc_36 = 0  local loc_37 = 0  local loc_38 = 0  local loc_39 = 0  local loc_40 = 0  local loc_41 = 0  local loc_42 = 0  local loc_43 = 0  local loc_44 = 0  local loc_45 = 0  local loc_46 = 0  local loc_47 = 0  local loc_48 = 0  local loc_49 = 0  local loc_50 = 0  local loc_51 = 0  local loc_52 = 0  local loc_53 = 0  local loc_54 = 0  local loc_55 = 0  local loc_56 = 0  local loc_57 = 0  local loc_58 = 0  local loc_59 = 0  local loc_60 = 0  local loc_61 = 0  local loc_62 = 0  local loc_63 = 0  local loc_64 = 0  local loc_65 = 0  local loc_66 = 0  local loc_67 = 0  local loc_68 = 0  local loc_69 = 0  local loc_70 = 0  local loc_71 = 0  local loc_72 = 0  local loc_73 = 0  local loc_74 = 0  local loc_75 = 0  local loc_76 = 0  local loc_77 = 0  local loc_78 = 0  local loc_79 = 0  local loc_80 = 0  local loc_81 = 0  local loc_82 = 0  local loc_83 = 0  local loc_84 = 0  local loc_85 = 0  local loc_86 = 0  local loc_87 = 0  local loc_88 = 0  local loc_89 = 0  local loc_90 = 0  local loc_91 = 0  local loc_92 = 0  local loc_93 = 0  local loc_94 = 0  local loc_95 = 0  local loc_96 = 0  local loc_97 = 0  local loc_98 = 0  local loc_99 = 0  local loc_100 = 0  local loc_101 = 0  local loc_102 = 0  local loc_103 = 0  local loc_104 = 0  local loc_105 = 0  local loc_106 = 0  local loc_107 = 0  local loc_108 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 128 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 loc_3 = 12 reg_0 = FUNC_LIST[99](loc_3 )loc_4 = reg_0 loc_5 = 112 loc_6 = add_i32(loc_2 , loc_5 )loc_7 = loc_6 loc_8 = 65536 reg_0 = FUNC_LIST[9](loc_7 , loc_8 )loc_9 = 100 loc_10 = add_i32(loc_2 , loc_9 )loc_11 = loc_10 loc_12 = 65570 reg_0 = FUNC_LIST[9](loc_11 , loc_12 )loc_13 = 100 loc_14 = add_i32(loc_2 , loc_13 )loc_15 = loc_14 reg_0 = FUNC_LIST[21](loc_15 )loc_16 = reg_0 loc_17 = 112 loc_18 = add_i32(loc_2 , loc_17 )loc_19 = loc_18 loc_20 = 0 loc_21 = 1 loc_22 = band_i32(loc_20 , loc_21 )reg_0 = FUNC_LIST[22](loc_4 , loc_19 , loc_16 , loc_22 )loc_23 = 100 loc_24 = add_i32(loc_2 , loc_23 )loc_25 = loc_24 reg_0 = FUNC_LIST[143](loc_25 )loc_26 = 112 loc_27 = add_i32(loc_2 , loc_26 )loc_28 = loc_27 reg_0 = FUNC_LIST[143](loc_28 )store_i32(memory_at_0, loc_2 + 124, loc_4 )loc_29 = load_i32(memory_at_0, loc_2 + 124)loc_30 = 88 loc_31 = add_i32(loc_2 , loc_30 )loc_32 = loc_31 loc_33 = 65565 reg_0 = FUNC_LIST[9](loc_32 , loc_33 )loc_34 = 76 loc_35 = add_i32(loc_2 , loc_34 )loc_36 = loc_35 loc_37 = 66593 reg_0 = FUNC_LIST[9](loc_36 , loc_37 )loc_38 = 88 loc_39 = add_i32(loc_2 , loc_38 )loc_40 = loc_39 loc_41 = 76 loc_42 = add_i32(loc_2 , loc_41 )loc_43 = loc_42 FUNC_LIST[23](loc_29 , loc_40 , loc_43 )loc_44 = 76 loc_45 = add_i32(loc_2 , loc_44 )loc_46 = loc_45 reg_0 = FUNC_LIST[143](loc_46 )loc_47 = 88 loc_48 = add_i32(loc_2 , loc_47 )loc_49 = loc_48 reg_0 = FUNC_LIST[143](loc_49 )loc_50 = load_i32(memory_at_0, loc_2 + 124)reg_0 = FUNC_LIST[24](loc_50 )loc_51 = reg_0 store_i32(memory_at_0, loc_2 + 72, loc_51 )loc_52 = load_i32(memory_at_0, loc_2 + 72)loc_53 = 60 loc_54 = add_i32(loc_2 , loc_53 )loc_55 = loc_54 loc_56 = 65541 reg_0 = FUNC_LIST[9](loc_55 , loc_56 )loc_57 = 48 loc_58 = add_i32(loc_2 , loc_57 )loc_59 = loc_58 loc_60 = 65570 reg_0 = FUNC_LIST[9](loc_59 , loc_60 )loc_61 = 60 loc_62 = add_i32(loc_2 , loc_61 )loc_63 = loc_62 loc_64 = 48 loc_65 = add_i32(loc_2 , loc_64 )loc_66 = loc_65 FUNC_LIST[23](loc_52 , loc_63 , loc_66 )loc_67 = 48 loc_68 = add_i32(loc_2 , loc_67 )loc_69 = loc_68 reg_0 = FUNC_LIST[143](loc_69 )loc_70 = 60 loc_71 = add_i32(loc_2 , loc_70 )loc_72 = loc_71 reg_0 = FUNC_LIST[143](loc_72 )loc_73 = 24 loc_74 = add_i32(loc_2 , loc_73 )loc_75 = loc_74 loc_76 = 66905 reg_0 = FUNC_LIST[9](loc_75 , loc_76 )loc_77 = load_i32(memory_at_0, loc_2 + 124)loc_78 = loc_2 loc_79 = 65565 reg_0 = FUNC_LIST[9](loc_78 , loc_79 )loc_80 = 12 loc_81 = add_i32(loc_2 , loc_80 )loc_82 = loc_81 loc_83 = loc_2 FUNC_LIST[25](loc_82 , loc_77 , loc_83 )loc_84 = 36 loc_85 = add_i32(loc_2 , loc_84 )loc_86 = loc_85 loc_87 = 24 loc_88 = add_i32(loc_2 , loc_87 )loc_89 = loc_88 loc_90 = 12 loc_91 = add_i32(loc_2 , loc_90 )loc_92 = loc_91 FUNC_LIST[19](loc_86 , loc_89 , loc_92 )loc_93 = 36 loc_94 = add_i32(loc_2 , loc_93 )loc_95 = loc_94 FUNC_LIST[8](loc_95 )loc_96 = 36 loc_97 = add_i32(loc_2 , loc_96 )loc_98 = loc_97 reg_0 = FUNC_LIST[143](loc_98 )loc_99 = 12 loc_100 = add_i32(loc_2 , loc_99 )loc_101 = loc_100 reg_0 = FUNC_LIST[143](loc_101 )loc_102 = loc_2 reg_0 = FUNC_LIST[143](loc_102 )loc_103 = 24 loc_104 = add_i32(loc_2 , loc_103 )loc_105 = loc_104 reg_0 = FUNC_LIST[143](loc_105 )loc_106 = 0 loc_107 = 128 loc_108 = add_i32(loc_2 , loc_107 )GLOBAL_LIST[0].value = loc_108 reg_0 = loc_106 break end return reg_0 end FUNC_LIST[21] =--[[ RBX::Instance::GetInstance(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>) ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local loc_16 = 0  local loc_17 = 0  local loc_18 = 0  local loc_19 = 0  local loc_20 = 0  local loc_21 = 0  local loc_22 = 0  local loc_23 = 0  local loc_24 = 0  local loc_25 = 0  local loc_26 = 0  local loc_27 = 0  local loc_28 = 0  local loc_29 = 0  local loc_30 = 0  local loc_31 = 0  local loc_32 = 0  local loc_33 = 0  local loc_34 = 0  local loc_35 = 0  local loc_36 = 0  local loc_37 = 0  local loc_38 = 0  local loc_39 = 0  local loc_40 = 0  local loc_41 = 0  local loc_42 = 0  local loc_43 = 0  local loc_44 = 0  local loc_45 = 0  local loc_46 = 0  local loc_47 = 0  local loc_48 = 0  local loc_49 = 0  local loc_50 = 0  local loc_51 = 0  local loc_52 = 0  local loc_53 = 0  local loc_54 = 0  local loc_55 = 0  local loc_56 = 0  local loc_57 = 0  local loc_58 = 0  local loc_59 = 0  local loc_60 = 0  local loc_61 = 0  local loc_62 = 0  local loc_63 = 0  local loc_64 = 0  local loc_65 = 0  local loc_66 = 0  local loc_67 = 0  local loc_68 = 0  local loc_69 = 0  local loc_70 = 0  local loc_71 = 0  local loc_72 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 80 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 72, param_0 )loc_3 = 24 loc_4 = add_i32(loc_2 , loc_3 )loc_5 = loc_4 loc_6 = 66891 reg_0 = FUNC_LIST[9](loc_5 , loc_6 )loc_7 = 36 loc_8 = add_i32(loc_2 , loc_7 )loc_9 = loc_8 loc_10 = 24 loc_11 = add_i32(loc_2 , loc_10 )loc_12 = loc_11 FUNC_LIST[10](loc_9 , loc_12 , param_0 )loc_13 = 48 loc_14 = add_i32(loc_2 , loc_13 )loc_15 = loc_14 loc_16 = 36 loc_17 = add_i32(loc_2 , loc_16 )loc_18 = loc_17 loc_19 = 65655 FUNC_LIST[11](loc_15 , loc_18 , loc_19 )loc_20 = 48 loc_21 = add_i32(loc_2 , loc_20 )loc_22 = loc_21 reg_0 = FUNC_LIST[12](loc_22 )loc_23 = reg_0 reg_0 = FUNC_LIST[7](loc_23 )loc_24 = reg_0 loc_25 = 60 loc_26 = add_i32(loc_2 , loc_25 )loc_27 = loc_26 reg_0 = FUNC_LIST[9](loc_27 , loc_24 )loc_28 = 48 loc_29 = add_i32(loc_2 , loc_28 )loc_30 = loc_29 reg_0 = FUNC_LIST[143](loc_30 )loc_31 = 36 loc_32 = add_i32(loc_2 , loc_31 )loc_33 = loc_32 reg_0 = FUNC_LIST[143](loc_33 )loc_34 = 24 loc_35 = add_i32(loc_2 , loc_34 )loc_36 = loc_35 reg_0 = FUNC_LIST[143](loc_36 )loc_37 = 60 loc_38 = add_i32(loc_2 , loc_37 )loc_39 = loc_38 loc_40 = 66926 reg_0 = FUNC_LIST[26](loc_39 , loc_40 )loc_41 = reg_0 loc_42 = 1 loc_43 = band_i32(loc_41 , loc_42 )while true do while true do while true do if loc_43 ~= 0 then break end loc_44 = 60 loc_45 = add_i32(loc_2 , loc_44 )loc_46 = loc_45 loc_47 = 65548 reg_0 = FUNC_LIST[26](loc_46 , loc_47 )loc_48 = reg_0 loc_49 = 1 loc_50 = band_i32(loc_48 , loc_49 )if loc_50 == 0 then desired = 2 break end break end if desired then if desired == 2 then desired = nil end break end loc_51 = 0 store_i32(memory_at_0, loc_2 + 76, loc_51 )loc_52 = 1 store_i32(memory_at_0, loc_2 + 20, loc_52 )desired = 1 break end if desired then if desired == 1 then desired = nil end break end loc_53 = 8 loc_54 = add_i32(loc_2 , loc_53 )loc_55 = loc_54 loc_56 = 60 loc_57 = add_i32(loc_2 , loc_56 )loc_58 = loc_57 reg_0 = FUNC_LIST[27](loc_55 , loc_58 )loc_59 = 8 loc_60 = add_i32(loc_2 , loc_59 )loc_61 = loc_60 reg_0 = FUNC_LIST[28](loc_61 )loc_62 = reg_0 store_i32(memory_at_0, loc_2 + 76, loc_62 )loc_63 = 8 loc_64 = add_i32(loc_2 , loc_63 )loc_65 = loc_64 reg_0 = FUNC_LIST[143](loc_65 )loc_66 = 1 store_i32(memory_at_0, loc_2 + 20, loc_66 )break end if desired then if desired == 0 then desired = nil end break end loc_67 = 60 loc_68 = add_i32(loc_2 , loc_67 )loc_69 = loc_68 reg_0 = FUNC_LIST[143](loc_69 )loc_70 = load_i32(memory_at_0, loc_2 + 76)loc_71 = 80 loc_72 = add_i32(loc_2 , loc_71 )GLOBAL_LIST[0].value = loc_72 reg_0 = loc_70 break end return reg_0 end FUNC_LIST[22] =--[[ RBX::Instance::Instance(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, RBX::Instance*, bool) ]]function(param_0, param_1, param_2, param_3)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local loc_16 = 0  local loc_17 = 0  local loc_18 = 0  local loc_19 = 0  local loc_20 = 0  local loc_21 = 0  local loc_22 = 0  local loc_23 = 0  local loc_24 = 0  local loc_25 = 0  local loc_26 = 0  local loc_27 = 0  local loc_28 = 0  local loc_29 = 0  local loc_30 = 0  local loc_31 = 0  local loc_32 = 0  local loc_33 = 0  local loc_34 = 0  local loc_35 = 0  local loc_36 = 0  local loc_37 = 0  local loc_38 = 0  local loc_39 = 0  local loc_40 = 0  local loc_41 = 0  local loc_42 = 0  local loc_43 = 0  local loc_44 = 0  local loc_45 = 0  local loc_46 = 0  local loc_47 = 0  local loc_48 = 0  local loc_49 = 0  local loc_50 = 0  local loc_51 = 0  local loc_52 = 0  local loc_53 = 0  local loc_54 = 0  local loc_55 = 0  local loc_56 = 0  local loc_57 = 0  local loc_58 = 0  local loc_59 = 0  local loc_60 = 0  local loc_61 = 0  local loc_62 = 0  local loc_63 = 0  local loc_64 = 0  local loc_65 = 0  local loc_66 = 0  local loc_67 = 0  local loc_68 = 0  local loc_69 = 0  local loc_70 = 0  local loc_71 = 0  local loc_72 = 0  local loc_73 = 0  local loc_74 = 0  local loc_75 = 0  local loc_76 = 0  local loc_77 = 0  local loc_78 = 0  local loc_79 = 0  local loc_80 = 0  local loc_81 = 0  local loc_82 = 0  local loc_83 = 0  local loc_84 = 0  local loc_85 = 0  local loc_86 = 0  local loc_87 = 0  local loc_88 = 0  local loc_89 = 0  local loc_90 = 0  local loc_91 = 0  local loc_92 = 0  local loc_93 = 0  local loc_94 = 0  local loc_95 = 0  local loc_96 = 0  local loc_97 = 0  local loc_98 = 0  local loc_99 = 0  local loc_100 = 0  local loc_101 = 0  local loc_102 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 128 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 120, param_0 )store_i32(memory_at_0, loc_2 + 116, param_1 )store_i32(memory_at_0, loc_2 + 112, param_2 )loc_3 = param_3 store_i32_n8(memory_at_0, loc_2 + 111, loc_3 )loc_4 = load_i32(memory_at_0, loc_2 + 120)store_i32(memory_at_0, loc_2 + 124, loc_4 )reg_0 = FUNC_LIST[29](loc_4 )loc_5 = load_i32_u8(memory_at_0, loc_2 + 111)loc_6 = 1 loc_7 = band_i32(loc_5 , loc_6 )while true do while true do if loc_7 ~= 0 then break end loc_8 = load_i32(memory_at_0, loc_2 + 112)loc_9 = 0 loc_10 = loc_8 loc_11 = loc_9 loc_12 = (loc_10 ~= loc_11 and 1 or 0)loc_13 = 1 loc_14 = band_i32(loc_12 , loc_13 )while true do while true do if loc_14 == 0 then break end loc_15 = 48 loc_16 = add_i32(loc_2 , loc_15 )loc_17 = loc_16 loc_18 = 66603 reg_0 = FUNC_LIST[9](loc_17 , loc_18 )loc_19 = 60 loc_20 = add_i32(loc_2 , loc_19 )loc_21 = loc_20 loc_22 = 48 loc_23 = add_i32(loc_2 , loc_22 )loc_24 = loc_23 FUNC_LIST[10](loc_21 , loc_24 , param_1 )loc_25 = 72 loc_26 = add_i32(loc_2 , loc_25 )loc_27 = loc_26 loc_28 = 60 loc_29 = add_i32(loc_2 , loc_28 )loc_30 = loc_29 loc_31 = 66715 FUNC_LIST[11](loc_27 , loc_30 , loc_31 )loc_32 = load_i32(memory_at_0, loc_2 + 112)loc_33 = 36 loc_34 = add_i32(loc_2 , loc_33 )loc_35 = loc_34 FUNC_LIST[30](loc_35 , loc_32 )loc_36 = 84 loc_37 = add_i32(loc_2 , loc_36 )loc_38 = loc_37 loc_39 = 72 loc_40 = add_i32(loc_2 , loc_39 )loc_41 = loc_40 loc_42 = 36 loc_43 = add_i32(loc_2 , loc_42 )loc_44 = loc_43 FUNC_LIST[19](loc_38 , loc_41 , loc_44 )loc_45 = 96 loc_46 = add_i32(loc_2 , loc_45 )loc_47 = loc_46 loc_48 = 84 loc_49 = add_i32(loc_2 , loc_48 )loc_50 = loc_49 loc_51 = 66367 FUNC_LIST[11](loc_47 , loc_50 , loc_51 )loc_52 = 96 loc_53 = add_i32(loc_2 , loc_52 )loc_54 = loc_53 reg_0 = FUNC_LIST[12](loc_54 )loc_55 = reg_0 reg_0 = FUNC_LIST[7](loc_55 )loc_56 = reg_0 reg_0 = FUNC_LIST[31](loc_4 , loc_56 )loc_57 = 96 loc_58 = add_i32(loc_2 , loc_57 )loc_59 = loc_58 reg_0 = FUNC_LIST[143](loc_59 )loc_60 = 84 loc_61 = add_i32(loc_2 , loc_60 )loc_62 = loc_61 reg_0 = FUNC_LIST[143](loc_62 )loc_63 = 36 loc_64 = add_i32(loc_2 , loc_63 )loc_65 = loc_64 reg_0 = FUNC_LIST[143](loc_65 )loc_66 = 72 loc_67 = add_i32(loc_2 , loc_66 )loc_68 = loc_67 reg_0 = FUNC_LIST[143](loc_68 )loc_69 = 60 loc_70 = add_i32(loc_2 , loc_69 )loc_71 = loc_70 reg_0 = FUNC_LIST[143](loc_71 )loc_72 = 48 loc_73 = add_i32(loc_2 , loc_72 )loc_74 = loc_73 reg_0 = FUNC_LIST[143](loc_74 )desired = 3 break end if desired then if desired == 3 then desired = nil end break end loc_75 = loc_2 loc_76 = 66603 reg_0 = FUNC_LIST[9](loc_75 , loc_76 )loc_77 = 12 loc_78 = add_i32(loc_2 , loc_77 )loc_79 = loc_78 loc_80 = loc_2 FUNC_LIST[10](loc_79 , loc_80 , param_1 )loc_81 = 24 loc_82 = add_i32(loc_2 , loc_81 )loc_83 = loc_82 loc_84 = 12 loc_85 = add_i32(loc_2 , loc_84 )loc_86 = loc_85 loc_87 = 66384 FUNC_LIST[11](loc_83 , loc_86 , loc_87 )loc_88 = 24 loc_89 = add_i32(loc_2 , loc_88 )loc_90 = loc_89 reg_0 = FUNC_LIST[12](loc_90 )loc_91 = reg_0 reg_0 = FUNC_LIST[7](loc_91 )loc_92 = reg_0 reg_0 = FUNC_LIST[31](loc_4 , loc_92 )loc_93 = 24 loc_94 = add_i32(loc_2 , loc_93 )loc_95 = loc_94 reg_0 = FUNC_LIST[143](loc_95 )loc_96 = 12 loc_97 = add_i32(loc_2 , loc_96 )loc_98 = loc_97 reg_0 = FUNC_LIST[143](loc_98 )loc_99 = loc_2 reg_0 = FUNC_LIST[143](loc_99 )break end if desired then if desired == 2 then desired = nil end break end desired = 1 break end if desired then if desired == 1 then desired = nil end break end reg_0 = FUNC_LIST[32](loc_4 , param_1 )break end if desired then if desired == 0 then desired = nil end break end loc_100 = load_i32(memory_at_0, loc_2 + 124)loc_101 = 128 loc_102 = add_i32(loc_2 , loc_101 )GLOBAL_LIST[0].value = loc_102 reg_0 = loc_100 break end return reg_0 end FUNC_LIST[23] =--[[ RBX::Instance::SetPropertyRaw(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local loc_16 = 0  local loc_17 = 0  local loc_18 = 0  local loc_19 = 0  local loc_20 = 0  local loc_21 = 0  local loc_22 = 0  local loc_23 = 0  local loc_24 = 0  local loc_25 = 0  local loc_26 = 0  local loc_27 = 0  local loc_28 = 0  local loc_29 = 0  local loc_30 = 0  local loc_31 = 0  local loc_32 = 0  local loc_33 = 0  local loc_34 = 0  local loc_35 = 0  local loc_36 = 0  local loc_37 = 0  local loc_38 = 0  local loc_39 = 0  local loc_40 = 0  local loc_41 = 0  local loc_42 = 0  local loc_43 = 0  local loc_44 = 0  local loc_45 = 0  local loc_46 = 0  local loc_47 = 0  local loc_48 = 0  local loc_49 = 0  local loc_50 = 0  local loc_51 = 0  local loc_52 = 0  local loc_53 = 0  local loc_54 = 0  local loc_55 = 0  local loc_56 = 0  local loc_57 = 0  local loc_58 = 0  local loc_59 = 0  local loc_60 = 0  local loc_61 = 0  local loc_62 = 0  local loc_63 = 0  local loc_64 = 0  local loc_65 = 0  local loc_66 = 0  local loc_67 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 96 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 92, param_0 )store_i32(memory_at_0, loc_2 + 88, param_1 )store_i32(memory_at_0, loc_2 + 84, param_2 )loc_3 = load_i32(memory_at_0, loc_2 + 92)loc_4 = loc_2 loc_5 = 66639 reg_0 = FUNC_LIST[9](loc_4 , loc_5 )loc_6 = 12 loc_7 = add_i32(loc_2 , loc_6 )loc_8 = loc_7 loc_9 = loc_2 FUNC_LIST[10](loc_8 , loc_9 , loc_3 )loc_10 = 24 loc_11 = add_i32(loc_2 , loc_10 )loc_12 = loc_11 loc_13 = 12 loc_14 = add_i32(loc_2 , loc_13 )loc_15 = loc_14 loc_16 = 66528 FUNC_LIST[11](loc_12 , loc_15 , loc_16 )loc_17 = 36 loc_18 = add_i32(loc_2 , loc_17 )loc_19 = loc_18 loc_20 = 24 loc_21 = add_i32(loc_2 , loc_20 )loc_22 = loc_21 FUNC_LIST[10](loc_19 , loc_22 , param_1 )loc_23 = 48 loc_24 = add_i32(loc_2 , loc_23 )loc_25 = loc_24 loc_26 = 36 loc_27 = add_i32(loc_2 , loc_26 )loc_28 = loc_27 loc_29 = 66901 FUNC_LIST[11](loc_25 , loc_28 , loc_29 )loc_30 = 60 loc_31 = add_i32(loc_2 , loc_30 )loc_32 = loc_31 loc_33 = 48 loc_34 = add_i32(loc_2 , loc_33 )loc_35 = loc_34 FUNC_LIST[10](loc_32 , loc_35 , param_2 )loc_36 = 72 loc_37 = add_i32(loc_2 , loc_36 )loc_38 = loc_37 loc_39 = 60 loc_40 = add_i32(loc_2 , loc_39 )loc_41 = loc_40 loc_42 = 66362 FUNC_LIST[11](loc_38 , loc_41 , loc_42 )loc_43 = 72 loc_44 = add_i32(loc_2 , loc_43 )loc_45 = loc_44 reg_0 = FUNC_LIST[12](loc_45 )loc_46 = reg_0 reg_0 = FUNC_LIST[7](loc_46 )loc_47 = 72 loc_48 = add_i32(loc_2 , loc_47 )loc_49 = loc_48 reg_0 = FUNC_LIST[143](loc_49 )loc_50 = 60 loc_51 = add_i32(loc_2 , loc_50 )loc_52 = loc_51 reg_0 = FUNC_LIST[143](loc_52 )loc_53 = 48 loc_54 = add_i32(loc_2 , loc_53 )loc_55 = loc_54 reg_0 = FUNC_LIST[143](loc_55 )loc_56 = 36 loc_57 = add_i32(loc_2 , loc_56 )loc_58 = loc_57 reg_0 = FUNC_LIST[143](loc_58 )loc_59 = 24 loc_60 = add_i32(loc_2 , loc_59 )loc_61 = loc_60 reg_0 = FUNC_LIST[143](loc_61 )loc_62 = 12 loc_63 = add_i32(loc_2 , loc_62 )loc_64 = loc_63 reg_0 = FUNC_LIST[143](loc_64 )loc_65 = loc_2 reg_0 = FUNC_LIST[143](loc_65 )loc_66 = 96 loc_67 = add_i32(loc_2 , loc_66 )GLOBAL_LIST[0].value = loc_67 break end end FUNC_LIST[24] =--[[ RBX::Instance::Clone() ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local loc_16 = 0  local loc_17 = 0  local loc_18 = 0  local loc_19 = 0  local loc_20 = 0  local loc_21 = 0  local loc_22 = 0  local loc_23 = 0  local loc_24 = 0  local loc_25 = 0  local loc_26 = 0  local loc_27 = 0  local loc_28 = 0  local loc_29 = 0  local loc_30 = 0  local loc_31 = 0  local loc_32 = 0  local loc_33 = 0  local loc_34 = 0  local loc_35 = 0  local loc_36 = 0  local loc_37 = 0  local loc_38 = 0  local loc_39 = 0  local loc_40 = 0  local loc_41 = 0  local loc_42 = 0  local loc_43 = 0  local loc_44 = 0  local loc_45 = 0  local loc_46 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 64 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 60, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 60)loc_4 = 12 loc_5 = add_i32(loc_2 , loc_4 )loc_6 = loc_5 loc_7 = 66639 reg_0 = FUNC_LIST[9](loc_6 , loc_7 )loc_8 = 24 loc_9 = add_i32(loc_2 , loc_8 )loc_10 = loc_9 loc_11 = 12 loc_12 = add_i32(loc_2 , loc_11 )loc_13 = loc_12 FUNC_LIST[10](loc_10 , loc_13 , loc_3 )loc_14 = 36 loc_15 = add_i32(loc_2 , loc_14 )loc_16 = loc_15 loc_17 = 24 loc_18 = add_i32(loc_2 , loc_17 )loc_19 = loc_18 loc_20 = 65975 FUNC_LIST[11](loc_16 , loc_19 , loc_20 )loc_21 = 36 loc_22 = add_i32(loc_2 , loc_21 )loc_23 = loc_22 reg_0 = FUNC_LIST[12](loc_23 )loc_24 = reg_0 reg_0 = FUNC_LIST[7](loc_24 )loc_25 = reg_0 loc_26 = 48 loc_27 = add_i32(loc_2 , loc_26 )loc_28 = loc_27 reg_0 = FUNC_LIST[9](loc_28 , loc_25 )loc_29 = 48 loc_30 = add_i32(loc_2 , loc_29 )loc_31 = loc_30 reg_0 = FUNC_LIST[28](loc_31 )loc_32 = reg_0 loc_33 = 48 loc_34 = add_i32(loc_2 , loc_33 )loc_35 = loc_34 reg_0 = FUNC_LIST[143](loc_35 )loc_36 = 36 loc_37 = add_i32(loc_2 , loc_36 )loc_38 = loc_37 reg_0 = FUNC_LIST[143](loc_38 )loc_39 = 24 loc_40 = add_i32(loc_2 , loc_39 )loc_41 = loc_40 reg_0 = FUNC_LIST[143](loc_41 )loc_42 = 12 loc_43 = add_i32(loc_2 , loc_42 )loc_44 = loc_43 reg_0 = FUNC_LIST[143](loc_44 )loc_45 = 64 loc_46 = add_i32(loc_2 , loc_45 )GLOBAL_LIST[0].value = loc_46 reg_0 = loc_32 break end return reg_0 end FUNC_LIST[25] =--[[ RBX::Instance::GetPropertyRaw(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local loc_16 = 0  local loc_17 = 0  local loc_18 = 0  local loc_19 = 0  local loc_20 = 0  local loc_21 = 0  local loc_22 = 0  local loc_23 = 0  local loc_24 = 0  local loc_25 = 0  local loc_26 = 0  local loc_27 = 0  local loc_28 = 0  local loc_29 = 0  local loc_30 = 0  local loc_31 = 0  local loc_32 = 0  local loc_33 = 0  local loc_34 = 0  local loc_35 = 0  local loc_36 = 0  local loc_37 = 0  local loc_38 = 0  local loc_39 = 0  local loc_40 = 0  local loc_41 = 0  local loc_42 = 0  local loc_43 = 0  local loc_44 = 0  local loc_45 = 0  local loc_46 = 0  local loc_47 = 0  local loc_48 = 0  local loc_49 = 0  local loc_50 = 0  local loc_51 = 0  local loc_52 = 0  local loc_53 = 0  local loc_54 = 0  local loc_55 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 80 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 76, param_0 )store_i32(memory_at_0, loc_2 + 72, param_1 )store_i32(memory_at_0, loc_2 + 68, param_2 )loc_3 = load_i32(memory_at_0, loc_2 + 72)loc_4 = 8 loc_5 = add_i32(loc_2 , loc_4 )loc_6 = loc_5 loc_7 = 66639 reg_0 = FUNC_LIST[9](loc_6 , loc_7 )loc_8 = 20 loc_9 = add_i32(loc_2 , loc_8 )loc_10 = loc_9 loc_11 = 8 loc_12 = add_i32(loc_2 , loc_11 )loc_13 = loc_12 FUNC_LIST[10](loc_10 , loc_13 , loc_3 )loc_14 = 32 loc_15 = add_i32(loc_2 , loc_14 )loc_16 = loc_15 loc_17 = 20 loc_18 = add_i32(loc_2 , loc_17 )loc_19 = loc_18 loc_20 = 66553 FUNC_LIST[11](loc_16 , loc_19 , loc_20 )loc_21 = 44 loc_22 = add_i32(loc_2 , loc_21 )loc_23 = loc_22 loc_24 = 32 loc_25 = add_i32(loc_2 , loc_24 )loc_26 = loc_25 FUNC_LIST[10](loc_23 , loc_26 , param_2 )loc_27 = 56 loc_28 = add_i32(loc_2 , loc_27 )loc_29 = loc_28 loc_30 = 44 loc_31 = add_i32(loc_2 , loc_30 )loc_32 = loc_31 loc_33 = 65585 FUNC_LIST[11](loc_29 , loc_32 , loc_33 )loc_34 = 56 loc_35 = add_i32(loc_2 , loc_34 )loc_36 = loc_35 reg_0 = FUNC_LIST[12](loc_36 )loc_37 = reg_0 reg_0 = FUNC_LIST[7](loc_37 )loc_38 = reg_0 reg_0 = FUNC_LIST[9](param_0 , loc_38 )loc_39 = 56 loc_40 = add_i32(loc_2 , loc_39 )loc_41 = loc_40 reg_0 = FUNC_LIST[143](loc_41 )loc_42 = 44 loc_43 = add_i32(loc_2 , loc_42 )loc_44 = loc_43 reg_0 = FUNC_LIST[143](loc_44 )loc_45 = 32 loc_46 = add_i32(loc_2 , loc_45 )loc_47 = loc_46 reg_0 = FUNC_LIST[143](loc_47 )loc_48 = 20 loc_49 = add_i32(loc_2 , loc_48 )loc_50 = loc_49 reg_0 = FUNC_LIST[143](loc_50 )loc_51 = 8 loc_52 = add_i32(loc_2 , loc_51 )loc_53 = loc_52 reg_0 = FUNC_LIST[143](loc_53 )loc_54 = 80 loc_55 = add_i32(loc_2 , loc_54 )GLOBAL_LIST[0].value = loc_55 break end end FUNC_LIST[26] =--[[ bool std::__2::operator==[abi:v160004]<char, std::__2::char_traits<char>, std::__2::allocator<char>>(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> const&, char const*) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = i64_ZERO  local loc_12 = i64_ZERO  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local loc_16 = 0  local loc_17 = 0  local loc_18 = 0  local loc_19 = 0  local loc_20 = 0  local loc_21 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 48 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 44, param_0 )store_i32(memory_at_0, loc_2 + 40, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 44)loc_4 = 32 loc_5 = add_i32(loc_2 , loc_4 )loc_6 = loc_5 FUNC_LIST[53](loc_6 , loc_3 )loc_7 = load_i32(memory_at_0, loc_2 + 40)loc_8 = 24 loc_9 = add_i32(loc_2 , loc_8 )loc_10 = loc_9 reg_0 = FUNC_LIST[54](loc_10 , loc_7 )loc_11 = load_i64(memory_at_0, loc_2 + 32)store_i64(memory_at_0, loc_2 + 16, loc_11 )loc_12 = load_i64(memory_at_0, loc_2 + 24)store_i64(memory_at_0, loc_2 + 8, loc_12 )loc_13 = 16 loc_14 = add_i32(loc_2 , loc_13 )loc_15 = 8 loc_16 = add_i32(loc_2 , loc_15 )reg_0 = FUNC_LIST[55](loc_14 , loc_16 )loc_17 = reg_0 loc_18 = 1 loc_19 = band_i32(loc_17 , loc_18 )loc_20 = 48 loc_21 = add_i32(loc_2 , loc_20 )GLOBAL_LIST[0].value = loc_21 reg_0 = loc_19 break end return reg_0 end FUNC_LIST[27] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::basic_string(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> const&) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local loc_16 = 0  local loc_17 = 0  local loc_18 = 0  local loc_19 = i64_ZERO  local loc_20 = 0  local loc_21 = 0  local loc_22 = 0  local loc_23 = 0  local loc_24 = 0  local loc_25 = 0  local loc_26 = 0  local loc_27 = 0  local loc_28 = 0  local loc_29 = 0  local loc_30 = 0  local loc_31 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 8, param_0 )store_i32(memory_at_0, loc_2 + 4, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 8)store_i32(memory_at_0, loc_2 + 12, loc_3 )loc_4 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[56](loc_4 )loc_5 = reg_0 FUNC_LIST[57](loc_5 )loc_6 = 3 loc_7 = add_i32(loc_2 , loc_6 )loc_8 = loc_7 loc_9 = 2 loc_10 = add_i32(loc_2 , loc_9 )loc_11 = loc_10 reg_0 = FUNC_LIST[58](loc_3 , loc_8 , loc_11 )loc_12 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[34](loc_12 )loc_13 = reg_0 loc_14 = 1 loc_15 = band_i32(loc_13 , loc_14 )while true do while true do if loc_15 ~= 0 then break end loc_16 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[37](loc_16 )loc_17 = reg_0 reg_0 = FUNC_LIST[36](loc_3 )loc_18 = reg_0 loc_19 = load_i64(memory_at_0, loc_17 )store_i64(memory_at_0, loc_18 , loc_19 )loc_20 = 8 loc_21 = add_i32(loc_18 , loc_20 )loc_22 = add_i32(loc_17 , loc_20 )loc_23 = load_i32(memory_at_0, loc_22 )store_i32(memory_at_0, loc_21 , loc_23 )desired = 1 break end if desired then if desired == 1 then desired = nil end break end loc_24 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[45](loc_24 )loc_25 = reg_0 reg_0 = FUNC_LIST[42](loc_25 )loc_26 = reg_0 loc_27 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[43](loc_27 )loc_28 = reg_0 FUNC_LIST[147](loc_3 , loc_26 , loc_28 )break end if desired then if desired == 0 then desired = nil end break end FUNC_LIST[17](loc_3 )loc_29 = load_i32(memory_at_0, loc_2 + 12)loc_30 = 16 loc_31 = add_i32(loc_2 , loc_30 )GLOBAL_LIST[0].value = loc_31 reg_0 = loc_29 break end return reg_0 end FUNC_LIST[28] =--[[ RBX::Instance::FromDebugId(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>) ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = 12 reg_0 = FUNC_LIST[99](loc_3 )loc_4 = reg_0 loc_5 = loc_2 reg_0 = FUNC_LIST[27](loc_5 , param_0 )loc_6 = loc_2 loc_7 = 0 loc_8 = 1 loc_9 = 1 loc_10 = band_i32(loc_8 , loc_9 )reg_0 = FUNC_LIST[22](loc_4 , loc_6 , loc_7 , loc_10 )loc_11 = loc_2 reg_0 = FUNC_LIST[143](loc_11 )loc_12 = 16 loc_13 = add_i32(loc_2 , loc_12 )GLOBAL_LIST[0].value = loc_13 reg_0 = loc_4 break end return reg_0 end FUNC_LIST[29] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::basic_string[abi:v160004]() ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = 11 loc_5 = add_i32(loc_2 , loc_4 )loc_6 = loc_5 loc_7 = 10 loc_8 = add_i32(loc_2 , loc_7 )loc_9 = loc_8 reg_0 = FUNC_LIST[15](loc_3 , loc_6 , loc_9 )FUNC_LIST[17](loc_3 )FUNC_LIST[33](loc_3 )loc_10 = 16 loc_11 = add_i32(loc_2 , loc_10 )GLOBAL_LIST[0].value = loc_11 reg_0 = loc_3 break end return reg_0 end FUNC_LIST[30] =--[[ RBX::Instance::GetDebugId() ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 8)reg_0 = FUNC_LIST[27](param_0 , loc_3 )loc_4 = 16 loc_5 = add_i32(loc_2 , loc_4 )GLOBAL_LIST[0].value = loc_5 break end end FUNC_LIST[31] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::operator=[abi:v160004](char const*) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = load_i32(memory_at_0, loc_2 + 8)reg_0 = FUNC_LIST[72](loc_3 , loc_4 )loc_5 = reg_0 loc_6 = 16 loc_7 = add_i32(loc_2 , loc_6 )GLOBAL_LIST[0].value = loc_7 reg_0 = loc_5 break end return reg_0 end FUNC_LIST[32] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::operator=(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> const&) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local loc_16 = 0  local loc_17 = 0  local loc_18 = 0  local loc_19 = 0  local loc_20 = 0  local loc_21 = i64_ZERO  local loc_22 = 0  local loc_23 = 0  local loc_24 = 0  local loc_25 = 0  local loc_26 = 0  local loc_27 = 0  local loc_28 = 0  local loc_29 = 0  local loc_30 = 0  local loc_31 = 0  local loc_32 = 0  local loc_33 = 0  local loc_34 = 0  local loc_35 = 0  local loc_36 = 0  local loc_37 = 0  local loc_38 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 8, param_0 )store_i32(memory_at_0, loc_2 + 4, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 8)loc_4 = load_i32(memory_at_0, loc_2 + 4)loc_5 = loc_3 loc_6 = loc_4 loc_7 = (loc_5 ~= loc_6 and 1 or 0)loc_8 = 1 loc_9 = band_i32(loc_7 , loc_8 )while true do while true do if loc_9 == 0 then break end loc_10 = load_i32(memory_at_0, loc_2 + 4)FUNC_LIST[73](loc_3 , loc_10 )reg_0 = FUNC_LIST[34](loc_3 )loc_11 = reg_0 loc_12 = 1 loc_13 = band_i32(loc_11 , loc_12 )while true do while true do if loc_13 ~= 0 then break end loc_14 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[34](loc_14 )loc_15 = reg_0 loc_16 = 1 loc_17 = band_i32(loc_15 , loc_16 )while true do while true do if loc_17 ~= 0 then break end loc_18 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[37](loc_18 )loc_19 = reg_0 reg_0 = FUNC_LIST[36](loc_3 )loc_20 = reg_0 loc_21 = load_i64(memory_at_0, loc_19 )store_i64(memory_at_0, loc_20 , loc_21 )loc_22 = 8 loc_23 = add_i32(loc_20 , loc_22 )loc_24 = add_i32(loc_19 , loc_22 )loc_25 = load_i32(memory_at_0, loc_24 )store_i32(memory_at_0, loc_23 , loc_25 )desired = 5 break end if desired then if desired == 5 then desired = nil end break end loc_26 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[18](loc_26 )loc_27 = reg_0 loc_28 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[40](loc_28 )loc_29 = reg_0 reg_0 = FUNC_LIST[153](loc_3 , loc_27 , loc_29 )loc_30 = reg_0 store_i32(memory_at_0, loc_2 + 12, loc_30 )desired = 1 break end if desired then if desired == 4 then desired = nil end break end desired = 3 break end if desired then if desired == 3 then desired = nil end break end loc_31 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[18](loc_31 )loc_32 = reg_0 loc_33 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[40](loc_33 )loc_34 = reg_0 reg_0 = FUNC_LIST[152](loc_3 , loc_32 , loc_34 )loc_35 = reg_0 store_i32(memory_at_0, loc_2 + 12, loc_35 )desired = 1 break end if desired then if desired == 2 then desired = nil end break end break end if desired then if desired == 1 then desired = nil end break end store_i32(memory_at_0, loc_2 + 12, loc_3 )break end if desired then if desired == 0 then desired = nil end break end loc_36 = load_i32(memory_at_0, loc_2 + 12)loc_37 = 16 loc_38 = add_i32(loc_2 , loc_37 )GLOBAL_LIST[0].value = loc_38 reg_0 = loc_36 break end return reg_0 end FUNC_LIST[33] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__default_init[abi:v160004]() ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = i64_ZERO  local loc_8 = 0  local loc_9 = i64_ZERO  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = 8 loc_5 = add_i32(loc_2 , loc_4 )loc_6 = 0 store_i32(memory_at_0, loc_5 , loc_6 )loc_7 = i64_ZERO store_i64(memory_at_0, loc_2 , loc_7 )reg_0 = FUNC_LIST[36](loc_3 )loc_8 = reg_0 loc_9 = load_i64(memory_at_0, loc_2 )store_i64(memory_at_0, loc_8 , loc_9 )loc_10 = 8 loc_11 = add_i32(loc_8 , loc_10 )loc_12 = add_i32(loc_2 , loc_10 )loc_13 = load_i32(memory_at_0, loc_12 )store_i32(memory_at_0, loc_11 , loc_13 )loc_14 = 16 loc_15 = add_i32(loc_2 , loc_14 )GLOBAL_LIST[0].value = loc_15 break end end FUNC_LIST[34] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__is_long[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local loc_16 = 0  local loc_17 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[37](loc_3 )loc_4 = reg_0 loc_5 = load_i32_u8(memory_at_0, loc_4 + 11)loc_6 = 7 loc_7 = shr_u32(loc_5 , loc_6 )loc_8 = 0 loc_9 = 255 loc_10 = band_i32(loc_7 , loc_9 )loc_11 = 255 loc_12 = band_i32(loc_8 , loc_11 )loc_13 = (loc_10 ~= loc_12 and 1 or 0)loc_14 = 1 loc_15 = band_i32(loc_13 , loc_14 )loc_16 = 16 loc_17 = add_i32(loc_2 , loc_16 )GLOBAL_LIST[0].value = loc_17 reg_0 = loc_15 break end return reg_0 end FUNC_LIST[35] =--[[ void std::__2::__debug_db_swap[abi:v160004]<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>>(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>*, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>*) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )break end end FUNC_LIST[36] =--[[ std::__2::__compressed_pair<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, std::__2::allocator<char>>::first[abi:v160004]() ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[38](loc_3 )loc_4 = reg_0 loc_5 = 16 loc_6 = add_i32(loc_2 , loc_5 )GLOBAL_LIST[0].value = loc_6 reg_0 = loc_4 break end return reg_0 end FUNC_LIST[37] =--[[ std::__2::__compressed_pair<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, std::__2::allocator<char>>::first[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[39](loc_3 )loc_4 = reg_0 loc_5 = 16 loc_6 = add_i32(loc_2 , loc_5 )GLOBAL_LIST[0].value = loc_6 reg_0 = loc_4 break end return reg_0 end FUNC_LIST[38] =--[[ std::__2::__compressed_pair_elem<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, 0, false>::__get[abi:v160004]() ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = loc_3 break end return reg_0 end FUNC_LIST[39] =--[[ std::__2::__compressed_pair_elem<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, 0, false>::__get[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = loc_3 break end return reg_0 end FUNC_LIST[40] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::size[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[34](loc_3 )loc_4 = reg_0 loc_5 = 1 loc_6 = band_i32(loc_4 , loc_5 )while true do while true do if loc_6 == 0 then break end reg_0 = FUNC_LIST[43](loc_3 )loc_7 = reg_0 loc_8 = loc_7 desired = 1 break end if desired then if desired == 1 then desired = nil end break end reg_0 = FUNC_LIST[44](loc_3 )loc_9 = reg_0 loc_8 = loc_9 break end if desired then if desired == 0 then desired = nil end break end loc_10 = loc_8 loc_11 = 16 loc_12 = add_i32(loc_2 , loc_11 )GLOBAL_LIST[0].value = loc_12 reg_0 = loc_10 break end return reg_0 end FUNC_LIST[41] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__get_pointer[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[34](loc_3 )loc_4 = reg_0 loc_5 = 1 loc_6 = band_i32(loc_4 , loc_5 )while true do while true do if loc_6 == 0 then break end reg_0 = FUNC_LIST[45](loc_3 )loc_7 = reg_0 loc_8 = loc_7 desired = 1 break end if desired then if desired == 1 then desired = nil end break end reg_0 = FUNC_LIST[46](loc_3 )loc_9 = reg_0 loc_8 = loc_9 break end if desired then if desired == 0 then desired = nil end break end loc_10 = loc_8 loc_11 = 16 loc_12 = add_i32(loc_2 , loc_11 )GLOBAL_LIST[0].value = loc_12 reg_0 = loc_10 break end return reg_0 end FUNC_LIST[42] =--[[ char const* std::__2::__to_address[abi:v160004]<char const>(char const*) ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = loc_3 break end return reg_0 end FUNC_LIST[43] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__get_long_size[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[37](loc_3 )loc_4 = reg_0 loc_5 = load_i32(memory_at_0, loc_4 + 4)loc_6 = 16 loc_7 = add_i32(loc_2 , loc_6 )GLOBAL_LIST[0].value = loc_7 reg_0 = loc_5 break end return reg_0 end FUNC_LIST[44] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__get_short_size[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[37](loc_3 )loc_4 = reg_0 loc_5 = load_i32_u8(memory_at_0, loc_4 + 11)loc_6 = 127 loc_7 = band_i32(loc_5 , loc_6 )loc_8 = 255 loc_9 = band_i32(loc_7 , loc_8 )loc_10 = 16 loc_11 = add_i32(loc_2 , loc_10 )GLOBAL_LIST[0].value = loc_11 reg_0 = loc_9 break end return reg_0 end FUNC_LIST[45] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__get_long_pointer[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[37](loc_3 )loc_4 = reg_0 loc_5 = load_i32(memory_at_0, loc_4 )loc_6 = 16 loc_7 = add_i32(loc_2 , loc_6 )GLOBAL_LIST[0].value = loc_7 reg_0 = loc_5 break end return reg_0 end FUNC_LIST[46] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__get_short_pointer[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[37](loc_3 )loc_4 = reg_0 reg_0 = FUNC_LIST[47](loc_4 )loc_5 = reg_0 loc_6 = 16 loc_7 = add_i32(loc_2 , loc_6 )GLOBAL_LIST[0].value = loc_7 reg_0 = loc_5 break end return reg_0 end FUNC_LIST[47] =--[[ std::__2::pointer_traits<char const*>::pointer_to[abi:v160004](char const&) ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = loc_3 break end return reg_0 end FUNC_LIST[48] =--[[ std::__2::__compressed_pair_elem<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, 0, false>::__compressed_pair_elem[abi:v160004](std::__2::__default_init_tag) ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 8, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 8)reg_0 = loc_3 break end return reg_0 end FUNC_LIST[49] =--[[ std::__2::__compressed_pair_elem<std::__2::allocator<char>, 1, true>::__compressed_pair_elem[abi:v160004](std::__2::__default_init_tag) ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 8, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 8)reg_0 = FUNC_LIST[51](loc_3 )loc_4 = 16 loc_5 = add_i32(loc_2 , loc_4 )GLOBAL_LIST[0].value = loc_5 reg_0 = loc_3 break end return reg_0 end FUNC_LIST[50] =--[[ std::__2::__constexpr_strlen[abi:v160004](char const*) ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[89](loc_3 )loc_4 = reg_0 loc_5 = 16 loc_6 = add_i32(loc_2 , loc_5 )GLOBAL_LIST[0].value = loc_6 reg_0 = loc_4 break end return reg_0 end FUNC_LIST[51] =--[[ std::__2::allocator<char>::allocator[abi:v160004]() ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[52](loc_3 )loc_4 = 16 loc_5 = add_i32(loc_2 , loc_4 )GLOBAL_LIST[0].value = loc_5 reg_0 = loc_3 break end return reg_0 end FUNC_LIST[52] =--[[ std::__2::__non_trivial_if<true, std::__2::allocator<char>>::__non_trivial_if[abi:v160004]() ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = loc_3 break end return reg_0 end FUNC_LIST[53] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::operator std::__2::basic_string_view<char, std::__2::char_traits<char>>[abi:v160004]() const ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[18](loc_3 )loc_4 = reg_0 reg_0 = FUNC_LIST[40](loc_3 )loc_5 = reg_0 reg_0 = FUNC_LIST[61](param_0 , loc_4 , loc_5 )loc_6 = 16 loc_7 = add_i32(loc_2 , loc_6 )GLOBAL_LIST[0].value = loc_7 break end end FUNC_LIST[54] =--[[ std::__2::basic_string_view<char, std::__2::char_traits<char>>::basic_string_view[abi:v160004](char const*) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = load_i32(memory_at_0, loc_2 + 8)store_i32(memory_at_0, loc_3 , loc_4 )loc_5 = load_i32(memory_at_0, loc_2 + 8)reg_0 = FUNC_LIST[62](loc_5 )loc_6 = reg_0 store_i32(memory_at_0, loc_3 + 4, loc_6 )loc_7 = 16 loc_8 = add_i32(loc_2 , loc_7 )GLOBAL_LIST[0].value = loc_8 reg_0 = loc_3 break end return reg_0 end FUNC_LIST[55] =--[[ bool std::__2::operator==[abi:v160004]<char, std::__2::char_traits<char>>(std::__2::basic_string_view<char, std::__2::char_traits<char>>, std::__2::basic_string_view<char, std::__2::char_traits<char>>) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = i64_ZERO  local loc_14 = i64_ZERO  local loc_15 = 0  local loc_16 = 0  local loc_17 = 0  local loc_18 = 0  local loc_19 = 0  local loc_20 = 0  local loc_21 = 0  local loc_22 = 0  local loc_23 = 0  local loc_24 = 0  local loc_25 = 0  local loc_26 = 0  local loc_27 = 0  local loc_28 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 32 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 reg_0 = FUNC_LIST[59](param_0 )loc_3 = reg_0 reg_0 = FUNC_LIST[59](param_1 )loc_4 = reg_0 loc_5 = loc_3 loc_6 = loc_4 loc_7 = (loc_5 ~= loc_6 and 1 or 0)loc_8 = 1 loc_9 = band_i32(loc_7 , loc_8 )while true do while true do if loc_9 == 0 then break end loc_10 = 0 loc_11 = 1 loc_12 = band_i32(loc_10 , loc_11 )store_i32_n8(memory_at_0, loc_2 + 31, loc_12 )desired = 1 break end if desired then if desired == 1 then desired = nil end break end loc_13 = load_i64(memory_at_0, param_1 )store_i64(memory_at_0, loc_2 + 16, loc_13 )loc_14 = load_i64(memory_at_0, loc_2 + 16)store_i64(memory_at_0, loc_2 + 8, loc_14 )loc_15 = 8 loc_16 = add_i32(loc_2 , loc_15 )reg_0 = FUNC_LIST[60](param_0 , loc_16 )loc_17 = reg_0 loc_18 = 0 loc_19 = loc_17 loc_20 = loc_18 loc_21 = (loc_19 == loc_20 and 1 or 0)loc_22 = 1 loc_23 = band_i32(loc_21 , loc_22 )store_i32_n8(memory_at_0, loc_2 + 31, loc_23 )break end if desired then if desired == 0 then desired = nil end break end loc_24 = load_i32_u8(memory_at_0, loc_2 + 31)loc_25 = 1 loc_26 = band_i32(loc_24 , loc_25 )loc_27 = 32 loc_28 = add_i32(loc_2 , loc_27 )GLOBAL_LIST[0].value = loc_28 reg_0 = loc_26 break end return reg_0 end FUNC_LIST[56] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__alloc[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[69](loc_3 )loc_4 = reg_0 loc_5 = 16 loc_6 = add_i32(loc_2 , loc_5 )GLOBAL_LIST[0].value = loc_6 reg_0 = loc_4 break end return reg_0 end FUNC_LIST[57] =--[[ std::__2::allocator<char> std::__2::allocator_traits<std::__2::allocator<char>>::select_on_container_copy_construction[abi:v160004]<std::__2::allocator<char>, void, void>(std::__2::allocator<char> const&) ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )break end end FUNC_LIST[58] =--[[ std::__2::__compressed_pair<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, std::__2::allocator<char>>::__compressed_pair[abi:v160004]<std::__2::__default_init_tag, std::__2::allocator<char>>(std::__2::__default_init_tag&&, std::__2::allocator<char>&&) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )store_i32(memory_at_0, loc_2 + 4, param_2 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[48](loc_3 )loc_4 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[70](loc_3 , loc_4 )loc_5 = 16 loc_6 = add_i32(loc_2 , loc_5 )GLOBAL_LIST[0].value = loc_6 reg_0 = loc_3 break end return reg_0 end FUNC_LIST[59] =--[[ std::__2::basic_string_view<char, std::__2::char_traits<char>>::size[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = load_i32(memory_at_0, loc_3 + 4)reg_0 = loc_4 break end return reg_0 end FUNC_LIST[60] =--[[ std::__2::basic_string_view<char, std::__2::char_traits<char>>::compare(std::__2::basic_string_view<char, std::__2::char_traits<char>>) const ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local loc_16 = 0  local loc_17 = 0  local loc_18 = 0  local loc_19 = 0  local loc_20 = 0  local loc_21 = 0  local loc_22 = 0  local loc_23 = 0  local loc_24 = 0  local loc_25 = 0  local loc_26 = 0  local loc_27 = 0  local loc_28 = 0  local loc_29 = 0  local loc_30 = 0  local loc_31 = 0  local loc_32 = 0  local loc_33 = 0  local loc_34 = 0  local loc_35 = 0  local loc_36 = 0  local loc_37 = 0  local loc_38 = 0  local loc_39 = 0  local loc_40 = 0  local loc_41 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 32 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 28, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 28)reg_0 = FUNC_LIST[59](loc_3 )loc_4 = reg_0 store_i32(memory_at_0, loc_2 + 20, loc_4 )reg_0 = FUNC_LIST[59](param_1 )loc_5 = reg_0 store_i32(memory_at_0, loc_2 + 16, loc_5 )loc_6 = 20 loc_7 = add_i32(loc_2 , loc_6 )loc_8 = loc_7 loc_9 = 16 loc_10 = add_i32(loc_2 , loc_9 )loc_11 = loc_10 reg_0 = FUNC_LIST[63](loc_8 , loc_11 )loc_12 = reg_0 loc_13 = load_i32(memory_at_0, loc_12 )store_i32(memory_at_0, loc_2 + 24, loc_13 )reg_0 = FUNC_LIST[64](loc_3 )loc_14 = reg_0 reg_0 = FUNC_LIST[64](param_1 )loc_15 = reg_0 loc_16 = load_i32(memory_at_0, loc_2 + 24)reg_0 = FUNC_LIST[65](loc_14 , loc_15 , loc_16 )loc_17 = reg_0 store_i32(memory_at_0, loc_2 + 12, loc_17 )loc_18 = load_i32(memory_at_0, loc_2 + 12)while true do if loc_18 ~= 0 then break end reg_0 = FUNC_LIST[59](loc_3 )loc_19 = reg_0 reg_0 = FUNC_LIST[59](param_1 )loc_20 = reg_0 loc_21 = loc_19 loc_22 = loc_20 loc_23 = (loc_21 == loc_22 and 1 or 0)loc_24 = 1 loc_25 = band_i32(loc_23 , loc_24 )while true do while true do if loc_25 == 0 then break end loc_26 = 0 loc_27 = loc_26 desired = 2 break end if desired then if desired == 2 then desired = nil end break end reg_0 = FUNC_LIST[59](loc_3 )loc_28 = reg_0 reg_0 = FUNC_LIST[59](param_1 )loc_29 = reg_0 loc_30 = loc_28 loc_31 = loc_29 loc_32 = (loc_30 < loc_31 and 1 or 0)loc_33 = 4294967295 loc_34 = 1 loc_35 = 1 loc_36 = band_i32(loc_32 , loc_35 )loc_37 = (loc_36 ~= 0 and loc_33 or loc_34 )loc_27 = loc_37 break end if desired then if desired == 1 then desired = nil end break end loc_38 = loc_27 store_i32(memory_at_0, loc_2 + 12, loc_38 )break end if desired then if desired == 0 then desired = nil end break end loc_39 = load_i32(memory_at_0, loc_2 + 12)loc_40 = 32 loc_41 = add_i32(loc_2 , loc_40 )GLOBAL_LIST[0].value = loc_41 reg_0 = loc_39 break end return reg_0 end FUNC_LIST[61] =--[[ std::__2::basic_string_view<char, std::__2::char_traits<char>>::basic_string_view[abi:v160004](char const*, unsigned long) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 8, param_0 )store_i32(memory_at_0, loc_2 + 4, param_1 )store_i32(memory_at_0, loc_2 , param_2 )loc_3 = load_i32(memory_at_0, loc_2 + 8)store_i32(memory_at_0, loc_2 + 12, loc_3 )loc_4 = load_i32(memory_at_0, loc_2 + 4)store_i32(memory_at_0, loc_3 , loc_4 )loc_5 = load_i32(memory_at_0, loc_2 )store_i32(memory_at_0, loc_3 + 4, loc_5 )loc_6 = load_i32(memory_at_0, loc_2 )loc_7 = 1 loc_8 = loc_7 while true do if loc_6 == 0 then break end loc_9 = load_i32(memory_at_0, loc_2 + 4)loc_10 = 0 loc_11 = loc_9 loc_12 = loc_10 loc_13 = (loc_11 ~= loc_12 and 1 or 0)loc_8 = loc_13 break end if desired then if desired == 0 then desired = nil end break end loc_14 = load_i32(memory_at_0, loc_2 + 12)reg_0 = loc_14 break end return reg_0 end FUNC_LIST[62] =--[[ unsigned long std::__2::__char_traits_length_checked[abi:v160004]<std::__2::char_traits<char>>(std::__2::char_traits<char>::char_type const*) ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[16](loc_3 )loc_4 = reg_0 loc_5 = 16 loc_6 = add_i32(loc_2 , loc_5 )GLOBAL_LIST[0].value = loc_6 reg_0 = loc_4 break end return reg_0 end FUNC_LIST[63] =--[[ unsigned long const& std::__2::min[abi:v160004]<unsigned long>(unsigned long const&, unsigned long const&) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = load_i32(memory_at_0, loc_2 + 8)reg_0 = FUNC_LIST[66](loc_3 , loc_4 )loc_5 = reg_0 loc_6 = 16 loc_7 = add_i32(loc_2 , loc_6 )GLOBAL_LIST[0].value = loc_7 reg_0 = loc_5 break end return reg_0 end FUNC_LIST[64] =--[[ std::__2::basic_string_view<char, std::__2::char_traits<char>>::data[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = load_i32(memory_at_0, loc_3 )reg_0 = loc_4 break end return reg_0 end FUNC_LIST[65] =--[[ std::__2::char_traits<char>::compare(char const*, char const*, unsigned long) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 8, param_0 )store_i32(memory_at_0, loc_2 + 4, param_1 )store_i32(memory_at_0, loc_2 , param_2 )loc_3 = load_i32(memory_at_0, loc_2 )while true do while true do if loc_3 ~= 0 then break end loc_4 = 0 store_i32(memory_at_0, loc_2 + 12, loc_4 )desired = 1 break end if desired then if desired == 1 then desired = nil end break end loc_5 = load_i32(memory_at_0, loc_2 + 8)loc_6 = load_i32(memory_at_0, loc_2 + 4)loc_7 = load_i32(memory_at_0, loc_2 )reg_0 = FUNC_LIST[67](loc_5 , loc_6 , loc_7 )loc_8 = reg_0 store_i32(memory_at_0, loc_2 + 12, loc_8 )break end if desired then if desired == 0 then desired = nil end break end loc_9 = load_i32(memory_at_0, loc_2 + 12)loc_10 = 16 loc_11 = add_i32(loc_2 , loc_10 )GLOBAL_LIST[0].value = loc_11 reg_0 = loc_9 break end return reg_0 end FUNC_LIST[66] =--[[ unsigned long const& std::__2::min[abi:v160004]<unsigned long, std::__2::__less<unsigned long, unsigned long>>(unsigned long const&, unsigned long const&, std::__2::__less<unsigned long, unsigned long>) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local loc_16 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 8, param_0 )store_i32(memory_at_0, loc_2 + 4, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 4)loc_4 = load_i32(memory_at_0, loc_2 + 8)loc_5 = 15 loc_6 = add_i32(loc_2 , loc_5 )loc_7 = loc_6 reg_0 = FUNC_LIST[68](loc_7 , loc_3 , loc_4 )loc_8 = reg_0 loc_9 = 1 loc_10 = band_i32(loc_8 , loc_9 )while true do while true do if loc_10 == 0 then break end loc_11 = load_i32(memory_at_0, loc_2 + 4)loc_12 = loc_11 desired = 1 break end if desired then if desired == 1 then desired = nil end break end loc_13 = load_i32(memory_at_0, loc_2 + 8)loc_12 = loc_13 break end if desired then if desired == 0 then desired = nil end break end loc_14 = loc_12 loc_15 = 16 loc_16 = add_i32(loc_2 , loc_15 )GLOBAL_LIST[0].value = loc_16 reg_0 = loc_14 break end return reg_0 end FUNC_LIST[67] =--[[ int std::__2::__constexpr_memcmp[abi:v160004]<char>(char const*, char const*, unsigned long) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )store_i32(memory_at_0, loc_2 + 4, param_2 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = load_i32(memory_at_0, loc_2 + 8)loc_5 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[88](loc_3 , loc_4 , loc_5 )loc_6 = reg_0 loc_7 = 16 loc_8 = add_i32(loc_2 , loc_7 )GLOBAL_LIST[0].value = loc_8 reg_0 = loc_6 break end return reg_0 end FUNC_LIST[68] =--[[ std::__2::__less<unsigned long, unsigned long>::operator()[abi:v160004](unsigned long const&, unsigned long const&) const ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )store_i32(memory_at_0, loc_2 + 4, param_2 )loc_3 = load_i32(memory_at_0, loc_2 + 8)loc_4 = load_i32(memory_at_0, loc_3 )loc_5 = load_i32(memory_at_0, loc_2 + 4)loc_6 = load_i32(memory_at_0, loc_5 )loc_7 = loc_4 loc_8 = loc_6 loc_9 = (loc_7 < loc_8 and 1 or 0)loc_10 = 1 loc_11 = band_i32(loc_9 , loc_10 )reg_0 = loc_11 break end return reg_0 end FUNC_LIST[69] =--[[ std::__2::__compressed_pair<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, std::__2::allocator<char>>::second[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[71](loc_3 )loc_4 = reg_0 loc_5 = 16 loc_6 = add_i32(loc_2 , loc_5 )GLOBAL_LIST[0].value = loc_6 reg_0 = loc_4 break end return reg_0 end FUNC_LIST[70] =--[[ std::__2::__compressed_pair_elem<std::__2::allocator<char>, 1, true>::__compressed_pair_elem[abi:v160004]<std::__2::allocator<char>, void>(std::__2::allocator<char>&&) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = loc_3 break end return reg_0 end FUNC_LIST[71] =--[[ std::__2::__compressed_pair_elem<std::__2::allocator<char>, 1, true>::__get[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = loc_3 break end return reg_0 end FUNC_LIST[72] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::assign(char const*) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = load_i32(memory_at_0, loc_2 + 8)reg_0 = FUNC_LIST[149](loc_3 , loc_4 )loc_5 = reg_0 loc_6 = 16 loc_7 = add_i32(loc_2 , loc_6 )GLOBAL_LIST[0].value = loc_7 reg_0 = loc_5 break end return reg_0 end FUNC_LIST[73] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__copy_assign_alloc[abi:v160004](std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> const&) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = load_i32(memory_at_0, loc_2 + 8)FUNC_LIST[84](loc_3 , loc_4 )loc_5 = 16 loc_6 = add_i32(loc_2 , loc_5 )GLOBAL_LIST[0].value = loc_6 break end end FUNC_LIST[74] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__fits_in_sso[abi:v160004](unsigned long) ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = 11 loc_5 = loc_3 loc_6 = loc_4 loc_7 = (loc_5 < loc_6 and 1 or 0)loc_8 = 1 loc_9 = band_i32(loc_7 , loc_8 )reg_0 = loc_9 break end return reg_0 end FUNC_LIST[75] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__set_long_size[abi:v160004](unsigned long) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = load_i32(memory_at_0, loc_2 + 8)reg_0 = FUNC_LIST[36](loc_3 )loc_5 = reg_0 store_i32(memory_at_0, loc_5 + 4, loc_4 )loc_6 = 16 loc_7 = add_i32(loc_2 , loc_6 )GLOBAL_LIST[0].value = loc_7 break end end FUNC_LIST[76] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__get_long_pointer[abi:v160004]() ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[36](loc_3 )loc_4 = reg_0 loc_5 = load_i32(memory_at_0, loc_4 )loc_6 = 16 loc_7 = add_i32(loc_2 , loc_6 )GLOBAL_LIST[0].value = loc_7 reg_0 = loc_5 break end return reg_0 end FUNC_LIST[77] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__set_short_size[abi:v160004](unsigned long) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local loc_11 = 0  local loc_12 = 0  local loc_13 = 0  local loc_14 = 0  local loc_15 = 0  local loc_16 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = load_i32_u8(memory_at_0, loc_2 + 8)reg_0 = FUNC_LIST[36](loc_3 )loc_5 = reg_0 loc_6 = load_i32_u8(memory_at_0, loc_5 + 11)loc_7 = 127 loc_8 = band_i32(loc_4 , loc_7 )loc_9 = 128 loc_10 = band_i32(loc_6 , loc_9 )loc_11 = bor_i32(loc_10 , loc_8 )store_i32_n8(memory_at_0, loc_5 + 11, loc_11 )reg_0 = FUNC_LIST[36](loc_3 )loc_12 = reg_0 loc_13 = load_i32_u8(memory_at_0, loc_12 + 11)loc_14 = band_i32(loc_13 , loc_7 )store_i32_n8(memory_at_0, loc_12 + 11, loc_14 )loc_15 = 16 loc_16 = add_i32(loc_2 , loc_15 )GLOBAL_LIST[0].value = loc_16 break end end FUNC_LIST[78] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__get_short_pointer[abi:v160004]() ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = FUNC_LIST[36](loc_3 )loc_4 = reg_0 reg_0 = FUNC_LIST[82](loc_4 )loc_5 = reg_0 loc_6 = 16 loc_7 = add_i32(loc_2 , loc_6 )GLOBAL_LIST[0].value = loc_7 reg_0 = loc_5 break end return reg_0 end FUNC_LIST[79] =--[[ char* std::__2::__to_address[abi:v160004]<char>(char*) ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = loc_3 break end return reg_0 end FUNC_LIST[80] =--[[ std::__2::char_traits<char>::move(char*, char const*, unsigned long) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )store_i32(memory_at_0, loc_2 + 4, param_2 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = load_i32(memory_at_0, loc_2 + 8)loc_5 = load_i32(memory_at_0, loc_2 + 4)reg_0 = FUNC_LIST[83](loc_3 , loc_4 , loc_5 )loc_6 = reg_0 loc_7 = 16 loc_8 = add_i32(loc_2 , loc_7 )GLOBAL_LIST[0].value = loc_8 reg_0 = loc_6 break end return reg_0 end FUNC_LIST[81] =--[[ std::__2::char_traits<char>::assign(char&, char const&) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )loc_3 = load_i32(memory_at_0, loc_2 + 8)loc_4 = load_i32_u8(memory_at_0, loc_3 )loc_5 = load_i32(memory_at_0, loc_2 + 12)store_i32_n8(memory_at_0, loc_5 , loc_4 )break end end FUNC_LIST[82] =--[[ std::__2::pointer_traits<char*>::pointer_to[abi:v160004](char&) ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 12, param_0 )loc_3 = load_i32(memory_at_0, loc_2 + 12)reg_0 = loc_3 break end return reg_0 end FUNC_LIST[83] =--[[ char* std::__2::__char_traits_move[abi:v160004]<char>(char*, char const*, unsigned long) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local reg_0 local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )GLOBAL_LIST[0].value = loc_2 store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_1 )store_i32(memory_at_0, loc_2 + 4, param_2 )loc_3 = load_i32(memory_at_0, loc_2 + 12)loc_4 = load_i32(memory_at_0, loc_2 + 8)loc_5 = load_i32(memory_at_0, loc_2 + 4)loc_6 = 0 loc_7 = shl_i32(loc_5 , loc_6 )reg_0 = FUNC_LIST[87](loc_3 , loc_4 , loc_7 )loc_8 = load_i32(memory_at_0, loc_2 + 12)loc_9 = 16 loc_10 = add_i32(loc_2 , loc_9 )GLOBAL_LIST[0].value = loc_10 reg_0 = loc_8 break end return reg_0 end FUNC_LIST[84] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__copy_assign_alloc[abi:v160004](std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> const&, std::__2::integral_constant<bool, false>) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local desired while true do loc_0 = GLOBAL_LIST[0].value loc_1 = 16 loc_2 = sub_i32(loc_0 , loc_1 )store_i32(memory_at_0, loc_2 + 8, param_0 )store_i32(memory_at_0, loc_2 + 4, param_1 )break end end FUNC_LIST[85] =--[[ main ]]function(param_0, param_1)local loc_0 = 0  local reg_0 local desired while true do reg_0 = FUNC_LIST[20]()loc_0 = reg_0 reg_0 = loc_0 break end return reg_0 end FUNC_LIST[86] =--[[ __memcpy ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local reg_0 local desired while true do while true do if param_2 < 512 then break end FUNC_LIST[0](param_0 , param_1 , param_2 )reg_0 = param_0 desired = 0 break end if desired then if desired == 0 then desired = nil end break end loc_0 = add_i32(param_0 , param_2 )while true do while true do if band_i32(bxor_i32(param_1 , param_0 ), 3 )~= 0 then break end while true do while true do if band_i32(param_0 , 3 )~= 0 then break end param_2 = param_0 desired = 3 break end if desired then if desired == 3 then desired = nil end break end while true do if param_2 ~= 0 then break end param_2 = param_0 desired = 3 break end if desired then if desired == 3 then desired = nil end break end param_2 = param_0 while true do store_i32_n8(memory_at_0, param_2 , load_i32_u8(memory_at_0, param_1 ))param_1 = add_i32(param_1 , 1 )param_2 = add_i32(param_2 , 1 )if band_i32(param_2 , 3 )== 0 then desired = 3 break end if param_2 < loc_0 then continue end break end if desired then if desired == 3 then desired = nil end break end break end if desired then if desired == 2 then desired = nil end break end while true do loc_1 = band_i32(loc_0 , 4294967292 )if loc_1 < 64 then break end loc_2 = add_i32(loc_1 , 4294967232 )if param_2 > loc_2 then break end while true do store_i32(memory_at_0, param_2 , load_i32(memory_at_0, param_1 ))store_i32(memory_at_0, param_2 + 4, load_i32(memory_at_0, param_1 + 4))store_i32(memory_at_0, param_2 + 8, load_i32(memory_at_0, param_1 + 8))store_i32(memory_at_0, param_2 + 12, load_i32(memory_at_0, param_1 + 12))store_i32(memory_at_0, param_2 + 16, load_i32(memory_at_0, param_1 + 16))store_i32(memory_at_0, param_2 + 20, load_i32(memory_at_0, param_1 + 20))store_i32(memory_at_0, param_2 + 24, load_i32(memory_at_0, param_1 + 24))store_i32(memory_at_0, param_2 + 28, load_i32(memory_at_0, param_1 + 28))store_i32(memory_at_0, param_2 + 32, load_i32(memory_at_0, param_1 + 32))store_i32(memory_at_0, param_2 + 36, load_i32(memory_at_0, param_1 + 36))store_i32(memory_at_0, param_2 + 40, load_i32(memory_at_0, param_1 + 40))store_i32(memory_at_0, param_2 + 44, load_i32(memory_at_0, param_1 + 44))store_i32(memory_at_0, param_2 + 48, load_i32(memory_at_0, param_1 + 48))store_i32(memory_at_0, param_2 + 52, load_i32(memory_at_0, param_1 + 52))store_i32(memory_at_0, param_2 + 56, load_i32(memory_at_0, param_1 + 56))store_i32(memory_at_0, param_2 + 60, load_i32(memory_at_0, param_1 + 60))param_1 = add_i32(param_1 , 64 )param_2 = add_i32(param_2 , 64 )if param_2 <= loc_2 then continue end break end if desired then if desired == 3 then desired = nil end break end break end if desired then if desired == 2 then desired = nil end break end if param_2 >= loc_1 then desired = 1 break end while true do store_i32(memory_at_0, param_2 , load_i32(memory_at_0, param_1 ))param_1 = add_i32(param_1 , 4 )param_2 = add_i32(param_2 , 4 )if param_2 < loc_1 then continue end desired = 1 break end if desired then if desired == 2 then desired = nil end break end error("out of code bounds")end if desired then if desired == 1 then desired = nil end break end while true do if loc_0 >= 4 then break end param_2 = param_0 desired = 1 break end if desired then if desired == 1 then desired = nil end break end while true do loc_1 = add_i32(loc_0 , 4294967292 )if loc_1 >= param_0 then break end param_2 = param_0 desired = 1 break end if desired then if desired == 1 then desired = nil end break end param_2 = param_0 while true do store_i32_n8(memory_at_0, param_2 , load_i32_u8(memory_at_0, param_1 ))store_i32_n8(memory_at_0, param_2 + 1, load_i32_u8(memory_at_0, param_1 + 1))store_i32_n8(memory_at_0, param_2 + 2, load_i32_u8(memory_at_0, param_1 + 2))store_i32_n8(memory_at_0, param_2 + 3, load_i32_u8(memory_at_0, param_1 + 3))param_1 = add_i32(param_1 , 4 )param_2 = add_i32(param_2 , 4 )if param_2 <= loc_1 then continue end break end if desired then if desired == 1 then desired = nil end break end break end if desired then if desired == 0 then desired = nil end break end while true do if param_2 >= loc_0 then break end while true do store_i32_n8(memory_at_0, param_2 , load_i32_u8(memory_at_0, param_1 ))param_1 = add_i32(param_1 , 1 )param_2 = add_i32(param_2 , 1 )if param_2 ~= loc_0 then continue end break end if desired then if desired == 1 then desired = nil end break end break end if desired then if desired == 0 then desired = nil end break end reg_0 = param_0 break end return reg_0 end FUNC_LIST[87] =--[[ memmove ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local reg_0 local desired while true do while true do if param_0 == param_1 then break end while true do loc_0 = add_i32(param_0 , param_2 )if sub_i32(param_1 , loc_0 )> sub_i32(0 , shl_i32(param_2 , 1 ))then break end reg_0 = FUNC_LIST[86](param_0 , param_1 , param_2 )desired = 0 break end if desired then if desired == 1 then desired = nil end break end loc_1 = band_i32(bxor_i32(param_1 , param_0 ), 3 )while true do while true do while true do if param_0 >= param_1 then break end while true do if loc_1 == 0 then break end loc_0 = param_0 desired = 2 break end if desired then if desired == 4 then desired = nil end break end while true do if band_i32(param_0 , 3 )~= 0 then break end loc_0 = param_0 desired = 3 break end if desired then if desired == 4 then desired = nil end break end loc_0 = param_0 while true do if param_2 == 0 then desired = 1 break end store_i32_n8(memory_at_0, loc_0 , load_i32_u8(memory_at_0, param_1 ))param_1 = add_i32(param_1 , 1 )param_2 = add_i32(param_2 , 4294967295 )loc_0 = add_i32(loc_0 , 1 )if band_i32(loc_0 , 3 )== 0 then desired = 3 break end continue end if desired then if desired == 4 then desired = nil end break end error("out of code bounds")end if desired then if desired == 3 then desired = nil end break end while true do if loc_1 ~= 0 then break end while true do if band_i32(loc_0 , 3 )== 0 then break end while true do if param_2 == 0 then desired = 1 break end param_2 = add_i32(param_2 , 4294967295 )loc_0 = add_i32(param_0 , param_2 )store_i32_n8(memory_at_0, loc_0 , load_i32_u8(memory_at_0, add_i32(param_1 , param_2 )))if band_i32(loc_0 , 3 )~= 0 then continue end break end if desired then if desired == 5 then desired = nil end break end break end if desired then if desired == 4 then desired = nil end break end if param_2 <= 3 then break end while true do param_2 = add_i32(param_2 , 4294967292 )store_i32(memory_at_0, add_i32(param_0 , param_2 ), load_i32(memory_at_0, add_i32(param_1 , param_2 )))if param_2 > 3 then continue end break end if desired then if desired == 4 then desired = nil end break end break end if desired then if desired == 3 then desired = nil end break end if param_2 == 0 then desired = 1 break end while true do param_2 = add_i32(param_2 , 4294967295 )store_i32_n8(memory_at_0, add_i32(param_0 , param_2 ), load_i32_u8(memory_at_0, add_i32(param_1 , param_2 )))if param_2 ~= 0 then continue end desired = 1 break end if desired then if desired == 3 then desired = nil end break end error("out of code bounds")end if desired then if desired == 2 then desired = nil end break end if param_2 <= 3 then break end while true do store_i32(memory_at_0, loc_0 , load_i32(memory_at_0, param_1 ))param_1 = add_i32(param_1 , 4 )loc_0 = add_i32(loc_0 , 4 )param_2 = add_i32(param_2 , 4294967292 )if param_2 > 3 then continue end break end if desired then if desired == 2 then desired = nil end break end break end if desired then if desired == 1 then desired = nil end break end if param_2 == 0 then break end while true do store_i32_n8(memory_at_0, loc_0 , load_i32_u8(memory_at_0, param_1 ))loc_0 = add_i32(loc_0 , 1 )param_1 = add_i32(param_1 , 1 )param_2 = add_i32(param_2 , 4294967295 )if param_2 ~= 0 then continue end break end if desired then if desired == 1 then desired = nil end break end break end if desired then if desired == 0 then desired = nil end break end reg_0 = param_0 break end return reg_0 end FUNC_LIST[88] =--[[ memcmp ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local reg_0 local desired while true do while true do while true do while true do if param_2 < 4 then break end if band_i32(bor_i32(param_1 , param_0 ), 3 )~= 0 then desired = 2 break end while true do if load_i32(memory_at_0, param_0 )~= load_i32(memory_at_0, param_1 )then desired = 2 break end param_1 = add_i32(param_1 , 4 )param_0 = add_i32(param_0 , 4 )param_2 = add_i32(param_2 , 4294967292 )if param_2 > 3 then continue end break end if desired then if desired == 3 then desired = nil end break end break end if desired then if desired == 2 then desired = nil end break end if param_2 == 0 then desired = 1 break end break end if desired then if desired == 1 then desired = nil end break end while true do while true do loc_0 = load_i32_u8(memory_at_0, param_0 )loc_1 = load_i32_u8(memory_at_0, param_1 )if loc_0 ~= loc_1 then desired = 2 break end param_1 = add_i32(param_1 , 1 )param_0 = add_i32(param_0 , 1 )param_2 = add_i32(param_2 , 4294967295 )if param_2 == 0 then desired = 1 break end continue end if desired then if desired == 2 then desired = nil end break end error("out of code bounds")end if desired then if desired == 1 then desired = nil end break end reg_0 = sub_i32(loc_0 , loc_1 )desired = 0 break end if desired then if desired == 0 then desired = nil end break end reg_0 = 0 break end return reg_0 end FUNC_LIST[89] =--[[ strlen ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local reg_0 local desired while true do loc_0 = param_0 while true do while true do if band_i32(param_0 , 3 )== 0 then break end while true do if load_i32_u8(memory_at_0, param_0 )~= 0 then break end reg_0 = sub_i32(param_0 , param_0 )desired = 0 break end if desired then if desired == 2 then desired = nil end break end loc_0 = param_0 while true do loc_0 = add_i32(loc_0 , 1 )if band_i32(loc_0 , 3 )== 0 then desired = 2 break end if load_i32_u8(memory_at_0, loc_0 )~= 0 then continue end desired = 1 break end if desired then if desired == 2 then desired = nil end break end error("out of code bounds")end if desired then if desired == 1 then desired = nil end break end while true do loc_1 = loc_0 loc_0 = add_i32(loc_1 , 4 )loc_2 = load_i32(memory_at_0, loc_1 )if band_i32(band_i32(bxor_i32(loc_2 , 4294967295 ), add_i32(loc_2 , 4278124287 )), 2155905152 )== 0 then continue end break end if desired then if desired == 1 then desired = nil end break end while true do loc_0 = loc_1 loc_1 = add_i32(loc_0 , 1 )if load_i32_u8(memory_at_0, loc_0 )~= 0 then continue end break end if desired then if desired == 1 then desired = nil end break end break end if desired then if desired == 0 then desired = nil end break end reg_0 = sub_i32(loc_0 , param_0 )break end return reg_0 end FUNC_LIST[90] =--[[ emscripten_get_heap_size ]]function()local reg_0 while true do reg_0 = shl_i32(memory_at_0.min , 16 )break end return reg_0 end FUNC_LIST[91] =--[[ __errno_location ]]function()local reg_0 while true do reg_0 = 67084 break end return reg_0 end FUNC_LIST[92] =--[[ sbrk ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local reg_0, reg_1 local desired while true do loc_0 = load_i32(memory_at_0, 0 + 66928)loc_1 = band_i32(add_i32(param_0 , 7 ), 4294967288 )param_0 = add_i32(loc_0 , loc_1 )while true do while true do if loc_1 == 0 then break end if param_0 <= loc_0 then desired = 1 break end break end if desired then if desired == 1 then desired = nil end break end while true do reg_1 = FUNC_LIST[90]()if param_0 <= reg_1 then break end reg_0 = FUNC_LIST[2](param_0 )if reg_0 == 0 then desired = 1 break end break end if desired then if desired == 1 then desired = nil end break end store_i32(memory_at_0, 0 + 66928, param_0 )reg_0 = loc_0 desired = 0 break end if desired then if desired == 0 then desired = nil end break end reg_0 = FUNC_LIST[91]()store_i32(memory_at_0, reg_0 , 48 )reg_0 = 4294967295 break end return reg_0 end FUNC_LIST[93] =--[[ dlmalloc ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local loc_7 = 0  local loc_8 = 0  local loc_9 = 0  local loc_10 = 0  local reg_0 local desired while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 16 )GLOBAL_LIST[0].value = loc_0 while true do while true do while true do while true do while true do while true do while true do while true do while true do while true do while true do while true do while true do while true do while true do if param_0 > 244 then break end while true do loc_1 = load_i32(memory_at_0, 0 + 67088)loc_2 = (param_0 < 11 and 16 or band_i32(add_i32(param_0 , 11 ), 4294967288 ))loc_3 = shr_u32(loc_2 , 3 )param_0 = shr_u32(loc_1 , loc_3 )if band_i32(param_0 , 3 )== 0 then break end while true do while true do loc_4 = add_i32(band_i32(bxor_i32(param_0 , 4294967295 ), 1 ), loc_3 )loc_3 = shl_i32(loc_4 , 3 )param_0 = add_i32(loc_3 , 67128 )loc_3 = load_i32(memory_at_0, add_i32(loc_3 , 67136 ))loc_2 = load_i32(memory_at_0, loc_3 + 8)if param_0 ~= loc_2 then break end store_i32(memory_at_0, 0 + 67088, band_i32(loc_1 , rotl_i32(4294967294 , loc_4 )))desired = 17 break end if desired then if desired == 17 then desired = nil end break end store_i32(memory_at_0, loc_2 + 12, param_0 )store_i32(memory_at_0, param_0 + 8, loc_2 )break end if desired then if desired == 16 then desired = nil end break end param_0 = add_i32(loc_3 , 8 )loc_4 = shl_i32(loc_4 , 3 )store_i32(memory_at_0, loc_3 + 4, bor_i32(loc_4 , 3 ))loc_3 = add_i32(loc_3 , loc_4 )store_i32(memory_at_0, loc_3 + 4, bor_i32(load_i32(memory_at_0, loc_3 + 4), 1 ))desired = 1 break end if desired then if desired == 15 then desired = nil end break end loc_5 = load_i32(memory_at_0, 0 + 67096)if loc_2 <= loc_5 then desired = 14 break end while true do if param_0 == 0 then break end while true do while true do reg_0 = shl_i32(param_0 , loc_3 )param_0 = shl_i32(2 , loc_3 )loc_3 = ctz_i32(band_i32(reg_0 , bor_i32(param_0 , sub_i32(0 , param_0 ))))param_0 = shl_i32(loc_3 , 3 )loc_4 = add_i32(param_0 , 67128 )param_0 = load_i32(memory_at_0, add_i32(param_0 , 67136 ))loc_6 = load_i32(memory_at_0, param_0 + 8)if loc_4 ~= loc_6 then break end loc_1 = band_i32(loc_1 , rotl_i32(4294967294 , loc_3 ))store_i32(memory_at_0, 0 + 67088, loc_1 )desired = 17 break end if desired then if desired == 17 then desired = nil end break end store_i32(memory_at_0, loc_6 + 12, loc_4 )store_i32(memory_at_0, loc_4 + 8, loc_6 )break end if desired then if desired == 16 then desired = nil end break end store_i32(memory_at_0, param_0 + 4, bor_i32(loc_2 , 3 ))loc_6 = add_i32(param_0 , loc_2 )loc_3 = shl_i32(loc_3 , 3 )loc_4 = sub_i32(loc_3 , loc_2 )store_i32(memory_at_0, loc_6 + 4, bor_i32(loc_4 , 1 ))store_i32(memory_at_0, add_i32(param_0 , loc_3 ), loc_4 )while true do if loc_5 == 0 then break end loc_2 = add_i32(band_i32(loc_5 , 4294967288 ), 67128 )loc_3 = load_i32(memory_at_0, 0 + 67108)while true do while true do loc_7 = shl_i32(1 , shr_u32(loc_5 , 3 ))if band_i32(loc_1 , loc_7 )~= 0 then break end store_i32(memory_at_0, 0 + 67088, bor_i32(loc_1 , loc_7 ))loc_7 = loc_2 desired = 18 break end if desired then if desired == 18 then desired = nil end break end loc_7 = load_i32(memory_at_0, loc_2 + 8)break end if desired then if desired == 17 then desired = nil end break end store_i32(memory_at_0, loc_2 + 8, loc_3 )store_i32(memory_at_0, loc_7 + 12, loc_3 )store_i32(memory_at_0, loc_3 + 12, loc_2 )store_i32(memory_at_0, loc_3 + 8, loc_7 )break end if desired then if desired == 16 then desired = nil end break end param_0 = add_i32(param_0 , 8 )store_i32(memory_at_0, 0 + 67108, loc_6 )store_i32(memory_at_0, 0 + 67096, loc_4 )desired = 1 break end if desired then if desired == 15 then desired = nil end break end loc_8 = load_i32(memory_at_0, 0 + 67092)if loc_8 == 0 then desired = 14 break end loc_6 = load_i32(memory_at_0, add_i32(shl_i32(ctz_i32(loc_8 ), 2 ), 67392 ))loc_3 = sub_i32(band_i32(load_i32(memory_at_0, loc_6 + 4), 4294967288 ), loc_2 )loc_4 = loc_6 while true do while true do while true do param_0 = load_i32(memory_at_0, loc_4 + 16)if param_0 ~= 0 then break end param_0 = load_i32(memory_at_0, add_i32(loc_4 , 20 ))if param_0 == 0 then desired = 16 break end break end if desired then if desired == 17 then desired = nil continue end break end loc_4 = sub_i32(band_i32(load_i32(memory_at_0, param_0 + 4), 4294967288 ), loc_2 )reg_0 = loc_4 loc_4 = (loc_4 < loc_3 and 1 or 0)loc_3 = (loc_4 ~= 0 and reg_0 or loc_3 )loc_6 = (loc_4 ~= 0 and param_0 or loc_6 )loc_4 = param_0 continue end if desired then if desired == 16 then desired = nil end break end error("out of code bounds")end if desired then if desired == 15 then desired = nil end break end loc_9 = load_i32(memory_at_0, loc_6 + 24)while true do loc_7 = load_i32(memory_at_0, loc_6 + 12)if loc_7 == loc_6 then break end param_0 = load_i32(memory_at_0, loc_6 + 8)store_i32(memory_at_0, param_0 + 12, loc_7 )store_i32(memory_at_0, loc_7 + 8, param_0 )desired = 2 break end if desired then if desired == 15 then desired = nil end break end while true do loc_4 = add_i32(loc_6 , 20 )param_0 = load_i32(memory_at_0, loc_4 )if param_0 ~= 0 then break end param_0 = load_i32(memory_at_0, loc_6 + 16)if param_0 == 0 then desired = 13 break end loc_4 = add_i32(loc_6 , 16 )break end if desired then if desired == 15 then desired = nil end break end while true do loc_10 = loc_4 loc_7 = param_0 loc_4 = add_i32(loc_7 , 20 )param_0 = load_i32(memory_at_0, loc_4 )if param_0 ~= 0 then continue end loc_4 = add_i32(loc_7 , 16 )param_0 = load_i32(memory_at_0, loc_7 + 16)if param_0 ~= 0 then continue end break end if desired then if desired == 15 then desired = nil end break end store_i32(memory_at_0, loc_10 , 0 )desired = 2 break end if desired then if desired == 14 then desired = nil end break end loc_2 = 4294967295 if param_0 > 4294967231 then break end param_0 = add_i32(param_0 , 11 )loc_2 = band_i32(param_0 , 4294967288 )loc_5 = load_i32(memory_at_0, 0 + 67092)if loc_5 == 0 then break end loc_10 = 0 while true do if loc_2 < 256 then break end loc_10 = 31 if loc_2 > 16777215 then break end param_0 = clz_i32(shr_u32(param_0 , 8 ))loc_10 = add_i32(sub_i32(band_i32(shr_u32(loc_2 , sub_i32(38 , param_0 )), 1 ), shl_i32(param_0 , 1 )), 62 )break end if desired then if desired == 14 then desired = nil end break end loc_3 = sub_i32(0 , loc_2 )while true do while true do while true do while true do loc_4 = load_i32(memory_at_0, add_i32(shl_i32(loc_10 , 2 ), 67392 ))if loc_4 ~= 0 then break end param_0 = 0 loc_7 = 0 desired = 17 break end if desired then if desired == 17 then desired = nil end break end param_0 = 0 loc_6 = shl_i32(loc_2 , (loc_10 == 31 and 0 or sub_i32(25 , shr_u32(loc_10 , 1 ))))loc_7 = 0 while true do while true do loc_1 = sub_i32(band_i32(load_i32(memory_at_0, loc_4 + 4), 4294967288 ), loc_2 )if loc_1 >= loc_3 then break end loc_3 = loc_1 loc_7 = loc_4 if loc_1 ~= 0 then break end loc_3 = 0 loc_7 = loc_4 param_0 = loc_4 desired = 16 break end if desired then if desired == 18 then desired = nil continue end break end loc_1 = load_i32(memory_at_0, add_i32(loc_4 , 20 ))loc_4 = load_i32(memory_at_0, add_i32(add_i32(loc_4 , band_i32(shr_u32(loc_6 , 29 ), 4 )), 16 ))param_0 = (loc_1 ~= 0 and (loc_1 == loc_4 and param_0 or loc_1 )or param_0 )loc_6 = shl_i32(loc_6 , 1 )if loc_4 ~= 0 then continue end break end if desired then if desired == 17 then desired = nil end break end break end if desired then if desired == 16 then desired = nil end break end while true do if bor_i32(param_0 , loc_7 )~= 0 then break end loc_7 = 0 param_0 = shl_i32(2 , loc_10 )param_0 = band_i32(bor_i32(param_0 , sub_i32(0 , param_0 )), loc_5 )if param_0 == 0 then desired = 14 break end param_0 = load_i32(memory_at_0, add_i32(shl_i32(ctz_i32(param_0 ), 2 ), 67392 ))break end if desired then if desired == 16 then desired = nil end break end if param_0 == 0 then desired = 15 break end break end if desired then if desired == 15 then desired = nil end break end while true do loc_1 = sub_i32(band_i32(load_i32(memory_at_0, param_0 + 4), 4294967288 ), loc_2 )loc_6 = (loc_1 < loc_3 and 1 or 0)while true do loc_4 = load_i32(memory_at_0, param_0 + 16)if loc_4 ~= 0 then break end loc_4 = load_i32(memory_at_0, add_i32(param_0 , 20 ))break end if desired then if desired == 16 then desired = nil continue end break end loc_3 = (loc_6 ~= 0 and loc_1 or loc_3 )loc_7 = (loc_6 ~= 0 and param_0 or loc_7 )param_0 = loc_4 if loc_4 ~= 0 then continue end break end if desired then if desired == 15 then desired = nil end break end break end if desired then if desired == 14 then desired = nil end break end if loc_7 == 0 then break end if loc_3 >= sub_i32(load_i32(memory_at_0, 0 + 67096), loc_2 )then break end loc_10 = load_i32(memory_at_0, loc_7 + 24)while true do loc_6 = load_i32(memory_at_0, loc_7 + 12)if loc_6 == loc_7 then break end param_0 = load_i32(memory_at_0, loc_7 + 8)store_i32(memory_at_0, param_0 + 12, loc_6 )store_i32(memory_at_0, loc_6 + 8, param_0 )desired = 3 break end if desired then if desired == 14 then desired = nil end break end while true do loc_4 = add_i32(loc_7 , 20 )param_0 = load_i32(memory_at_0, loc_4 )if param_0 ~= 0 then break end param_0 = load_i32(memory_at_0, loc_7 + 16)if param_0 == 0 then desired = 12 break end loc_4 = add_i32(loc_7 , 16 )break end if desired then if desired == 14 then desired = nil end break end while true do loc_1 = loc_4 loc_6 = param_0 loc_4 = add_i32(loc_6 , 20 )param_0 = load_i32(memory_at_0, loc_4 )if param_0 ~= 0 then continue end loc_4 = add_i32(loc_6 , 16 )param_0 = load_i32(memory_at_0, loc_6 + 16)if param_0 ~= 0 then continue end break end if desired then if desired == 14 then desired = nil end break end store_i32(memory_at_0, loc_1 , 0 )desired = 3 break end if desired then if desired == 13 then desired = nil end break end while true do param_0 = load_i32(memory_at_0, 0 + 67096)if param_0 < loc_2 then break end loc_3 = load_i32(memory_at_0, 0 + 67108)while true do while true do loc_4 = sub_i32(param_0 , loc_2 )if loc_4 < 16 then break end loc_6 = add_i32(loc_3 , loc_2 )store_i32(memory_at_0, loc_6 + 4, bor_i32(loc_4 , 1 ))store_i32(memory_at_0, add_i32(loc_3 , param_0 ), loc_4 )store_i32(memory_at_0, loc_3 + 4, bor_i32(loc_2 , 3 ))desired = 15 break end if desired then if desired == 15 then desired = nil end break end store_i32(memory_at_0, loc_3 + 4, bor_i32(param_0 , 3 ))param_0 = add_i32(loc_3 , param_0 )store_i32(memory_at_0, param_0 + 4, bor_i32(load_i32(memory_at_0, param_0 + 4), 1 ))loc_6 = 0 loc_4 = 0 break end if desired then if desired == 14 then desired = nil end break end store_i32(memory_at_0, 0 + 67096, loc_4 )store_i32(memory_at_0, 0 + 67108, loc_6 )param_0 = add_i32(loc_3 , 8 )desired = 1 break end if desired then if desired == 13 then desired = nil end break end while true do loc_6 = load_i32(memory_at_0, 0 + 67100)if loc_6 <= loc_2 then break end loc_3 = sub_i32(loc_6 , loc_2 )store_i32(memory_at_0, 0 + 67100, loc_3 )param_0 = load_i32(memory_at_0, 0 + 67112)loc_4 = add_i32(param_0 , loc_2 )store_i32(memory_at_0, 0 + 67112, loc_4 )store_i32(memory_at_0, loc_4 + 4, bor_i32(loc_3 , 1 ))store_i32(memory_at_0, param_0 + 4, bor_i32(loc_2 , 3 ))param_0 = add_i32(param_0 , 8 )desired = 1 break end if desired then if desired == 13 then desired = nil end break end while true do while true do if load_i32(memory_at_0, 0 + 67560)== 0 then break end loc_3 = load_i32(memory_at_0, 0 + 67568)desired = 14 break end if desired then if desired == 14 then desired = nil end break end store_i64(memory_at_0, 0 + 67572, i64_from_u32(4294967295, 4294967295) )store_i64(memory_at_0, 0 + 67564, i64_from_u32(4096, 4096) )store_i32(memory_at_0, 0 + 67560, bxor_i32(band_i32(add_i32(loc_0 , 12 ), 4294967280 ), 1431655768 ))store_i32(memory_at_0, 0 + 67580, 0 )store_i32(memory_at_0, 0 + 67532, 0 )loc_3 = 4096 break end if desired then if desired == 13 then desired = nil end break end param_0 = 0 loc_5 = add_i32(loc_2 , 47 )loc_1 = add_i32(loc_3 , loc_5 )loc_10 = sub_i32(0 , loc_3 )loc_7 = band_i32(loc_1 , loc_10 )if loc_7 <= loc_2 then desired = 1 break end param_0 = 0 while true do loc_3 = load_i32(memory_at_0, 0 + 67528)if loc_3 == 0 then break end loc_4 = load_i32(memory_at_0, 0 + 67520)loc_9 = add_i32(loc_4 , loc_7 )if loc_9 <= loc_4 then desired = 1 break end if loc_9 > loc_3 then desired = 1 break end break end if desired then if desired == 13 then desired = nil end break end while true do while true do if band_i32(load_i32_u8(memory_at_0, 0 + 67532), 4 )~= 0 then break end while true do while true do while true do while true do while true do loc_3 = load_i32(memory_at_0, 0 + 67112)if loc_3 == 0 then break end param_0 = 67536 while true do while true do loc_4 = load_i32(memory_at_0, param_0 )if loc_4 > loc_3 then break end if add_i32(loc_4 , load_i32(memory_at_0, param_0 + 4))> loc_3 then desired = 19 break end break end if desired then if desired == 21 then desired = nil continue end break end param_0 = load_i32(memory_at_0, param_0 + 8)if param_0 ~= 0 then continue end break end if desired then if desired == 20 then desired = nil end break end break end if desired then if desired == 19 then desired = nil end break end reg_0 = FUNC_LIST[92](0 )loc_6 = reg_0 if loc_6 == 4294967295 then desired = 16 break end loc_1 = loc_7 while true do param_0 = load_i32(memory_at_0, 0 + 67564)loc_3 = add_i32(param_0 , 4294967295 )if band_i32(loc_3 , loc_6 )== 0 then break end loc_1 = add_i32(sub_i32(loc_7 , loc_6 ), band_i32(add_i32(loc_3 , loc_6 ), sub_i32(0 , param_0 )))break end if desired then if desired == 19 then desired = nil end break end if loc_1 <= loc_2 then desired = 16 break end while true do param_0 = load_i32(memory_at_0, 0 + 67528)if param_0 == 0 then break end loc_3 = load_i32(memory_at_0, 0 + 67520)loc_4 = add_i32(loc_3 , loc_1 )if loc_4 <= loc_3 then desired = 16 break end if loc_4 > param_0 then desired = 16 break end break end if desired then if desired == 19 then desired = nil end break end reg_0 = FUNC_LIST[92](loc_1 )param_0 = reg_0 if param_0 ~= loc_6 then desired = 18 break end desired = 14 break end if desired then if desired == 18 then desired = nil end break end loc_1 = band_i32(sub_i32(loc_1 , loc_6 ), loc_10 )reg_0 = FUNC_LIST[92](loc_1 )loc_6 = reg_0 if loc_6 == add_i32(load_i32(memory_at_0, param_0 ), load_i32(memory_at_0, param_0 + 4))then desired = 17 break end param_0 = loc_6 break end if desired then if desired == 17 then desired = nil end break end if param_0 == 4294967295 then desired = 16 break end while true do if loc_1 < add_i32(loc_2 , 48 )then break end loc_6 = param_0 desired = 14 break end if desired then if desired == 17 then desired = nil end break end loc_3 = load_i32(memory_at_0, 0 + 67568)loc_3 = band_i32(add_i32(sub_i32(loc_5 , loc_1 ), loc_3 ), sub_i32(0 , loc_3 ))reg_0 = FUNC_LIST[92](loc_3 )if reg_0 == 4294967295 then desired = 16 break end loc_1 = add_i32(loc_3 , loc_1 )loc_6 = param_0 desired = 14 break end if desired then if desired == 16 then desired = nil end break end if loc_6 ~= 4294967295 then desired = 14 break end break end if desired then if desired == 15 then desired = nil end break end store_i32(memory_at_0, 0 + 67532, bor_i32(load_i32(memory_at_0, 0 + 67532), 4 ))break end if desired then if desired == 14 then desired = nil end break end reg_0 = FUNC_LIST[92](loc_7 )loc_6 = reg_0 reg_0 = FUNC_LIST[92](0 )param_0 = reg_0 if loc_6 == 4294967295 then desired = 9 break end if param_0 == 4294967295 then desired = 9 break end if loc_6 >= param_0 then desired = 9 break end loc_1 = sub_i32(param_0 , loc_6 )if loc_1 <= add_i32(loc_2 , 40 )then desired = 9 break end break end if desired then if desired == 13 then desired = nil end break end param_0 = add_i32(load_i32(memory_at_0, 0 + 67520), loc_1 )store_i32(memory_at_0, 0 + 67520, param_0 )while true do if param_0 <= load_i32(memory_at_0, 0 + 67524)then break end store_i32(memory_at_0, 0 + 67524, param_0 )break end if desired then if desired == 13 then desired = nil end break end while true do while true do loc_3 = load_i32(memory_at_0, 0 + 67112)if loc_3 == 0 then break end param_0 = 67536 while true do loc_4 = load_i32(memory_at_0, param_0 )loc_7 = load_i32(memory_at_0, param_0 + 4)if loc_6 == add_i32(loc_4 , loc_7 )then desired = 14 break end param_0 = load_i32(memory_at_0, param_0 + 8)if param_0 ~= 0 then continue end desired = 11 break end if desired then if desired == 15 then desired = nil end break end error("out of code bounds")end if desired then if desired == 14 then desired = nil end break end while true do while true do param_0 = load_i32(memory_at_0, 0 + 67104)if param_0 == 0 then break end if loc_6 >= param_0 then desired = 15 break end break end if desired then if desired == 15 then desired = nil end break end store_i32(memory_at_0, 0 + 67104, loc_6 )break end if desired then if desired == 14 then desired = nil end break end param_0 = 0 store_i32(memory_at_0, 0 + 67540, loc_1 )store_i32(memory_at_0, 0 + 67536, loc_6 )store_i32(memory_at_0, 0 + 67120, 4294967295 )store_i32(memory_at_0, 0 + 67124, load_i32(memory_at_0, 0 + 67560))store_i32(memory_at_0, 0 + 67548, 0 )while true do loc_3 = shl_i32(param_0 , 3 )loc_4 = add_i32(loc_3 , 67128 )store_i32(memory_at_0, add_i32(loc_3 , 67136 ), loc_4 )store_i32(memory_at_0, add_i32(loc_3 , 67140 ), loc_4 )param_0 = add_i32(param_0 , 1 )if param_0 ~= 32 then continue end break end if desired then if desired == 14 then desired = nil end break end param_0 = add_i32(loc_1 , 4294967256 )loc_3 = band_i32(sub_i32(4294967288 , loc_6 ), 7 )loc_4 = sub_i32(param_0 , loc_3 )store_i32(memory_at_0, 0 + 67100, loc_4 )loc_3 = add_i32(loc_6 , loc_3 )store_i32(memory_at_0, 0 + 67112, loc_3 )store_i32(memory_at_0, loc_3 + 4, bor_i32(loc_4 , 1 ))store_i32(memory_at_0, add_i32(loc_6 , param_0 )+ 4, 40 )store_i32(memory_at_0, 0 + 67116, load_i32(memory_at_0, 0 + 67576))desired = 10 break end if desired then if desired == 13 then desired = nil end break end if loc_3 >= loc_6 then desired = 11 break end if loc_3 < loc_4 then desired = 11 break end if band_i32(load_i32(memory_at_0, param_0 + 12), 8 )~= 0 then desired = 11 break end store_i32(memory_at_0, param_0 + 4, add_i32(loc_7 , loc_1 ))param_0 = band_i32(sub_i32(4294967288 , loc_3 ), 7 )loc_4 = add_i32(loc_3 , param_0 )store_i32(memory_at_0, 0 + 67112, loc_4 )loc_6 = add_i32(load_i32(memory_at_0, 0 + 67100), loc_1 )param_0 = sub_i32(loc_6 , param_0 )store_i32(memory_at_0, 0 + 67100, param_0 )store_i32(memory_at_0, loc_4 + 4, bor_i32(param_0 , 1 ))store_i32(memory_at_0, add_i32(loc_3 , loc_6 )+ 4, 40 )store_i32(memory_at_0, 0 + 67116, load_i32(memory_at_0, 0 + 67576))desired = 10 break end if desired then if desired == 12 then desired = nil end break end loc_7 = 0 desired = 2 break end if desired then if desired == 11 then desired = nil end break end loc_6 = 0 desired = 3 break end if desired then if desired == 10 then desired = nil end break end while true do loc_7 = load_i32(memory_at_0, 0 + 67104)if loc_6 >= loc_7 then break end store_i32(memory_at_0, 0 + 67104, loc_6 )loc_7 = loc_6 break end if desired then if desired == 10 then desired = nil end break end loc_4 = add_i32(loc_6 , loc_1 )param_0 = 67536 while true do while true do while true do while true do while true do if load_i32(memory_at_0, param_0 )== loc_4 then desired = 14 break end param_0 = load_i32(memory_at_0, param_0 + 8)if param_0 ~= 0 then continue end desired = 13 break end if desired then if desired == 14 then desired = nil end break end error("out of code bounds")end if desired then if desired == 13 then desired = nil end break end if band_i32(load_i32_u8(memory_at_0, param_0 + 12), 8 )== 0 then desired = 12 break end break end if desired then if desired == 12 then desired = nil end break end param_0 = 67536 while true do while true do loc_4 = load_i32(memory_at_0, param_0 )if loc_4 > loc_3 then break end loc_4 = add_i32(loc_4 , load_i32(memory_at_0, param_0 + 4))if loc_4 > loc_3 then desired = 11 break end break end if desired then if desired == 13 then desired = nil continue end break end param_0 = load_i32(memory_at_0, param_0 + 8)continue end if desired then if desired == 12 then desired = nil end break end error("out of code bounds")end if desired then if desired == 11 then desired = nil end break end store_i32(memory_at_0, param_0 , loc_6 )store_i32(memory_at_0, param_0 + 4, add_i32(load_i32(memory_at_0, param_0 + 4), loc_1 ))loc_10 = add_i32(loc_6 , band_i32(sub_i32(4294967288 , loc_6 ), 7 ))store_i32(memory_at_0, loc_10 + 4, bor_i32(loc_2 , 3 ))loc_1 = add_i32(loc_4 , band_i32(sub_i32(4294967288 , loc_4 ), 7 ))loc_2 = add_i32(loc_10 , loc_2 )param_0 = sub_i32(loc_1 , loc_2 )while true do if loc_1 ~= loc_3 then break end store_i32(memory_at_0, 0 + 67112, loc_2 )param_0 = add_i32(load_i32(memory_at_0, 0 + 67100), param_0 )store_i32(memory_at_0, 0 + 67100, param_0 )store_i32(memory_at_0, loc_2 + 4, bor_i32(param_0 , 1 ))desired = 4 break end if desired then if desired == 11 then desired = nil end break end while true do if loc_1 ~= load_i32(memory_at_0, 0 + 67108)then break end store_i32(memory_at_0, 0 + 67108, loc_2 )param_0 = add_i32(load_i32(memory_at_0, 0 + 67096), param_0 )store_i32(memory_at_0, 0 + 67096, param_0 )store_i32(memory_at_0, loc_2 + 4, bor_i32(param_0 , 1 ))store_i32(memory_at_0, add_i32(loc_2 , param_0 ), param_0 )desired = 4 break end if desired then if desired == 11 then desired = nil end break end loc_3 = load_i32(memory_at_0, loc_1 + 4)if band_i32(loc_3 , 3 )~= 1 then desired = 5 break end loc_5 = band_i32(loc_3 , 4294967288 )while true do if loc_3 > 255 then break end loc_4 = load_i32(memory_at_0, loc_1 + 8)loc_7 = shr_u32(loc_3 , 3 )loc_6 = add_i32(shl_i32(loc_7 , 3 ), 67128 )while true do loc_3 = load_i32(memory_at_0, loc_1 + 12)if loc_3 ~= loc_4 then break end store_i32(memory_at_0, 0 + 67088, band_i32(load_i32(memory_at_0, 0 + 67088), rotl_i32(4294967294 , loc_7 )))desired = 6 break end if desired then if desired == 12 then desired = nil end break end store_i32(memory_at_0, loc_4 + 12, loc_3 )store_i32(memory_at_0, loc_3 + 8, loc_4 )desired = 6 break end if desired then if desired == 11 then desired = nil end break end loc_9 = load_i32(memory_at_0, loc_1 + 24)while true do loc_6 = load_i32(memory_at_0, loc_1 + 12)if loc_6 == loc_1 then break end loc_3 = load_i32(memory_at_0, loc_1 + 8)store_i32(memory_at_0, loc_3 + 12, loc_6 )store_i32(memory_at_0, loc_6 + 8, loc_3 )desired = 7 break end if desired then if desired == 11 then desired = nil end break end while true do loc_4 = add_i32(loc_1 , 20 )loc_3 = load_i32(memory_at_0, loc_4 )if loc_3 ~= 0 then break end loc_3 = load_i32(memory_at_0, loc_1 + 16)if loc_3 == 0 then desired = 8 break end loc_4 = add_i32(loc_1 , 16 )break end if desired then if desired == 11 then desired = nil end break end while true do loc_7 = loc_4 loc_6 = loc_3 loc_4 = add_i32(loc_6 , 20 )loc_3 = load_i32(memory_at_0, loc_4 )if loc_3 ~= 0 then continue end loc_4 = add_i32(loc_6 , 16 )loc_3 = load_i32(memory_at_0, loc_6 + 16)if loc_3 ~= 0 then continue end break end if desired then if desired == 11 then desired = nil end break end store_i32(memory_at_0, loc_7 , 0 )desired = 7 break end if desired then if desired == 10 then desired = nil end break end param_0 = add_i32(loc_1 , 4294967256 )loc_7 = band_i32(sub_i32(4294967288 , loc_6 ), 7 )loc_10 = sub_i32(param_0 , loc_7 )store_i32(memory_at_0, 0 + 67100, loc_10 )loc_7 = add_i32(loc_6 , loc_7 )store_i32(memory_at_0, 0 + 67112, loc_7 )store_i32(memory_at_0, loc_7 + 4, bor_i32(loc_10 , 1 ))store_i32(memory_at_0, add_i32(loc_6 , param_0 )+ 4, 40 )store_i32(memory_at_0, 0 + 67116, load_i32(memory_at_0, 0 + 67576))param_0 = add_i32(add_i32(loc_4 , band_i32(sub_i32(39 , loc_4 ), 7 )), 4294967249 )loc_7 = (param_0 < add_i32(loc_3 , 16 )and loc_3 or param_0 )store_i32(memory_at_0, loc_7 + 4, 27 )store_i64(memory_at_0, add_i32(loc_7 , 16 ), load_i64(memory_at_0, 0 + 67544))store_i64(memory_at_0, loc_7 + 8, load_i64(memory_at_0, 0 + 67536))store_i32(memory_at_0, 0 + 67544, add_i32(loc_7 , 8 ))store_i32(memory_at_0, 0 + 67540, loc_1 )store_i32(memory_at_0, 0 + 67536, loc_6 )store_i32(memory_at_0, 0 + 67548, 0 )param_0 = add_i32(loc_7 , 24 )while true do store_i32(memory_at_0, param_0 + 4, 7 )loc_6 = add_i32(param_0 , 8 )param_0 = add_i32(param_0 , 4 )if loc_6 < loc_4 then continue end break end if desired then if desired == 10 then desired = nil end break end if loc_7 == loc_3 then break end store_i32(memory_at_0, loc_7 + 4, band_i32(load_i32(memory_at_0, loc_7 + 4), 4294967294 ))loc_6 = sub_i32(loc_7 , loc_3 )store_i32(memory_at_0, loc_3 + 4, bor_i32(loc_6 , 1 ))store_i32(memory_at_0, loc_7 , loc_6 )while true do if loc_6 > 255 then break end param_0 = add_i32(band_i32(loc_6 , 4294967288 ), 67128 )while true do while true do loc_4 = load_i32(memory_at_0, 0 + 67088)loc_6 = shl_i32(1 , shr_u32(loc_6 , 3 ))if band_i32(loc_4 , loc_6 )~= 0 then break end store_i32(memory_at_0, 0 + 67088, bor_i32(loc_4 , loc_6 ))loc_4 = param_0 desired = 12 break end if desired then if desired == 12 then desired = nil end break end loc_4 = load_i32(memory_at_0, param_0 + 8)break end if desired then if desired == 11 then desired = nil end break end store_i32(memory_at_0, param_0 + 8, loc_3 )store_i32(memory_at_0, loc_4 + 12, loc_3 )store_i32(memory_at_0, loc_3 + 12, param_0 )store_i32(memory_at_0, loc_3 + 8, loc_4 )desired = 10 break end if desired then if desired == 10 then desired = nil end break end param_0 = 31 while true do if loc_6 > 16777215 then break end param_0 = clz_i32(shr_u32(loc_6 , 8 ))param_0 = add_i32(sub_i32(band_i32(shr_u32(loc_6 , sub_i32(38 , param_0 )), 1 ), shl_i32(param_0 , 1 )), 62 )break end if desired then if desired == 10 then desired = nil end break end store_i32(memory_at_0, loc_3 + 28, param_0 )store_i64(memory_at_0, loc_3 + 16, i64_ZERO )loc_4 = add_i32(shl_i32(param_0 , 2 ), 67392 )while true do while true do while true do loc_7 = load_i32(memory_at_0, 0 + 67092)loc_1 = shl_i32(1 , param_0 )if band_i32(loc_7 , loc_1 )~= 0 then break end store_i32(memory_at_0, 0 + 67092, bor_i32(loc_7 , loc_1 ))store_i32(memory_at_0, loc_4 , loc_3 )store_i32(memory_at_0, loc_3 + 24, loc_4 )desired = 12 break end if desired then if desired == 12 then desired = nil end break end param_0 = shl_i32(loc_6 , (param_0 == 31 and 0 or sub_i32(25 , shr_u32(param_0 , 1 ))))loc_7 = load_i32(memory_at_0, loc_4 )while true do loc_4 = loc_7 if band_i32(load_i32(memory_at_0, loc_4 + 4), 4294967288 )== loc_6 then desired = 11 break end loc_7 = shr_u32(param_0 , 29 )param_0 = shl_i32(param_0 , 1 )loc_1 = add_i32(add_i32(loc_4 , band_i32(loc_7 , 4 )), 16 )loc_7 = load_i32(memory_at_0, loc_1 )if loc_7 ~= 0 then continue end break end if desired then if desired == 12 then desired = nil end break end store_i32(memory_at_0, loc_1 , loc_3 )store_i32(memory_at_0, loc_3 + 24, loc_4 )break end if desired then if desired == 11 then desired = nil end break end store_i32(memory_at_0, loc_3 + 12, loc_3 )store_i32(memory_at_0, loc_3 + 8, loc_3 )desired = 10 break end if desired then if desired == 10 then desired = nil end break end param_0 = load_i32(memory_at_0, loc_4 + 8)store_i32(memory_at_0, param_0 + 12, loc_3 )store_i32(memory_at_0, loc_4 + 8, loc_3 )store_i32(memory_at_0, loc_3 + 24, 0 )store_i32(memory_at_0, loc_3 + 12, loc_4 )store_i32(memory_at_0, loc_3 + 8, param_0 )break end if desired then if desired == 9 then desired = nil end break end param_0 = load_i32(memory_at_0, 0 + 67100)if param_0 <= loc_2 then break end loc_3 = sub_i32(param_0 , loc_2 )store_i32(memory_at_0, 0 + 67100, loc_3 )param_0 = load_i32(memory_at_0, 0 + 67112)loc_4 = add_i32(param_0 , loc_2 )store_i32(memory_at_0, 0 + 67112, loc_4 )store_i32(memory_at_0, loc_4 + 4, bor_i32(loc_3 , 1 ))store_i32(memory_at_0, param_0 + 4, bor_i32(loc_2 , 3 ))param_0 = add_i32(param_0 , 8 )desired = 1 break end if desired then if desired == 8 then desired = nil end break end reg_0 = FUNC_LIST[91]()store_i32(memory_at_0, reg_0 , 48 )param_0 = 0 desired = 1 break end if desired then if desired == 7 then desired = nil end break end loc_6 = 0 break end if desired then if desired == 6 then desired = nil end break end if loc_9 == 0 then break end while true do while true do loc_4 = load_i32(memory_at_0, loc_1 + 28)loc_3 = add_i32(shl_i32(loc_4 , 2 ), 67392 )if loc_1 ~= load_i32(memory_at_0, loc_3 )then break end store_i32(memory_at_0, loc_3 , loc_6 )if loc_6 ~= 0 then desired = 7 break end store_i32(memory_at_0, 0 + 67092, band_i32(load_i32(memory_at_0, 0 + 67092), rotl_i32(4294967294 , loc_4 )))desired = 6 break end if desired then if desired == 7 then desired = nil end break end store_i32(memory_at_0, add_i32(loc_9 , (load_i32(memory_at_0, loc_9 + 16)== loc_1 and 16 or 20 )), loc_6 )if loc_6 == 0 then desired = 6 break end break end if desired then if desired == 6 then desired = nil end break end store_i32(memory_at_0, loc_6 + 24, loc_9 )while true do loc_3 = load_i32(memory_at_0, loc_1 + 16)if loc_3 == 0 then break end store_i32(memory_at_0, loc_6 + 16, loc_3 )store_i32(memory_at_0, loc_3 + 24, loc_6 )break end if desired then if desired == 6 then desired = nil end break end loc_3 = load_i32(memory_at_0, add_i32(loc_1 , 20 ))if loc_3 == 0 then break end store_i32(memory_at_0, add_i32(loc_6 , 20 ), loc_3 )store_i32(memory_at_0, loc_3 + 24, loc_6 )break end if desired then if desired == 5 then desired = nil end break end param_0 = add_i32(loc_5 , param_0 )loc_1 = add_i32(loc_1 , loc_5 )loc_3 = load_i32(memory_at_0, loc_1 + 4)break end if desired then if desired == 4 then desired = nil end break end store_i32(memory_at_0, loc_1 + 4, band_i32(loc_3 , 4294967294 ))store_i32(memory_at_0, loc_2 + 4, bor_i32(param_0 , 1 ))store_i32(memory_at_0, add_i32(loc_2 , param_0 ), param_0 )while true do if param_0 > 255 then break end loc_3 = add_i32(band_i32(param_0 , 4294967288 ), 67128 )while true do while true do loc_4 = load_i32(memory_at_0, 0 + 67088)param_0 = shl_i32(1 , shr_u32(param_0 , 3 ))if band_i32(loc_4 , param_0 )~= 0 then break end store_i32(memory_at_0, 0 + 67088, bor_i32(loc_4 , param_0 ))param_0 = loc_3 desired = 6 break end if desired then if desired == 6 then desired = nil end break end param_0 = load_i32(memory_at_0, loc_3 + 8)break end if desired then if desired == 5 then desired = nil end break end store_i32(memory_at_0, loc_3 + 8, loc_2 )store_i32(memory_at_0, param_0 + 12, loc_2 )store_i32(memory_at_0, loc_2 + 12, loc_3 )store_i32(memory_at_0, loc_2 + 8, param_0 )desired = 4 break end if desired then if desired == 4 then desired = nil end break end loc_3 = 31 while true do if param_0 > 16777215 then break end loc_3 = clz_i32(shr_u32(param_0 , 8 ))loc_3 = add_i32(sub_i32(band_i32(shr_u32(param_0 , sub_i32(38 , loc_3 )), 1 ), shl_i32(loc_3 , 1 )), 62 )break end if desired then if desired == 4 then desired = nil end break end store_i32(memory_at_0, loc_2 + 28, loc_3 )store_i64(memory_at_0, loc_2 + 16, i64_ZERO )loc_4 = add_i32(shl_i32(loc_3 , 2 ), 67392 )while true do while true do while true do loc_6 = load_i32(memory_at_0, 0 + 67092)loc_7 = shl_i32(1 , loc_3 )if band_i32(loc_6 , loc_7 )~= 0 then break end store_i32(memory_at_0, 0 + 67092, bor_i32(loc_6 , loc_7 ))store_i32(memory_at_0, loc_4 , loc_2 )store_i32(memory_at_0, loc_2 + 24, loc_4 )desired = 6 break end if desired then if desired == 6 then desired = nil end break end loc_3 = shl_i32(param_0 , (loc_3 == 31 and 0 or sub_i32(25 , shr_u32(loc_3 , 1 ))))loc_6 = load_i32(memory_at_0, loc_4 )while true do loc_4 = loc_6 if band_i32(load_i32(memory_at_0, loc_4 + 4), 4294967288 )== param_0 then desired = 5 break end loc_6 = shr_u32(loc_3 , 29 )loc_3 = shl_i32(loc_3 , 1 )loc_7 = add_i32(add_i32(loc_4 , band_i32(loc_6 , 4 )), 16 )loc_6 = load_i32(memory_at_0, loc_7 )if loc_6 ~= 0 then continue end break end if desired then if desired == 6 then desired = nil end break end store_i32(memory_at_0, loc_7 , loc_2 )store_i32(memory_at_0, loc_2 + 24, loc_4 )break end if desired then if desired == 5 then desired = nil end break end store_i32(memory_at_0, loc_2 + 12, loc_2 )store_i32(memory_at_0, loc_2 + 8, loc_2 )desired = 4 break end if desired then if desired == 4 then desired = nil end break end param_0 = load_i32(memory_at_0, loc_4 + 8)store_i32(memory_at_0, param_0 + 12, loc_2 )store_i32(memory_at_0, loc_4 + 8, loc_2 )store_i32(memory_at_0, loc_2 + 24, 0 )store_i32(memory_at_0, loc_2 + 12, loc_4 )store_i32(memory_at_0, loc_2 + 8, param_0 )break end if desired then if desired == 3 then desired = nil end break end param_0 = add_i32(loc_10 , 8 )desired = 1 break end if desired then if desired == 2 then desired = nil end break end while true do if loc_10 == 0 then break end while true do while true do loc_4 = load_i32(memory_at_0, loc_7 + 28)param_0 = add_i32(shl_i32(loc_4 , 2 ), 67392 )if loc_7 ~= load_i32(memory_at_0, param_0 )then break end store_i32(memory_at_0, param_0 , loc_6 )if loc_6 ~= 0 then desired = 4 break end loc_5 = band_i32(loc_5 , rotl_i32(4294967294 , loc_4 ))store_i32(memory_at_0, 0 + 67092, loc_5 )desired = 3 break end if desired then if desired == 4 then desired = nil end break end store_i32(memory_at_0, add_i32(loc_10 , (load_i32(memory_at_0, loc_10 + 16)== loc_7 and 16 or 20 )), loc_6 )if loc_6 == 0 then desired = 3 break end break end if desired then if desired == 3 then desired = nil end break end store_i32(memory_at_0, loc_6 + 24, loc_10 )while true do param_0 = load_i32(memory_at_0, loc_7 + 16)if param_0 == 0 then break end store_i32(memory_at_0, loc_6 + 16, param_0 )store_i32(memory_at_0, param_0 + 24, loc_6 )break end if desired then if desired == 3 then desired = nil end break end param_0 = load_i32(memory_at_0, add_i32(loc_7 , 20 ))if param_0 == 0 then break end store_i32(memory_at_0, add_i32(loc_6 , 20 ), param_0 )store_i32(memory_at_0, param_0 + 24, loc_6 )break end if desired then if desired == 2 then desired = nil end break end while true do while true do if loc_3 > 15 then break end param_0 = add_i32(loc_3 , loc_2 )store_i32(memory_at_0, loc_7 + 4, bor_i32(param_0 , 3 ))param_0 = add_i32(loc_7 , param_0 )store_i32(memory_at_0, param_0 + 4, bor_i32(load_i32(memory_at_0, param_0 + 4), 1 ))desired = 3 break end if desired then if desired == 3 then desired = nil end break end store_i32(memory_at_0, loc_7 + 4, bor_i32(loc_2 , 3 ))loc_6 = add_i32(loc_7 , loc_2 )store_i32(memory_at_0, loc_6 + 4, bor_i32(loc_3 , 1 ))store_i32(memory_at_0, add_i32(loc_6 , loc_3 ), loc_3 )while true do if loc_3 > 255 then break end param_0 = add_i32(band_i32(loc_3 , 4294967288 ), 67128 )while true do while true do loc_4 = load_i32(memory_at_0, 0 + 67088)loc_3 = shl_i32(1 , shr_u32(loc_3 , 3 ))if band_i32(loc_4 , loc_3 )~= 0 then break end store_i32(memory_at_0, 0 + 67088, bor_i32(loc_4 , loc_3 ))loc_3 = param_0 desired = 5 break end if desired then if desired == 5 then desired = nil end break end loc_3 = load_i32(memory_at_0, param_0 + 8)break end if desired then if desired == 4 then desired = nil end break end store_i32(memory_at_0, param_0 + 8, loc_6 )store_i32(memory_at_0, loc_3 + 12, loc_6 )store_i32(memory_at_0, loc_6 + 12, param_0 )store_i32(memory_at_0, loc_6 + 8, loc_3 )desired = 3 break end if desired then if desired == 3 then desired = nil end break end param_0 = 31 while true do if loc_3 > 16777215 then break end param_0 = clz_i32(shr_u32(loc_3 , 8 ))param_0 = add_i32(sub_i32(band_i32(shr_u32(loc_3 , sub_i32(38 , param_0 )), 1 ), shl_i32(param_0 , 1 )), 62 )break end if desired then if desired == 3 then desired = nil end break end store_i32(memory_at_0, loc_6 + 28, param_0 )store_i64(memory_at_0, loc_6 + 16, i64_ZERO )loc_4 = add_i32(shl_i32(param_0 , 2 ), 67392 )while true do while true do while true do loc_2 = shl_i32(1 , param_0 )if band_i32(loc_5 , loc_2 )~= 0 then break end store_i32(memory_at_0, 0 + 67092, bor_i32(loc_5 , loc_2 ))store_i32(memory_at_0, loc_4 , loc_6 )store_i32(memory_at_0, loc_6 + 24, loc_4 )desired = 5 break end if desired then if desired == 5 then desired = nil end break end param_0 = shl_i32(loc_3 , (param_0 == 31 and 0 or sub_i32(25 , shr_u32(param_0 , 1 ))))loc_2 = load_i32(memory_at_0, loc_4 )while true do loc_4 = loc_2 if band_i32(load_i32(memory_at_0, loc_4 + 4), 4294967288 )== loc_3 then desired = 4 break end loc_2 = shr_u32(param_0 , 29 )param_0 = shl_i32(param_0 , 1 )loc_1 = add_i32(add_i32(loc_4 , band_i32(loc_2 , 4 )), 16 )loc_2 = load_i32(memory_at_0, loc_1 )if loc_2 ~= 0 then continue end break end if desired then if desired == 5 then desired = nil end break end store_i32(memory_at_0, loc_1 , loc_6 )store_i32(memory_at_0, loc_6 + 24, loc_4 )break end if desired then if desired == 4 then desired = nil end break end store_i32(memory_at_0, loc_6 + 12, loc_6 )store_i32(memory_at_0, loc_6 + 8, loc_6 )desired = 3 break end if desired then if desired == 3 then desired = nil end break end param_0 = load_i32(memory_at_0, loc_4 + 8)store_i32(memory_at_0, param_0 + 12, loc_6 )store_i32(memory_at_0, loc_4 + 8, loc_6 )store_i32(memory_at_0, loc_6 + 24, 0 )store_i32(memory_at_0, loc_6 + 12, loc_4 )store_i32(memory_at_0, loc_6 + 8, param_0 )break end if desired then if desired == 2 then desired = nil end break end param_0 = add_i32(loc_7 , 8 )desired = 1 break end if desired then if desired == 1 then desired = nil end break end while true do if loc_9 == 0 then break end while true do while true do loc_4 = load_i32(memory_at_0, loc_6 + 28)param_0 = add_i32(shl_i32(loc_4 , 2 ), 67392 )if loc_6 ~= load_i32(memory_at_0, param_0 )then break end store_i32(memory_at_0, param_0 , loc_7 )if loc_7 ~= 0 then desired = 3 break end store_i32(memory_at_0, 0 + 67092, band_i32(loc_8 , rotl_i32(4294967294 , loc_4 )))desired = 2 break end if desired then if desired == 3 then desired = nil end break end store_i32(memory_at_0, add_i32(loc_9 , (load_i32(memory_at_0, loc_9 + 16)== loc_6 and 16 or 20 )), loc_7 )if loc_7 == 0 then desired = 2 break end break end if desired then if desired == 2 then desired = nil end break end store_i32(memory_at_0, loc_7 + 24, loc_9 )while true do param_0 = load_i32(memory_at_0, loc_6 + 16)if param_0 == 0 then break end store_i32(memory_at_0, loc_7 + 16, param_0 )store_i32(memory_at_0, param_0 + 24, loc_7 )break end if desired then if desired == 2 then desired = nil end break end param_0 = load_i32(memory_at_0, add_i32(loc_6 , 20 ))if param_0 == 0 then break end store_i32(memory_at_0, add_i32(loc_7 , 20 ), param_0 )store_i32(memory_at_0, param_0 + 24, loc_7 )break end if desired then if desired == 1 then desired = nil end break end while true do while true do if loc_3 > 15 then break end param_0 = add_i32(loc_3 , loc_2 )store_i32(memory_at_0, loc_6 + 4, bor_i32(param_0 , 3 ))param_0 = add_i32(loc_6 , param_0 )store_i32(memory_at_0, param_0 + 4, bor_i32(load_i32(memory_at_0, param_0 + 4), 1 ))desired = 2 break end if desired then if desired == 2 then desired = nil end break end store_i32(memory_at_0, loc_6 + 4, bor_i32(loc_2 , 3 ))loc_4 = add_i32(loc_6 , loc_2 )store_i32(memory_at_0, loc_4 + 4, bor_i32(loc_3 , 1 ))store_i32(memory_at_0, add_i32(loc_4 , loc_3 ), loc_3 )while true do if loc_5 == 0 then break end loc_2 = add_i32(band_i32(loc_5 , 4294967288 ), 67128 )param_0 = load_i32(memory_at_0, 0 + 67108)while true do while true do loc_7 = shl_i32(1 , shr_u32(loc_5 , 3 ))if band_i32(loc_7 , loc_1 )~= 0 then break end store_i32(memory_at_0, 0 + 67088, bor_i32(loc_7 , loc_1 ))loc_7 = loc_2 desired = 4 break end if desired then if desired == 4 then desired = nil end break end loc_7 = load_i32(memory_at_0, loc_2 + 8)break end if desired then if desired == 3 then desired = nil end break end store_i32(memory_at_0, loc_2 + 8, param_0 )store_i32(memory_at_0, loc_7 + 12, param_0 )store_i32(memory_at_0, param_0 + 12, loc_2 )store_i32(memory_at_0, param_0 + 8, loc_7 )break end if desired then if desired == 2 then desired = nil end break end store_i32(memory_at_0, 0 + 67108, loc_4 )store_i32(memory_at_0, 0 + 67096, loc_3 )break end if desired then if desired == 1 then desired = nil end break end param_0 = add_i32(loc_6 , 8 )break end GLOBAL_LIST[0].value = add_i32(loc_0 , 16 )reg_0 = param_0 break end return reg_0 end FUNC_LIST[94] =--[[ dlfree ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local desired while true do while true do if param_0 == 0 then break end loc_0 = add_i32(param_0 , 4294967288 )loc_1 = load_i32(memory_at_0, add_i32(param_0 , 4294967292 ))param_0 = band_i32(loc_1 , 4294967288 )loc_2 = add_i32(loc_0 , param_0 )while true do if band_i32(loc_1 , 1 )~= 0 then break end if band_i32(loc_1 , 3 )== 0 then desired = 1 break end loc_1 = load_i32(memory_at_0, loc_0 )loc_0 = sub_i32(loc_0 , loc_1 )loc_3 = load_i32(memory_at_0, 0 + 67104)if loc_0 < loc_3 then desired = 1 break end param_0 = add_i32(loc_1 , param_0 )while true do while true do while true do if loc_0 == load_i32(memory_at_0, 0 + 67108)then break end while true do if loc_1 > 255 then break end loc_3 = load_i32(memory_at_0, loc_0 + 8)loc_4 = shr_u32(loc_1 , 3 )loc_5 = add_i32(shl_i32(loc_4 , 3 ), 67128 )while true do loc_1 = load_i32(memory_at_0, loc_0 + 12)if loc_1 ~= loc_3 then break end store_i32(memory_at_0, 0 + 67088, band_i32(load_i32(memory_at_0, 0 + 67088), rotl_i32(4294967294 , loc_4 )))desired = 2 break end if desired then if desired == 6 then desired = nil end break end store_i32(memory_at_0, loc_3 + 12, loc_1 )store_i32(memory_at_0, loc_1 + 8, loc_3 )desired = 2 break end if desired then if desired == 5 then desired = nil end break end loc_6 = load_i32(memory_at_0, loc_0 + 24)while true do loc_5 = load_i32(memory_at_0, loc_0 + 12)if loc_5 == loc_0 then break end loc_1 = load_i32(memory_at_0, loc_0 + 8)store_i32(memory_at_0, loc_1 + 12, loc_5 )store_i32(memory_at_0, loc_5 + 8, loc_1 )desired = 3 break end if desired then if desired == 5 then desired = nil end break end while true do loc_3 = add_i32(loc_0 , 20 )loc_1 = load_i32(memory_at_0, loc_3 )if loc_1 ~= 0 then break end loc_1 = load_i32(memory_at_0, loc_0 + 16)if loc_1 == 0 then desired = 4 break end loc_3 = add_i32(loc_0 , 16 )break end if desired then if desired == 5 then desired = nil end break end while true do loc_4 = loc_3 loc_5 = loc_1 loc_3 = add_i32(loc_5 , 20 )loc_1 = load_i32(memory_at_0, loc_3 )if loc_1 ~= 0 then continue end loc_3 = add_i32(loc_5 , 16 )loc_1 = load_i32(memory_at_0, loc_5 + 16)if loc_1 ~= 0 then continue end break end if desired then if desired == 5 then desired = nil end break end store_i32(memory_at_0, loc_4 , 0 )desired = 3 break end if desired then if desired == 4 then desired = nil end break end loc_1 = load_i32(memory_at_0, loc_2 + 4)if band_i32(loc_1 , 3 )~= 3 then desired = 2 break end store_i32(memory_at_0, 0 + 67096, param_0 )store_i32(memory_at_0, loc_2 + 4, band_i32(loc_1 , 4294967294 ))store_i32(memory_at_0, loc_0 + 4, bor_i32(param_0 , 1 ))store_i32(memory_at_0, loc_2 , param_0 )desired = 0 break end if desired then if desired == 3 then desired = nil end break end loc_5 = 0 break end if desired then if desired == 2 then desired = nil end break end if loc_6 == 0 then break end while true do while true do loc_3 = load_i32(memory_at_0, loc_0 + 28)loc_1 = add_i32(shl_i32(loc_3 , 2 ), 67392 )if loc_0 ~= load_i32(memory_at_0, loc_1 )then break end store_i32(memory_at_0, loc_1 , loc_5 )if loc_5 ~= 0 then desired = 3 break end store_i32(memory_at_0, 0 + 67092, band_i32(load_i32(memory_at_0, 0 + 67092), rotl_i32(4294967294 , loc_3 )))desired = 2 break end if desired then if desired == 3 then desired = nil end break end store_i32(memory_at_0, add_i32(loc_6 , (load_i32(memory_at_0, loc_6 + 16)== loc_0 and 16 or 20 )), loc_5 )if loc_5 == 0 then desired = 2 break end break end if desired then if desired == 2 then desired = nil end break end store_i32(memory_at_0, loc_5 + 24, loc_6 )while true do loc_1 = load_i32(memory_at_0, loc_0 + 16)if loc_1 == 0 then break end store_i32(memory_at_0, loc_5 + 16, loc_1 )store_i32(memory_at_0, loc_1 + 24, loc_5 )break end if desired then if desired == 2 then desired = nil end break end loc_1 = load_i32(memory_at_0, add_i32(loc_0 , 20 ))if loc_1 == 0 then break end store_i32(memory_at_0, add_i32(loc_5 , 20 ), loc_1 )store_i32(memory_at_0, loc_1 + 24, loc_5 )break end if desired then if desired == 1 then desired = nil end break end if loc_0 >= loc_2 then break end loc_1 = load_i32(memory_at_0, loc_2 + 4)if band_i32(loc_1 , 1 )== 0 then break end while true do while true do while true do while true do while true do if band_i32(loc_1 , 2 )~= 0 then break end while true do if loc_2 ~= load_i32(memory_at_0, 0 + 67112)then break end store_i32(memory_at_0, 0 + 67112, loc_0 )param_0 = add_i32(load_i32(memory_at_0, 0 + 67100), param_0 )store_i32(memory_at_0, 0 + 67100, param_0 )store_i32(memory_at_0, loc_0 + 4, bor_i32(param_0 , 1 ))if loc_0 ~= load_i32(memory_at_0, 0 + 67108)then desired = 1 break end store_i32(memory_at_0, 0 + 67096, 0 )store_i32(memory_at_0, 0 + 67108, 0 )desired = 0 break end if desired then if desired == 6 then desired = nil end break end while true do if loc_2 ~= load_i32(memory_at_0, 0 + 67108)then break end store_i32(memory_at_0, 0 + 67108, loc_0 )param_0 = add_i32(load_i32(memory_at_0, 0 + 67096), param_0 )store_i32(memory_at_0, 0 + 67096, param_0 )store_i32(memory_at_0, loc_0 + 4, bor_i32(param_0 , 1 ))store_i32(memory_at_0, add_i32(loc_0 , param_0 ), param_0 )desired = 0 break end if desired then if desired == 6 then desired = nil end break end param_0 = add_i32(band_i32(loc_1 , 4294967288 ), param_0 )while true do if loc_1 > 255 then break end loc_3 = load_i32(memory_at_0, loc_2 + 8)loc_4 = shr_u32(loc_1 , 3 )loc_5 = add_i32(shl_i32(loc_4 , 3 ), 67128 )while true do loc_1 = load_i32(memory_at_0, loc_2 + 12)if loc_1 ~= loc_3 then break end store_i32(memory_at_0, 0 + 67088, band_i32(load_i32(memory_at_0, 0 + 67088), rotl_i32(4294967294 , loc_4 )))desired = 3 break end if desired then if desired == 7 then desired = nil end break end store_i32(memory_at_0, loc_3 + 12, loc_1 )store_i32(memory_at_0, loc_1 + 8, loc_3 )desired = 3 break end if desired then if desired == 6 then desired = nil end break end loc_6 = load_i32(memory_at_0, loc_2 + 24)while true do loc_5 = load_i32(memory_at_0, loc_2 + 12)if loc_5 == loc_2 then break end loc_1 = load_i32(memory_at_0, loc_2 + 8)store_i32(memory_at_0, loc_1 + 12, loc_5 )store_i32(memory_at_0, loc_5 + 8, loc_1 )desired = 4 break end if desired then if desired == 6 then desired = nil end break end while true do loc_3 = add_i32(loc_2 , 20 )loc_1 = load_i32(memory_at_0, loc_3 )if loc_1 ~= 0 then break end loc_1 = load_i32(memory_at_0, loc_2 + 16)if loc_1 == 0 then desired = 5 break end loc_3 = add_i32(loc_2 , 16 )break end if desired then if desired == 6 then desired = nil end break end while true do loc_4 = loc_3 loc_5 = loc_1 loc_3 = add_i32(loc_5 , 20 )loc_1 = load_i32(memory_at_0, loc_3 )if loc_1 ~= 0 then continue end loc_3 = add_i32(loc_5 , 16 )loc_1 = load_i32(memory_at_0, loc_5 + 16)if loc_1 ~= 0 then continue end break end if desired then if desired == 6 then desired = nil end break end store_i32(memory_at_0, loc_4 , 0 )desired = 4 break end if desired then if desired == 5 then desired = nil end break end store_i32(memory_at_0, loc_2 + 4, band_i32(loc_1 , 4294967294 ))store_i32(memory_at_0, loc_0 + 4, bor_i32(param_0 , 1 ))store_i32(memory_at_0, add_i32(loc_0 , param_0 ), param_0 )desired = 2 break end if desired then if desired == 4 then desired = nil end break end loc_5 = 0 break end if desired then if desired == 3 then desired = nil end break end if loc_6 == 0 then break end while true do while true do loc_3 = load_i32(memory_at_0, loc_2 + 28)loc_1 = add_i32(shl_i32(loc_3 , 2 ), 67392 )if loc_2 ~= load_i32(memory_at_0, loc_1 )then break end store_i32(memory_at_0, loc_1 , loc_5 )if loc_5 ~= 0 then desired = 4 break end store_i32(memory_at_0, 0 + 67092, band_i32(load_i32(memory_at_0, 0 + 67092), rotl_i32(4294967294 , loc_3 )))desired = 3 break end if desired then if desired == 4 then desired = nil end break end store_i32(memory_at_0, add_i32(loc_6 , (load_i32(memory_at_0, loc_6 + 16)== loc_2 and 16 or 20 )), loc_5 )if loc_5 == 0 then desired = 3 break end break end if desired then if desired == 3 then desired = nil end break end store_i32(memory_at_0, loc_5 + 24, loc_6 )while true do loc_1 = load_i32(memory_at_0, loc_2 + 16)if loc_1 == 0 then break end store_i32(memory_at_0, loc_5 + 16, loc_1 )store_i32(memory_at_0, loc_1 + 24, loc_5 )break end if desired then if desired == 3 then desired = nil end break end loc_1 = load_i32(memory_at_0, add_i32(loc_2 , 20 ))if loc_1 == 0 then break end store_i32(memory_at_0, add_i32(loc_5 , 20 ), loc_1 )store_i32(memory_at_0, loc_1 + 24, loc_5 )break end if desired then if desired == 2 then desired = nil end break end store_i32(memory_at_0, loc_0 + 4, bor_i32(param_0 , 1 ))store_i32(memory_at_0, add_i32(loc_0 , param_0 ), param_0 )if loc_0 ~= load_i32(memory_at_0, 0 + 67108)then break end store_i32(memory_at_0, 0 + 67096, param_0 )desired = 0 break end if desired then if desired == 1 then desired = nil end break end while true do if param_0 > 255 then break end loc_1 = add_i32(band_i32(param_0 , 4294967288 ), 67128 )while true do while true do loc_3 = load_i32(memory_at_0, 0 + 67088)param_0 = shl_i32(1 , shr_u32(param_0 , 3 ))if band_i32(loc_3 , param_0 )~= 0 then break end store_i32(memory_at_0, 0 + 67088, bor_i32(loc_3 , param_0 ))param_0 = loc_1 desired = 3 break end if desired then if desired == 3 then desired = nil end break end param_0 = load_i32(memory_at_0, loc_1 + 8)break end if desired then if desired == 2 then desired = nil end break end store_i32(memory_at_0, loc_1 + 8, loc_0 )store_i32(memory_at_0, param_0 + 12, loc_0 )store_i32(memory_at_0, loc_0 + 12, loc_1 )store_i32(memory_at_0, loc_0 + 8, param_0 )desired = 0 break end if desired then if desired == 1 then desired = nil end break end loc_1 = 31 while true do if param_0 > 16777215 then break end loc_1 = clz_i32(shr_u32(param_0 , 8 ))loc_1 = add_i32(sub_i32(band_i32(shr_u32(param_0 , sub_i32(38 , loc_1 )), 1 ), shl_i32(loc_1 , 1 )), 62 )break end if desired then if desired == 1 then desired = nil end break end store_i32(memory_at_0, loc_0 + 28, loc_1 )store_i64(memory_at_0, loc_0 + 16, i64_ZERO )loc_3 = add_i32(shl_i32(loc_1 , 2 ), 67392 )while true do while true do while true do while true do loc_5 = load_i32(memory_at_0, 0 + 67092)loc_2 = shl_i32(1 , loc_1 )if band_i32(loc_5 , loc_2 )~= 0 then break end store_i32(memory_at_0, 0 + 67092, bor_i32(loc_5 , loc_2 ))store_i32(memory_at_0, loc_3 , loc_0 )store_i32(memory_at_0, loc_0 + 24, loc_3 )desired = 4 break end if desired then if desired == 4 then desired = nil end break end loc_1 = shl_i32(param_0 , (loc_1 == 31 and 0 or sub_i32(25 , shr_u32(loc_1 , 1 ))))loc_5 = load_i32(memory_at_0, loc_3 )while true do loc_3 = loc_5 if band_i32(load_i32(memory_at_0, loc_3 + 4), 4294967288 )== param_0 then desired = 3 break end loc_5 = shr_u32(loc_1 , 29 )loc_1 = shl_i32(loc_1 , 1 )loc_2 = add_i32(add_i32(loc_3 , band_i32(loc_5 , 4 )), 16 )loc_5 = load_i32(memory_at_0, loc_2 )if loc_5 ~= 0 then continue end break end if desired then if desired == 4 then desired = nil end break end store_i32(memory_at_0, loc_2 , loc_0 )store_i32(memory_at_0, loc_0 + 24, loc_3 )break end if desired then if desired == 3 then desired = nil end break end store_i32(memory_at_0, loc_0 + 12, loc_0 )store_i32(memory_at_0, loc_0 + 8, loc_0 )desired = 2 break end if desired then if desired == 2 then desired = nil end break end param_0 = load_i32(memory_at_0, loc_3 + 8)store_i32(memory_at_0, param_0 + 12, loc_0 )store_i32(memory_at_0, loc_3 + 8, loc_0 )store_i32(memory_at_0, loc_0 + 24, 0 )store_i32(memory_at_0, loc_0 + 12, loc_3 )store_i32(memory_at_0, loc_0 + 8, param_0 )break end if desired then if desired == 1 then desired = nil end break end loc_0 = add_i32(load_i32(memory_at_0, 0 + 67120), 4294967295 )store_i32(memory_at_0, 0 + 67120, (loc_0 ~= 0 and loc_0 or 4294967295 ))break end if desired then if desired == 0 then desired = nil end break end break end end FUNC_LIST[95] =--[[ internal_memalign ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local reg_0 local desired while true do loc_0 = 16 while true do while true do loc_1 = (param_0 > 16 and param_0 or 16 )if band_i32(loc_1 , add_i32(loc_1 , 4294967295 ))~= 0 then break end param_0 = loc_1 desired = 1 break end if desired then if desired == 1 then desired = nil end break end while true do param_0 = loc_0 loc_0 = shl_i32(param_0 , 1 )if param_0 < loc_1 then continue end break end if desired then if desired == 1 then desired = nil end break end break end if desired then if desired == 0 then desired = nil end break end while true do if sub_i32(4294967232 , param_0 )> param_1 then break end reg_0 = FUNC_LIST[91]()store_i32(memory_at_0, reg_0 , 48 )reg_0 = 0 desired = 0 break end if desired then if desired == 0 then desired = nil end break end while true do param_1 = (param_1 < 11 and 16 or band_i32(add_i32(param_1 , 11 ), 4294967288 ))reg_0 = FUNC_LIST[93](add_i32(add_i32(param_1 , param_0 ), 12 ))loc_0 = reg_0 if loc_0 ~= 0 then break end reg_0 = 0 desired = 0 break end if desired then if desired == 0 then desired = nil end break end loc_1 = add_i32(loc_0 , 4294967288 )while true do while true do if band_i32(add_i32(param_0 , 4294967295 ), loc_0 )~= 0 then break end param_0 = loc_1 desired = 1 break end if desired then if desired == 1 then desired = nil end break end loc_2 = add_i32(loc_0 , 4294967292 )loc_3 = load_i32(memory_at_0, loc_2 )loc_0 = add_i32(band_i32(add_i32(add_i32(loc_0 , param_0 ), 4294967295 ), sub_i32(0 , param_0 )), 4294967288 )param_0 = add_i32(loc_0 , (sub_i32(loc_0 , loc_1 )> 15 and 0 or param_0 ))loc_0 = sub_i32(param_0 , loc_1 )loc_4 = sub_i32(band_i32(loc_3 , 4294967288 ), loc_0 )while true do if band_i32(loc_3 , 3 )~= 0 then break end loc_1 = load_i32(memory_at_0, loc_1 )store_i32(memory_at_0, param_0 + 4, loc_4 )store_i32(memory_at_0, param_0 , add_i32(loc_1 , loc_0 ))desired = 1 break end if desired then if desired == 1 then desired = nil end break end store_i32(memory_at_0, param_0 + 4, bor_i32(bor_i32(loc_4 , band_i32(load_i32(memory_at_0, param_0 + 4), 1 )), 2 ))loc_4 = add_i32(param_0 , loc_4 )store_i32(memory_at_0, loc_4 + 4, bor_i32(load_i32(memory_at_0, loc_4 + 4), 1 ))store_i32(memory_at_0, loc_2 , bor_i32(bor_i32(loc_0 , band_i32(load_i32(memory_at_0, loc_2 ), 1 )), 2 ))loc_4 = add_i32(loc_1 , loc_0 )store_i32(memory_at_0, loc_4 + 4, bor_i32(load_i32(memory_at_0, loc_4 + 4), 1 ))FUNC_LIST[97](loc_1 , loc_0 )break end if desired then if desired == 0 then desired = nil end break end while true do loc_0 = load_i32(memory_at_0, param_0 + 4)if band_i32(loc_0 , 3 )== 0 then break end loc_1 = band_i32(loc_0 , 4294967288 )if loc_1 <= add_i32(param_1 , 16 )then break end store_i32(memory_at_0, param_0 + 4, bor_i32(bor_i32(param_1 , band_i32(loc_0 , 1 )), 2 ))loc_0 = add_i32(param_0 , param_1 )param_1 = sub_i32(loc_1 , param_1 )store_i32(memory_at_0, loc_0 + 4, bor_i32(param_1 , 3 ))loc_1 = add_i32(param_0 , loc_1 )store_i32(memory_at_0, loc_1 + 4, bor_i32(load_i32(memory_at_0, loc_1 + 4), 1 ))FUNC_LIST[97](loc_0 , param_1 )break end if desired then if desired == 0 then desired = nil end break end reg_0 = add_i32(param_0 , 8 )break end return reg_0 end FUNC_LIST[96] =--[[ dlposix_memalign ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local reg_0 local desired while true do while true do while true do while true do if param_1 ~= 8 then break end reg_0 = FUNC_LIST[93](param_2 )param_1 = reg_0 desired = 2 break end if desired then if desired == 2 then desired = nil end break end loc_0 = 28 if param_1 < 4 then desired = 1 break end if band_i32(param_1 , 3 )~= 0 then desired = 1 break end loc_1 = shr_u32(param_1 , 2 )if band_i32(loc_1 , add_i32(loc_1 , 4294967295 ))~= 0 then desired = 1 break end loc_0 = 48 if sub_i32(4294967232 , param_1 )< param_2 then desired = 1 break end reg_0 = FUNC_LIST[95]((param_1 > 16 and param_1 or 16 ), param_2 )param_1 = reg_0 break end if desired then if desired == 1 then desired = nil end break end while true do if param_1 ~= 0 then break end reg_0 = 48 desired = 0 break end if desired then if desired == 1 then desired = nil end break end store_i32(memory_at_0, param_0 , param_1 )loc_0 = 0 break end if desired then if desired == 0 then desired = nil end break end reg_0 = loc_0 break end return reg_0 end FUNC_LIST[97] =--[[ dispose_chunk ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local desired while true do loc_0 = add_i32(param_0 , param_1 )while true do while true do loc_1 = load_i32(memory_at_0, param_0 + 4)if band_i32(loc_1 , 1 )~= 0 then break end if band_i32(loc_1 , 3 )== 0 then desired = 1 break end loc_1 = load_i32(memory_at_0, param_0 )param_1 = add_i32(loc_1 , param_1 )while true do while true do while true do while true do param_0 = sub_i32(param_0 , loc_1 )if param_0 == load_i32(memory_at_0, 0 + 67108)then break end while true do if loc_1 > 255 then break end loc_2 = load_i32(memory_at_0, param_0 + 8)loc_3 = shr_u32(loc_1 , 3 )loc_4 = add_i32(shl_i32(loc_3 , 3 ), 67128 )loc_1 = load_i32(memory_at_0, param_0 + 12)if loc_1 ~= loc_2 then desired = 5 break end store_i32(memory_at_0, 0 + 67088, band_i32(load_i32(memory_at_0, 0 + 67088), rotl_i32(4294967294 , loc_3 )))desired = 2 break end if desired then if desired == 6 then desired = nil end break end loc_5 = load_i32(memory_at_0, param_0 + 24)while true do loc_4 = load_i32(memory_at_0, param_0 + 12)if loc_4 == param_0 then break end loc_1 = load_i32(memory_at_0, param_0 + 8)store_i32(memory_at_0, loc_1 + 12, loc_4 )store_i32(memory_at_0, loc_4 + 8, loc_1 )desired = 3 break end if desired then if desired == 6 then desired = nil end break end while true do loc_2 = add_i32(param_0 , 20 )loc_1 = load_i32(memory_at_0, loc_2 )if loc_1 ~= 0 then break end loc_1 = load_i32(memory_at_0, param_0 + 16)if loc_1 == 0 then desired = 4 break end loc_2 = add_i32(param_0 , 16 )break end if desired then if desired == 6 then desired = nil end break end while true do loc_3 = loc_2 loc_4 = loc_1 loc_2 = add_i32(loc_4 , 20 )loc_1 = load_i32(memory_at_0, loc_2 )if loc_1 ~= 0 then continue end loc_2 = add_i32(loc_4 , 16 )loc_1 = load_i32(memory_at_0, loc_4 + 16)if loc_1 ~= 0 then continue end break end if desired then if desired == 6 then desired = nil end break end store_i32(memory_at_0, loc_3 , 0 )desired = 3 break end if desired then if desired == 5 then desired = nil end break end loc_1 = load_i32(memory_at_0, loc_0 + 4)if band_i32(loc_1 , 3 )~= 3 then desired = 2 break end store_i32(memory_at_0, 0 + 67096, param_1 )store_i32(memory_at_0, loc_0 + 4, band_i32(loc_1 , 4294967294 ))store_i32(memory_at_0, param_0 + 4, bor_i32(param_1 , 1 ))store_i32(memory_at_0, loc_0 , param_1 )desired = 0 break end if desired then if desired == 4 then desired = nil end break end store_i32(memory_at_0, loc_2 + 12, loc_1 )store_i32(memory_at_0, loc_1 + 8, loc_2 )desired = 2 break end if desired then if desired == 3 then desired = nil end break end loc_4 = 0 break end if desired then if desired == 2 then desired = nil end break end if loc_5 == 0 then break end while true do while true do loc_2 = load_i32(memory_at_0, param_0 + 28)loc_1 = add_i32(shl_i32(loc_2 , 2 ), 67392 )if param_0 ~= load_i32(memory_at_0, loc_1 )then break end store_i32(memory_at_0, loc_1 , loc_4 )if loc_4 ~= 0 then desired = 3 break end store_i32(memory_at_0, 0 + 67092, band_i32(load_i32(memory_at_0, 0 + 67092), rotl_i32(4294967294 , loc_2 )))desired = 2 break end if desired then if desired == 3 then desired = nil end break end store_i32(memory_at_0, add_i32(loc_5 , (load_i32(memory_at_0, loc_5 + 16)== param_0 and 16 or 20 )), loc_4 )if loc_4 == 0 then desired = 2 break end break end if desired then if desired == 2 then desired = nil end break end store_i32(memory_at_0, loc_4 + 24, loc_5 )while true do loc_1 = load_i32(memory_at_0, param_0 + 16)if loc_1 == 0 then break end store_i32(memory_at_0, loc_4 + 16, loc_1 )store_i32(memory_at_0, loc_1 + 24, loc_4 )break end if desired then if desired == 2 then desired = nil end break end loc_1 = load_i32(memory_at_0, add_i32(param_0 , 20 ))if loc_1 == 0 then break end store_i32(memory_at_0, add_i32(loc_4 , 20 ), loc_1 )store_i32(memory_at_0, loc_1 + 24, loc_4 )break end if desired then if desired == 1 then desired = nil end break end while true do while true do while true do while true do while true do loc_1 = load_i32(memory_at_0, loc_0 + 4)if band_i32(loc_1 , 2 )~= 0 then break end while true do if loc_0 ~= load_i32(memory_at_0, 0 + 67112)then break end store_i32(memory_at_0, 0 + 67112, param_0 )param_1 = add_i32(load_i32(memory_at_0, 0 + 67100), param_1 )store_i32(memory_at_0, 0 + 67100, param_1 )store_i32(memory_at_0, param_0 + 4, bor_i32(param_1 , 1 ))if param_0 ~= load_i32(memory_at_0, 0 + 67108)then desired = 1 break end store_i32(memory_at_0, 0 + 67096, 0 )store_i32(memory_at_0, 0 + 67108, 0 )desired = 0 break end if desired then if desired == 6 then desired = nil end break end while true do if loc_0 ~= load_i32(memory_at_0, 0 + 67108)then break end store_i32(memory_at_0, 0 + 67108, param_0 )param_1 = add_i32(load_i32(memory_at_0, 0 + 67096), param_1 )store_i32(memory_at_0, 0 + 67096, param_1 )store_i32(memory_at_0, param_0 + 4, bor_i32(param_1 , 1 ))store_i32(memory_at_0, add_i32(param_0 , param_1 ), param_1 )desired = 0 break end if desired then if desired == 6 then desired = nil end break end param_1 = add_i32(band_i32(loc_1 , 4294967288 ), param_1 )while true do if loc_1 > 255 then break end loc_2 = load_i32(memory_at_0, loc_0 + 8)loc_3 = shr_u32(loc_1 , 3 )loc_4 = add_i32(shl_i32(loc_3 , 3 ), 67128 )while true do loc_1 = load_i32(memory_at_0, loc_0 + 12)if loc_1 ~= loc_2 then break end store_i32(memory_at_0, 0 + 67088, band_i32(load_i32(memory_at_0, 0 + 67088), rotl_i32(4294967294 , loc_3 )))desired = 3 break end if desired then if desired == 7 then desired = nil end break end store_i32(memory_at_0, loc_2 + 12, loc_1 )store_i32(memory_at_0, loc_1 + 8, loc_2 )desired = 3 break end if desired then if desired == 6 then desired = nil end break end loc_5 = load_i32(memory_at_0, loc_0 + 24)while true do loc_4 = load_i32(memory_at_0, loc_0 + 12)if loc_4 == loc_0 then break end loc_1 = load_i32(memory_at_0, loc_0 + 8)store_i32(memory_at_0, loc_1 + 12, loc_4 )store_i32(memory_at_0, loc_4 + 8, loc_1 )desired = 4 break end if desired then if desired == 6 then desired = nil end break end while true do loc_2 = add_i32(loc_0 , 20 )loc_1 = load_i32(memory_at_0, loc_2 )if loc_1 ~= 0 then break end loc_1 = load_i32(memory_at_0, loc_0 + 16)if loc_1 == 0 then desired = 5 break end loc_2 = add_i32(loc_0 , 16 )break end if desired then if desired == 6 then desired = nil end break end while true do loc_3 = loc_2 loc_4 = loc_1 loc_2 = add_i32(loc_4 , 20 )loc_1 = load_i32(memory_at_0, loc_2 )if loc_1 ~= 0 then continue end loc_2 = add_i32(loc_4 , 16 )loc_1 = load_i32(memory_at_0, loc_4 + 16)if loc_1 ~= 0 then continue end break end if desired then if desired == 6 then desired = nil end break end store_i32(memory_at_0, loc_3 , 0 )desired = 4 break end if desired then if desired == 5 then desired = nil end break end store_i32(memory_at_0, loc_0 + 4, band_i32(loc_1 , 4294967294 ))store_i32(memory_at_0, param_0 + 4, bor_i32(param_1 , 1 ))store_i32(memory_at_0, add_i32(param_0 , param_1 ), param_1 )desired = 2 break end if desired then if desired == 4 then desired = nil end break end loc_4 = 0 break end if desired then if desired == 3 then desired = nil end break end if loc_5 == 0 then break end while true do while true do loc_2 = load_i32(memory_at_0, loc_0 + 28)loc_1 = add_i32(shl_i32(loc_2 , 2 ), 67392 )if loc_0 ~= load_i32(memory_at_0, loc_1 )then break end store_i32(memory_at_0, loc_1 , loc_4 )if loc_4 ~= 0 then desired = 4 break end store_i32(memory_at_0, 0 + 67092, band_i32(load_i32(memory_at_0, 0 + 67092), rotl_i32(4294967294 , loc_2 )))desired = 3 break end if desired then if desired == 4 then desired = nil end break end store_i32(memory_at_0, add_i32(loc_5 , (load_i32(memory_at_0, loc_5 + 16)== loc_0 and 16 or 20 )), loc_4 )if loc_4 == 0 then desired = 3 break end break end if desired then if desired == 3 then desired = nil end break end store_i32(memory_at_0, loc_4 + 24, loc_5 )while true do loc_1 = load_i32(memory_at_0, loc_0 + 16)if loc_1 == 0 then break end store_i32(memory_at_0, loc_4 + 16, loc_1 )store_i32(memory_at_0, loc_1 + 24, loc_4 )break end if desired then if desired == 3 then desired = nil end break end loc_1 = load_i32(memory_at_0, add_i32(loc_0 , 20 ))if loc_1 == 0 then break end store_i32(memory_at_0, add_i32(loc_4 , 20 ), loc_1 )store_i32(memory_at_0, loc_1 + 24, loc_4 )break end if desired then if desired == 2 then desired = nil end break end store_i32(memory_at_0, param_0 + 4, bor_i32(param_1 , 1 ))store_i32(memory_at_0, add_i32(param_0 , param_1 ), param_1 )if param_0 ~= load_i32(memory_at_0, 0 + 67108)then break end store_i32(memory_at_0, 0 + 67096, param_1 )desired = 0 break end if desired then if desired == 1 then desired = nil end break end while true do if param_1 > 255 then break end loc_1 = add_i32(band_i32(param_1 , 4294967288 ), 67128 )while true do while true do loc_2 = load_i32(memory_at_0, 0 + 67088)param_1 = shl_i32(1 , shr_u32(param_1 , 3 ))if band_i32(loc_2 , param_1 )~= 0 then break end store_i32(memory_at_0, 0 + 67088, bor_i32(loc_2 , param_1 ))param_1 = loc_1 desired = 3 break end if desired then if desired == 3 then desired = nil end break end param_1 = load_i32(memory_at_0, loc_1 + 8)break end if desired then if desired == 2 then desired = nil end break end store_i32(memory_at_0, loc_1 + 8, param_0 )store_i32(memory_at_0, param_1 + 12, param_0 )store_i32(memory_at_0, param_0 + 12, loc_1 )store_i32(memory_at_0, param_0 + 8, param_1 )desired = 0 break end if desired then if desired == 1 then desired = nil end break end loc_1 = 31 while true do if param_1 > 16777215 then break end loc_1 = clz_i32(shr_u32(param_1 , 8 ))loc_1 = add_i32(sub_i32(band_i32(shr_u32(param_1 , sub_i32(38 , loc_1 )), 1 ), shl_i32(loc_1 , 1 )), 62 )break end if desired then if desired == 1 then desired = nil end break end store_i32(memory_at_0, param_0 + 28, loc_1 )store_i64(memory_at_0, param_0 + 16, i64_ZERO )loc_2 = add_i32(shl_i32(loc_1 , 2 ), 67392 )while true do while true do while true do loc_4 = load_i32(memory_at_0, 0 + 67092)loc_0 = shl_i32(1 , loc_1 )if band_i32(loc_4 , loc_0 )~= 0 then break end store_i32(memory_at_0, 0 + 67092, bor_i32(loc_4 , loc_0 ))store_i32(memory_at_0, loc_2 , param_0 )store_i32(memory_at_0, param_0 + 24, loc_2 )desired = 3 break end if desired then if desired == 3 then desired = nil end break end loc_1 = shl_i32(param_1 , (loc_1 == 31 and 0 or sub_i32(25 , shr_u32(loc_1 , 1 ))))loc_4 = load_i32(memory_at_0, loc_2 )while true do loc_2 = loc_4 if band_i32(load_i32(memory_at_0, loc_2 + 4), 4294967288 )== param_1 then desired = 2 break end loc_4 = shr_u32(loc_1 , 29 )loc_1 = shl_i32(loc_1 , 1 )loc_0 = add_i32(add_i32(loc_2 , band_i32(loc_4 , 4 )), 16 )loc_4 = load_i32(memory_at_0, loc_0 )if loc_4 ~= 0 then continue end break end if desired then if desired == 3 then desired = nil end break end store_i32(memory_at_0, loc_0 , param_0 )store_i32(memory_at_0, param_0 + 24, loc_2 )break end if desired then if desired == 2 then desired = nil end break end store_i32(memory_at_0, param_0 + 12, param_0 )store_i32(memory_at_0, param_0 + 8, param_0 )desired = 0 break end if desired then if desired == 1 then desired = nil end break end param_1 = load_i32(memory_at_0, loc_2 + 8)store_i32(memory_at_0, param_1 + 12, param_0 )store_i32(memory_at_0, loc_2 + 8, param_0 )store_i32(memory_at_0, param_0 + 24, 0 )store_i32(memory_at_0, param_0 + 12, loc_2 )store_i32(memory_at_0, param_0 + 8, param_1 )break end if desired then if desired == 0 then desired = nil end break end break end end FUNC_LIST[98] =--[[ aligned_alloc ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local reg_0 local desired while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 16 )GLOBAL_LIST[0].value = loc_0 loc_1 = 0 while true do if band_i32(param_0 , 3 )~= 0 then break end if (param_1 % param_0 )~= 0 then break end reg_0 = FUNC_LIST[96](add_i32(loc_0 , 12 ), param_0 , param_1 )param_0 = reg_0 loc_1 = (param_0 ~= 0 and 0 or load_i32(memory_at_0, loc_0 + 12))break end GLOBAL_LIST[0].value = add_i32(loc_0 , 16 )reg_0 = loc_1 break end return reg_0 end FUNC_LIST[99] =--[[ operator new(unsigned long) ]]function(param_0)local loc_0 = 0  local reg_0 local desired while true do loc_0 = (param_0 > 1 and param_0 or 1 )while true do while true do reg_0 = FUNC_LIST[93](loc_0 )param_0 = reg_0 if param_0 ~= 0 then desired = 1 break end while true do reg_0 = FUNC_LIST[191]()param_0 = reg_0 if param_0 == 0 then break end TABLE_LIST[0].data[param_0 ]()desired = 2 break end if desired then if desired == 2 then desired = nil continue end break end break end if desired then if desired == 1 then desired = nil end break end FUNC_LIST[1]()error("out of code bounds")end reg_0 = param_0 break end return reg_0 end FUNC_LIST[100] =--[[ operator delete(void*) ]]function(param_0)while true do FUNC_LIST[94](param_0 )break end end FUNC_LIST[101] =--[[ operator new(unsigned long, std::align_val_t) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local reg_0 local desired while true do loc_0 = (param_1 > 4 and param_1 or 4 )param_0 = (param_0 > 1 and param_0 or 1 )while true do while true do reg_0 = FUNC_LIST[102](loc_0 , param_0 )loc_1 = reg_0 if loc_1 ~= 0 then desired = 1 break end reg_0 = FUNC_LIST[191]()param_1 = reg_0 if param_1 == 0 then desired = 1 break end TABLE_LIST[0].data[param_1 ]()continue end if desired then if desired == 1 then desired = nil end break end error("out of code bounds")end reg_0 = loc_1 break end return reg_0 end FUNC_LIST[102] =--[[ std::__2::__libcpp_aligned_alloc[abi:v160004](unsigned long, unsigned long) ]]function(param_0, param_1)local loc_0 = 0  local reg_0 while true do loc_0 = band_i32(add_i32(add_i32(param_0 , param_1 ), 4294967295 ), sub_i32(0 , param_0 ))reg_0 = FUNC_LIST[98](param_0 , (loc_0 > param_1 and loc_0 or param_1 ))break end return reg_0 end FUNC_LIST[103] =--[[ operator delete(void*, std::align_val_t) ]]function(param_0, param_1)while true do FUNC_LIST[104](param_0 )break end end FUNC_LIST[104] =--[[ std::__2::__libcpp_aligned_free[abi:v160004](void*) ]]function(param_0)while true do FUNC_LIST[94](param_0 )break end end FUNC_LIST[105] =--[[ __lockfile ]]function(param_0)local reg_0 while true do reg_0 = 1 break end return reg_0 end FUNC_LIST[106] =--[[ __unlockfile ]]function(param_0)while true do break end end FUNC_LIST[107] =--[[ __lock ]]function(param_0)while true do break end end FUNC_LIST[108] =--[[ __unlock ]]function(param_0)while true do break end end FUNC_LIST[109] =--[[ __ofl_lock ]]function()local reg_0 while true do FUNC_LIST[107](67584 )reg_0 = 67588 break end return reg_0 end FUNC_LIST[110] =--[[ __ofl_unlock ]]function()while true do FUNC_LIST[108](67584 )break end end FUNC_LIST[111] =--[[ dummy ]]function(param_0)local reg_0 while true do reg_0 = param_0 break end return reg_0 end FUNC_LIST[112] =--[[ __stdio_close ]]function(param_0)local reg_0 while true do reg_0 = FUNC_LIST[111](load_i32(memory_at_0, param_0 + 60))reg_0 = FUNC_LIST[3](reg_0 )break end return reg_0 end FUNC_LIST[113] =--[[ __wasi_syscall_ret ]]function(param_0)local reg_0 local desired while true do while true do if param_0 ~= 0 then break end reg_0 = 0 desired = 0 break end if desired then if desired == 0 then desired = nil end break end reg_0 = FUNC_LIST[91]()store_i32(memory_at_0, reg_0 , param_0 )reg_0 = 4294967295 break end return reg_0 end FUNC_LIST[114] =--[[ __stdio_write ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local loc_3 = 0  local loc_4 = 0  local loc_5 = 0  local loc_6 = 0  local reg_0 local desired while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 32 )GLOBAL_LIST[0].value = loc_0 loc_1 = load_i32(memory_at_0, param_0 + 28)store_i32(memory_at_0, loc_0 + 16, loc_1 )loc_2 = load_i32(memory_at_0, param_0 + 20)store_i32(memory_at_0, loc_0 + 28, param_2 )store_i32(memory_at_0, loc_0 + 24, param_1 )param_1 = sub_i32(loc_2 , loc_1 )store_i32(memory_at_0, loc_0 + 20, param_1 )loc_3 = add_i32(param_1 , param_2 )loc_1 = add_i32(loc_0 , 16 )loc_4 = 2 while true do while true do while true do while true do while true do reg_0 = FUNC_LIST[4](load_i32(memory_at_0, param_0 + 60), add_i32(loc_0 , 16 ), 2 , add_i32(loc_0 , 12 ))reg_0 = FUNC_LIST[113](reg_0 )if reg_0 == 0 then break end loc_2 = loc_1 desired = 4 break end if desired then if desired == 4 then desired = nil end break end while true do param_1 = load_i32(memory_at_0, loc_0 + 12)if loc_3 == param_1 then desired = 3 break end while true do if gt_i32(param_1 , 4294967295 )then break end loc_2 = loc_1 desired = 2 break end if desired then if desired == 5 then desired = nil continue end break end loc_5 = load_i32(memory_at_0, loc_1 + 4)loc_6 = (param_1 > loc_5 and 1 or 0)loc_2 = add_i32(loc_1 , shl_i32(loc_6 , 3 ))loc_5 = sub_i32(param_1 , (loc_6 ~= 0 and loc_5 or 0 ))store_i32(memory_at_0, loc_2 , add_i32(load_i32(memory_at_0, loc_2 ), loc_5 ))loc_1 = add_i32(loc_1 , (loc_6 ~= 0 and 12 or 4 ))store_i32(memory_at_0, loc_1 , sub_i32(load_i32(memory_at_0, loc_1 ), loc_5 ))loc_3 = sub_i32(loc_3 , param_1 )loc_1 = loc_2 loc_4 = sub_i32(loc_4 , loc_6 )reg_0 = FUNC_LIST[4](load_i32(memory_at_0, param_0 + 60), loc_2 , loc_4 , add_i32(loc_0 , 12 ))reg_0 = FUNC_LIST[113](reg_0 )if reg_0 == 0 then continue end break end if desired then if desired == 4 then desired = nil end break end break end if desired then if desired == 3 then desired = nil end break end if loc_3 ~= 4294967295 then desired = 2 break end break end if desired then if desired == 2 then desired = nil end break end param_1 = load_i32(memory_at_0, param_0 + 44)store_i32(memory_at_0, param_0 + 28, param_1 )store_i32(memory_at_0, param_0 + 20, param_1 )store_i32(memory_at_0, param_0 + 16, add_i32(param_1 , load_i32(memory_at_0, param_0 + 48)))param_1 = param_2 desired = 1 break end if desired then if desired == 1 then desired = nil end break end param_1 = 0 store_i32(memory_at_0, param_0 + 28, 0 )store_i64(memory_at_0, param_0 + 16, i64_ZERO )store_i32(memory_at_0, param_0 , bor_i32(load_i32(memory_at_0, param_0 ), 32 ))if loc_4 == 2 then break end param_1 = sub_i32(param_2 , load_i32(memory_at_0, loc_2 + 4))break end GLOBAL_LIST[0].value = add_i32(loc_0 , 32 )reg_0 = param_1 break end return reg_0 end FUNC_LIST[115] =--[[ __lseek ]]function(param_0, param_1, param_2)local loc_0 = 0  local reg_0 while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 16 )GLOBAL_LIST[0].value = loc_0 reg_0 = FUNC_LIST[205](param_0 , param_1 , band_i32(param_2 , 255 ), add_i32(loc_0 , 8 ))reg_0 = FUNC_LIST[113](reg_0 )param_2 = reg_0 param_1 = load_i64(memory_at_0, loc_0 + 8)GLOBAL_LIST[0].value = add_i32(loc_0 , 16 )reg_0 = (param_2 ~= 0 and i64_from_u32(4294967295, 4294967295) or param_1 )break end return reg_0 end FUNC_LIST[116] =--[[ __stdio_seek ]]function(param_0, param_1, param_2)local reg_0 while true do reg_0 = FUNC_LIST[115](load_i32(memory_at_0, param_0 + 60), param_1 , param_2 )break end return reg_0 end FUNC_LIST[117] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::capacity[abi:v160004]() const ]]function(param_0)local loc_0 = 0  local reg_0 local desired while true do loc_0 = 10 while true do reg_0 = FUNC_LIST[34](param_0 )if reg_0 == 0 then break end reg_0 = FUNC_LIST[121](param_0 )loc_0 = add_i32(reg_0 , 4294967295 )break end reg_0 = loc_0 break end return reg_0 end FUNC_LIST[118] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__get_pointer[abi:v160004]() ]]function(param_0)local reg_0 local desired while true do while true do reg_0 = FUNC_LIST[34](param_0 )if reg_0 == 0 then break end reg_0 = FUNC_LIST[76](param_0 )desired = 0 break end if desired then if desired == 0 then desired = nil end break end reg_0 = FUNC_LIST[78](param_0 )break end return reg_0 end FUNC_LIST[119] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__null_terminate_at[abi:v160004](char*, unsigned long) ]]function(param_0, param_1, param_2)local loc_0 = 0  local reg_0 while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 16 )GLOBAL_LIST[0].value = loc_0 FUNC_LIST[122](param_0 , param_2 )FUNC_LIST[123](param_0 , param_2 )store_i32_n8(memory_at_0, loc_0 + 15, 0 )FUNC_LIST[81](add_i32(param_1 , param_2 ), add_i32(loc_0 , 15 ))GLOBAL_LIST[0].value = add_i32(loc_0 , 16 )reg_0 = param_0 break end return reg_0 end FUNC_LIST[120] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__grow_by_and_replace(unsigned long, unsigned long, unsigned long, unsigned long, unsigned long, unsigned long, char const*) ]]function(param_0, param_1, param_2, param_3, param_4, param_5, param_6, param_7)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local reg_0, reg_1 local desired while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 16 )GLOBAL_LIST[0].value = loc_0 while true do reg_0 = FUNC_LIST[124](param_0 )loc_1 = reg_0 if add_i32(loc_1 , bxor_i32(param_1 , 4294967295 ))< param_2 then break end reg_0 = FUNC_LIST[118](param_0 )loc_2 = reg_0 while true do if add_i32(shr_u32(loc_1 , 1 ), 4294967280 )<= param_1 then break end store_i32(memory_at_0, loc_0 + 12, shl_i32(param_1 , 1 ))store_i32(memory_at_0, loc_0 + 4, add_i32(param_2 , param_1 ))reg_0 = FUNC_LIST[125](add_i32(loc_0 , 4 ), add_i32(loc_0 , 12 ))reg_0 = FUNC_LIST[126](load_i32(memory_at_0, reg_0 ))loc_1 = add_i32(reg_0 , 1 )break end if desired then if desired == 1 then desired = nil end break end reg_1 = FUNC_LIST[127](param_0 )FUNC_LIST[128](add_i32(loc_0 , 4 ), reg_1 , loc_1 )loc_1 = load_i32(memory_at_0, loc_0 + 4)FUNC_LIST[129](loc_1 , load_i32(memory_at_0, loc_0 + 8))FUNC_LIST[130](param_0 )while true do if param_4 == 0 then break end reg_0 = FUNC_LIST[79](loc_1 )reg_1 = FUNC_LIST[79](loc_2 )reg_0 = FUNC_LIST[131](reg_0 , reg_1 , param_4 )break end if desired then if desired == 1 then desired = nil end break end while true do if param_6 == 0 then break end reg_0 = FUNC_LIST[79](loc_1 )reg_0 = FUNC_LIST[131](add_i32(reg_0 , param_4 ), param_7 , param_6 )break end if desired then if desired == 1 then desired = nil end break end param_7 = add_i32(param_5 , param_4 )param_2 = sub_i32(param_3 , param_7 )while true do if param_3 == param_7 then break end reg_0 = FUNC_LIST[79](loc_1 )reg_1 = FUNC_LIST[79](loc_2 )reg_0 = FUNC_LIST[131](add_i32(add_i32(reg_0 , param_4 ), param_6 ), add_i32(add_i32(reg_1 , param_4 ), param_5 ), param_2 )break end if desired then if desired == 1 then desired = nil end break end while true do param_1 = add_i32(param_1 , 1 )if param_1 == 11 then break end reg_0 = FUNC_LIST[127](param_0 )FUNC_LIST[132](reg_0 , loc_2 , param_1 )break end if desired then if desired == 1 then desired = nil end break end FUNC_LIST[133](param_0 , loc_1 )FUNC_LIST[134](param_0 , load_i32(memory_at_0, loc_0 + 8))param_4 = add_i32(add_i32(param_6 , param_4 ), param_2 )FUNC_LIST[75](param_0 , param_4 )store_i32_n8(memory_at_0, loc_0 + 12, 0 )FUNC_LIST[81](add_i32(loc_1 , param_4 ), add_i32(loc_0 , 12 ))GLOBAL_LIST[0].value = add_i32(loc_0 , 16 )desired = 0 break end if desired then if desired == 0 then desired = nil end break end FUNC_LIST[135](param_0 )error("out of code bounds")end end FUNC_LIST[121] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__get_long_cap[abi:v160004]() const ]]function(param_0)local reg_0 while true do reg_0 = FUNC_LIST[37](param_0 )reg_0 = band_i32(load_i32(memory_at_0, reg_0 + 8), 2147483647 )break end return reg_0 end FUNC_LIST[122] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__set_size[abi:v160004](unsigned long) ]]function(param_0, param_1)local reg_0 local desired while true do while true do reg_0 = FUNC_LIST[34](param_0 )if reg_0 == 0 then break end FUNC_LIST[75](param_0 , param_1 )desired = 0 break end if desired then if desired == 0 then desired = nil end break end FUNC_LIST[77](param_0 , param_1 )break end end FUNC_LIST[123] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__invalidate_iterators_past[abi:v160004](unsigned long) ]]function(param_0, param_1)while true do break end end FUNC_LIST[124] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::max_size[abi:v160004]() const ]]function(param_0)local reg_0, reg_1, reg_2 while true do reg_0 = FUNC_LIST[56](param_0 )reg_0 = FUNC_LIST[136](reg_0 )param_0 = reg_0 reg_2 = FUNC_LIST[137]()reg_0 = add_i32(shr_u32(param_0 , (param_0 > shr_u32(reg_2 , 1 )and 1 or 0)), 4294967280 )break end return reg_0 end FUNC_LIST[125] =--[[ unsigned long const& std::__2::max[abi:v160004]<unsigned long>(unsigned long const&, unsigned long const&) ]]function(param_0, param_1)local reg_0 while true do reg_0 = FUNC_LIST[150](param_0 , param_1 )break end return reg_0 end FUNC_LIST[126] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__recommend[abi:v160004](unsigned long) ]]function(param_0)local loc_0 = 0  local reg_0 local desired while true do loc_0 = 10 while true do if param_0 < 11 then break end reg_0 = FUNC_LIST[141](add_i32(param_0 , 1 ))param_0 = reg_0 reg_0 = param_0 param_0 = add_i32(param_0 , 4294967295 )loc_0 = (param_0 == 11 and reg_0 or param_0 )break end reg_0 = loc_0 break end return reg_0 end FUNC_LIST[127] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__alloc[abi:v160004]() ]]function(param_0)local reg_0 while true do reg_0 = FUNC_LIST[140](param_0 )break end return reg_0 end FUNC_LIST[128] =--[[ std::__2::__allocation_result<std::__2::allocator_traits<std::__2::allocator<char>>::pointer> std::__2::__allocate_at_least[abi:v160004]<std::__2::allocator<char>>(std::__2::allocator<char>&, unsigned long) ]]function(param_0, param_1, param_2)local reg_0 while true do reg_0 = FUNC_LIST[139](param_1 , param_2 )param_1 = reg_0 store_i32(memory_at_0, param_0 + 4, param_2 )store_i32(memory_at_0, param_0 , param_1 )break end end FUNC_LIST[129] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__begin_lifetime[abi:v160004](char*, unsigned long) ]]function(param_0, param_1)while true do break end end FUNC_LIST[130] =--[[ void std::__2::__debug_db_invalidate_all[abi:v160004]<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>>(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>*) ]]function(param_0)while true do break end end FUNC_LIST[131] =--[[ std::__2::char_traits<char>::copy(char*, char const*, unsigned long) ]]function(param_0, param_1, param_2)local reg_0 while true do reg_0 = FUNC_LIST[142](param_1 , param_2 , param_0 )reg_0 = param_0 break end return reg_0 end FUNC_LIST[132] =--[[ std::__2::allocator_traits<std::__2::allocator<char>>::deallocate[abi:v160004](std::__2::allocator<char>&, char*, unsigned long) ]]function(param_0, param_1, param_2)while true do FUNC_LIST[145](param_0 , param_1 , param_2 )break end end FUNC_LIST[133] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__set_long_pointer[abi:v160004](char*) ]]function(param_0, param_1)local reg_0 while true do reg_0 = FUNC_LIST[36](param_0 )store_i32(memory_at_0, reg_0 , param_1 )break end end FUNC_LIST[134] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__set_long_cap[abi:v160004](unsigned long) ]]function(param_0, param_1)local loc_0 = 0  local reg_0 while true do reg_0 = FUNC_LIST[36](param_0 )loc_0 = reg_0 store_i32(memory_at_0, loc_0 + 8, bor_i32(band_i32(load_i32(memory_at_0, loc_0 + 8), 2147483648 ), band_i32(param_1 , 2147483647 )))reg_0 = FUNC_LIST[36](param_0 )param_0 = reg_0 store_i32(memory_at_0, param_0 + 8, bor_i32(load_i32(memory_at_0, param_0 + 8), 2147483648 ))break end end FUNC_LIST[135] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__throw_length_error[abi:v160004]() const ]]function(param_0)while true do FUNC_LIST[138](65552 )error("out of code bounds")end end FUNC_LIST[136] =--[[ unsigned long std::__2::allocator_traits<std::__2::allocator<char>>::max_size[abi:v160004]<std::__2::allocator<char>, void, void>(std::__2::allocator<char> const&) ]]function(param_0)local reg_0 while true do reg_0 = FUNC_LIST[137]()break end return reg_0 end FUNC_LIST[137] =--[[ std::__2::numeric_limits<unsigned long>::max[abi:v160004]() ]]function()local reg_0 while true do reg_0 = FUNC_LIST[155]()break end return reg_0 end FUNC_LIST[138] =--[[ std::__2::__throw_length_error[abi:v160004](char const*) ]]function(param_0)while true do FUNC_LIST[1]()error("out of code bounds")end end FUNC_LIST[139] =--[[ std::__2::allocator<char>::allocate[abi:v160004](unsigned long) ]]function(param_0, param_1)local reg_0 local desired while true do while true do reg_0 = FUNC_LIST[136](param_0 )if reg_0 >= param_1 then break end FUNC_LIST[156]()error("out of code bounds")end reg_0 = FUNC_LIST[157](param_1 , 1 )break end return reg_0 end FUNC_LIST[140] =--[[ std::__2::__compressed_pair<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, std::__2::allocator<char>>::second[abi:v160004]() ]]function(param_0)local reg_0 while true do reg_0 = FUNC_LIST[161](param_0 )break end return reg_0 end FUNC_LIST[141] =--[[ unsigned long std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__align_it[abi:v160004]<16ul>(unsigned long) ]]function(param_0)local reg_0 while true do reg_0 = band_i32(add_i32(param_0 , 15 ), 4294967280 )break end return reg_0 end FUNC_LIST[142] =--[[ std::__2::enable_if<__is_cpp17_random_access_iterator<char const*>::value, char*>::type std::__2::copy_n[abi:v160004]<char const*, unsigned long, char*>(char const*, unsigned long, char*) ]]function(param_0, param_1, param_2)local reg_0 while true do reg_0 = FUNC_LIST[162](param_0 , add_i32(param_0 , param_1 ), param_2 )break end return reg_0 end FUNC_LIST[143] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::~basic_string() ]]function(param_0)local reg_0, reg_1, reg_2 local desired while true do FUNC_LIST[144](param_0 )while true do reg_0 = FUNC_LIST[34](param_0 )if reg_0 == 0 then break end reg_0 = FUNC_LIST[127](param_0 )reg_1 = FUNC_LIST[76](param_0 )reg_2 = FUNC_LIST[121](param_0 )FUNC_LIST[132](reg_0 , reg_1 , reg_2 )break end reg_0 = param_0 break end return reg_0 end FUNC_LIST[144] =--[[ void std::__2::__debug_db_erase_c[abi:v160004]<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>>(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>*) ]]function(param_0)while true do break end end FUNC_LIST[145] =--[[ std::__2::allocator<char>::deallocate[abi:v160004](char*, unsigned long) ]]function(param_0, param_1, param_2)while true do FUNC_LIST[185](param_1 , param_2 , 1 )break end end FUNC_LIST[146] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__init(char const*, unsigned long) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local reg_0, reg_1, reg_2 local desired while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 16 )GLOBAL_LIST[0].value = loc_0 while true do reg_0 = FUNC_LIST[124](param_0 )if reg_0 < param_2 then break end while true do while true do reg_0 = FUNC_LIST[74](param_2 )if reg_0 == 0 then break end FUNC_LIST[77](param_0 , param_2 )reg_0 = FUNC_LIST[78](param_0 )loc_1 = reg_0 desired = 2 break end if desired then if desired == 2 then desired = nil end break end reg_1 = FUNC_LIST[127](param_0 )reg_2 = FUNC_LIST[126](param_2 )FUNC_LIST[128](add_i32(loc_0 , 8 ), reg_1 , add_i32(reg_2 , 1 ))loc_1 = load_i32(memory_at_0, loc_0 + 8)FUNC_LIST[129](loc_1 , load_i32(memory_at_0, loc_0 + 12))FUNC_LIST[133](param_0 , loc_1 )FUNC_LIST[134](param_0 , load_i32(memory_at_0, loc_0 + 12))FUNC_LIST[75](param_0 , param_2 )break end if desired then if desired == 1 then desired = nil end break end reg_0 = FUNC_LIST[79](loc_1 )reg_0 = FUNC_LIST[131](reg_0 , param_1 , param_2 )store_i32_n8(memory_at_0, loc_0 + 7, 0 )FUNC_LIST[81](add_i32(loc_1 , param_2 ), add_i32(loc_0 , 7 ))GLOBAL_LIST[0].value = add_i32(loc_0 , 16 )desired = 0 break end if desired then if desired == 0 then desired = nil end break end FUNC_LIST[135](param_0 )error("out of code bounds")end end FUNC_LIST[147] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__init_copy_ctor_external(char const*, unsigned long) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local reg_0, reg_1, reg_2 local desired while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 16 )GLOBAL_LIST[0].value = loc_0 while true do while true do while true do reg_0 = FUNC_LIST[74](param_2 )if reg_0 == 0 then break end reg_0 = FUNC_LIST[78](param_0 )loc_1 = reg_0 FUNC_LIST[77](param_0 , param_2 )desired = 2 break end if desired then if desired == 2 then desired = nil end break end reg_0 = FUNC_LIST[124](param_0 )if reg_0 < param_2 then desired = 1 break end reg_1 = FUNC_LIST[127](param_0 )reg_2 = FUNC_LIST[126](param_2 )FUNC_LIST[128](add_i32(loc_0 , 8 ), reg_1 , add_i32(reg_2 , 1 ))loc_1 = load_i32(memory_at_0, loc_0 + 8)FUNC_LIST[129](loc_1 , load_i32(memory_at_0, loc_0 + 12))FUNC_LIST[133](param_0 , loc_1 )FUNC_LIST[134](param_0 , load_i32(memory_at_0, loc_0 + 12))FUNC_LIST[75](param_0 , param_2 )break end if desired then if desired == 1 then desired = nil end break end reg_0 = FUNC_LIST[79](loc_1 )reg_0 = FUNC_LIST[131](reg_0 , param_1 , add_i32(param_2 , 1 ))GLOBAL_LIST[0].value = add_i32(loc_0 , 16 )desired = 0 break end if desired then if desired == 0 then desired = nil end break end FUNC_LIST[135](param_0 )error("out of code bounds")end end FUNC_LIST[148] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__assign_external(char const*, unsigned long) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local reg_0, reg_1, reg_2, reg_3 local desired while true do while true do reg_1 = FUNC_LIST[117](param_0 )loc_0 = reg_1 if param_2 > loc_0 then break end reg_0 = FUNC_LIST[118](param_0 )reg_0 = FUNC_LIST[79](reg_0 )loc_0 = reg_0 reg_0 = FUNC_LIST[80](loc_0 , param_1 , param_2 )reg_0 = FUNC_LIST[119](param_0 , loc_0 , param_2 )desired = 0 break end if desired then if desired == 0 then desired = nil end break end reg_3 = FUNC_LIST[40](param_0 )loc_1 = reg_3 FUNC_LIST[120](param_0 , loc_0 , sub_i32(param_2 , loc_0 ), loc_1 , 0 , loc_1 , param_2 , param_1 )reg_0 = param_0 break end return reg_0 end FUNC_LIST[149] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__assign_external(char const*) ]]function(param_0, param_1)local reg_0, reg_1, reg_2 while true do reg_2 = FUNC_LIST[16](param_1 )reg_0 = FUNC_LIST[148](param_0 , param_1 , reg_2 )break end return reg_0 end FUNC_LIST[150] =--[[ unsigned long const& std::__2::max[abi:v160004]<unsigned long, std::__2::__less<unsigned long, unsigned long>>(unsigned long const&, unsigned long const&, std::__2::__less<unsigned long, unsigned long>) ]]function(param_0, param_1)local loc_0 = 0  local loc_1 = 0  local reg_0 while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 16 )GLOBAL_LIST[0].value = loc_0 reg_0 = FUNC_LIST[68](add_i32(loc_0 , 15 ), param_0 , param_1 )loc_1 = reg_0 GLOBAL_LIST[0].value = add_i32(loc_0 , 16 )reg_0 = (loc_1 ~= 0 and param_1 or param_0 )break end return reg_0 end FUNC_LIST[151] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::append(char const*, unsigned long) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local reg_0, reg_1 local desired while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 16 )GLOBAL_LIST[0].value = loc_0 while true do while true do reg_0 = FUNC_LIST[117](param_0 )loc_1 = reg_0 reg_1 = FUNC_LIST[40](param_0 )loc_2 = reg_1 if sub_i32(loc_1 , loc_2 )< param_2 then break end if param_2 == 0 then desired = 1 break end reg_0 = FUNC_LIST[118](param_0 )reg_0 = FUNC_LIST[79](reg_0 )loc_1 = reg_0 reg_0 = FUNC_LIST[131](add_i32(loc_1 , loc_2 ), param_1 , param_2 )param_2 = add_i32(loc_2 , param_2 )FUNC_LIST[122](param_0 , param_2 )store_i32_n8(memory_at_0, loc_0 + 15, 0 )FUNC_LIST[81](add_i32(loc_1 , param_2 ), add_i32(loc_0 , 15 ))desired = 1 break end if desired then if desired == 1 then desired = nil end break end FUNC_LIST[120](param_0 , loc_1 , add_i32(sub_i32(param_2 , loc_1 ), loc_2 ), loc_2 , loc_2 , 0 , param_2 , param_1 )break end GLOBAL_LIST[0].value = add_i32(loc_0 , 16 )reg_0 = param_0 break end return reg_0 end FUNC_LIST[152] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>& std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__assign_no_alias<false>(char const*, unsigned long) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local reg_0, reg_1, reg_2, reg_3 local desired while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 16 )GLOBAL_LIST[0].value = loc_0 while true do while true do reg_0 = FUNC_LIST[121](param_0 )loc_1 = reg_0 if loc_1 <= param_2 then break end reg_0 = FUNC_LIST[76](param_0 )loc_1 = reg_0 FUNC_LIST[75](param_0 , param_2 )reg_0 = FUNC_LIST[79](loc_1 )reg_0 = FUNC_LIST[131](reg_0 , param_1 , param_2 )store_i32_n8(memory_at_0, loc_0 + 15, 0 )FUNC_LIST[81](add_i32(loc_1 , param_2 ), add_i32(loc_0 , 15 ))FUNC_LIST[123](param_0 , param_2 )desired = 1 break end if desired then if desired == 1 then desired = nil end break end reg_3 = FUNC_LIST[43](param_0 )reg_1 = add_i32(loc_1 , 4294967295 )reg_2 = add_i32(sub_i32(param_2 , loc_1 ), 1 )loc_1 = reg_3 FUNC_LIST[120](param_0 , reg_1 , reg_2 , loc_1 , 0 , loc_1 , param_2 , param_1 )break end GLOBAL_LIST[0].value = add_i32(loc_0 , 16 )reg_0 = param_0 break end return reg_0 end FUNC_LIST[153] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>& std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__assign_no_alias<true>(char const*, unsigned long) ]]function(param_0, param_1, param_2)local loc_0 = 0  local loc_1 = 0  local reg_0, reg_1, reg_2, reg_3 local desired while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 16 )GLOBAL_LIST[0].value = loc_0 while true do while true do if param_2 > 10 then break end reg_0 = FUNC_LIST[78](param_0 )loc_1 = reg_0 FUNC_LIST[77](param_0 , param_2 )reg_0 = FUNC_LIST[79](loc_1 )reg_0 = FUNC_LIST[131](reg_0 , param_1 , param_2 )store_i32_n8(memory_at_0, loc_0 + 15, 0 )FUNC_LIST[81](add_i32(loc_1 , param_2 ), add_i32(loc_0 , 15 ))FUNC_LIST[123](param_0 , param_2 )desired = 1 break end if desired then if desired == 1 then desired = nil end break end reg_3 = FUNC_LIST[44](param_0 )loc_1 = reg_3 FUNC_LIST[120](param_0 , 10 , add_i32(param_2 , 4294967286 ), loc_1 , 0 , loc_1 , param_2 , param_1 )break end GLOBAL_LIST[0].value = add_i32(loc_0 , 16 )reg_0 = param_0 break end return reg_0 end FUNC_LIST[154] =--[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::append(char const*) ]]function(param_0, param_1)local reg_0, reg_1, reg_2 while true do reg_2 = FUNC_LIST[16](param_1 )reg_0 = FUNC_LIST[151](param_0 , param_1 , reg_2 )break end return reg_0 end FUNC_LIST[155] =--[[ std::__2::__libcpp_numeric_limits<unsigned long, true>::max[abi:v160004]() ]]function()local reg_0 while true do reg_0 = 4294967295 break end return reg_0 end FUNC_LIST[156] =--[[ std::__throw_bad_array_new_length[abi:v160004]() ]]function()while true do FUNC_LIST[1]()error("out of code bounds")end end FUNC_LIST[157] =--[[ std::__2::__libcpp_allocate[abi:v160004](unsigned long, unsigned long) ]]function(param_0, param_1)local reg_0 local desired while true do while true do reg_0 = FUNC_LIST[158](param_1 )if reg_0 == 0 then break end reg_0 = FUNC_LIST[159](param_0 , param_1 )desired = 0 break end if desired then if desired == 0 then desired = nil end break end reg_0 = FUNC_LIST[160](param_0 )break end return reg_0 end FUNC_LIST[158] =--[[ std::__2::__is_overaligned_for_new[abi:v160004](unsigned long) ]]function(param_0)local reg_0 while true do reg_0 = (param_0 > 8 and 1 or 0)break end return reg_0 end FUNC_LIST[159] =--[[ void* std::__2::__libcpp_operator_new[abi:v160004]<unsigned long, std::align_val_t>(unsigned long, std::align_val_t) ]]function(param_0, param_1)local reg_0 while true do reg_0 = FUNC_LIST[101](param_0 , param_1 )break end return reg_0 end FUNC_LIST[160] =--[[ void* std::__2::__libcpp_operator_new[abi:v160004]<unsigned long>(unsigned long) ]]function(param_0)local reg_0 while true do reg_0 = FUNC_LIST[99](param_0 )break end return reg_0 end FUNC_LIST[161] =--[[ std::__2::__compressed_pair_elem<std::__2::allocator<char>, 1, true>::__get[abi:v160004]() ]]function(param_0)local reg_0 while true do reg_0 = param_0 break end return reg_0 end FUNC_LIST[162] =--[[ char* std::__2::copy[abi:v160004]<char const*, char*>(char const*, char const*, char*) ]]function(param_0, param_1, param_2)local loc_0 = 0  local reg_0 while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 16 )GLOBAL_LIST[0].value = loc_0 FUNC_LIST[163](add_i32(loc_0 , 8 ), param_0 , param_1 , param_2 )param_2 = load_i32(memory_at_0, loc_0 + 12)GLOBAL_LIST[0].value = add_i32(loc_0 , 16 )reg_0 = param_2 break end return reg_0 end FUNC_LIST[163] =--[[ std::__2::pair<char const*, char*> std::__2::__copy[abi:v160004]<std::__2::_ClassicAlgPolicy, char const*, char const*, char*>(char const*, char const*, char*) ]]function(param_0, param_1, param_2, param_3)while true do FUNC_LIST[164](param_0 , param_1 , param_2 , param_3 )break end end FUNC_LIST[164] =--[[ std::__2::pair<char const*, char*> std::__2::__dispatch_copy_or_move[abi:v160004]<std::__2::_ClassicAlgPolicy, std::__2::__copy_loop<std::__2::_ClassicAlgPolicy>, std::__2::__copy_trivial, char const*, char const*, char*>(char const*, char const*, char*) ]]function(param_0, param_1, param_2, param_3)while true do FUNC_LIST[165](param_0 , param_1 , param_2 , param_3 )break end end FUNC_LIST[165] =--[[ std::__2::pair<char const*, char*> std::__2::__unwrap_and_dispatch[abi:v160004]<std::__2::__overload<std::__2::__copy_loop<std::__2::_ClassicAlgPolicy>, std::__2::__copy_trivial>, char const*, char const*, char*, 0>(char const*, char const*, char*) ]]function(param_0, param_1, param_2, param_3)local loc_0 = 0  local reg_0, reg_1, reg_2, reg_3, reg_4 while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 32 )GLOBAL_LIST[0].value = loc_0 FUNC_LIST[166](add_i32(loc_0 , 24 ), param_1 , param_2 )reg_2 = load_i32(memory_at_0, loc_0 + 24)reg_3 = load_i32(memory_at_0, loc_0 + 28)reg_4 = FUNC_LIST[167](param_3 )FUNC_LIST[168](add_i32(loc_0 , 16 ), add_i32(loc_0 , 12 ), reg_2 , reg_3 , reg_4 )reg_1 = FUNC_LIST[169](param_1 , load_i32(memory_at_0, loc_0 + 16))store_i32(memory_at_0, loc_0 + 12, reg_1 )reg_1 = FUNC_LIST[170](param_3 , load_i32(memory_at_0, loc_0 + 20))store_i32(memory_at_0, loc_0 + 8, reg_1 )FUNC_LIST[171](param_0 , add_i32(loc_0 , 12 ), add_i32(loc_0 , 8 ))GLOBAL_LIST[0].value = add_i32(loc_0 , 32 )break end end FUNC_LIST[166] =--[[ auto std::__2::__unwrap_range[abi:v160004]<char const*, char const*>(char const*, char const*) ]]function(param_0, param_1, param_2)while true do FUNC_LIST[172](param_0 , param_1 , param_2 )break end end FUNC_LIST[167] =--[[ decltype(std::__2::__unwrap_iter_impl<char*, true>::__unwrap(std::declval<char*>())) std::__2::__unwrap_iter[abi:v160004]<char*, std::__2::__unwrap_iter_impl<char*, true>, 0>(char*) ]]function(param_0)local reg_0 while true do reg_0 = FUNC_LIST[174](param_0 )break end return reg_0 end FUNC_LIST[168] =--[[ std::__2::pair<char const*, char*> std::__2::__copy_trivial::operator()[abi:v160004]<char const, char, 0>(char const*, char const*, char*) const ]]function(param_0, param_1, param_2, param_3, param_4)while true do FUNC_LIST[173](param_0 , param_2 , param_3 , param_4 )break end end FUNC_LIST[169] =--[[ char const* std::__2::__rewrap_range[abi:v160004]<char const*, char const*, char const*>(char const*, char const*) ]]function(param_0, param_1)local reg_0 while true do reg_0 = FUNC_LIST[176](param_0 , param_1 )break end return reg_0 end FUNC_LIST[170] =--[[ char* std::__2::__rewrap_iter[abi:v160004]<char*, char*, std::__2::__unwrap_iter_impl<char*, true>>(char*, char*) ]]function(param_0, param_1)local reg_0 while true do reg_0 = FUNC_LIST[177](param_0 , param_1 )break end return reg_0 end FUNC_LIST[171] =--[[ std::__2::pair<std::__2::__unwrap_ref_decay<char const*>::type, std::__2::__unwrap_ref_decay<char*>::type> std::__2::make_pair[abi:v160004]<char const*, char*>(char const*&&, char*&&) ]]function(param_0, param_1, param_2)local reg_0 while true do reg_0 = FUNC_LIST[175](param_0 , param_1 , param_2 )break end end FUNC_LIST[172] =--[[ std::__2::__unwrap_range_impl<char const*, char const*>::__unwrap[abi:v160004](char const*, char const*) ]]function(param_0, param_1, param_2)local loc_0 = 0  local reg_0, reg_1 while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 16 )GLOBAL_LIST[0].value = loc_0 reg_1 = FUNC_LIST[178](param_1 )store_i32(memory_at_0, loc_0 + 12, reg_1 )reg_1 = FUNC_LIST[178](param_2 )store_i32(memory_at_0, loc_0 + 8, reg_1 )reg_0 = FUNC_LIST[179](param_0 , add_i32(loc_0 , 12 ), add_i32(loc_0 , 8 ))GLOBAL_LIST[0].value = add_i32(loc_0 , 16 )break end end FUNC_LIST[173] =--[[ std::__2::pair<char const*, char*> std::__2::__copy_trivial_impl[abi:v160004]<char const, char>(char const*, char const*, char*) ]]function(param_0, param_1, param_2, param_3)local loc_0 = 0  local reg_0, reg_1 while true do loc_0 = sub_i32(GLOBAL_LIST[0].value , 16 )GLOBAL_LIST[0].value = loc_0 store_i32(memory_at_0, loc_0 + 12, param_2 )param_2 = sub_i32(param_2 , param_1 )reg_1 = FUNC_LIST[87](param_3 , param_1 , param_2 )store_i32(memory_at_0, loc_0 + 8, add_i32(reg_1 , param_2 ))FUNC_LIST[181](param_0 , add_i32(loc_0 , 12 ), add_i32(loc_0 , 8 ))GLOBAL_LIST[0].value = add_i32(loc_0 , 16 )break end end FUNC_LIST[174] =--[[ std::__2::__unwrap_iter_impl<char*, true>::__unwrap[abi:v160004](char*) ]]function(param_0)local reg_0 while true do reg_0 = FUNC_LIST[79](param_0 )break end return reg_0 end FUNC_LIST[175] =--[[ std::__2::pair<char const*, char*>::pair[abi:v160004]<char const*, char*, (void*)0>(char const*&&, char*&&) ]]function(param_0, param_1, param_2)local reg_0 while true do store_i32(memory_at_0, param_0 , load_i32(memory_at_0, param_1 ))store_i32(memory_at_0, param_0 + 4, load_i32(memory_at_0, param_2 ))reg_0 = param_0 break end return reg_0 end FUNC_LIST[176] =--[[ std::__2::__unwrap_range_impl<char const*, char const*>::__rewrap[abi:v160004](char const*, char const*) ]]function(param_0, param_1)local reg_0 while true do reg_0 = FUNC_LIST[183](param_0 , param_1 )break end return reg_0 end FUNC_LIST[177] =--[[ std::__2::__unwrap_iter_impl<char*, true>::__rewrap[abi:v160004](char*, char*) ]]function(param_0, param_1)local reg_0, reg_1, reg_2 while true do reg_2 = FUNC_LIST[79](param_0 )reg_0 = add_i32(param_0 , sub_i32(param_1 , reg_2 ))break end return reg_0 end FUNC_LIST[178] =--[[ decltype(std::__2::__unwrap_iter_impl<char const*, true>::__unwrap(std::declval<char const*>())) std::__2::__unwrap_iter[abi:v160004]<char const*, std::__2::__unwrap_iter_impl<char const*, true>, 0>(char const*) ]]function(param_0)local reg_0 while true do reg_0 = FUNC_LIST[180](param_0 )break end return reg_0 end FUNC_LIST[179] =--[[ std::__2::pair<char const*, char const*>::pair[abi:v160004]<char const*, char const*, (void*)0>(char const*&&, char const*&&) ]]function(param_0, param_1, param_2)local reg_0 while true do store_i32(memory_at_0, param_0 , load_i32(memory_at_0, param_1 ))store_i32(memory_at_0, param_0 + 4, load_i32(memory_at_0, param_2 ))reg_0 = param_0 break end return reg_0 end FUNC_LIST[180] =--[[ std::__2::__unwrap_iter_impl<char const*, true>::__unwrap[abi:v160004](char const*) ]]function(param_0)local reg_0 while true do reg_0 = FUNC_LIST[42](param_0 )break end return reg_0 end FUNC_LIST[181] =--[[ std::__2::pair<std::__2::__unwrap_ref_decay<char const*&>::type, std::__2::__unwrap_ref_decay<char*>::type> std::__2::make_pair[abi:v160004]<char const*&, char*>(char const*&, char*&&) ]]function(param_0, param_1, param_2)local reg_0 while true do reg_0 = FUNC_LIST[182](param_0 , param_1 , param_2 )break end end FUNC_LIST[182] =--[[ std::__2::pair<char const*, char*>::pair[abi:v160004]<char const*&, char*, (void*)0>(char const*&, char*&&) ]]function(param_0, param_1, param_2)local reg_0 while true do store_i32(memory_at_0, param_0 , load_i32(memory_at_0, param_1 ))store_i32(memory_at_0, param_0 + 4, load_i32(memory_at_0, param_2 ))reg_0 = param_0 break end return reg_0 end FUNC_LIST[183] =--[[ char const* std::__2::__rewrap_iter[abi:v160004]<char const*, char const*, std::__2::__unwrap_iter_impl<char const*, true>>(char const*, char const*) ]]function(param_0, param_1)local reg_0 while true do reg_0 = FUNC_LIST[184](param_0 , param_1 )break end return reg_0 end FUNC_LIST[184] =--[[ std::__2::__unwrap_iter_impl<char const*, true>::__rewrap[abi:v160004](char const*, char const*) ]]function(param_0, param_1)local reg_0, reg_1, reg_2 while true do reg_2 = FUNC_LIST[42](param_0 )reg_0 = add_i32(param_0 , sub_i32(param_1 , reg_2 ))break end return reg_0 end FUNC_LIST[185] =--[[ std::__2::__libcpp_deallocate[abi:v160004](void*, unsigned long, unsigned long) ]]function(param_0, param_1, param_2)local reg_0 local desired while true do while true do reg_0 = FUNC_LIST[158](param_2 )if reg_0 == 0 then break end FUNC_LIST[186](param_0 , param_1 , param_2 )desired = 0 break end if desired then if desired == 0 then desired = nil end break end FUNC_LIST[187](param_0 , param_1 )break end end FUNC_LIST[186] =--[[ void std::__2::__do_deallocate_handle_size[abi:v160004]<std::align_val_t>(void*, unsigned long, std::align_val_t) ]]function(param_0, param_1, param_2)while true do FUNC_LIST[188](param_0 , param_2 )break end end FUNC_LIST[187] =--[[ void std::__2::__do_deallocate_handle_size[abi:v160004]<>(void*, unsigned long) ]]function(param_0, param_1)while true do FUNC_LIST[189](param_0 )break end end FUNC_LIST[188] =--[[ void std::__2::__libcpp_operator_delete[abi:v160004]<void*, std::align_val_t>(void*, std::align_val_t) ]]function(param_0, param_1)while true do FUNC_LIST[103](param_0 , param_1 )break end end FUNC_LIST[189] =--[[ void std::__2::__libcpp_operator_delete[abi:v160004]<void*>(void*) ]]function(param_0)while true do FUNC_LIST[100](param_0 )break end end FUNC_LIST[190] =--[[ void (*std::__2::(anonymous namespace)::__libcpp_atomic_load[abi:v160004]<void (*)()>(void (* const*)(), int))() ]]function(param_0)local reg_0 while true do reg_0 = load_i32(memory_at_0, param_0 )break end return reg_0 end FUNC_LIST[191] =--[[ std::get_new_handler() ]]function()local reg_0 while true do reg_0 = FUNC_LIST[190](67604 )break end return reg_0 end FUNC_LIST[192] =--[[ setTempRet0 ]]function(param_0)while true do GLOBAL_LIST[1].value = param_0 break end end FUNC_LIST[193] =--[[ getTempRet0 ]]function()local reg_0 while true do reg_0 = GLOBAL_LIST[1].value break end return reg_0 end FUNC_LIST[194] =--[[ fflush ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local loc_2 = 0  local reg_0 local desired while true do while true do if param_0 ~= 0 then break end loc_0 = 0 while true do if load_i32(memory_at_0, 0 + 67592)== 0 then break end reg_0 = FUNC_LIST[194](load_i32(memory_at_0, 0 + 67592))loc_0 = reg_0 break end if desired then if desired == 1 then desired = nil end break end while true do if load_i32(memory_at_0, 0 + 67080)== 0 then break end reg_0 = FUNC_LIST[194](load_i32(memory_at_0, 0 + 67080))loc_0 = bor_i32(reg_0 , loc_0 )break end if desired then if desired == 1 then desired = nil end break end while true do reg_0 = FUNC_LIST[109]()param_0 = load_i32(memory_at_0, reg_0 )if param_0 == 0 then break end while true do loc_1 = 0 while true do if lt_i32(load_i32(memory_at_0, param_0 + 76), 0 )then break end reg_0 = FUNC_LIST[105](param_0 )loc_1 = reg_0 break end if desired then if desired == 3 then desired = nil continue end break end while true do if load_i32(memory_at_0, param_0 + 20)== load_i32(memory_at_0, param_0 + 28)then break end reg_0 = FUNC_LIST[194](param_0 )loc_0 = bor_i32(reg_0 , loc_0 )break end if desired then if desired == 3 then desired = nil continue end break end while true do if loc_1 == 0 then break end FUNC_LIST[106](param_0 )break end if desired then if desired == 3 then desired = nil continue end break end param_0 = load_i32(memory_at_0, param_0 + 56)if param_0 ~= 0 then continue end break end if desired then if desired == 2 then desired = nil end break end break end if desired then if desired == 1 then desired = nil end break end FUNC_LIST[110]()reg_0 = loc_0 desired = 0 break end if desired then if desired == 0 then desired = nil end break end loc_1 = 0 while true do if lt_i32(load_i32(memory_at_0, param_0 + 76), 0 )then break end reg_0 = FUNC_LIST[105](param_0 )loc_1 = reg_0 break end if desired then if desired == 0 then desired = nil end break end while true do while true do while true do if load_i32(memory_at_0, param_0 + 20)== load_i32(memory_at_0, param_0 + 28)then break end reg_0 = TABLE_LIST[0].data[load_i32(memory_at_0, param_0 + 36)](param_0 , 0 , 0 )if load_i32(memory_at_0, param_0 + 20)~= 0 then break end loc_0 = 4294967295 if loc_1 ~= 0 then desired = 2 break end desired = 1 break end if desired then if desired == 2 then desired = nil end break end while true do loc_0 = load_i32(memory_at_0, param_0 + 4)loc_2 = load_i32(memory_at_0, param_0 + 8)if loc_0 == loc_2 then break end reg_0 = TABLE_LIST[0].data[load_i32(memory_at_0, param_0 + 40)](param_0 , extend_i64_i32(sub_i32(loc_0 , loc_2 )), 1 )break end if desired then if desired == 2 then desired = nil end break end loc_0 = 0 store_i32(memory_at_0, param_0 + 28, 0 )store_i64(memory_at_0, param_0 + 16, i64_ZERO )store_i64(memory_at_0, param_0 + 4, i64_ZERO )if loc_1 == 0 then desired = 1 break end break end if desired then if desired == 1 then desired = nil end break end FUNC_LIST[106](param_0 )break end if desired then if desired == 0 then desired = nil end break end reg_0 = loc_0 break end return reg_0 end FUNC_LIST[195] =--[[ emscripten_stack_init ]]function()while true do GLOBAL_LIST[3].value = 65536 GLOBAL_LIST[2].value = band_i32(add_i32(0 , 15 ), 4294967280 )break end end FUNC_LIST[196] =--[[ emscripten_stack_get_free ]]function()local reg_0 while true do reg_0 = sub_i32(GLOBAL_LIST[0].value , GLOBAL_LIST[2].value )break end return reg_0 end FUNC_LIST[197] =--[[ emscripten_stack_get_base ]]function()local reg_0 while true do reg_0 = GLOBAL_LIST[3].value break end return reg_0 end FUNC_LIST[198] =--[[ emscripten_stack_get_end ]]function()local reg_0 while true do reg_0 = GLOBAL_LIST[2].value break end return reg_0 end FUNC_LIST[199] =--[[ stackSave ]]function()local reg_0 while true do reg_0 = GLOBAL_LIST[0].value break end return reg_0 end FUNC_LIST[200] =--[[ stackRestore ]]function(param_0)while true do GLOBAL_LIST[0].value = param_0 break end end FUNC_LIST[201] =--[[ stackAlloc ]]function(param_0)local loc_0 = 0  local loc_1 = 0  local reg_0 while true do loc_0 = band_i32(sub_i32(GLOBAL_LIST[0].value , param_0 ), 4294967280 )GLOBAL_LIST[0].value = loc_0 reg_0 = loc_0 break end return reg_0 end FUNC_LIST[202] =--[[ emscripten_stack_get_current ]]function()local reg_0 while true do reg_0 = GLOBAL_LIST[0].value break end return reg_0 end FUNC_LIST[203] =--[[ dynCall_jiji ]]function(param_0, param_1, param_2, param_3)local reg_0 while true do reg_0 = TABLE_LIST[0].data[param_0 ](param_1 , param_2 , param_3 )break end return reg_0 end FUNC_LIST[204] =--[[ legalstub$dynCall_jiji ]]function(param_0, param_1, param_2, param_3, param_4)local loc_0 = i64_ZERO  local reg_0 while true do reg_0 = FUNC_LIST[203](param_0 , param_1 , bor_i64(extend_i64_u32(param_2 ), shl_i64(extend_i64_u32(param_3 ), i64_from_u32(32, 0) )), param_4 )loc_0 = reg_0 FUNC_LIST[192](wrap_i32_i64(shr_u64(loc_0 , i64_from_u32(32, 0) )))reg_0 = wrap_i32_i64(loc_0 )break end return reg_0 end FUNC_LIST[205] =--[[ legalfunc$__wasi_fd_seek ]]function(param_0, param_1, param_2, param_3)local reg_0 while true do reg_0 = FUNC_LIST[5](param_0 , wrap_i32_i64(param_1 ), wrap_i32_i64(shr_u64(param_1 , i64_from_u32(32, 0) )), param_2 , param_3 )break end return reg_0 end local function run_init_code()TABLE_LIST[0] = { min = 4, max = 4, data = {} }MEMORY_LIST[0] = rt.allocator.new(256, 256)GLOBAL_LIST[0] = { value =65536 }GLOBAL_LIST[1] = { value =0 }GLOBAL_LIST[2] = { value =0 }GLOBAL_LIST[3] = { value =0 }do local target = TABLE_LIST[0].data local offset =1 local data = {FUNC_LIST[112],FUNC_LIST[114],FUNC_LIST[116],}table.move(data, 1, #data, offset, target)end rt.store.string(MEMORY_LIST[0],65536 ,"Part\x00Parent\x00nil\x00basic_string\x00Name\x00game.Workspace\x00 if type(ret) ~= \"string\" then ret = tostring(ret) end return ret end\x00 if inst and typeof(inst) == \"Instance\" then local cs = game:GetService(\"CollectionService\") local tags = cs:GetTags(inst) for i,v in ipairs(tags) do if #v > 6 and v:sub(1,6) == \"__dID_\" then return v end end local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(inst, dID) return dID end\x00\")[1] if inst then local cln = inst:Clone() local storage = game:GetService(\"ServerStorage\"):FindFirstChild(\"__CppStorage\") if not storage then storage = Instance.new(\"Folder\") storage.Name = \"__CppStorage\" storage.Parent = game:GetService(\"ServerStorage\") end cln.Parent = storage local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(cln, dID) return dID end\x00\")[1] return dID\x00\") local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) game:GetService(\"CollectionService\"):AddTag(inst, dID) return dID\x00\")[1] if inst then inst.\x00\")[1] if inst then local ret = inst.\x00\")\x00\"CppPart\"\x00local inst = Instance.new(\"\x00print(\"\x00local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"\x00\") local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) local cs = game:GetService(\"CollectionService\") cs:AddTag(inst, dID) inst.Parent = cs:GetTagged(\"\x00local inst = \x00PARTS CREATED? Name: \x00")rt.store.string(MEMORY_LIST[0],66928 ," \x08\x01\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x14\x08\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00x\x05\x01\x00")end  

--// Named function dictionary
local NamedFunctionList = {
    --// WebAssembly
    ["__wasm_call_ctors"] = FUNC_LIST[6];

	--// Standard Library
	["strlen"] = FUNC_LIST[89];
	["malloc"] = FUNC_LIST[93];

	--// Main
	["main"] = FUNC_LIST[85];
}

--// Pre-init environment function setup
local load_string = rt.load.string

--// lua_call
FUNC_LIST[7] = function (code)
	code = load_string(memory_at_0, code, NamedFunctionList.strlen(code))
	local ret = loadstring(code)() or ""
	if type(ret) ~= "string" then
		ret = tostring(ret)
	end
	
	local addr = NamedFunctionList.malloc(#ret + 1)
	rt.store.string(memory_at_0, addr, ret .. "\0", #ret + 1)
	return addr
end

--// Initialize
run_init_code()
memory_at_0 = MEMORY_LIST[0]
NamedFunctionList.__wasm_call_ctors()

local function main()
    return NamedFunctionList.main()
end

--// Return
return {
    --// Main functions
	["main"] = main;

    --// Wasynth related utilities
    NamedFunctionList = NamedFunctionList;
    rt = rt;
    FUNC_LIST = FUNC_LIST;
}