--!optimize 2
local lastSleep = 0
function peformSleepCheck()
    if tick() - lastSleep >= 1 then
        lastSleep = tick()
        task.wait()
    end
end

local function no_op(num)
    peformSleepCheck()
	return num
end

local bit_lshift = bit32.lshift
local bit_rshift = bit32.rshift
local bit_arshift = bit32.arshift

local bit_and = bit32.band
local bit_or = bit32.bor
local bit_xor = bit32.bxor
local bit_not = bit32.bnot

local bit_extract = bit32.extract
local bit_replace = bit32.replace

-- X: a[0 __21]
-- Y: a[22__31]
--  | b[0 __11]
-- Z: b[12__31]
local constructor = Vector3.new

local rt_i64_ZERO = constructor(0, 0, 0)

local function rt_i64_from_u32(data_1, data_2)
    peformSleepCheck()
	local x = bit_and(data_1, 0x3FFFFF)
	local y = bit_and(data_2, 0x3FFFFF)
	local z = bit_replace(bit_rshift(data_1, 22), bit_rshift(data_2, 22), 10, 10)

	return constructor(x, y, z)
end

local function rt_i64_is_zero(value)
    peformSleepCheck()
	return value == rt_i64_ZERO
end

local function load_d1(value)
    peformSleepCheck()
	return bit_replace(bit_and(value.X, 0x3FFFFF), value.Z, 22, 10)
end

local function load_d2(value)
    peformSleepCheck()
	return bit_replace(bit_and(value.Y, 0x3FFFFF), bit_rshift(value.Z, 10), 22, 10)
end

local function rt_i64_into_u32(value)
    peformSleepCheck()
	local x, y, z = value.X, value.Y, value.Z
	return bit_replace(bit_and(x, 0x3FFFFF), z, 22, 10), bit_replace(bit_and(y, 0x3FFFFF), bit_rshift(z, 10), 22, 10)
end

local function rt_i64_from_u64(value)
    peformSleepCheck()
	return rt_i64_from_u32(bit_and(value % 0x100000000), bit_and(value / 0x100000000))
end

local function rt_convert_f64_u64(value)
    peformSleepCheck()
	local value_1, value_2 = rt_i64_into_u32(value)
	return value_1 + value_2 * 0x100000000
end

local rt_i64_ONE = rt_i64_from_u64(1)
local NUM_SIX_FOUR = rt_i64_from_u64(64)
local NUM_BIT_26 = rt_i64_from_u64(0x4000000)
local NUM_BIT_52 = rt_i64_from_u64(0x10000000000000)

local function rt_add_i64(lhs, rhs)
    peformSleepCheck()
	local lhs_1, lhs_2 = rt_i64_into_u32(lhs)
	local rhs_1, rhs_2 = rt_i64_into_u32(rhs)
	local data_1 = lhs_1 + rhs_1
	local data_2 = lhs_2 + rhs_2

	if data_1 >= 0x100000000 then
		data_1 = data_1 - 0x100000000
		data_2 = data_2 + 1
	end

	if data_2 >= 0x100000000 then
		data_2 = data_2 - 0x100000000
	end

	return rt_i64_from_u32(data_1, data_2)
end

local function rt_sub_i64(lhs, rhs)
    peformSleepCheck()
	local lhs_1, lhs_2 = rt_i64_into_u32(lhs)
	local rhs_1, rhs_2 = rt_i64_into_u32(rhs)
	local data_1 = lhs_1 - rhs_1
	local data_2 = lhs_2 - rhs_2

	if data_1 < 0 then
		data_1 = data_1 + 0x100000000
		data_2 = data_2 - 1
	end

	if data_2 < 0 then
		data_2 = data_2 + 0x100000000
	end

	return rt_i64_from_u32(data_1, data_2)
end

local function rt_lt_u64(lhs, rhs)
    peformSleepCheck()
	local data_l_2 = load_d2(lhs)
	local data_r_2 = load_d2(rhs)

	return data_l_2 < data_r_2 or (data_l_2 == data_r_2 and load_d1(lhs) < load_d1(rhs))
end

local function rt_mul_i64(lhs, rhs)
    peformSleepCheck()
	if rt_i64_is_zero(lhs) or rt_i64_is_zero(rhs) then
		return rt_i64_ZERO
	elseif rt_lt_u64(lhs, NUM_BIT_26) and rt_lt_u64(rhs, NUM_BIT_26) then
		return rt_i64_from_u64(load_d1(lhs) * load_d1(rhs))
	end

	-- Divide each long into 4 chunks of 16 bits, and then add up 4x4 products_
	-- We can skip products that would overflow_
	local lhs_1, lhs_2 = rt_i64_into_u32(lhs)
	local rhs_1, rhs_2 = rt_i64_into_u32(rhs)

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

	return rt_i64_from_u32(data_1, data_2)
end

local function rt_bit_or_i64(lhs, rhs)
    peformSleepCheck()
	local x = bit_or(lhs.X, rhs.X)
	local y = bit_or(lhs.Y, rhs.Y)
	local z = bit_or(lhs.Z, rhs.Z)

	return constructor(x, y, z)
end

local function rt_shl_i64(lhs, rhs)
    peformSleepCheck()
	local count = rhs.X % 64

	if count == 0 then
		return lhs
	elseif count < 32 then
		local pad = 32 - count
		local lhs_1, lhs_2 = rt_i64_into_u32(lhs)

		local data_1 = bit_lshift(lhs_1, count)
		local data_2 = bit_replace(bit_rshift(lhs_1, pad), lhs_2, count, pad)

		return rt_i64_from_u32(data_1, data_2)
	else
		local lhs_1 = load_d1(lhs)

		return rt_i64_from_u32(0, bit_lshift(lhs_1, count - 32))
	end
end

local function rt_div_u64(lhs, rhs)
    peformSleepCheck()
	if rt_i64_is_zero(rhs) then
		error("division by zero")
	elseif rt_i64_is_zero(lhs) then
		return rt_i64_ZERO, rt_i64_ZERO
	elseif rt_lt_u64(lhs, NUM_BIT_52) and rt_lt_u64(rhs, NUM_BIT_52) then
		local lhs_u = rt_convert_f64_u64(lhs)
		local rhs_u = rt_convert_f64_u64(rhs)

		return rt_i64_from_u64(lhs_u / rhs_u), rt_i64_from_u64(lhs_u % rhs_u)
	end

	local quotient = rt_i64_ZERO
	local remainder = rt_i64_ZERO

	local num_1, num_2 = rt_i64_into_u32(lhs)

	for i = 63, 0, -1 do
		local rem_1, rem_2 = rt_i64_into_u32(rt_shl_i64(remainder, rt_i64_ONE))

		if i > 31 then
			rem_1 = bit_or(rem_1, bit_extract(num_2, i - 32, 1))
		else
			rem_1 = bit_or(rem_1, bit_extract(num_1, i, 1))
		end

		remainder = rt_i64_from_u32(rem_1, rem_2)

		if not rt_lt_u64(remainder, rhs) then
			remainder = rt_sub_i64(remainder, rhs)
			quotient = rt_bit_or_i64(quotient, rt_shl_i64(rt_i64_ONE, rt_i64_from_u32(i, 0)))
		end
	end

	return quotient, remainder
end

local function rt_i64_is_negative(value)
    peformSleepCheck()
	return value.Z >= 0x80000
end

local function rt_i64_negate(value)
    peformSleepCheck()
	local value_1, value_2 = rt_i64_into_u32(value)
	local data_1 = bit_not(value_1) + 1
	local data_2 = bit_not(value_2)

	if data_1 >= 0x100000000 then
		data_1 = data_1 - 0x100000000
		data_2 = data_2 + 1
	end

	if data_2 >= 0x100000000 then
		data_2 = data_2 - 0x100000000
	end

	return rt_i64_from_u32(data_1, data_2)
end

local function rt_div_i64(lhs, rhs)
    peformSleepCheck()
	local left_negative = rt_i64_is_negative(lhs)
	local right_negative = rt_i64_is_negative(rhs)

	if left_negative then
		lhs = rt_i64_negate(lhs)
	end

	if right_negative then
		rhs = rt_i64_negate(rhs)
	end

	local quotient, remainder = rt_div_u64(lhs, rhs)

	if left_negative ~= right_negative then
		quotient = rt_i64_negate(quotient)
	end

	if left_negative then
		remainder = rt_i64_negate(remainder)
	end

	return quotient, remainder
end

local function rt_bit_and_i64(lhs, rhs)
    peformSleepCheck()
	local x = bit_and(lhs.X, rhs.X)
	local y = bit_and(lhs.Y, rhs.Y)
	local z = bit_and(lhs.Z, rhs.Z)

	return constructor(x, y, z)
end

local function rt_bit_not_i64(value)
    peformSleepCheck()
	local x = bit_and(bit_not(value.X), 0xFFFFFF)
	local y = bit_and(bit_not(value.Y), 0xFFFFFF)
	local z = bit_and(bit_not(value.Z), 0xFFFFFF)

	return constructor(x, y, z)
end

local function rt_bit_xor_i64(lhs, rhs)
    peformSleepCheck()
	local x = bit_xor(lhs.X, rhs.X)
	local y = bit_xor(lhs.Y, rhs.Y)
	local z = bit_xor(lhs.Z, rhs.Z)

	return constructor(x, y, z)
end

local function rt_shr_u64(lhs, rhs)
    peformSleepCheck()
	local count = rhs.X % 64

	if count == 0 then
		return lhs
	elseif count < 32 then
		local lhs_1, lhs_2 = rt_i64_into_u32(lhs)

		local data_1 = bit_replace(bit_rshift(lhs_1, count), lhs_2, 32 - count, count)
		local data_2 = bit_rshift(lhs_2, count)

		return rt_i64_from_u32(data_1, data_2)
	else
		local lhs_2 = load_d2(lhs)

		return rt_i64_from_u32(bit_rshift(lhs_2, count - 32), 0)
	end
end

local function rt_shr_i64(lhs, rhs)
    peformSleepCheck()
	local count = rhs.X % 64

	if count == 0 then
		return lhs
	elseif count < 32 then
		local lhs_1, lhs_2 = rt_i64_into_u32(lhs)

		local data_1 = bit_replace(bit_rshift(lhs_1, count), lhs_2, 32 - count, count)
		local data_2 = bit_arshift(lhs_2, count)

		return rt_i64_from_u32(data_1, data_2)
	else
		local lhs_2 = load_d2(lhs)

		local data_1 = bit_arshift(lhs_2, count - 32)
		local data_2 = lhs_2 >= 0x80000000 and 0xFFFFFFFF or 0

		return rt_i64_from_u32(data_1, data_2)
	end
end

local function rt_rotl_i64(lhs, rhs)
    peformSleepCheck()
	if rt_i64_is_zero(rhs) then
		return lhs
	else
		local data_1 = rt_shl_i64(lhs, rhs)
		local data_2 = rt_shr_u64(lhs, rt_sub_i64(NUM_SIX_FOUR, rhs))

		return rt_bit_or_i64(data_1, data_2)
	end
end

local function rt_rotr_i64(lhs, rhs)
    peformSleepCheck()
	if rt_i64_is_zero(rhs) then
		return lhs
	else
		local data_1 = rt_shr_u64(lhs, rhs)
		local data_2 = rt_shl_i64(lhs, rt_sub_i64(NUM_SIX_FOUR, rhs))

		return rt_bit_or_i64(data_1, data_2)
	end
end

local function rt_eq_i64(lhs, rhs)
    peformSleepCheck()
	return lhs == rhs
end

local function rt_gt_u64(lhs, rhs)
    peformSleepCheck()
	local data_l_2 = load_d2(lhs)
	local data_r_2 = load_d2(rhs)

	return data_l_2 > data_r_2 or (data_l_2 == data_r_2 and load_d1(lhs) > load_d1(rhs))
end

local function rt_lt_i64(lhs, rhs)
    peformSleepCheck()
	local neg_a = rt_i64_is_negative(lhs)
	local neg_b = rt_i64_is_negative(rhs)

	if neg_a and not neg_b then
		return true
	elseif not neg_a and neg_b then
		return false
	else
		return rt_i64_is_negative(rt_sub_i64(lhs, rhs))
	end
end

local function rt_gt_i64(lhs, rhs)
    peformSleepCheck()
	local neg_a = rt_i64_is_negative(lhs)
	local neg_b = rt_i64_is_negative(rhs)

	if neg_a and not neg_b then
		return false
	elseif not neg_a and neg_b then
		return true
	else
		return rt_i64_is_negative(rt_sub_i64(rhs, lhs))
	end
end

local function rt_convert_f64_i32(num)
    peformSleepCheck()
	return bit_xor(num, 0x80000000) - 0x80000000
end

local assert = assert

local math_abs = math.abs
local math_fmod = math.fmod
local math_modf = math.modf
local math_round = math.round
local math_sign = math.sign
local math_min = math.min
local math_max = math.max

local function rt_add_i32(lhs, rhs)
    peformSleepCheck()
	return bit_or(lhs + rhs, 0)
end

local function rt_sub_i32(lhs, rhs)
    peformSleepCheck()
	return bit_or(lhs - rhs, 0)
end

local function rt_mul_i32(lhs, rhs)
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

local function rt_div_i32(lhs, rhs)
    peformSleepCheck()
	assert(rhs ~= 0, "division by zero")

	lhs = rt_convert_f64_i32(lhs)
	rhs = rt_convert_f64_i32(rhs)

	return bit_or(math_modf(lhs / rhs), 0)
end

local function rt_div_u32(lhs, rhs)
    peformSleepCheck()
	assert(rhs ~= 0, "division by zero")

	return bit_or(math_modf(lhs / rhs), 0)
end

local function rt_rem_i32(lhs, rhs)
    peformSleepCheck()
	assert(rhs ~= 0, "division by zero")

	lhs = rt_convert_f64_i32(lhs)
	rhs = rt_convert_f64_i32(rhs)

	return bit_or(math_fmod(lhs, rhs), 0)
end

local function rt_rem_i64(lhs, rhs)
    peformSleepCheck()
	local _, remainder = rt_div_i64(lhs, rhs)

	return remainder
end

local function rt_rem_u64(lhs, rhs)
    peformSleepCheck()
	local _, remainder = rt_div_u64(lhs, rhs)

	return remainder
end

local function rt_neg_f64(num)
    peformSleepCheck()
	return -num
end

local function rt_min_f64(lhs, rhs)
    peformSleepCheck()
	if rhs == rhs then
		return math_min(lhs, rhs)
	else
		return rhs
	end
end

local function rt_max_f64(lhs, rhs)
    peformSleepCheck()
	if rhs == rhs then
		return math_max(lhs, rhs)
	else
		return rhs
	end
end

local buffer_create = buffer.create

local CP_INSTANCE = buffer_create(8)

local buffer_write_f64 = buffer.writef64
local buffer_read_i8 = buffer.readi8

local function rt_copysign_f64(lhs, rhs)
    peformSleepCheck()
	buffer_write_f64(CP_INSTANCE, 0, rhs)

	if buffer_read_i8(CP_INSTANCE, 7) >= 0 then
		return (math_abs(lhs))
	else
		return -math_abs(lhs)
	end
end

local function rt_nearest_f32(num)
    peformSleepCheck()
	local result = math_round(num)

	if (math_abs(num) + 0_5) % 2 == 1 then
		return result - math_sign(result)
	else
		return result
	end
end

local bit_countlz = bit32.countlz
local bit_countrz = bit32.countrz

local function rt_popcnt_i32(num)
    peformSleepCheck()
	num = num - bit_and(bit_rshift(num, 1), 0x55555555)
	num = bit_and(num, 0x33333333) + bit_and(bit_rshift(num, 2), 0x33333333)
	num = bit_and((num + bit_rshift(num, 4)), 0x0F0F0F0F)
	num = num + bit_rshift(num, 8)
	num = num + bit_rshift(num, 16)
	return bit_and(num, 0x0000003F)
end

local function rt_clz_i64(num)
    peformSleepCheck()
	local data_1, data_2 = rt_i64_into_u32(num)
	local temp

	if data_2 == 0 then
		temp = bit_countlz(data_1) + 32
	else
		temp = bit_countlz(data_2)
	end

	return rt_i64_from_u32(temp, 0)
end

local function rt_ctz_i64(num)
    peformSleepCheck()
	local data_1, data_2 = rt_i64_into_u32(num)
	local temp

	if data_1 == 0 then
		temp = bit_countrz(data_2) + 32
	else
		temp = bit_countrz(data_1)
	end

	return rt_i64_from_u32(temp, 0)
end

local function rt_popcnt_i64(num)
    peformSleepCheck()
	local data_1, data_2 = rt_i64_into_u32(num)
	local temp = rt_popcnt_i32(data_1) + rt_popcnt_i32(data_2)

	return rt_i64_from_u32(temp, 0)
end

local function rt_le_i32(lhs, rhs)
    peformSleepCheck()
	return rt_convert_f64_i32(lhs) <= rt_convert_f64_i32(rhs)
end

local function rt_lt_i32(lhs, rhs)
    peformSleepCheck()
	return rt_convert_f64_i32(lhs) < rt_convert_f64_i32(rhs)
end

local function rt_ge_i32(lhs, rhs)
    peformSleepCheck()
	return rt_convert_f64_i32(lhs) >= rt_convert_f64_i32(rhs)
end

local function rt_gt_i32(lhs, rhs)
    peformSleepCheck()
	return rt_convert_f64_i32(lhs) > rt_convert_f64_i32(rhs)
end

local function rt_ne_i64(lhs, rhs)
    peformSleepCheck()
	return not rt_eq_i64(lhs, rhs)
end

local function rt_le_i64(lhs, rhs)
    peformSleepCheck()
	return rt_lt_i64(lhs, rhs) or rt_eq_i64(lhs, rhs)
end

local function rt_le_u64(lhs, rhs)
    peformSleepCheck()
	return rt_lt_u64(lhs, rhs) or rt_eq_i64(lhs, rhs)
end

local function rt_ge_i64(lhs, rhs)
    peformSleepCheck()
	return rt_gt_i64(lhs, rhs) or rt_eq_i64(lhs, rhs)
end

local function rt_ge_u64(lhs, rhs)
    peformSleepCheck()
	return rt_gt_u64(lhs, rhs) or rt_eq_i64(lhs, rhs)
end

local bit_lrotate = bit32.lrotate
local bit_rrotate = bit32.rrotate

local function rt_shl_i32(lhs, rhs)
    peformSleepCheck()
	return bit_lshift(lhs, rhs % 32)
end

local function rt_shr_u32(lhs, rhs)
    peformSleepCheck()
	return bit_rshift(lhs, rhs % 32)
end

local function rt_shr_i32(lhs, rhs)
    peformSleepCheck()
	return bit_arshift(lhs, rhs % 32)
end

local function rt_rotl_i32(lhs, rhs)
    peformSleepCheck()
	return bit_lrotate(lhs, rhs % 32)
end

local function rt_rotr_i32(lhs, rhs)
    peformSleepCheck()
	return bit_rrotate(lhs, rhs % 32)
end

local math_ceil = math.ceil
local math_floor = math.floor
local math_clamp = math.clamp

local NUM_MIN_I64 = rt_i64_from_u32(0, 0x80000000)
local NUM_MAX_I64 = rt_i64_from_u32(0xFFFFFFFF, 0x7FFFFFFF)
local NUM_MAX_U64 = rt_i64_from_u32(0xFFFFFFFF, 0xFFFFFFFF)

local function rt_truncate_f64(num)
    peformSleepCheck()
	if num >= 0 then
		return math_floor(num)
	else
		return math_ceil(num)
	end
end

local function rt_wrap_i32_i64(num)
    peformSleepCheck()
	local data_1, _ = rt_i64_into_u32(num)

	return data_1
end

local function rt_truncate_i32_f64(num)
    peformSleepCheck()
	return bit_or(rt_truncate_f64(num), 0)
end

local function rt_truncate_i64_f64(num)
    peformSleepCheck()
	if num < 0 then
		local temp = rt_i64_from_u64(-num)

		return rt_i64_negate(temp)
	else
		return rt_i64_from_u64(num)
	end
end

local function rt_truncate_u64_f64(num)
    peformSleepCheck()
	if num <= 0 then
		return rt_i64_ZERO
	else
		return rt_i64_from_u64(math_floor(num))
	end
end

local function rt_saturate_i32_f64(num)
    peformSleepCheck()
	local temp = math_clamp(rt_truncate_f64(num), -0x80000000, 0x7FFFFFFF)

	return bit_or(temp, 0)
end

local function rt_saturate_u32_f64(num)
    peformSleepCheck()
	return math_clamp(rt_truncate_f64(num), 0, 0xFFFFFFFF)
end

local function rt_saturate_i64_f64(num)
    peformSleepCheck()
	if num >= 2 ^ 63 - 1 then
		return NUM_MAX_I64
	elseif num <= -2 ^ 63 then
		return NUM_MIN_I64
	else
		return rt_truncate_i64_f64(num)
	end
end

local function rt_saturate_u64_f64(num)
    peformSleepCheck()
	if num >= 2 ^ 64 then
		return NUM_MAX_U64
	elseif num <= 0 then
		return rt_i64_ZERO
	else
		return rt_truncate_i64_f64(num)
	end
end

local function rt_extend_i32_n8(num)
    peformSleepCheck()
	num = bit_and(num, 0xFF)

	if num >= 0x80 then
		return bit_or(num - 0x100, 0)
	else
		return num
	end
end

local function rt_extend_i32_n16(num)
    peformSleepCheck()
	num = bit_and(num, 0xFFFF)

	if num >= 0x8000 then
		return bit_or(num - 0x10000, 0)
	else
		return num
	end
end

local function rt_extend_i64_n8(num)
    peformSleepCheck()
	local data_1, _ = rt_i64_into_u32(num)

	data_1 = bit_and(data_1, 0xFF)

	if data_1 >= 0x80 then
		local temp = rt_i64_from_u32(-data_1 + 0x100, 0)

		return rt_i64_negate(temp)
	else
		return rt_i64_from_u32(data_1, 0)
	end
end

local function rt_extend_i64_n16(num)
    peformSleepCheck()
	local data_1, _ = rt_i64_into_u32(num)

	data_1 = bit_and(data_1, 0xFFFF)

	if data_1 >= 0x8000 then
		local temp = rt_i64_from_u32(-data_1 + 0x10000, 0)

		return rt_i64_negate(temp)
	else
		return rt_i64_from_u32(data_1, 0)
	end
end

local function rt_extend_i64_n32(num)
    peformSleepCheck()
	local data_1, _ = rt_i64_into_u32(num)

	if data_1 >= 0x80000000 then
		local temp = rt_i64_from_u32(-data_1 + 0x100000000, 0)

		return rt_i64_negate(temp)
	else
		return rt_i64_from_u32(data_1, 0)
	end
end

local function rt_extend_i64_i32(num)
    peformSleepCheck()
	if num >= 0x80000000 then
		local temp = rt_i64_from_u32(-num + 0x100000000, 0)

		return rt_i64_negate(temp)
	else
		return rt_i64_from_u32(num, 0)
	end
end

local function rt_extend_i64_u32(num)
    peformSleepCheck()
	return rt_i64_from_u32(num, 0)
end

local function rt_convert_f64_i64(num)
    peformSleepCheck()
	if rt_i64_is_negative(num) then
		local temp = rt_i64_negate(num)

		return -rt_convert_f64_u64(temp)
	else
		return rt_convert_f64_u64(num)
	end
end

local RE_INSTANCE = buffer_create(8)

local buffer_read_f32 = buffer.readf32
local buffer_read_f64 = buffer.readf64
local buffer_read_u32 = buffer.readu32

local buffer_write_f32 = buffer.writef32
local buffer_write_u32 = buffer.writeu32

local function rt_reinterpret_i32_f32(num)
    peformSleepCheck()
	buffer_write_f32(RE_INSTANCE, 0, num)

	return buffer_read_u32(RE_INSTANCE, 0)
end

local function rt_reinterpret_i64_f64(num)
    peformSleepCheck()
	buffer_write_f64(RE_INSTANCE, 0, num)

	local data_1 = buffer_read_u32(RE_INSTANCE, 0)
	local data_2 = buffer_read_u32(RE_INSTANCE, 4)

	return rt_i64_from_u32(data_1, data_2)
end

local function rt_reinterpret_f32_i32(num)
    peformSleepCheck()
	buffer_write_u32(RE_INSTANCE, 0, num)

	return buffer_read_f32(RE_INSTANCE, 0)
end

local function rt_reinterpret_f64_i64(num)
    peformSleepCheck()
	local data_1, data_2 = rt_i64_into_u32(num)

	buffer_write_u32(RE_INSTANCE, 0, data_1)
	buffer_write_u32(RE_INSTANCE, 4, data_2)

	return buffer_read_f64(RE_INSTANCE, 0)
end

local string_sub = string.sub

local buffer_to_string = buffer.tostring
local buffer_from_string = buffer.fromstring

local buffer_len = buffer.len
local buffer_copy = buffer.copy
local buffer_fill = buffer.fill

local buffer_read_u8 = buffer.readu8
local buffer_read_i16 = buffer.readi16
local buffer_read_u16 = buffer.readu16
local buffer_read_i32 = buffer.readi32

local buffer_write_u8 = buffer.writeu8
local buffer_write_u16 = buffer.writeu16

local function rt_load_i32_i8(memory, addr)
    peformSleepCheck()
	return bit_or(buffer_read_i8(memory.data, addr), 0)
end

local function rt_load_i32_u8(memory, addr)
    peformSleepCheck()
	return buffer_read_u8(memory.data, addr)
end

local function rt_load_i32_i16(memory, addr)
    peformSleepCheck()
	return bit_or(buffer_read_i16(memory.data, addr), 0)
end

local function rt_load_i32_u16(memory, addr)
    peformSleepCheck()
	return buffer_read_u16(memory.data, addr)
end

local function rt_load_i32(memory, addr)
    peformSleepCheck()
	return buffer_read_u32(memory.data, addr)
end

local function rt_load_i64_i8(memory, addr)
    peformSleepCheck()
	local value = buffer_read_i8(memory.data, addr)

	if value >= 0 then
		return rt_i64_from_u32(value, 0)
	else
		return rt_i64_from_u32(value + 0x100000000, 0xFFFFFFFF)
	end
end

local function rt_load_i64_u8(memory, addr)
    peformSleepCheck()
	return rt_i64_from_u32(buffer_read_u8(memory.data, addr), 0)
end

local function rt_load_i64_i16(memory, addr)
    peformSleepCheck()
	local value = buffer_read_i16(memory.data, addr)

	if value >= 0 then
		return rt_i64_from_u32(value, 0)
	else
		return rt_i64_from_u32(value + 0x100000000, 0xFFFFFFFF)
	end
end

local function rt_load_i64_u16(memory, addr)
    peformSleepCheck()
	return rt_i64_from_u32(buffer_read_u16(memory.data, addr), 0)
end

local function rt_load_i64_i32(memory, addr)
    peformSleepCheck()
	local value = buffer_read_i32(memory.data, addr)

	if value >= 0 then
		return rt_i64_from_u32(value, 0)
	else
		return rt_i64_from_u32(value + 0x100000000, 0xFFFFFFFF)
	end
end

local function rt_load_i64_u32(memory, addr)
    peformSleepCheck()
	return rt_i64_from_u32(buffer_read_u32(memory.data, addr), 0)
end

local function rt_load_i64(memory, addr)
    peformSleepCheck()
	local data = memory.data
	local value_1 = buffer_read_u32(data, addr)
	local value_2 = buffer_read_u32(data, addr + 4)

	return rt_i64_from_u32(value_1, value_2)
end

local function rt_load_f32(memory, addr)
    peformSleepCheck()
	return buffer_read_f32(memory.data, addr)
end

local function rt_load_f64(memory, addr)
    peformSleepCheck()
	return buffer_read_f64(memory.data, addr)
end

local function rt_load_string(memory, addr, len)
    peformSleepCheck()
	local temp = buffer_create(len)

	buffer_copy(temp, 0, memory.data, addr, len)

	return buffer_to_string(temp)
end

local function rt_store_i32_n8(memory, addr, value)
    peformSleepCheck()
	buffer_write_u8(memory.data, addr, value)
end

local function rt_store_i32_n16(memory, addr, value)
    peformSleepCheck()
	buffer_write_u16(memory.data, addr, value)
end

local function rt_store_i32(memory, addr, value)
    peformSleepCheck()
	buffer_write_u32(memory.data, addr, value)
end

local function rt_store_i64_n8(memory, addr, value)
    peformSleepCheck()
	local value_1, _ = rt_i64_into_u32(value)

	buffer_write_u8(memory.data, addr, value_1)
end

local function rt_store_i64_n16(memory, addr, value)
    peformSleepCheck()
	local value_1, _ = rt_i64_into_u32(value)

	buffer_write_u16(memory.data, addr, value_1)
end

local function rt_store_i64_n32(memory, addr, value)
    peformSleepCheck()
	local value_1, _ = rt_i64_into_u32(value)

	buffer_write_u32(memory.data, addr, value_1)
end

local function rt_store_i64(memory, addr, value)
    peformSleepCheck()
	local data = memory.data
	local value_1, value_2 = rt_i64_into_u32(value)

	buffer_write_u32(data, addr, value_1)
	buffer_write_u32(data, addr + 4, value_2)
end

local function rt_store_f32(memory, addr, value)
    peformSleepCheck()
	buffer_write_f32(memory.data, addr, value)
end

local function rt_store_f64(memory, addr, value)
    peformSleepCheck()
	buffer_write_f64(memory.data, addr, value)
end

local function rt_store_string(memory, addr, data, len)
    peformSleepCheck()
	local content = if not len or len == #data then data else string_sub(data, 1, len)
	local temp = buffer_from_string(content)

	buffer_copy(memory.data, addr, temp)
end

local function rt_store_copy(memory_1, addr_1, memory_2, addr_2, len)
    peformSleepCheck()
	buffer_copy(memory_1.data, addr_1, memory_2.data, addr_2, len)
end

local function rt_store_fill(memory, addr, len, value)
    peformSleepCheck()
	buffer_fill(memory.data, addr, value, len)
end

local WASM_PAGE_SIZE = 65536

local function rt_allocator_new(min, max)
    peformSleepCheck()
	return { max = max, data = buffer_create(min * WASM_PAGE_SIZE) }
end

local function rt_allocator_size(memory)
    peformSleepCheck()
	return buffer_len(memory.data) / WASM_PAGE_SIZE
end

local function rt_allocator_grow(memory, num)
    peformSleepCheck()
	local old = rt_allocator_size(memory)
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
local function load_string(memory, addr, len)
    peformSleepCheck()
    peformSleepCheck()
    local temp = buffer_create(len)

    buffer_copy(temp, 0, memory.data, addr, len)

    return buffer_to_string(temp)
end

local function store_string(memory, addr, data, len)
    peformSleepCheck()
    peformSleepCheck()
    local content = if not len or len == #data then data else string_sub(data, 1, len)
    local temp = buffer_from_string(content)

    buffer_copy(memory.data, addr, temp)
end

local function to_signed(num)
    peformSleepCheck()
    return num >= 0x80000000 and num - 0x100000000 or num
end

local function truncate_f64(num)
    peformSleepCheck()
	if num >= 0 then
		return (math_floor(num))
	else
		return (math_ceil(num))
	end
end

local function rt_truncate_u32_f32(num)
    peformSleepCheck()
    return (to_signed(truncate_f64(num)))
end

local memory_at_0
local FUNC_LIST = table.create(512)
local TABLE_LIST = table.create(0)
local MEMORY_LIST = table.create(0)
local GLOBAL_LIST = table.create(0)
FUNC_LIST[4] = --[[ __wasm_call_ctors ]] function()
	while true do
		FUNC_LIST[185]()
		FUNC_LIST[253]()
		break
	end
end
FUNC_LIST[5] = --[[ lua_call ]] function(loc_0)
	local loc_1 = 0
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local reg_0
	local desired
	while true do
		loc_1 = rt_sub_i32(GLOBAL_LIST[0].value, 32)
		GLOBAL_LIST[0].value = loc_1
		while true do
			while true do
				reg_0 = FUNC_LIST[192](loc_0)
				loc_2 = reg_0
				if loc_2 < 2147483640 then
					while true do
						while true do
							while true do
								if loc_2 >= 11 then
									while true do
										loc_4 = rt_add_i32(bit_or(loc_2, 7), 1)
										reg_0 = FUNC_LIST[227](loc_4)
										loc_3 = reg_0
										rt_store_i32(memory_at_0, loc_1 + 28, bit_or(loc_4, 2147483648))
										rt_store_i32(memory_at_0, loc_1 + 20, loc_3)
										rt_store_i32(memory_at_0, loc_1 + 24, loc_2)
										desired = 5
										break
									end
									if desired then
										if desired == 5 then
											desired = nil
										end
										break
									end
								end
								rt_store_i32_n8(memory_at_0, loc_1 + 31, loc_2)
								loc_3 = rt_add_i32(loc_1, 20)
								if loc_2 == 0 then
									desired = 4
									break
								end
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
							reg_0 = FUNC_LIST[188](loc_3, loc_0, loc_2)
							break
						end
						if desired then
							break
						end
						loc_0 = 0
						rt_store_i32_n8(memory_at_0, rt_add_i32(loc_2, loc_3), 0)
						rt_store_i32_n8(memory_at_0, loc_1 + 8, 0)
						rt_store_i32_n8(memory_at_0, loc_1 + 19, 0)
						loc_4 = rt_load_i32_i8(memory_at_0, loc_1 + 31)
						loc_3 = (if rt_lt_i32(loc_4, 0) then 1 else 0)
						loc_2 = (if loc_3 ~= 0 then rt_load_i32(memory_at_0, loc_1 + 24) else loc_4)
						if loc_2 == 0 then
							while true do
								loc_2 = rt_load_i32(memory_at_0, loc_1 + 8)
								reg_0 = 0
								desired = 1
								break
							end
							if desired then
								break
							end
						end
						loc_3 = (if loc_3 ~= 0 then rt_load_i32(memory_at_0, loc_1 + 20) else rt_add_i32(loc_1, 20))
						while true do
							FUNC_LIST[340](rt_add_i32(loc_1, 8), rt_load_i32_i8(memory_at_0, rt_add_i32(loc_0, loc_3)))
							loc_0 = rt_add_i32(loc_0, 1)
							if loc_0 < loc_2 then
								continue
							end
							break
						end
						if desired then
							break
						end
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				FUNC_LIST[6](rt_add_i32(loc_1, 20))
				error("out of code bounds")
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_2 = rt_load_i32(memory_at_0, loc_1 + 8)
			loc_0 = 0
			reg_0 = loc_0
			if rt_ge_i32(rt_load_i32_i8(memory_at_0, loc_1 + 19), 0) then
				break
			end
			FUNC_LIST[230](loc_2)
			reg_0 = 1
			break
		end
		loc_0 = reg_0
		if rt_lt_i32(loc_4, 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1 + 20))
				break
			end
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_1, 32)
		reg_0 = (if loc_0 ~= 0 then loc_2 else rt_add_i32(loc_1, 8))
		break
	end
	return reg_0
end
FUNC_LIST[6] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__throw_length_error[abi:ne180100]() const ]] function(loc_0)
	while true do
		FUNC_LIST[19](1184)
		error("out of code bounds")
	end
end
FUNC_LIST[7] = --[[ RBX::print(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>) ]] function(loc_0)
	local loc_1 = 0
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local reg_0
	local reg_1
	while true do
		loc_1 = rt_sub_i32(GLOBAL_LIST[0].value, 48)
		GLOBAL_LIST[0].value = loc_1
		rt_store_i32(memory_at_0, loc_1 + 4, rt_load_i32(memory_at_0, 2306))
		rt_store_i32_n8(memory_at_0, loc_1 + 15, 7)
		rt_store_i32(memory_at_0, loc_1 + 7, rt_load_i32(memory_at_0, 2309))
		rt_store_i32_n8(memory_at_0, loc_1 + 11, 0)
		loc_2 = rt_load_i32_i8(memory_at_0, loc_0 + 11)
		loc_3 = (if rt_lt_i32(loc_2, 0) then 1 else 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_1, 4), (if loc_3 ~= 0 then rt_load_i32(memory_at_0, loc_0) else loc_0), (if loc_3 ~= 0 then rt_load_i32(memory_at_0, loc_0 + 4) else loc_2))
		loc_0 = reg_1
		loc_4 = loc_0
		loc_2 = rt_add_i32(loc_4, 8)
		rt_store_i32(memory_at_0, loc_1 + 24, rt_load_i32(memory_at_0, loc_2))
		rt_store_i64(memory_at_0, loc_1 + 16, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_4 + 8, 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_1, 16), 2275, 2)
		loc_0 = reg_1
		loc_5 = loc_0
		loc_2 = rt_add_i32(loc_5, 8)
		rt_store_i32(memory_at_0, loc_1 + 40, rt_load_i32(memory_at_0, loc_2))
		rt_store_i64(memory_at_0, loc_1 + 32, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_5 + 8, 0)
		loc_2 = rt_load_i32(memory_at_0, loc_1 + 32)
		loc_0 = rt_load_i32_i8(memory_at_0, loc_1 + 43)
		reg_0 = FUNC_LIST[5]((if rt_lt_i32(loc_0, 0) then loc_2 else rt_add_i32(loc_1, 32)))
		if rt_lt_i32(loc_0, 0) then
			while true do
				FUNC_LIST[230](loc_2)
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_1 + 27), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1 + 16))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_1 + 15), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1 + 4))
				break
			end
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_1, 48)
		break
	end
end
FUNC_LIST[8] = --[[ RBX::error(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>) ]] function(loc_0)
	local loc_1 = 0
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local reg_0
	local reg_1
	while true do
		loc_1 = rt_sub_i32(GLOBAL_LIST[0].value, 48)
		GLOBAL_LIST[0].value = loc_1
		rt_store_i32(memory_at_0, loc_1 + 4, rt_load_i32(memory_at_0, 2314))
		rt_store_i32_n8(memory_at_0, loc_1 + 15, 7)
		rt_store_i32(memory_at_0, loc_1 + 7, rt_load_i32(memory_at_0, 2317))
		rt_store_i32_n8(memory_at_0, loc_1 + 11, 0)
		loc_2 = rt_load_i32_i8(memory_at_0, loc_0 + 11)
		loc_3 = (if rt_lt_i32(loc_2, 0) then 1 else 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_1, 4), (if loc_3 ~= 0 then rt_load_i32(memory_at_0, loc_0) else loc_0), (if loc_3 ~= 0 then rt_load_i32(memory_at_0, loc_0 + 4) else loc_2))
		loc_0 = reg_1
		loc_4 = loc_0
		loc_2 = rt_add_i32(loc_4, 8)
		rt_store_i32(memory_at_0, loc_1 + 24, rt_load_i32(memory_at_0, loc_2))
		rt_store_i64(memory_at_0, loc_1 + 16, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_4 + 8, 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_1, 16), 2275, 2)
		loc_0 = reg_1
		loc_5 = loc_0
		loc_2 = rt_add_i32(loc_5, 8)
		rt_store_i32(memory_at_0, loc_1 + 40, rt_load_i32(memory_at_0, loc_2))
		rt_store_i64(memory_at_0, loc_1 + 32, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_5 + 8, 0)
		loc_2 = rt_load_i32(memory_at_0, loc_1 + 32)
		loc_0 = rt_load_i32_i8(memory_at_0, loc_1 + 43)
		reg_0 = FUNC_LIST[5]((if rt_lt_i32(loc_0, 0) then loc_2 else rt_add_i32(loc_1, 32)))
		if rt_lt_i32(loc_0, 0) then
			while true do
				FUNC_LIST[230](loc_2)
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_1 + 27), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1 + 16))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_1 + 15), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1 + 4))
				break
			end
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_1, 48)
		break
	end
end
FUNC_LIST[9] = --[[ RBX::Instance::GetInstance(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>) ]] function(loc_0)
	local loc_1 = 0
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local reg_0
	local reg_1
	local desired
	local br_map = {}
	while true do
		loc_1 = rt_add_i32(GLOBAL_LIST[0].value, 4294967232)
		GLOBAL_LIST[0].value = loc_1
		reg_1 = FUNC_LIST[227](16)
		loc_2 = reg_1
		rt_store_i32(memory_at_0, loc_1 + 12, loc_2)
		rt_store_i64(memory_at_0, loc_1 + 16, rt_i64_from_u32(13, 2147483664))
		rt_store_i64(memory_at_0, loc_2 + 5, rt_load_i64(memory_at_0, 2628))
		rt_store_i64(memory_at_0, loc_2, rt_load_i64(memory_at_0, 2623))
		rt_store_i32_n8(memory_at_0, loc_2 + 13, 0)
		loc_2 = rt_load_i32_i8(memory_at_0, loc_0 + 11)
		loc_3 = (if rt_lt_i32(loc_2, 0) then 1 else 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_1, 12), (if loc_3 ~= 0 then rt_load_i32(memory_at_0, loc_0) else loc_0), (if loc_3 ~= 0 then rt_load_i32(memory_at_0, loc_0 + 4) else loc_2))
		loc_0 = reg_1
		loc_4 = loc_0
		loc_2 = rt_add_i32(loc_4, 8)
		rt_store_i32(memory_at_0, loc_1 + 32, rt_load_i32(memory_at_0, loc_2))
		rt_store_i64(memory_at_0, loc_1 + 24, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_4 + 8, 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_1, 24), 1304, 319)
		loc_0 = reg_1
		loc_5 = loc_0
		loc_2 = rt_add_i32(loc_5, 8)
		rt_store_i32(memory_at_0, loc_1 + 56, rt_load_i32(memory_at_0, loc_2))
		rt_store_i64(memory_at_0, loc_1 + 48, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_5 + 8, 0)
		reg_0 = FUNC_LIST[5]((if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_1 + 59), 0) then rt_load_i32(memory_at_0, loc_1 + 48) else rt_add_i32(loc_1, 48)))
		loc_3 = reg_0
		reg_0 = FUNC_LIST[192](loc_3)
		loc_0 = reg_0
		if loc_0 < 2147483640 then
			while true do
				while true do
					while true do
						if loc_0 >= 11 then
							while true do
								loc_6 = rt_add_i32(bit_or(loc_0, 7), 1)
								reg_0 = FUNC_LIST[227](loc_6)
								loc_2 = reg_0
								rt_store_i32(memory_at_0, loc_1 + 44, bit_or(loc_6, 2147483648))
								rt_store_i32(memory_at_0, loc_1 + 36, loc_2)
								rt_store_i32(memory_at_0, loc_1 + 40, loc_0)
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						rt_store_i32_n8(memory_at_0, loc_1 + 47, loc_0)
						loc_2 = rt_add_i32(loc_1, 36)
						if loc_0 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					reg_0 = FUNC_LIST[189](loc_2, loc_3, loc_0)
					break
				end
				if desired then
					break
				end
				rt_store_i32_n8(memory_at_0, rt_add_i32(loc_0, loc_2), 0)
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_1 + 59), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1 + 48))
						break
					end
					if desired then
						break
					end
				end
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_1 + 35), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1 + 24))
						break
					end
					if desired then
						break
					end
				end
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_1 + 23), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1 + 12))
						break
					end
					if desired then
						break
					end
				end
				loc_0 = 0
				while true do
					while true do
						while true do
							loc_3 = rt_load_i32(memory_at_0, loc_1 + 40)
							loc_2 = rt_load_i32_i8(memory_at_0, loc_1 + 47)
							if not br_map[1] then
								br_map[1] = (function()
									return { [0] = 2, 1, 1, 0, }
								end)()
							end
							temp = br_map[1][(if rt_lt_i32(loc_2, 0) then loc_3 else loc_2)] or 1
							if temp < 1 then
								break
							elseif temp > 1 then
								desired = 2
								break
							else
								desired = 3
								break
							end
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
						reg_0 = FUNC_LIST[191]((if rt_lt_i32(loc_2, 0) then rt_load_i32(memory_at_0, loc_1 + 36) else rt_add_i32(loc_1, 36)), 1159, 3)
						if reg_0 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					while true do
						if rt_ge_i32(loc_2, 0) then
							while true do
								rt_store_i32(memory_at_0, loc_1 + 8, rt_load_i32(memory_at_0, loc_1 + 44))
								rt_store_i64(memory_at_0, loc_1, rt_load_i64(memory_at_0, loc_1 + 36))
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						FUNC_LIST[332](loc_1, rt_load_i32(memory_at_0, loc_1 + 36), loc_3)
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					reg_0 = FUNC_LIST[227](12)
					loc_0 = reg_0
					while true do
						if rt_ge_i32(rt_load_i32_i8(memory_at_0, loc_1 + 11), 0) then
							while true do
								rt_store_i32(memory_at_0, loc_1 + 56, rt_load_i32(memory_at_0, loc_1 + 8))
								rt_store_i64(memory_at_0, loc_1 + 48, rt_load_i64(memory_at_0, loc_1))
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						FUNC_LIST[332](rt_add_i32(loc_1, 48), rt_load_i32(memory_at_0, loc_1), rt_load_i32(memory_at_0, loc_1 + 4))
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
					loc_3 = rt_add_i32(loc_0, 8)
					rt_store_i32(memory_at_0, loc_3, 0)
					while true do
						if rt_ge_i32(rt_load_i32_i8(memory_at_0, loc_1 + 59), 0) then
							while true do
								rt_store_i64(memory_at_0, loc_0, rt_load_i64(memory_at_0, loc_1 + 48))
								rt_store_i32(memory_at_0, loc_0 + 8, rt_load_i32(memory_at_0, loc_1 + 56))
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						reg_0 = FUNC_LIST[339](loc_0, rt_load_i32(memory_at_0, loc_1 + 48), rt_load_i32(memory_at_0, loc_1 + 52))
						if rt_ge_i32(rt_load_i32_i8(memory_at_0, loc_1 + 59), 0) then
							break
						end
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1 + 48))
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					if rt_ge_i32(rt_load_i32_i8(memory_at_0, loc_1 + 11), 0) then
						break
					end
					FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1))
					break
				end
				if desired then
					break
				end
				if rt_lt_i32(loc_2, 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1 + 36))
						break
					end
					if desired then
						break
					end
				end
				GLOBAL_LIST[0].value = rt_sub_i32(loc_1, 4294967232)
				reg_0 = loc_0
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		FUNC_LIST[6](rt_add_i32(loc_1, 36))
		error("out of code bounds")
	end
	return reg_0
end
FUNC_LIST[10] = --[[ __original_main ]] function()
	local loc_0 = 0
	local loc_1 = 0
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local reg_0
	local reg_1
	local reg_2
	while true do
		loc_0 = rt_sub_i32(GLOBAL_LIST[0].value, 176)
		GLOBAL_LIST[0].value = loc_0
		reg_0 = FUNC_LIST[227](12)
		loc_1 = reg_0
		rt_store_i32_n8(memory_at_0, loc_0 + 168, 0)
		rt_store_i32(memory_at_0, loc_0 + 164, 1953653072)
		rt_store_i32_n8(memory_at_0, loc_0 + 175, 4)
		reg_1 = FUNC_LIST[227](16)
		loc_2 = reg_1
		rt_store_i32(memory_at_0, loc_0 + 152, loc_2)
		rt_store_i64(memory_at_0, loc_0 + 156, rt_i64_from_u32(14, 2147483664))
		rt_store_i64(memory_at_0, loc_2 + 6, rt_load_i64(memory_at_0, 1225))
		rt_store_i64(memory_at_0, loc_2, rt_load_i64(memory_at_0, 1219))
		rt_store_i32_n8(memory_at_0, loc_2 + 14, 0)
		reg_2 = FUNC_LIST[9](rt_add_i32(loc_0, 152))
		reg_0 = FUNC_LIST[11](loc_1, rt_add_i32(loc_0, 164), reg_2, 0)
		loc_2 = reg_0
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_0 + 163), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_0 + 152))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_0 + 175), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_0 + 164))
				break
			end
		end
		rt_store_i32_n8(memory_at_0, loc_0 + 144, 0)
		rt_store_i32(memory_at_0, loc_0 + 140, 1701667150)
		rt_store_i32_n8(memory_at_0, loc_0 + 151, 4)
		rt_store_i32(memory_at_0, loc_0 + 128, 1102)
		rt_store_i32(memory_at_0, loc_0 + 124, 1)
		FUNC_LIST[13](loc_2, rt_add_i32(loc_0, 140), rt_add_i32(loc_0, 124))
		loc_1 = rt_load_i32(memory_at_0, loc_0 + 124)
		if loc_1 ~= 0 then
			while true do
				reg_0 = TABLE_LIST[0].data[loc_1](0, rt_add_i32(loc_0, 124), 0, 0, 0)
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_0 + 151), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_0 + 140))
				break
			end
		end
		rt_store_i32_n16(memory_at_0, loc_0 + 116, rt_load_i32_u16(memory_at_0, 1114))
		rt_store_i32_n8(memory_at_0, loc_0 + 118, 0)
		rt_store_i32(memory_at_0, loc_0 + 112, rt_load_i32(memory_at_0, 1110))
		rt_store_i32_n8(memory_at_0, loc_0 + 123, 6)
		reg_1 = FUNC_LIST[227](16)
		loc_1 = reg_1
		rt_store_i32(memory_at_0, loc_0 + 84, loc_1)
		rt_store_i64(memory_at_0, loc_0 + 88, rt_i64_from_u32(14, 2147483664))
		rt_store_i64(memory_at_0, loc_1 + 6, rt_load_i64(memory_at_0, 1225))
		rt_store_i64(memory_at_0, loc_1, rt_load_i64(memory_at_0, 1219))
		rt_store_i32_n8(memory_at_0, loc_1 + 14, 0)
		reg_0 = FUNC_LIST[9](rt_add_i32(loc_0, 84))
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_0 + 96, 2)
		rt_store_i32(memory_at_0, loc_0 + 100, loc_1)
		FUNC_LIST[13](loc_2, rt_add_i32(loc_0, 112), rt_add_i32(loc_0, 96))
		loc_1 = rt_load_i32(memory_at_0, loc_0 + 96)
		if loc_1 ~= 0 then
			while true do
				reg_0 = TABLE_LIST[0].data[loc_1](0, rt_add_i32(loc_0, 96), 0, 0, 0)
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_0 + 95), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_0 + 84))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_0 + 123), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_0 + 112))
				break
			end
		end
		reg_0 = FUNC_LIST[15](loc_2)
		loc_3 = reg_0
		rt_store_i32_n16(memory_at_0, loc_0 + 76, rt_load_i32_u16(memory_at_0, 1114))
		rt_store_i32_n8(memory_at_0, loc_0 + 78, 0)
		rt_store_i32(memory_at_0, loc_0 + 72, rt_load_i32(memory_at_0, 1110))
		rt_store_i32_n8(memory_at_0, loc_0 + 83, 6)
		reg_1 = FUNC_LIST[227](16)
		loc_1 = reg_1
		rt_store_i32(memory_at_0, loc_0 + 60, loc_1)
		rt_store_i64(memory_at_0, loc_0 + 64, rt_i64_from_u32(14, 2147483664))
		rt_store_i64(memory_at_0, loc_1 + 6, rt_load_i64(memory_at_0, 1225))
		rt_store_i64(memory_at_0, loc_1, rt_load_i64(memory_at_0, 1219))
		rt_store_i32_n8(memory_at_0, loc_1 + 14, 0)
		FUNC_LIST[16](loc_3, rt_add_i32(loc_0, 72), rt_add_i32(loc_0, 60))
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_0 + 71), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_0 + 60))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_0 + 83), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_0 + 72))
				break
			end
		end
		reg_1 = FUNC_LIST[227](24)
		loc_1 = reg_1
		rt_store_i32(memory_at_0, loc_0 + 36, loc_1)
		rt_store_i64(memory_at_0, loc_0 + 40, rt_i64_from_u32(21, 2147483672))
		rt_store_i64(memory_at_0, loc_1 + 13, rt_load_i64(memory_at_0, 2665))
		rt_store_i64(memory_at_0, loc_1 + 8, rt_load_i64(memory_at_0, 2660))
		rt_store_i64(memory_at_0, loc_1, rt_load_i64(memory_at_0, 2652))
		rt_store_i32_n8(memory_at_0, loc_1 + 21, 0)
		rt_store_i32_n8(memory_at_0, loc_0 + 23, 4)
		rt_store_i32_n8(memory_at_0, loc_0 + 16, 0)
		rt_store_i32(memory_at_0, loc_0 + 12, 1701667150)
		FUNC_LIST[17](rt_add_i32(loc_0, 24), loc_2, rt_add_i32(loc_0, 12))
		loc_2 = rt_load_i32_i8(memory_at_0, loc_0 + 35)
		loc_1 = (if rt_lt_i32(loc_2, 0) then 1 else 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_0, 36), (if loc_1 ~= 0 then rt_load_i32(memory_at_0, loc_0 + 24) else rt_add_i32(loc_0, 24)), (if loc_1 ~= 0 then rt_load_i32(memory_at_0, loc_0 + 28) else loc_2))
		loc_2 = reg_1
		loc_4 = loc_2
		loc_1 = rt_add_i32(loc_4, 8)
		rt_store_i32(memory_at_0, loc_0 + 56, rt_load_i32(memory_at_0, loc_1))
		rt_store_i64(memory_at_0, loc_0 + 48, rt_load_i64(memory_at_0, loc_2))
		rt_store_i64(memory_at_0, loc_2, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_4 + 8, 0)
		FUNC_LIST[7](rt_add_i32(loc_0, 48))
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_0 + 59), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_0 + 48))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_0 + 35), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_0 + 24))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_0 + 23), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_0 + 12))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_0 + 47), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_0 + 36))
				break
			end
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_0, 176)
		reg_0 = 0
		break
	end
	return reg_0
end
FUNC_LIST[11] = --[[ RBX::Instance::Instance(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, RBX::Instance*, bool) ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local loc_8 = 0
	local loc_9 = 0
	local loc_10 = 0
	local loc_11 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_4 = rt_sub_i32(GLOBAL_LIST[0].value, 96)
		GLOBAL_LIST[0].value = loc_4
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_0 + 8, 0)
		while true do
			if loc_3 == 0 then
				while true do
					if loc_2 ~= 0 then
						while true do
							reg_1 = FUNC_LIST[227](32)
							loc_3 = reg_1
							rt_store_i32(memory_at_0, loc_4 + 20, loc_3)
							rt_store_i64(memory_at_0, loc_4 + 24, rt_i64_from_u32(27, 2147483680))
							rt_store_i32(memory_at_0, loc_3 + 23, rt_load_i32(memory_at_0, 2301))
							rt_store_i64(memory_at_0, loc_3 + 16, rt_load_i64(memory_at_0, 2294))
							rt_store_i64(memory_at_0, loc_3 + 8, rt_load_i64(memory_at_0, 2286))
							rt_store_i64(memory_at_0, loc_3, rt_load_i64(memory_at_0, 2278))
							rt_store_i32_n8(memory_at_0, loc_3 + 27, 0)
							loc_3 = rt_load_i32_i8(memory_at_0, loc_1 + 11)
							loc_5 = (if rt_lt_i32(loc_3, 0) then 1 else 0)
							reg_1 = FUNC_LIST[337](rt_add_i32(loc_4, 20), (if loc_5 ~= 0 then rt_load_i32(memory_at_0, loc_1) else loc_1), (if loc_5 ~= 0 then rt_load_i32(memory_at_0, loc_1 + 4) else loc_3))
							loc_1 = reg_1
							loc_6 = loc_1
							loc_3 = rt_add_i32(loc_6, 8)
							rt_store_i32(memory_at_0, loc_4 + 40, rt_load_i32(memory_at_0, loc_3))
							rt_store_i64(memory_at_0, loc_4 + 32, rt_load_i64(memory_at_0, loc_1))
							rt_store_i64(memory_at_0, loc_1, rt_i64_ZERO)
							rt_store_i32(memory_at_0, loc_6 + 8, 0)
							reg_1 = FUNC_LIST[337](rt_add_i32(loc_4, 32), 2398, 175)
							loc_1 = reg_1
							loc_7 = loc_1
							loc_3 = rt_add_i32(loc_7, 8)
							rt_store_i32(memory_at_0, loc_4 + 56, rt_load_i32(memory_at_0, loc_3))
							rt_store_i64(memory_at_0, loc_4 + 48, rt_load_i64(memory_at_0, loc_1))
							rt_store_i64(memory_at_0, loc_1, rt_i64_ZERO)
							rt_store_i32(memory_at_0, loc_7 + 8, 0)
							while true do
								if rt_ge_i32(rt_load_i32_i8(memory_at_0, loc_2 + 11), 0) then
									while true do
										rt_store_i32(memory_at_0, loc_4 + 16, rt_load_i32(memory_at_0, loc_2 + 8))
										rt_store_i64(memory_at_0, loc_4 + 8, rt_load_i64(memory_at_0, loc_2))
										desired = 4
										break
									end
									if desired then
										if desired == 4 then
											desired = nil
										end
										break
									end
								end
								FUNC_LIST[332](rt_add_i32(loc_4, 8), rt_load_i32(memory_at_0, loc_2), rt_load_i32(memory_at_0, loc_2 + 4))
								break
							end
							if desired then
								break
							end
							loc_1 = rt_load_i32_i8(memory_at_0, loc_4 + 19)
							loc_2 = (if rt_lt_i32(loc_1, 0) then 1 else 0)
							reg_1 = FUNC_LIST[337](rt_add_i32(loc_4, 48), (if loc_2 ~= 0 then rt_load_i32(memory_at_0, loc_4 + 8) else rt_add_i32(loc_4, 8)), (if loc_2 ~= 0 then rt_load_i32(memory_at_0, loc_4 + 12) else loc_1))
							loc_1 = reg_1
							loc_8 = loc_1
							loc_2 = rt_add_i32(loc_8, 8)
							rt_store_i32(memory_at_0, loc_4 + 72, rt_load_i32(memory_at_0, loc_2))
							rt_store_i64(memory_at_0, loc_4 + 64, rt_load_i64(memory_at_0, loc_1))
							rt_store_i64(memory_at_0, loc_1, rt_i64_ZERO)
							rt_store_i32(memory_at_0, loc_8 + 8, 0)
							reg_1 = FUNC_LIST[337](rt_sub_i32(loc_4, 4294967232), 2045, 16)
							loc_1 = reg_1
							loc_9 = loc_1
							loc_2 = rt_add_i32(loc_9, 8)
							rt_store_i32(memory_at_0, loc_4 + 88, rt_load_i32(memory_at_0, loc_2))
							rt_store_i64(memory_at_0, loc_4 + 80, rt_load_i64(memory_at_0, loc_1))
							rt_store_i64(memory_at_0, loc_1, rt_i64_ZERO)
							rt_store_i32(memory_at_0, loc_9 + 8, 0)
							loc_2 = rt_load_i32(memory_at_0, loc_4 + 80)
							loc_1 = rt_load_i32_i8(memory_at_0, loc_4 + 91)
							reg_1 = FUNC_LIST[5]((if rt_lt_i32(loc_1, 0) then loc_2 else rt_add_i32(loc_4, 80)))
							reg_0 = FUNC_LIST[335](loc_0, reg_1)
							if rt_lt_i32(loc_1, 0) then
								while true do
									FUNC_LIST[230](loc_2)
									break
								end
								if desired then
									break
								end
							end
							if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_4 + 75), 0) then
								while true do
									FUNC_LIST[230](rt_load_i32(memory_at_0, loc_4 + 64))
									break
								end
								if desired then
									break
								end
							end
							if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_4 + 19), 0) then
								while true do
									FUNC_LIST[230](rt_load_i32(memory_at_0, loc_4 + 8))
									break
								end
								if desired then
									break
								end
							end
							if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_4 + 59), 0) then
								while true do
									FUNC_LIST[230](rt_load_i32(memory_at_0, loc_4 + 48))
									break
								end
								if desired then
									break
								end
							end
							if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_4 + 43), 0) then
								while true do
									FUNC_LIST[230](rt_load_i32(memory_at_0, loc_4 + 32))
									break
								end
								if desired then
									break
								end
							end
							if rt_ge_i32(rt_load_i32_i8(memory_at_0, loc_4 + 31), 0) then
								desired = 1
								break
							end
							FUNC_LIST[230](rt_load_i32(memory_at_0, loc_4 + 20))
							desired = 1
							break
						end
						if desired then
							break
						end
					end
					reg_1 = FUNC_LIST[227](32)
					loc_2 = reg_1
					rt_store_i32(memory_at_0, loc_4 + 48, loc_2)
					rt_store_i64(memory_at_0, loc_4 + 52, rt_i64_from_u32(27, 2147483680))
					rt_store_i32(memory_at_0, loc_2 + 23, rt_load_i32(memory_at_0, 2301))
					rt_store_i64(memory_at_0, loc_2 + 16, rt_load_i64(memory_at_0, 2294))
					rt_store_i64(memory_at_0, loc_2 + 8, rt_load_i64(memory_at_0, 2286))
					rt_store_i64(memory_at_0, loc_2, rt_load_i64(memory_at_0, 2278))
					rt_store_i32_n8(memory_at_0, loc_2 + 27, 0)
					loc_2 = rt_load_i32_i8(memory_at_0, loc_1 + 11)
					loc_3 = (if rt_lt_i32(loc_2, 0) then 1 else 0)
					reg_1 = FUNC_LIST[337](rt_add_i32(loc_4, 48), (if loc_3 ~= 0 then rt_load_i32(memory_at_0, loc_1) else loc_1), (if loc_3 ~= 0 then rt_load_i32(memory_at_0, loc_1 + 4) else loc_2))
					loc_1 = reg_1
					loc_10 = loc_1
					loc_2 = rt_add_i32(loc_10, 8)
					rt_store_i32(memory_at_0, loc_4 + 72, rt_load_i32(memory_at_0, loc_2))
					rt_store_i64(memory_at_0, loc_4 + 64, rt_load_i64(memory_at_0, loc_1))
					rt_store_i64(memory_at_0, loc_1, rt_i64_ZERO)
					rt_store_i32(memory_at_0, loc_10 + 8, 0)
					reg_1 = FUNC_LIST[337](rt_sub_i32(loc_4, 4294967232), 2062, 143)
					loc_1 = reg_1
					loc_11 = loc_1
					loc_2 = rt_add_i32(loc_11, 8)
					rt_store_i32(memory_at_0, loc_4 + 88, rt_load_i32(memory_at_0, loc_2))
					rt_store_i64(memory_at_0, loc_4 + 80, rt_load_i64(memory_at_0, loc_1))
					rt_store_i64(memory_at_0, loc_1, rt_i64_ZERO)
					rt_store_i32(memory_at_0, loc_11 + 8, 0)
					loc_2 = rt_load_i32(memory_at_0, loc_4 + 80)
					loc_1 = rt_load_i32_i8(memory_at_0, loc_4 + 91)
					reg_1 = FUNC_LIST[5]((if rt_lt_i32(loc_1, 0) then loc_2 else rt_add_i32(loc_4, 80)))
					reg_0 = FUNC_LIST[335](loc_0, reg_1)
					if rt_lt_i32(loc_1, 0) then
						while true do
							FUNC_LIST[230](loc_2)
							break
						end
						if desired then
							break
						end
					end
					if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_4 + 75), 0) then
						while true do
							FUNC_LIST[230](rt_load_i32(memory_at_0, loc_4 + 64))
							break
						end
						if desired then
							break
						end
					end
					if rt_ge_i32(rt_load_i32_i8(memory_at_0, loc_4 + 59), 0) then
						desired = 1
						break
					end
					FUNC_LIST[230](rt_load_i32(memory_at_0, loc_4 + 48))
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			if loc_0 == loc_1 then
				break
			end
			if rt_ge_i32(rt_load_i32_i8(memory_at_0, loc_1 + 11), 0) then
				while true do
					rt_store_i64(memory_at_0, loc_0, rt_load_i64(memory_at_0, loc_1))
					rt_store_i32(memory_at_0, loc_0 + 8, rt_load_i32(memory_at_0, loc_1 + 8))
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			reg_0 = FUNC_LIST[339](loc_0, rt_load_i32(memory_at_0, loc_1), rt_load_i32(memory_at_0, loc_1 + 4))
			break
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_4, 96)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[12] = --[[ std::__2::__any_imp::_SmallHandler<char const*>::__handle[abi:ne180100](std::__2::__any_imp::_Action, std::__2::any const*, std::__2::any*, std::type_info const*, void const*) ]] function(loc_0, loc_1, loc_2, loc_3, loc_4)
	local loc_5 = 0
	local reg_0
	local reg_1
	local reg_2
	local desired
	local br_map = {}
	while true do
		loc_5 = 9848
		while true do
			while true do
				while true do
					while true do
						while true do
							if not br_map[1] then
								br_map[1] = (function()
									return { [0] = 0, 1, 3, 4, }
								end)()
							end
							temp = br_map[1][rt_sub_i32(loc_0, 1)] or 2
							if temp < 2 then
								if temp < 1 then
									break
								else
									desired = 4
									break
								end
							elseif temp > 2 then
								if temp < 4 then
									desired = 2
									break
								else
									desired = 1
									break
								end
							else
								desired = 3
								break
							end
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						loc_1 = rt_load_i32(memory_at_0, loc_1 + 4)
						rt_store_i32(memory_at_0, loc_2, 1)
						rt_store_i32(memory_at_0, loc_2 + 4, loc_1)
						reg_0 = 0
						desired = 0
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					loc_0 = rt_load_i32(memory_at_0, loc_1 + 4)
					rt_store_i32(memory_at_0, loc_2, 1)
					rt_store_i32(memory_at_0, loc_2 + 4, loc_0)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				rt_store_i32(memory_at_0, loc_1, 0)
				reg_0 = 0
				desired = 0
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			reg_0 = rt_add_i32(loc_1, 4)
			reg_1 = 0
			while true do
				if loc_3 ~= 0 then
					while true do
						reg_2 = (if rt_load_i32(memory_at_0, loc_3 + 4) == rt_load_i32(memory_at_0, 9852) then 1 else 0)
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				reg_2 = (if loc_4 == 3380 then 1 else 0)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_0 = reg_2
			loc_5 = (if loc_0 ~= 0 then reg_0 else reg_1)
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		reg_0 = loc_5
		break
	end
	return reg_0
end
FUNC_LIST[13] = --[[ RBX::Instance::SetProperty(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, std::__2::any) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local loc_8 = 0
	local loc_9 = 0
	local loc_10 = 0
	local reg_0
	local reg_1
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 144)
		GLOBAL_LIST[0].value = loc_3
		rt_store_i64(memory_at_0, loc_3 + 116, rt_i64_ZERO)
		loc_4 = rt_load_i32(memory_at_0, loc_2)
		if loc_4 ~= 0 then
			while true do
				reg_0 = TABLE_LIST[0].data[loc_4](1, loc_2, rt_add_i32(loc_3, 116), 0, 0)
				break
			end
		end
		FUNC_LIST[18](rt_add_i32(loc_3, 132), loc_0, rt_add_i32(loc_3, 116))
		loc_2 = rt_load_i32(memory_at_0, loc_3 + 116)
		if loc_2 ~= 0 then
			while true do
				reg_0 = TABLE_LIST[0].data[loc_2](0, rt_add_i32(loc_3, 116), 0, 0, 0)
				break
			end
		end
		reg_1 = FUNC_LIST[227](80)
		loc_2 = reg_1
		rt_store_i32(memory_at_0, loc_3 + 12, loc_2)
		rt_store_i64(memory_at_0, loc_3 + 16, rt_i64_from_u32(75, 2147483728))
		reg_0 = FUNC_LIST[188](loc_2, 2322, 75)
		rt_store_i32_n8(memory_at_0, reg_0 + 75, 0)
		loc_2 = rt_load_i32_i8(memory_at_0, loc_0 + 11)
		loc_4 = (if rt_lt_i32(loc_2, 0) then 1 else 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 12), (if loc_4 ~= 0 then rt_load_i32(memory_at_0, loc_0) else loc_0), (if loc_4 ~= 0 then rt_load_i32(memory_at_0, loc_0 + 4) else loc_2))
		loc_0 = reg_1
		loc_5 = loc_0
		loc_2 = rt_add_i32(loc_5, 8)
		rt_store_i32(memory_at_0, loc_3 + 32, rt_load_i32(memory_at_0, loc_2))
		rt_store_i64(memory_at_0, loc_3 + 24, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_5 + 8, 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 24), 2206, 24)
		loc_0 = reg_1
		loc_6 = loc_0
		loc_2 = rt_add_i32(loc_6, 8)
		rt_store_i32(memory_at_0, loc_3 + 48, rt_load_i32(memory_at_0, loc_2))
		rt_store_i64(memory_at_0, loc_3 + 40, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_6 + 8, 0)
		loc_0 = rt_load_i32_i8(memory_at_0, loc_1 + 11)
		loc_2 = (if rt_lt_i32(loc_0, 0) then 1 else 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 40), (if loc_2 ~= 0 then rt_load_i32(memory_at_0, loc_1) else loc_1), (if loc_2 ~= 0 then rt_load_i32(memory_at_0, loc_1 + 4) else loc_0))
		loc_0 = reg_1
		loc_7 = loc_0
		loc_1 = rt_add_i32(loc_7, 8)
		rt_store_i32(memory_at_0, rt_sub_i32(loc_3, 4294967232), rt_load_i32(memory_at_0, loc_1))
		rt_store_i64(memory_at_0, loc_3 + 56, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_7 + 8, 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 56), 2633, 3)
		loc_0 = reg_1
		loc_8 = loc_0
		loc_1 = rt_add_i32(loc_8, 8)
		rt_store_i32(memory_at_0, loc_3 + 80, rt_load_i32(memory_at_0, loc_1))
		rt_store_i64(memory_at_0, loc_3 + 72, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_8 + 8, 0)
		loc_0 = rt_load_i32_i8(memory_at_0, loc_3 + 143)
		loc_1 = (if rt_lt_i32(loc_0, 0) then 1 else 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 72), (if loc_1 ~= 0 then rt_load_i32(memory_at_0, loc_3 + 132) else rt_add_i32(loc_3, 132)), (if loc_1 ~= 0 then rt_load_i32(memory_at_0, loc_3 + 136) else loc_0))
		loc_0 = reg_1
		loc_9 = loc_0
		loc_1 = rt_add_i32(loc_9, 8)
		rt_store_i32(memory_at_0, loc_3 + 96, rt_load_i32(memory_at_0, loc_1))
		rt_store_i64(memory_at_0, loc_3 + 88, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_9 + 8, 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 88), 2011, 4)
		loc_0 = reg_1
		loc_10 = loc_0
		loc_1 = rt_add_i32(loc_10, 8)
		rt_store_i32(memory_at_0, loc_3 + 112, rt_load_i32(memory_at_0, loc_1))
		rt_store_i64(memory_at_0, loc_3 + 104, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_10 + 8, 0)
		loc_1 = rt_load_i32(memory_at_0, loc_3 + 104)
		loc_0 = rt_load_i32_i8(memory_at_0, loc_3 + 115)
		reg_0 = FUNC_LIST[5]((if rt_lt_i32(loc_0, 0) then loc_1 else rt_add_i32(loc_3, 104)))
		if rt_lt_i32(loc_0, 0) then
			while true do
				FUNC_LIST[230](loc_1)
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 99), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 88))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 83), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 72))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 67), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 56))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 51), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 40))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 35), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 24))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 23), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 12))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 143), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 132))
				break
			end
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_3, 144)
		break
	end
end
FUNC_LIST[14] = --[[ std::__2::__any_imp::_SmallHandler<RBX::Instance*>::__handle[abi:ne180100](std::__2::__any_imp::_Action, std::__2::any const*, std::__2::any*, std::type_info const*, void const*) ]] function(loc_0, loc_1, loc_2, loc_3, loc_4)
	local loc_5 = 0
	local reg_0
	local reg_1
	local reg_2
	local desired
	local br_map = {}
	while true do
		loc_5 = 2792
		while true do
			while true do
				while true do
					while true do
						while true do
							if not br_map[1] then
								br_map[1] = (function()
									return { [0] = 0, 1, 3, 4, }
								end)()
							end
							temp = br_map[1][rt_sub_i32(loc_0, 1)] or 2
							if temp < 2 then
								if temp < 1 then
									break
								else
									desired = 4
									break
								end
							elseif temp > 2 then
								if temp < 4 then
									desired = 2
									break
								else
									desired = 1
									break
								end
							else
								desired = 3
								break
							end
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						loc_1 = rt_load_i32(memory_at_0, loc_1 + 4)
						rt_store_i32(memory_at_0, loc_2, 2)
						rt_store_i32(memory_at_0, loc_2 + 4, loc_1)
						reg_0 = 0
						desired = 0
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					loc_0 = rt_load_i32(memory_at_0, loc_1 + 4)
					rt_store_i32(memory_at_0, loc_2, 2)
					rt_store_i32(memory_at_0, loc_2 + 4, loc_0)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				rt_store_i32(memory_at_0, loc_1, 0)
				reg_0 = 0
				desired = 0
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			reg_0 = rt_add_i32(loc_1, 4)
			reg_1 = 0
			while true do
				if loc_3 ~= 0 then
					while true do
						reg_2 = (if rt_load_i32(memory_at_0, loc_3 + 4) == 2748 then 1 else 0)
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				reg_2 = (if loc_4 == 7936 then 1 else 0)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_0 = reg_2
			loc_5 = (if loc_0 ~= 0 then reg_0 else reg_1)
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		reg_0 = loc_5
		break
	end
	return reg_0
end
FUNC_LIST[15] = --[[ RBX::Instance::Clone() ]] function(loc_0)
	local loc_1 = 0
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_1 = rt_sub_i32(GLOBAL_LIST[0].value, 80)
		GLOBAL_LIST[0].value = loc_1
		reg_1 = FUNC_LIST[227](80)
		loc_2 = reg_1
		rt_store_i32(memory_at_0, loc_1 + 12, loc_2)
		rt_store_i64(memory_at_0, loc_1 + 16, rt_i64_from_u32(75, 2147483728))
		reg_0 = FUNC_LIST[188](loc_2, 2322, 75)
		rt_store_i32_n8(memory_at_0, reg_0 + 75, 0)
		loc_2 = rt_load_i32_i8(memory_at_0, loc_0 + 11)
		loc_3 = (if rt_lt_i32(loc_2, 0) then 1 else 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_1, 12), (if loc_3 ~= 0 then rt_load_i32(memory_at_0, loc_0) else loc_0), (if loc_3 ~= 0 then rt_load_i32(memory_at_0, loc_0 + 4) else loc_2))
		loc_0 = reg_1
		loc_4 = loc_0
		loc_2 = rt_add_i32(loc_4, 8)
		rt_store_i32(memory_at_0, loc_1 + 32, rt_load_i32(memory_at_0, loc_2))
		rt_store_i64(memory_at_0, loc_1 + 24, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_4 + 8, 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_1, 24), 1624, 391)
		loc_0 = reg_1
		loc_5 = loc_0
		loc_2 = rt_add_i32(loc_5, 8)
		rt_store_i32(memory_at_0, loc_1 + 48, rt_load_i32(memory_at_0, loc_2))
		rt_store_i64(memory_at_0, loc_1 + 40, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_5 + 8, 0)
		reg_0 = FUNC_LIST[5]((if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_1 + 51), 0) then rt_load_i32(memory_at_0, loc_1 + 40) else rt_add_i32(loc_1, 40)))
		loc_3 = reg_0
		reg_0 = FUNC_LIST[192](loc_3)
		loc_0 = reg_0
		if loc_0 < 2147483640 then
			while true do
				while true do
					while true do
						if loc_0 >= 11 then
							while true do
								loc_6 = rt_add_i32(bit_or(loc_0, 7), 1)
								reg_0 = FUNC_LIST[227](loc_6)
								loc_2 = reg_0
								rt_store_i32(memory_at_0, loc_1 + 60, bit_or(loc_6, 2147483648))
								rt_store_i32(memory_at_0, loc_1 + 52, loc_2)
								rt_store_i32(memory_at_0, loc_1 + 56, loc_0)
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						rt_store_i32_n8(memory_at_0, loc_1 + 63, loc_0)
						loc_2 = rt_add_i32(loc_1, 52)
						if loc_0 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					reg_0 = FUNC_LIST[189](loc_2, loc_3, loc_0)
					break
				end
				if desired then
					break
				end
				rt_store_i32_n8(memory_at_0, rt_add_i32(loc_0, loc_2), 0)
				reg_0 = FUNC_LIST[227](12)
				loc_0 = reg_0
				while true do
					loc_2 = rt_load_i32_i8(memory_at_0, loc_1 + 63)
					if rt_ge_i32(loc_2, 0) then
						while true do
							rt_store_i32(memory_at_0, loc_1 + 72, rt_load_i32(memory_at_0, loc_1 + 60))
							rt_store_i64(memory_at_0, loc_1 + 64, rt_load_i64(memory_at_0, loc_1 + 52))
							desired = 2
							break
						end
						if desired then
							if desired == 2 then
								desired = nil
							end
							break
						end
					end
					FUNC_LIST[332](rt_sub_i32(loc_1, 4294967232), rt_load_i32(memory_at_0, loc_1 + 52), rt_load_i32(memory_at_0, loc_1 + 56))
					break
				end
				if desired then
					break
				end
				rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
				loc_3 = rt_add_i32(loc_0, 8)
				rt_store_i32(memory_at_0, loc_3, 0)
				while true do
					if rt_ge_i32(rt_load_i32_i8(memory_at_0, loc_1 + 75), 0) then
						while true do
							rt_store_i64(memory_at_0, loc_0, rt_load_i64(memory_at_0, loc_1 + 64))
							rt_store_i32(memory_at_0, loc_0 + 8, rt_load_i32(memory_at_0, loc_1 + 72))
							desired = 2
							break
						end
						if desired then
							if desired == 2 then
								desired = nil
							end
							break
						end
					end
					reg_0 = FUNC_LIST[339](loc_0, rt_load_i32(memory_at_0, loc_1 + 64), rt_load_i32(memory_at_0, loc_1 + 68))
					if rt_ge_i32(rt_load_i32_i8(memory_at_0, loc_1 + 75), 0) then
						break
					end
					FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1 + 64))
					break
				end
				if desired then
					break
				end
				if rt_lt_i32(loc_2, 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1 + 52))
						break
					end
					if desired then
						break
					end
				end
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_1 + 51), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1 + 40))
						break
					end
					if desired then
						break
					end
				end
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_1 + 35), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1 + 24))
						break
					end
					if desired then
						break
					end
				end
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_1 + 23), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_1 + 12))
						break
					end
					if desired then
						break
					end
				end
				GLOBAL_LIST[0].value = rt_add_i32(loc_1, 80)
				reg_0 = loc_0
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		FUNC_LIST[6](rt_add_i32(loc_1, 52))
		error("out of code bounds")
	end
	return reg_0
end
FUNC_LIST[16] = --[[ RBX::Instance::SetPropertyRaw(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local loc_8 = 0
	local loc_9 = 0
	local loc_10 = 0
	local loc_11 = 0
	local reg_0
	local reg_1
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 112)
		GLOBAL_LIST[0].value = loc_3
		reg_1 = FUNC_LIST[227](80)
		loc_4 = reg_1
		rt_store_i32(memory_at_0, loc_3 + 4, loc_4)
		rt_store_i64(memory_at_0, loc_3 + 8, rt_i64_from_u32(75, 2147483728))
		reg_0 = FUNC_LIST[188](loc_4, 2322, 75)
		rt_store_i32_n8(memory_at_0, reg_0 + 75, 0)
		loc_4 = rt_load_i32_i8(memory_at_0, loc_0 + 11)
		loc_5 = (if rt_lt_i32(loc_4, 0) then 1 else 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 4), (if loc_5 ~= 0 then rt_load_i32(memory_at_0, loc_0) else loc_0), (if loc_5 ~= 0 then rt_load_i32(memory_at_0, loc_0 + 4) else loc_4))
		loc_0 = reg_1
		loc_6 = loc_0
		loc_4 = rt_add_i32(loc_6, 8)
		rt_store_i32(memory_at_0, loc_3 + 24, rt_load_i32(memory_at_0, loc_4))
		rt_store_i64(memory_at_0, loc_3 + 16, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_6 + 8, 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 16), 2206, 24)
		loc_0 = reg_1
		loc_7 = loc_0
		loc_4 = rt_add_i32(loc_7, 8)
		rt_store_i32(memory_at_0, loc_3 + 40, rt_load_i32(memory_at_0, loc_4))
		rt_store_i64(memory_at_0, loc_3 + 32, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_7 + 8, 0)
		loc_0 = rt_load_i32_i8(memory_at_0, loc_1 + 11)
		loc_4 = (if rt_lt_i32(loc_0, 0) then 1 else 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 32), (if loc_4 ~= 0 then rt_load_i32(memory_at_0, loc_1) else loc_1), (if loc_4 ~= 0 then rt_load_i32(memory_at_0, loc_1 + 4) else loc_0))
		loc_0 = reg_1
		loc_8 = loc_0
		loc_1 = rt_add_i32(loc_8, 8)
		rt_store_i32(memory_at_0, loc_3 + 56, rt_load_i32(memory_at_0, loc_1))
		rt_store_i64(memory_at_0, loc_3 + 48, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_8 + 8, 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 48), 2633, 3)
		loc_0 = reg_1
		loc_9 = loc_0
		loc_1 = rt_add_i32(loc_9, 8)
		rt_store_i32(memory_at_0, loc_3 + 72, rt_load_i32(memory_at_0, loc_1))
		rt_store_i64(memory_at_0, loc_3 + 64, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_9 + 8, 0)
		loc_0 = rt_load_i32_i8(memory_at_0, loc_2 + 11)
		loc_1 = (if rt_lt_i32(loc_0, 0) then 1 else 0)
		reg_1 = FUNC_LIST[337](rt_sub_i32(loc_3, 4294967232), (if loc_1 ~= 0 then rt_load_i32(memory_at_0, loc_2) else loc_2), (if loc_1 ~= 0 then rt_load_i32(memory_at_0, loc_2 + 4) else loc_0))
		loc_0 = reg_1
		loc_10 = loc_0
		loc_1 = rt_add_i32(loc_10, 8)
		rt_store_i32(memory_at_0, loc_3 + 88, rt_load_i32(memory_at_0, loc_1))
		rt_store_i64(memory_at_0, loc_3 + 80, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_10 + 8, 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 80), 2011, 4)
		loc_0 = reg_1
		loc_11 = loc_0
		loc_1 = rt_add_i32(loc_11, 8)
		rt_store_i32(memory_at_0, loc_3 + 104, rt_load_i32(memory_at_0, loc_1))
		rt_store_i64(memory_at_0, loc_3 + 96, rt_load_i64(memory_at_0, loc_0))
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_11 + 8, 0)
		loc_1 = rt_load_i32(memory_at_0, loc_3 + 96)
		loc_0 = rt_load_i32_i8(memory_at_0, loc_3 + 107)
		reg_0 = FUNC_LIST[5]((if rt_lt_i32(loc_0, 0) then loc_1 else rt_add_i32(loc_3, 96)))
		if rt_lt_i32(loc_0, 0) then
			while true do
				FUNC_LIST[230](loc_1)
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 91), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 80))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 75), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 64))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 59), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 48))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 43), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 32))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 27), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 16))
				break
			end
		end
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 15), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 4))
				break
			end
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_3, 112)
		break
	end
end
FUNC_LIST[17] = --[[ RBX::Instance::GetPropertyRaw(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local loc_8 = 0
	local loc_9 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 80)
		GLOBAL_LIST[0].value = loc_3
		reg_1 = FUNC_LIST[227](80)
		loc_4 = reg_1
		rt_store_i32(memory_at_0, loc_3 + 4, loc_4)
		rt_store_i64(memory_at_0, loc_3 + 8, rt_i64_from_u32(75, 2147483728))
		reg_0 = FUNC_LIST[188](loc_4, 2322, 75)
		rt_store_i32_n8(memory_at_0, reg_0 + 75, 0)
		loc_4 = rt_load_i32_i8(memory_at_0, loc_1 + 11)
		loc_5 = (if rt_lt_i32(loc_4, 0) then 1 else 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 4), (if loc_5 ~= 0 then rt_load_i32(memory_at_0, loc_1) else loc_1), (if loc_5 ~= 0 then rt_load_i32(memory_at_0, loc_1 + 4) else loc_4))
		loc_1 = reg_1
		loc_6 = loc_1
		loc_4 = rt_add_i32(loc_6, 8)
		rt_store_i32(memory_at_0, loc_3 + 24, rt_load_i32(memory_at_0, loc_4))
		rt_store_i64(memory_at_0, loc_3 + 16, rt_load_i64(memory_at_0, loc_1))
		rt_store_i64(memory_at_0, loc_1, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_6 + 8, 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 16), 2231, 36)
		loc_1 = reg_1
		loc_7 = loc_1
		loc_4 = rt_add_i32(loc_7, 8)
		rt_store_i32(memory_at_0, loc_3 + 40, rt_load_i32(memory_at_0, loc_4))
		rt_store_i64(memory_at_0, loc_3 + 32, rt_load_i64(memory_at_0, loc_1))
		rt_store_i64(memory_at_0, loc_1, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_7 + 8, 0)
		loc_1 = rt_load_i32_i8(memory_at_0, loc_2 + 11)
		loc_4 = (if rt_lt_i32(loc_1, 0) then 1 else 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 32), (if loc_4 ~= 0 then rt_load_i32(memory_at_0, loc_2) else loc_2), (if loc_4 ~= 0 then rt_load_i32(memory_at_0, loc_2 + 4) else loc_1))
		loc_1 = reg_1
		loc_8 = loc_1
		loc_2 = rt_add_i32(loc_8, 8)
		rt_store_i32(memory_at_0, loc_3 + 56, rt_load_i32(memory_at_0, loc_2))
		rt_store_i64(memory_at_0, loc_3 + 48, rt_load_i64(memory_at_0, loc_1))
		rt_store_i64(memory_at_0, loc_1, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_8 + 8, 0)
		reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 48), 1234, 69)
		loc_1 = reg_1
		loc_9 = loc_1
		loc_2 = rt_add_i32(loc_9, 8)
		rt_store_i32(memory_at_0, loc_3 + 72, rt_load_i32(memory_at_0, loc_2))
		rt_store_i64(memory_at_0, loc_3 + 64, rt_load_i64(memory_at_0, loc_1))
		rt_store_i64(memory_at_0, loc_1, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_9 + 8, 0)
		reg_0 = FUNC_LIST[5]((if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 75), 0) then rt_load_i32(memory_at_0, loc_3 + 64) else rt_sub_i32(loc_3, 4294967232)))
		loc_2 = reg_0
		reg_0 = FUNC_LIST[192](loc_2)
		loc_1 = reg_0
		if loc_1 < 2147483640 then
			while true do
				while true do
					while true do
						if loc_1 >= 11 then
							while true do
								loc_5 = rt_add_i32(bit_or(loc_1, 7), 1)
								reg_0 = FUNC_LIST[227](loc_5)
								loc_4 = reg_0
								rt_store_i32(memory_at_0, loc_0 + 8, bit_or(loc_5, 2147483648))
								rt_store_i32(memory_at_0, loc_0, loc_4)
								rt_store_i32(memory_at_0, loc_0 + 4, loc_1)
								loc_0 = loc_4
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						rt_store_i32_n8(memory_at_0, loc_0 + 11, loc_1)
						if loc_1 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					reg_0 = FUNC_LIST[189](loc_0, loc_2, loc_1)
					break
				end
				if desired then
					break
				end
				rt_store_i32_n8(memory_at_0, rt_add_i32(loc_0, loc_1), 0)
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 75), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 64))
						break
					end
					if desired then
						break
					end
				end
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 59), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 48))
						break
					end
					if desired then
						break
					end
				end
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 43), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 32))
						break
					end
					if desired then
						break
					end
				end
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 27), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 16))
						break
					end
					if desired then
						break
					end
				end
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 15), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 4))
						break
					end
					if desired then
						break
					end
				end
				GLOBAL_LIST[0].value = rt_add_i32(loc_3, 80)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		FUNC_LIST[6](loc_0)
		error("out of code bounds")
	end
end
FUNC_LIST[18] = --[[ RBX::Instance::toString(std::__2::any) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local loc_8 = 0
	local loc_9 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 640)
		GLOBAL_LIST[0].value = loc_3
		if bit_and(rt_load_i32_u8(memory_at_0, 10516), 1) == 0 then
			while true do
				rt_store_i32(memory_at_0, loc_3 + 600, 2792)
				rt_store_i32(memory_at_0, loc_3 + 568, 9804)
				rt_store_i32(memory_at_0, loc_3 + 536, 10000)
				rt_store_i32(memory_at_0, loc_3 + 504, 9988)
				rt_store_i32(memory_at_0, loc_3 + 472, 9976)
				rt_store_i32(memory_at_0, loc_3 + 440, 9964)
				rt_store_i32(memory_at_0, loc_3 + 408, 9952)
				rt_store_i32(memory_at_0, loc_3 + 376, 9940)
				rt_store_i32(memory_at_0, loc_3 + 344, 9928)
				rt_store_i32(memory_at_0, loc_3 + 312, 9916)
				rt_store_i32(memory_at_0, loc_3 + 280, 9904)
				rt_store_i32(memory_at_0, loc_3 + 248, 9892)
				rt_store_i32(memory_at_0, loc_3 + 216, 9880)
				rt_store_i32(memory_at_0, loc_3 + 184, 9868)
				rt_store_i32(memory_at_0, loc_3 + 152, 9816)
				rt_store_i32(memory_at_0, loc_3 + 120, 2740)
				rt_store_i32(memory_at_0, loc_3 + 88, 9828)
				rt_store_i32(memory_at_0, loc_3 + 56, 9848)
				rt_store_i32(memory_at_0, loc_3 + 24, 9792)
				loc_5 = rt_add_i32(loc_3, 608)
				rt_store_i32(memory_at_0, loc_3 + 624, loc_5)
				rt_store_i32(memory_at_0, loc_3 + 608, 7740)
				rt_store_i32(memory_at_0, loc_3 + 592, rt_add_i32(loc_3, 576))
				rt_store_i32(memory_at_0, loc_3 + 576, 7472)
				rt_store_i32(memory_at_0, loc_3 + 560, rt_add_i32(loc_3, 544))
				rt_store_i32(memory_at_0, loc_3 + 544, 7204)
				rt_store_i32(memory_at_0, loc_3 + 528, rt_add_i32(loc_3, 512))
				rt_store_i32(memory_at_0, loc_3 + 512, 6936)
				rt_store_i32(memory_at_0, loc_3 + 496, rt_add_i32(loc_3, 480))
				rt_store_i32(memory_at_0, loc_3 + 480, 6668)
				rt_store_i32(memory_at_0, loc_3 + 464, rt_add_i32(loc_3, 448))
				rt_store_i32(memory_at_0, loc_3 + 448, 6400)
				rt_store_i32(memory_at_0, loc_3 + 432, rt_add_i32(loc_3, 416))
				rt_store_i32(memory_at_0, loc_3 + 416, 6132)
				rt_store_i32(memory_at_0, loc_3 + 400, rt_add_i32(loc_3, 384))
				rt_store_i32(memory_at_0, loc_3 + 384, 5864)
				rt_store_i32(memory_at_0, loc_3 + 368, rt_add_i32(loc_3, 352))
				rt_store_i32(memory_at_0, loc_3 + 352, 5596)
				rt_store_i32(memory_at_0, loc_3 + 336, rt_add_i32(loc_3, 320))
				rt_store_i32(memory_at_0, loc_3 + 320, 5328)
				rt_store_i32(memory_at_0, loc_3 + 304, rt_add_i32(loc_3, 288))
				rt_store_i32(memory_at_0, loc_3 + 288, 5060)
				rt_store_i32(memory_at_0, loc_3 + 272, rt_add_i32(loc_3, 256))
				rt_store_i32(memory_at_0, loc_3 + 256, 4792)
				rt_store_i32(memory_at_0, loc_3 + 240, rt_add_i32(loc_3, 224))
				rt_store_i32(memory_at_0, loc_3 + 224, 4524)
				rt_store_i32(memory_at_0, loc_3 + 208, rt_add_i32(loc_3, 192))
				rt_store_i32(memory_at_0, loc_3 + 192, 4256)
				rt_store_i32(memory_at_0, loc_3 + 176, rt_add_i32(loc_3, 160))
				rt_store_i32(memory_at_0, loc_3 + 160, 3988)
				rt_store_i32(memory_at_0, loc_3 + 144, rt_add_i32(loc_3, 128))
				rt_store_i32(memory_at_0, loc_3 + 128, 3720)
				rt_store_i32(memory_at_0, loc_3 + 112, rt_add_i32(loc_3, 96))
				rt_store_i32(memory_at_0, loc_3 + 96, 3452)
				rt_store_i32(memory_at_0, loc_3 + 80, rt_sub_i32(loc_3, 4294967232))
				rt_store_i32(memory_at_0, loc_3 + 64, 3184)
				rt_store_i32(memory_at_0, loc_3 + 48, rt_add_i32(loc_3, 32))
				rt_store_i32(memory_at_0, loc_3 + 32, 2816)
				rt_store_i64(memory_at_0, 10504, rt_i64_ZERO)
				rt_store_i64(memory_at_0, 10496, rt_i64_ZERO)
				rt_store_i32(memory_at_0, 10512, 1065353216)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, rt_add_i32(loc_3, 24), rt_add_i32(loc_3, 24))
				loc_4 = rt_add_i32(loc_3, 56)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 88)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 120)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 152)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 184)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 216)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 248)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 280)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 312)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 344)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 376)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 408)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 440)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 472)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 504)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 536)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 568)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				loc_4 = rt_add_i32(loc_3, 600)
				FUNC_LIST[22](rt_add_i32(loc_3, 632), 10496, loc_4, loc_4)
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 624)
						if loc_5 == loc_4 then
							while true do
								loc_6 = 16
								loc_4 = loc_5
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_6 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_6))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 592)
						loc_6 = rt_add_i32(loc_3, 576)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 560)
						loc_6 = rt_add_i32(loc_3, 544)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 528)
						loc_6 = rt_add_i32(loc_3, 512)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 496)
						loc_6 = rt_add_i32(loc_3, 480)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 464)
						loc_6 = rt_add_i32(loc_3, 448)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 432)
						loc_6 = rt_add_i32(loc_3, 416)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 400)
						loc_6 = rt_add_i32(loc_3, 384)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 368)
						loc_6 = rt_add_i32(loc_3, 352)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 336)
						loc_6 = rt_add_i32(loc_3, 320)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 304)
						loc_6 = rt_add_i32(loc_3, 288)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 272)
						loc_6 = rt_add_i32(loc_3, 256)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 240)
						loc_6 = rt_add_i32(loc_3, 224)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 208)
						loc_6 = rt_add_i32(loc_3, 192)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 176)
						loc_6 = rt_add_i32(loc_3, 160)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 144)
						loc_6 = rt_add_i32(loc_3, 128)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 112)
						loc_6 = rt_add_i32(loc_3, 96)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 80)
						loc_6 = rt_sub_i32(loc_3, 4294967232)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				while true do
					while true do
						loc_4 = rt_load_i32(memory_at_0, loc_3 + 48)
						loc_6 = rt_add_i32(loc_3, 32)
						if loc_4 == loc_6 then
							while true do
								loc_5 = 16
								loc_4 = loc_6
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_5 = 20
						if loc_4 == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
					TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_4), loc_5))](loc_4)
					break
				end
				if desired then
					break
				end
				reg_0 = FUNC_LIST[187](3, 0, 1024)
				rt_store_i32_n8(memory_at_0, 10516, 1)
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		loc_4 = rt_load_i32(memory_at_0, loc_2)
		if loc_4 ~= 0 then
			while true do
				reg_0 = TABLE_LIST[0].data[loc_4](4, loc_2, 0, 0, 0)
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		else
			while true do
				reg_0 = 9780
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		loc_4 = reg_0
		loc_5 = rt_load_i32(memory_at_0, loc_4 + 4)
		while true do
			while true do
				while true do
					loc_7 = rt_load_i32(memory_at_0, 10500)
					if loc_7 == 0 then
						break
					end
					reg_0 = rt_load_i32(memory_at_0, 10496)
					while true do
						loc_6 = rt_popcnt_i32(loc_7)
						reg_1 = bit_and(rt_sub_i32(loc_7, 1), loc_5)
						if loc_6 <= 1 then
							break
						end
						loc_8 = loc_5
						reg_1 = loc_8
						if loc_5 < loc_7 then
							break
						end
						reg_1 = (loc_5 % loc_7)
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					loc_8 = reg_1
					loc_4 = rt_load_i32(memory_at_0, rt_add_i32(reg_0, rt_shl_i32(loc_8, 2)))
					if loc_4 == 0 then
						break
					end
					loc_4 = rt_load_i32(memory_at_0, loc_4)
					if loc_4 == 0 then
						break
					end
					while true do
						if loc_6 <= 1 then
							while true do
								loc_7 = rt_sub_i32(loc_7, 1)
								while true do
									while true do
										loc_6 = rt_load_i32(memory_at_0, loc_4 + 4)
										if loc_5 ~= loc_6 then
											while true do
												if bit_and(loc_6, loc_7) == loc_8 then
													desired = 7
													break
												end
												desired = 3
												break
											end
											if desired then
												if desired == 7 then
													desired = nil
												end
												break
											end
										end
										if rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_4 + 8) + 4) == loc_5 then
											desired = 4
											break
										end
										break
									end
									if desired then
										if desired == 6 then
											desired = nil
											continue
										end
										break
									end
									loc_4 = rt_load_i32(memory_at_0, loc_4)
									if loc_4 ~= 0 then
										continue
									end
									break
								end
								if desired then
									break
								end
								desired = 3
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						while true do
							while true do
								loc_6 = rt_load_i32(memory_at_0, loc_4 + 4)
								if loc_5 ~= loc_6 then
									while true do
										if loc_6 >= loc_7 then
											while true do
												loc_6 = (loc_6 % loc_7)
												break
											end
											if desired then
												break
											end
										end
										if loc_6 == loc_8 then
											desired = 6
											break
										end
										desired = 3
										break
									end
									if desired then
										if desired == 6 then
											desired = nil
										end
										break
									end
								end
								if rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_4 + 8) + 4) == loc_5 then
									desired = 4
									break
								end
								break
							end
							if desired then
								if desired == 5 then
									desired = nil
									continue
								end
								break
							end
							loc_4 = rt_load_i32(memory_at_0, loc_4)
							if loc_4 ~= 0 then
								continue
							end
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						desired = 3
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					loc_4 = rt_load_i32(memory_at_0, loc_4 + 32)
					if loc_4 ~= 0 then
						while true do
							TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_4) + 24)](loc_0, loc_4, loc_2)
							desired = 2
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					FUNC_LIST[24]()
					error("out of code bounds")
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				reg_0 = FUNC_LIST[192](loc_5)
				loc_4 = reg_0
				if loc_4 >= 2147483640 then
					desired = 1
					break
				end
				while true do
					while true do
						if loc_4 >= 11 then
							while true do
								loc_7 = rt_add_i32(bit_or(loc_4, 7), 1)
								reg_0 = FUNC_LIST[227](loc_7)
								loc_6 = reg_0
								rt_store_i32(memory_at_0, loc_3 + 32, bit_or(loc_7, 2147483648))
								rt_store_i32(memory_at_0, loc_3 + 24, loc_6)
								rt_store_i32(memory_at_0, loc_3 + 28, loc_4)
								desired = 4
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						rt_store_i32_n8(memory_at_0, loc_3 + 35, loc_4)
						loc_6 = rt_add_i32(loc_3, 24)
						if loc_4 == 0 then
							desired = 3
							break
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					reg_0 = FUNC_LIST[189](loc_6, loc_5, loc_4)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				rt_store_i32_n8(memory_at_0, rt_add_i32(loc_4, loc_6), 0)
				reg_1 = FUNC_LIST[333](rt_add_i32(loc_3, 24), 0, 2637, 14)
				loc_4 = reg_1
				loc_9 = loc_4
				loc_5 = rt_add_i32(loc_9, 8)
				rt_store_i32(memory_at_0, loc_3 + 16, rt_load_i32(memory_at_0, loc_5))
				rt_store_i64(memory_at_0, loc_3 + 8, rt_load_i64(memory_at_0, loc_4))
				rt_store_i64(memory_at_0, loc_4, rt_i64_ZERO)
				rt_store_i32(memory_at_0, loc_9 + 8, 0)
				FUNC_LIST[8](rt_add_i32(loc_3, 8))
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 19), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 8))
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 35), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 24))
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				rt_store_i32_n8(memory_at_0, loc_0, 0)
				rt_store_i32_n8(memory_at_0, loc_0 + 11, 0)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			GLOBAL_LIST[0].value = rt_add_i32(loc_3, 640)
			desired = 0
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		FUNC_LIST[6](rt_add_i32(loc_3, 24))
		error("out of code bounds")
	end
end
FUNC_LIST[19] = --[[ std::__2::__throw_length_error[abi:ne180100](char const*) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[456](8)
		reg_0 = FUNC_LIST[20](reg_0, loc_0)
		FUNC_LIST[0](reg_0, 10408, 4)
		error("out of code bounds")
	end
end
FUNC_LIST[20] = --[[ std::length_error::length_error[abi:ne180100](char const*) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[240](loc_0, loc_1)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 10376)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[21] = --[[ std::__throw_bad_array_new_length[abi:ne180100]() ]] function()
	local reg_0
	while true do
		reg_0 = FUNC_LIST[456](4)
		reg_0 = FUNC_LIST[494](reg_0)
		FUNC_LIST[0](reg_0, 10308, 5)
		error("out of code bounds")
	end
end
FUNC_LIST[22] = --[[ std::__2::pair<std::__2::__hash_iterator<std::__2::__hash_node<std::__2::__hash_value_type<std::__2::type_index, std::__2::function<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>>, void*>*>, bool> std::__2::__hash_table<std::__2::__hash_value_type<std::__2::type_index, std::__2::function<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>>, std::__2::__unordered_map_hasher<std::__2::type_index, std::__2::__hash_value_type<std::__2::type_index, std::__2::function<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>>, std::__2::hash<std::__2::type_index>, std::__2::equal_to<std::__2::type_index>, true>, std::__2::__unordered_map_equal<std::__2::type_index, std::__2::__hash_value_type<std::__2::type_index, std::__2::function<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>>, std::__2::equal_to<std::__2::type_index>, std::__2::hash<std::__2::type_index>, true>, std::__2::allocator<std::__2::__hash_value_type<std::__2::type_index, std::__2::function<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>>>>::__emplace_unique_key_args<std::__2::type_index, std::__2::pair<std::__2::type_index const, std::__2::function<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>> const&>(std::__2::type_index const&, std::__2::pair<std::__2::type_index const, std::__2::function<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>> const&) ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local loc_8 = 0.0
	local loc_9 = 0.0
	local loc_10 = 0
	local reg_0
	local reg_1
	local reg_2
	local reg_3
	local desired
	while true do
		loc_4 = rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_2) + 4)
		reg_0 = loc_0
		while true do
			while true do
				loc_5 = rt_load_i32(memory_at_0, loc_1 + 4)
				if loc_5 == 0 then
					break
				end
				reg_1 = rt_load_i32(memory_at_0, loc_1)
				while true do
					loc_7 = rt_popcnt_i32(loc_5)
					reg_2 = bit_and(rt_sub_i32(loc_5, 1), loc_4)
					if loc_7 <= 1 then
						break
					end
					loc_6 = loc_4
					reg_2 = loc_6
					if loc_4 < loc_5 then
						break
					end
					reg_2 = (loc_4 % loc_5)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_6 = reg_2
				loc_2 = rt_load_i32(memory_at_0, rt_add_i32(reg_1, rt_shl_i32(loc_6, 2)))
				if loc_2 == 0 then
					break
				end
				loc_2 = rt_load_i32(memory_at_0, loc_2)
				if loc_2 == 0 then
					break
				end
				if loc_7 <= 1 then
					while true do
						loc_10 = rt_sub_i32(loc_5, 1)
						while true do
							while true do
								loc_7 = rt_load_i32(memory_at_0, loc_2 + 4)
								if loc_4 ~= loc_7 then
									while true do
										if bit_and(loc_7, loc_10) ~= loc_6 then
											desired = 2
											break
										end
										desired = 5
										break
									end
									if desired then
										if desired == 5 then
											desired = nil
										end
										break
									end
								end
								if rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_2 + 8) + 4) ~= loc_4 then
									break
								end
								reg_1 = 0
								desired = 1
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
									continue
								end
								break
							end
							loc_2 = rt_load_i32(memory_at_0, loc_2)
							if loc_2 ~= 0 then
								continue
							end
							break
						end
						if desired then
							break
						end
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				while true do
					while true do
						loc_7 = rt_load_i32(memory_at_0, loc_2 + 4)
						if loc_4 ~= loc_7 then
							while true do
								if loc_5 <= loc_7 then
									while true do
										loc_7 = (loc_7 % loc_5)
										break
									end
									if desired then
										break
									end
								end
								if loc_6 ~= loc_7 then
									desired = 2
									break
								end
								desired = 4
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						if rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_2 + 8) + 4) ~= loc_4 then
							break
						end
						reg_1 = 0
						desired = 1
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
							continue
						end
						break
					end
					loc_2 = rt_load_i32(memory_at_0, loc_2)
					if loc_2 ~= 0 then
						continue
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			reg_1 = FUNC_LIST[227](40)
			loc_2 = reg_1
			rt_store_i32(memory_at_0, loc_2 + 4, loc_4)
			rt_store_i32(memory_at_0, loc_2, 0)
			rt_store_i32(memory_at_0, loc_2 + 8, rt_load_i32(memory_at_0, loc_3))
			while true do
				loc_7 = rt_load_i32(memory_at_0, loc_3 + 24)
				if loc_7 == 0 then
					while true do
						rt_store_i32(memory_at_0, loc_2 + 32, 0)
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				if rt_add_i32(loc_3, 8) == loc_7 then
					while true do
						loc_3 = rt_add_i32(loc_2, 16)
						rt_store_i32(memory_at_0, loc_2 + 32, loc_3)
						TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_7) + 12)](loc_7, loc_3)
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				reg_2 = TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_7) + 8)](loc_7)
				rt_store_i32(memory_at_0, loc_2 + 32, reg_2)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_8 = rt_load_f32(memory_at_0, loc_1 + 16)
			loc_9 = no_op(rt_add_i32(rt_load_i32(memory_at_0, loc_1 + 12), 1))
			while true do
				if loc_5 ~= 0 then
					while true do
						if (if (loc_8 * no_op(loc_5)) < loc_9 then 1 else 0) == 0 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				loc_6 = bit_or(bit_or((if bit_and(loc_5, rt_sub_i32(loc_5, 1)) ~= 0 then 1 else 0), (if loc_5 < 3 then 1 else 0)), rt_shl_i32(loc_5, 1))
				while true do
					while true do
						loc_5 = 2
						reg_1 = loc_5
						reg_2 = loc_6
						while true do
							loc_8 = math_ceil((loc_9 / loc_8))
							if bit_and((if loc_8 < 4.2949673e9 then 1 else 0), (if loc_8 >= 0e0 then 1 else 0)) ~= 0 then
								while true do
									reg_3 = rt_truncate_u32_f32(loc_8)
									desired = 5
									break
								end
								if desired then
									if desired == 5 then
										desired = nil
									end
									break
								end
							end
							reg_3 = 0
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						loc_7 = reg_3
						loc_6 = (if loc_6 > loc_7 then reg_2 else loc_7)
						if loc_6 == 1 then
							break
						end
						reg_1 = loc_6
						if bit_and(loc_6, rt_sub_i32(loc_6, 1)) == 0 then
							break
						end
						reg_1 = FUNC_LIST[198](loc_6)
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					loc_5 = reg_1
					loc_6 = rt_load_i32(memory_at_0, loc_1 + 4)
					if loc_5 <= loc_6 then
						while true do
							if loc_5 >= loc_6 then
								desired = 3
								break
							end
							loc_3 = (if loc_6 < 3 then 1 else 0)
							while true do
								loc_8 = math_ceil((no_op(rt_load_i32(memory_at_0, loc_1 + 12)) / rt_load_f32(memory_at_0, loc_1 + 16)))
								if bit_and((if loc_8 < 4.2949673e9 then 1 else 0), (if loc_8 >= 0e0 then 1 else 0)) ~= 0 then
									while true do
										reg_1 = rt_truncate_u32_f32(loc_8)
										desired = 5
										break
									end
									if desired then
										if desired == 5 then
											desired = nil
										end
										break
									end
								end
								reg_1 = 0
								break
							end
							if desired then
								break
							end
							loc_7 = reg_1
							reg_1 = loc_5
							while true do
								while true do
									if loc_3 ~= 0 then
										break
									end
									if rt_popcnt_i32(loc_6) > 1 then
										break
									end
									reg_2 = (if loc_7 < 2 then loc_7 else rt_shl_i32(1, rt_sub_i32(32, bit_countlz(rt_sub_i32(loc_7, 1)))))
									desired = 5
									break
								end
								if desired then
									if desired == 5 then
										desired = nil
									end
									break
								end
								reg_2 = FUNC_LIST[198](loc_7)
								break
							end
							if desired then
								break
							end
							loc_7 = reg_2
							loc_5 = (if loc_5 > loc_7 then reg_1 else loc_7)
							if loc_5 >= loc_6 then
								desired = 3
								break
							end
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					FUNC_LIST[184](loc_1, loc_5)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_5 = rt_load_i32(memory_at_0, loc_1 + 4)
				loc_6 = rt_sub_i32(loc_5, 1)
				if bit_and(loc_5, loc_6) == 0 then
					while true do
						loc_6 = bit_and(loc_4, loc_6)
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				if loc_4 < loc_5 then
					while true do
						loc_6 = loc_4
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				loc_6 = (loc_4 % loc_5)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			while true do
				while true do
					loc_6 = rt_add_i32(rt_load_i32(memory_at_0, loc_1), rt_shl_i32(loc_6, 2))
					loc_4 = rt_load_i32(memory_at_0, loc_6)
					if loc_4 == 0 then
						while true do
							loc_4 = rt_add_i32(loc_1, 8)
							rt_store_i32(memory_at_0, loc_2, rt_load_i32(memory_at_0, loc_4))
							rt_store_i32(memory_at_0, loc_1 + 8, loc_2)
							rt_store_i32(memory_at_0, loc_6, loc_4)
							loc_4 = rt_load_i32(memory_at_0, loc_2)
							if loc_4 == 0 then
								desired = 2
								break
							end
							loc_4 = rt_load_i32(memory_at_0, loc_4 + 4)
							while true do
								loc_6 = rt_sub_i32(loc_5, 1)
								if bit_and(loc_5, loc_6) == 0 then
									while true do
										loc_4 = bit_and(loc_4, loc_6)
										desired = 5
										break
									end
									if desired then
										if desired == 5 then
											desired = nil
										end
										break
									end
								end
								if loc_4 < loc_5 then
									break
								end
								loc_4 = (loc_4 % loc_5)
								break
							end
							if desired then
								break
							end
							loc_4 = rt_add_i32(rt_load_i32(memory_at_0, loc_1), rt_shl_i32(loc_4, 2))
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					rt_store_i32(memory_at_0, loc_2, rt_load_i32(memory_at_0, loc_4))
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				rt_store_i32(memory_at_0, loc_4, loc_2)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			rt_store_i32(memory_at_0, loc_1 + 12, rt_add_i32(rt_load_i32(memory_at_0, loc_1 + 12), 1))
			reg_1 = 1
			break
		end
		loc_4 = reg_1
		rt_store_i32_n8(memory_at_0, reg_0 + 4, loc_4)
		rt_store_i32(memory_at_0, loc_0, loc_2)
		break
	end
end
FUNC_LIST[23] = --[[ __cxx_global_array_dtor ]] function(loc_0)
	local loc_1 = 0
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local desired
	while true do
		loc_1 = rt_load_i32(memory_at_0, 10504)
		if loc_1 ~= 0 then
			while true do
				while true do
					loc_3 = 16
					loc_4 = rt_load_i32(memory_at_0, loc_1)
					while true do
						loc_5 = rt_load_i32(memory_at_0, loc_1 + 32)
						loc_2 = rt_add_i32(loc_1, 16)
						if loc_5 ~= loc_2 then
							while true do
								loc_3 = 20
								loc_2 = loc_5
								if loc_2 == 0 then
									desired = 3
									break
								end
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_2), loc_3))](loc_2)
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
							continue
						end
						break
					end
					FUNC_LIST[230](loc_1)
					loc_1 = loc_4
					if loc_1 ~= 0 then
						continue
					end
					break
				end
				break
			end
		end
		loc_1 = rt_load_i32(memory_at_0, 10496)
		rt_store_i32(memory_at_0, 10496, 0)
		if loc_1 ~= 0 then
			while true do
				FUNC_LIST[230](loc_1)
				break
			end
		end
		break
	end
end
FUNC_LIST[24] = --[[ std::__2::__throw_bad_function_call[abi:ne180100]() ]] function()
	local loc_0 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[456](4)
		loc_0 = reg_0
		rt_store_i32(memory_at_0, loc_0, 8060)
		FUNC_LIST[0](loc_0, 8100, 6)
		error("out of code bounds")
	end
end
FUNC_LIST[25] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[26] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 2816)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[27] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 2816)
		break
	end
end
FUNC_LIST[28] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[29] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[30] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	while true do
		rt_store_i32_n8(memory_at_0, loc_0 + 11, 3)
		rt_store_i32_n8(memory_at_0, loc_0 + 3, 0)
		rt_store_i32_n16(memory_at_0, loc_0, rt_load_i32_u16(memory_at_0, 1159))
		rt_store_i32_n8(memory_at_0, loc_0 + 2, rt_load_i32_u8(memory_at_0, 1161))
		break
	end
end
FUNC_LIST[31] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 3120 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[32] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 3168
		break
	end
	return reg_0
end
FUNC_LIST[33] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[34] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 3184)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[35] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 3184)
		break
	end
end
FUNC_LIST[36] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[37] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[38] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	while true do
		FUNC_LIST[39](loc_0, rt_add_i32(loc_1, 4), loc_2)
		break
	end
end
FUNC_LIST[39] = --[[ RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&)::operator()(std::__2::any const&) const ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 32)
		GLOBAL_LIST[0].value = loc_3
		while true do
			while true do
				loc_4 = rt_load_i32(memory_at_0, loc_2)
				if loc_4 == 0 then
					break
				end
				reg_0 = TABLE_LIST[0].data[loc_4](3, loc_2, 0, 9848, 3380)
				loc_2 = reg_0
				if loc_2 == 0 then
					break
				end
				loc_5 = rt_load_i32(memory_at_0, loc_2)
				reg_0 = FUNC_LIST[192](loc_5)
				loc_2 = reg_0
				if loc_2 >= 2147483640 then
					desired = 1
					break
				end
				while true do
					while true do
						if loc_2 >= 11 then
							while true do
								loc_6 = rt_add_i32(bit_or(loc_2, 7), 1)
								reg_0 = FUNC_LIST[227](loc_6)
								loc_4 = reg_0
								rt_store_i32(memory_at_0, loc_3 + 12, bit_or(loc_6, 2147483648))
								rt_store_i32(memory_at_0, loc_3 + 4, loc_4)
								rt_store_i32(memory_at_0, loc_3 + 8, loc_2)
								desired = 4
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						rt_store_i32_n8(memory_at_0, loc_3 + 15, loc_2)
						loc_4 = rt_add_i32(loc_3, 4)
						if loc_2 == 0 then
							desired = 3
							break
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					reg_0 = FUNC_LIST[189](loc_4, loc_5, loc_2)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				rt_store_i32_n8(memory_at_0, rt_add_i32(loc_2, loc_4), 0)
				reg_1 = FUNC_LIST[333](rt_add_i32(loc_3, 4), 0, 2621, 1)
				loc_2 = reg_1
				loc_7 = loc_2
				loc_4 = rt_add_i32(loc_7, 8)
				rt_store_i32(memory_at_0, loc_3 + 24, rt_load_i32(memory_at_0, loc_4))
				rt_store_i64(memory_at_0, loc_3 + 16, rt_load_i64(memory_at_0, loc_2))
				rt_store_i64(memory_at_0, loc_2, rt_i64_ZERO)
				rt_store_i32(memory_at_0, loc_7 + 8, 0)
				reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 16), 2621, 1)
				loc_2 = reg_1
				rt_store_i64(memory_at_0, loc_0, rt_load_i64(memory_at_0, loc_2))
				loc_4 = rt_add_i32(loc_2, 8)
				rt_store_i32(memory_at_0, loc_0 + 8, rt_load_i32(memory_at_0, loc_4))
				rt_store_i64(memory_at_0, loc_2, rt_i64_ZERO)
				rt_store_i32(memory_at_0, loc_2 + 8, 0)
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 27), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 16))
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 15), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 4))
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				GLOBAL_LIST[0].value = rt_add_i32(loc_3, 32)
				desired = 0
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		FUNC_LIST[6](rt_add_i32(loc_3, 4))
		error("out of code bounds")
	end
end
FUNC_LIST[40] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 3384 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[41] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda0'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 3436
		break
	end
	return reg_0
end
FUNC_LIST[42] = --[[ std::__2::__throw_bad_any_cast[abi:ne180100]() ]] function()
	local loc_0 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[456](4)
		loc_0 = reg_0
		rt_store_i32(memory_at_0, loc_0, 0)
		reg_0 = FUNC_LIST[43](loc_0)
		FUNC_LIST[0](reg_0, 8040, 7)
		error("out of code bounds")
	end
end
FUNC_LIST[43] = --[[ std::bad_any_cast::bad_any_cast() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[506](loc_0)
		loc_0 = reg_0
		rt_store_i32(memory_at_0, loc_0, 8008)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[44] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[45] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 3452)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[46] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 3452)
		break
	end
end
FUNC_LIST[47] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[48] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[49] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	while true do
		FUNC_LIST[50](loc_0, rt_add_i32(loc_1, 4), loc_2)
		break
	end
end
FUNC_LIST[50] = --[[ RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&)::operator()(std::__2::any const&) const ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 32)
		GLOBAL_LIST[0].value = loc_3
		while true do
			while true do
				loc_4 = rt_load_i32(memory_at_0, loc_2)
				if loc_4 == 0 then
					break
				end
				reg_0 = TABLE_LIST[0].data[loc_4](3, loc_2, 0, 9828, 3648)
				loc_2 = reg_0
				if loc_2 == 0 then
					break
				end
				loc_5 = rt_load_i32(memory_at_0, loc_2)
				reg_0 = FUNC_LIST[192](loc_5)
				loc_2 = reg_0
				if loc_2 >= 2147483640 then
					desired = 1
					break
				end
				while true do
					while true do
						if loc_2 >= 11 then
							while true do
								loc_6 = rt_add_i32(bit_or(loc_2, 7), 1)
								reg_0 = FUNC_LIST[227](loc_6)
								loc_4 = reg_0
								rt_store_i32(memory_at_0, loc_3 + 12, bit_or(loc_6, 2147483648))
								rt_store_i32(memory_at_0, loc_3 + 4, loc_4)
								rt_store_i32(memory_at_0, loc_3 + 8, loc_2)
								desired = 4
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						rt_store_i32_n8(memory_at_0, loc_3 + 15, loc_2)
						loc_4 = rt_add_i32(loc_3, 4)
						if loc_2 == 0 then
							desired = 3
							break
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					reg_0 = FUNC_LIST[189](loc_4, loc_5, loc_2)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				rt_store_i32_n8(memory_at_0, rt_add_i32(loc_2, loc_4), 0)
				reg_1 = FUNC_LIST[333](rt_add_i32(loc_3, 4), 0, 2621, 1)
				loc_2 = reg_1
				loc_7 = loc_2
				loc_4 = rt_add_i32(loc_7, 8)
				rt_store_i32(memory_at_0, loc_3 + 24, rt_load_i32(memory_at_0, loc_4))
				rt_store_i64(memory_at_0, loc_3 + 16, rt_load_i64(memory_at_0, loc_2))
				rt_store_i64(memory_at_0, loc_2, rt_i64_ZERO)
				rt_store_i32(memory_at_0, loc_7 + 8, 0)
				reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 16), 2621, 1)
				loc_2 = reg_1
				rt_store_i64(memory_at_0, loc_0, rt_load_i64(memory_at_0, loc_2))
				loc_4 = rt_add_i32(loc_2, 8)
				rt_store_i32(memory_at_0, loc_0 + 8, rt_load_i32(memory_at_0, loc_4))
				rt_store_i64(memory_at_0, loc_2, rt_i64_ZERO)
				rt_store_i32(memory_at_0, loc_2 + 8, 0)
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 27), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 16))
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 15), 0) then
					while true do
						FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 4))
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				GLOBAL_LIST[0].value = rt_add_i32(loc_3, 32)
				desired = 0
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		FUNC_LIST[6](rt_add_i32(loc_3, 4))
		error("out of code bounds")
	end
end
FUNC_LIST[51] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 3652 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[52] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda1'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 3704
		break
	end
	return reg_0
end
FUNC_LIST[53] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[54] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 3720)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[55] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 3720)
		break
	end
end
FUNC_LIST[56] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[57] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[58] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	while true do
		FUNC_LIST[59](loc_0, rt_add_i32(loc_1, 4), loc_2)
		break
	end
end
FUNC_LIST[59] = --[[ RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&)::operator()(std::__2::any const&) const ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 32)
		GLOBAL_LIST[0].value = loc_3
		while true do
			loc_4 = rt_load_i32(memory_at_0, loc_2)
			if loc_4 == 0 then
				break
			end
			reg_0 = TABLE_LIST[0].data[loc_4](3, loc_2, 0, 2740, 3916)
			loc_2 = reg_0
			if loc_2 == 0 then
				break
			end
			while true do
				if rt_ge_i32(rt_load_i32_i8(memory_at_0, loc_2 + 11), 0) then
					while true do
						rt_store_i32(memory_at_0, loc_3 + 8, rt_load_i32(memory_at_0, loc_2 + 8))
						rt_store_i64(memory_at_0, loc_3, rt_load_i64(memory_at_0, loc_2))
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				FUNC_LIST[332](loc_3, rt_load_i32(memory_at_0, loc_2), rt_load_i32(memory_at_0, loc_2 + 4))
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			reg_1 = FUNC_LIST[333](loc_3, 0, 2621, 1)
			loc_2 = reg_1
			loc_5 = loc_2
			loc_4 = rt_add_i32(loc_5, 8)
			rt_store_i32(memory_at_0, loc_3 + 24, rt_load_i32(memory_at_0, loc_4))
			rt_store_i64(memory_at_0, loc_3 + 16, rt_load_i64(memory_at_0, loc_2))
			rt_store_i64(memory_at_0, loc_2, rt_i64_ZERO)
			rt_store_i32(memory_at_0, loc_5 + 8, 0)
			reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 16), 2621, 1)
			loc_2 = reg_1
			rt_store_i64(memory_at_0, loc_0, rt_load_i64(memory_at_0, loc_2))
			reg_0 = loc_0
			loc_0 = rt_add_i32(loc_2, 8)
			rt_store_i32(memory_at_0, reg_0 + 8, rt_load_i32(memory_at_0, loc_0))
			rt_store_i64(memory_at_0, loc_2, rt_i64_ZERO)
			rt_store_i32(memory_at_0, loc_2 + 8, 0)
			if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 27), 0) then
				while true do
					FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 16))
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 11), 0) then
				while true do
					FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3))
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			GLOBAL_LIST[0].value = rt_add_i32(loc_3, 32)
			desired = 0
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		FUNC_LIST[42]()
		error("out of code bounds")
	end
end
FUNC_LIST[60] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 3920 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[61] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda2'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 3972
		break
	end
	return reg_0
end
FUNC_LIST[62] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[63] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 3988)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[64] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 3988)
		break
	end
end
FUNC_LIST[65] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[66] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[67] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			loc_3 = rt_load_i32(memory_at_0, loc_2)
			if loc_3 ~= 0 then
				while true do
					reg_0 = TABLE_LIST[0].data[loc_3](3, loc_2, 0, 9816, 4184)
					loc_2 = reg_0
					if loc_2 ~= 0 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		FUNC_LIST[346](loc_0, rt_load_i32_i8(memory_at_0, loc_2))
		break
	end
end
FUNC_LIST[68] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 4188 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[69] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda3'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 4240
		break
	end
	return reg_0
end
FUNC_LIST[70] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[71] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 4256)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[72] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 4256)
		break
	end
end
FUNC_LIST[73] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[74] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[75] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			loc_3 = rt_load_i32(memory_at_0, loc_2)
			if loc_3 ~= 0 then
				while true do
					reg_0 = TABLE_LIST[0].data[loc_3](3, loc_2, 0, 9868, 4452)
					loc_2 = reg_0
					if loc_2 ~= 0 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		FUNC_LIST[346](loc_0, rt_load_i32_u8(memory_at_0, loc_2))
		break
	end
end
FUNC_LIST[76] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 4456 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[77] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda4'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 4508
		break
	end
	return reg_0
end
FUNC_LIST[78] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[79] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 4524)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[80] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 4524)
		break
	end
end
FUNC_LIST[81] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[82] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[83] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			loc_3 = rt_load_i32(memory_at_0, loc_2)
			if loc_3 ~= 0 then
				while true do
					reg_0 = TABLE_LIST[0].data[loc_3](3, loc_2, 0, 9880, 4720)
					loc_2 = reg_0
					if loc_2 ~= 0 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		FUNC_LIST[346](loc_0, rt_load_i32_i16(memory_at_0, loc_2))
		break
	end
end
FUNC_LIST[84] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 4724 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[85] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda5'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 4776
		break
	end
	return reg_0
end
FUNC_LIST[86] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[87] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 4792)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[88] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 4792)
		break
	end
end
FUNC_LIST[89] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[90] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[91] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			loc_3 = rt_load_i32(memory_at_0, loc_2)
			if loc_3 ~= 0 then
				while true do
					reg_0 = TABLE_LIST[0].data[loc_3](3, loc_2, 0, 9892, 4988)
					loc_2 = reg_0
					if loc_2 ~= 0 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		FUNC_LIST[346](loc_0, rt_load_i32_u16(memory_at_0, loc_2))
		break
	end
end
FUNC_LIST[92] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 4992 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[93] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda6'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 5044
		break
	end
	return reg_0
end
FUNC_LIST[94] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[95] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 5060)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[96] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 5060)
		break
	end
end
FUNC_LIST[97] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[98] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[99] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			loc_3 = rt_load_i32(memory_at_0, loc_2)
			if loc_3 ~= 0 then
				while true do
					reg_0 = TABLE_LIST[0].data[loc_3](3, loc_2, 0, 9904, 5256)
					loc_2 = reg_0
					if loc_2 ~= 0 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		FUNC_LIST[346](loc_0, rt_load_i32(memory_at_0, loc_2))
		break
	end
end
FUNC_LIST[100] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 5260 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[101] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda7'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 5312
		break
	end
	return reg_0
end
FUNC_LIST[102] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[103] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 5328)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[104] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 5328)
		break
	end
end
FUNC_LIST[105] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[106] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[107] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			loc_3 = rt_load_i32(memory_at_0, loc_2)
			if loc_3 ~= 0 then
				while true do
					reg_0 = TABLE_LIST[0].data[loc_3](3, loc_2, 0, 9916, 5524)
					loc_2 = reg_0
					if loc_2 ~= 0 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		FUNC_LIST[356](loc_0, rt_load_i32(memory_at_0, loc_2))
		break
	end
end
FUNC_LIST[108] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 5528 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[109] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda8'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 5580
		break
	end
	return reg_0
end
FUNC_LIST[110] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[111] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 5596)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[112] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 5596)
		break
	end
end
FUNC_LIST[113] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[114] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[115] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			loc_3 = rt_load_i32(memory_at_0, loc_2)
			if loc_3 ~= 0 then
				while true do
					reg_0 = TABLE_LIST[0].data[loc_3](3, loc_2, 0, 9928, 5792)
					loc_2 = reg_0
					if loc_2 ~= 0 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		FUNC_LIST[350](loc_0, rt_load_i32(memory_at_0, loc_2))
		break
	end
end
FUNC_LIST[116] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 5796 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[117] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda9'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 5848
		break
	end
	return reg_0
end
FUNC_LIST[118] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[119] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 5864)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[120] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 5864)
		break
	end
end
FUNC_LIST[121] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[122] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[123] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			loc_3 = rt_load_i32(memory_at_0, loc_2)
			if loc_3 ~= 0 then
				while true do
					reg_0 = TABLE_LIST[0].data[loc_3](3, loc_2, 0, 9940, 6060)
					loc_2 = reg_0
					if loc_2 ~= 0 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		FUNC_LIST[359](loc_0, rt_load_i32(memory_at_0, loc_2))
		break
	end
end
FUNC_LIST[124] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 6064 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[125] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda10'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 6116
		break
	end
	return reg_0
end
FUNC_LIST[126] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[127] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 6132)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[128] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 6132)
		break
	end
end
FUNC_LIST[129] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[130] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[131] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			loc_3 = rt_load_i32(memory_at_0, loc_2)
			if loc_3 ~= 0 then
				while true do
					reg_0 = TABLE_LIST[0].data[loc_3](3, loc_2, 0, 9952, 6328)
					loc_2 = reg_0
					if loc_2 ~= 0 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		FUNC_LIST[353](loc_0, rt_load_i64(memory_at_0, loc_2))
		break
	end
end
FUNC_LIST[132] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 6332 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[133] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda11'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 6384
		break
	end
	return reg_0
end
FUNC_LIST[134] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[135] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 6400)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[136] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 6400)
		break
	end
end
FUNC_LIST[137] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[138] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[139] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			loc_3 = rt_load_i32(memory_at_0, loc_2)
			if loc_3 ~= 0 then
				while true do
					reg_0 = TABLE_LIST[0].data[loc_3](3, loc_2, 0, 9964, 6596)
					loc_2 = reg_0
					if loc_2 ~= 0 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		FUNC_LIST[362](loc_0, rt_load_i64(memory_at_0, loc_2))
		break
	end
end
FUNC_LIST[140] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 6600 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[141] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda12'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 6652
		break
	end
	return reg_0
end
FUNC_LIST[142] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[143] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 6668)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[144] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 6668)
		break
	end
end
FUNC_LIST[145] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[146] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[147] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			loc_3 = rt_load_i32(memory_at_0, loc_2)
			if loc_3 ~= 0 then
				while true do
					reg_0 = TABLE_LIST[0].data[loc_3](3, loc_2, 0, 9976, 6864)
					loc_2 = reg_0
					if loc_2 ~= 0 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		FUNC_LIST[365](loc_0, rt_load_f32(memory_at_0, loc_2))
		break
	end
end
FUNC_LIST[148] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 6868 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[149] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda13'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 6920
		break
	end
	return reg_0
end
FUNC_LIST[150] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[151] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 6936)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[152] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 6936)
		break
	end
end
FUNC_LIST[153] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[154] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[155] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			loc_3 = rt_load_i32(memory_at_0, loc_2)
			if loc_3 ~= 0 then
				while true do
					reg_0 = TABLE_LIST[0].data[loc_3](3, loc_2, 0, 9988, 7132)
					loc_2 = reg_0
					if loc_2 ~= 0 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		FUNC_LIST[371](loc_0, rt_load_f64(memory_at_0, loc_2))
		break
	end
end
FUNC_LIST[156] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 7136 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[157] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda14'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 7188
		break
	end
	return reg_0
end
FUNC_LIST[158] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[159] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 7204)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[160] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 7204)
		break
	end
end
FUNC_LIST[161] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[162] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[163] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			loc_3 = rt_load_i32(memory_at_0, loc_2)
			if loc_3 ~= 0 then
				while true do
					reg_0 = TABLE_LIST[0].data[loc_3](3, loc_2, 0, 10000, 7400)
					loc_2 = reg_0
					if loc_2 ~= 0 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		FUNC_LIST[373](loc_0, rt_load_i64(memory_at_0, loc_2), rt_load_i64(memory_at_0, loc_2 + 8))
		break
	end
end
FUNC_LIST[164] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 7404 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[165] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda15'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 7456
		break
	end
	return reg_0
end
FUNC_LIST[166] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[167] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 7472)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[168] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 7472)
		break
	end
end
FUNC_LIST[169] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[170] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[171] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			loc_3 = rt_load_i32(memory_at_0, loc_2)
			if loc_3 ~= 0 then
				while true do
					reg_0 = TABLE_LIST[0].data[loc_3](3, loc_2, 0, 9804, 7668)
					loc_2 = reg_0
					if loc_2 ~= 0 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[42]()
			error("out of code bounds")
		end
		loc_3 = rt_load_i32_u8(memory_at_0, loc_2)
		loc_2 = (if loc_3 ~= 0 then 4 else 5)
		rt_store_i32_n8(memory_at_0, loc_0 + 11, loc_2)
		reg_0 = FUNC_LIST[188](loc_0, (if loc_3 ~= 0 then 1208 else 1213), loc_2)
		rt_store_i32_n8(memory_at_0, rt_add_i32(reg_0, loc_2), 0)
		break
	end
end
FUNC_LIST[172] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 7672 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[173] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda16'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 7724
		break
	end
	return reg_0
end
FUNC_LIST[174] = --[[ std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__base() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[175] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::~__func() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[176] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](8)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_1, 7740)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[177] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::__clone(std::__2::__function::__base<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>*) const ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_1, 7740)
		break
	end
end
FUNC_LIST[178] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy() ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[179] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::destroy_deallocate() ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[180] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::operator()(std::__2::any const&) ]] function(loc_0, loc_1, loc_2)
	while true do
		FUNC_LIST[181](loc_0, rt_add_i32(loc_1, 4), loc_2)
		break
	end
end
FUNC_LIST[181] = --[[ RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&)::operator()(std::__2::any const&) const ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 32)
		GLOBAL_LIST[0].value = loc_3
		while true do
			loc_4 = rt_load_i32(memory_at_0, loc_2)
			if loc_4 == 0 then
				break
			end
			reg_0 = TABLE_LIST[0].data[loc_4](3, loc_2, 0, 2792, 7936)
			loc_2 = reg_0
			if loc_2 == 0 then
				break
			end
			while true do
				loc_2 = rt_load_i32(memory_at_0, loc_2)
				if rt_ge_i32(rt_load_i32_i8(memory_at_0, loc_2 + 11), 0) then
					while true do
						rt_store_i32(memory_at_0, loc_3 + 8, rt_load_i32(memory_at_0, loc_2 + 8))
						rt_store_i64(memory_at_0, loc_3, rt_load_i64(memory_at_0, loc_2))
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				FUNC_LIST[332](loc_3, rt_load_i32(memory_at_0, loc_2), rt_load_i32(memory_at_0, loc_2 + 4))
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			reg_1 = FUNC_LIST[333](loc_3, 0, 2574, 48)
			loc_2 = reg_1
			loc_5 = loc_2
			loc_4 = rt_add_i32(loc_5, 8)
			rt_store_i32(memory_at_0, loc_3 + 24, rt_load_i32(memory_at_0, loc_4))
			rt_store_i64(memory_at_0, loc_3 + 16, rt_load_i64(memory_at_0, loc_2))
			rt_store_i64(memory_at_0, loc_2, rt_i64_ZERO)
			rt_store_i32(memory_at_0, loc_5 + 8, 0)
			reg_1 = FUNC_LIST[337](rt_add_i32(loc_3, 16), 2031, 5)
			loc_2 = reg_1
			rt_store_i64(memory_at_0, loc_0, rt_load_i64(memory_at_0, loc_2))
			reg_0 = loc_0
			loc_0 = rt_add_i32(loc_2, 8)
			rt_store_i32(memory_at_0, reg_0 + 8, rt_load_i32(memory_at_0, loc_0))
			rt_store_i64(memory_at_0, loc_2, rt_i64_ZERO)
			rt_store_i32(memory_at_0, loc_2 + 8, 0)
			if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 27), 0) then
				while true do
					FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3 + 16))
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_3 + 11), 0) then
				while true do
					FUNC_LIST[230](rt_load_i32(memory_at_0, loc_3))
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			GLOBAL_LIST[0].value = rt_add_i32(loc_3, 32)
			desired = 0
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		FUNC_LIST[42]()
		error("out of code bounds")
	end
end
FUNC_LIST[182] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target(std::type_info const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1 + 4) == 7940 then rt_add_i32(loc_0, 4) else 0)
		break
	end
	return reg_0
end
FUNC_LIST[183] = --[[ std::__2::__function::__func<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&), std::__2::allocator<RBX::Instance::toString(std::__2::any)::'lambda17'(std::__2::any const&)>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>::target_type() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 7992
		break
	end
	return reg_0
end
FUNC_LIST[184] = --[[ void std::__2::__hash_table<std::__2::__hash_value_type<std::__2::type_index, std::__2::function<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>>, std::__2::__unordered_map_hasher<std::__2::type_index, std::__2::__hash_value_type<std::__2::type_index, std::__2::function<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>>, std::__2::hash<std::__2::type_index>, std::__2::equal_to<std::__2::type_index>, true>, std::__2::__unordered_map_equal<std::__2::type_index, std::__2::__hash_value_type<std::__2::type_index, std::__2::function<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>>, std::__2::equal_to<std::__2::type_index>, std::__2::hash<std::__2::type_index>, true>, std::__2::allocator<std::__2::__hash_value_type<std::__2::type_index, std::__2::function<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> (std::__2::any const&)>>>>::__do_rehash<true>(unsigned long) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local reg_0
	local desired
	while true do
		while true do
			while true do
				while true do
					while true do
						if loc_1 ~= 0 then
							while true do
								if loc_1 >= 1073741824 then
									desired = 4
									break
								end
								reg_0 = FUNC_LIST[227](rt_shl_i32(loc_1, 2))
								loc_3 = reg_0
								loc_2 = rt_load_i32(memory_at_0, loc_0)
								rt_store_i32(memory_at_0, loc_0, loc_3)
								if loc_2 ~= 0 then
									while true do
										FUNC_LIST[230](loc_2)
										break
									end
									if desired then
										break
									end
								end
								rt_store_i32(memory_at_0, loc_0 + 4, loc_1)
								loc_6 = bit_and(loc_1, 3)
								loc_2 = 0
								if loc_1 >= 4 then
									while true do
										loc_7 = bit_and(loc_1, 1073741820)
										while true do
											loc_3 = rt_shl_i32(loc_2, 2)
											rt_store_i32(memory_at_0, rt_add_i32(loc_3, rt_load_i32(memory_at_0, loc_0)), 0)
											rt_store_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_0), loc_3) + 4, 0)
											rt_store_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_0), loc_3) + 8, 0)
											rt_store_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_0), loc_3) + 12, 0)
											loc_2 = rt_add_i32(loc_2, 4)
											loc_5 = rt_add_i32(loc_5, 4)
											if loc_5 ~= loc_7 then
												continue
											end
											break
										end
										if desired then
											break
										end
										break
									end
									if desired then
										break
									end
								end
								if loc_6 ~= 0 then
									while true do
										while true do
											rt_store_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_0), rt_shl_i32(loc_2, 2)), 0)
											loc_2 = rt_add_i32(loc_2, 1)
											loc_4 = rt_add_i32(loc_4, 1)
											if loc_4 ~= loc_6 then
												continue
											end
											break
										end
										if desired then
											break
										end
										break
									end
									if desired then
										break
									end
								end
								loc_3 = rt_load_i32(memory_at_0, loc_0 + 8)
								if loc_3 == 0 then
									desired = 1
									break
								end
								loc_2 = rt_add_i32(loc_0, 8)
								loc_4 = rt_load_i32(memory_at_0, loc_3 + 4)
								loc_5 = rt_popcnt_i32(loc_1)
								if loc_5 < 2 then
									desired = 3
									break
								end
								if loc_1 <= loc_4 then
									while true do
										loc_4 = (loc_4 % loc_1)
										break
									end
									if desired then
										break
									end
								end
								rt_store_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_0), rt_shl_i32(loc_4, 2)), loc_2)
								loc_2 = rt_load_i32(memory_at_0, loc_3)
								if loc_2 == 0 then
									desired = 1
									break
								end
								if loc_5 <= 1 then
									desired = 2
									break
								end
								while true do
									loc_5 = rt_load_i32(memory_at_0, loc_2 + 4)
									if loc_1 <= loc_5 then
										while true do
											loc_5 = (loc_5 % loc_1)
											break
										end
										if desired then
											if desired == 6 then
												desired = nil
												continue
											end
											break
										end
									end
									while true do
										if loc_4 == loc_5 then
											while true do
												loc_3 = loc_2
												desired = 7
												break
											end
											if desired then
												if desired == 7 then
													desired = nil
												end
												break
											end
										end
										loc_6 = rt_shl_i32(loc_5, 2)
										loc_7 = rt_add_i32(loc_6, rt_load_i32(memory_at_0, loc_0))
										if rt_load_i32(memory_at_0, loc_7) == 0 then
											while true do
												rt_store_i32(memory_at_0, loc_7, loc_3)
												loc_3 = loc_2
												loc_4 = loc_5
												desired = 7
												break
											end
											if desired then
												if desired == 7 then
													desired = nil
												end
												break
											end
										end
										rt_store_i32(memory_at_0, loc_3, rt_load_i32(memory_at_0, loc_2))
										rt_store_i32(memory_at_0, loc_2, rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_0), loc_6))))
										rt_store_i32(memory_at_0, rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_0), loc_6)), loc_2)
										break
									end
									if desired then
										if desired == 6 then
											desired = nil
											continue
										end
										break
									end
									loc_2 = rt_load_i32(memory_at_0, loc_3)
									if loc_2 ~= 0 then
										continue
									end
									break
								end
								if desired then
									break
								end
								desired = 1
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						loc_2 = rt_load_i32(memory_at_0, loc_0)
						rt_store_i32(memory_at_0, loc_0, 0)
						if loc_2 ~= 0 then
							while true do
								FUNC_LIST[230](loc_2)
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						rt_store_i32(memory_at_0, loc_0 + 4, 0)
						desired = 1
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					FUNC_LIST[21]()
					error("out of code bounds")
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_4 = bit_and(loc_4, rt_sub_i32(loc_1, 1))
				rt_store_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_0), rt_shl_i32(loc_4, 2)), loc_2)
				loc_2 = rt_load_i32(memory_at_0, loc_3)
				if loc_2 == 0 then
					desired = 1
					break
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_7 = rt_sub_i32(loc_1, 1)
			while true do
				while true do
					loc_5 = bit_and(rt_load_i32(memory_at_0, loc_2 + 4), loc_7)
					if loc_4 == loc_5 then
						while true do
							loc_3 = loc_2
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					loc_6 = rt_shl_i32(loc_5, 2)
					loc_1 = rt_add_i32(loc_6, rt_load_i32(memory_at_0, loc_0))
					if rt_load_i32(memory_at_0, loc_1) ~= 0 then
						while true do
							rt_store_i32(memory_at_0, loc_3, rt_load_i32(memory_at_0, loc_2))
							rt_store_i32(memory_at_0, loc_2, rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_0), loc_6))))
							rt_store_i32(memory_at_0, rt_load_i32(memory_at_0, rt_add_i32(rt_load_i32(memory_at_0, loc_0), loc_6)), loc_2)
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					rt_store_i32(memory_at_0, loc_1, loc_3)
					loc_3 = loc_2
					loc_4 = loc_5
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
						continue
					end
					break
				end
				loc_2 = rt_load_i32(memory_at_0, loc_3)
				if loc_2 ~= 0 then
					continue
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			break
		end
		break
	end
end
FUNC_LIST[185] = --[[ _GLOBAL__sub_I_main.cpp ]] function()
	local loc_0 = 0
	local loc_1 = 0
	local reg_0
	while true do
		loc_0 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_0
		rt_store_i32_n8(memory_at_0, loc_0 + 8, 0)
		rt_store_i32(memory_at_0, loc_0 + 4, 1701667175)
		rt_store_i32_n8(memory_at_0, loc_0 + 15, 4)
		reg_0 = FUNC_LIST[9](rt_add_i32(loc_0, 4))
		loc_1 = reg_0
		if rt_lt_i32(rt_load_i32_i8(memory_at_0, loc_0 + 15), 0) then
			while true do
				FUNC_LIST[230](rt_load_i32(memory_at_0, loc_0 + 4))
				break
			end
		end
		rt_store_i32(memory_at_0, 10492, loc_1)
		GLOBAL_LIST[0].value = rt_add_i32(loc_0, 16)
		break
	end
end
FUNC_LIST[186] = --[[ main ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[10]()
		break
	end
	return reg_0
end
FUNC_LIST[187] = --[[ __cxa_atexit ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = 0
		break
	end
	return reg_0
end
FUNC_LIST[188] = --[[ __memcpy ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local reg_0
	local desired
	while true do
		if loc_2 >= 512 then
			while true do
				FUNC_LIST[1](loc_0, loc_1, loc_2)
				reg_0 = loc_0
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		loc_3 = rt_add_i32(loc_0, loc_2)
		while true do
			if bit_and(bit_xor(loc_0, loc_1), 3) == 0 then
				while true do
					while true do
						if bit_and(loc_0, 3) == 0 then
							while true do
								loc_2 = loc_0
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						if loc_2 == 0 then
							while true do
								loc_2 = loc_0
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_2 = loc_0
						while true do
							rt_store_i32_n8(memory_at_0, loc_2, rt_load_i32_u8(memory_at_0, loc_1))
							loc_1 = rt_add_i32(loc_1, 1)
							loc_2 = rt_add_i32(loc_2, 1)
							if bit_and(loc_2, 3) == 0 then
								desired = 3
								break
							end
							if loc_2 < loc_3 then
								continue
							end
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
						break
					end
					if desired then
						break
					end
					while true do
						loc_4 = bit_and(loc_3, 4294967292)
						if loc_4 < 64 then
							break
						end
						loc_5 = rt_add_i32(loc_4, 4294967232)
						if loc_2 > loc_5 then
							break
						end
						while true do
							rt_store_i32(memory_at_0, loc_2, rt_load_i32(memory_at_0, loc_1))
							rt_store_i32(memory_at_0, loc_2 + 4, rt_load_i32(memory_at_0, loc_1 + 4))
							rt_store_i32(memory_at_0, loc_2 + 8, rt_load_i32(memory_at_0, loc_1 + 8))
							rt_store_i32(memory_at_0, loc_2 + 12, rt_load_i32(memory_at_0, loc_1 + 12))
							rt_store_i32(memory_at_0, loc_2 + 16, rt_load_i32(memory_at_0, loc_1 + 16))
							rt_store_i32(memory_at_0, loc_2 + 20, rt_load_i32(memory_at_0, loc_1 + 20))
							rt_store_i32(memory_at_0, loc_2 + 24, rt_load_i32(memory_at_0, loc_1 + 24))
							rt_store_i32(memory_at_0, loc_2 + 28, rt_load_i32(memory_at_0, loc_1 + 28))
							rt_store_i32(memory_at_0, loc_2 + 32, rt_load_i32(memory_at_0, loc_1 + 32))
							rt_store_i32(memory_at_0, loc_2 + 36, rt_load_i32(memory_at_0, loc_1 + 36))
							rt_store_i32(memory_at_0, loc_2 + 40, rt_load_i32(memory_at_0, loc_1 + 40))
							rt_store_i32(memory_at_0, loc_2 + 44, rt_load_i32(memory_at_0, loc_1 + 44))
							rt_store_i32(memory_at_0, loc_2 + 48, rt_load_i32(memory_at_0, loc_1 + 48))
							rt_store_i32(memory_at_0, loc_2 + 52, rt_load_i32(memory_at_0, loc_1 + 52))
							rt_store_i32(memory_at_0, loc_2 + 56, rt_load_i32(memory_at_0, loc_1 + 56))
							rt_store_i32(memory_at_0, loc_2 + 60, rt_load_i32(memory_at_0, loc_1 + 60))
							loc_1 = rt_sub_i32(loc_1, 4294967232)
							loc_2 = rt_sub_i32(loc_2, 4294967232)
							if loc_2 <= loc_5 then
								continue
							end
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
						break
					end
					if desired then
						break
					end
					if loc_2 >= loc_4 then
						desired = 1
						break
					end
					while true do
						rt_store_i32(memory_at_0, loc_2, rt_load_i32(memory_at_0, loc_1))
						loc_1 = rt_add_i32(loc_1, 4)
						loc_2 = rt_add_i32(loc_2, 4)
						if loc_2 < loc_4 then
							continue
						end
						break
					end
					if desired then
						break
					end
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			if loc_3 < 4 then
				while true do
					loc_2 = loc_0
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_4 = rt_sub_i32(loc_3, 4)
			if loc_0 > loc_4 then
				while true do
					loc_2 = loc_0
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_2 = loc_0
			while true do
				rt_store_i32_n8(memory_at_0, loc_2, rt_load_i32_u8(memory_at_0, loc_1))
				rt_store_i32_n8(memory_at_0, loc_2 + 1, rt_load_i32_u8(memory_at_0, loc_1 + 1))
				rt_store_i32_n8(memory_at_0, loc_2 + 2, rt_load_i32_u8(memory_at_0, loc_1 + 2))
				rt_store_i32_n8(memory_at_0, loc_2 + 3, rt_load_i32_u8(memory_at_0, loc_1 + 3))
				loc_1 = rt_add_i32(loc_1, 4)
				loc_2 = rt_add_i32(loc_2, 4)
				if loc_2 <= loc_4 then
					continue
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		if loc_2 < loc_3 then
			while true do
				while true do
					rt_store_i32_n8(memory_at_0, loc_2, rt_load_i32_u8(memory_at_0, loc_1))
					loc_1 = rt_add_i32(loc_1, 1)
					loc_2 = rt_add_i32(loc_2, 1)
					if loc_2 ~= loc_3 then
						continue
					end
					break
				end
				if desired then
					break
				end
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[189] = --[[ memmove ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local reg_0
	local desired
	while true do
		while true do
			if loc_0 == loc_1 then
				break
			end
			loc_3 = rt_add_i32(loc_0, loc_2)
			if rt_sub_i32(loc_1, loc_3) <= rt_sub_i32(0, rt_shl_i32(loc_2, 1)) then
				while true do
					reg_0 = FUNC_LIST[188](loc_0, loc_1, loc_2)
					desired = 0
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_4 = bit_and(bit_xor(loc_0, loc_1), 3)
			while true do
				while true do
					if loc_0 < loc_1 then
						while true do
							if loc_4 ~= 0 then
								while true do
									loc_3 = loc_0
									desired = 2
									break
								end
								if desired then
									break
								end
							end
							if bit_and(loc_0, 3) == 0 then
								while true do
									loc_3 = loc_0
									desired = 3
									break
								end
								if desired then
									break
								end
							end
							loc_3 = loc_0
							while true do
								if loc_2 == 0 then
									desired = 1
									break
								end
								rt_store_i32_n8(memory_at_0, loc_3, rt_load_i32_u8(memory_at_0, loc_1))
								loc_1 = rt_add_i32(loc_1, 1)
								loc_2 = rt_sub_i32(loc_2, 1)
								loc_3 = rt_add_i32(loc_3, 1)
								if bit_and(loc_3, 3) ~= 0 then
									continue
								end
								break
							end
							if desired then
								break
							end
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					while true do
						if loc_4 ~= 0 then
							break
						end
						if bit_and(loc_3, 3) ~= 0 then
							while true do
								while true do
									if loc_2 == 0 then
										desired = 1
										break
									end
									loc_2 = rt_sub_i32(loc_2, 1)
									loc_3 = rt_add_i32(loc_0, loc_2)
									rt_store_i32_n8(memory_at_0, loc_3, rt_load_i32_u8(memory_at_0, rt_add_i32(loc_1, loc_2)))
									if bit_and(loc_3, 3) ~= 0 then
										continue
									end
									break
								end
								if desired then
									break
								end
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						if loc_2 <= 3 then
							break
						end
						while true do
							loc_2 = rt_sub_i32(loc_2, 4)
							rt_store_i32(memory_at_0, rt_add_i32(loc_0, loc_2), rt_load_i32(memory_at_0, rt_add_i32(loc_1, loc_2)))
							if loc_2 > 3 then
								continue
							end
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					if loc_2 == 0 then
						desired = 1
						break
					end
					while true do
						loc_2 = rt_sub_i32(loc_2, 1)
						rt_store_i32_n8(memory_at_0, rt_add_i32(loc_0, loc_2), rt_load_i32_u8(memory_at_0, rt_add_i32(loc_1, loc_2)))
						if loc_2 ~= 0 then
							continue
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					desired = 1
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				if loc_2 <= 3 then
					break
				end
				while true do
					rt_store_i32(memory_at_0, loc_3, rt_load_i32(memory_at_0, loc_1))
					loc_1 = rt_add_i32(loc_1, 4)
					loc_3 = rt_add_i32(loc_3, 4)
					loc_2 = rt_sub_i32(loc_2, 4)
					if loc_2 > 3 then
						continue
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			if loc_2 == 0 then
				break
			end
			while true do
				rt_store_i32_n8(memory_at_0, loc_3, rt_load_i32_u8(memory_at_0, loc_1))
				loc_3 = rt_add_i32(loc_3, 1)
				loc_1 = rt_add_i32(loc_1, 1)
				loc_2 = rt_sub_i32(loc_2, 1)
				if loc_2 ~= 0 then
					continue
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[190] = --[[ memchr ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local reg_0
	local desired
	while true do
		loc_3 = (if loc_2 ~= 0 then 1 else 0)
		while true do
			while true do
				while true do
					if bit_and(loc_0, 3) == 0 then
						break
					end
					if loc_2 == 0 then
						break
					end
					loc_4 = bit_and(loc_1, 255)
					while true do
						if rt_load_i32_u8(memory_at_0, loc_0) == loc_4 then
							desired = 2
							break
						end
						loc_2 = rt_sub_i32(loc_2, 1)
						loc_3 = (if loc_2 ~= 0 then 1 else 0)
						loc_0 = rt_add_i32(loc_0, 1)
						if bit_and(loc_0, 3) == 0 then
							desired = 3
							break
						end
						if loc_2 ~= 0 then
							continue
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				if loc_3 == 0 then
					desired = 1
					break
				end
				while true do
					if rt_load_i32_u8(memory_at_0, loc_0) == bit_and(loc_1, 255) then
						break
					end
					if loc_2 < 4 then
						break
					end
					loc_4 = rt_mul_i32(bit_and(loc_1, 255), 16843009)
					while true do
						loc_3 = bit_xor(rt_load_i32(memory_at_0, loc_0), loc_4)
						if bit_and(bit_or(rt_sub_i32(16843008, loc_3), loc_3), 2155905152) ~= 2155905152 then
							desired = 2
							break
						end
						loc_0 = rt_add_i32(loc_0, 4)
						loc_2 = rt_sub_i32(loc_2, 4)
						if loc_2 > 3 then
							continue
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				if loc_2 == 0 then
					desired = 1
					break
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_3 = bit_and(loc_1, 255)
			while true do
				if loc_3 == rt_load_i32_u8(memory_at_0, loc_0) then
					while true do
						reg_0 = loc_0
						desired = 0
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
							continue
						end
						break
					end
				end
				loc_0 = rt_add_i32(loc_0, 1)
				loc_2 = rt_sub_i32(loc_2, 1)
				if loc_2 ~= 0 then
					continue
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		reg_0 = 0
		break
	end
	return reg_0
end
FUNC_LIST[191] = --[[ memcmp ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local reg_0
	local desired
	while true do
		while true do
			while true do
				if loc_2 >= 4 then
					while true do
						if bit_and(bit_or(loc_0, loc_1), 3) ~= 0 then
							desired = 2
							break
						end
						while true do
							if rt_load_i32(memory_at_0, loc_0) ~= rt_load_i32(memory_at_0, loc_1) then
								desired = 2
								break
							end
							loc_1 = rt_add_i32(loc_1, 4)
							loc_0 = rt_add_i32(loc_0, 4)
							loc_2 = rt_sub_i32(loc_2, 4)
							if loc_2 > 3 then
								continue
							end
							break
						end
						if desired then
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				if loc_2 == 0 then
					desired = 1
					break
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			while true do
				loc_3 = rt_load_i32_u8(memory_at_0, loc_0)
				loc_4 = rt_load_i32_u8(memory_at_0, loc_1)
				if loc_3 == loc_4 then
					while true do
						loc_1 = rt_add_i32(loc_1, 1)
						loc_0 = rt_add_i32(loc_0, 1)
						loc_2 = rt_sub_i32(loc_2, 1)
						if loc_2 ~= 0 then
							desired = 2
							break
						end
						desired = 1
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
							continue
						end
						break
					end
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			reg_0 = rt_sub_i32(loc_3, loc_4)
			desired = 0
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		reg_0 = 0
		break
	end
	return reg_0
end
FUNC_LIST[192] = --[[ strlen ]] function(loc_0)
	local loc_1 = 0
	local loc_2 = 0
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			while true do
				loc_1 = loc_0
				if bit_and(loc_1, 3) == 0 then
					break
				end
				if rt_load_i32_u8(memory_at_0, loc_1) == 0 then
					while true do
						reg_0 = 0
						desired = 0
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				while true do
					loc_1 = rt_add_i32(loc_1, 1)
					if bit_and(loc_1, 3) == 0 then
						desired = 2
						break
					end
					if rt_load_i32_u8(memory_at_0, loc_1) ~= 0 then
						continue
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				desired = 1
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			while true do
				loc_2 = loc_1
				loc_1 = rt_add_i32(loc_2, 4)
				loc_3 = rt_load_i32(memory_at_0, loc_2)
				if bit_and(bit_or(rt_sub_i32(16843008, loc_3), loc_3), 2155905152) == 2155905152 then
					continue
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			while true do
				loc_1 = loc_2
				loc_2 = rt_add_i32(loc_1, 1)
				if rt_load_i32_u8(memory_at_0, loc_1) ~= 0 then
					continue
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		reg_0 = rt_sub_i32(loc_1, loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[193] = --[[ std::bad_any_cast::what() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 1089
		break
	end
	return reg_0
end
FUNC_LIST[194] = --[[ std::bad_any_cast::~bad_any_cast() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[507](loc_0)
		FUNC_LIST[230](reg_0)
		break
	end
end
FUNC_LIST[195] = --[[ std::__2::bad_function_call::~bad_function_call() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[488](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[196] = --[[ std::__2::bad_function_call::~bad_function_call().1 ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[195](loc_0)
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[197] = --[[ std::__2::bad_function_call::what() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 1136
		break
	end
	return reg_0
end
FUNC_LIST[198] = --[[ std::__2::__next_prime(unsigned long) ]] function(loc_0)
	local loc_1 = 0
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_4 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_4
		rt_store_i32(memory_at_0, loc_4 + 12, loc_0)
		while true do
			if loc_0 <= 211 then
				while true do
					reg_0 = FUNC_LIST[199](8112, 8304, rt_add_i32(loc_4, 12))
					loc_0 = rt_load_i32(memory_at_0, reg_0)
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[200](loc_0)
			loc_6 = rt_div_u32(loc_0, 210)
			loc_3 = rt_mul_i32(loc_6, 210)
			rt_store_i32(memory_at_0, loc_4 + 8, rt_sub_i32(loc_0, loc_3))
			reg_0 = FUNC_LIST[199](8304, 8496, rt_add_i32(loc_4, 8))
			loc_5 = rt_shr_i32(rt_sub_i32(reg_0, 8304), 2)
			while true do
				loc_0 = rt_add_i32(rt_load_i32(memory_at_0, rt_add_i32(rt_shl_i32(loc_5, 2), 8304)), loc_3)
				loc_2 = 5
				while true do
					while true do
						while true do
							loc_3 = loc_2
							if loc_3 == 47 then
								desired = 4
								break
							end
							loc_1 = rt_load_i32(memory_at_0, rt_add_i32(rt_shl_i32(loc_3, 2), 8112))
							loc_7 = rt_div_u32(loc_0, loc_1)
							if loc_7 < loc_1 then
								desired = 1
								break
							end
							loc_2 = rt_add_i32(loc_3, 1)
							if loc_0 ~= rt_mul_i32(loc_1, loc_7) then
								continue
							end
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						if loc_3 < 47 then
							desired = 3
							break
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					loc_3 = 211
					while true do
						loc_1 = rt_div_u32(loc_0, loc_3)
						if loc_1 < loc_3 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_3) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 10)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 12)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 16)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 18)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 22)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 28)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 30)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 36)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 40)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 42)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 46)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 52)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 58)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 60)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 66)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 70)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 72)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 78)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 82)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 88)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 96)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 100)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 102)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 106)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 108)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 112)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 120)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 126)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 130)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 136)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 138)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 142)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 148)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 150)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 156)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 162)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 166)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 168)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 172)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 178)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 180)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 186)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 190)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 192)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 196)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 198)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						if loc_0 == rt_mul_i32(loc_1, loc_2) then
							desired = 3
							break
						end
						loc_1 = rt_add_i32(loc_3, 208)
						loc_2 = rt_div_u32(loc_0, loc_1)
						if loc_2 < loc_1 then
							desired = 1
							break
						end
						loc_3 = rt_add_i32(loc_3, 210)
						if loc_0 ~= rt_mul_i32(loc_1, loc_2) then
							continue
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
						continue
					end
					break
				end
				loc_0 = rt_add_i32(loc_5, 1)
				reg_1 = loc_0
				loc_0 = (if loc_0 == 48 then 1 else 0)
				loc_5 = (if loc_0 ~= 0 then 0 else reg_1)
				loc_6 = rt_add_i32(loc_0, loc_6)
				loc_3 = rt_mul_i32(loc_6, 210)
				continue
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			error("out of code bounds")
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_4, 16)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[199] = --[[ unsigned int const* std::__2::lower_bound[abi:nn180100]<unsigned int const*, unsigned long>(unsigned int const*, unsigned int const*, unsigned long const&) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[201](loc_0, loc_1, loc_2)
		break
	end
	return reg_0
end
FUNC_LIST[200] = --[[ std::__2::enable_if<4ul == 4, void>::type std::__2::__check_for_overflow[abi:nn180100]<4ul>(unsigned long) ]] function(loc_0)
	while true do
		if loc_0 >= 4294967292 then
			while true do
				FUNC_LIST[202](1053)
				error("out of code bounds")
			end
		end
		break
	end
end
FUNC_LIST[201] = --[[ unsigned int const* std::__2::lower_bound[abi:nn180100]<unsigned int const*, unsigned long, std::__2::__less<void, void>>(unsigned int const*, unsigned int const*, unsigned long const&, std::__2::__less<void, void>) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_3
		rt_store_i32_n8(memory_at_0, loc_3 + 14, 0)
		reg_0 = FUNC_LIST[203](loc_0, loc_1, loc_2, rt_add_i32(loc_3, 15), rt_add_i32(loc_3, 14))
		loc_2 = reg_0
		GLOBAL_LIST[0].value = rt_add_i32(loc_3, 16)
		reg_0 = loc_2
		break
	end
	return reg_0
end
FUNC_LIST[202] = --[[ std::__2::__throw_overflow_error[abi:nn180100](char const*) ]] function(loc_0)
	while true do
		FUNC_LIST[2]()
		error("out of code bounds")
	end
end
FUNC_LIST[203] = --[[ unsigned int const* std::__2::__lower_bound[abi:nn180100]<std::__2::_ClassicAlgPolicy, unsigned int const*, unsigned int const*, unsigned long, std::__2::__identity, std::__2::__less<void, void>>(unsigned int const*, unsigned int const*, unsigned long const&, std::__2::__less<void, void>&, std::__2::__identity&) ]] function(loc_0, loc_1, loc_2, loc_3, loc_4)
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local reg_0
	local reg_1
	local reg_2
	local reg_3
	local desired
	while true do
		loc_5 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_5
		reg_0 = FUNC_LIST[204](loc_0, loc_1)
		loc_1 = reg_0
		while true do
			if loc_1 ~= 0 then
				while true do
					reg_0 = FUNC_LIST[205](loc_1)
					loc_6 = reg_0
					rt_store_i32(memory_at_0, loc_5 + 12, loc_0)
					FUNC_LIST[206](rt_add_i32(loc_5, 12), loc_6)
					reg_3 = FUNC_LIST[207](loc_4, rt_load_i32(memory_at_0, loc_5 + 12))
					reg_2 = FUNC_LIST[208](loc_3, reg_3, loc_2)
					loc_7 = reg_2
					loc_1 = (if loc_7 ~= 0 then rt_add_i32(loc_1, bit_xor(loc_6, 4294967295)) else loc_6)
					loc_0 = (if loc_7 ~= 0 then rt_add_i32(rt_load_i32(memory_at_0, loc_5 + 12), 4) else loc_0)
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
						continue
					end
					break
				end
			end
			break
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_5, 16)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[204] = --[[ std::__2::iterator_traits<unsigned int const*>::difference_type std::__2::_IterOps<std::__2::_ClassicAlgPolicy>::distance[abi:nn180100]<unsigned int const*>(unsigned int const*, unsigned int const*) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[209](loc_0, loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[205] = --[[ long std::__2::__half_positive[abi:nn180100]<long, 0>(long) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = rt_shr_u32(loc_0, 1)
		break
	end
	return reg_0
end
FUNC_LIST[206] = --[[ void std::__2::_IterOps<std::__2::_ClassicAlgPolicy>::advance[abi:nn180100]<unsigned int const*, long>(unsigned int const*&, long) ]] function(loc_0, loc_1)
	while true do
		FUNC_LIST[210](loc_0, loc_1)
		break
	end
end
FUNC_LIST[207] = --[[ decltype(std::declval<std::__2::__identity&>()(std::declval<unsigned int const&>())) std::__2::__invoke[abi:nn180100]<std::__2::__identity&, unsigned int const&>(std::__2::__identity&, unsigned int const&) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[212](loc_0, loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[208] = --[[ decltype(std::declval<std::__2::__less<void, void>&>()(std::declval<unsigned int const&>(), std::declval<unsigned long const&>())) std::__2::__invoke[abi:nn180100]<std::__2::__less<void, void>&, unsigned int const&, unsigned long const&>(std::__2::__less<void, void>&, unsigned int const&, unsigned long const&) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[211](loc_0, loc_1, loc_2)
		break
	end
	return reg_0
end
FUNC_LIST[209] = --[[ std::__2::iterator_traits<unsigned int const*>::difference_type std::__2::distance[abi:nn180100]<unsigned int const*>(unsigned int const*, unsigned int const*) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[213](loc_0, loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[210] = --[[ void std::__2::advance[abi:nn180100]<unsigned int const*, long, long, void>(unsigned int const*&, long) ]] function(loc_0, loc_1)
	local reg_0
	local reg_1
	while true do
		reg_1 = FUNC_LIST[214](loc_1)
		FUNC_LIST[215](loc_0, reg_1)
		break
	end
end
FUNC_LIST[211] = --[[ bool std::__2::__less<void, void>::operator()[abi:nn180100]<unsigned int, unsigned long>(unsigned int const&, unsigned long const&) const ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1) < rt_load_i32(memory_at_0, loc_2) then 1 else 0)
		break
	end
	return reg_0
end
FUNC_LIST[212] = --[[ unsigned int const& std::__2::__identity::operator()[abi:nn180100]<unsigned int const&>(unsigned int const&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[213] = --[[ std::__2::iterator_traits<unsigned int const*>::difference_type std::__2::__distance[abi:nn180100]<unsigned int const*>(unsigned int const*, unsigned int const*, std::__2::random_access_iterator_tag) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = rt_shr_i32(rt_sub_i32(loc_1, loc_0), 2)
		break
	end
	return reg_0
end
FUNC_LIST[214] = --[[ std::__2::__convert_to_integral[abi:nn180100](long) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[215] = --[[ void std::__2::__advance[abi:nn180100]<unsigned int const*>(unsigned int const*&, std::__2::iterator_traits<unsigned int const*>::difference_type, std::__2::random_access_iterator_tag) ]] function(loc_0, loc_1)
	while true do
		rt_store_i32(memory_at_0, loc_0, rt_add_i32(rt_load_i32(memory_at_0, loc_0), rt_shl_i32(loc_1, 2)))
		break
	end
end
FUNC_LIST[216] = --[[ emscripten_get_heap_size ]] function()
	local reg_0
	while true do
		reg_0 = rt_shl_i32(rt_allocator_size(memory_at_0), 16)
		break
	end
	return reg_0
end
FUNC_LIST[217] = --[[ __errno_location ]] function()
	local reg_0
	while true do
		reg_0 = 10520
		break
	end
	return reg_0
end
FUNC_LIST[218] = --[[ sbrk ]] function(loc_0)
	local loc_1 = 0
	local loc_2 = 0
	local reg_0
	local desired
	while true do
		loc_1 = rt_load_i32(memory_at_0, 10488)
		loc_2 = bit_and(rt_add_i32(loc_0, 7), 4294967288)
		loc_0 = rt_add_i32(loc_1, loc_2)
		while true do
			if (if loc_0 <= loc_1 then loc_2 else 0) == 0 then
				while true do
					reg_0 = FUNC_LIST[216]()
					if reg_0 >= loc_0 then
						desired = 1
						break
					end
					reg_0 = FUNC_LIST[3](loc_0)
					if reg_0 ~= 0 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			reg_0 = FUNC_LIST[217]()
			rt_store_i32(memory_at_0, reg_0, 48)
			reg_0 = 4294967295
			desired = 0
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		rt_store_i32(memory_at_0, 10488, loc_0)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[219] = --[[ __memset ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = rt_i64_ZERO
	local loc_6 = 0
	local reg_0
	while true do
		while true do
			if loc_2 == 0 then
				break
			end
			rt_store_i32_n8(memory_at_0, loc_0, loc_1)
			loc_3 = rt_add_i32(loc_0, loc_2)
			rt_store_i32_n8(memory_at_0, rt_sub_i32(loc_3, 1), loc_1)
			if loc_2 < 3 then
				break
			end
			rt_store_i32_n8(memory_at_0, loc_0 + 2, loc_1)
			rt_store_i32_n8(memory_at_0, loc_0 + 1, loc_1)
			rt_store_i32_n8(memory_at_0, rt_sub_i32(loc_3, 3), loc_1)
			rt_store_i32_n8(memory_at_0, rt_sub_i32(loc_3, 2), loc_1)
			if loc_2 < 7 then
				break
			end
			rt_store_i32_n8(memory_at_0, loc_0 + 3, loc_1)
			rt_store_i32_n8(memory_at_0, rt_sub_i32(loc_3, 4), loc_1)
			if loc_2 < 9 then
				break
			end
			loc_4 = bit_and(rt_sub_i32(0, loc_0), 3)
			loc_3 = rt_add_i32(loc_0, loc_4)
			loc_1 = rt_mul_i32(bit_and(loc_1, 255), 16843009)
			rt_store_i32(memory_at_0, loc_3, loc_1)
			loc_4 = bit_and(rt_sub_i32(loc_2, loc_4), 4294967292)
			loc_2 = rt_add_i32(loc_3, loc_4)
			rt_store_i32(memory_at_0, rt_sub_i32(loc_2, 4), loc_1)
			if loc_4 < 9 then
				break
			end
			rt_store_i32(memory_at_0, loc_3 + 8, loc_1)
			rt_store_i32(memory_at_0, loc_3 + 4, loc_1)
			rt_store_i32(memory_at_0, rt_sub_i32(loc_2, 8), loc_1)
			rt_store_i32(memory_at_0, rt_sub_i32(loc_2, 12), loc_1)
			if loc_4 < 25 then
				break
			end
			rt_store_i32(memory_at_0, loc_3 + 24, loc_1)
			rt_store_i32(memory_at_0, loc_3 + 20, loc_1)
			rt_store_i32(memory_at_0, loc_3 + 16, loc_1)
			rt_store_i32(memory_at_0, loc_3 + 12, loc_1)
			rt_store_i32(memory_at_0, rt_sub_i32(loc_2, 16), loc_1)
			rt_store_i32(memory_at_0, rt_sub_i32(loc_2, 20), loc_1)
			rt_store_i32(memory_at_0, rt_sub_i32(loc_2, 24), loc_1)
			rt_store_i32(memory_at_0, rt_sub_i32(loc_2, 28), loc_1)
			loc_6 = bit_or(bit_and(loc_3, 4), 24)
			loc_2 = rt_sub_i32(loc_4, loc_6)
			if loc_2 < 32 then
				break
			end
			loc_5 = rt_mul_i64(rt_extend_i64_u32(loc_1), rt_i64_from_u32(1, 1))
			loc_1 = rt_add_i32(loc_3, loc_6)
			while true do
				rt_store_i64(memory_at_0, loc_1 + 24, loc_5)
				rt_store_i64(memory_at_0, loc_1 + 16, loc_5)
				rt_store_i64(memory_at_0, loc_1 + 8, loc_5)
				rt_store_i64(memory_at_0, loc_1, loc_5)
				loc_1 = rt_add_i32(loc_1, 32)
				loc_2 = rt_sub_i32(loc_2, 32)
				if loc_2 > 31 then
					continue
				end
				break
			end
			break
		end
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[220] = --[[ dlmalloc ]] function(loc_0)
	local loc_1 = 0
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local loc_8 = 0
	local loc_9 = 0
	local loc_10 = 0
	local loc_11 = 0
	local loc_12 = 0
	local loc_13 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_10 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_10
		while true do
			while true do
				while true do
					while true do
						while true do
							while true do
								while true do
									while true do
										while true do
											while true do
												if loc_0 <= 244 then
													while true do
														loc_6 = rt_load_i32(memory_at_0, 10524)
														loc_5 = (if loc_0 < 11 then 16 else bit_and(rt_add_i32(loc_0, 11), 504))
														loc_1 = rt_shr_u32(loc_5, 3)
														loc_0 = rt_shr_u32(loc_6, loc_1)
														if bit_and(loc_0, 3) ~= 0 then
															while true do
																while true do
																	loc_5 = rt_add_i32(bit_and(bit_xor(loc_0, 4294967295), 1), loc_1)
																	loc_1 = rt_shl_i32(loc_5, 3)
																	loc_0 = rt_add_i32(loc_1, 10564)
																	loc_1 = rt_load_i32(memory_at_0, rt_add_i32(loc_1, 10572))
																	loc_2 = rt_load_i32(memory_at_0, loc_1 + 8)
																	if loc_0 == loc_2 then
																		while true do
																			rt_store_i32(memory_at_0, 10524, bit_and(loc_6, rt_rotl_i32(4294967294, loc_5)))
																			desired = 13
																			break
																		end
																		if desired then
																			if desired == 13 then
																				desired = nil
																			end
																			break
																		end
																	end
																	rt_store_i32(memory_at_0, loc_2 + 12, loc_0)
																	rt_store_i32(memory_at_0, loc_0 + 8, loc_2)
																	break
																end
																if desired then
																	break
																end
																loc_0 = rt_add_i32(loc_1, 8)
																loc_5 = rt_shl_i32(loc_5, 3)
																rt_store_i32(memory_at_0, loc_1 + 4, bit_or(loc_5, 3))
																loc_1 = rt_add_i32(loc_1, loc_5)
																rt_store_i32(memory_at_0, loc_1 + 4, bit_or(rt_load_i32(memory_at_0, loc_1 + 4), 1))
																desired = 1
																break
															end
															if desired then
																break
															end
														end
														loc_7 = rt_load_i32(memory_at_0, 10532)
														if loc_5 <= loc_7 then
															desired = 10
															break
														end
														if loc_0 ~= 0 then
															while true do
																while true do
																	reg_0 = rt_shl_i32(loc_0, loc_1)
																	loc_0 = rt_shl_i32(2, loc_1)
																	loc_1 = bit_countrz(bit_and(reg_0, bit_or(loc_0, rt_sub_i32(0, loc_0))))
																	loc_0 = rt_shl_i32(loc_1, 3)
																	loc_2 = rt_add_i32(loc_0, 10564)
																	loc_0 = rt_load_i32(memory_at_0, rt_add_i32(loc_0, 10572))
																	loc_3 = rt_load_i32(memory_at_0, loc_0 + 8)
																	if loc_2 == loc_3 then
																		while true do
																			loc_6 = bit_and(loc_6, rt_rotl_i32(4294967294, loc_1))
																			rt_store_i32(memory_at_0, 10524, loc_6)
																			desired = 13
																			break
																		end
																		if desired then
																			if desired == 13 then
																				desired = nil
																			end
																			break
																		end
																	end
																	rt_store_i32(memory_at_0, loc_3 + 12, loc_2)
																	rt_store_i32(memory_at_0, loc_2 + 8, loc_3)
																	break
																end
																if desired then
																	break
																end
																rt_store_i32(memory_at_0, loc_0 + 4, bit_or(loc_5, 3))
																loc_3 = rt_add_i32(loc_0, loc_5)
																loc_1 = rt_shl_i32(loc_1, 3)
																loc_5 = rt_sub_i32(loc_1, loc_5)
																rt_store_i32(memory_at_0, loc_3 + 4, bit_or(loc_5, 1))
																rt_store_i32(memory_at_0, rt_add_i32(loc_0, loc_1), loc_5)
																if loc_7 ~= 0 then
																	while true do
																		loc_2 = rt_add_i32(bit_and(loc_7, 4294967288), 10564)
																		loc_1 = rt_load_i32(memory_at_0, 10544)
																		while true do
																			loc_4 = rt_shl_i32(1, rt_shr_u32(loc_7, 3))
																			if bit_and(loc_6, loc_4) == 0 then
																				while true do
																					rt_store_i32(memory_at_0, 10524, bit_or(loc_4, loc_6))
																					reg_0 = loc_2
																					desired = 14
																					break
																				end
																				if desired then
																					if desired == 14 then
																						desired = nil
																					end
																					break
																				end
																			end
																			reg_0 = rt_load_i32(memory_at_0, loc_2 + 8)
																			break
																		end
																		if desired then
																			break
																		end
																		loc_4 = reg_0
																		rt_store_i32(memory_at_0, loc_2 + 8, loc_1)
																		rt_store_i32(memory_at_0, loc_4 + 12, loc_1)
																		rt_store_i32(memory_at_0, loc_1 + 12, loc_2)
																		rt_store_i32(memory_at_0, loc_1 + 8, loc_4)
																		break
																	end
																	if desired then
																		break
																	end
																end
																loc_0 = rt_add_i32(loc_0, 8)
																rt_store_i32(memory_at_0, 10544, loc_3)
																rt_store_i32(memory_at_0, 10532, loc_5)
																desired = 1
																break
															end
															if desired then
																break
															end
														end
														loc_11 = rt_load_i32(memory_at_0, 10528)
														if loc_11 == 0 then
															desired = 10
															break
														end
														loc_3 = rt_load_i32(memory_at_0, rt_add_i32(rt_shl_i32(bit_countrz(loc_11), 2), 10828))
														loc_1 = rt_sub_i32(bit_and(rt_load_i32(memory_at_0, loc_3 + 4), 4294967288), loc_5)
														loc_2 = loc_3
														while true do
															while true do
																loc_0 = rt_load_i32(memory_at_0, loc_2 + 16)
																if loc_0 == 0 then
																	while true do
																		loc_0 = rt_load_i32(memory_at_0, loc_2 + 20)
																		if loc_0 == 0 then
																			desired = 13
																			break
																		end
																		break
																	end
																	if desired then
																		if desired == 13 then
																			desired = nil
																		end
																		break
																	end
																end
																loc_2 = rt_sub_i32(bit_and(rt_load_i32(memory_at_0, loc_0 + 4), 4294967288), loc_5)
																reg_0 = loc_2
																loc_2 = (if loc_1 > loc_2 then 1 else 0)
																loc_1 = (if loc_2 ~= 0 then reg_0 else loc_1)
																loc_3 = (if loc_2 ~= 0 then loc_0 else loc_3)
																loc_2 = loc_0
																desired = 12
																break
															end
															if desired then
																if desired == 12 then
																	desired = nil
																	continue
																end
																break
															end
															break
														end
														if desired then
															break
														end
														loc_8 = rt_load_i32(memory_at_0, loc_3 + 24)
														loc_0 = rt_load_i32(memory_at_0, loc_3 + 12)
														if loc_3 ~= loc_0 then
															while true do
																loc_2 = rt_load_i32(memory_at_0, loc_3 + 8)
																rt_store_i32(memory_at_0, loc_2 + 12, loc_0)
																rt_store_i32(memory_at_0, loc_0 + 8, loc_2)
																desired = 2
																break
															end
															if desired then
																break
															end
														end
														loc_2 = rt_load_i32(memory_at_0, loc_3 + 20)
														if loc_2 ~= 0 then
															while true do
																reg_0 = rt_add_i32(loc_3, 20)
																break
															end
															if desired then
																break
															end
														else
															while true do
																loc_2 = rt_load_i32(memory_at_0, loc_3 + 16)
																if loc_2 == 0 then
																	desired = 9
																	break
																end
																reg_0 = rt_add_i32(loc_3, 16)
																break
															end
															if desired then
																break
															end
														end
														loc_4 = reg_0
														while true do
															loc_9 = loc_4
															loc_0 = loc_2
															loc_4 = rt_add_i32(loc_0, 20)
															loc_2 = rt_load_i32(memory_at_0, loc_0 + 20)
															if loc_2 ~= 0 then
																continue
															end
															loc_4 = rt_add_i32(loc_0, 16)
															loc_2 = rt_load_i32(memory_at_0, loc_0 + 16)
															if loc_2 ~= 0 then
																continue
															end
															break
														end
														if desired then
															break
														end
														rt_store_i32(memory_at_0, loc_9, 0)
														desired = 2
														break
													end
													if desired then
														if desired == 10 then
															desired = nil
														end
														break
													end
												end
												loc_5 = 4294967295
												if loc_0 > 4294967231 then
													break
												end
												loc_0 = rt_add_i32(loc_0, 11)
												loc_5 = bit_and(loc_0, 4294967288)
												loc_8 = rt_load_i32(memory_at_0, 10528)
												if loc_8 == 0 then
													break
												end
												loc_1 = rt_sub_i32(0, loc_5)
												while true do
													while true do
														while true do
															while true do
																reg_0 = 0
																if loc_5 < 256 then
																	break
																end
																loc_7 = 31
																reg_0 = loc_7
																if loc_5 > 16777215 then
																	break
																end
																loc_0 = bit_countlz(rt_shr_u32(loc_0, 8))
																reg_0 = rt_add_i32(rt_sub_i32(bit_and(rt_shr_u32(loc_5, rt_sub_i32(38, loc_0)), 1), rt_shl_i32(loc_0, 1)), 62)
																break
															end
															if desired then
																if desired == 13 then
																	desired = nil
																end
																break
															end
															loc_7 = reg_0
															loc_2 = rt_load_i32(memory_at_0, rt_add_i32(rt_shl_i32(loc_7, 2), 10828))
															if loc_2 == 0 then
																while true do
																	loc_0 = 0
																	desired = 13
																	break
																end
																if desired then
																	if desired == 13 then
																		desired = nil
																	end
																	break
																end
															end
															loc_0 = 0
															loc_3 = rt_shl_i32(loc_5, (if loc_7 ~= 31 then rt_sub_i32(25, rt_shr_u32(loc_7, 1)) else 0))
															while true do
																while true do
																	loc_6 = rt_sub_i32(bit_and(rt_load_i32(memory_at_0, loc_2 + 4), 4294967288), loc_5)
																	if loc_6 >= loc_1 then
																		break
																	end
																	loc_4 = loc_2
																	loc_1 = loc_6
																	if loc_1 ~= 0 then
																		break
																	end
																	loc_1 = 0
																	loc_0 = loc_2
																	desired = 12
																	break
																end
																if desired then
																	if desired == 14 then
																		desired = nil
																		continue
																	end
																	break
																end
																loc_6 = rt_load_i32(memory_at_0, loc_2 + 20)
																loc_9 = rt_load_i32(memory_at_0, rt_add_i32(loc_2, bit_and(rt_shr_u32(loc_3, 29), 4)) + 16)
																loc_0 = (if loc_6 ~= 0 then (if loc_6 == loc_9 then loc_0 else loc_6) else loc_0)
																loc_3 = rt_shl_i32(loc_3, 1)
																loc_2 = loc_9
																if loc_2 ~= 0 then
																	continue
																end
																break
															end
															if desired then
																if desired == 13 then
																	desired = nil
																end
																break
															end
															break
														end
														if desired then
															if desired == 12 then
																desired = nil
															end
															break
														end
														if bit_or(loc_0, loc_4) == 0 then
															while true do
																loc_4 = 0
																loc_0 = rt_shl_i32(2, loc_7)
																loc_0 = bit_and(bit_or(loc_0, rt_sub_i32(0, loc_0)), loc_8)
																if loc_0 == 0 then
																	desired = 10
																	break
																end
																loc_0 = rt_load_i32(memory_at_0, rt_add_i32(rt_shl_i32(bit_countrz(loc_0), 2), 10828))
																break
															end
															if desired then
																if desired == 12 then
																	desired = nil
																end
																break
															end
														end
														if loc_0 == 0 then
															desired = 11
															break
														end
														break
													end
													if desired then
														if desired == 11 then
															desired = nil
														end
														break
													end
													while true do
														loc_6 = rt_sub_i32(bit_and(rt_load_i32(memory_at_0, loc_0 + 4), 4294967288), loc_5)
														loc_3 = (if loc_6 < loc_1 then 1 else 0)
														loc_1 = (if loc_3 ~= 0 then loc_6 else loc_1)
														loc_4 = (if loc_3 ~= 0 then loc_0 else loc_4)
														loc_2 = rt_load_i32(memory_at_0, loc_0 + 16)
														if loc_2 == 0 then
															while true do
																loc_2 = rt_load_i32(memory_at_0, loc_0 + 20)
																break
															end
															if desired then
																if desired == 12 then
																	desired = nil
																	continue
																end
																break
															end
														end
														loc_0 = loc_2
														if loc_0 ~= 0 then
															continue
														end
														break
													end
													if desired then
														if desired == 11 then
															desired = nil
														end
														break
													end
													break
												end
												if desired then
													if desired == 10 then
														desired = nil
													end
													break
												end
												if loc_4 == 0 then
													break
												end
												if loc_1 >= rt_sub_i32(rt_load_i32(memory_at_0, 10532), loc_5) then
													break
												end
												loc_9 = rt_load_i32(memory_at_0, loc_4 + 24)
												loc_0 = rt_load_i32(memory_at_0, loc_4 + 12)
												if loc_4 ~= loc_0 then
													while true do
														loc_2 = rt_load_i32(memory_at_0, loc_4 + 8)
														rt_store_i32(memory_at_0, loc_2 + 12, loc_0)
														rt_store_i32(memory_at_0, loc_0 + 8, loc_2)
														desired = 3
														break
													end
													if desired then
														if desired == 10 then
															desired = nil
														end
														break
													end
												end
												loc_2 = rt_load_i32(memory_at_0, loc_4 + 20)
												if loc_2 ~= 0 then
													while true do
														reg_0 = rt_add_i32(loc_4, 20)
														break
													end
													if desired then
														if desired == 10 then
															desired = nil
														end
														break
													end
												else
													while true do
														loc_2 = rt_load_i32(memory_at_0, loc_4 + 16)
														if loc_2 == 0 then
															desired = 8
															break
														end
														reg_0 = rt_add_i32(loc_4, 16)
														break
													end
													if desired then
														if desired == 10 then
															desired = nil
														end
														break
													end
												end
												loc_3 = reg_0
												while true do
													loc_6 = loc_3
													loc_0 = loc_2
													loc_3 = rt_add_i32(loc_0, 20)
													loc_2 = rt_load_i32(memory_at_0, loc_0 + 20)
													if loc_2 ~= 0 then
														continue
													end
													loc_3 = rt_add_i32(loc_0, 16)
													loc_2 = rt_load_i32(memory_at_0, loc_0 + 16)
													if loc_2 ~= 0 then
														continue
													end
													break
												end
												if desired then
													if desired == 10 then
														desired = nil
													end
													break
												end
												rt_store_i32(memory_at_0, loc_6, 0)
												desired = 3
												break
											end
											if desired then
												if desired == 9 then
													desired = nil
												end
												break
											end
											loc_0 = rt_load_i32(memory_at_0, 10532)
											if loc_5 <= loc_0 then
												while true do
													loc_1 = rt_load_i32(memory_at_0, 10544)
													while true do
														loc_2 = rt_sub_i32(loc_0, loc_5)
														if loc_2 >= 16 then
															while true do
																loc_3 = rt_add_i32(loc_1, loc_5)
																rt_store_i32(memory_at_0, loc_3 + 4, bit_or(loc_2, 1))
																rt_store_i32(memory_at_0, rt_add_i32(loc_0, loc_1), loc_2)
																rt_store_i32(memory_at_0, loc_1 + 4, bit_or(loc_5, 3))
																desired = 11
																break
															end
															if desired then
																if desired == 11 then
																	desired = nil
																end
																break
															end
														end
														rt_store_i32(memory_at_0, loc_1 + 4, bit_or(loc_0, 3))
														loc_0 = rt_add_i32(loc_0, loc_1)
														rt_store_i32(memory_at_0, loc_0 + 4, bit_or(rt_load_i32(memory_at_0, loc_0 + 4), 1))
														loc_3 = 0
														loc_2 = 0
														break
													end
													if desired then
														break
													end
													rt_store_i32(memory_at_0, 10532, loc_2)
													rt_store_i32(memory_at_0, 10544, loc_3)
													loc_0 = rt_add_i32(loc_1, 8)
													desired = 1
													break
												end
												if desired then
													if desired == 9 then
														desired = nil
													end
													break
												end
											end
											loc_3 = rt_load_i32(memory_at_0, 10536)
											if loc_5 < loc_3 then
												while true do
													loc_1 = rt_sub_i32(loc_3, loc_5)
													rt_store_i32(memory_at_0, 10536, loc_1)
													loc_0 = rt_load_i32(memory_at_0, 10548)
													loc_2 = rt_add_i32(loc_0, loc_5)
													rt_store_i32(memory_at_0, 10548, loc_2)
													rt_store_i32(memory_at_0, loc_2 + 4, bit_or(loc_1, 1))
													rt_store_i32(memory_at_0, loc_0 + 4, bit_or(loc_5, 3))
													loc_0 = rt_add_i32(loc_0, 8)
													desired = 1
													break
												end
												if desired then
													if desired == 9 then
														desired = nil
													end
													break
												end
											end
											loc_0 = 0
											loc_7 = rt_add_i32(loc_5, 47)
											reg_0 = loc_7
											while true do
												if rt_load_i32(memory_at_0, 10996) ~= 0 then
													while true do
														reg_1 = rt_load_i32(memory_at_0, 11004)
														desired = 10
														break
													end
													if desired then
														if desired == 10 then
															desired = nil
														end
														break
													end
												end
												rt_store_i64(memory_at_0, 11008, rt_i64_from_u32(4294967295, 4294967295))
												rt_store_i64(memory_at_0, 11000, rt_i64_from_u32(4096, 4096))
												rt_store_i32(memory_at_0, 10996, bit_xor(bit_and(rt_add_i32(loc_10, 12), 4294967280), 1431655768))
												rt_store_i32(memory_at_0, 11016, 0)
												rt_store_i32(memory_at_0, 10968, 0)
												reg_1 = 4096
												break
											end
											if desired then
												if desired == 9 then
													desired = nil
												end
												break
											end
											loc_1 = reg_1
											loc_6 = rt_add_i32(reg_0, loc_1)
											loc_9 = rt_sub_i32(0, loc_1)
											loc_4 = bit_and(loc_6, loc_9)
											if loc_4 <= loc_5 then
												desired = 1
												break
											end
											loc_1 = rt_load_i32(memory_at_0, 10964)
											if loc_1 ~= 0 then
												while true do
													loc_2 = rt_load_i32(memory_at_0, 10956)
													loc_8 = rt_add_i32(loc_2, loc_4)
													if loc_8 <= loc_2 then
														desired = 1
														break
													end
													if loc_1 < loc_8 then
														desired = 1
														break
													end
													break
												end
												if desired then
													if desired == 9 then
														desired = nil
													end
													break
												end
											end
											while true do
												if bit_and(rt_load_i32_u8(memory_at_0, 10968), 4) == 0 then
													while true do
														while true do
															while true do
																while true do
																	while true do
																		loc_1 = rt_load_i32(memory_at_0, 10548)
																		if loc_1 ~= 0 then
																			while true do
																				loc_0 = 10972
																				while true do
																					loc_2 = rt_load_i32(memory_at_0, loc_0)
																					if loc_1 >= loc_2 then
																						while true do
																							if rt_add_i32(loc_2, rt_load_i32(memory_at_0, loc_0 + 4)) > loc_1 then
																								desired = 15
																								break
																							end
																							break
																						end
																						if desired then
																							if desired == 17 then
																								desired = nil
																								continue
																							end
																							break
																						end
																					end
																					loc_0 = rt_load_i32(memory_at_0, loc_0 + 8)
																					if loc_0 ~= 0 then
																						continue
																					end
																					break
																				end
																				if desired then
																					break
																				end
																				break
																			end
																			if desired then
																				if desired == 15 then
																					desired = nil
																				end
																				break
																			end
																		end
																		reg_0 = FUNC_LIST[218](0)
																		loc_3 = reg_0
																		if loc_3 == 4294967295 then
																			desired = 12
																			break
																		end
																		loc_6 = loc_4
																		loc_0 = rt_load_i32(memory_at_0, 11000)
																		loc_1 = rt_sub_i32(loc_0, 1)
																		if bit_and(loc_1, loc_3) ~= 0 then
																			while true do
																				loc_6 = rt_add_i32(rt_sub_i32(loc_4, loc_3), bit_and(rt_add_i32(loc_1, loc_3), rt_sub_i32(0, loc_0)))
																				break
																			end
																			if desired then
																				if desired == 15 then
																					desired = nil
																				end
																				break
																			end
																		end
																		if loc_5 >= loc_6 then
																			desired = 12
																			break
																		end
																		loc_0 = rt_load_i32(memory_at_0, 10964)
																		if loc_0 ~= 0 then
																			while true do
																				loc_1 = rt_load_i32(memory_at_0, 10956)
																				loc_2 = rt_add_i32(loc_1, loc_6)
																				if loc_2 <= loc_1 then
																					desired = 12
																					break
																				end
																				if loc_0 < loc_2 then
																					desired = 12
																					break
																				end
																				break
																			end
																			if desired then
																				if desired == 15 then
																					desired = nil
																				end
																				break
																			end
																		end
																		reg_0 = FUNC_LIST[218](loc_6)
																		loc_0 = reg_0
																		if loc_0 ~= loc_3 then
																			desired = 14
																			break
																		end
																		desired = 10
																		break
																	end
																	if desired then
																		if desired == 14 then
																			desired = nil
																		end
																		break
																	end
																	loc_6 = bit_and(rt_sub_i32(loc_6, loc_3), loc_9)
																	reg_0 = FUNC_LIST[218](loc_6)
																	loc_3 = reg_0
																	if loc_3 == rt_add_i32(rt_load_i32(memory_at_0, loc_0), rt_load_i32(memory_at_0, loc_0 + 4)) then
																		desired = 13
																		break
																	end
																	loc_0 = loc_3
																	break
																end
																if desired then
																	if desired == 13 then
																		desired = nil
																	end
																	break
																end
																if loc_0 == 4294967295 then
																	desired = 12
																	break
																end
																if rt_add_i32(loc_5, 48) <= loc_6 then
																	while true do
																		loc_3 = loc_0
																		desired = 10
																		break
																	end
																	if desired then
																		if desired == 13 then
																			desired = nil
																		end
																		break
																	end
																end
																loc_1 = rt_load_i32(memory_at_0, 11004)
																loc_1 = bit_and(rt_add_i32(loc_1, rt_sub_i32(loc_7, loc_6)), rt_sub_i32(0, loc_1))
																reg_0 = FUNC_LIST[218](loc_1)
																if reg_0 == 4294967295 then
																	desired = 12
																	break
																end
																loc_6 = rt_add_i32(loc_1, loc_6)
																loc_3 = loc_0
																desired = 10
																break
															end
															if desired then
																if desired == 12 then
																	desired = nil
																end
																break
															end
															if loc_3 ~= 4294967295 then
																desired = 10
																break
															end
															break
														end
														if desired then
															break
														end
														rt_store_i32(memory_at_0, 10968, bit_or(rt_load_i32(memory_at_0, 10968), 4))
														break
													end
													if desired then
														if desired == 10 then
															desired = nil
														end
														break
													end
												end
												reg_0 = FUNC_LIST[218](loc_4)
												loc_3 = reg_0
												reg_0 = FUNC_LIST[218](0)
												loc_0 = reg_0
												if loc_3 == 4294967295 then
													desired = 5
													break
												end
												if loc_0 == 4294967295 then
													desired = 5
													break
												end
												if loc_0 <= loc_3 then
													desired = 5
													break
												end
												loc_6 = rt_sub_i32(loc_0, loc_3)
												if loc_6 <= rt_add_i32(loc_5, 40) then
													desired = 5
													break
												end
												break
											end
											if desired then
												if desired == 9 then
													desired = nil
												end
												break
											end
											loc_0 = rt_add_i32(rt_load_i32(memory_at_0, 10956), loc_6)
											rt_store_i32(memory_at_0, 10956, loc_0)
											if rt_load_i32(memory_at_0, 10960) < loc_0 then
												while true do
													rt_store_i32(memory_at_0, 10960, loc_0)
													break
												end
												if desired then
													if desired == 9 then
														desired = nil
													end
													break
												end
											end
											while true do
												loc_1 = rt_load_i32(memory_at_0, 10548)
												if loc_1 ~= 0 then
													while true do
														loc_0 = 10972
														while true do
															loc_2 = rt_load_i32(memory_at_0, loc_0)
															loc_4 = rt_load_i32(memory_at_0, loc_0 + 4)
															if loc_3 == rt_add_i32(loc_2, loc_4) then
																desired = 10
																break
															end
															loc_0 = rt_load_i32(memory_at_0, loc_0 + 8)
															if loc_0 ~= 0 then
																continue
															end
															break
														end
														if desired then
															break
														end
														desired = 7
														break
													end
													if desired then
														if desired == 10 then
															desired = nil
														end
														break
													end
												end
												loc_0 = rt_load_i32(memory_at_0, 10540)
												if (if loc_0 <= loc_3 then loc_0 else 0) == 0 then
													while true do
														rt_store_i32(memory_at_0, 10540, loc_3)
														break
													end
													if desired then
														if desired == 10 then
															desired = nil
														end
														break
													end
												end
												loc_0 = 0
												rt_store_i32(memory_at_0, 10976, loc_6)
												rt_store_i32(memory_at_0, 10972, loc_3)
												rt_store_i32(memory_at_0, 10556, 4294967295)
												rt_store_i32(memory_at_0, 10560, rt_load_i32(memory_at_0, 10996))
												rt_store_i32(memory_at_0, 10984, 0)
												while true do
													loc_1 = rt_shl_i32(loc_0, 3)
													loc_2 = rt_add_i32(loc_1, 10564)
													rt_store_i32(memory_at_0, rt_add_i32(loc_1, 10572), loc_2)
													rt_store_i32(memory_at_0, rt_add_i32(loc_1, 10576), loc_2)
													loc_0 = rt_add_i32(loc_0, 1)
													if loc_0 ~= 32 then
														continue
													end
													break
												end
												if desired then
													if desired == 10 then
														desired = nil
													end
													break
												end
												loc_0 = rt_sub_i32(loc_6, 40)
												loc_1 = bit_and(rt_sub_i32(4294967288, loc_3), 7)
												loc_2 = rt_sub_i32(loc_0, loc_1)
												rt_store_i32(memory_at_0, 10536, loc_2)
												loc_1 = rt_add_i32(loc_1, loc_3)
												rt_store_i32(memory_at_0, 10548, loc_1)
												rt_store_i32(memory_at_0, loc_1 + 4, bit_or(loc_2, 1))
												rt_store_i32(memory_at_0, rt_add_i32(loc_0, loc_3) + 4, 40)
												rt_store_i32(memory_at_0, 10552, rt_load_i32(memory_at_0, 11012))
												desired = 6
												break
											end
											if desired then
												if desired == 9 then
													desired = nil
												end
												break
											end
											if loc_1 >= loc_3 then
												desired = 7
												break
											end
											if loc_1 < loc_2 then
												desired = 7
												break
											end
											if bit_and(rt_load_i32(memory_at_0, loc_0 + 12), 8) ~= 0 then
												desired = 7
												break
											end
											rt_store_i32(memory_at_0, loc_0 + 4, rt_add_i32(loc_4, loc_6))
											loc_0 = bit_and(rt_sub_i32(4294967288, loc_1), 7)
											loc_2 = rt_add_i32(loc_1, loc_0)
											rt_store_i32(memory_at_0, 10548, loc_2)
											loc_3 = rt_add_i32(rt_load_i32(memory_at_0, 10536), loc_6)
											loc_0 = rt_sub_i32(loc_3, loc_0)
											rt_store_i32(memory_at_0, 10536, loc_0)
											rt_store_i32(memory_at_0, loc_2 + 4, bit_or(loc_0, 1))
											rt_store_i32(memory_at_0, rt_add_i32(loc_1, loc_3) + 4, 40)
											rt_store_i32(memory_at_0, 10552, rt_load_i32(memory_at_0, 11012))
											desired = 6
											break
										end
										if desired then
											if desired == 8 then
												desired = nil
											end
											break
										end
										loc_0 = 0
										desired = 2
										break
									end
									if desired then
										if desired == 7 then
											desired = nil
										end
										break
									end
									loc_0 = 0
									desired = 3
									break
								end
								if desired then
									if desired == 6 then
										desired = nil
									end
									break
								end
								if rt_load_i32(memory_at_0, 10540) > loc_3 then
									while true do
										rt_store_i32(memory_at_0, 10540, loc_3)
										break
									end
									if desired then
										if desired == 6 then
											desired = nil
										end
										break
									end
								end
								loc_2 = rt_add_i32(loc_3, loc_6)
								loc_0 = 10972
								while true do
									while true do
										if loc_2 ~= rt_load_i32(memory_at_0, loc_0) then
											while true do
												loc_0 = rt_load_i32(memory_at_0, loc_0 + 8)
												if loc_0 ~= 0 then
													desired = 8
													break
												end
												desired = 7
												break
											end
											if desired then
												if desired == 8 then
													desired = nil
													continue
												end
												break
											end
										end
										break
									end
									if desired then
										if desired == 7 then
											desired = nil
										end
										break
									end
									if bit_and(rt_load_i32_u8(memory_at_0, loc_0 + 12), 8) == 0 then
										desired = 4
										break
									end
									break
								end
								if desired then
									if desired == 6 then
										desired = nil
									end
									break
								end
								loc_0 = 10972
								while true do
									while true do
										loc_2 = rt_load_i32(memory_at_0, loc_0)
										if loc_1 >= loc_2 then
											while true do
												loc_2 = rt_add_i32(loc_2, rt_load_i32(memory_at_0, loc_0 + 4))
												if loc_2 > loc_1 then
													desired = 8
													break
												end
												break
											end
											if desired then
												if desired == 8 then
													desired = nil
												end
												break
											end
										end
										loc_0 = rt_load_i32(memory_at_0, loc_0 + 8)
										desired = 7
										break
									end
									if desired then
										if desired == 7 then
											desired = nil
											continue
										end
										break
									end
									break
								end
								if desired then
									if desired == 6 then
										desired = nil
									end
									break
								end
								loc_0 = rt_sub_i32(loc_6, 40)
								loc_4 = bit_and(rt_sub_i32(4294967288, loc_3), 7)
								loc_9 = rt_sub_i32(loc_0, loc_4)
								rt_store_i32(memory_at_0, 10536, loc_9)
								loc_4 = rt_add_i32(loc_3, loc_4)
								rt_store_i32(memory_at_0, 10548, loc_4)
								rt_store_i32(memory_at_0, loc_4 + 4, bit_or(loc_9, 1))
								rt_store_i32(memory_at_0, rt_add_i32(loc_0, loc_3) + 4, 40)
								rt_store_i32(memory_at_0, 10552, rt_load_i32(memory_at_0, 11012))
								loc_0 = rt_sub_i32(rt_add_i32(loc_2, bit_and(rt_sub_i32(39, loc_2), 7)), 47)
								loc_4 = (if loc_0 < rt_add_i32(loc_1, 16) then loc_1 else loc_0)
								rt_store_i32(memory_at_0, loc_4 + 4, 27)
								rt_store_i64(memory_at_0, loc_4 + 16, rt_load_i64(memory_at_0, 10980))
								rt_store_i64(memory_at_0, loc_4 + 8, rt_load_i64(memory_at_0, 10972))
								rt_store_i32(memory_at_0, 10980, rt_add_i32(loc_4, 8))
								rt_store_i32(memory_at_0, 10976, loc_6)
								rt_store_i32(memory_at_0, 10972, loc_3)
								rt_store_i32(memory_at_0, 10984, 0)
								loc_0 = rt_add_i32(loc_4, 24)
								while true do
									rt_store_i32(memory_at_0, loc_0 + 4, 7)
									loc_3 = rt_add_i32(loc_0, 8)
									loc_0 = rt_add_i32(loc_0, 4)
									if loc_2 > loc_3 then
										continue
									end
									break
								end
								if desired then
									if desired == 6 then
										desired = nil
									end
									break
								end
								if loc_1 == loc_4 then
									break
								end
								rt_store_i32(memory_at_0, loc_4 + 4, bit_and(rt_load_i32(memory_at_0, loc_4 + 4), 4294967294))
								loc_3 = rt_sub_i32(loc_4, loc_1)
								rt_store_i32(memory_at_0, loc_1 + 4, bit_or(loc_3, 1))
								rt_store_i32(memory_at_0, loc_4, loc_3)
								while true do
									if loc_3 <= 255 then
										while true do
											loc_0 = rt_add_i32(bit_and(loc_3, 4294967288), 10564)
											while true do
												loc_2 = rt_load_i32(memory_at_0, 10524)
												loc_3 = rt_shl_i32(1, rt_shr_u32(loc_3, 3))
												if bit_and(loc_2, loc_3) == 0 then
													while true do
														rt_store_i32(memory_at_0, 10524, bit_or(loc_2, loc_3))
														reg_0 = loc_0
														desired = 9
														break
													end
													if desired then
														if desired == 9 then
															desired = nil
														end
														break
													end
												end
												reg_0 = rt_load_i32(memory_at_0, loc_0 + 8)
												break
											end
											if desired then
												break
											end
											loc_2 = reg_0
											rt_store_i32(memory_at_0, loc_0 + 8, loc_1)
											rt_store_i32(memory_at_0, loc_2 + 12, loc_1)
											loc_4 = 8
											reg_0 = 12
											desired = 7
											break
										end
										if desired then
											if desired == 7 then
												desired = nil
											end
											break
										end
									end
									loc_0 = 31
									if loc_3 <= 16777215 then
										while true do
											loc_0 = bit_countlz(rt_shr_u32(loc_3, 8))
											loc_0 = rt_add_i32(rt_sub_i32(bit_and(rt_shr_u32(loc_3, rt_sub_i32(38, loc_0)), 1), rt_shl_i32(loc_0, 1)), 62)
											break
										end
										if desired then
											if desired == 7 then
												desired = nil
											end
											break
										end
									end
									rt_store_i32(memory_at_0, loc_1 + 28, loc_0)
									rt_store_i64(memory_at_0, loc_1 + 16, rt_i64_ZERO)
									loc_2 = rt_add_i32(rt_shl_i32(loc_0, 2), 10828)
									while true do
										while true do
											loc_4 = rt_load_i32(memory_at_0, 10528)
											loc_6 = rt_shl_i32(1, loc_0)
											if bit_and(loc_4, loc_6) == 0 then
												while true do
													rt_store_i32(memory_at_0, 10528, bit_or(loc_4, loc_6))
													rt_store_i32(memory_at_0, loc_2, loc_1)
													desired = 9
													break
												end
												if desired then
													if desired == 9 then
														desired = nil
													end
													break
												end
											end
											loc_0 = rt_shl_i32(loc_3, (if loc_0 ~= 31 then rt_sub_i32(25, rt_shr_u32(loc_0, 1)) else 0))
											loc_4 = rt_load_i32(memory_at_0, loc_2)
											while true do
												loc_2 = loc_4
												if bit_and(rt_load_i32(memory_at_0, loc_2 + 4), 4294967288) == loc_3 then
													desired = 8
													break
												end
												loc_4 = rt_shr_u32(loc_0, 29)
												loc_0 = rt_shl_i32(loc_0, 1)
												loc_12 = rt_add_i32(loc_2, bit_and(loc_4, 4))
												loc_6 = rt_add_i32(loc_12, 16)
												loc_4 = rt_load_i32(memory_at_0, loc_6)
												if loc_4 ~= 0 then
													continue
												end
												break
											end
											if desired then
												if desired == 9 then
													desired = nil
												end
												break
											end
											rt_store_i32(memory_at_0, loc_12 + 16, loc_1)
											break
										end
										if desired then
											if desired == 8 then
												desired = nil
											end
											break
										end
										rt_store_i32(memory_at_0, loc_1 + 24, loc_2)
										loc_4 = 12
										loc_2 = loc_1
										loc_0 = loc_1
										reg_0 = 8
										desired = 7
										break
									end
									if desired then
										if desired == 7 then
											desired = nil
										end
										break
									end
									loc_0 = rt_load_i32(memory_at_0, loc_2 + 8)
									rt_store_i32(memory_at_0, loc_0 + 12, loc_1)
									rt_store_i32(memory_at_0, loc_2 + 8, loc_1)
									rt_store_i32(memory_at_0, loc_1 + 8, loc_0)
									loc_0 = 0
									loc_4 = 12
									reg_0 = 24
									break
								end
								if desired then
									if desired == 6 then
										desired = nil
									end
									break
								end
								loc_3 = reg_0
								rt_store_i32(memory_at_0, rt_add_i32(loc_1, loc_4), loc_2)
								rt_store_i32(memory_at_0, rt_add_i32(loc_1, loc_3), loc_0)
								break
							end
							if desired then
								if desired == 5 then
									desired = nil
								end
								break
							end
							loc_0 = rt_load_i32(memory_at_0, 10536)
							if loc_0 <= loc_5 then
								break
							end
							loc_1 = rt_sub_i32(loc_0, loc_5)
							rt_store_i32(memory_at_0, 10536, loc_1)
							loc_0 = rt_load_i32(memory_at_0, 10548)
							loc_2 = rt_add_i32(loc_0, loc_5)
							rt_store_i32(memory_at_0, 10548, loc_2)
							rt_store_i32(memory_at_0, loc_2 + 4, bit_or(loc_1, 1))
							rt_store_i32(memory_at_0, loc_0 + 4, bit_or(loc_5, 3))
							loc_0 = rt_add_i32(loc_0, 8)
							desired = 1
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						reg_0 = FUNC_LIST[217]()
						rt_store_i32(memory_at_0, reg_0, 48)
						loc_0 = 0
						desired = 1
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					rt_store_i32(memory_at_0, loc_0, loc_3)
					rt_store_i32(memory_at_0, loc_0 + 4, rt_add_i32(rt_load_i32(memory_at_0, loc_0 + 4), loc_6))
					reg_0 = FUNC_LIST[221](loc_3, loc_2, loc_5)
					loc_0 = reg_0
					desired = 1
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				while true do
					if loc_9 == 0 then
						break
					end
					while true do
						loc_3 = rt_load_i32(memory_at_0, loc_4 + 28)
						loc_2 = rt_add_i32(rt_shl_i32(loc_3, 2), 10828)
						if rt_load_i32(memory_at_0, loc_2) == loc_4 then
							while true do
								rt_store_i32(memory_at_0, loc_2, loc_0)
								if loc_0 ~= 0 then
									desired = 4
									break
								end
								loc_8 = bit_and(loc_8, rt_rotl_i32(4294967294, loc_3))
								rt_store_i32(memory_at_0, 10528, loc_8)
								desired = 3
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						rt_store_i32(memory_at_0, rt_add_i32(loc_9, (if rt_load_i32(memory_at_0, loc_9 + 16) == loc_4 then 16 else 20)), loc_0)
						if loc_0 == 0 then
							desired = 3
							break
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					rt_store_i32(memory_at_0, loc_0 + 24, loc_9)
					loc_2 = rt_load_i32(memory_at_0, loc_4 + 16)
					if loc_2 ~= 0 then
						while true do
							rt_store_i32(memory_at_0, loc_0 + 16, loc_2)
							rt_store_i32(memory_at_0, loc_2 + 24, loc_0)
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					loc_2 = rt_load_i32(memory_at_0, loc_4 + 20)
					if loc_2 == 0 then
						break
					end
					rt_store_i32(memory_at_0, loc_0 + 20, loc_2)
					rt_store_i32(memory_at_0, loc_2 + 24, loc_0)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				while true do
					if loc_1 <= 15 then
						while true do
							loc_0 = rt_add_i32(loc_1, loc_5)
							rt_store_i32(memory_at_0, loc_4 + 4, bit_or(loc_0, 3))
							loc_0 = rt_add_i32(loc_0, loc_4)
							rt_store_i32(memory_at_0, loc_0 + 4, bit_or(rt_load_i32(memory_at_0, loc_0 + 4), 1))
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					rt_store_i32(memory_at_0, loc_4 + 4, bit_or(loc_5, 3))
					loc_3 = rt_add_i32(loc_4, loc_5)
					rt_store_i32(memory_at_0, loc_3 + 4, bit_or(loc_1, 1))
					rt_store_i32(memory_at_0, rt_add_i32(loc_1, loc_3), loc_1)
					if loc_1 <= 255 then
						while true do
							loc_0 = rt_add_i32(bit_and(loc_1, 4294967288), 10564)
							while true do
								loc_5 = rt_load_i32(memory_at_0, 10524)
								loc_1 = rt_shl_i32(1, rt_shr_u32(loc_1, 3))
								if bit_and(loc_5, loc_1) == 0 then
									while true do
										rt_store_i32(memory_at_0, 10524, bit_or(loc_1, loc_5))
										reg_0 = loc_0
										desired = 5
										break
									end
									if desired then
										if desired == 5 then
											desired = nil
										end
										break
									end
								end
								reg_0 = rt_load_i32(memory_at_0, loc_0 + 8)
								break
							end
							if desired then
								break
							end
							loc_1 = reg_0
							rt_store_i32(memory_at_0, loc_0 + 8, loc_3)
							rt_store_i32(memory_at_0, loc_1 + 12, loc_3)
							rt_store_i32(memory_at_0, loc_3 + 12, loc_0)
							rt_store_i32(memory_at_0, loc_3 + 8, loc_1)
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					loc_0 = 31
					if loc_1 <= 16777215 then
						while true do
							loc_0 = bit_countlz(rt_shr_u32(loc_1, 8))
							loc_0 = rt_add_i32(rt_sub_i32(bit_and(rt_shr_u32(loc_1, rt_sub_i32(38, loc_0)), 1), rt_shl_i32(loc_0, 1)), 62)
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					rt_store_i32(memory_at_0, loc_3 + 28, loc_0)
					rt_store_i64(memory_at_0, loc_3 + 16, rt_i64_ZERO)
					loc_5 = rt_add_i32(rt_shl_i32(loc_0, 2), 10828)
					while true do
						while true do
							loc_2 = rt_shl_i32(1, loc_0)
							if bit_and(loc_8, loc_2) == 0 then
								while true do
									rt_store_i32(memory_at_0, 10528, bit_or(loc_2, loc_8))
									rt_store_i32(memory_at_0, loc_5, loc_3)
									desired = 5
									break
								end
								if desired then
									if desired == 5 then
										desired = nil
									end
									break
								end
							end
							loc_0 = rt_shl_i32(loc_1, (if loc_0 ~= 31 then rt_sub_i32(25, rt_shr_u32(loc_0, 1)) else 0))
							loc_2 = rt_load_i32(memory_at_0, loc_5)
							while true do
								loc_5 = loc_2
								if bit_and(rt_load_i32(memory_at_0, loc_5 + 4), 4294967288) == loc_1 then
									desired = 4
									break
								end
								loc_2 = rt_shr_u32(loc_0, 29)
								loc_0 = rt_shl_i32(loc_0, 1)
								loc_13 = rt_add_i32(loc_5, bit_and(loc_2, 4))
								loc_6 = rt_add_i32(loc_13, 16)
								loc_2 = rt_load_i32(memory_at_0, loc_6)
								if loc_2 ~= 0 then
									continue
								end
								break
							end
							if desired then
								if desired == 5 then
									desired = nil
								end
								break
							end
							rt_store_i32(memory_at_0, loc_13 + 16, loc_3)
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						rt_store_i32(memory_at_0, loc_3 + 24, loc_5)
						rt_store_i32(memory_at_0, loc_3 + 12, loc_3)
						rt_store_i32(memory_at_0, loc_3 + 8, loc_3)
						desired = 3
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					loc_0 = rt_load_i32(memory_at_0, loc_5 + 8)
					rt_store_i32(memory_at_0, loc_0 + 12, loc_3)
					rt_store_i32(memory_at_0, loc_5 + 8, loc_3)
					rt_store_i32(memory_at_0, loc_3 + 24, 0)
					rt_store_i32(memory_at_0, loc_3 + 12, loc_5)
					rt_store_i32(memory_at_0, loc_3 + 8, loc_0)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_0 = rt_add_i32(loc_4, 8)
				desired = 1
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			while true do
				if loc_8 == 0 then
					break
				end
				while true do
					loc_4 = rt_load_i32(memory_at_0, loc_3 + 28)
					loc_2 = rt_add_i32(rt_shl_i32(loc_4, 2), 10828)
					if rt_load_i32(memory_at_0, loc_2) == loc_3 then
						while true do
							rt_store_i32(memory_at_0, loc_2, loc_0)
							if loc_0 ~= 0 then
								desired = 3
								break
							end
							rt_store_i32(memory_at_0, 10528, bit_and(loc_11, rt_rotl_i32(4294967294, loc_4)))
							desired = 2
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					rt_store_i32(memory_at_0, rt_add_i32(loc_8, (if rt_load_i32(memory_at_0, loc_8 + 16) == loc_3 then 16 else 20)), loc_0)
					if loc_0 == 0 then
						desired = 2
						break
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				rt_store_i32(memory_at_0, loc_0 + 24, loc_8)
				loc_2 = rt_load_i32(memory_at_0, loc_3 + 16)
				if loc_2 ~= 0 then
					while true do
						rt_store_i32(memory_at_0, loc_0 + 16, loc_2)
						rt_store_i32(memory_at_0, loc_2 + 24, loc_0)
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				loc_2 = rt_load_i32(memory_at_0, loc_3 + 20)
				if loc_2 == 0 then
					break
				end
				rt_store_i32(memory_at_0, loc_0 + 20, loc_2)
				rt_store_i32(memory_at_0, loc_2 + 24, loc_0)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			while true do
				if loc_1 <= 15 then
					while true do
						loc_0 = rt_add_i32(loc_1, loc_5)
						rt_store_i32(memory_at_0, loc_3 + 4, bit_or(loc_0, 3))
						loc_0 = rt_add_i32(loc_0, loc_3)
						rt_store_i32(memory_at_0, loc_0 + 4, bit_or(rt_load_i32(memory_at_0, loc_0 + 4), 1))
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				rt_store_i32(memory_at_0, loc_3 + 4, bit_or(loc_5, 3))
				loc_5 = rt_add_i32(loc_3, loc_5)
				rt_store_i32(memory_at_0, loc_5 + 4, bit_or(loc_1, 1))
				rt_store_i32(memory_at_0, rt_add_i32(loc_1, loc_5), loc_1)
				if loc_7 ~= 0 then
					while true do
						loc_2 = rt_add_i32(bit_and(loc_7, 4294967288), 10564)
						loc_0 = rt_load_i32(memory_at_0, 10544)
						while true do
							loc_4 = rt_shl_i32(1, rt_shr_u32(loc_7, 3))
							if bit_and(loc_4, loc_6) == 0 then
								while true do
									rt_store_i32(memory_at_0, 10524, bit_or(loc_4, loc_6))
									reg_0 = loc_2
									desired = 4
									break
								end
								if desired then
									if desired == 4 then
										desired = nil
									end
									break
								end
							end
							reg_0 = rt_load_i32(memory_at_0, loc_2 + 8)
							break
						end
						if desired then
							break
						end
						loc_4 = reg_0
						rt_store_i32(memory_at_0, loc_2 + 8, loc_0)
						rt_store_i32(memory_at_0, loc_4 + 12, loc_0)
						rt_store_i32(memory_at_0, loc_0 + 12, loc_2)
						rt_store_i32(memory_at_0, loc_0 + 8, loc_4)
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				rt_store_i32(memory_at_0, 10544, loc_5)
				rt_store_i32(memory_at_0, 10532, loc_1)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_0 = rt_add_i32(loc_3, 8)
			break
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_10, 16)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[221] = --[[ prepend_alloc ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local loc_8 = 0
	local loc_9 = 0
	local loc_10 = 0
	local reg_0
	local desired
	while true do
		loc_8 = rt_add_i32(loc_0, bit_and(rt_sub_i32(4294967288, loc_0), 7))
		rt_store_i32(memory_at_0, loc_8 + 4, bit_or(loc_2, 3))
		loc_4 = rt_add_i32(loc_1, bit_and(rt_sub_i32(4294967288, loc_1), 7))
		loc_3 = rt_add_i32(loc_2, loc_8)
		loc_0 = rt_sub_i32(loc_4, loc_3)
		while true do
			if rt_load_i32(memory_at_0, 10548) == loc_4 then
				while true do
					rt_store_i32(memory_at_0, 10548, loc_3)
					loc_2 = rt_add_i32(rt_load_i32(memory_at_0, 10536), loc_0)
					rt_store_i32(memory_at_0, 10536, loc_2)
					rt_store_i32(memory_at_0, loc_3 + 4, bit_or(loc_2, 1))
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			if rt_load_i32(memory_at_0, 10544) == loc_4 then
				while true do
					rt_store_i32(memory_at_0, 10544, loc_3)
					loc_2 = rt_add_i32(rt_load_i32(memory_at_0, 10532), loc_0)
					rt_store_i32(memory_at_0, 10532, loc_2)
					rt_store_i32(memory_at_0, loc_3 + 4, bit_or(loc_2, 1))
					rt_store_i32(memory_at_0, rt_add_i32(loc_2, loc_3), loc_2)
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_1 = rt_load_i32(memory_at_0, loc_4 + 4)
			if bit_and(loc_1, 3) == 1 then
				while true do
					loc_9 = bit_and(loc_1, 4294967288)
					loc_2 = rt_load_i32(memory_at_0, loc_4 + 12)
					while true do
						if loc_1 <= 255 then
							while true do
								loc_7 = rt_shr_u32(loc_1, 3)
								loc_1 = rt_add_i32(rt_shl_i32(loc_7, 3), 10564)
								loc_5 = rt_load_i32(memory_at_0, loc_4 + 8)
								if loc_5 == loc_2 then
									while true do
										rt_store_i32(memory_at_0, 10524, bit_and(rt_load_i32(memory_at_0, 10524), rt_rotl_i32(4294967294, loc_7)))
										desired = 3
										break
									end
									if desired then
										break
									end
								end
								rt_store_i32(memory_at_0, loc_5 + 12, loc_2)
								rt_store_i32(memory_at_0, loc_2 + 8, loc_5)
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_6 = rt_load_i32(memory_at_0, loc_4 + 24)
						while true do
							if loc_2 ~= loc_4 then
								while true do
									loc_1 = rt_load_i32(memory_at_0, loc_4 + 8)
									rt_store_i32(memory_at_0, loc_1 + 12, loc_2)
									rt_store_i32(memory_at_0, loc_2 + 8, loc_1)
									desired = 4
									break
								end
								if desired then
									if desired == 4 then
										desired = nil
									end
									break
								end
							end
							while true do
								loc_1 = rt_load_i32(memory_at_0, loc_4 + 20)
								if loc_1 ~= 0 then
									while true do
										reg_0 = rt_add_i32(loc_4, 20)
										break
									end
									if desired then
										if desired == 5 then
											desired = nil
										end
										break
									end
								else
									while true do
										loc_1 = rt_load_i32(memory_at_0, loc_4 + 16)
										if loc_1 == 0 then
											desired = 5
											break
										end
										reg_0 = rt_add_i32(loc_4, 16)
										break
									end
									if desired then
										if desired == 5 then
											desired = nil
										end
										break
									end
								end
								loc_5 = reg_0
								while true do
									loc_7 = loc_5
									loc_2 = loc_1
									loc_5 = rt_add_i32(loc_2, 20)
									loc_1 = rt_load_i32(memory_at_0, loc_2 + 20)
									if loc_1 ~= 0 then
										continue
									end
									loc_5 = rt_add_i32(loc_2, 16)
									loc_1 = rt_load_i32(memory_at_0, loc_2 + 16)
									if loc_1 ~= 0 then
										continue
									end
									break
								end
								if desired then
									if desired == 5 then
										desired = nil
									end
									break
								end
								rt_store_i32(memory_at_0, loc_7, 0)
								desired = 4
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
							loc_2 = 0
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
						if loc_6 == 0 then
							break
						end
						while true do
							loc_5 = rt_load_i32(memory_at_0, loc_4 + 28)
							loc_1 = rt_add_i32(rt_shl_i32(loc_5, 2), 10828)
							if rt_load_i32(memory_at_0, loc_1) == loc_4 then
								while true do
									rt_store_i32(memory_at_0, loc_1, loc_2)
									if loc_2 ~= 0 then
										desired = 4
										break
									end
									rt_store_i32(memory_at_0, 10528, bit_and(rt_load_i32(memory_at_0, 10528), rt_rotl_i32(4294967294, loc_5)))
									desired = 3
									break
								end
								if desired then
									if desired == 4 then
										desired = nil
									end
									break
								end
							end
							rt_store_i32(memory_at_0, rt_add_i32(loc_6, (if rt_load_i32(memory_at_0, loc_6 + 16) == loc_4 then 16 else 20)), loc_2)
							if loc_2 == 0 then
								desired = 3
								break
							end
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
						rt_store_i32(memory_at_0, loc_2 + 24, loc_6)
						loc_1 = rt_load_i32(memory_at_0, loc_4 + 16)
						if loc_1 ~= 0 then
							while true do
								rt_store_i32(memory_at_0, loc_2 + 16, loc_1)
								rt_store_i32(memory_at_0, loc_1 + 24, loc_2)
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						loc_1 = rt_load_i32(memory_at_0, loc_4 + 20)
						if loc_1 == 0 then
							break
						end
						rt_store_i32(memory_at_0, loc_2 + 20, loc_1)
						rt_store_i32(memory_at_0, loc_1 + 24, loc_2)
						break
					end
					if desired then
						break
					end
					loc_4 = rt_add_i32(loc_4, loc_9)
					loc_1 = rt_load_i32(memory_at_0, loc_4 + 4)
					loc_0 = rt_add_i32(loc_0, loc_9)
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			rt_store_i32(memory_at_0, loc_4 + 4, bit_and(loc_1, 4294967294))
			rt_store_i32(memory_at_0, loc_3 + 4, bit_or(loc_0, 1))
			rt_store_i32(memory_at_0, rt_add_i32(loc_0, loc_3), loc_0)
			if loc_0 <= 255 then
				while true do
					loc_2 = rt_add_i32(bit_and(loc_0, 4294967288), 10564)
					while true do
						loc_1 = rt_load_i32(memory_at_0, 10524)
						loc_0 = rt_shl_i32(1, rt_shr_u32(loc_0, 3))
						if bit_and(loc_1, loc_0) == 0 then
							while true do
								rt_store_i32(memory_at_0, 10524, bit_or(loc_0, loc_1))
								reg_0 = loc_2
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						reg_0 = rt_load_i32(memory_at_0, loc_2 + 8)
						break
					end
					if desired then
						break
					end
					loc_0 = reg_0
					rt_store_i32(memory_at_0, loc_2 + 8, loc_3)
					rt_store_i32(memory_at_0, loc_0 + 12, loc_3)
					rt_store_i32(memory_at_0, loc_3 + 12, loc_2)
					rt_store_i32(memory_at_0, loc_3 + 8, loc_0)
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_2 = 31
			if loc_0 <= 16777215 then
				while true do
					loc_2 = bit_countlz(rt_shr_u32(loc_0, 8))
					loc_2 = rt_add_i32(rt_sub_i32(bit_and(rt_shr_u32(loc_0, rt_sub_i32(38, loc_2)), 1), rt_shl_i32(loc_2, 1)), 62)
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			rt_store_i32(memory_at_0, loc_3 + 28, loc_2)
			rt_store_i64(memory_at_0, loc_3 + 16, rt_i64_ZERO)
			loc_1 = rt_add_i32(rt_shl_i32(loc_2, 2), 10828)
			while true do
				while true do
					loc_5 = rt_load_i32(memory_at_0, 10528)
					loc_4 = rt_shl_i32(1, loc_2)
					if bit_and(loc_5, loc_4) == 0 then
						while true do
							rt_store_i32(memory_at_0, 10528, bit_or(loc_4, loc_5))
							rt_store_i32(memory_at_0, loc_1, loc_3)
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					loc_2 = rt_shl_i32(loc_0, (if loc_2 ~= 31 then rt_sub_i32(25, rt_shr_u32(loc_2, 1)) else 0))
					loc_5 = rt_load_i32(memory_at_0, loc_1)
					while true do
						loc_1 = loc_5
						if bit_and(rt_load_i32(memory_at_0, loc_1 + 4), 4294967288) == loc_0 then
							desired = 2
							break
						end
						loc_5 = rt_shr_u32(loc_2, 29)
						loc_2 = rt_shl_i32(loc_2, 1)
						loc_10 = rt_add_i32(loc_1, bit_and(loc_5, 4))
						loc_4 = rt_add_i32(loc_10, 16)
						loc_5 = rt_load_i32(memory_at_0, loc_4)
						if loc_5 ~= 0 then
							continue
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					rt_store_i32(memory_at_0, loc_10 + 16, loc_3)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				rt_store_i32(memory_at_0, loc_3 + 24, loc_1)
				rt_store_i32(memory_at_0, loc_3 + 12, loc_3)
				rt_store_i32(memory_at_0, loc_3 + 8, loc_3)
				desired = 1
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_2 = rt_load_i32(memory_at_0, loc_1 + 8)
			rt_store_i32(memory_at_0, loc_2 + 12, loc_3)
			rt_store_i32(memory_at_0, loc_1 + 8, loc_3)
			rt_store_i32(memory_at_0, loc_3 + 24, 0)
			rt_store_i32(memory_at_0, loc_3 + 12, loc_1)
			rt_store_i32(memory_at_0, loc_3 + 8, loc_2)
			break
		end
		reg_0 = rt_add_i32(loc_8, 8)
		break
	end
	return reg_0
end
FUNC_LIST[222] = --[[ dlfree ]] function(loc_0)
	local loc_1 = 0
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local reg_0
	local desired
	while true do
		while true do
			if loc_0 == 0 then
				break
			end
			loc_3 = rt_sub_i32(loc_0, 8)
			loc_1 = rt_load_i32(memory_at_0, rt_sub_i32(loc_0, 4))
			loc_0 = bit_and(loc_1, 4294967288)
			loc_4 = rt_add_i32(loc_3, loc_0)
			while true do
				if bit_and(loc_1, 1) ~= 0 then
					break
				end
				if bit_and(loc_1, 2) == 0 then
					desired = 1
					break
				end
				loc_2 = rt_load_i32(memory_at_0, loc_3)
				loc_3 = rt_sub_i32(loc_3, loc_2)
				loc_5 = rt_load_i32(memory_at_0, 10540)
				if loc_3 < loc_5 then
					desired = 1
					break
				end
				loc_0 = rt_add_i32(loc_0, loc_2)
				while true do
					while true do
						if rt_load_i32(memory_at_0, 10544) ~= loc_3 then
							while true do
								loc_1 = rt_load_i32(memory_at_0, loc_3 + 12)
								if loc_2 <= 255 then
									while true do
										loc_7 = rt_shr_u32(loc_2, 3)
										loc_2 = rt_add_i32(rt_shl_i32(loc_7, 3), 10564)
										loc_5 = rt_load_i32(memory_at_0, loc_3 + 8)
										if loc_5 == loc_1 then
											while true do
												rt_store_i32(memory_at_0, 10524, bit_and(rt_load_i32(memory_at_0, 10524), rt_rotl_i32(4294967294, loc_7)))
												desired = 2
												break
											end
											if desired then
												break
											end
										end
										rt_store_i32(memory_at_0, loc_5 + 12, loc_1)
										rt_store_i32(memory_at_0, loc_1 + 8, loc_5)
										desired = 2
										break
									end
									if desired then
										break
									end
								end
								loc_6 = rt_load_i32(memory_at_0, loc_3 + 24)
								if loc_1 ~= loc_3 then
									while true do
										loc_2 = rt_load_i32(memory_at_0, loc_3 + 8)
										rt_store_i32(memory_at_0, loc_2 + 12, loc_1)
										rt_store_i32(memory_at_0, loc_1 + 8, loc_2)
										desired = 3
										break
									end
									if desired then
										break
									end
								end
								loc_2 = rt_load_i32(memory_at_0, loc_3 + 20)
								if loc_2 ~= 0 then
									while true do
										reg_0 = rt_add_i32(loc_3, 20)
										break
									end
									if desired then
										break
									end
								else
									while true do
										loc_2 = rt_load_i32(memory_at_0, loc_3 + 16)
										if loc_2 == 0 then
											desired = 4
											break
										end
										reg_0 = rt_add_i32(loc_3, 16)
										break
									end
									if desired then
										break
									end
								end
								loc_5 = reg_0
								while true do
									loc_7 = loc_5
									loc_1 = loc_2
									loc_5 = rt_add_i32(loc_1, 20)
									loc_2 = rt_load_i32(memory_at_0, loc_1 + 20)
									if loc_2 ~= 0 then
										continue
									end
									loc_5 = rt_add_i32(loc_1, 16)
									loc_2 = rt_load_i32(memory_at_0, loc_1 + 16)
									if loc_2 ~= 0 then
										continue
									end
									break
								end
								if desired then
									break
								end
								rt_store_i32(memory_at_0, loc_7, 0)
								desired = 3
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						loc_1 = rt_load_i32(memory_at_0, loc_4 + 4)
						if bit_and(loc_1, 3) ~= 3 then
							desired = 2
							break
						end
						rt_store_i32(memory_at_0, 10532, loc_0)
						rt_store_i32(memory_at_0, loc_4 + 4, bit_and(loc_1, 4294967294))
						rt_store_i32(memory_at_0, loc_3 + 4, bit_or(loc_0, 1))
						rt_store_i32(memory_at_0, loc_4, loc_0)
						desired = 0
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					loc_1 = 0
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				if loc_6 == 0 then
					break
				end
				while true do
					loc_5 = rt_load_i32(memory_at_0, loc_3 + 28)
					loc_2 = rt_add_i32(rt_shl_i32(loc_5, 2), 10828)
					if rt_load_i32(memory_at_0, loc_2) == loc_3 then
						while true do
							rt_store_i32(memory_at_0, loc_2, loc_1)
							if loc_1 ~= 0 then
								desired = 3
								break
							end
							rt_store_i32(memory_at_0, 10528, bit_and(rt_load_i32(memory_at_0, 10528), rt_rotl_i32(4294967294, loc_5)))
							desired = 2
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					rt_store_i32(memory_at_0, rt_add_i32(loc_6, (if rt_load_i32(memory_at_0, loc_6 + 16) == loc_3 then 16 else 20)), loc_1)
					if loc_1 == 0 then
						desired = 2
						break
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				rt_store_i32(memory_at_0, loc_1 + 24, loc_6)
				loc_2 = rt_load_i32(memory_at_0, loc_3 + 16)
				if loc_2 ~= 0 then
					while true do
						rt_store_i32(memory_at_0, loc_1 + 16, loc_2)
						rt_store_i32(memory_at_0, loc_2 + 24, loc_1)
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				loc_2 = rt_load_i32(memory_at_0, loc_3 + 20)
				if loc_2 == 0 then
					break
				end
				rt_store_i32(memory_at_0, loc_1 + 20, loc_2)
				rt_store_i32(memory_at_0, loc_2 + 24, loc_1)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			if loc_3 >= loc_4 then
				break
			end
			loc_2 = rt_load_i32(memory_at_0, loc_4 + 4)
			if bit_and(loc_2, 1) == 0 then
				break
			end
			while true do
				while true do
					while true do
						while true do
							if bit_and(loc_2, 2) == 0 then
								while true do
									if rt_load_i32(memory_at_0, 10548) == loc_4 then
										while true do
											rt_store_i32(memory_at_0, 10548, loc_3)
											loc_0 = rt_add_i32(rt_load_i32(memory_at_0, 10536), loc_0)
											rt_store_i32(memory_at_0, 10536, loc_0)
											rt_store_i32(memory_at_0, loc_3 + 4, bit_or(loc_0, 1))
											if loc_3 ~= rt_load_i32(memory_at_0, 10544) then
												desired = 1
												break
											end
											rt_store_i32(memory_at_0, 10532, 0)
											rt_store_i32(memory_at_0, 10544, 0)
											desired = 0
											break
										end
										if desired then
											break
										end
									end
									if rt_load_i32(memory_at_0, 10544) == loc_4 then
										while true do
											rt_store_i32(memory_at_0, 10544, loc_3)
											loc_0 = rt_add_i32(rt_load_i32(memory_at_0, 10532), loc_0)
											rt_store_i32(memory_at_0, 10532, loc_0)
											rt_store_i32(memory_at_0, loc_3 + 4, bit_or(loc_0, 1))
											rt_store_i32(memory_at_0, rt_add_i32(loc_0, loc_3), loc_0)
											desired = 0
											break
										end
										if desired then
											break
										end
									end
									loc_0 = rt_add_i32(bit_and(loc_2, 4294967288), loc_0)
									loc_1 = rt_load_i32(memory_at_0, loc_4 + 12)
									if loc_2 <= 255 then
										while true do
											loc_5 = rt_load_i32(memory_at_0, loc_4 + 8)
											loc_4 = rt_shr_u32(loc_2, 3)
											loc_2 = rt_add_i32(rt_shl_i32(loc_4, 3), 10564)
											if loc_1 == loc_5 then
												while true do
													rt_store_i32(memory_at_0, 10524, bit_and(rt_load_i32(memory_at_0, 10524), rt_rotl_i32(4294967294, loc_4)))
													desired = 3
													break
												end
												if desired then
													break
												end
											end
											rt_store_i32(memory_at_0, loc_5 + 12, loc_1)
											rt_store_i32(memory_at_0, loc_1 + 8, loc_5)
											desired = 3
											break
										end
										if desired then
											break
										end
									end
									loc_6 = rt_load_i32(memory_at_0, loc_4 + 24)
									if loc_1 ~= loc_4 then
										while true do
											loc_2 = rt_load_i32(memory_at_0, loc_4 + 8)
											rt_store_i32(memory_at_0, loc_2 + 12, loc_1)
											rt_store_i32(memory_at_0, loc_1 + 8, loc_2)
											desired = 4
											break
										end
										if desired then
											break
										end
									end
									loc_2 = rt_load_i32(memory_at_0, loc_4 + 20)
									if loc_2 ~= 0 then
										while true do
											reg_0 = rt_add_i32(loc_4, 20)
											break
										end
										if desired then
											break
										end
									else
										while true do
											loc_2 = rt_load_i32(memory_at_0, loc_4 + 16)
											if loc_2 == 0 then
												desired = 5
												break
											end
											reg_0 = rt_add_i32(loc_4, 16)
											break
										end
										if desired then
											break
										end
									end
									loc_5 = reg_0
									while true do
										loc_7 = loc_5
										loc_1 = loc_2
										loc_5 = rt_add_i32(loc_1, 20)
										loc_2 = rt_load_i32(memory_at_0, loc_1 + 20)
										if loc_2 ~= 0 then
											continue
										end
										loc_5 = rt_add_i32(loc_1, 16)
										loc_2 = rt_load_i32(memory_at_0, loc_1 + 16)
										if loc_2 ~= 0 then
											continue
										end
										break
									end
									if desired then
										break
									end
									rt_store_i32(memory_at_0, loc_7, 0)
									desired = 4
									break
								end
								if desired then
									if desired == 5 then
										desired = nil
									end
									break
								end
							end
							rt_store_i32(memory_at_0, loc_4 + 4, bit_and(loc_2, 4294967294))
							rt_store_i32(memory_at_0, loc_3 + 4, bit_or(loc_0, 1))
							rt_store_i32(memory_at_0, rt_add_i32(loc_0, loc_3), loc_0)
							desired = 2
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						loc_1 = 0
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					if loc_6 == 0 then
						break
					end
					while true do
						loc_5 = rt_load_i32(memory_at_0, loc_4 + 28)
						loc_2 = rt_add_i32(rt_shl_i32(loc_5, 2), 10828)
						if rt_load_i32(memory_at_0, loc_2) == loc_4 then
							while true do
								rt_store_i32(memory_at_0, loc_2, loc_1)
								if loc_1 ~= 0 then
									desired = 4
									break
								end
								rt_store_i32(memory_at_0, 10528, bit_and(rt_load_i32(memory_at_0, 10528), rt_rotl_i32(4294967294, loc_5)))
								desired = 3
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						rt_store_i32(memory_at_0, rt_add_i32(loc_6, (if rt_load_i32(memory_at_0, loc_6 + 16) == loc_4 then 16 else 20)), loc_1)
						if loc_1 == 0 then
							desired = 3
							break
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					rt_store_i32(memory_at_0, loc_1 + 24, loc_6)
					loc_2 = rt_load_i32(memory_at_0, loc_4 + 16)
					if loc_2 ~= 0 then
						while true do
							rt_store_i32(memory_at_0, loc_1 + 16, loc_2)
							rt_store_i32(memory_at_0, loc_2 + 24, loc_1)
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					loc_2 = rt_load_i32(memory_at_0, loc_4 + 20)
					if loc_2 == 0 then
						break
					end
					rt_store_i32(memory_at_0, loc_1 + 20, loc_2)
					rt_store_i32(memory_at_0, loc_2 + 24, loc_1)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				rt_store_i32(memory_at_0, loc_3 + 4, bit_or(loc_0, 1))
				rt_store_i32(memory_at_0, rt_add_i32(loc_0, loc_3), loc_0)
				if loc_3 ~= rt_load_i32(memory_at_0, 10544) then
					break
				end
				rt_store_i32(memory_at_0, 10532, loc_0)
				desired = 0
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			if loc_0 <= 255 then
				while true do
					loc_1 = rt_add_i32(bit_and(loc_0, 4294967288), 10564)
					while true do
						loc_2 = rt_load_i32(memory_at_0, 10524)
						loc_0 = rt_shl_i32(1, rt_shr_u32(loc_0, 3))
						if bit_and(loc_2, loc_0) == 0 then
							while true do
								rt_store_i32(memory_at_0, 10524, bit_or(loc_0, loc_2))
								reg_0 = loc_1
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						reg_0 = rt_load_i32(memory_at_0, loc_1 + 8)
						break
					end
					if desired then
						break
					end
					loc_0 = reg_0
					rt_store_i32(memory_at_0, loc_1 + 8, loc_3)
					rt_store_i32(memory_at_0, loc_0 + 12, loc_3)
					rt_store_i32(memory_at_0, loc_3 + 12, loc_1)
					rt_store_i32(memory_at_0, loc_3 + 8, loc_0)
					desired = 0
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_1 = 31
			if loc_0 <= 16777215 then
				while true do
					loc_1 = bit_countlz(rt_shr_u32(loc_0, 8))
					loc_1 = rt_add_i32(rt_sub_i32(bit_and(rt_shr_u32(loc_0, rt_sub_i32(38, loc_1)), 1), rt_shl_i32(loc_1, 1)), 62)
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			rt_store_i32(memory_at_0, loc_3 + 28, loc_1)
			rt_store_i64(memory_at_0, loc_3 + 16, rt_i64_ZERO)
			loc_4 = rt_add_i32(rt_shl_i32(loc_1, 2), 10828)
			while true do
				while true do
					while true do
						loc_2 = rt_load_i32(memory_at_0, 10528)
						loc_5 = rt_shl_i32(1, loc_1)
						if bit_and(loc_2, loc_5) == 0 then
							while true do
								rt_store_i32(memory_at_0, 10528, bit_or(loc_2, loc_5))
								loc_1 = 24
								loc_5 = loc_4
								reg_0 = 8
								desired = 4
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						loc_1 = rt_shl_i32(loc_0, (if loc_1 ~= 31 then rt_sub_i32(25, rt_shr_u32(loc_1, 1)) else 0))
						loc_5 = rt_load_i32(memory_at_0, loc_4)
						while true do
							loc_2 = loc_5
							if bit_and(rt_load_i32(memory_at_0, loc_2 + 4), 4294967288) == loc_0 then
								desired = 3
								break
							end
							loc_5 = rt_shr_u32(loc_1, 29)
							loc_1 = rt_shl_i32(loc_1, 1)
							loc_4 = rt_add_i32(rt_add_i32(loc_2, bit_and(loc_5, 4)), 16)
							loc_5 = rt_load_i32(memory_at_0, loc_4)
							if loc_5 ~= 0 then
								continue
							end
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						loc_1 = 24
						loc_5 = loc_2
						reg_0 = 8
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					loc_0 = reg_0
					loc_2 = loc_3
					reg_0 = loc_3
					desired = 2
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_5 = rt_load_i32(memory_at_0, loc_2 + 8)
				rt_store_i32(memory_at_0, loc_5 + 12, loc_3)
				loc_1 = 8
				loc_4 = rt_add_i32(loc_2, 8)
				loc_0 = 24
				reg_0 = 0
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_7 = reg_0
			rt_store_i32(memory_at_0, loc_4, loc_3)
			rt_store_i32(memory_at_0, rt_add_i32(loc_1, loc_3), loc_5)
			rt_store_i32(memory_at_0, loc_3 + 12, loc_2)
			rt_store_i32(memory_at_0, rt_add_i32(loc_0, loc_3), loc_7)
			loc_3 = rt_sub_i32(rt_load_i32(memory_at_0, 10556), 1)
			rt_store_i32(memory_at_0, 10556, (if loc_3 ~= 0 then loc_3 else 4294967295))
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		break
	end
end
FUNC_LIST[223] = --[[ internal_memalign ]] function(loc_0, loc_1)
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local reg_0
	local desired
	while true do
		loc_2 = 16
		while true do
			loc_3 = (if loc_0 <= 16 then 16 else loc_0)
			if bit_and(loc_3, rt_sub_i32(loc_3, 1)) == 0 then
				while true do
					loc_0 = loc_3
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			while true do
				loc_0 = loc_2
				loc_2 = rt_shl_i32(loc_0, 1)
				if loc_0 < loc_3 then
					continue
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		if loc_1 >= rt_sub_i32(4294967232, loc_0) then
			while true do
				reg_0 = FUNC_LIST[217]()
				rt_store_i32(memory_at_0, reg_0, 48)
				reg_0 = 0
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		loc_1 = (if loc_1 < 11 then 16 else bit_and(rt_add_i32(loc_1, 11), 4294967288))
		reg_0 = FUNC_LIST[220](rt_add_i32(rt_add_i32(loc_1, loc_0), 12))
		loc_2 = reg_0
		if loc_2 == 0 then
			while true do
				reg_0 = 0
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		loc_3 = rt_sub_i32(loc_2, 8)
		while true do
			if bit_and(rt_sub_i32(loc_0, 1), loc_2) == 0 then
				while true do
					loc_0 = loc_3
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_5 = rt_sub_i32(loc_2, 4)
			loc_6 = rt_load_i32(memory_at_0, loc_5)
			loc_2 = rt_sub_i32(bit_and(rt_sub_i32(rt_add_i32(loc_0, loc_2), 1), rt_sub_i32(0, loc_0)), 8)
			loc_0 = rt_add_i32(loc_2, (if rt_sub_i32(loc_2, loc_3) <= 15 then loc_0 else 0))
			loc_2 = rt_sub_i32(loc_0, loc_3)
			loc_4 = rt_sub_i32(bit_and(loc_6, 4294967288), loc_2)
			if bit_and(loc_6, 3) == 0 then
				while true do
					loc_3 = rt_load_i32(memory_at_0, loc_3)
					rt_store_i32(memory_at_0, loc_0 + 4, loc_4)
					rt_store_i32(memory_at_0, loc_0, rt_add_i32(loc_2, loc_3))
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			rt_store_i32(memory_at_0, loc_0 + 4, bit_or(bit_or(loc_4, bit_and(rt_load_i32(memory_at_0, loc_0 + 4), 1)), 2))
			loc_4 = rt_add_i32(loc_0, loc_4)
			rt_store_i32(memory_at_0, loc_4 + 4, bit_or(rt_load_i32(memory_at_0, loc_4 + 4), 1))
			rt_store_i32(memory_at_0, loc_5, bit_or(bit_or(loc_2, bit_and(rt_load_i32(memory_at_0, loc_5), 1)), 2))
			loc_4 = rt_add_i32(loc_2, loc_3)
			rt_store_i32(memory_at_0, loc_4 + 4, bit_or(rt_load_i32(memory_at_0, loc_4 + 4), 1))
			FUNC_LIST[225](loc_3, loc_2)
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		while true do
			loc_2 = rt_load_i32(memory_at_0, loc_0 + 4)
			if bit_and(loc_2, 3) == 0 then
				break
			end
			loc_3 = bit_and(loc_2, 4294967288)
			if loc_3 <= rt_add_i32(loc_1, 16) then
				break
			end
			rt_store_i32(memory_at_0, loc_0 + 4, bit_or(bit_or(loc_1, bit_and(loc_2, 1)), 2))
			loc_2 = rt_add_i32(loc_0, loc_1)
			loc_1 = rt_sub_i32(loc_3, loc_1)
			rt_store_i32(memory_at_0, loc_2 + 4, bit_or(loc_1, 3))
			loc_3 = rt_add_i32(loc_0, loc_3)
			rt_store_i32(memory_at_0, loc_3 + 4, bit_or(rt_load_i32(memory_at_0, loc_3 + 4), 1))
			FUNC_LIST[225](loc_2, loc_1)
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		reg_0 = rt_add_i32(loc_0, 8)
		break
	end
	return reg_0
end
FUNC_LIST[224] = --[[ dlposix_memalign ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local reg_0
	local desired
	while true do
		while true do
			while true do
				if loc_1 == 8 then
					while true do
						reg_0 = FUNC_LIST[220](loc_2)
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				loc_3 = 28
				if loc_1 < 4 then
					desired = 1
					break
				end
				if bit_and(loc_1, 3) ~= 0 then
					desired = 1
					break
				end
				loc_4 = rt_shr_u32(loc_1, 2)
				if bit_and(loc_4, rt_sub_i32(loc_4, 1)) ~= 0 then
					desired = 1
					break
				end
				loc_3 = 48
				if rt_sub_i32(4294967232, loc_1) < loc_2 then
					desired = 1
					break
				end
				reg_0 = FUNC_LIST[223]((if loc_1 <= 16 then 16 else loc_1), loc_2)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_1 = reg_0
			if loc_1 == 0 then
				while true do
					reg_0 = 48
					desired = 0
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			rt_store_i32(memory_at_0, loc_0, loc_1)
			loc_3 = 0
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		reg_0 = loc_3
		break
	end
	return reg_0
end
FUNC_LIST[225] = --[[ dispose_chunk ]] function(loc_0, loc_1)
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local loc_8 = 0
	local reg_0
	local desired
	while true do
		loc_4 = rt_add_i32(loc_0, loc_1)
		while true do
			while true do
				loc_2 = rt_load_i32(memory_at_0, loc_0 + 4)
				if bit_and(loc_2, 1) ~= 0 then
					break
				end
				if bit_and(loc_2, 2) == 0 then
					desired = 1
					break
				end
				loc_3 = rt_load_i32(memory_at_0, loc_0)
				loc_1 = rt_add_i32(loc_3, loc_1)
				while true do
					while true do
						while true do
							loc_0 = rt_sub_i32(loc_0, loc_3)
							if loc_0 ~= rt_load_i32(memory_at_0, 10544) then
								while true do
									loc_2 = rt_load_i32(memory_at_0, loc_0 + 12)
									if loc_3 <= 255 then
										while true do
											loc_7 = rt_shr_u32(loc_3, 3)
											loc_3 = rt_add_i32(rt_shl_i32(loc_7, 3), 10564)
											loc_5 = rt_load_i32(memory_at_0, loc_0 + 8)
											if loc_5 ~= loc_2 then
												desired = 5
												break
											end
											rt_store_i32(memory_at_0, 10524, bit_and(rt_load_i32(memory_at_0, 10524), rt_rotl_i32(4294967294, loc_7)))
											desired = 2
											break
										end
										if desired then
											break
										end
									end
									loc_6 = rt_load_i32(memory_at_0, loc_0 + 24)
									if loc_0 ~= loc_2 then
										while true do
											loc_3 = rt_load_i32(memory_at_0, loc_0 + 8)
											rt_store_i32(memory_at_0, loc_3 + 12, loc_2)
											rt_store_i32(memory_at_0, loc_2 + 8, loc_3)
											desired = 3
											break
										end
										if desired then
											break
										end
									end
									loc_3 = rt_load_i32(memory_at_0, loc_0 + 20)
									if loc_3 ~= 0 then
										while true do
											reg_0 = rt_add_i32(loc_0, 20)
											break
										end
										if desired then
											break
										end
									else
										while true do
											loc_3 = rt_load_i32(memory_at_0, loc_0 + 16)
											if loc_3 == 0 then
												desired = 4
												break
											end
											reg_0 = rt_add_i32(loc_0, 16)
											break
										end
										if desired then
											break
										end
									end
									loc_5 = reg_0
									while true do
										loc_7 = loc_5
										loc_2 = loc_3
										loc_5 = rt_add_i32(loc_2, 20)
										loc_3 = rt_load_i32(memory_at_0, loc_2 + 20)
										if loc_3 ~= 0 then
											continue
										end
										loc_5 = rt_add_i32(loc_2, 16)
										loc_3 = rt_load_i32(memory_at_0, loc_2 + 16)
										if loc_3 ~= 0 then
											continue
										end
										break
									end
									if desired then
										break
									end
									rt_store_i32(memory_at_0, loc_7, 0)
									desired = 3
									break
								end
								if desired then
									if desired == 5 then
										desired = nil
									end
									break
								end
							end
							loc_2 = rt_load_i32(memory_at_0, loc_4 + 4)
							if bit_and(loc_2, 3) ~= 3 then
								desired = 2
								break
							end
							rt_store_i32(memory_at_0, 10532, loc_1)
							rt_store_i32(memory_at_0, loc_4 + 4, bit_and(loc_2, 4294967294))
							rt_store_i32(memory_at_0, loc_0 + 4, bit_or(loc_1, 1))
							rt_store_i32(memory_at_0, loc_4, loc_1)
							desired = 0
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						rt_store_i32(memory_at_0, loc_5 + 12, loc_2)
						rt_store_i32(memory_at_0, loc_2 + 8, loc_5)
						desired = 2
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					loc_2 = 0
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				if loc_6 == 0 then
					break
				end
				while true do
					loc_5 = rt_load_i32(memory_at_0, loc_0 + 28)
					loc_3 = rt_add_i32(rt_shl_i32(loc_5, 2), 10828)
					if rt_load_i32(memory_at_0, loc_3) == loc_0 then
						while true do
							rt_store_i32(memory_at_0, loc_3, loc_2)
							if loc_2 ~= 0 then
								desired = 3
								break
							end
							rt_store_i32(memory_at_0, 10528, bit_and(rt_load_i32(memory_at_0, 10528), rt_rotl_i32(4294967294, loc_5)))
							desired = 2
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					rt_store_i32(memory_at_0, rt_add_i32(loc_6, (if rt_load_i32(memory_at_0, loc_6 + 16) == loc_0 then 16 else 20)), loc_2)
					if loc_2 == 0 then
						desired = 2
						break
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				rt_store_i32(memory_at_0, loc_2 + 24, loc_6)
				loc_3 = rt_load_i32(memory_at_0, loc_0 + 16)
				if loc_3 ~= 0 then
					while true do
						rt_store_i32(memory_at_0, loc_2 + 16, loc_3)
						rt_store_i32(memory_at_0, loc_3 + 24, loc_2)
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				loc_3 = rt_load_i32(memory_at_0, loc_0 + 20)
				if loc_3 == 0 then
					break
				end
				rt_store_i32(memory_at_0, loc_2 + 20, loc_3)
				rt_store_i32(memory_at_0, loc_3 + 24, loc_2)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			while true do
				while true do
					while true do
						while true do
							loc_3 = rt_load_i32(memory_at_0, loc_4 + 4)
							if bit_and(loc_3, 2) == 0 then
								while true do
									if rt_load_i32(memory_at_0, 10548) == loc_4 then
										while true do
											rt_store_i32(memory_at_0, 10548, loc_0)
											loc_1 = rt_add_i32(rt_load_i32(memory_at_0, 10536), loc_1)
											rt_store_i32(memory_at_0, 10536, loc_1)
											rt_store_i32(memory_at_0, loc_0 + 4, bit_or(loc_1, 1))
											if loc_0 ~= rt_load_i32(memory_at_0, 10544) then
												desired = 1
												break
											end
											rt_store_i32(memory_at_0, 10532, 0)
											rt_store_i32(memory_at_0, 10544, 0)
											desired = 0
											break
										end
										if desired then
											break
										end
									end
									if rt_load_i32(memory_at_0, 10544) == loc_4 then
										while true do
											rt_store_i32(memory_at_0, 10544, loc_0)
											loc_1 = rt_add_i32(rt_load_i32(memory_at_0, 10532), loc_1)
											rt_store_i32(memory_at_0, 10532, loc_1)
											rt_store_i32(memory_at_0, loc_0 + 4, bit_or(loc_1, 1))
											rt_store_i32(memory_at_0, rt_add_i32(loc_0, loc_1), loc_1)
											desired = 0
											break
										end
										if desired then
											break
										end
									end
									loc_1 = rt_add_i32(bit_and(loc_3, 4294967288), loc_1)
									loc_2 = rt_load_i32(memory_at_0, loc_4 + 12)
									if loc_3 <= 255 then
										while true do
											loc_5 = rt_load_i32(memory_at_0, loc_4 + 8)
											loc_4 = rt_shr_u32(loc_3, 3)
											loc_3 = rt_add_i32(rt_shl_i32(loc_4, 3), 10564)
											if loc_2 == loc_5 then
												while true do
													rt_store_i32(memory_at_0, 10524, bit_and(rt_load_i32(memory_at_0, 10524), rt_rotl_i32(4294967294, loc_4)))
													desired = 3
													break
												end
												if desired then
													break
												end
											end
											rt_store_i32(memory_at_0, loc_5 + 12, loc_2)
											rt_store_i32(memory_at_0, loc_2 + 8, loc_5)
											desired = 3
											break
										end
										if desired then
											break
										end
									end
									loc_6 = rt_load_i32(memory_at_0, loc_4 + 24)
									if loc_2 ~= loc_4 then
										while true do
											loc_3 = rt_load_i32(memory_at_0, loc_4 + 8)
											rt_store_i32(memory_at_0, loc_3 + 12, loc_2)
											rt_store_i32(memory_at_0, loc_2 + 8, loc_3)
											desired = 4
											break
										end
										if desired then
											break
										end
									end
									loc_3 = rt_load_i32(memory_at_0, loc_4 + 20)
									if loc_3 ~= 0 then
										while true do
											reg_0 = rt_add_i32(loc_4, 20)
											break
										end
										if desired then
											break
										end
									else
										while true do
											loc_3 = rt_load_i32(memory_at_0, loc_4 + 16)
											if loc_3 == 0 then
												desired = 5
												break
											end
											reg_0 = rt_add_i32(loc_4, 16)
											break
										end
										if desired then
											break
										end
									end
									loc_5 = reg_0
									while true do
										loc_7 = loc_5
										loc_2 = loc_3
										loc_5 = rt_add_i32(loc_2, 20)
										loc_3 = rt_load_i32(memory_at_0, loc_2 + 20)
										if loc_3 ~= 0 then
											continue
										end
										loc_5 = rt_add_i32(loc_2, 16)
										loc_3 = rt_load_i32(memory_at_0, loc_2 + 16)
										if loc_3 ~= 0 then
											continue
										end
										break
									end
									if desired then
										break
									end
									rt_store_i32(memory_at_0, loc_7, 0)
									desired = 4
									break
								end
								if desired then
									if desired == 5 then
										desired = nil
									end
									break
								end
							end
							rt_store_i32(memory_at_0, loc_4 + 4, bit_and(loc_3, 4294967294))
							rt_store_i32(memory_at_0, loc_0 + 4, bit_or(loc_1, 1))
							rt_store_i32(memory_at_0, rt_add_i32(loc_0, loc_1), loc_1)
							desired = 2
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						loc_2 = 0
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					if loc_6 == 0 then
						break
					end
					while true do
						loc_5 = rt_load_i32(memory_at_0, loc_4 + 28)
						loc_3 = rt_add_i32(rt_shl_i32(loc_5, 2), 10828)
						if rt_load_i32(memory_at_0, loc_3) == loc_4 then
							while true do
								rt_store_i32(memory_at_0, loc_3, loc_2)
								if loc_2 ~= 0 then
									desired = 4
									break
								end
								rt_store_i32(memory_at_0, 10528, bit_and(rt_load_i32(memory_at_0, 10528), rt_rotl_i32(4294967294, loc_5)))
								desired = 3
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						rt_store_i32(memory_at_0, rt_add_i32(loc_6, (if rt_load_i32(memory_at_0, loc_6 + 16) == loc_4 then 16 else 20)), loc_2)
						if loc_2 == 0 then
							desired = 3
							break
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					rt_store_i32(memory_at_0, loc_2 + 24, loc_6)
					loc_3 = rt_load_i32(memory_at_0, loc_4 + 16)
					if loc_3 ~= 0 then
						while true do
							rt_store_i32(memory_at_0, loc_2 + 16, loc_3)
							rt_store_i32(memory_at_0, loc_3 + 24, loc_2)
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					loc_3 = rt_load_i32(memory_at_0, loc_4 + 20)
					if loc_3 == 0 then
						break
					end
					rt_store_i32(memory_at_0, loc_2 + 20, loc_3)
					rt_store_i32(memory_at_0, loc_3 + 24, loc_2)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				rt_store_i32(memory_at_0, loc_0 + 4, bit_or(loc_1, 1))
				rt_store_i32(memory_at_0, rt_add_i32(loc_0, loc_1), loc_1)
				if loc_0 ~= rt_load_i32(memory_at_0, 10544) then
					break
				end
				rt_store_i32(memory_at_0, 10532, loc_1)
				desired = 0
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			if loc_1 <= 255 then
				while true do
					loc_2 = rt_add_i32(bit_and(loc_1, 4294967288), 10564)
					while true do
						loc_3 = rt_load_i32(memory_at_0, 10524)
						loc_1 = rt_shl_i32(1, rt_shr_u32(loc_1, 3))
						if bit_and(loc_3, loc_1) == 0 then
							while true do
								rt_store_i32(memory_at_0, 10524, bit_or(loc_1, loc_3))
								reg_0 = loc_2
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						reg_0 = rt_load_i32(memory_at_0, loc_2 + 8)
						break
					end
					if desired then
						break
					end
					loc_1 = reg_0
					rt_store_i32(memory_at_0, loc_2 + 8, loc_0)
					rt_store_i32(memory_at_0, loc_1 + 12, loc_0)
					rt_store_i32(memory_at_0, loc_0 + 12, loc_2)
					rt_store_i32(memory_at_0, loc_0 + 8, loc_1)
					desired = 0
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_2 = 31
			if loc_1 <= 16777215 then
				while true do
					loc_2 = bit_countlz(rt_shr_u32(loc_1, 8))
					loc_2 = rt_add_i32(rt_sub_i32(bit_and(rt_shr_u32(loc_1, rt_sub_i32(38, loc_2)), 1), rt_shl_i32(loc_2, 1)), 62)
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			rt_store_i32(memory_at_0, loc_0 + 28, loc_2)
			rt_store_i64(memory_at_0, loc_0 + 16, rt_i64_ZERO)
			loc_3 = rt_add_i32(rt_shl_i32(loc_2, 2), 10828)
			while true do
				while true do
					loc_5 = rt_load_i32(memory_at_0, 10528)
					loc_4 = rt_shl_i32(1, loc_2)
					if bit_and(loc_5, loc_4) == 0 then
						while true do
							rt_store_i32(memory_at_0, 10528, bit_or(loc_4, loc_5))
							rt_store_i32(memory_at_0, loc_3, loc_0)
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					loc_2 = rt_shl_i32(loc_1, (if loc_2 ~= 31 then rt_sub_i32(25, rt_shr_u32(loc_2, 1)) else 0))
					loc_5 = rt_load_i32(memory_at_0, loc_3)
					while true do
						loc_3 = loc_5
						if bit_and(rt_load_i32(memory_at_0, loc_3 + 4), 4294967288) == loc_1 then
							desired = 2
							break
						end
						loc_5 = rt_shr_u32(loc_2, 29)
						loc_2 = rt_shl_i32(loc_2, 1)
						loc_8 = rt_add_i32(loc_3, bit_and(loc_5, 4))
						loc_4 = rt_add_i32(loc_8, 16)
						loc_5 = rt_load_i32(memory_at_0, loc_4)
						if loc_5 ~= 0 then
							continue
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					rt_store_i32(memory_at_0, loc_8 + 16, loc_0)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				rt_store_i32(memory_at_0, loc_0 + 24, loc_3)
				rt_store_i32(memory_at_0, loc_0 + 12, loc_0)
				rt_store_i32(memory_at_0, loc_0 + 8, loc_0)
				desired = 0
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_1 = rt_load_i32(memory_at_0, loc_3 + 8)
			rt_store_i32(memory_at_0, loc_1 + 12, loc_0)
			rt_store_i32(memory_at_0, loc_3 + 8, loc_0)
			rt_store_i32(memory_at_0, loc_0 + 24, 0)
			rt_store_i32(memory_at_0, loc_0 + 12, loc_3)
			rt_store_i32(memory_at_0, loc_0 + 8, loc_1)
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		break
	end
end
FUNC_LIST[226] = --[[ aligned_alloc ]] function(loc_0, loc_1)
	local loc_2 = 0
	local loc_3 = 0
	local reg_0
	while true do
		loc_2 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_2
		while true do
			if bit_and(loc_0, 3) ~= 0 then
				break
			end
			if (loc_1 % loc_0) ~= 0 then
				break
			end
			reg_0 = FUNC_LIST[224](rt_add_i32(loc_2, 12), loc_0, loc_1)
			loc_0 = reg_0
			loc_3 = (if loc_0 ~= 0 then 0 else rt_load_i32(memory_at_0, loc_2 + 12))
			break
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_2, 16)
		reg_0 = loc_3
		break
	end
	return reg_0
end
FUNC_LIST[227] = --[[ operator new(unsigned long) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[228](loc_0)
		loc_0 = reg_0
		if loc_0 == 0 then
			while true do
				FUNC_LIST[229]()
				break
			end
		end
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[228] = --[[ operator_new_impl(unsigned long) ]] function(loc_0)
	local loc_1 = 0
	local loc_2 = 0
	local reg_0
	local desired
	while true do
		loc_1 = (if loc_0 <= 1 then 1 else loc_0)
		while true do
			while true do
				reg_0 = FUNC_LIST[220](loc_1)
				loc_2 = reg_0
				if loc_2 ~= 0 then
					break
				end
				reg_0 = FUNC_LIST[455]()
				loc_0 = reg_0
				if loc_0 == 0 then
					break
				end
				TABLE_LIST[0].data[loc_0]()
				desired = 1
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
					continue
				end
				break
			end
			break
		end
		reg_0 = loc_2
		break
	end
	return reg_0
end
FUNC_LIST[229] = --[[ __throw_bad_alloc_shim() ]] function()
	while true do
		FUNC_LIST[236]()
		error("out of code bounds")
	end
end
FUNC_LIST[230] = --[[ operator delete(void*) ]] function(loc_0)
	while true do
		FUNC_LIST[222](loc_0)
		break
	end
end
FUNC_LIST[231] = --[[ operator new(unsigned long, std::align_val_t) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[232](loc_0, loc_1)
		loc_1 = reg_0
		if loc_1 == 0 then
			while true do
				FUNC_LIST[229]()
				break
			end
		end
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[232] = --[[ operator_new_aligned_impl(unsigned long, std::align_val_t) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		loc_2 = (if loc_1 <= 4 then 4 else loc_1)
		loc_0 = (if loc_0 <= 1 then 1 else loc_0)
		while true do
			while true do
				reg_0 = FUNC_LIST[233](loc_2, loc_0)
				loc_3 = reg_0
				if loc_3 ~= 0 then
					break
				end
				reg_0 = FUNC_LIST[455]()
				loc_1 = reg_0
				if loc_1 == 0 then
					break
				end
				TABLE_LIST[0].data[loc_1]()
				desired = 1
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
					continue
				end
				break
			end
			break
		end
		reg_0 = loc_3
		break
	end
	return reg_0
end
FUNC_LIST[233] = --[[ std::__2::__libcpp_aligned_alloc[abi:nn180100](unsigned long, unsigned long) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = bit_and(rt_sub_i32(rt_add_i32(loc_0, loc_1), 1), rt_sub_i32(0, loc_0))
		reg_0 = FUNC_LIST[226](loc_0, (if loc_1 < loc_2 then loc_2 else loc_1))
		break
	end
	return reg_0
end
FUNC_LIST[234] = --[[ operator delete(void*, std::align_val_t) ]] function(loc_0, loc_1)
	while true do
		FUNC_LIST[235](loc_0)
		break
	end
end
FUNC_LIST[235] = --[[ std::__2::__libcpp_aligned_free[abi:nn180100](void*) ]] function(loc_0)
	while true do
		FUNC_LIST[222](loc_0)
		break
	end
end
FUNC_LIST[236] = --[[ std::__throw_bad_alloc() ]] function()
	while true do
		FUNC_LIST[2]()
		error("out of code bounds")
	end
end
FUNC_LIST[237] = --[[ std::exception::exception[abi:nn180100]() ]] function(loc_0)
	local reg_0
	while true do
		rt_store_i32(memory_at_0, loc_0, 10216)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[238] = --[[ std::__2::__libcpp_refstring::__libcpp_refstring(char const*) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local loc_3 = 0
	local reg_0
	local reg_1
	while true do
		reg_0 = FUNC_LIST[192](loc_1)
		loc_2 = reg_0
		reg_0 = FUNC_LIST[227](rt_add_i32(loc_2, 13))
		loc_3 = reg_0
		rt_store_i32(memory_at_0, loc_3 + 8, 0)
		rt_store_i32(memory_at_0, loc_3 + 4, loc_2)
		rt_store_i32(memory_at_0, loc_3, loc_2)
		reg_1 = FUNC_LIST[239](loc_3)
		reg_1 = FUNC_LIST[188](reg_1, loc_1, rt_add_i32(loc_2, 1))
		rt_store_i32(memory_at_0, loc_0, reg_1)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[239] = --[[ std::__2::__refstring_imp::(anonymous namespace)::data_from_rep(std::__2::__refstring_imp::(anonymous namespace)::_Rep_base*) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = rt_add_i32(loc_0, 12)
		break
	end
	return reg_0
end
FUNC_LIST[240] = --[[ std::logic_error::logic_error(char const*) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[237](loc_0)
		loc_0 = reg_0
		rt_store_i32(memory_at_0, loc_0, 10328)
		reg_0 = FUNC_LIST[238](rt_add_i32(loc_0, 4), loc_1)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[241] = --[[ std::__2::__libcpp_refstring::__uses_refcount() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 1
		break
	end
	return reg_0
end
FUNC_LIST[242] = --[[ char const* std::__2::__to_address[abi:nn180100]<char const>(char const*) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[243] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__is_long[abi:nn180100]() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[244](loc_0)
		reg_0 = rt_shr_u32(rt_load_i32_u8(memory_at_0, reg_0 + 11), 7)
		break
	end
	return reg_0
end
FUNC_LIST[244] = --[[ std::__2::__compressed_pair<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, std::__2::allocator<char>>::first[abi:nn180100]() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[245](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[245] = --[[ std::__2::__compressed_pair_elem<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, 0, false>::__get[abi:nn180100]() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[246] = --[[ __lockfile ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 1
		break
	end
	return reg_0
end
FUNC_LIST[247] = --[[ __unlockfile ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[248] = --[[ __towrite ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	local desired
	while true do
		loc_1 = rt_load_i32(memory_at_0, loc_0 + 72)
		rt_store_i32(memory_at_0, loc_0 + 72, bit_or(rt_sub_i32(loc_1, 1), loc_1))
		loc_1 = rt_load_i32(memory_at_0, loc_0)
		if bit_and(loc_1, 8) ~= 0 then
			while true do
				rt_store_i32(memory_at_0, loc_0, bit_or(loc_1, 32))
				reg_0 = 4294967295
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		rt_store_i64(memory_at_0, loc_0 + 4, rt_i64_ZERO)
		loc_1 = rt_load_i32(memory_at_0, loc_0 + 44)
		rt_store_i32(memory_at_0, loc_0 + 28, loc_1)
		rt_store_i32(memory_at_0, loc_0 + 20, loc_1)
		rt_store_i32(memory_at_0, loc_0 + 16, rt_add_i32(loc_1, rt_load_i32(memory_at_0, loc_0 + 48)))
		reg_0 = 0
		break
	end
	return reg_0
end
FUNC_LIST[249] = --[[ strnlen ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[190](loc_0, 0, loc_1)
		loc_2 = reg_0
		reg_0 = (if loc_2 ~= 0 then rt_sub_i32(loc_2, loc_0) else loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[250] = --[[ __syscall_getpid ]] function()
	local reg_0
	while true do
		reg_0 = 42
		break
	end
	return reg_0
end
FUNC_LIST[251] = --[[ getpid ]] function()
	local reg_0
	while true do
		reg_0 = FUNC_LIST[250]()
		break
	end
	return reg_0
end
FUNC_LIST[252] = --[[ __get_tp ]] function()
	local reg_0
	while true do
		reg_0 = 11076
		break
	end
	return reg_0
end
FUNC_LIST[253] = --[[ init_pthread_self ]] function()
	local reg_0
	local reg_1
	while true do
		rt_store_i32(memory_at_0, 11172, 11052)
		reg_1 = FUNC_LIST[251]()
		rt_store_i32(memory_at_0, 11100, reg_1)
		break
	end
end
FUNC_LIST[254] = --[[ wcrtomb ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		loc_3 = 1
		while true do
			if loc_0 ~= 0 then
				while true do
					if loc_1 <= 127 then
						desired = 1
						break
					end
					while true do
						reg_0 = FUNC_LIST[252]()
						if rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, reg_0 + 96)) == 0 then
							while true do
								if bit_and(loc_1, 4294967168) == 57216 then
									desired = 1
									break
								end
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						if loc_1 <= 2047 then
							while true do
								rt_store_i32_n8(memory_at_0, loc_0 + 1, bit_or(bit_and(loc_1, 63), 128))
								rt_store_i32_n8(memory_at_0, loc_0, bit_or(rt_shr_u32(loc_1, 6), 192))
								reg_0 = 2
								desired = 0
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						if bit_and((if bit_and(loc_1, 4294959104) ~= 57344 then 1 else 0), (if loc_1 >= 55296 then 1 else 0)) == 0 then
							while true do
								rt_store_i32_n8(memory_at_0, loc_0 + 2, bit_or(bit_and(loc_1, 63), 128))
								rt_store_i32_n8(memory_at_0, loc_0, bit_or(rt_shr_u32(loc_1, 12), 224))
								rt_store_i32_n8(memory_at_0, loc_0 + 1, bit_or(bit_and(rt_shr_u32(loc_1, 6), 63), 128))
								reg_0 = 3
								desired = 0
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						if rt_sub_i32(loc_1, 65536) <= 1048575 then
							while true do
								rt_store_i32_n8(memory_at_0, loc_0 + 3, bit_or(bit_and(loc_1, 63), 128))
								rt_store_i32_n8(memory_at_0, loc_0, bit_or(rt_shr_u32(loc_1, 18), 240))
								rt_store_i32_n8(memory_at_0, loc_0 + 2, bit_or(bit_and(rt_shr_u32(loc_1, 6), 63), 128))
								rt_store_i32_n8(memory_at_0, loc_0 + 1, bit_or(bit_and(rt_shr_u32(loc_1, 12), 63), 128))
								reg_0 = 4
								desired = 0
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						break
					end
					if desired then
						break
					end
					reg_0 = FUNC_LIST[217]()
					rt_store_i32(memory_at_0, reg_0, 25)
					loc_3 = 4294967295
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			reg_0 = loc_3
			desired = 0
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		rt_store_i32_n8(memory_at_0, loc_0, loc_1)
		reg_0 = 1
		break
	end
	return reg_0
end
FUNC_LIST[255] = --[[ wctomb ]] function(loc_0, loc_1)
	local reg_0
	local desired
	while true do
		if loc_0 == 0 then
			while true do
				reg_0 = 0
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		reg_0 = FUNC_LIST[254](loc_0, loc_1, 0)
		break
	end
	return reg_0
end
FUNC_LIST[256] = --[[ frexp ]] function(loc_0, loc_1)
	local loc_2 = 0
	local loc_3 = rt_i64_ZERO
	local reg_0
	local reg_1
	local desired
	while true do
		loc_3 = rt_reinterpret_i64_f64(loc_0)
		loc_2 = bit_and(rt_wrap_i32_i64(rt_shr_u64(loc_3, rt_i64_from_u32(52, 0))), 2047)
		if loc_2 ~= 2047 then
			while true do
				if loc_2 == 0 then
					while true do
						reg_0 = loc_1
						if loc_0 == 0e0 then
							while true do
								reg_1 = 0
								break
							end
							if desired then
								break
							end
						else
							while true do
								reg_1 = FUNC_LIST[256]((loc_0 * 1.8446744073709552e19), loc_1)
								loc_0 = reg_1
								reg_1 = rt_add_i32(rt_load_i32(memory_at_0, loc_1), 4294967232)
								break
							end
							if desired then
								break
							end
						end
						loc_2 = reg_1
						rt_store_i32(memory_at_0, reg_0, loc_2)
						reg_0 = loc_0
						desired = 0
						break
					end
					if desired then
						break
					end
				end
				rt_store_i32(memory_at_0, loc_1, rt_sub_i32(loc_2, 1022))
				loc_0 = rt_reinterpret_f64_i64(rt_bit_or_i64(rt_bit_and_i64(loc_3, rt_i64_from_u32(4294967295, 2148532223)), rt_i64_from_u32(0, 1071644672)))
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[257] = --[[ __ashlti3 ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = rt_i64_ZERO
	local desired
	while true do
		while true do
			if bit_and(loc_3, 64) ~= 0 then
				while true do
					loc_2 = rt_shl_i64(loc_1, rt_extend_i64_u32(rt_add_i32(loc_3, 4294967232)))
					loc_1 = rt_i64_ZERO
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			if loc_3 == 0 then
				break
			end
			loc_4 = rt_extend_i64_u32(loc_3)
			loc_2 = rt_bit_or_i64(rt_shl_i64(loc_2, loc_4), rt_shr_u64(loc_1, rt_extend_i64_u32(rt_sub_i32(64, loc_3))))
			loc_1 = rt_shl_i64(loc_1, loc_4)
			break
		end
		rt_store_i64(memory_at_0, loc_0, loc_1)
		rt_store_i64(memory_at_0, loc_0 + 8, loc_2)
		break
	end
end
FUNC_LIST[258] = --[[ __lshrti3 ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = rt_i64_ZERO
	local desired
	while true do
		while true do
			if bit_and(loc_3, 64) ~= 0 then
				while true do
					loc_1 = rt_shr_u64(loc_2, rt_extend_i64_u32(rt_add_i32(loc_3, 4294967232)))
					loc_2 = rt_i64_ZERO
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			if loc_3 == 0 then
				break
			end
			loc_4 = rt_extend_i64_u32(loc_3)
			loc_1 = rt_bit_or_i64(rt_shl_i64(loc_2, rt_extend_i64_u32(rt_sub_i32(64, loc_3))), rt_shr_u64(loc_1, loc_4))
			loc_2 = rt_shr_u64(loc_2, loc_4)
			break
		end
		rt_store_i64(memory_at_0, loc_0, loc_1)
		rt_store_i64(memory_at_0, loc_0 + 8, loc_2)
		break
	end
end
FUNC_LIST[259] = --[[ __trunctfdf2 ]] function(loc_0, loc_1)
	local loc_2 = rt_i64_ZERO
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = rt_i64_ZERO
	local loc_6 = rt_i64_ZERO
	local loc_7 = rt_i64_ZERO
	local loc_8 = 0
	local loc_9 = 0
	local loc_10 = 0
	local reg_0
	local desired
	while true do
		loc_4 = rt_sub_i32(GLOBAL_LIST[0].value, 32)
		GLOBAL_LIST[0].value = loc_4
		loc_2 = rt_bit_and_i64(loc_1, rt_i64_from_u32(4294967295, 65535))
		while true do
			loc_7 = rt_bit_and_i64(rt_shr_u64(loc_1, rt_i64_from_u32(48, 0)), rt_i64_from_u32(32767, 0))
			loc_3 = rt_wrap_i32_i64(loc_7)
			if rt_sub_i32(loc_3, 15361) <= 2045 then
				while true do
					loc_2 = rt_bit_or_i64(rt_shl_i64(loc_2, rt_i64_from_u32(4, 0)), rt_shr_u64(loc_0, rt_i64_from_u32(60, 0)))
					loc_6 = rt_extend_i64_u32(rt_sub_i32(loc_3, 15360))
					while true do
						loc_0 = rt_bit_and_i64(loc_0, rt_i64_from_u32(4294967295, 268435455))
						if rt_ge_u64(loc_0, rt_i64_from_u32(1, 134217728)) then
							while true do
								loc_2 = rt_add_i64(loc_2, rt_i64_ONE)
								desired = 3
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						if rt_ne_i64(loc_0, rt_i64_from_u32(0, 134217728)) then
							break
						end
						loc_2 = rt_add_i64(rt_bit_and_i64(loc_2, rt_i64_ONE), loc_2)
						break
					end
					if desired then
						break
					end
					loc_3 = (if rt_gt_u64(loc_2, rt_i64_from_u32(4294967295, 1048575)) then 1 else 0)
					loc_5 = (if loc_3 ~= 0 then rt_i64_ZERO else loc_2)
					reg_0 = rt_add_i64(rt_extend_i64_u32(loc_3), loc_6)
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			while true do
				if rt_eq_i64(rt_bit_or_i64(loc_0, loc_2), rt_i64_ZERO) then
					break
				end
				if rt_ne_i64(loc_7, rt_i64_from_u32(32767, 0)) then
					break
				end
				loc_5 = rt_bit_or_i64(rt_bit_or_i64(rt_shl_i64(loc_2, rt_i64_from_u32(4, 0)), rt_shr_u64(loc_0, rt_i64_from_u32(60, 0))), rt_i64_from_u32(0, 524288))
				reg_0 = rt_i64_from_u32(2047, 0)
				desired = 1
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			if loc_3 > 17406 then
				while true do
					reg_0 = rt_i64_from_u32(2047, 0)
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_9 = (if rt_eq_i64(loc_7, rt_i64_ZERO) then 1 else 0)
			loc_10 = (if loc_9 ~= 0 then 15360 else 15361)
			loc_8 = rt_sub_i32(loc_10, loc_3)
			reg_0 = rt_i64_ZERO
			if rt_gt_i32(loc_8, 112) then
				break
			end
			loc_2 = (if loc_9 ~= 0 then loc_2 else rt_bit_or_i64(loc_2, rt_i64_from_u32(0, 65536)))
			FUNC_LIST[257](rt_add_i32(loc_4, 16), loc_0, loc_2, rt_sub_i32(128, loc_8))
			FUNC_LIST[258](loc_4, loc_0, loc_2, loc_8)
			loc_2 = rt_load_i64(memory_at_0, loc_4)
			loc_0 = rt_bit_or_i64(rt_shl_i64(rt_load_i64(memory_at_0, loc_4 + 8), rt_i64_from_u32(4, 0)), rt_shr_u64(loc_2, rt_i64_from_u32(60, 0)))
			while true do
				loc_2 = rt_bit_or_i64(rt_extend_i64_u32(bit_and((if loc_3 ~= loc_10 then 1 else 0), (if rt_ne_i64(rt_bit_or_i64(rt_load_i64(memory_at_0, loc_4 + 16), rt_load_i64(memory_at_0, loc_4 + 24)), rt_i64_ZERO) then 1 else 0))), rt_bit_and_i64(loc_2, rt_i64_from_u32(4294967295, 268435455)))
				if rt_ge_u64(loc_2, rt_i64_from_u32(1, 134217728)) then
					while true do
						loc_0 = rt_add_i64(loc_0, rt_i64_ONE)
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				if rt_ne_i64(loc_2, rt_i64_from_u32(0, 134217728)) then
					break
				end
				loc_0 = rt_add_i64(rt_bit_and_i64(loc_0, rt_i64_ONE), loc_0)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_3 = (if rt_gt_u64(loc_0, rt_i64_from_u32(4294967295, 1048575)) then 1 else 0)
			loc_5 = (if loc_3 ~= 0 then rt_bit_xor_i64(loc_0, rt_i64_from_u32(0, 1048576)) else loc_0)
			reg_0 = rt_extend_i64_u32(loc_3)
			break
		end
		loc_6 = reg_0
		GLOBAL_LIST[0].value = rt_add_i32(loc_4, 32)
		reg_0 = rt_reinterpret_f64_i64(rt_bit_or_i64(rt_bit_or_i64(rt_bit_and_i64(loc_1, rt_i64_from_u32(0, 2147483648)), rt_shl_i64(loc_6, rt_i64_from_u32(52, 0))), loc_5))
		break
	end
	return reg_0
end
FUNC_LIST[260] = --[[ __fwritex ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local reg_0
	local desired
	while true do
		while true do
			loc_3 = rt_load_i32(memory_at_0, loc_2 + 16)
			if loc_3 == 0 then
				while true do
					reg_0 = FUNC_LIST[248](loc_2)
					if reg_0 ~= 0 then
						desired = 1
						break
					end
					loc_3 = rt_load_i32(memory_at_0, loc_2 + 16)
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_4 = rt_load_i32(memory_at_0, loc_2 + 20)
			if loc_1 > rt_sub_i32(loc_3, loc_4) then
				while true do
					reg_0 = TABLE_LIST[0].data[rt_load_i32(memory_at_0, loc_2 + 36)](loc_2, loc_0, loc_1)
					desired = 0
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			while true do
				while true do
					if rt_lt_i32(rt_load_i32(memory_at_0, loc_2 + 80), 0) then
						break
					end
					if loc_1 == 0 then
						break
					end
					loc_3 = loc_1
					while true do
						loc_5 = rt_add_i32(loc_0, loc_3)
						if rt_load_i32_u8(memory_at_0, rt_sub_i32(loc_5, 1)) ~= 10 then
							while true do
								loc_3 = rt_sub_i32(loc_3, 1)
								if loc_3 ~= 0 then
									desired = 4
									break
								end
								desired = 3
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
									continue
								end
								break
							end
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					reg_0 = TABLE_LIST[0].data[rt_load_i32(memory_at_0, loc_2 + 36)](loc_2, loc_0, loc_3)
					loc_4 = reg_0
					if loc_4 < loc_3 then
						desired = 1
						break
					end
					loc_1 = rt_sub_i32(loc_1, loc_3)
					loc_4 = rt_load_i32(memory_at_0, loc_2 + 20)
					desired = 2
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_5 = loc_0
				loc_3 = 0
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			reg_0 = FUNC_LIST[188](loc_4, loc_5, loc_1)
			rt_store_i32(memory_at_0, loc_2 + 20, rt_add_i32(rt_load_i32(memory_at_0, loc_2 + 20), loc_1))
			loc_4 = rt_add_i32(loc_1, loc_3)
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		reg_0 = loc_4
		break
	end
	return reg_0
end
FUNC_LIST[261] = --[[ __vfprintf_internal ]] function(loc_0, loc_1, loc_2, loc_3, loc_4)
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local loc_8 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_5 = rt_sub_i32(GLOBAL_LIST[0].value, 208)
		GLOBAL_LIST[0].value = loc_5
		rt_store_i32(memory_at_0, loc_5 + 204, loc_2)
		reg_0 = FUNC_LIST[219](rt_add_i32(loc_5, 160), 0, 40)
		rt_store_i32(memory_at_0, loc_5 + 200, rt_load_i32(memory_at_0, loc_5 + 204))
		while true do
			reg_0 = FUNC_LIST[262](0, loc_1, rt_add_i32(loc_5, 200), rt_add_i32(loc_5, 80), rt_add_i32(loc_5, 160), loc_3, loc_4)
			if rt_lt_i32(reg_0, 0) then
				while true do
					loc_4 = 4294967295
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			if rt_lt_i32(rt_load_i32(memory_at_0, loc_0 + 76), 0) then
				while true do
					reg_0 = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			else
				while true do
					reg_0 = FUNC_LIST[246](loc_0)
					reg_0 = (if reg_0 == 0 then 1 else 0)
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_7 = reg_0
			loc_8 = rt_load_i32(memory_at_0, loc_0)
			rt_store_i32(memory_at_0, loc_0, bit_and(loc_8, 4294967263))
			while true do
				while true do
					while true do
						if rt_load_i32(memory_at_0, loc_0 + 48) == 0 then
							while true do
								rt_store_i32(memory_at_0, loc_0 + 48, 80)
								rt_store_i32(memory_at_0, loc_0 + 28, 0)
								rt_store_i64(memory_at_0, loc_0 + 16, rt_i64_ZERO)
								loc_6 = rt_load_i32(memory_at_0, loc_0 + 44)
								rt_store_i32(memory_at_0, loc_0 + 44, loc_5)
								desired = 4
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						if rt_load_i32(memory_at_0, loc_0 + 16) ~= 0 then
							desired = 3
							break
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					loc_2 = 4294967295
					reg_1 = FUNC_LIST[248](loc_0)
					reg_0 = loc_2
					if reg_1 ~= 0 then
						desired = 2
						break
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				reg_0 = FUNC_LIST[262](loc_0, loc_1, rt_add_i32(loc_5, 200), rt_add_i32(loc_5, 80), rt_add_i32(loc_5, 160), loc_3, loc_4)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_2 = reg_0
			loc_4 = bit_and(loc_8, 32)
			if loc_6 ~= 0 then
				while true do
					reg_0 = TABLE_LIST[0].data[rt_load_i32(memory_at_0, loc_0 + 36)](loc_0, 0, 0)
					rt_store_i32(memory_at_0, loc_0 + 48, 0)
					rt_store_i32(memory_at_0, loc_0 + 44, loc_6)
					rt_store_i32(memory_at_0, loc_0 + 28, 0)
					loc_3 = rt_load_i32(memory_at_0, loc_0 + 20)
					rt_store_i64(memory_at_0, loc_0 + 16, rt_i64_ZERO)
					loc_2 = (if loc_3 ~= 0 then loc_2 else 4294967295)
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_3 = rt_load_i32(memory_at_0, loc_0)
			rt_store_i32(memory_at_0, loc_0, bit_or(loc_3, loc_4))
			loc_4 = (if bit_and(loc_3, 32) ~= 0 then 4294967295 else loc_2)
			if loc_7 ~= 0 then
				break
			end
			FUNC_LIST[247](loc_0)
			break
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_5, 208)
		reg_0 = loc_4
		break
	end
	return reg_0
end
FUNC_LIST[262] = --[[ printf_core ]] function(loc_0, loc_1, loc_2, loc_3, loc_4, loc_5, loc_6)
	local loc_7 = 0
	local loc_8 = 0
	local loc_9 = 0
	local loc_10 = 0
	local loc_11 = 0
	local loc_12 = 0
	local loc_13 = 0
	local loc_14 = 0
	local loc_15 = 0
	local loc_16 = 0
	local loc_17 = 0
	local loc_18 = 0
	local loc_19 = rt_i64_ZERO
	local loc_20 = 0
	local loc_21 = 0
	local loc_22 = 0
	local loc_23 = 0
	local loc_24 = 0
	local loc_25 = 0
	local reg_0
	local desired
	local br_map = {}
	while true do
		loc_8 = rt_add_i32(GLOBAL_LIST[0].value, 4294967232)
		GLOBAL_LIST[0].value = loc_8
		rt_store_i32(memory_at_0, loc_8 + 60, loc_1)
		loc_25 = rt_add_i32(loc_8, 39)
		loc_18 = rt_add_i32(loc_8, 40)
		while true do
			while true do
				while true do
					while true do
						while true do
							loc_7 = 0
							while true do
								loc_14 = loc_1
								if rt_gt_i32(loc_7, bit_xor(loc_13, 2147483647)) then
									desired = 4
									break
								end
								loc_13 = rt_add_i32(loc_7, loc_13)
								while true do
									while true do
										while true do
											while true do
												while true do
													loc_7 = loc_1
													loc_12 = rt_load_i32_u8(memory_at_0, loc_7)
													if loc_12 ~= 0 then
														while true do
															while true do
																while true do
																	while true do
																		loc_12 = bit_and(loc_12, 255)
																		if loc_12 == 0 then
																			while true do
																				loc_1 = loc_7
																				desired = 15
																				break
																			end
																			if desired then
																				if desired == 15 then
																					desired = nil
																				end
																				break
																			end
																		end
																		if loc_12 ~= 37 then
																			desired = 14
																			break
																		end
																		loc_12 = loc_7
																		while true do
																			if rt_load_i32_u8(memory_at_0, loc_12 + 1) ~= 37 then
																				while true do
																					loc_1 = loc_12
																					desired = 15
																					break
																				end
																				if desired then
																					if desired == 16 then
																						desired = nil
																						continue
																					end
																					break
																				end
																			end
																			loc_7 = rt_add_i32(loc_7, 1)
																			loc_9 = rt_load_i32_u8(memory_at_0, loc_12 + 2)
																			loc_1 = rt_add_i32(loc_12, 2)
																			loc_12 = loc_1
																			if loc_9 == 37 then
																				continue
																			end
																			break
																		end
																		if desired then
																			if desired == 15 then
																				desired = nil
																			end
																			break
																		end
																		break
																	end
																	if desired then
																		if desired == 14 then
																			desired = nil
																		end
																		break
																	end
																	loc_7 = rt_sub_i32(loc_7, loc_14)
																	loc_12 = bit_xor(loc_13, 2147483647)
																	if rt_gt_i32(loc_7, loc_12) then
																		desired = 4
																		break
																	end
																	if loc_0 ~= 0 then
																		while true do
																			FUNC_LIST[263](loc_0, loc_14, loc_7)
																			break
																		end
																		if desired then
																			if desired == 14 then
																				desired = nil
																			end
																			break
																		end
																	end
																	if loc_7 ~= 0 then
																		desired = 6
																		break
																	end
																	rt_store_i32(memory_at_0, loc_8 + 60, loc_1)
																	loc_7 = rt_add_i32(loc_1, 1)
																	loc_15 = 4294967295
																	while true do
																		loc_9 = rt_sub_i32(rt_load_i32_i8(memory_at_0, loc_1 + 1), 48)
																		if loc_9 > 9 then
																			break
																		end
																		if rt_load_i32_u8(memory_at_0, loc_1 + 2) ~= 36 then
																			break
																		end
																		loc_7 = rt_add_i32(loc_1, 3)
																		loc_20 = 1
																		loc_15 = loc_9
																		break
																	end
																	if desired then
																		if desired == 14 then
																			desired = nil
																		end
																		break
																	end
																	rt_store_i32(memory_at_0, loc_8 + 60, loc_7)
																	loc_11 = 0
																	while true do
																		loc_21 = rt_load_i32_i8(memory_at_0, loc_7)
																		loc_1 = rt_sub_i32(loc_21, 32)
																		if loc_1 > 31 then
																			while true do
																				loc_9 = loc_7
																				desired = 15
																				break
																			end
																			if desired then
																				if desired == 15 then
																					desired = nil
																				end
																				break
																			end
																		end
																		loc_9 = loc_7
																		loc_1 = rt_shl_i32(1, loc_1)
																		if bit_and(loc_1, 75913) == 0 then
																			break
																		end
																		while true do
																			loc_9 = rt_add_i32(loc_7, 1)
																			rt_store_i32(memory_at_0, loc_8 + 60, loc_9)
																			loc_11 = bit_or(loc_1, loc_11)
																			loc_21 = rt_load_i32_i8(memory_at_0, loc_7 + 1)
																			loc_1 = rt_sub_i32(loc_21, 32)
																			if loc_1 >= 32 then
																				desired = 15
																				break
																			end
																			loc_7 = loc_9
																			loc_1 = rt_shl_i32(1, loc_1)
																			if bit_and(loc_1, 75913) ~= 0 then
																				continue
																			end
																			break
																		end
																		if desired then
																			if desired == 15 then
																				desired = nil
																			end
																			break
																		end
																		break
																	end
																	if desired then
																		if desired == 14 then
																			desired = nil
																		end
																		break
																	end
																	while true do
																		if loc_21 == 42 then
																			while true do
																				while true do
																					while true do
																						loc_7 = rt_sub_i32(rt_load_i32_i8(memory_at_0, loc_9 + 1), 48)
																						if loc_7 > 9 then
																							break
																						end
																						if rt_load_i32_u8(memory_at_0, loc_9 + 2) ~= 36 then
																							break
																						end
																						loc_1 = rt_add_i32(loc_9, 3)
																						loc_20 = 1
																						while true do
																							if loc_0 == 0 then
																								while true do
																									rt_store_i32(memory_at_0, rt_add_i32(loc_4, rt_shl_i32(loc_7, 2)), 10)
																									reg_0 = 0
																									desired = 19
																									break
																								end
																								if desired then
																									if desired == 19 then
																										desired = nil
																									end
																									break
																								end
																							end
																							reg_0 = rt_load_i32(memory_at_0, rt_add_i32(loc_3, rt_shl_i32(loc_7, 3)))
																							break
																						end
																						if desired then
																							if desired == 18 then
																								desired = nil
																							end
																							break
																						end
																						desired = 17
																						break
																					end
																					if desired then
																						if desired == 17 then
																							desired = nil
																						end
																						break
																					end
																					if loc_20 ~= 0 then
																						desired = 11
																						break
																					end
																					loc_1 = rt_add_i32(loc_9, 1)
																					if loc_0 == 0 then
																						while true do
																							rt_store_i32(memory_at_0, loc_8 + 60, loc_1)
																							loc_20 = 0
																							loc_17 = 0
																							desired = 15
																							break
																						end
																						if desired then
																							if desired == 17 then
																								desired = nil
																							end
																							break
																						end
																					end
																					loc_7 = rt_load_i32(memory_at_0, loc_2)
																					rt_store_i32(memory_at_0, loc_2, rt_add_i32(loc_7, 4))
																					loc_20 = 0
																					reg_0 = rt_load_i32(memory_at_0, loc_7)
																					break
																				end
																				if desired then
																					break
																				end
																				loc_17 = reg_0
																				rt_store_i32(memory_at_0, loc_8 + 60, loc_1)
																				if rt_ge_i32(loc_17, 0) then
																					desired = 15
																					break
																				end
																				loc_17 = rt_sub_i32(0, loc_17)
																				loc_11 = bit_or(loc_11, 8192)
																				desired = 15
																				break
																			end
																			if desired then
																				if desired == 15 then
																					desired = nil
																				end
																				break
																			end
																		end
																		reg_0 = FUNC_LIST[264](rt_add_i32(loc_8, 60))
																		loc_17 = reg_0
																		if rt_lt_i32(loc_17, 0) then
																			desired = 4
																			break
																		end
																		loc_1 = rt_load_i32(memory_at_0, loc_8 + 60)
																		break
																	end
																	if desired then
																		if desired == 14 then
																			desired = nil
																		end
																		break
																	end
																	loc_7 = 0
																	loc_10 = 4294967295
																	while true do
																		reg_0 = 0
																		if rt_load_i32_u8(memory_at_0, loc_1) ~= 46 then
																			break
																		end
																		if rt_load_i32_u8(memory_at_0, loc_1 + 1) == 42 then
																			while true do
																				while true do
																					while true do
																						loc_9 = rt_sub_i32(rt_load_i32_i8(memory_at_0, loc_1 + 2), 48)
																						if loc_9 > 9 then
																							break
																						end
																						if rt_load_i32_u8(memory_at_0, loc_1 + 3) ~= 36 then
																							break
																						end
																						loc_1 = rt_add_i32(loc_1, 4)
																						while true do
																							if loc_0 == 0 then
																								while true do
																									rt_store_i32(memory_at_0, rt_add_i32(loc_4, rt_shl_i32(loc_9, 2)), 10)
																									reg_0 = 0
																									desired = 19
																									break
																								end
																								if desired then
																									if desired == 19 then
																										desired = nil
																									end
																									break
																								end
																							end
																							reg_0 = rt_load_i32(memory_at_0, rt_add_i32(loc_3, rt_shl_i32(loc_9, 3)))
																							break
																						end
																						if desired then
																							if desired == 18 then
																								desired = nil
																							end
																							break
																						end
																						desired = 17
																						break
																					end
																					if desired then
																						if desired == 17 then
																							desired = nil
																						end
																						break
																					end
																					if loc_20 ~= 0 then
																						desired = 11
																						break
																					end
																					loc_1 = rt_add_i32(loc_1, 2)
																					reg_0 = 0
																					if loc_0 == 0 then
																						break
																					end
																					loc_9 = rt_load_i32(memory_at_0, loc_2)
																					rt_store_i32(memory_at_0, loc_2, rt_add_i32(loc_9, 4))
																					reg_0 = rt_load_i32(memory_at_0, loc_9)
																					break
																				end
																				if desired then
																					break
																				end
																				loc_10 = reg_0
																				rt_store_i32(memory_at_0, loc_8 + 60, loc_1)
																				reg_0 = (if rt_ge_i32(loc_10, 0) then 1 else 0)
																				desired = 15
																				break
																			end
																			if desired then
																				if desired == 15 then
																					desired = nil
																				end
																				break
																			end
																		end
																		rt_store_i32(memory_at_0, loc_8 + 60, rt_add_i32(loc_1, 1))
																		reg_0 = FUNC_LIST[264](rt_add_i32(loc_8, 60))
																		loc_10 = reg_0
																		loc_1 = rt_load_i32(memory_at_0, loc_8 + 60)
																		reg_0 = 1
																		break
																	end
																	if desired then
																		if desired == 14 then
																			desired = nil
																		end
																		break
																	end
																	loc_23 = reg_0
																	while true do
																		loc_9 = loc_7
																		loc_16 = 28
																		loc_21 = loc_1
																		loc_7 = rt_load_i32_i8(memory_at_0, loc_21)
																		if rt_sub_i32(loc_7, 123) < 4294967238 then
																			desired = 3
																			break
																		end
																		loc_1 = rt_add_i32(loc_1, 1)
																		loc_7 = rt_load_i32_u8(memory_at_0, rt_add_i32(rt_add_i32(loc_7, rt_mul_i32(loc_9, 58)), 8431))
																		if rt_sub_i32(loc_7, 1) < 8 then
																			continue
																		end
																		break
																	end
																	if desired then
																		if desired == 14 then
																			desired = nil
																		end
																		break
																	end
																	rt_store_i32(memory_at_0, loc_8 + 60, loc_1)
																	while true do
																		if loc_7 ~= 27 then
																			while true do
																				if loc_7 == 0 then
																					desired = 3
																					break
																				end
																				if rt_ge_i32(loc_15, 0) then
																					while true do
																						if loc_0 == 0 then
																							while true do
																								rt_store_i32(memory_at_0, rt_add_i32(loc_4, rt_shl_i32(loc_15, 2)), loc_7)
																								desired = 5
																								break
																							end
																							if desired then
																								break
																							end
																						end
																						rt_store_i64(memory_at_0, loc_8 + 48, rt_load_i64(memory_at_0, rt_add_i32(loc_3, rt_shl_i32(loc_15, 3))))
																						desired = 15
																						break
																					end
																					if desired then
																						break
																					end
																				end
																				if loc_0 == 0 then
																					desired = 7
																					break
																				end
																				FUNC_LIST[265](rt_add_i32(loc_8, 48), loc_7, loc_2, loc_6)
																				desired = 15
																				break
																			end
																			if desired then
																				if desired == 15 then
																					desired = nil
																				end
																				break
																			end
																		end
																		if rt_ge_i32(loc_15, 0) then
																			desired = 3
																			break
																		end
																		loc_7 = 0
																		if loc_0 == 0 then
																			desired = 6
																			break
																		end
																		break
																	end
																	if desired then
																		if desired == 14 then
																			desired = nil
																		end
																		break
																	end
																	if bit_and(rt_load_i32_u8(memory_at_0, loc_0), 32) ~= 0 then
																		desired = 2
																		break
																	end
																	loc_24 = bit_and(loc_11, 4294901759)
																	loc_11 = (if bit_and(loc_11, 8192) ~= 0 then loc_24 else loc_11)
																	loc_15 = 0
																	loc_22 = 1024
																	loc_16 = loc_18
																	while true do
																		while true do
																			while true do
																				while true do
																					while true do
																						while true do
																							while true do
																								while true do
																									while true do
																										while true do
																											while true do
																												while true do
																													while true do
																														while true do
																															while true do
																																while true do
																																	while true do
																																		loc_7 = rt_load_i32_i8(memory_at_0, loc_21)
																																		loc_7 = (if loc_9 ~= 0 then (if bit_and(loc_7, 15) == 3 then bit_and(loc_7, 4294967251) else loc_7) else loc_7)
																																		if not br_map[1] then
																																			br_map[1] = (function()
																																				return { [0] = 4, 23, 23, 23, 23, 23, 23, 23, 23, 16, 23, 9, 6, 16, 16, 16, 23, 6, 23, 23, 23, 23, 2, 5, 3, 23, 23, 10, 23, 1, 23, 23, 4, }
																																			end)()
																																		end
																																		temp = br_map[1][rt_sub_i32(loc_7, 88)] or 0
																																		if temp < 5 then
																																			if temp < 2 then
																																				if temp < 1 then
																																					break
																																				else
																																					desired = 30
																																					break
																																				end
																																			elseif temp > 2 then
																																				if temp < 4 then
																																					desired = 28
																																					break
																																				else
																																					desired = 27
																																					break
																																				end
																																			else
																																				desired = 29
																																				break
																																			end
																																		elseif temp > 5 then
																																			if temp < 10 then
																																				if temp < 9 then
																																					desired = 25
																																					break
																																				else
																																					desired = 22
																																					break
																																				end
																																			elseif temp > 10 then
																																				if temp < 23 then
																																					desired = 15
																																					break
																																				else
																																					desired = 8
																																					break
																																				end
																																			else
																																				desired = 21
																																				break
																																			end
																																		else
																																			desired = 26
																																			break
																																		end
																																	end
																																	if desired then
																																		if desired == 30 then
																																			desired = nil
																																		end
																																		break
																																	end
																																	while true do
																																		if not br_map[2] then
																																			br_map[2] = (function()
																																				return { [0] = 16, 23, 11, 23, 16, 16, 16, }
																																			end)()
																																		end
																																		temp = br_map[2][rt_sub_i32(loc_7, 65)] or 0
																																		if temp < 16 then
																																			if temp < 11 then
																																				break
																																			else
																																				desired = 20
																																				break
																																			end
																																		elseif temp > 16 then
																																			desired = 8
																																			break
																																		else
																																			desired = 15
																																			break
																																		end
																																	end
																																	if desired then
																																		if desired == 30 then
																																			desired = nil
																																		end
																																		break
																																	end
																																	if loc_7 == 83 then
																																		desired = 19
																																		break
																																	end
																																	desired = 9
																																	break
																																end
																																if desired then
																																	if desired == 29 then
																																		desired = nil
																																	end
																																	break
																																end
																																loc_19 = rt_load_i64(memory_at_0, loc_8 + 48)
																																reg_0 = 1024
																																desired = 24
																																break
																															end
																															if desired then
																																if desired == 28 then
																																	desired = nil
																																end
																																break
																															end
																															loc_7 = 0
																															while true do
																																while true do
																																	while true do
																																		while true do
																																			while true do
																																				while true do
																																					while true do
																																						if not br_map[3] then
																																							br_map[3] = (function()
																																								return { [0] = 0, 1, 2, 3, 4, 29, 5, 6, }
																																							end)()
																																						end
																																						temp = br_map[3][bit_and(loc_9, 255)] or 29
																																						if temp < 4 then
																																							if temp < 2 then
																																								if temp < 1 then
																																									break
																																								else
																																									desired = 34
																																									break
																																								end
																																							elseif temp > 2 then
																																								desired = 32
																																								break
																																							else
																																								desired = 33
																																								break
																																							end
																																						elseif temp > 4 then
																																							if temp < 6 then
																																								desired = 30
																																								break
																																							elseif temp > 6 then
																																								desired = 6
																																								break
																																							else
																																								desired = 29
																																								break
																																							end
																																						else
																																							desired = 31
																																							break
																																						end
																																					end
																																					if desired then
																																						if desired == 34 then
																																							desired = nil
																																						end
																																						break
																																					end
																																					rt_store_i32(memory_at_0, rt_load_i32(memory_at_0, loc_8 + 48), loc_13)
																																					desired = 6
																																					break
																																				end
																																				if desired then
																																					if desired == 33 then
																																						desired = nil
																																					end
																																					break
																																				end
																																				rt_store_i32(memory_at_0, rt_load_i32(memory_at_0, loc_8 + 48), loc_13)
																																				desired = 6
																																				break
																																			end
																																			if desired then
																																				if desired == 32 then
																																					desired = nil
																																				end
																																				break
																																			end
																																			rt_store_i64(memory_at_0, rt_load_i32(memory_at_0, loc_8 + 48), rt_extend_i64_i32(loc_13))
																																			desired = 6
																																			break
																																		end
																																		if desired then
																																			if desired == 31 then
																																				desired = nil
																																			end
																																			break
																																		end
																																		rt_store_i32_n16(memory_at_0, rt_load_i32(memory_at_0, loc_8 + 48), loc_13)
																																		desired = 6
																																		break
																																	end
																																	if desired then
																																		if desired == 30 then
																																			desired = nil
																																		end
																																		break
																																	end
																																	rt_store_i32_n8(memory_at_0, rt_load_i32(memory_at_0, loc_8 + 48), loc_13)
																																	desired = 6
																																	break
																																end
																																if desired then
																																	if desired == 29 then
																																		desired = nil
																																	end
																																	break
																																end
																																rt_store_i32(memory_at_0, rt_load_i32(memory_at_0, loc_8 + 48), loc_13)
																																desired = 6
																																break
																															end
																															if desired then
																																if desired == 28 then
																																	desired = nil
																																end
																																break
																															end
																															rt_store_i64(memory_at_0, rt_load_i32(memory_at_0, loc_8 + 48), rt_extend_i64_i32(loc_13))
																															desired = 6
																															break
																														end
																														if desired then
																															if desired == 27 then
																																desired = nil
																															end
																															break
																														end
																														loc_10 = (if loc_10 <= 8 then 8 else loc_10)
																														loc_11 = bit_or(loc_11, 8)
																														loc_7 = 120
																														break
																													end
																													if desired then
																														if desired == 26 then
																															desired = nil
																														end
																														break
																													end
																													reg_0 = FUNC_LIST[266](rt_load_i64(memory_at_0, loc_8 + 48), loc_18, bit_and(loc_7, 32))
																													loc_14 = reg_0
																													if rt_eq_i64(rt_load_i64(memory_at_0, loc_8 + 48), rt_i64_ZERO) then
																														desired = 23
																														break
																													end
																													if bit_and(loc_11, 8) == 0 then
																														desired = 23
																														break
																													end
																													loc_22 = rt_add_i32(rt_shr_u32(loc_7, 4), 1024)
																													loc_15 = 2
																													desired = 23
																													break
																												end
																												if desired then
																													if desired == 25 then
																														desired = nil
																													end
																													break
																												end
																												reg_0 = FUNC_LIST[267](rt_load_i64(memory_at_0, loc_8 + 48), loc_18)
																												loc_14 = reg_0
																												if bit_and(loc_11, 8) == 0 then
																													desired = 23
																													break
																												end
																												loc_7 = rt_sub_i32(loc_18, loc_14)
																												loc_10 = (if rt_lt_i32(loc_7, loc_10) then loc_10 else rt_add_i32(loc_7, 1))
																												desired = 23
																												break
																											end
																											if desired then
																												if desired == 24 then
																													desired = nil
																												end
																												break
																											end
																											loc_19 = rt_load_i64(memory_at_0, loc_8 + 48)
																											if rt_lt_i64(loc_19, rt_i64_ZERO) then
																												while true do
																													loc_19 = rt_sub_i64(rt_i64_ZERO, loc_19)
																													rt_store_i64(memory_at_0, loc_8 + 48, loc_19)
																													loc_15 = 1
																													reg_0 = 1024
																													desired = 24
																													break
																												end
																												if desired then
																													if desired == 24 then
																														desired = nil
																													end
																													break
																												end
																											end
																											if bit_and(loc_11, 2048) ~= 0 then
																												while true do
																													loc_15 = 1
																													reg_0 = 1025
																													desired = 24
																													break
																												end
																												if desired then
																													if desired == 24 then
																														desired = nil
																													end
																													break
																												end
																											end
																											loc_15 = bit_and(loc_11, 1)
																											reg_0 = (if loc_15 ~= 0 then 1026 else 1024)
																											break
																										end
																										if desired then
																											if desired == 23 then
																												desired = nil
																											end
																											break
																										end
																										loc_22 = reg_0
																										reg_0 = FUNC_LIST[268](loc_19, loc_18)
																										loc_14 = reg_0
																										break
																									end
																									if desired then
																										if desired == 22 then
																											desired = nil
																										end
																										break
																									end
																									if bit_and(loc_23, (if rt_lt_i32(loc_10, 0) then 1 else 0)) ~= 0 then
																										desired = 4
																										break
																									end
																									loc_11 = (if loc_23 ~= 0 then bit_and(loc_11, 4294901759) else loc_11)
																									while true do
																										loc_19 = rt_load_i64(memory_at_0, loc_8 + 48)
																										if rt_ne_i64(loc_19, rt_i64_ZERO) then
																											break
																										end
																										if loc_10 ~= 0 then
																											break
																										end
																										loc_14 = loc_18
																										loc_10 = 0
																										desired = 8
																										break
																									end
																									if desired then
																										if desired == 22 then
																											desired = nil
																										end
																										break
																									end
																									loc_7 = rt_add_i32((if rt_eq_i64(loc_19, rt_i64_ZERO) then 1 else 0), rt_sub_i32(loc_18, loc_14))
																									loc_10 = (if rt_lt_i32(loc_7, loc_10) then loc_10 else loc_7)
																									desired = 9
																									break
																								end
																								if desired then
																									if desired == 21 then
																										desired = nil
																									end
																									break
																								end
																								loc_19 = rt_load_i64(memory_at_0, loc_8 + 48)
																								desired = 10
																								break
																							end
																							if desired then
																								if desired == 20 then
																									desired = nil
																								end
																								break
																							end
																							loc_7 = rt_load_i32(memory_at_0, loc_8 + 48)
																							loc_14 = (if loc_7 ~= 0 then loc_7 else 2268)
																							reg_0 = FUNC_LIST[249](loc_14, (if loc_10 >= 2147483647 then 2147483647 else loc_10))
																							loc_7 = reg_0
																							loc_16 = rt_add_i32(loc_7, loc_14)
																							if rt_ge_i32(loc_10, 0) then
																								while true do
																									loc_11 = loc_24
																									loc_10 = loc_7
																									desired = 8
																									break
																								end
																								if desired then
																									if desired == 20 then
																										desired = nil
																									end
																									break
																								end
																							end
																							loc_11 = loc_24
																							loc_10 = loc_7
																							if rt_load_i32_u8(memory_at_0, loc_16) ~= 0 then
																								desired = 4
																								break
																							end
																							desired = 8
																							break
																						end
																						if desired then
																							if desired == 19 then
																								desired = nil
																							end
																							break
																						end
																						loc_19 = rt_load_i64(memory_at_0, loc_8 + 48)
																						if rt_ne_i64(loc_19, rt_i64_ZERO) then
																							desired = 18
																							break
																						end
																						loc_19 = rt_i64_ZERO
																						desired = 10
																						break
																					end
																					if desired then
																						if desired == 18 then
																							desired = nil
																						end
																						break
																					end
																					if loc_10 ~= 0 then
																						while true do
																							reg_0 = rt_load_i32(memory_at_0, loc_8 + 48)
																							desired = 17
																							break
																						end
																						if desired then
																							if desired == 18 then
																								desired = nil
																							end
																							break
																						end
																					end
																					loc_7 = 0
																					FUNC_LIST[269](loc_0, 32, loc_17, 0, loc_11)
																					desired = 16
																					break
																				end
																				if desired then
																					if desired == 17 then
																						desired = nil
																					end
																					break
																				end
																				rt_store_i32(memory_at_0, loc_8 + 12, 0)
																				rt_store_i64_n32(memory_at_0, loc_8 + 8, loc_19)
																				rt_store_i32(memory_at_0, loc_8 + 48, rt_add_i32(loc_8, 8))
																				loc_10 = 4294967295
																				reg_0 = rt_add_i32(loc_8, 8)
																				break
																			end
																			if desired then
																				if desired == 16 then
																					desired = nil
																				end
																				break
																			end
																			loc_12 = reg_0
																			loc_7 = 0
																			while true do
																				while true do
																					loc_9 = rt_load_i32(memory_at_0, loc_12)
																					if loc_9 == 0 then
																						break
																					end
																					reg_0 = FUNC_LIST[255](rt_add_i32(loc_8, 4), loc_9)
																					loc_9 = reg_0
																					if rt_lt_i32(loc_9, 0) then
																						desired = 2
																						break
																					end
																					if loc_9 > rt_sub_i32(loc_10, loc_7) then
																						break
																					end
																					loc_12 = rt_add_i32(loc_12, 4)
																					loc_7 = rt_add_i32(loc_7, loc_9)
																					if loc_7 < loc_10 then
																						desired = 17
																						break
																					end
																					break
																				end
																				if desired then
																					if desired == 17 then
																						desired = nil
																						continue
																					end
																					break
																				end
																				break
																			end
																			if desired then
																				if desired == 16 then
																					desired = nil
																				end
																				break
																			end
																			loc_16 = 61
																			if rt_lt_i32(loc_7, 0) then
																				desired = 3
																				break
																			end
																			FUNC_LIST[269](loc_0, 32, loc_17, loc_7, loc_11)
																			if loc_7 == 0 then
																				while true do
																					loc_7 = 0
																					desired = 16
																					break
																				end
																				if desired then
																					if desired == 16 then
																						desired = nil
																					end
																					break
																				end
																			end
																			loc_9 = 0
																			loc_12 = rt_load_i32(memory_at_0, loc_8 + 48)
																			while true do
																				loc_14 = rt_load_i32(memory_at_0, loc_12)
																				if loc_14 == 0 then
																					desired = 16
																					break
																				end
																				reg_0 = FUNC_LIST[255](rt_add_i32(loc_8, 4), loc_14)
																				loc_14 = reg_0
																				loc_9 = rt_add_i32(loc_14, loc_9)
																				if loc_9 > loc_7 then
																					desired = 16
																					break
																				end
																				FUNC_LIST[263](loc_0, rt_add_i32(loc_8, 4), loc_14)
																				loc_12 = rt_add_i32(loc_12, 4)
																				if loc_7 > loc_9 then
																					continue
																				end
																				break
																			end
																			if desired then
																				if desired == 16 then
																					desired = nil
																				end
																				break
																			end
																			break
																		end
																		if desired then
																			if desired == 15 then
																				desired = nil
																			end
																			break
																		end
																		FUNC_LIST[269](loc_0, 32, loc_17, loc_7, bit_xor(loc_11, 8192))
																		loc_7 = (if rt_lt_i32(loc_7, loc_17) then loc_17 else loc_7)
																		desired = 6
																		break
																	end
																	if desired then
																		if desired == 14 then
																			desired = nil
																		end
																		break
																	end
																	if bit_and(loc_23, (if rt_lt_i32(loc_10, 0) then 1 else 0)) ~= 0 then
																		desired = 4
																		break
																	end
																	loc_16 = 61
																	reg_0 = TABLE_LIST[0].data[loc_5](loc_0, rt_load_f64(memory_at_0, loc_8 + 48), loc_17, loc_10, loc_11, loc_7)
																	loc_7 = reg_0
																	if rt_ge_i32(loc_7, 0) then
																		desired = 6
																		break
																	end
																	desired = 3
																	break
																end
																if desired then
																	if desired == 13 then
																		desired = nil
																		continue
																	end
																	break
																end
																loc_12 = rt_load_i32_u8(memory_at_0, loc_7 + 1)
																loc_7 = rt_add_i32(loc_7, 1)
																continue
															end
															if desired then
																break
															end
															error("out of code bounds")
														end
														if desired then
															if desired == 11 then
																desired = nil
															end
															break
														end
													end
													if loc_0 ~= 0 then
														desired = 1
														break
													end
													if loc_20 == 0 then
														desired = 7
														break
													end
													loc_7 = 1
													while true do
														loc_12 = rt_load_i32(memory_at_0, rt_add_i32(loc_4, rt_shl_i32(loc_7, 2)))
														if loc_12 ~= 0 then
															while true do
																FUNC_LIST[265](rt_add_i32(loc_3, rt_shl_i32(loc_7, 3)), loc_12, loc_2, loc_6)
																loc_13 = 1
																loc_7 = rt_add_i32(loc_7, 1)
																if loc_7 ~= 10 then
																	desired = 12
																	break
																end
																desired = 1
																break
															end
															if desired then
																if desired == 12 then
																	desired = nil
																	continue
																end
																break
															end
														end
														break
													end
													if desired then
														if desired == 11 then
															desired = nil
														end
														break
													end
													loc_13 = 1
													if loc_7 >= 10 then
														desired = 1
														break
													end
													while true do
														if rt_load_i32(memory_at_0, rt_add_i32(loc_4, rt_shl_i32(loc_7, 2))) ~= 0 then
															desired = 11
															break
														end
														loc_7 = rt_add_i32(loc_7, 1)
														if loc_7 ~= 10 then
															continue
														end
														break
													end
													if desired then
														if desired == 11 then
															desired = nil
														end
														break
													end
													desired = 1
													break
												end
												if desired then
													if desired == 10 then
														desired = nil
													end
													break
												end
												loc_16 = 28
												desired = 3
												break
											end
											if desired then
												if desired == 9 then
													desired = nil
												end
												break
											end
											rt_store_i64_n8(memory_at_0, loc_8 + 39, loc_19)
											loc_10 = 1
											loc_14 = loc_25
											loc_11 = loc_24
											desired = 8
											break
										end
										if desired then
											if desired == 8 then
												desired = nil
											end
											break
										end
										break
									end
									if desired then
										if desired == 7 then
											desired = nil
										end
										break
									end
									loc_1 = rt_sub_i32(loc_16, loc_14)
									loc_21 = (if rt_lt_i32(loc_1, loc_10) then loc_10 else loc_1)
									if rt_gt_i32(loc_21, bit_xor(loc_15, 2147483647)) then
										desired = 4
										break
									end
									loc_16 = 61
									loc_9 = rt_add_i32(loc_15, loc_21)
									loc_7 = (if rt_lt_i32(loc_9, loc_17) then loc_17 else loc_9)
									if rt_gt_i32(loc_7, loc_12) then
										desired = 3
										break
									end
									FUNC_LIST[269](loc_0, 32, loc_7, loc_9, loc_11)
									FUNC_LIST[263](loc_0, loc_22, loc_15)
									FUNC_LIST[269](loc_0, 48, loc_7, loc_9, bit_xor(loc_11, 65536))
									FUNC_LIST[269](loc_0, 48, loc_21, loc_1, 0)
									FUNC_LIST[263](loc_0, loc_14, loc_1)
									FUNC_LIST[269](loc_0, 32, loc_7, loc_9, bit_xor(loc_11, 8192))
									loc_1 = rt_load_i32(memory_at_0, loc_8 + 60)
									desired = 6
									break
								end
								if desired then
									if desired == 6 then
										desired = nil
										continue
									end
									break
								end
								break
							end
							if desired then
								if desired == 5 then
									desired = nil
									continue
								end
								break
							end
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						loc_13 = 0
						desired = 1
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					loc_16 = 61
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				reg_0 = FUNC_LIST[217]()
				rt_store_i32(memory_at_0, reg_0, loc_16)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_13 = 4294967295
			break
		end
		GLOBAL_LIST[0].value = rt_sub_i32(loc_8, 4294967232)
		reg_0 = loc_13
		break
	end
	return reg_0
end
FUNC_LIST[263] = --[[ out ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		if bit_and(rt_load_i32_u8(memory_at_0, loc_0), 32) == 0 then
			while true do
				reg_0 = FUNC_LIST[260](loc_1, loc_2, loc_0)
				break
			end
		end
		break
	end
end
FUNC_LIST[264] = --[[ getint ]] function(loc_0)
	local loc_1 = 0
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local reg_0
	local desired
	while true do
		loc_3 = rt_load_i32(memory_at_0, loc_0)
		loc_2 = rt_sub_i32(rt_load_i32_i8(memory_at_0, loc_3), 48)
		if loc_2 > 9 then
			while true do
				reg_0 = 0
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		while true do
			loc_4 = 4294967295
			if loc_1 <= 214748364 then
				while true do
					loc_1 = rt_mul_i32(loc_1, 10)
					loc_4 = (if loc_2 > bit_xor(loc_1, 2147483647) then 4294967295 else rt_add_i32(loc_2, loc_1))
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
						continue
					end
					break
				end
			end
			loc_2 = rt_add_i32(loc_3, 1)
			rt_store_i32(memory_at_0, loc_0, loc_2)
			loc_5 = rt_load_i32_i8(memory_at_0, loc_3 + 1)
			loc_1 = loc_4
			loc_3 = loc_2
			loc_2 = rt_sub_i32(loc_5, 48)
			if loc_2 < 10 then
				continue
			end
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[265] = --[[ pop_arg ]] function(loc_0, loc_1, loc_2, loc_3)
	local desired
	local br_map = {}
	while true do
		while true do
			while true do
				while true do
					while true do
						while true do
							while true do
								while true do
									while true do
										while true do
											while true do
												while true do
													while true do
														while true do
															if not br_map[1] then
																br_map[1] = (function()
																	return { [0] = 0, 10, 11, 12, 10, 11, 2, 3, 4, 5, 12, 11, 12, 12, 10, 11, 7, 8, }
																end)()
															end
															temp = br_map[1][rt_sub_i32(loc_1, 9)] or 9
															if temp < 7 then
																if temp < 3 then
																	if temp < 2 then
																		break
																	else
																		desired = 11
																		break
																	end
																elseif temp > 3 then
																	if temp < 5 then
																		desired = 9
																		break
																	else
																		desired = 8
																		break
																	end
																else
																	desired = 10
																	break
																end
															elseif temp > 7 then
																if temp < 10 then
																	if temp < 9 then
																		desired = 5
																		break
																	else
																		desired = 4
																		break
																	end
																elseif temp > 10 then
																	if temp < 12 then
																		desired = 2
																		break
																	else
																		desired = 1
																		break
																	end
																else
																	desired = 3
																	break
																end
															else
																desired = 6
																break
															end
														end
														if desired then
															break
														end
														loc_1 = rt_load_i32(memory_at_0, loc_2)
														rt_store_i32(memory_at_0, loc_2, rt_add_i32(loc_1, 4))
														rt_store_i32(memory_at_0, loc_0, rt_load_i32(memory_at_0, loc_1))
														desired = 0
														break
													end
													if desired then
														if desired == 11 then
															desired = nil
														end
														break
													end
													error("out of code bounds")
												end
												if desired then
													if desired == 10 then
														desired = nil
													end
													break
												end
												loc_1 = rt_load_i32(memory_at_0, loc_2)
												rt_store_i32(memory_at_0, loc_2, rt_add_i32(loc_1, 4))
												rt_store_i64(memory_at_0, loc_0, rt_load_i64_i16(memory_at_0, loc_1))
												desired = 0
												break
											end
											if desired then
												if desired == 9 then
													desired = nil
												end
												break
											end
											loc_1 = rt_load_i32(memory_at_0, loc_2)
											rt_store_i32(memory_at_0, loc_2, rt_add_i32(loc_1, 4))
											rt_store_i64(memory_at_0, loc_0, rt_load_i64_u16(memory_at_0, loc_1))
											desired = 0
											break
										end
										if desired then
											if desired == 8 then
												desired = nil
											end
											break
										end
										loc_1 = rt_load_i32(memory_at_0, loc_2)
										rt_store_i32(memory_at_0, loc_2, rt_add_i32(loc_1, 4))
										rt_store_i64(memory_at_0, loc_0, rt_load_i64_i8(memory_at_0, loc_1))
										desired = 0
										break
									end
									if desired then
										break
									end
									loc_1 = rt_load_i32(memory_at_0, loc_2)
									rt_store_i32(memory_at_0, loc_2, rt_add_i32(loc_1, 4))
									rt_store_i64(memory_at_0, loc_0, rt_load_i64_u8(memory_at_0, loc_1))
									desired = 0
									break
								end
								if desired then
									if desired == 6 then
										desired = nil
									end
									break
								end
								error("out of code bounds")
							end
							if desired then
								if desired == 5 then
									desired = nil
								end
								break
							end
							loc_1 = bit_and(rt_add_i32(rt_load_i32(memory_at_0, loc_2), 7), 4294967288)
							rt_store_i32(memory_at_0, loc_2, rt_add_i32(loc_1, 8))
							rt_store_f64(memory_at_0, loc_0, rt_load_f64(memory_at_0, loc_1))
							desired = 0
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						TABLE_LIST[0].data[loc_3](loc_0, loc_2)
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					desired = 0
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_1 = rt_load_i32(memory_at_0, loc_2)
				rt_store_i32(memory_at_0, loc_2, rt_add_i32(loc_1, 4))
				rt_store_i64(memory_at_0, loc_0, rt_load_i64_i32(memory_at_0, loc_1))
				desired = 0
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_1 = rt_load_i32(memory_at_0, loc_2)
			rt_store_i32(memory_at_0, loc_2, rt_add_i32(loc_1, 4))
			rt_store_i64(memory_at_0, loc_0, rt_load_i64_u32(memory_at_0, loc_1))
			desired = 0
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		loc_1 = bit_and(rt_add_i32(rt_load_i32(memory_at_0, loc_2), 7), 4294967288)
		rt_store_i32(memory_at_0, loc_2, rt_add_i32(loc_1, 8))
		rt_store_i64(memory_at_0, loc_0, rt_load_i64(memory_at_0, loc_1))
		break
	end
end
FUNC_LIST[266] = --[[ fmt_x ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	while true do
		if rt_ne_i64(loc_0, rt_i64_ZERO) then
			while true do
				while true do
					loc_1 = rt_sub_i32(loc_1, 1)
					rt_store_i32_n8(memory_at_0, loc_1, bit_or(rt_load_i32_u8(memory_at_0, rt_add_i32(bit_and(rt_wrap_i32_i64(loc_0), 15), 8960)), loc_2))
					loc_3 = (if rt_gt_u64(loc_0, rt_i64_from_u32(15, 0)) then 1 else 0)
					loc_0 = rt_shr_u64(loc_0, rt_i64_from_u32(4, 0))
					if loc_3 ~= 0 then
						continue
					end
					break
				end
				break
			end
		end
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[267] = --[[ fmt_o ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		if rt_ne_i64(loc_0, rt_i64_ZERO) then
			while true do
				while true do
					loc_1 = rt_sub_i32(loc_1, 1)
					rt_store_i32_n8(memory_at_0, loc_1, bit_or(bit_and(rt_wrap_i32_i64(loc_0), 7), 48))
					loc_2 = (if rt_gt_u64(loc_0, rt_i64_from_u32(7, 0)) then 1 else 0)
					loc_0 = rt_shr_u64(loc_0, rt_i64_from_u32(3, 0))
					if loc_2 ~= 0 then
						continue
					end
					break
				end
				break
			end
		end
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[268] = --[[ fmt_u ]] function(loc_0, loc_1)
	local loc_2 = 0
	local loc_3 = rt_i64_ZERO
	local loc_4 = 0
	local loc_5 = 0
	local reg_0
	local desired
	while true do
		while true do
			if rt_lt_u64(loc_0, rt_i64_from_u32(0, 1)) then
				while true do
					loc_3 = loc_0
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			while true do
				loc_1 = rt_sub_i32(loc_1, 1)
				loc_3 = rt_div_u64(loc_0, rt_i64_from_u32(10, 0))
				rt_store_i32_n8(memory_at_0, loc_1, bit_or(rt_wrap_i32_i64(rt_sub_i64(loc_0, rt_mul_i64(loc_3, rt_i64_from_u32(10, 0)))), 48))
				loc_2 = (if rt_gt_u64(loc_0, rt_i64_from_u32(4294967295, 9)) then 1 else 0)
				loc_0 = loc_3
				if loc_2 ~= 0 then
					continue
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			break
		end
		loc_2 = rt_wrap_i32_i64(loc_3)
		if loc_2 ~= 0 then
			while true do
				while true do
					loc_1 = rt_sub_i32(loc_1, 1)
					loc_4 = rt_div_u32(loc_2, 10)
					rt_store_i32_n8(memory_at_0, loc_1, bit_or(rt_sub_i32(loc_2, rt_mul_i32(loc_4, 10)), 48))
					loc_5 = (if loc_2 > 9 then 1 else 0)
					loc_2 = loc_4
					if loc_5 ~= 0 then
						continue
					end
					break
				end
				break
			end
		end
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[269] = --[[ pad ]] function(loc_0, loc_1, loc_2, loc_3, loc_4)
	local loc_5 = 0
	local reg_0
	while true do
		loc_5 = rt_sub_i32(GLOBAL_LIST[0].value, 256)
		GLOBAL_LIST[0].value = loc_5
		while true do
			if rt_le_i32(loc_2, loc_3) then
				break
			end
			if bit_and(loc_4, 73728) ~= 0 then
				break
			end
			loc_3 = rt_sub_i32(loc_2, loc_3)
			loc_2 = (if loc_3 < 256 then 1 else 0)
			reg_0 = FUNC_LIST[219](loc_5, loc_1, (if loc_2 ~= 0 then loc_3 else 256))
			if loc_2 == 0 then
				while true do
					while true do
						FUNC_LIST[263](loc_0, loc_5, 256)
						loc_3 = rt_sub_i32(loc_3, 256)
						if loc_3 > 255 then
							continue
						end
						break
					end
					break
				end
			end
			FUNC_LIST[263](loc_0, loc_5, loc_3)
			break
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_5, 256)
		break
	end
end
FUNC_LIST[270] = --[[ vfprintf ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[261](loc_0, loc_1, loc_2, 165, 166)
		break
	end
	return reg_0
end
FUNC_LIST[271] = --[[ fmt_fp ]] function(loc_0, loc_1, loc_2, loc_3, loc_4, loc_5)
	local loc_6 = 0
	local loc_7 = 0
	local loc_8 = 0
	local loc_9 = 0
	local loc_10 = 0
	local loc_11 = 0
	local loc_12 = 0
	local loc_13 = 0
	local loc_14 = 0
	local loc_15 = 0
	local loc_16 = 0
	local loc_17 = 0.0
	local loc_18 = 0
	local loc_19 = rt_i64_ZERO
	local loc_20 = 0
	local loc_21 = 0
	local loc_22 = 0
	local loc_23 = 0
	local loc_24 = 0
	local loc_25 = 0
	local loc_26 = rt_i64_ZERO
	local reg_0
	local reg_1
	local desired
	while true do
		loc_10 = rt_sub_i32(GLOBAL_LIST[0].value, 560)
		GLOBAL_LIST[0].value = loc_10
		rt_store_i32(memory_at_0, loc_10 + 44, 0)
		while true do
			reg_0 = FUNC_LIST[273](loc_1)
			loc_19 = reg_0
			if rt_lt_i64(loc_19, rt_i64_ZERO) then
				while true do
					loc_18 = 1
					loc_21 = 1034
					loc_1 = rt_neg_f64(loc_1)
					reg_0 = FUNC_LIST[273](loc_1)
					loc_19 = reg_0
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			if bit_and(loc_4, 2048) ~= 0 then
				while true do
					loc_18 = 1
					loc_21 = 1037
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_18 = bit_and(loc_4, 1)
			loc_21 = (if loc_18 ~= 0 then 1040 else 1035)
			loc_25 = (if loc_18 == 0 then 1 else 0)
			break
		end
		while true do
			if rt_eq_i64(rt_bit_and_i64(loc_19, rt_i64_from_u32(0, 2146435072)), rt_i64_from_u32(0, 2146435072)) then
				while true do
					loc_6 = rt_add_i32(loc_18, 3)
					FUNC_LIST[269](loc_0, 32, loc_2, loc_6, bit_and(loc_4, 4294901759))
					FUNC_LIST[263](loc_0, loc_21, loc_18)
					loc_7 = bit_and(loc_5, 32)
					FUNC_LIST[263](loc_0, (if loc_1 ~= loc_1 then (if loc_7 ~= 0 then 1132 else 2037) else (if loc_7 ~= 0 then 1197 else 2041)), 3)
					FUNC_LIST[269](loc_0, 32, loc_2, loc_6, bit_xor(loc_4, 8192))
					loc_9 = (if rt_lt_i32(loc_2, loc_6) then loc_6 else loc_2)
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_20 = rt_add_i32(loc_10, 16)
			while true do
				while true do
					while true do
						reg_0 = FUNC_LIST[256](loc_1, rt_add_i32(loc_10, 44))
						loc_1 = reg_0
						loc_1 = (loc_1 + loc_1)
						if loc_1 ~= 0e0 then
							while true do
								loc_6 = rt_load_i32(memory_at_0, loc_10 + 44)
								rt_store_i32(memory_at_0, loc_10 + 44, rt_sub_i32(loc_6, 1))
								loc_23 = bit_or(loc_5, 32)
								if loc_23 ~= 97 then
									desired = 4
									break
								end
								desired = 2
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						loc_23 = bit_or(loc_5, 32)
						if loc_23 == 97 then
							desired = 2
							break
						end
						loc_22 = rt_load_i32(memory_at_0, loc_10 + 44)
						reg_0 = (if rt_lt_i32(loc_3, 0) then 6 else loc_3)
						desired = 3
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					loc_22 = rt_sub_i32(loc_6, 29)
					rt_store_i32(memory_at_0, loc_10 + 44, loc_22)
					loc_1 = (loc_1 * 2.68435456e8)
					reg_0 = (if rt_lt_i32(loc_3, 0) then 6 else loc_3)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_12 = reg_0
				loc_15 = rt_add_i32(rt_add_i32(loc_10, 48), (if rt_ge_i32(loc_22, 0) then 288 else 0))
				loc_7 = loc_15
				while true do
					reg_0 = loc_7
					while true do
						if bit_and((if loc_1 < 4.294967296e9 then 1 else 0), (if loc_1 >= 0e0 then 1 else 0)) ~= 0 then
							while true do
								reg_1 = rt_truncate_u32_f64(loc_1)
								desired = 4
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
						end
						reg_1 = 0
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
							continue
						end
						break
					end
					loc_6 = reg_1
					rt_store_i32(memory_at_0, reg_0, loc_6)
					loc_7 = rt_add_i32(loc_7, 4)
					loc_1 = ((loc_1 - no_op(loc_6)) * 1e9)
					if loc_1 ~= 0e0 then
						continue
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				while true do
					if rt_le_i32(loc_22, 0) then
						while true do
							loc_3 = loc_22
							loc_6 = loc_7
							loc_8 = loc_15
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					loc_8 = loc_15
					loc_3 = loc_22
					while true do
						loc_3 = (if loc_3 >= 29 then 29 else loc_3)
						while true do
							loc_6 = rt_sub_i32(loc_7, 4)
							if loc_6 < loc_8 then
								break
							end
							loc_26 = rt_extend_i64_u32(loc_3)
							loc_19 = rt_i64_ZERO
							while true do
								loc_19 = rt_add_i64(rt_bit_and_i64(loc_19, rt_i64_from_u32(4294967295, 0)), rt_shl_i64(rt_load_i64_u32(memory_at_0, loc_6), loc_26))
								reg_1 = loc_19
								loc_19 = rt_div_u64(loc_19, rt_i64_from_u32(1000000000, 0))
								rt_store_i64_n32(memory_at_0, loc_6, rt_sub_i64(reg_1, rt_mul_i64(loc_19, rt_i64_from_u32(1000000000, 0))))
								loc_6 = rt_sub_i32(loc_6, 4)
								if loc_6 >= loc_8 then
									continue
								end
								break
							end
							if desired then
								if desired == 5 then
									desired = nil
								end
								break
							end
							loc_6 = rt_wrap_i32_i64(loc_19)
							if loc_6 == 0 then
								break
							end
							loc_8 = rt_sub_i32(loc_8, 4)
							rt_store_i32(memory_at_0, loc_8, loc_6)
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
								continue
							end
							break
						end
						while true do
							loc_6 = loc_7
							if loc_8 < loc_6 then
								while true do
									loc_7 = rt_sub_i32(loc_6, 4)
									if rt_load_i32(memory_at_0, loc_7) == 0 then
										desired = 5
										break
									end
									break
								end
								if desired then
									if desired == 5 then
										desired = nil
										continue
									end
									break
								end
							end
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
								continue
							end
							break
						end
						loc_3 = rt_sub_i32(rt_load_i32(memory_at_0, loc_10 + 44), loc_3)
						rt_store_i32(memory_at_0, loc_10 + 44, loc_3)
						loc_7 = loc_6
						if rt_gt_i32(loc_3, 0) then
							continue
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				if rt_lt_i32(loc_3, 0) then
					while true do
						loc_16 = rt_add_i32(rt_div_u32(rt_add_i32(loc_12, 25), 9), 1)
						loc_24 = (if loc_23 == 102 then 1 else 0)
						while true do
							loc_7 = rt_sub_i32(0, loc_3)
							loc_11 = (if loc_7 >= 9 then 9 else loc_7)
							while true do
								if loc_6 <= loc_8 then
									while true do
										loc_7 = rt_shl_i32((if rt_load_i32(memory_at_0, loc_8) == 0 then 1 else 0), 2)
										desired = 5
										break
									end
									if desired then
										if desired == 5 then
											desired = nil
										end
										break
									end
								end
								loc_13 = rt_shr_u32(1000000000, loc_11)
								loc_14 = bit_xor(rt_shl_i32(4294967295, loc_11), 4294967295)
								loc_3 = 0
								loc_7 = loc_8
								while true do
									loc_9 = rt_load_i32(memory_at_0, loc_7)
									rt_store_i32(memory_at_0, loc_7, rt_add_i32(rt_shr_u32(loc_9, loc_11), loc_3))
									loc_3 = rt_mul_i32(bit_and(loc_9, loc_14), loc_13)
									loc_7 = rt_add_i32(loc_7, 4)
									if loc_7 < loc_6 then
										continue
									end
									break
								end
								if desired then
									if desired == 5 then
										desired = nil
									end
									break
								end
								loc_7 = rt_shl_i32((if rt_load_i32(memory_at_0, loc_8) == 0 then 1 else 0), 2)
								if loc_3 == 0 then
									break
								end
								rt_store_i32(memory_at_0, loc_6, loc_3)
								loc_6 = rt_add_i32(loc_6, 4)
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
									continue
								end
								break
							end
							loc_3 = rt_add_i32(rt_load_i32(memory_at_0, loc_10 + 44), loc_11)
							rt_store_i32(memory_at_0, loc_10 + 44, loc_3)
							loc_8 = rt_add_i32(loc_7, loc_8)
							loc_7 = (if loc_24 ~= 0 then loc_15 else loc_8)
							loc_6 = (if rt_gt_i32(rt_shr_i32(rt_sub_i32(loc_6, loc_7), 2), loc_16) then rt_add_i32(loc_7, rt_shl_i32(loc_16, 2)) else loc_6)
							if rt_lt_i32(loc_3, 0) then
								continue
							end
							break
						end
						if desired then
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				loc_3 = 0
				while true do
					if loc_6 <= loc_8 then
						break
					end
					loc_3 = rt_mul_i32(rt_shr_i32(rt_sub_i32(loc_15, loc_8), 2), 9)
					loc_7 = 10
					loc_9 = rt_load_i32(memory_at_0, loc_8)
					if loc_9 < 10 then
						break
					end
					while true do
						loc_3 = rt_add_i32(loc_3, 1)
						loc_7 = rt_mul_i32(loc_7, 10)
						if loc_9 >= loc_7 then
							continue
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_7 = rt_sub_i32(rt_sub_i32(loc_12, (if loc_23 ~= 102 then loc_3 else 0)), bit_and((if loc_23 == 103 then 1 else 0), (if loc_12 ~= 0 then 1 else 0)))
				if rt_lt_i32(loc_7, rt_sub_i32(rt_mul_i32(rt_shr_i32(rt_sub_i32(loc_6, loc_15), 2), 9), 9)) then
					while true do
						loc_9 = rt_add_i32(loc_7, 9216)
						loc_13 = rt_div_i32(loc_9, 9)
						loc_16 = rt_add_i32(rt_add_i32(rt_add_i32(loc_10, 48), (if rt_lt_i32(loc_22, 0) then 4 else 292)), rt_shl_i32(loc_13, 2))
						loc_11 = rt_sub_i32(loc_16, 4096)
						loc_7 = 10
						loc_9 = rt_sub_i32(loc_9, rt_mul_i32(loc_13, 9))
						if rt_le_i32(loc_9, 7) then
							while true do
								while true do
									loc_7 = rt_mul_i32(loc_7, 10)
									loc_9 = rt_add_i32(loc_9, 1)
									if loc_9 ~= 8 then
										continue
									end
									break
								end
								if desired then
									break
								end
								break
							end
							if desired then
								break
							end
						end
						while true do
							loc_9 = rt_load_i32(memory_at_0, loc_11)
							loc_24 = rt_div_u32(loc_9, loc_7)
							loc_13 = rt_sub_i32(loc_9, rt_mul_i32(loc_24, loc_7))
							loc_14 = rt_sub_i32(loc_16, 4092)
							if bit_and((if loc_13 == 0 then 1 else 0), (if loc_14 == loc_6 then 1 else 0)) ~= 0 then
								break
							end
							while true do
								if bit_and(loc_24, 1) == 0 then
									while true do
										loc_1 = 9.007199254740992e15
										if loc_7 ~= 1000000000 then
											desired = 5
											break
										end
										if loc_8 >= loc_11 then
											desired = 5
											break
										end
										if bit_and(rt_load_i32_u8(memory_at_0, rt_sub_i32(loc_16, 4100)), 1) == 0 then
											desired = 5
											break
										end
										break
									end
									if desired then
										if desired == 5 then
											desired = nil
										end
										break
									end
								end
								loc_1 = 9.007199254740994e15
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
							reg_1 = (if loc_6 == loc_14 then 1e0 else 1.5e0)
							loc_14 = rt_shr_u32(loc_7, 1)
							loc_17 = (if loc_13 < loc_14 then 5e-1 else (if loc_13 == loc_14 then reg_1 else 1.5e0))
							while true do
								if loc_25 ~= 0 then
									break
								end
								if rt_load_i32_u8(memory_at_0, loc_21) ~= 45 then
									break
								end
								loc_17 = rt_neg_f64(loc_17)
								loc_1 = rt_neg_f64(loc_1)
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
							loc_9 = rt_sub_i32(loc_9, loc_13)
							rt_store_i32(memory_at_0, loc_11, loc_9)
							if (loc_1 + loc_17) == loc_1 then
								break
							end
							loc_7 = rt_add_i32(loc_7, loc_9)
							rt_store_i32(memory_at_0, loc_11, loc_7)
							if loc_7 >= 1000000000 then
								while true do
									while true do
										rt_store_i32(memory_at_0, loc_11, 0)
										loc_11 = rt_sub_i32(loc_11, 4)
										if loc_8 > loc_11 then
											while true do
												loc_8 = rt_sub_i32(loc_8, 4)
												rt_store_i32(memory_at_0, loc_8, 0)
												break
											end
											if desired then
												if desired == 6 then
													desired = nil
													continue
												end
												break
											end
										end
										loc_7 = rt_add_i32(rt_load_i32(memory_at_0, loc_11), 1)
										rt_store_i32(memory_at_0, loc_11, loc_7)
										if loc_7 > 999999999 then
											continue
										end
										break
									end
									if desired then
										break
									end
									break
								end
								if desired then
									if desired == 4 then
										desired = nil
									end
									break
								end
							end
							loc_3 = rt_mul_i32(rt_shr_i32(rt_sub_i32(loc_15, loc_8), 2), 9)
							loc_7 = 10
							loc_9 = rt_load_i32(memory_at_0, loc_8)
							if loc_9 < 10 then
								break
							end
							while true do
								loc_3 = rt_add_i32(loc_3, 1)
								loc_7 = rt_mul_i32(loc_7, 10)
								if loc_9 >= loc_7 then
									continue
								end
								break
							end
							if desired then
								if desired == 4 then
									desired = nil
								end
								break
							end
							break
						end
						if desired then
							break
						end
						loc_7 = rt_add_i32(loc_11, 4)
						loc_6 = (if loc_6 > loc_7 then loc_7 else loc_6)
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				while true do
					loc_7 = loc_6
					loc_9 = (if loc_7 <= loc_8 then 1 else 0)
					if loc_9 == 0 then
						while true do
							loc_6 = rt_sub_i32(loc_6, 4)
							if rt_load_i32(memory_at_0, loc_6) == 0 then
								desired = 3
								break
							end
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
								continue
							end
							break
						end
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				while true do
					if loc_23 ~= 103 then
						while true do
							loc_11 = bit_and(loc_4, 8)
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					loc_6 = (if loc_12 ~= 0 then loc_12 else 1)
					loc_11 = bit_and((if rt_gt_i32(loc_6, loc_3) then 1 else 0), (if rt_gt_i32(loc_3, 4294967291) then 1 else 0))
					loc_12 = rt_add_i32((if loc_11 ~= 0 then bit_xor(loc_3, 4294967295) else 4294967295), loc_6)
					loc_5 = rt_add_i32((if loc_11 ~= 0 then 4294967295 else 4294967294), loc_5)
					loc_11 = bit_and(loc_4, 8)
					if loc_11 ~= 0 then
						break
					end
					loc_6 = 4294967287
					while true do
						if loc_9 ~= 0 then
							break
						end
						loc_11 = rt_load_i32(memory_at_0, rt_sub_i32(loc_7, 4))
						if loc_11 == 0 then
							break
						end
						loc_9 = 10
						loc_6 = 0
						if (loc_11 % 10) ~= 0 then
							break
						end
						while true do
							loc_13 = loc_6
							loc_6 = rt_add_i32(loc_13, 1)
							loc_9 = rt_mul_i32(loc_9, 10)
							if (loc_11 % loc_9) == 0 then
								continue
							end
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						loc_6 = bit_xor(loc_13, 4294967295)
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					loc_9 = rt_mul_i32(rt_shr_i32(rt_sub_i32(loc_7, loc_15), 2), 9)
					if bit_and(loc_5, 4294967263) == 70 then
						while true do
							loc_11 = 0
							loc_6 = rt_sub_i32(rt_add_i32(loc_6, loc_9), 9)
							loc_6 = (if rt_gt_i32(loc_6, 0) then loc_6 else 0)
							loc_12 = (if rt_gt_i32(loc_6, loc_12) then loc_12 else loc_6)
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					loc_11 = 0
					loc_6 = rt_sub_i32(rt_add_i32(rt_add_i32(loc_3, loc_9), loc_6), 9)
					loc_6 = (if rt_gt_i32(loc_6, 0) then loc_6 else 0)
					loc_12 = (if rt_gt_i32(loc_6, loc_12) then loc_12 else loc_6)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_9 = 4294967295
				loc_13 = bit_or(loc_11, loc_12)
				if rt_gt_i32(loc_12, (if loc_13 ~= 0 then 2147483645 else 2147483646)) then
					desired = 1
					break
				end
				loc_14 = rt_add_i32(rt_add_i32(loc_12, (if loc_13 ~= 0 then 1 else 0)), 1)
				while true do
					loc_24 = bit_and(loc_5, 4294967263)
					if loc_24 == 70 then
						while true do
							if rt_gt_i32(loc_3, bit_xor(loc_14, 2147483647)) then
								desired = 1
								break
							end
							loc_6 = (if rt_gt_i32(loc_3, 0) then loc_3 else 0)
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					loc_6 = rt_shr_i32(loc_3, 31)
					reg_1 = FUNC_LIST[268](rt_extend_i64_u32(rt_sub_i32(bit_xor(loc_3, loc_6), loc_6)), loc_20)
					loc_6 = reg_1
					if rt_le_i32(rt_sub_i32(loc_20, loc_6), 1) then
						while true do
							while true do
								loc_6 = rt_sub_i32(loc_6, 1)
								rt_store_i32_n8(memory_at_0, loc_6, 48)
								if rt_lt_i32(rt_sub_i32(loc_20, loc_6), 2) then
									continue
								end
								break
							end
							if desired then
								break
							end
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					loc_16 = rt_sub_i32(loc_6, 2)
					rt_store_i32_n8(memory_at_0, loc_16, loc_5)
					rt_store_i32_n8(memory_at_0, rt_sub_i32(loc_6, 1), (if rt_lt_i32(loc_3, 0) then 45 else 43))
					loc_6 = rt_sub_i32(loc_20, loc_16)
					if rt_gt_i32(loc_6, bit_xor(loc_14, 2147483647)) then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_6 = rt_add_i32(loc_6, loc_14)
				if rt_gt_i32(loc_6, bit_xor(loc_18, 2147483647)) then
					desired = 1
					break
				end
				loc_14 = rt_add_i32(loc_6, loc_18)
				FUNC_LIST[269](loc_0, 32, loc_2, loc_14, loc_4)
				FUNC_LIST[263](loc_0, loc_21, loc_18)
				FUNC_LIST[269](loc_0, 48, loc_2, loc_14, bit_xor(loc_4, 65536))
				while true do
					while true do
						while true do
							if loc_24 == 70 then
								while true do
									loc_11 = bit_or(rt_add_i32(loc_10, 16), 8)
									loc_3 = bit_or(rt_add_i32(loc_10, 16), 9)
									loc_9 = (if loc_8 > loc_15 then loc_15 else loc_8)
									loc_8 = loc_9
									while true do
										reg_0 = FUNC_LIST[268](rt_load_i64_u32(memory_at_0, loc_8), loc_3)
										loc_6 = reg_0
										while true do
											if loc_8 ~= loc_9 then
												while true do
													if loc_6 <= rt_add_i32(loc_10, 16) then
														desired = 8
														break
													end
													while true do
														loc_6 = rt_sub_i32(loc_6, 1)
														rt_store_i32_n8(memory_at_0, loc_6, 48)
														if loc_6 > rt_add_i32(loc_10, 16) then
															continue
														end
														break
													end
													if desired then
														break
													end
													desired = 8
													break
												end
												if desired then
													if desired == 8 then
														desired = nil
													end
													break
												end
											end
											if loc_3 ~= loc_6 then
												break
											end
											rt_store_i32_n8(memory_at_0, loc_10 + 24, 48)
											loc_6 = loc_11
											break
										end
										if desired then
											if desired == 7 then
												desired = nil
												continue
											end
											break
										end
										FUNC_LIST[263](loc_0, loc_6, rt_sub_i32(loc_3, loc_6))
										loc_8 = rt_add_i32(loc_8, 4)
										if loc_8 <= loc_15 then
											continue
										end
										break
									end
									if desired then
										break
									end
									if loc_13 ~= 0 then
										while true do
											FUNC_LIST[263](loc_0, 2266, 1)
											break
										end
										if desired then
											break
										end
									end
									if loc_7 <= loc_8 then
										desired = 5
										break
									end
									if rt_le_i32(loc_12, 0) then
										desired = 5
										break
									end
									while true do
										reg_0 = FUNC_LIST[268](rt_load_i64_u32(memory_at_0, loc_8), loc_3)
										loc_6 = reg_0
										if loc_6 > rt_add_i32(loc_10, 16) then
											while true do
												while true do
													loc_6 = rt_sub_i32(loc_6, 1)
													rt_store_i32_n8(memory_at_0, loc_6, 48)
													if loc_6 > rt_add_i32(loc_10, 16) then
														continue
													end
													break
												end
												if desired then
													break
												end
												break
											end
											if desired then
												if desired == 7 then
													desired = nil
													continue
												end
												break
											end
										end
										FUNC_LIST[263](loc_0, loc_6, (if rt_ge_i32(loc_12, 9) then 9 else loc_12))
										loc_6 = rt_sub_i32(loc_12, 9)
										loc_8 = rt_add_i32(loc_8, 4)
										if loc_8 >= loc_7 then
											desired = 4
											break
										end
										loc_9 = (if rt_gt_i32(loc_12, 9) then 1 else 0)
										loc_12 = loc_6
										if loc_9 ~= 0 then
											continue
										end
										break
									end
									if desired then
										break
									end
									desired = 4
									break
								end
								if desired then
									if desired == 5 then
										desired = nil
									end
									break
								end
							end
							while true do
								if rt_lt_i32(loc_12, 0) then
									break
								end
								loc_13 = (if loc_7 > loc_8 then loc_7 else rt_add_i32(loc_8, 4))
								loc_15 = bit_or(rt_add_i32(loc_10, 16), 8)
								loc_3 = bit_or(rt_add_i32(loc_10, 16), 9)
								loc_7 = loc_8
								while true do
									reg_1 = FUNC_LIST[268](rt_load_i64_u32(memory_at_0, loc_7), loc_3)
									loc_6 = reg_1
									if loc_3 == loc_6 then
										while true do
											rt_store_i32_n8(memory_at_0, loc_10 + 24, 48)
											loc_6 = loc_15
											break
										end
										if desired then
											if desired == 7 then
												desired = nil
												continue
											end
											break
										end
									end
									while true do
										if loc_7 ~= loc_8 then
											while true do
												if loc_6 <= rt_add_i32(loc_10, 16) then
													desired = 8
													break
												end
												while true do
													loc_6 = rt_sub_i32(loc_6, 1)
													rt_store_i32_n8(memory_at_0, loc_6, 48)
													if loc_6 > rt_add_i32(loc_10, 16) then
														continue
													end
													break
												end
												if desired then
													break
												end
												desired = 8
												break
											end
											if desired then
												if desired == 8 then
													desired = nil
												end
												break
											end
										end
										FUNC_LIST[263](loc_0, loc_6, 1)
										loc_6 = rt_add_i32(loc_6, 1)
										if bit_or(loc_11, loc_12) == 0 then
											break
										end
										FUNC_LIST[263](loc_0, 2266, 1)
										break
									end
									if desired then
										if desired == 7 then
											desired = nil
											continue
										end
										break
									end
									loc_9 = rt_sub_i32(loc_3, loc_6)
									FUNC_LIST[263](loc_0, loc_6, (if rt_lt_i32(loc_9, loc_12) then loc_9 else loc_12))
									loc_12 = rt_sub_i32(loc_12, loc_9)
									loc_7 = rt_add_i32(loc_7, 4)
									if loc_7 >= loc_13 then
										desired = 6
										break
									end
									if rt_ge_i32(loc_12, 0) then
										continue
									end
									break
								end
								if desired then
									if desired == 6 then
										desired = nil
									end
									break
								end
								break
							end
							if desired then
								if desired == 5 then
									desired = nil
								end
								break
							end
							FUNC_LIST[269](loc_0, 48, rt_add_i32(loc_12, 18), 18, 0)
							FUNC_LIST[263](loc_0, loc_16, rt_sub_i32(loc_20, loc_16))
							desired = 3
							break
						end
						if desired then
							if desired == 4 then
								desired = nil
							end
							break
						end
						loc_6 = loc_12
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					FUNC_LIST[269](loc_0, 48, rt_add_i32(loc_6, 9), 9, 0)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				FUNC_LIST[269](loc_0, 32, loc_2, loc_14, bit_xor(loc_4, 8192))
				loc_9 = (if rt_lt_i32(loc_2, loc_14) then loc_14 else loc_2)
				desired = 1
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_14 = rt_add_i32(loc_21, bit_and(rt_shr_i32(rt_shl_i32(loc_5, 26), 31), 9))
			while true do
				if loc_3 > 11 then
					break
				end
				loc_6 = rt_sub_i32(12, loc_3)
				loc_17 = 1.6e1
				while true do
					loc_17 = (loc_17 * 1.6e1)
					loc_6 = rt_sub_i32(loc_6, 1)
					if loc_6 ~= 0 then
						continue
					end
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				if rt_load_i32_u8(memory_at_0, loc_14) == 45 then
					while true do
						loc_1 = rt_neg_f64((loc_17 + (rt_neg_f64(loc_1) - loc_17)))
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				loc_1 = ((loc_1 + loc_17) - loc_17)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_6 = rt_load_i32(memory_at_0, loc_10 + 44)
			reg_1 = loc_6
			loc_6 = rt_shr_i32(loc_6, 31)
			reg_1 = FUNC_LIST[268](rt_extend_i64_u32(rt_sub_i32(bit_xor(reg_1, loc_6), loc_6)), loc_20)
			loc_6 = reg_1
			if loc_20 == loc_6 then
				while true do
					rt_store_i32_n8(memory_at_0, loc_10 + 15, 48)
					loc_6 = rt_add_i32(loc_10, 15)
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_11 = bit_or(loc_18, 2)
			loc_8 = bit_and(loc_5, 32)
			loc_7 = rt_load_i32(memory_at_0, loc_10 + 44)
			loc_13 = rt_sub_i32(loc_6, 2)
			rt_store_i32_n8(memory_at_0, loc_13, rt_add_i32(loc_5, 15))
			rt_store_i32_n8(memory_at_0, rt_sub_i32(loc_6, 1), (if rt_lt_i32(loc_7, 0) then 45 else 43))
			loc_9 = bit_and(loc_4, 8)
			loc_7 = rt_add_i32(loc_10, 16)
			while true do
				loc_6 = loc_7
				reg_0 = loc_6
				while true do
					if math_abs(loc_1) < 2.147483648e9 then
						while true do
							reg_1 = rt_truncate_u32_f64(loc_1)
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					reg_1 = 2147483648
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
						continue
					end
					break
				end
				loc_7 = reg_1
				rt_store_i32_n8(memory_at_0, reg_0, bit_or(rt_load_i32_u8(memory_at_0, rt_add_i32(loc_7, 8960)), loc_8))
				loc_1 = ((loc_1 - rt_convert_f64_i32(loc_7)) * 1.6e1)
				while true do
					loc_7 = rt_add_i32(loc_6, 1)
					if rt_sub_i32(loc_7, rt_add_i32(loc_10, 16)) ~= 1 then
						break
					end
					while true do
						if loc_9 ~= 0 then
							break
						end
						if rt_gt_i32(loc_3, 0) then
							break
						end
						if loc_1 == 0e0 then
							desired = 3
							break
						end
						break
					end
					if desired then
						if desired == 3 then
							desired = nil
						end
						break
					end
					rt_store_i32_n8(memory_at_0, loc_6 + 1, 46)
					loc_7 = rt_add_i32(loc_6, 2)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
						continue
					end
					break
				end
				if loc_1 ~= 0e0 then
					continue
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_9 = 4294967295
			loc_8 = rt_sub_i32(loc_20, loc_13)
			loc_16 = rt_add_i32(loc_11, loc_8)
			if rt_lt_i32(rt_sub_i32(2147483645, loc_16), loc_3) then
				break
			end
			loc_6 = rt_sub_i32(loc_7, rt_add_i32(loc_10, 16))
			loc_3 = (if loc_3 ~= 0 then (if rt_lt_i32(rt_sub_i32(loc_6, 2), loc_3) then rt_add_i32(loc_3, 2) else loc_6) else loc_6)
			loc_7 = rt_add_i32(loc_16, loc_3)
			FUNC_LIST[269](loc_0, 32, loc_2, loc_7, loc_4)
			FUNC_LIST[263](loc_0, loc_14, loc_11)
			FUNC_LIST[269](loc_0, 48, loc_2, loc_7, bit_xor(loc_4, 65536))
			FUNC_LIST[263](loc_0, rt_add_i32(loc_10, 16), loc_6)
			FUNC_LIST[269](loc_0, 48, rt_sub_i32(loc_3, loc_6), 0, 0)
			FUNC_LIST[263](loc_0, loc_13, loc_8)
			FUNC_LIST[269](loc_0, 32, loc_2, loc_7, bit_xor(loc_4, 8192))
			loc_9 = (if rt_lt_i32(loc_2, loc_7) then loc_7 else loc_2)
			break
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_10, 560)
		reg_0 = loc_9
		break
	end
	return reg_0
end
FUNC_LIST[272] = --[[ pop_arg_long_double ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	local reg_1
	while true do
		loc_2 = bit_and(rt_add_i32(rt_load_i32(memory_at_0, loc_1), 7), 4294967288)
		rt_store_i32(memory_at_0, loc_1, rt_add_i32(loc_2, 16))
		reg_1 = FUNC_LIST[259](rt_load_i64(memory_at_0, loc_2), rt_load_i64(memory_at_0, loc_2 + 8))
		rt_store_f64(memory_at_0, loc_0, reg_1)
		break
	end
end
FUNC_LIST[273] = --[[ __DOUBLE_BITS ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = rt_reinterpret_i64_f64(loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[274] = --[[ vsnprintf ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = 0
	local loc_5 = 0
	local reg_0
	while true do
		loc_4 = rt_sub_i32(GLOBAL_LIST[0].value, 160)
		GLOBAL_LIST[0].value = loc_4
		loc_0 = (if loc_1 ~= 0 then loc_0 else rt_add_i32(loc_4, 158))
		rt_store_i32(memory_at_0, loc_4 + 148, loc_0)
		loc_5 = rt_sub_i32(loc_1, 1)
		rt_store_i32(memory_at_0, loc_4 + 152, (if loc_1 >= loc_5 then loc_5 else 0))
		reg_0 = FUNC_LIST[219](loc_4, 0, 144)
		loc_4 = reg_0
		rt_store_i32(memory_at_0, loc_4 + 76, 4294967295)
		rt_store_i32(memory_at_0, loc_4 + 36, 167)
		rt_store_i32(memory_at_0, loc_4 + 80, 4294967295)
		rt_store_i32(memory_at_0, loc_4 + 44, rt_add_i32(loc_4, 159))
		rt_store_i32(memory_at_0, loc_4 + 84, rt_add_i32(loc_4, 148))
		rt_store_i32_n8(memory_at_0, loc_0, 0)
		reg_0 = FUNC_LIST[270](loc_4, loc_2, loc_3)
		loc_1 = reg_0
		GLOBAL_LIST[0].value = rt_add_i32(loc_4, 160)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[275] = --[[ sn_write ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local reg_0
	while true do
		loc_3 = rt_load_i32(memory_at_0, loc_0 + 84)
		loc_5 = rt_load_i32(memory_at_0, loc_3)
		loc_4 = rt_load_i32(memory_at_0, loc_3 + 4)
		loc_7 = rt_load_i32(memory_at_0, loc_0 + 28)
		loc_6 = rt_sub_i32(rt_load_i32(memory_at_0, loc_0 + 20), loc_7)
		loc_6 = (if loc_4 < loc_6 then loc_4 else loc_6)
		if loc_6 ~= 0 then
			while true do
				reg_0 = FUNC_LIST[188](loc_5, loc_7, loc_6)
				loc_5 = rt_add_i32(rt_load_i32(memory_at_0, loc_3), loc_6)
				rt_store_i32(memory_at_0, loc_3, loc_5)
				loc_4 = rt_sub_i32(rt_load_i32(memory_at_0, loc_3 + 4), loc_6)
				rt_store_i32(memory_at_0, loc_3 + 4, loc_4)
				break
			end
		end
		loc_4 = (if loc_2 > loc_4 then loc_4 else loc_2)
		if loc_4 ~= 0 then
			while true do
				reg_0 = FUNC_LIST[188](loc_5, loc_1, loc_4)
				loc_5 = rt_add_i32(rt_load_i32(memory_at_0, loc_3), loc_4)
				rt_store_i32(memory_at_0, loc_3, loc_5)
				rt_store_i32(memory_at_0, loc_3 + 4, rt_sub_i32(rt_load_i32(memory_at_0, loc_3 + 4), loc_4))
				break
			end
		end
		rt_store_i32_n8(memory_at_0, loc_5, 0)
		loc_3 = rt_load_i32(memory_at_0, loc_0 + 44)
		rt_store_i32(memory_at_0, loc_0 + 28, loc_3)
		rt_store_i32(memory_at_0, loc_0 + 20, loc_3)
		reg_0 = loc_2
		break
	end
	return reg_0
end
FUNC_LIST[276] = --[[ snprintf ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = 0
	local reg_0
	while true do
		loc_4 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_4
		rt_store_i32(memory_at_0, loc_4 + 12, loc_3)
		reg_0 = FUNC_LIST[274](loc_0, loc_1, loc_2, loc_3)
		loc_3 = reg_0
		GLOBAL_LIST[0].value = rt_add_i32(loc_4, 16)
		reg_0 = loc_3
		break
	end
	return reg_0
end
FUNC_LIST[277] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::size[abi:nn180100]() const ]] function(loc_0)
	local reg_0
	local desired
	while true do
		reg_0 = FUNC_LIST[243](loc_0)
		if reg_0 ~= 0 then
			while true do
				reg_0 = FUNC_LIST[287](loc_0)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		reg_0 = FUNC_LIST[288](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[278] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::capacity[abi:nn180100]() const ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		loc_1 = 10
		reg_0 = FUNC_LIST[243](loc_0)
		if reg_0 ~= 0 then
			while true do
				reg_0 = FUNC_LIST[290](loc_0)
				loc_1 = rt_sub_i32(reg_0, 1)
				break
			end
		end
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[279] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__get_pointer[abi:nn180100]() ]] function(loc_0)
	local reg_0
	local desired
	while true do
		reg_0 = FUNC_LIST[243](loc_0)
		if reg_0 ~= 0 then
			while true do
				reg_0 = FUNC_LIST[291](loc_0)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		reg_0 = FUNC_LIST[292](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[280] = --[[ char* std::__2::__to_address[abi:nn180100]<char>(char*) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[281] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__annotate_increase[abi:nn180100](unsigned long) const ]] function(loc_0, loc_1)
	while true do
		break
	end
end
FUNC_LIST[282] = --[[ bool std::__2::__is_pointer_in_range[abi:nn180100]<char, char, 0>(char const*, char const*, char const*) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_3
		rt_store_i32(memory_at_0, loc_3 + 8, loc_1)
		rt_store_i32(memory_at_0, loc_3 + 12, loc_0)
		rt_store_i32(memory_at_0, loc_3 + 4, loc_2)
		loc_1 = 0
		reg_0 = FUNC_LIST[297](rt_add_i32(loc_3, 3), rt_add_i32(loc_3, 4), rt_add_i32(loc_3, 12))
		if reg_0 == 0 then
			while true do
				reg_0 = FUNC_LIST[297](rt_add_i32(loc_3, 2), rt_add_i32(loc_3, 4), rt_add_i32(loc_3, 8))
				loc_1 = reg_0
				break
			end
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_3, 16)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[283] = --[[ std::__2::char_traits<char>::move[abi:nn180100](char*, char const*, unsigned long) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[293](loc_0, loc_1, loc_2)
		break
	end
	return reg_0
end
FUNC_LIST[284] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__null_terminate_at[abi:nn180100](char*, unsigned long) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local reg_0
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_3
		reg_0 = FUNC_LIST[277](loc_0)
		loc_4 = reg_0
		if loc_4 < loc_2 then
			while true do
				FUNC_LIST[281](loc_0, rt_sub_i32(loc_2, loc_4))
				break
			end
		end
		FUNC_LIST[294](loc_0, loc_2)
		rt_store_i32_n8(memory_at_0, loc_3 + 15, 0)
		FUNC_LIST[295](rt_add_i32(loc_1, loc_2), rt_add_i32(loc_3, 15))
		if loc_2 < loc_4 then
			while true do
				FUNC_LIST[296](loc_0, loc_4)
				break
			end
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_3, 16)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[285] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__grow_by_and_replace(unsigned long, unsigned long, unsigned long, unsigned long, unsigned long, unsigned long, char const*) ]] function(loc_0, loc_1, loc_2, loc_3, loc_4, loc_5, loc_6, loc_7)
	local loc_8 = 0
	local loc_9 = 0
	local loc_10 = 0
	local loc_11 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_8 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_8
		reg_1 = FUNC_LIST[298](loc_0)
		loc_9 = reg_1
		if loc_2 <= rt_add_i32(loc_9, bit_xor(loc_1, 4294967295)) then
			while true do
				reg_0 = FUNC_LIST[279](loc_0)
				loc_10 = reg_0
				if loc_1 < rt_sub_i32(rt_shr_u32(loc_9, 1), 8) then
					while true do
						rt_store_i32(memory_at_0, loc_8 + 12, rt_shl_i32(loc_1, 1))
						rt_store_i32(memory_at_0, loc_8 + 4, rt_add_i32(loc_1, loc_2))
						reg_0 = FUNC_LIST[299](rt_add_i32(loc_8, 4), rt_add_i32(loc_8, 12))
						reg_0 = FUNC_LIST[300](rt_load_i32(memory_at_0, reg_0))
						loc_9 = rt_add_i32(reg_0, 1)
						break
					end
					if desired then
						break
					end
				end
				FUNC_LIST[301](loc_0)
				reg_1 = FUNC_LIST[302](loc_0)
				FUNC_LIST[303](rt_add_i32(loc_8, 4), reg_1, loc_9)
				loc_9 = rt_load_i32(memory_at_0, loc_8 + 4)
				FUNC_LIST[304](loc_9, rt_load_i32(memory_at_0, loc_8 + 8))
				if loc_4 ~= 0 then
					while true do
						reg_0 = FUNC_LIST[280](loc_9)
						reg_1 = FUNC_LIST[280](loc_10)
						reg_0 = FUNC_LIST[305](reg_0, reg_1, loc_4)
						break
					end
					if desired then
						break
					end
				end
				if loc_6 ~= 0 then
					while true do
						reg_0 = FUNC_LIST[280](loc_9)
						reg_0 = FUNC_LIST[305](rt_add_i32(reg_0, loc_4), loc_7, loc_6)
						break
					end
					if desired then
						break
					end
				end
				loc_11 = rt_add_i32(loc_4, loc_5)
				loc_7 = rt_sub_i32(loc_3, loc_11)
				if loc_3 ~= loc_11 then
					while true do
						reg_0 = FUNC_LIST[280](loc_9)
						reg_1 = FUNC_LIST[280](loc_10)
						reg_0 = FUNC_LIST[305](rt_add_i32(rt_add_i32(reg_0, loc_4), loc_6), rt_add_i32(rt_add_i32(reg_1, loc_4), loc_5), loc_7)
						break
					end
					if desired then
						break
					end
				end
				loc_3 = rt_add_i32(loc_1, 1)
				if loc_3 ~= 11 then
					while true do
						reg_0 = FUNC_LIST[302](loc_0)
						FUNC_LIST[306](reg_0, loc_10, loc_3)
						break
					end
					if desired then
						break
					end
				end
				FUNC_LIST[307](loc_0, loc_9)
				FUNC_LIST[308](loc_0, rt_load_i32(memory_at_0, loc_8 + 8))
				loc_4 = rt_add_i32(rt_add_i32(loc_4, loc_6), loc_7)
				FUNC_LIST[309](loc_0, loc_4)
				rt_store_i32_n8(memory_at_0, loc_8 + 12, 0)
				FUNC_LIST[295](rt_add_i32(loc_4, loc_9), rt_add_i32(loc_8, 12))
				FUNC_LIST[310](loc_0, rt_add_i32(loc_1, loc_2))
				GLOBAL_LIST[0].value = rt_add_i32(loc_8, 16)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		FUNC_LIST[311](loc_0)
		error("out of code bounds")
	end
end
FUNC_LIST[286] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__throw_out_of_range[abi:nn180100]() const ]] function(loc_0)
	while true do
		FUNC_LIST[289](1184)
		error("out of code bounds")
	end
end
FUNC_LIST[287] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__get_long_size[abi:nn180100]() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[244](loc_0)
		reg_0 = rt_load_i32(memory_at_0, reg_0 + 4)
		break
	end
	return reg_0
end
FUNC_LIST[288] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__get_short_size[abi:nn180100]() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[244](loc_0)
		reg_0 = bit_and(rt_load_i32_u8(memory_at_0, reg_0 + 11), 127)
		break
	end
	return reg_0
end
FUNC_LIST[289] = --[[ std::__2::__throw_out_of_range[abi:nn180100](char const*) ]] function(loc_0)
	while true do
		FUNC_LIST[2]()
		error("out of code bounds")
	end
end
FUNC_LIST[290] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__get_long_cap[abi:nn180100]() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[244](loc_0)
		reg_0 = bit_and(rt_load_i32(memory_at_0, reg_0 + 8), 2147483647)
		break
	end
	return reg_0
end
FUNC_LIST[291] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__get_long_pointer[abi:nn180100]() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[318](loc_0)
		reg_0 = rt_load_i32(memory_at_0, reg_0)
		break
	end
	return reg_0
end
FUNC_LIST[292] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__get_short_pointer[abi:nn180100]() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[318](loc_0)
		reg_0 = FUNC_LIST[319](reg_0)
		break
	end
	return reg_0
end
FUNC_LIST[293] = --[[ char* std::__2::__constexpr_memmove[abi:nn180100]<char, char const, 0>(char*, char const*, std::__2::__element_count) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		if loc_2 ~= 0 then
			while true do
				reg_0 = FUNC_LIST[189](loc_0, loc_1, loc_2)
				break
			end
		end
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[294] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__set_size[abi:nn180100](unsigned long) ]] function(loc_0, loc_1)
	local reg_0
	local desired
	while true do
		reg_0 = FUNC_LIST[243](loc_0)
		if reg_0 ~= 0 then
			while true do
				FUNC_LIST[309](loc_0, loc_1)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		FUNC_LIST[313](loc_0, loc_1)
		break
	end
end
FUNC_LIST[295] = --[[ std::__2::char_traits<char>::assign[abi:nn180100](char&, char const&) ]] function(loc_0, loc_1)
	while true do
		rt_store_i32_n8(memory_at_0, loc_0, rt_load_i32_u8(memory_at_0, loc_1))
		break
	end
end
FUNC_LIST[296] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__annotate_shrink[abi:nn180100](unsigned long) const ]] function(loc_0, loc_1)
	while true do
		break
	end
end
FUNC_LIST[297] = --[[ bool std::__2::__less<void, void>::operator()[abi:nn180100]<char const*, char const*>(char const* const&, char const* const&) const ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1) < rt_load_i32(memory_at_0, loc_2) then 1 else 0)
		break
	end
	return reg_0
end
FUNC_LIST[298] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::max_size[abi:nn180100]() const ]] function(loc_0)
	local reg_0
	local reg_1
	local reg_2
	while true do
		reg_0 = FUNC_LIST[314](loc_0)
		reg_0 = FUNC_LIST[315](reg_0)
		loc_0 = reg_0
		reg_2 = FUNC_LIST[316]()
		reg_0 = rt_sub_i32(rt_shr_u32(loc_0, (if loc_0 > rt_shr_u32(reg_2, 1) then 1 else 0)), 8)
		break
	end
	return reg_0
end
FUNC_LIST[299] = --[[ unsigned long const& std::__2::max[abi:nn180100]<unsigned long>(unsigned long const&, unsigned long const&) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[336](loc_0, loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[300] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__recommend[abi:nn180100](unsigned long) ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		loc_1 = 10
		if loc_0 >= 11 then
			while true do
				reg_0 = FUNC_LIST[322](rt_add_i32(loc_0, 1))
				loc_0 = reg_0
				reg_0 = loc_0
				loc_0 = rt_sub_i32(loc_0, 1)
				loc_1 = (if loc_0 == 11 then reg_0 else loc_0)
				break
			end
		end
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[301] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__annotate_delete[abi:nn180100]() const ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[302] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__alloc[abi:nn180100]() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[321](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[303] = --[[ std::__2::__allocation_result<std::__2::allocator_traits<std::__2::allocator<char>>::pointer> std::__2::__allocate_at_least[abi:nn180100]<std::__2::allocator<char>>(std::__2::allocator<char>&, unsigned long) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[320](loc_1, loc_2)
		loc_1 = reg_0
		rt_store_i32(memory_at_0, loc_0 + 4, loc_2)
		rt_store_i32(memory_at_0, loc_0, loc_1)
		break
	end
end
FUNC_LIST[304] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__begin_lifetime[abi:nn180100](char*, unsigned long) ]] function(loc_0, loc_1)
	while true do
		break
	end
end
FUNC_LIST[305] = --[[ std::__2::char_traits<char>::copy[abi:nn180100](char*, char const*, unsigned long) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[323](loc_1, loc_2, loc_0)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[306] = --[[ std::__2::allocator_traits<std::__2::allocator<char>>::deallocate[abi:nn180100](std::__2::allocator<char>&, char*, unsigned long) ]] function(loc_0, loc_1, loc_2)
	while true do
		FUNC_LIST[327](loc_0, loc_1, loc_2)
		break
	end
end
FUNC_LIST[307] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__set_long_pointer[abi:nn180100](char*) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[318](loc_0)
		rt_store_i32(memory_at_0, reg_0, loc_1)
		break
	end
end
FUNC_LIST[308] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__set_long_cap[abi:nn180100](unsigned long) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[318](loc_0)
		loc_2 = reg_0
		rt_store_i32(memory_at_0, loc_2 + 8, bit_or(bit_and(rt_load_i32(memory_at_0, loc_2 + 8), 2147483648), bit_and(loc_1, 2147483647)))
		reg_0 = FUNC_LIST[318](loc_0)
		loc_0 = reg_0
		rt_store_i32(memory_at_0, loc_0 + 8, bit_or(rt_load_i32(memory_at_0, loc_0 + 8), 2147483648))
		break
	end
end
FUNC_LIST[309] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__set_long_size[abi:nn180100](unsigned long) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[318](loc_0)
		rt_store_i32(memory_at_0, reg_0 + 4, loc_1)
		break
	end
end
FUNC_LIST[310] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__annotate_new[abi:nn180100](unsigned long) const ]] function(loc_0, loc_1)
	while true do
		break
	end
end
FUNC_LIST[311] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__throw_length_error[abi:nn180100]() const ]] function(loc_0)
	while true do
		FUNC_LIST[317](1184)
		error("out of code bounds")
	end
end
FUNC_LIST[312] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__fits_in_sso[abi:nn180100](unsigned long) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = (if loc_0 < 11 then 1 else 0)
		break
	end
	return reg_0
end
FUNC_LIST[313] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__set_short_size[abi:nn180100](unsigned long) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[318](loc_0)
		loc_2 = reg_0
		rt_store_i32_n8(memory_at_0, loc_2 + 11, bit_or(bit_and(rt_load_i32_u8(memory_at_0, loc_2 + 11), 128), bit_and(loc_1, 127)))
		reg_0 = FUNC_LIST[318](loc_0)
		loc_0 = reg_0
		rt_store_i32_n8(memory_at_0, loc_0 + 11, bit_and(rt_load_i32_u8(memory_at_0, loc_0 + 11), 127))
		break
	end
end
FUNC_LIST[314] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__alloc[abi:nn180100]() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[376](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[315] = --[[ unsigned long std::__2::allocator_traits<std::__2::allocator<char>>::max_size[abi:nn180100]<std::__2::allocator<char>, void, void>(std::__2::allocator<char> const&) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[316]()
		break
	end
	return reg_0
end
FUNC_LIST[316] = --[[ std::__2::numeric_limits<unsigned long>::max[abi:nn180100]() ]] function()
	local reg_0
	while true do
		reg_0 = FUNC_LIST[377]()
		break
	end
	return reg_0
end
FUNC_LIST[317] = --[[ std::__2::__throw_length_error[abi:nn180100](char const*) ]] function(loc_0)
	while true do
		FUNC_LIST[2]()
		error("out of code bounds")
	end
end
FUNC_LIST[318] = --[[ std::__2::__compressed_pair<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, std::__2::allocator<char>>::first[abi:nn180100]() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[379](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[319] = --[[ std::__2::pointer_traits<char*>::pointer_to[abi:nn180100](char&) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[320] = --[[ std::__2::allocator<char>::allocate[abi:nn180100](unsigned long) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[315](loc_0)
		if reg_0 < loc_1 then
			while true do
				FUNC_LIST[380]()
				error("out of code bounds")
			end
		end
		reg_0 = FUNC_LIST[381](loc_1, 1)
		break
	end
	return reg_0
end
FUNC_LIST[321] = --[[ std::__2::__compressed_pair<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, std::__2::allocator<char>>::second[abi:nn180100]() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[385](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[322] = --[[ unsigned long std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__align_it[abi:nn180100]<8ul>(unsigned long) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = bit_and(rt_add_i32(loc_0, 7), 4294967288)
		break
	end
	return reg_0
end
FUNC_LIST[323] = --[[ char* std::__2::copy_n[abi:nn180100]<char const*, unsigned long, char*, 0>(char const*, unsigned long, char*) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[386](loc_0, rt_add_i32(loc_0, loc_1), loc_2)
		break
	end
	return reg_0
end
FUNC_LIST[324] = --[[ std::__2::char_traits<char>::length[abi:nn180100](char const*) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[325](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[325] = --[[ std::__2::__constexpr_strlen[abi:nn180100](char const*) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[192](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[326] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::~basic_string() ]] function(loc_0)
	local reg_0
	local reg_1
	local reg_2
	while true do
		FUNC_LIST[301](loc_0)
		reg_0 = FUNC_LIST[243](loc_0)
		if reg_0 ~= 0 then
			while true do
				reg_0 = FUNC_LIST[302](loc_0)
				reg_1 = FUNC_LIST[291](loc_0)
				reg_2 = FUNC_LIST[290](loc_0)
				FUNC_LIST[306](reg_0, reg_1, reg_2)
				break
			end
		end
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[327] = --[[ std::__2::allocator<char>::deallocate[abi:nn180100](char*, unsigned long) ]] function(loc_0, loc_1, loc_2)
	while true do
		FUNC_LIST[409](loc_1, loc_2, 1)
		break
	end
end
FUNC_LIST[328] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__grow_by_without_replace[abi:nn180100](unsigned long, unsigned long, unsigned long, unsigned long, unsigned long, unsigned long) ]] function(loc_0, loc_1, loc_2, loc_3, loc_4, loc_5, loc_6)
	while true do
		FUNC_LIST[330](loc_0, loc_1, loc_2, loc_3, loc_4, loc_5, loc_6)
		loc_6 = rt_add_i32(rt_sub_i32(loc_3, loc_5), loc_6)
		FUNC_LIST[309](loc_0, loc_6)
		FUNC_LIST[310](loc_0, loc_6)
		break
	end
end
FUNC_LIST[329] = --[[ std::__2::char_traits<char>::assign[abi:nn180100](char*, unsigned long, char) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_3
		rt_store_i32_n8(memory_at_0, loc_3 + 15, loc_2)
		reg_0 = FUNC_LIST[331](loc_0, loc_1, rt_add_i32(loc_3, 15))
		GLOBAL_LIST[0].value = rt_add_i32(loc_3, 16)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[330] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__grow_by[abi:nn180100](unsigned long, unsigned long, unsigned long, unsigned long, unsigned long, unsigned long) ]] function(loc_0, loc_1, loc_2, loc_3, loc_4, loc_5, loc_6)
	local loc_7 = 0
	local loc_8 = 0
	local loc_9 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_7 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_7
		reg_1 = FUNC_LIST[298](loc_0)
		loc_8 = reg_1
		if loc_2 <= rt_sub_i32(loc_8, loc_1) then
			while true do
				reg_0 = FUNC_LIST[279](loc_0)
				loc_9 = reg_0
				if loc_1 < rt_sub_i32(rt_shr_u32(loc_8, 1), 8) then
					while true do
						rt_store_i32(memory_at_0, loc_7 + 12, rt_shl_i32(loc_1, 1))
						rt_store_i32(memory_at_0, loc_7 + 4, rt_add_i32(loc_1, loc_2))
						reg_0 = FUNC_LIST[299](rt_add_i32(loc_7, 4), rt_add_i32(loc_7, 12))
						reg_0 = FUNC_LIST[300](rt_load_i32(memory_at_0, reg_0))
						loc_8 = rt_add_i32(reg_0, 1)
						break
					end
					if desired then
						break
					end
				end
				FUNC_LIST[301](loc_0)
				reg_1 = FUNC_LIST[302](loc_0)
				FUNC_LIST[303](rt_add_i32(loc_7, 4), reg_1, loc_8)
				loc_8 = rt_load_i32(memory_at_0, loc_7 + 4)
				FUNC_LIST[304](loc_8, rt_load_i32(memory_at_0, loc_7 + 8))
				if loc_4 ~= 0 then
					while true do
						reg_0 = FUNC_LIST[280](loc_8)
						reg_1 = FUNC_LIST[280](loc_9)
						reg_0 = FUNC_LIST[305](reg_0, reg_1, loc_4)
						break
					end
					if desired then
						break
					end
				end
				loc_2 = rt_add_i32(loc_4, loc_5)
				if loc_2 ~= loc_3 then
					while true do
						reg_0 = FUNC_LIST[280](loc_8)
						reg_1 = FUNC_LIST[280](loc_9)
						reg_0 = FUNC_LIST[305](rt_add_i32(rt_add_i32(reg_0, loc_4), loc_6), rt_add_i32(rt_add_i32(reg_1, loc_4), loc_5), rt_sub_i32(loc_3, loc_2))
						break
					end
					if desired then
						break
					end
				end
				loc_1 = rt_add_i32(loc_1, 1)
				if loc_1 ~= 11 then
					while true do
						reg_0 = FUNC_LIST[302](loc_0)
						FUNC_LIST[306](reg_0, loc_9, loc_1)
						break
					end
					if desired then
						break
					end
				end
				FUNC_LIST[307](loc_0, loc_8)
				FUNC_LIST[308](loc_0, rt_load_i32(memory_at_0, loc_7 + 8))
				GLOBAL_LIST[0].value = rt_add_i32(loc_7, 16)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		FUNC_LIST[311](loc_0)
		error("out of code bounds")
	end
end
FUNC_LIST[331] = --[[ char* std::__2::fill_n[abi:nn180100]<char*, unsigned long, char>(char*, unsigned long, char const&) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	local reg_1
	while true do
		reg_1 = FUNC_LIST[414](loc_1)
		reg_0 = FUNC_LIST[415](loc_0, reg_1, loc_2)
		break
	end
	return reg_0
end
FUNC_LIST[332] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__init_copy_ctor_external(char const*, unsigned long) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local reg_0
	local reg_1
	local reg_2
	local desired
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_3
		while true do
			while true do
				reg_0 = FUNC_LIST[312](loc_2)
				if reg_0 ~= 0 then
					while true do
						reg_0 = FUNC_LIST[292](loc_0)
						loc_4 = reg_0
						FUNC_LIST[313](loc_0, loc_2)
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				reg_0 = FUNC_LIST[298](loc_0)
				if reg_0 < loc_2 then
					desired = 1
					break
				end
				reg_1 = FUNC_LIST[302](loc_0)
				reg_2 = FUNC_LIST[300](loc_2)
				FUNC_LIST[303](rt_add_i32(loc_3, 8), reg_1, rt_add_i32(reg_2, 1))
				loc_4 = rt_load_i32(memory_at_0, loc_3 + 8)
				FUNC_LIST[304](loc_4, rt_load_i32(memory_at_0, loc_3 + 12))
				FUNC_LIST[307](loc_0, loc_4)
				FUNC_LIST[308](loc_0, rt_load_i32(memory_at_0, loc_3 + 12))
				FUNC_LIST[309](loc_0, loc_2)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			reg_0 = FUNC_LIST[280](loc_4)
			reg_0 = FUNC_LIST[305](reg_0, loc_1, rt_add_i32(loc_2, 1))
			FUNC_LIST[310](loc_0, loc_2)
			GLOBAL_LIST[0].value = rt_add_i32(loc_3, 16)
			desired = 0
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		FUNC_LIST[311](loc_0)
		error("out of code bounds")
	end
end
FUNC_LIST[333] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::insert(unsigned long, char const*, unsigned long) ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local loc_8 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_6 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_6
		reg_1 = FUNC_LIST[277](loc_0)
		loc_4 = reg_1
		if loc_1 <= loc_4 then
			while true do
				while true do
					reg_1 = FUNC_LIST[278](loc_0)
					loc_5 = reg_1
					if loc_3 <= rt_sub_i32(loc_5, loc_4) then
						while true do
							if loc_3 == 0 then
								desired = 2
								break
							end
							FUNC_LIST[281](loc_0, loc_3)
							reg_0 = FUNC_LIST[279](loc_0)
							reg_0 = FUNC_LIST[280](reg_0)
							loc_5 = reg_0
							reg_0 = rt_add_i32(loc_5, loc_1)
							if loc_1 ~= loc_4 then
								while true do
									loc_7 = rt_add_i32(loc_1, loc_5)
									reg_1 = FUNC_LIST[282](loc_7, rt_add_i32(loc_4, loc_5), loc_2)
									loc_8 = reg_1
									reg_1 = FUNC_LIST[283](rt_add_i32(loc_3, loc_7), loc_7, rt_sub_i32(loc_4, loc_1))
									loc_2 = rt_add_i32(loc_2, (if loc_8 ~= 0 then loc_3 else 0))
									break
								end
								if desired then
									break
								end
							end
							reg_0 = FUNC_LIST[283](reg_0, loc_2, loc_3)
							loc_3 = rt_add_i32(loc_3, loc_4)
							FUNC_LIST[294](loc_0, loc_3)
							rt_store_i32_n8(memory_at_0, loc_6 + 15, 0)
							FUNC_LIST[295](rt_add_i32(loc_3, loc_5), rt_add_i32(loc_6, 15))
							desired = 2
							break
						end
						if desired then
							if desired == 2 then
								desired = nil
							end
							break
						end
					end
					FUNC_LIST[285](loc_0, loc_5, rt_sub_i32(rt_add_i32(loc_3, loc_4), loc_5), loc_4, loc_1, 0, loc_3, loc_2)
					break
				end
				if desired then
					break
				end
				GLOBAL_LIST[0].value = rt_add_i32(loc_6, 16)
				reg_0 = loc_0
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		FUNC_LIST[286](loc_0)
		error("out of code bounds")
	end
	return reg_0
end
FUNC_LIST[334] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__assign_external(char const*, unsigned long) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local reg_0
	local desired
	while true do
		reg_0 = FUNC_LIST[278](loc_0)
		loc_3 = reg_0
		reg_0 = FUNC_LIST[277](loc_0)
		loc_4 = reg_0
		if loc_2 <= loc_3 then
			while true do
				if loc_2 > loc_4 then
					while true do
						FUNC_LIST[281](loc_0, rt_sub_i32(loc_2, loc_4))
						break
					end
					if desired then
						break
					end
				end
				reg_0 = FUNC_LIST[279](loc_0)
				reg_0 = FUNC_LIST[280](reg_0)
				loc_3 = reg_0
				reg_0 = FUNC_LIST[283](loc_3, loc_1, loc_2)
				reg_0 = FUNC_LIST[284](loc_0, loc_3, loc_2)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		FUNC_LIST[285](loc_0, loc_3, rt_sub_i32(loc_2, loc_3), loc_4, 0, loc_4, loc_2, loc_1)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[335] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__assign_external(char const*) ]] function(loc_0, loc_1)
	local reg_0
	local reg_1
	local reg_2
	while true do
		reg_2 = FUNC_LIST[324](loc_1)
		reg_0 = FUNC_LIST[334](loc_0, loc_1, reg_2)
		break
	end
	return reg_0
end
FUNC_LIST[336] = --[[ unsigned long const& std::__2::max[abi:nn180100]<unsigned long, std::__2::__less<void, void>>(unsigned long const&, unsigned long const&, std::__2::__less<void, void>) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local loc_3 = 0
	local reg_0
	while true do
		loc_2 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_2
		reg_0 = FUNC_LIST[375](rt_add_i32(loc_2, 15), loc_0, loc_1)
		loc_3 = reg_0
		GLOBAL_LIST[0].value = rt_add_i32(loc_2, 16)
		reg_0 = (if loc_3 ~= 0 then loc_1 else loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[337] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::append(char const*, unsigned long) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local reg_0
	local reg_1
	local reg_2
	local desired
	while true do
		loc_5 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_5
		while true do
			reg_1 = FUNC_LIST[278](loc_0)
			loc_4 = reg_1
			reg_2 = FUNC_LIST[277](loc_0)
			loc_3 = reg_2
			if loc_2 <= rt_sub_i32(loc_4, loc_3) then
				while true do
					if loc_2 == 0 then
						desired = 1
						break
					end
					FUNC_LIST[281](loc_0, loc_2)
					reg_0 = FUNC_LIST[279](loc_0)
					reg_0 = FUNC_LIST[280](reg_0)
					loc_4 = reg_0
					reg_0 = FUNC_LIST[305](rt_add_i32(loc_4, loc_3), loc_1, loc_2)
					loc_2 = rt_add_i32(loc_2, loc_3)
					FUNC_LIST[294](loc_0, loc_2)
					rt_store_i32_n8(memory_at_0, loc_5 + 15, 0)
					FUNC_LIST[295](rt_add_i32(loc_2, loc_4), rt_add_i32(loc_5, 15))
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[285](loc_0, loc_4, rt_add_i32(rt_sub_i32(loc_2, loc_4), loc_3), loc_3, loc_3, 0, loc_2, loc_1)
			break
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_5, 16)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[338] = --[[ std::__2::__compressed_pair_elem<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, 0, false>::__compressed_pair_elem[abi:nn180100](std::__2::__default_init_tag) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[339] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>& std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__assign_no_alias<true>(char const*, unsigned long) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local reg_0
	local desired
	while true do
		loc_4 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_4
		reg_0 = FUNC_LIST[288](loc_0)
		loc_3 = reg_0
		while true do
			if loc_2 <= 10 then
				while true do
					if loc_2 > loc_3 then
						while true do
							FUNC_LIST[281](loc_0, rt_sub_i32(loc_2, loc_3))
							break
						end
						if desired then
							break
						end
					end
					reg_0 = FUNC_LIST[292](loc_0)
					loc_5 = reg_0
					FUNC_LIST[313](loc_0, loc_2)
					reg_0 = FUNC_LIST[280](loc_5)
					reg_0 = FUNC_LIST[305](reg_0, loc_1, loc_2)
					rt_store_i32_n8(memory_at_0, loc_4 + 15, 0)
					FUNC_LIST[295](rt_add_i32(loc_2, loc_5), rt_add_i32(loc_4, 15))
					if loc_2 >= loc_3 then
						desired = 1
						break
					end
					FUNC_LIST[296](loc_0, loc_3)
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			FUNC_LIST[285](loc_0, 10, rt_sub_i32(loc_2, 10), loc_3, 0, loc_3, loc_2, loc_1)
			break
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_4, 16)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[340] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::push_back(char) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local reg_0
	local desired
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_3
		rt_store_i32_n8(memory_at_0, loc_3 + 15, loc_1)
		while true do
			while true do
				while true do
					reg_0 = FUNC_LIST[243](loc_0)
					loc_4 = reg_0
					if loc_4 == 0 then
						while true do
							loc_2 = 10
							reg_0 = FUNC_LIST[288](loc_0)
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					reg_0 = FUNC_LIST[290](loc_0)
					loc_2 = rt_sub_i32(reg_0, 1)
					reg_0 = FUNC_LIST[287](loc_0)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_1 = reg_0
				if loc_1 == loc_2 then
					while true do
						FUNC_LIST[328](loc_0, loc_2, 1, loc_2, loc_2, 0, 0)
						FUNC_LIST[281](loc_0, 1)
						reg_0 = FUNC_LIST[279](loc_0)
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				FUNC_LIST[281](loc_0, 1)
				reg_0 = FUNC_LIST[279](loc_0)
				if loc_4 ~= 0 then
					break
				end
				reg_0 = FUNC_LIST[292](loc_0)
				loc_2 = reg_0
				FUNC_LIST[313](loc_0, rt_add_i32(loc_1, 1))
				desired = 1
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			reg_0 = FUNC_LIST[291](loc_0)
			loc_2 = reg_0
			FUNC_LIST[309](loc_0, rt_add_i32(loc_1, 1))
			break
		end
		loc_0 = rt_add_i32(loc_1, loc_2)
		FUNC_LIST[295](loc_0, rt_add_i32(loc_3, 15))
		rt_store_i32_n8(memory_at_0, loc_3 + 14, 0)
		FUNC_LIST[295](rt_add_i32(loc_0, 1), rt_add_i32(loc_3, 14))
		GLOBAL_LIST[0].value = rt_add_i32(loc_3, 16)
		break
	end
end
FUNC_LIST[341] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::append(unsigned long, char) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local reg_0
	local reg_1
	local reg_2
	while true do
		loc_5 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_5
		if loc_1 ~= 0 then
			while true do
				reg_1 = FUNC_LIST[278](loc_0)
				loc_4 = reg_1
				reg_2 = FUNC_LIST[277](loc_0)
				loc_3 = reg_2
				if loc_1 > rt_sub_i32(loc_4, loc_3) then
					while true do
						FUNC_LIST[328](loc_0, loc_4, rt_add_i32(rt_sub_i32(loc_1, loc_4), loc_3), loc_3, loc_3, 0, 0)
						break
					end
				end
				FUNC_LIST[281](loc_0, loc_1)
				reg_0 = FUNC_LIST[279](loc_0)
				loc_4 = reg_0
				reg_0 = FUNC_LIST[280](loc_4)
				reg_0 = FUNC_LIST[329](rt_add_i32(reg_0, loc_3), loc_1, loc_2)
				loc_1 = rt_add_i32(loc_1, loc_3)
				FUNC_LIST[294](loc_0, loc_1)
				rt_store_i32_n8(memory_at_0, loc_5 + 15, 0)
				FUNC_LIST[295](rt_add_i32(loc_1, loc_4), rt_add_i32(loc_5, 15))
				break
			end
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_5, 16)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[342] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::operator[][abi:nn180100](unsigned long) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[279](loc_0)
		reg_0 = rt_add_i32(reg_0, loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[343] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::resize(unsigned long, char) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		reg_0 = FUNC_LIST[277](loc_0)
		loc_3 = reg_0
		if loc_3 < loc_1 then
			while true do
				reg_0 = FUNC_LIST[341](loc_0, rt_sub_i32(loc_1, loc_3), loc_2)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		FUNC_LIST[344](loc_0, loc_1)
		break
	end
end
FUNC_LIST[344] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__erase_to_end[abi:nn180100](unsigned long) ]] function(loc_0, loc_1)
	local reg_0
	local reg_1
	while true do
		reg_1 = FUNC_LIST[279](loc_0)
		reg_1 = FUNC_LIST[280](reg_1)
		reg_0 = FUNC_LIST[284](loc_0, reg_1, loc_1)
		break
	end
end
FUNC_LIST[345] = --[[ std::__2::__compressed_pair<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, std::__2::allocator<char>>::__compressed_pair[abi:nn180100]<std::__2::__default_init_tag, std::__2::__default_init_tag>(std::__2::__default_init_tag&&, std::__2::__default_init_tag&&) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[338](loc_0)
		reg_0 = FUNC_LIST[416](reg_0)
		break
	end
	return reg_0
end
FUNC_LIST[346] = --[[ std::__2::to_string(int) ]] function(loc_0, loc_1)
	while true do
		FUNC_LIST[347](loc_0, loc_1)
		break
	end
end
FUNC_LIST[347] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> std::__2::(anonymous namespace)::i_to_string<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, int>(int) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_sub_i32(GLOBAL_LIST[0].value, 32)
		GLOBAL_LIST[0].value = loc_2
		FUNC_LIST[348](rt_add_i32(loc_2, 12), rt_add_i32(loc_2, 21), rt_add_i32(loc_2, 32), loc_1)
		reg_0 = FUNC_LIST[349](loc_0, rt_add_i32(loc_2, 21), rt_load_i32(memory_at_0, loc_2 + 12))
		GLOBAL_LIST[0].value = rt_add_i32(loc_2, 32)
		break
	end
end
FUNC_LIST[348] = --[[ std::__2::to_chars_result std::__2::to_chars[abi:nn180100]<int, 0>(char*, char*, int) ]] function(loc_0, loc_1, loc_2, loc_3)
	while true do
		FUNC_LIST[421](loc_0, loc_1, loc_2, loc_3)
		break
	end
end
FUNC_LIST[349] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::basic_string[abi:nn180100]<char*, 0>(char*, char*) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_3
		reg_0 = FUNC_LIST[345](loc_0, rt_add_i32(loc_3, 15), rt_add_i32(loc_3, 14))
		loc_0 = reg_0
		FUNC_LIST[422](loc_0, loc_1, loc_2)
		GLOBAL_LIST[0].value = rt_add_i32(loc_3, 16)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[350] = --[[ std::__2::to_string(long) ]] function(loc_0, loc_1)
	while true do
		FUNC_LIST[351](loc_0, loc_1)
		break
	end
end
FUNC_LIST[351] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> std::__2::(anonymous namespace)::i_to_string<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, long>(long) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_sub_i32(GLOBAL_LIST[0].value, 32)
		GLOBAL_LIST[0].value = loc_2
		FUNC_LIST[352](rt_add_i32(loc_2, 12), rt_add_i32(loc_2, 21), rt_add_i32(loc_2, 32), loc_1)
		reg_0 = FUNC_LIST[349](loc_0, rt_add_i32(loc_2, 21), rt_load_i32(memory_at_0, loc_2 + 12))
		GLOBAL_LIST[0].value = rt_add_i32(loc_2, 32)
		break
	end
end
FUNC_LIST[352] = --[[ std::__2::to_chars_result std::__2::to_chars[abi:nn180100]<long, 0>(char*, char*, long) ]] function(loc_0, loc_1, loc_2, loc_3)
	while true do
		FUNC_LIST[421](loc_0, loc_1, loc_2, loc_3)
		break
	end
end
FUNC_LIST[353] = --[[ std::__2::to_string(long long) ]] function(loc_0, loc_1)
	while true do
		FUNC_LIST[354](loc_0, loc_1)
		break
	end
end
FUNC_LIST[354] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> std::__2::(anonymous namespace)::i_to_string<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, long long>(long long) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_sub_i32(GLOBAL_LIST[0].value, 48)
		GLOBAL_LIST[0].value = loc_2
		FUNC_LIST[355](rt_add_i32(loc_2, 8), rt_add_i32(loc_2, 16), rt_add_i32(loc_2, 36), loc_1)
		reg_0 = FUNC_LIST[349](loc_0, rt_add_i32(loc_2, 16), rt_load_i32(memory_at_0, loc_2 + 8))
		GLOBAL_LIST[0].value = rt_add_i32(loc_2, 48)
		break
	end
end
FUNC_LIST[355] = --[[ std::__2::to_chars_result std::__2::to_chars[abi:nn180100]<long long, 0>(char*, char*, long long) ]] function(loc_0, loc_1, loc_2, loc_3)
	while true do
		FUNC_LIST[444](loc_0, loc_1, loc_2, loc_3)
		break
	end
end
FUNC_LIST[356] = --[[ std::__2::to_string(unsigned int) ]] function(loc_0, loc_1)
	while true do
		FUNC_LIST[357](loc_0, loc_1)
		break
	end
end
FUNC_LIST[357] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> std::__2::(anonymous namespace)::i_to_string<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, unsigned int>(unsigned int) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_sub_i32(GLOBAL_LIST[0].value, 32)
		GLOBAL_LIST[0].value = loc_2
		FUNC_LIST[358](rt_add_i32(loc_2, 12), rt_add_i32(loc_2, 21), rt_add_i32(loc_2, 32), loc_1)
		reg_0 = FUNC_LIST[349](loc_0, rt_add_i32(loc_2, 21), rt_load_i32(memory_at_0, loc_2 + 12))
		GLOBAL_LIST[0].value = rt_add_i32(loc_2, 32)
		break
	end
end
FUNC_LIST[358] = --[[ std::__2::to_chars_result std::__2::to_chars[abi:nn180100]<unsigned int, 0>(char*, char*, unsigned int) ]] function(loc_0, loc_1, loc_2, loc_3)
	while true do
		FUNC_LIST[425](loc_0, loc_1, loc_2, loc_3)
		break
	end
end
FUNC_LIST[359] = --[[ std::__2::to_string(unsigned long) ]] function(loc_0, loc_1)
	while true do
		FUNC_LIST[360](loc_0, loc_1)
		break
	end
end
FUNC_LIST[360] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> std::__2::(anonymous namespace)::i_to_string<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, unsigned long>(unsigned long) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_sub_i32(GLOBAL_LIST[0].value, 32)
		GLOBAL_LIST[0].value = loc_2
		FUNC_LIST[361](rt_add_i32(loc_2, 12), rt_add_i32(loc_2, 21), rt_add_i32(loc_2, 32), loc_1)
		reg_0 = FUNC_LIST[349](loc_0, rt_add_i32(loc_2, 21), rt_load_i32(memory_at_0, loc_2 + 12))
		GLOBAL_LIST[0].value = rt_add_i32(loc_2, 32)
		break
	end
end
FUNC_LIST[361] = --[[ std::__2::to_chars_result std::__2::to_chars[abi:nn180100]<unsigned long, 0>(char*, char*, unsigned long) ]] function(loc_0, loc_1, loc_2, loc_3)
	while true do
		FUNC_LIST[425](loc_0, loc_1, loc_2, loc_3)
		break
	end
end
FUNC_LIST[362] = --[[ std::__2::to_string(unsigned long long) ]] function(loc_0, loc_1)
	while true do
		FUNC_LIST[363](loc_0, loc_1)
		break
	end
end
FUNC_LIST[363] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> std::__2::(anonymous namespace)::i_to_string<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, unsigned long long>(unsigned long long) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_sub_i32(GLOBAL_LIST[0].value, 48)
		GLOBAL_LIST[0].value = loc_2
		FUNC_LIST[364](rt_add_i32(loc_2, 8), rt_add_i32(loc_2, 16), rt_add_i32(loc_2, 37), loc_1)
		reg_0 = FUNC_LIST[349](loc_0, rt_add_i32(loc_2, 16), rt_load_i32(memory_at_0, loc_2 + 8))
		GLOBAL_LIST[0].value = rt_add_i32(loc_2, 48)
		break
	end
end
FUNC_LIST[364] = --[[ std::__2::to_chars_result std::__2::to_chars[abi:nn180100]<unsigned long long, 0>(char*, char*, unsigned long long) ]] function(loc_0, loc_1, loc_2, loc_3)
	while true do
		FUNC_LIST[447](loc_0, loc_1, loc_2, loc_3)
		break
	end
end
FUNC_LIST[365] = --[[ std::__2::to_string(float) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_2
		FUNC_LIST[366](rt_add_i32(loc_2, 4))
		FUNC_LIST[367](loc_0, rt_add_i32(loc_2, 4), loc_1)
		reg_0 = FUNC_LIST[326](rt_add_i32(loc_2, 4))
		GLOBAL_LIST[0].value = rt_add_i32(loc_2, 16)
		break
	end
end
FUNC_LIST[366] = --[[ std::__2::(anonymous namespace)::initial_string<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>>::operator()() const ]] function(loc_0)
	local reg_0
	local reg_1
	while true do
		reg_0 = FUNC_LIST[370](loc_0)
		loc_0 = reg_0
		reg_1 = FUNC_LIST[278](loc_0)
		FUNC_LIST[368](loc_0, reg_1)
		break
	end
end
FUNC_LIST[367] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> std::__2::(anonymous namespace)::as_string<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, int (*)(char*, unsigned long, char const*, ...), float>(int (*)(char*, unsigned long, char const*, ...), std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::value_type const*, float) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0.0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_5 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_5
		loc_6 = no_op(loc_2)
		reg_0 = FUNC_LIST[277](loc_1)
		loc_4 = reg_0
		while true do
			while true do
				reg_0 = FUNC_LIST[342](loc_1, 0)
				loc_3 = reg_0
				rt_store_f64(memory_at_0, loc_5, loc_6)
				reg_0 = loc_1
				while true do
					reg_1 = FUNC_LIST[276](loc_3, rt_add_i32(loc_4, 1), 1205, loc_5)
					loc_3 = reg_1
					if rt_ge_i32(loc_3, 0) then
						while true do
							if loc_3 <= loc_4 then
								desired = 2
								break
							end
							reg_1 = loc_3
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					reg_1 = bit_or(rt_shl_i32(loc_4, 1), 1)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_4 = reg_1
				FUNC_LIST[368](reg_0, loc_4)
				desired = 1
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
					continue
				end
				break
			end
			break
		end
		FUNC_LIST[368](loc_1, loc_3)
		reg_0 = FUNC_LIST[369](loc_0, loc_1)
		GLOBAL_LIST[0].value = rt_add_i32(loc_5, 16)
		break
	end
end
FUNC_LIST[368] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::resize[abi:nn180100](unsigned long) ]] function(loc_0, loc_1)
	while true do
		FUNC_LIST[343](loc_0, loc_1, 0)
		break
	end
end
FUNC_LIST[369] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::basic_string[abi:nn180100](std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>&&) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local loc_3 = 0
	local reg_0
	local reg_1
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_3
		reg_1 = FUNC_LIST[453](rt_add_i32(loc_3, 15), loc_1)
		loc_2 = reg_1
		rt_store_i64(memory_at_0, loc_0, rt_load_i64(memory_at_0, loc_2))
		rt_store_i32(memory_at_0, loc_0 + 8, rt_load_i32(memory_at_0, loc_2 + 8))
		reg_0 = FUNC_LIST[318](loc_1)
		loc_2 = reg_0
		rt_store_i64(memory_at_0, loc_2, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_2 + 8, 0)
		FUNC_LIST[310](loc_1, 0)
		reg_0 = FUNC_LIST[243](loc_0)
		if reg_0 == 0 then
			while true do
				reg_1 = FUNC_LIST[277](loc_0)
				FUNC_LIST[310](loc_0, reg_1)
				break
			end
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_3, 16)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[370] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::basic_string[abi:nn180100]() ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		loc_1 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_1
		reg_0 = FUNC_LIST[419](loc_0, rt_add_i32(loc_1, 15), rt_add_i32(loc_1, 14))
		loc_0 = reg_0
		FUNC_LIST[310](loc_0, 0)
		GLOBAL_LIST[0].value = rt_add_i32(loc_1, 16)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[371] = --[[ std::__2::to_string(double) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_2
		FUNC_LIST[366](rt_add_i32(loc_2, 4))
		FUNC_LIST[372](loc_0, rt_add_i32(loc_2, 4), loc_1)
		reg_0 = FUNC_LIST[326](rt_add_i32(loc_2, 4))
		GLOBAL_LIST[0].value = rt_add_i32(loc_2, 16)
		break
	end
end
FUNC_LIST[372] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> std::__2::(anonymous namespace)::as_string<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, int (*)(char*, unsigned long, char const*, ...), double>(int (*)(char*, unsigned long, char const*, ...), std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::value_type const*, double) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_5 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_5
		reg_0 = FUNC_LIST[277](loc_1)
		loc_4 = reg_0
		while true do
			while true do
				reg_0 = FUNC_LIST[342](loc_1, 0)
				loc_3 = reg_0
				rt_store_f64(memory_at_0, loc_5, loc_2)
				reg_0 = loc_1
				while true do
					reg_1 = FUNC_LIST[276](loc_3, rt_add_i32(loc_4, 1), 1205, loc_5)
					loc_3 = reg_1
					if rt_ge_i32(loc_3, 0) then
						while true do
							if loc_3 <= loc_4 then
								desired = 2
								break
							end
							reg_1 = loc_3
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					reg_1 = bit_or(rt_shl_i32(loc_4, 1), 1)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_4 = reg_1
				FUNC_LIST[368](reg_0, loc_4)
				desired = 1
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
					continue
				end
				break
			end
			break
		end
		FUNC_LIST[368](loc_1, loc_3)
		reg_0 = FUNC_LIST[369](loc_0, loc_1)
		GLOBAL_LIST[0].value = rt_add_i32(loc_5, 16)
		break
	end
end
FUNC_LIST[373] = --[[ std::__2::to_string(long double) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_3
		FUNC_LIST[366](rt_add_i32(loc_3, 4))
		FUNC_LIST[374](loc_0, rt_add_i32(loc_3, 4), loc_1, loc_2)
		reg_0 = FUNC_LIST[326](rt_add_i32(loc_3, 4))
		GLOBAL_LIST[0].value = rt_add_i32(loc_3, 16)
		break
	end
end
FUNC_LIST[374] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>> std::__2::(anonymous namespace)::as_string<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, int (*)(char*, unsigned long, char const*, ...), long double>(int (*)(char*, unsigned long, char const*, ...), std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>, std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::value_type const*, long double) ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local reg_0
	local reg_1
	local desired
	while true do
		loc_5 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_5
		reg_0 = FUNC_LIST[277](loc_1)
		loc_6 = reg_0
		while true do
			while true do
				reg_0 = FUNC_LIST[342](loc_1, 0)
				loc_4 = reg_0
				rt_store_i64(memory_at_0, loc_5 + 8, loc_3)
				rt_store_i64(memory_at_0, loc_5, loc_2)
				reg_0 = loc_1
				while true do
					reg_1 = FUNC_LIST[276](loc_4, rt_add_i32(loc_6, 1), 1201, loc_5)
					loc_4 = reg_1
					if rt_ge_i32(loc_4, 0) then
						while true do
							if loc_4 <= loc_6 then
								desired = 2
								break
							end
							reg_1 = loc_4
							desired = 3
							break
						end
						if desired then
							if desired == 3 then
								desired = nil
							end
							break
						end
					end
					reg_1 = bit_or(rt_shl_i32(loc_6, 1), 1)
					break
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_6 = reg_1
				FUNC_LIST[368](reg_0, loc_6)
				desired = 1
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
					continue
				end
				break
			end
			break
		end
		FUNC_LIST[368](loc_1, loc_4)
		reg_0 = FUNC_LIST[369](loc_0, loc_1)
		GLOBAL_LIST[0].value = rt_add_i32(loc_5, 16)
		break
	end
end
FUNC_LIST[375] = --[[ bool std::__2::__less<void, void>::operator()[abi:nn180100]<unsigned long, unsigned long>(unsigned long const&, unsigned long const&) const ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = (if rt_load_i32(memory_at_0, loc_1) < rt_load_i32(memory_at_0, loc_2) then 1 else 0)
		break
	end
	return reg_0
end
FUNC_LIST[376] = --[[ std::__2::__compressed_pair<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, std::__2::allocator<char>>::second[abi:nn180100]() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[378](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[377] = --[[ std::__2::__libcpp_numeric_limits<unsigned long, true>::max[abi:nn180100]() ]] function()
	local reg_0
	while true do
		reg_0 = 4294967295
		break
	end
	return reg_0
end
FUNC_LIST[378] = --[[ std::__2::__compressed_pair_elem<std::__2::allocator<char>, 1, true>::__get[abi:nn180100]() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[379] = --[[ std::__2::__compressed_pair_elem<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, 0, false>::__get[abi:nn180100]() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[380] = --[[ std::__throw_bad_array_new_length[abi:nn180100]() ]] function()
	while true do
		FUNC_LIST[2]()
		error("out of code bounds")
	end
end
FUNC_LIST[381] = --[[ std::__2::__libcpp_allocate[abi:nn180100](unsigned long, unsigned long) ]] function(loc_0, loc_1)
	local reg_0
	local desired
	while true do
		reg_0 = FUNC_LIST[382](loc_1)
		if reg_0 ~= 0 then
			while true do
				reg_0 = FUNC_LIST[383](loc_0, loc_1)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		reg_0 = FUNC_LIST[384](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[382] = --[[ std::__2::__is_overaligned_for_new[abi:nn180100](unsigned long) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = (if loc_0 > 8 then 1 else 0)
		break
	end
	return reg_0
end
FUNC_LIST[383] = --[[ void* std::__2::__libcpp_operator_new[abi:nn180100]<unsigned long, std::align_val_t>(unsigned long, std::align_val_t) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[231](loc_0, loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[384] = --[[ void* std::__2::__libcpp_operator_new[abi:nn180100]<unsigned long>(unsigned long) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[227](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[385] = --[[ std::__2::__compressed_pair_elem<std::__2::allocator<char>, 1, true>::__get[abi:nn180100]() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[386] = --[[ char* std::__2::copy[abi:nn180100]<char const*, char*>(char const*, char const*, char*) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_3
		FUNC_LIST[387](rt_add_i32(loc_3, 8), loc_0, loc_1, loc_2)
		loc_2 = rt_load_i32(memory_at_0, loc_3 + 12)
		GLOBAL_LIST[0].value = rt_add_i32(loc_3, 16)
		reg_0 = loc_2
		break
	end
	return reg_0
end
FUNC_LIST[387] = --[[ std::__2::pair<char const*, char*> std::__2::__copy[abi:nn180100]<std::__2::_ClassicAlgPolicy, char const*, char const*, char*>(char const*, char const*, char*) ]] function(loc_0, loc_1, loc_2, loc_3)
	while true do
		FUNC_LIST[388](loc_0, loc_1, loc_2, loc_3)
		break
	end
end
FUNC_LIST[388] = --[[ std::__2::pair<char const*, char*> std::__2::__dispatch_copy_or_move[abi:nn180100]<std::__2::_ClassicAlgPolicy, std::__2::__copy_loop<std::__2::_ClassicAlgPolicy>, std::__2::__copy_trivial, char const*, char const*, char*>(char const*, char const*, char*) ]] function(loc_0, loc_1, loc_2, loc_3)
	while true do
		FUNC_LIST[389](loc_0, loc_1, loc_2, loc_3)
		break
	end
end
FUNC_LIST[389] = --[[ std::__2::pair<char const*, char*> std::__2::__unwrap_and_dispatch[abi:nn180100]<std::__2::__overload<std::__2::__copy_loop<std::__2::_ClassicAlgPolicy>, std::__2::__copy_trivial>, char const*, char const*, char*, 0>(char const*, char const*, char*) ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = 0
	local reg_0
	local reg_1
	local reg_2
	local reg_3
	local reg_4
	while true do
		loc_4 = rt_sub_i32(GLOBAL_LIST[0].value, 32)
		GLOBAL_LIST[0].value = loc_4
		FUNC_LIST[390](rt_add_i32(loc_4, 24), loc_1, loc_2)
		reg_2 = rt_load_i32(memory_at_0, loc_4 + 24)
		reg_3 = rt_load_i32(memory_at_0, loc_4 + 28)
		reg_4 = FUNC_LIST[391](loc_3)
		FUNC_LIST[392](rt_add_i32(loc_4, 16), rt_add_i32(loc_4, 12), reg_2, reg_3, reg_4)
		reg_1 = FUNC_LIST[393](loc_1, rt_load_i32(memory_at_0, loc_4 + 16))
		rt_store_i32(memory_at_0, loc_4 + 12, reg_1)
		reg_1 = FUNC_LIST[394](loc_3, rt_load_i32(memory_at_0, loc_4 + 20))
		rt_store_i32(memory_at_0, loc_4 + 8, reg_1)
		FUNC_LIST[395](loc_0, rt_add_i32(loc_4, 12), rt_add_i32(loc_4, 8))
		GLOBAL_LIST[0].value = rt_add_i32(loc_4, 32)
		break
	end
end
FUNC_LIST[390] = --[[ auto std::__2::__unwrap_range[abi:nn180100]<char const*, char const*>(char const*, char const*) ]] function(loc_0, loc_1, loc_2)
	while true do
		FUNC_LIST[396](loc_0, loc_1, loc_2)
		break
	end
end
FUNC_LIST[391] = --[[ decltype(std::__2::__unwrap_iter_impl<char*, true>::__unwrap(std::declval<char*>())) std::__2::__unwrap_iter[abi:nn180100]<char*, std::__2::__unwrap_iter_impl<char*, true>, 0>(char*) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[398](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[392] = --[[ std::__2::pair<char const*, char*> std::__2::__copy_trivial::operator()[abi:nn180100]<char const, char, 0>(char const*, char const*, char*) const ]] function(loc_0, loc_1, loc_2, loc_3, loc_4)
	while true do
		FUNC_LIST[397](loc_0, loc_2, loc_3, loc_4)
		break
	end
end
FUNC_LIST[393] = --[[ char const* std::__2::__rewrap_range[abi:nn180100]<char const*, char const*, char const*>(char const*, char const*) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[400](loc_0, loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[394] = --[[ char* std::__2::__rewrap_iter[abi:nn180100]<char*, char*, std::__2::__unwrap_iter_impl<char*, true>>(char*, char*) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[401](loc_0, loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[395] = --[[ std::__2::pair<std::__2::__unwrap_ref_decay<char const*>::type, std::__2::__unwrap_ref_decay<char*>::type> std::__2::make_pair[abi:nn180100]<char const*, char*>(char const*&&, char*&&) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[399](loc_0, loc_1, loc_2)
		break
	end
end
FUNC_LIST[396] = --[[ std::__2::__unwrap_range_impl<char const*, char const*>::__unwrap[abi:nn180100](char const*, char const*) ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local reg_0
	local reg_1
	while true do
		loc_3 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_3
		reg_1 = FUNC_LIST[402](loc_1)
		rt_store_i32(memory_at_0, loc_3 + 12, reg_1)
		reg_1 = FUNC_LIST[402](loc_2)
		rt_store_i32(memory_at_0, loc_3 + 8, reg_1)
		reg_0 = FUNC_LIST[403](loc_0, rt_add_i32(loc_3, 12), rt_add_i32(loc_3, 8))
		GLOBAL_LIST[0].value = rt_add_i32(loc_3, 16)
		break
	end
end
FUNC_LIST[397] = --[[ std::__2::pair<char const*, char*> std::__2::__copy_trivial_impl[abi:nn180100]<char const, char>(char const*, char const*, char*) ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = 0
	local reg_0
	while true do
		loc_4 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_4
		rt_store_i32(memory_at_0, loc_4 + 12, loc_2)
		loc_2 = rt_sub_i32(loc_2, loc_1)
		reg_0 = FUNC_LIST[293](loc_3, loc_1, loc_2)
		rt_store_i32(memory_at_0, loc_4 + 8, rt_add_i32(loc_2, loc_3))
		FUNC_LIST[405](loc_0, rt_add_i32(loc_4, 12), rt_add_i32(loc_4, 8))
		GLOBAL_LIST[0].value = rt_add_i32(loc_4, 16)
		break
	end
end
FUNC_LIST[398] = --[[ std::__2::__unwrap_iter_impl<char*, true>::__unwrap[abi:nn180100](char*) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[280](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[399] = --[[ std::__2::pair<char const*, char*>::pair[abi:nn180100]<char const*, char*, 0>(char const*&&, char*&&) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		rt_store_i32(memory_at_0, loc_0, rt_load_i32(memory_at_0, loc_1))
		rt_store_i32(memory_at_0, loc_0 + 4, rt_load_i32(memory_at_0, loc_2))
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[400] = --[[ std::__2::__unwrap_range_impl<char const*, char const*>::__rewrap[abi:nn180100](char const*, char const*) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[407](loc_0, loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[401] = --[[ std::__2::__unwrap_iter_impl<char*, true>::__rewrap[abi:nn180100](char*, char*) ]] function(loc_0, loc_1)
	local reg_0
	local reg_1
	local reg_2
	while true do
		reg_2 = FUNC_LIST[280](loc_0)
		reg_0 = rt_add_i32(loc_0, rt_sub_i32(loc_1, reg_2))
		break
	end
	return reg_0
end
FUNC_LIST[402] = --[[ decltype(std::__2::__unwrap_iter_impl<char const*, true>::__unwrap(std::declval<char const*>())) std::__2::__unwrap_iter[abi:nn180100]<char const*, std::__2::__unwrap_iter_impl<char const*, true>, 0>(char const*) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[404](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[403] = --[[ std::__2::pair<char const*, char const*>::pair[abi:nn180100]<char const*, char const*, 0>(char const*&&, char const*&&) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		rt_store_i32(memory_at_0, loc_0, rt_load_i32(memory_at_0, loc_1))
		rt_store_i32(memory_at_0, loc_0 + 4, rt_load_i32(memory_at_0, loc_2))
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[404] = --[[ std::__2::__unwrap_iter_impl<char const*, true>::__unwrap[abi:nn180100](char const*) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[242](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[405] = --[[ std::__2::pair<std::__2::__unwrap_ref_decay<char const*&>::type, std::__2::__unwrap_ref_decay<char*>::type> std::__2::make_pair[abi:nn180100]<char const*&, char*>(char const*&, char*&&) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[406](loc_0, loc_1, loc_2)
		break
	end
end
FUNC_LIST[406] = --[[ std::__2::pair<char const*, char*>::pair[abi:nn180100]<char const*&, char*, 0>(char const*&, char*&&) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		rt_store_i32(memory_at_0, loc_0, rt_load_i32(memory_at_0, loc_1))
		rt_store_i32(memory_at_0, loc_0 + 4, rt_load_i32(memory_at_0, loc_2))
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[407] = --[[ char const* std::__2::__rewrap_iter[abi:nn180100]<char const*, char const*, std::__2::__unwrap_iter_impl<char const*, true>>(char const*, char const*) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[408](loc_0, loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[408] = --[[ std::__2::__unwrap_iter_impl<char const*, true>::__rewrap[abi:nn180100](char const*, char const*) ]] function(loc_0, loc_1)
	local reg_0
	local reg_1
	local reg_2
	while true do
		reg_2 = FUNC_LIST[242](loc_0)
		reg_0 = rt_add_i32(loc_0, rt_sub_i32(loc_1, reg_2))
		break
	end
	return reg_0
end
FUNC_LIST[409] = --[[ std::__2::__libcpp_deallocate[abi:nn180100](void*, unsigned long, unsigned long) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	local desired
	while true do
		reg_0 = FUNC_LIST[382](loc_2)
		if reg_0 ~= 0 then
			while true do
				FUNC_LIST[410](loc_0, loc_1, loc_2)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		FUNC_LIST[411](loc_0, loc_1)
		break
	end
end
FUNC_LIST[410] = --[[ void std::__2::__do_deallocate_handle_size[abi:nn180100]<std::align_val_t>(void*, unsigned long, std::align_val_t) ]] function(loc_0, loc_1, loc_2)
	while true do
		FUNC_LIST[412](loc_0, loc_2)
		break
	end
end
FUNC_LIST[411] = --[[ void std::__2::__do_deallocate_handle_size[abi:nn180100]<>(void*, unsigned long) ]] function(loc_0, loc_1)
	while true do
		FUNC_LIST[413](loc_0)
		break
	end
end
FUNC_LIST[412] = --[[ void std::__2::__libcpp_operator_delete[abi:nn180100]<void*, std::align_val_t>(void*, std::align_val_t) ]] function(loc_0, loc_1)
	while true do
		FUNC_LIST[234](loc_0, loc_1)
		break
	end
end
FUNC_LIST[413] = --[[ void std::__2::__libcpp_operator_delete[abi:nn180100]<void*>(void*) ]] function(loc_0)
	while true do
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[414] = --[[ std::__2::__convert_to_integral[abi:nn180100](unsigned long) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[415] = --[[ char* std::__2::__fill_n[abi:nn180100]<char*, unsigned long, char>(char*, unsigned long, char const&) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	local desired
	while true do
		while true do
			if loc_1 ~= 0 then
				while true do
					rt_store_i32_n8(memory_at_0, loc_0, rt_load_i32_u8(memory_at_0, loc_2))
					loc_1 = rt_sub_i32(loc_1, 1)
					loc_0 = rt_add_i32(loc_0, 1)
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
						continue
					end
					break
				end
			end
			break
		end
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[416] = --[[ std::__2::__compressed_pair_elem<std::__2::allocator<char>, 1, true>::__compressed_pair_elem[abi:nn180100](std::__2::__default_init_tag) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[417](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[417] = --[[ std::__2::allocator<char>::allocator[abi:nn180100]() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[418](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[418] = --[[ std::__2::__non_trivial_if<true, std::__2::allocator<char>>::__non_trivial_if[abi:nn180100]() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[419] = --[[ std::__2::__compressed_pair<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, std::__2::allocator<char>>::__compressed_pair[abi:nn180100]<std::__2::__value_init_tag, std::__2::__default_init_tag>(std::__2::__value_init_tag&&, std::__2::__default_init_tag&&) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[420](loc_0)
		reg_0 = FUNC_LIST[416](reg_0)
		break
	end
	return reg_0
end
FUNC_LIST[420] = --[[ std::__2::__compressed_pair_elem<std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__rep, 0, false>::__compressed_pair_elem[abi:nn180100](std::__2::__value_init_tag) ]] function(loc_0)
	local reg_0
	while true do
		rt_store_i64(memory_at_0, loc_0, rt_i64_ZERO)
		rt_store_i32(memory_at_0, loc_0 + 8, 0)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[421] = --[[ std::__2::to_chars_result std::__2::__to_chars_itoa[abi:nn180100]<int>(char*, char*, int, std::__2::integral_constant<bool, true>) ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = 0
	local reg_0
	while true do
		reg_0 = FUNC_LIST[423](loc_3)
		loc_4 = reg_0
		while true do
			if loc_1 == loc_2 then
				break
			end
			if rt_ge_i32(loc_3, 0) then
				break
			end
			rt_store_i32_n8(memory_at_0, loc_1, 45)
			loc_1 = rt_add_i32(loc_1, 1)
			reg_0 = FUNC_LIST[424](loc_4)
			loc_4 = reg_0
			break
		end
		FUNC_LIST[425](loc_0, loc_1, loc_2, loc_4)
		break
	end
end
FUNC_LIST[422] = --[[ void std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__init<char*, 0>(char*, char*) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	local reg_1
	local reg_2
	local reg_3
	while true do
		reg_3 = FUNC_LIST[441](loc_1, loc_2)
		FUNC_LIST[442](loc_0, loc_1, loc_2, reg_3)
		break
	end
end
FUNC_LIST[423] = --[[ __make_unsigned(int) std::__2::__to_unsigned_like[abi:nn180100]<int>(int) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[424] = --[[ unsigned int std::__2::__complement[abi:nn180100]<unsigned int>(unsigned int) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = rt_sub_i32(0, loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[425] = --[[ std::__2::to_chars_result std::__2::__to_chars_itoa[abi:nn180100]<unsigned int>(char*, char*, unsigned int, std::__2::integral_constant<bool, false>) ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = 0
	local reg_0
	local reg_1
	local reg_2
	local desired
	while true do
		reg_0 = loc_0
		while true do
			loc_4 = rt_sub_i32(loc_2, loc_1)
			if rt_le_i32(loc_4, 9) then
				while true do
					reg_2 = FUNC_LIST[426](loc_3)
					reg_1 = 61
					if rt_gt_i32(reg_2, loc_4) then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			reg_1 = FUNC_LIST[427](loc_1, loc_3)
			loc_2 = reg_1
			reg_1 = 0
			break
		end
		rt_store_i32(memory_at_0, reg_0 + 4, reg_1)
		rt_store_i32(memory_at_0, loc_0, loc_2)
		break
	end
end
FUNC_LIST[426] = --[[ std::__2::__itoa::__traits_base<unsigned int, void>::__width[abi:nn180100](unsigned int) ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	local reg_1
	while true do
		reg_1 = FUNC_LIST[428](bit_or(loc_0, 1))
		loc_1 = rt_shr_i32(rt_mul_i32(rt_sub_i32(32, reg_1), 1233), 12)
		reg_0 = rt_add_i32(loc_1, (if rt_load_i32(memory_at_0, rt_add_i32(rt_shl_i32(loc_1, 2), 8976)) <= loc_0 then 1 else 0))
		break
	end
	return reg_0
end
FUNC_LIST[427] = --[[ std::__2::__itoa::__traits_base<unsigned int, void>::__convert[abi:nn180100](char*, unsigned int) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[429](loc_0, loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[428] = --[[ std::__2::__libcpp_clz[abi:nn180100](unsigned int) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = bit_countlz(loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[429] = --[[ std::__2::__itoa::__base_10_u32[abi:nn180100](char*, unsigned int) ]] function(loc_0, loc_1)
	local reg_0
	local desired
	while true do
		if loc_1 <= 999999 then
			while true do
				if loc_1 <= 9999 then
					while true do
						if loc_1 <= 99 then
							while true do
								if loc_1 <= 9 then
									while true do
										reg_0 = FUNC_LIST[430](loc_0, loc_1)
										desired = 0
										break
									end
									if desired then
										break
									end
								end
								reg_0 = FUNC_LIST[431](loc_0, loc_1)
								desired = 0
								break
							end
							if desired then
								break
							end
						end
						if loc_1 <= 999 then
							while true do
								reg_0 = FUNC_LIST[432](loc_0, loc_1)
								desired = 0
								break
							end
							if desired then
								break
							end
						end
						reg_0 = FUNC_LIST[433](loc_0, loc_1)
						desired = 0
						break
					end
					if desired then
						break
					end
				end
				if loc_1 <= 99999 then
					while true do
						reg_0 = FUNC_LIST[434](loc_0, loc_1)
						desired = 0
						break
					end
					if desired then
						break
					end
				end
				reg_0 = FUNC_LIST[435](loc_0, loc_1)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		if loc_1 <= 99999999 then
			while true do
				if loc_1 <= 9999999 then
					while true do
						reg_0 = FUNC_LIST[436](loc_0, loc_1)
						desired = 0
						break
					end
					if desired then
						break
					end
				end
				reg_0 = FUNC_LIST[437](loc_0, loc_1)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		if loc_1 <= 999999999 then
			while true do
				reg_0 = FUNC_LIST[438](loc_0, loc_1)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		reg_0 = FUNC_LIST[439](loc_0, loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[430] = --[[ std::__2::__itoa::__append1[abi:nn180100](char*, unsigned int) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		rt_store_i32_n8(memory_at_0, loc_0, rt_add_i32(loc_1, 48))
		reg_0 = rt_add_i32(loc_0, 1)
		break
	end
	return reg_0
end
FUNC_LIST[431] = --[[ std::__2::__itoa::__append2[abi:nn180100](char*, unsigned int) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[440](rt_add_i32(rt_shl_i32(loc_1, 1), 9024), 2, loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[432] = --[[ std::__2::__itoa::__append3[abi:nn180100](char*, unsigned int) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_div_u32(loc_1, 100)
		reg_0 = FUNC_LIST[430](loc_0, loc_2)
		reg_0 = FUNC_LIST[431](reg_0, rt_sub_i32(loc_1, rt_mul_i32(loc_2, 100)))
		break
	end
	return reg_0
end
FUNC_LIST[433] = --[[ std::__2::__itoa::__append4[abi:nn180100](char*, unsigned int) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_div_u32(loc_1, 100)
		reg_0 = FUNC_LIST[431](loc_0, loc_2)
		reg_0 = FUNC_LIST[431](reg_0, rt_sub_i32(loc_1, rt_mul_i32(loc_2, 100)))
		break
	end
	return reg_0
end
FUNC_LIST[434] = --[[ std::__2::__itoa::__append5[abi:nn180100](char*, unsigned int) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_div_u32(loc_1, 10000)
		reg_0 = FUNC_LIST[430](loc_0, loc_2)
		reg_0 = FUNC_LIST[433](reg_0, rt_sub_i32(loc_1, rt_mul_i32(loc_2, 10000)))
		break
	end
	return reg_0
end
FUNC_LIST[435] = --[[ std::__2::__itoa::__append6[abi:nn180100](char*, unsigned int) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_div_u32(loc_1, 10000)
		reg_0 = FUNC_LIST[431](loc_0, loc_2)
		reg_0 = FUNC_LIST[433](reg_0, rt_sub_i32(loc_1, rt_mul_i32(loc_2, 10000)))
		break
	end
	return reg_0
end
FUNC_LIST[436] = --[[ std::__2::__itoa::__append7[abi:nn180100](char*, unsigned int) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_div_u32(loc_1, 1000000)
		reg_0 = FUNC_LIST[430](loc_0, loc_2)
		reg_0 = FUNC_LIST[435](reg_0, rt_sub_i32(loc_1, rt_mul_i32(loc_2, 1000000)))
		break
	end
	return reg_0
end
FUNC_LIST[437] = --[[ std::__2::__itoa::__append8[abi:nn180100](char*, unsigned int) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_div_u32(loc_1, 1000000)
		reg_0 = FUNC_LIST[431](loc_0, loc_2)
		reg_0 = FUNC_LIST[435](reg_0, rt_sub_i32(loc_1, rt_mul_i32(loc_2, 1000000)))
		break
	end
	return reg_0
end
FUNC_LIST[438] = --[[ std::__2::__itoa::__append9[abi:nn180100](char*, unsigned int) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_div_u32(loc_1, 100000000)
		reg_0 = FUNC_LIST[430](loc_0, loc_2)
		reg_0 = FUNC_LIST[437](reg_0, rt_sub_i32(loc_1, rt_mul_i32(loc_2, 100000000)))
		break
	end
	return reg_0
end
FUNC_LIST[439] = --[[ char* std::__2::__itoa::__append10[abi:nn180100]<unsigned int>(char*, unsigned int) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		loc_2 = rt_div_u32(loc_1, 100000000)
		reg_0 = FUNC_LIST[431](loc_0, loc_2)
		reg_0 = FUNC_LIST[437](reg_0, rt_sub_i32(loc_1, rt_mul_i32(loc_2, 100000000)))
		break
	end
	return reg_0
end
FUNC_LIST[440] = --[[ char* std::__2::copy_n[abi:nn180100]<char const*, int, char*, 0>(char const*, int, char*) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[386](loc_0, rt_add_i32(loc_0, loc_1), loc_2)
		break
	end
	return reg_0
end
FUNC_LIST[441] = --[[ std::__2::iterator_traits<char*>::difference_type std::__2::distance[abi:nn180100]<char*>(char*, char*) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[443](loc_0, loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[442] = --[[ void std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::__init_with_size[abi:nn180100]<char*, char*>(char*, char*, unsigned long) ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = 0
	local loc_5 = 0
	local reg_0
	local reg_1
	local reg_2
	local desired
	while true do
		loc_4 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_4
		reg_0 = FUNC_LIST[298](loc_0)
		if reg_0 >= loc_3 then
			while true do
				while true do
					reg_0 = FUNC_LIST[312](loc_3)
					if reg_0 ~= 0 then
						while true do
							FUNC_LIST[313](loc_0, loc_3)
							reg_0 = FUNC_LIST[292](loc_0)
							loc_5 = reg_0
							desired = 2
							break
						end
						if desired then
							if desired == 2 then
								desired = nil
							end
							break
						end
					end
					reg_1 = FUNC_LIST[302](loc_0)
					reg_2 = FUNC_LIST[300](loc_3)
					FUNC_LIST[303](rt_add_i32(loc_4, 8), reg_1, rt_add_i32(reg_2, 1))
					loc_5 = rt_load_i32(memory_at_0, loc_4 + 8)
					FUNC_LIST[304](loc_5, rt_load_i32(memory_at_0, loc_4 + 12))
					FUNC_LIST[307](loc_0, loc_5)
					FUNC_LIST[308](loc_0, rt_load_i32(memory_at_0, loc_4 + 12))
					FUNC_LIST[309](loc_0, loc_3)
					break
				end
				if desired then
					break
				end
				while true do
					if (if loc_1 == loc_2 then 1 else 0) == 0 then
						while true do
							FUNC_LIST[295](loc_5, loc_1)
							loc_5 = rt_add_i32(loc_5, 1)
							loc_1 = rt_add_i32(loc_1, 1)
							desired = 2
							break
						end
						if desired then
							if desired == 2 then
								desired = nil
								continue
							end
							break
						end
					end
					break
				end
				if desired then
					break
				end
				rt_store_i32_n8(memory_at_0, loc_4 + 7, 0)
				FUNC_LIST[295](loc_5, rt_add_i32(loc_4, 7))
				FUNC_LIST[310](loc_0, loc_3)
				GLOBAL_LIST[0].value = rt_add_i32(loc_4, 16)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		FUNC_LIST[311](loc_0)
		error("out of code bounds")
	end
end
FUNC_LIST[443] = --[[ std::__2::iterator_traits<char*>::difference_type std::__2::__distance[abi:nn180100]<char*>(char*, char*, std::__2::random_access_iterator_tag) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = rt_sub_i32(loc_1, loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[444] = --[[ std::__2::to_chars_result std::__2::__to_chars_itoa[abi:nn180100]<long long>(char*, char*, long long, std::__2::integral_constant<bool, true>) ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = rt_i64_ZERO
	local reg_0
	while true do
		reg_0 = FUNC_LIST[445](loc_3)
		loc_4 = reg_0
		while true do
			if loc_1 == loc_2 then
				break
			end
			if rt_ge_i64(loc_3, rt_i64_ZERO) then
				break
			end
			rt_store_i32_n8(memory_at_0, loc_1, 45)
			loc_1 = rt_add_i32(loc_1, 1)
			reg_0 = FUNC_LIST[446](loc_4)
			loc_4 = reg_0
			break
		end
		FUNC_LIST[447](loc_0, loc_1, loc_2, loc_4)
		break
	end
end
FUNC_LIST[445] = --[[ __make_unsigned(long long) std::__2::__to_unsigned_like[abi:nn180100]<long long>(long long) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[446] = --[[ unsigned long long std::__2::__complement[abi:nn180100]<unsigned long long>(unsigned long long) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = rt_sub_i64(rt_i64_ZERO, loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[447] = --[[ std::__2::to_chars_result std::__2::__to_chars_itoa[abi:nn180100]<unsigned long long>(char*, char*, unsigned long long, std::__2::integral_constant<bool, false>) ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = 0
	local reg_0
	local reg_1
	local reg_2
	local desired
	while true do
		reg_0 = loc_0
		while true do
			loc_4 = rt_sub_i32(loc_2, loc_1)
			if rt_le_i32(loc_4, 19) then
				while true do
					reg_2 = FUNC_LIST[448](loc_3)
					reg_1 = 61
					if rt_gt_i32(reg_2, loc_4) then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			reg_1 = FUNC_LIST[449](loc_1, loc_3)
			loc_2 = reg_1
			reg_1 = 0
			break
		end
		rt_store_i32(memory_at_0, reg_0 + 4, reg_1)
		rt_store_i32(memory_at_0, loc_0, loc_2)
		break
	end
end
FUNC_LIST[448] = --[[ std::__2::__itoa::__traits_base<unsigned long long, void>::__width[abi:nn180100](unsigned long long) ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	local reg_1
	while true do
		reg_1 = FUNC_LIST[450](rt_bit_or_i64(loc_0, rt_i64_ONE))
		loc_1 = rt_shr_i32(rt_mul_i32(rt_sub_i32(64, reg_1), 1233), 12)
		reg_0 = rt_add_i32(loc_1, (if rt_le_u64(rt_load_i64(memory_at_0, rt_add_i32(rt_shl_i32(loc_1, 3), 9232)), loc_0) then 1 else 0))
		break
	end
	return reg_0
end
FUNC_LIST[449] = --[[ std::__2::__itoa::__traits_base<unsigned long long, void>::__convert[abi:nn180100](char*, unsigned long long) ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[451](loc_0, loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[450] = --[[ std::__2::__libcpp_clz[abi:nn180100](unsigned long long) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = rt_wrap_i32_i64(rt_clz_i64(loc_0))
		break
	end
	return reg_0
end
FUNC_LIST[451] = --[[ std::__2::__itoa::__base_10_u64[abi:nn180100](char*, unsigned long long) ]] function(loc_0, loc_1)
	local loc_2 = rt_i64_ZERO
	local reg_0
	local desired
	while true do
		if rt_le_u64(loc_1, rt_i64_from_u32(4294967295, 0)) then
			while true do
				reg_0 = FUNC_LIST[429](loc_0, rt_wrap_i32_i64(loc_1))
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		if rt_ge_u64(loc_1, rt_i64_from_u32(1410065408, 2)) then
			while true do
				loc_2 = rt_div_u64(loc_1, rt_i64_from_u32(1410065408, 2))
				loc_1 = rt_sub_i64(loc_1, rt_mul_i64(loc_2, rt_i64_from_u32(1410065408, 2)))
				reg_0 = FUNC_LIST[429](loc_0, rt_wrap_i32_i64(loc_2))
				loc_0 = reg_0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		reg_0 = FUNC_LIST[452](loc_0, loc_1)
		break
	end
	return reg_0
end
FUNC_LIST[452] = --[[ char* std::__2::__itoa::__append10[abi:nn180100]<unsigned long long>(char*, unsigned long long) ]] function(loc_0, loc_1)
	local loc_2 = rt_i64_ZERO
	local reg_0
	while true do
		loc_2 = rt_div_u64(loc_1, rt_i64_from_u32(100000000, 0))
		reg_0 = FUNC_LIST[431](loc_0, rt_wrap_i32_i64(loc_2))
		reg_0 = FUNC_LIST[437](reg_0, rt_wrap_i32_i64(rt_sub_i64(loc_1, rt_mul_i64(loc_2, rt_i64_from_u32(100000000, 0)))))
		break
	end
	return reg_0
end
FUNC_LIST[453] = --[[ std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>::basic_string[abi:nn180100](std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>&&)::'lambda'(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>&)::operator()(std::__2::basic_string<char, std::__2::char_traits<char>, std::__2::allocator<char>>&) const ]] function(loc_0, loc_1)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[243](loc_1)
		if reg_0 == 0 then
			while true do
				FUNC_LIST[301](loc_1)
				break
			end
		end
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[454] = --[[ void (*std::__2::(anonymous namespace)::__libcpp_atomic_load[abi:nn180100]<void (*)()>(void (* const*)(), int))() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = rt_load_i32(memory_at_0, loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[455] = --[[ std::get_new_handler() ]] function()
	local reg_0
	while true do
		reg_0 = FUNC_LIST[454](11208)
		break
	end
	return reg_0
end
FUNC_LIST[456] = --[[ __cxa_allocate_exception ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[220](rt_add_i32(loc_0, 80))
		reg_0 = rt_add_i32(reg_0, 80)
		break
	end
	return reg_0
end
FUNC_LIST[457] = --[[ strcmp ]] function(loc_0, loc_1)
	local loc_2 = 0
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		loc_2 = rt_load_i32_u8(memory_at_0, loc_1)
		while true do
			loc_3 = rt_load_i32_u8(memory_at_0, loc_0)
			if loc_3 == 0 then
				break
			end
			if loc_2 ~= loc_3 then
				break
			end
			while true do
				loc_2 = rt_load_i32_u8(memory_at_0, loc_1 + 1)
				loc_3 = rt_load_i32_u8(memory_at_0, loc_0 + 1)
				if loc_3 == 0 then
					desired = 1
					break
				end
				loc_1 = rt_add_i32(loc_1, 1)
				loc_0 = rt_add_i32(loc_0, 1)
				if loc_2 == loc_3 then
					continue
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			break
		end
		reg_0 = rt_sub_i32(loc_3, loc_2)
		break
	end
	return reg_0
end
FUNC_LIST[458] = --[[ __cxxabiv1::__shim_type_info::~__shim_type_info() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[505](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[459] = --[[ __cxxabiv1::__shim_type_info::noop1() const ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[460] = --[[ __cxxabiv1::__shim_type_info::noop2() const ]] function(loc_0)
	while true do
		break
	end
end
FUNC_LIST[461] = --[[ __cxxabiv1::__fundamental_type_info::~__fundamental_type_info() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[458](loc_0)
		FUNC_LIST[230](reg_0)
		break
	end
end
FUNC_LIST[462] = --[[ __cxxabiv1::__class_type_info::~__class_type_info() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[458](loc_0)
		FUNC_LIST[230](reg_0)
		break
	end
end
FUNC_LIST[463] = --[[ __cxxabiv1::__si_class_type_info::~__si_class_type_info() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[458](loc_0)
		FUNC_LIST[230](reg_0)
		break
	end
end
FUNC_LIST[464] = --[[ __cxxabiv1::__pointer_type_info::~__pointer_type_info() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[458](loc_0)
		FUNC_LIST[230](reg_0)
		break
	end
end
FUNC_LIST[465] = --[[ __cxxabiv1::__fundamental_type_info::can_catch(__cxxabiv1::__shim_type_info const*, void*&) const ]] function(loc_0, loc_1, loc_2)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[466](loc_0, loc_1, 0)
		break
	end
	return reg_0
end
FUNC_LIST[466] = --[[ is_equal(std::type_info const*, std::type_info const*, bool) ]] function(loc_0, loc_1, loc_2)
	local reg_0
	local reg_1
	local desired
	while true do
		if loc_2 == 0 then
			while true do
				reg_0 = (if rt_load_i32(memory_at_0, loc_0 + 4) == rt_load_i32(memory_at_0, loc_1 + 4) then 1 else 0)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		if loc_0 == loc_1 then
			while true do
				reg_0 = 1
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		reg_0 = FUNC_LIST[467](loc_0)
		reg_1 = FUNC_LIST[467](loc_1)
		reg_0 = FUNC_LIST[457](reg_0, reg_1)
		reg_0 = (if reg_0 == 0 then 1 else 0)
		break
	end
	return reg_0
end
FUNC_LIST[467] = --[[ std::type_info::name[abi:nn180100]() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = rt_load_i32(memory_at_0, loc_0 + 4)
		break
	end
	return reg_0
end
FUNC_LIST[468] = --[[ __cxxabiv1::__class_type_info::can_catch(__cxxabiv1::__shim_type_info const*, void*&) const ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local reg_0
	local reg_1
	while true do
		loc_3 = rt_add_i32(GLOBAL_LIST[0].value, 4294967232)
		GLOBAL_LIST[0].value = loc_3
		while true do
			loc_4 = 1
			reg_1 = FUNC_LIST[466](loc_0, loc_1, 0)
			reg_0 = loc_4
			if reg_1 ~= 0 then
				break
			end
			loc_4 = 0
			reg_0 = loc_4
			if loc_1 == 0 then
				break
			end
			reg_1 = FUNC_LIST[469](loc_1, 9428, 9476, 0)
			loc_1 = reg_1
			reg_0 = 0
			if loc_1 == 0 then
				break
			end
			reg_0 = FUNC_LIST[219](rt_add_i32(loc_3, 8), 0, 56)
			rt_store_i32_n8(memory_at_0, loc_3 + 59, 1)
			rt_store_i32(memory_at_0, loc_3 + 16, 4294967295)
			rt_store_i32(memory_at_0, loc_3 + 12, loc_0)
			rt_store_i32(memory_at_0, loc_3 + 4, loc_1)
			rt_store_i32(memory_at_0, loc_3 + 52, 1)
			TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_1) + 28)](loc_1, rt_add_i32(loc_3, 4), rt_load_i32(memory_at_0, loc_2), 1)
			loc_4 = rt_load_i32(memory_at_0, loc_3 + 28)
			if loc_4 == 1 then
				while true do
					rt_store_i32(memory_at_0, loc_2, rt_load_i32(memory_at_0, loc_3 + 20))
					break
				end
			end
			reg_0 = (if loc_4 == 1 then 1 else 0)
			break
		end
		loc_4 = reg_0
		GLOBAL_LIST[0].value = rt_sub_i32(loc_3, 4294967232)
		reg_0 = loc_4
		break
	end
	return reg_0
end
FUNC_LIST[469] = --[[ __dynamic_cast ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local reg_0
	local desired
	while true do
		loc_4 = rt_sub_i32(GLOBAL_LIST[0].value, 16)
		GLOBAL_LIST[0].value = loc_4
		FUNC_LIST[470](rt_add_i32(loc_4, 4), loc_0)
		loc_7 = rt_load_i32(memory_at_0, loc_4 + 8)
		reg_0 = FUNC_LIST[466](loc_7, loc_2, 0)
		loc_5 = reg_0
		loc_6 = rt_load_i32(memory_at_0, loc_4 + 4)
		while true do
			if loc_5 ~= 0 then
				while true do
					reg_0 = FUNC_LIST[471](loc_0, loc_6, loc_1, loc_2, rt_load_i32(memory_at_0, loc_4 + 12), loc_3)
					loc_5 = reg_0
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			reg_0 = FUNC_LIST[472](loc_0, loc_6, loc_2, loc_7, loc_3)
			loc_5 = reg_0
			if loc_5 ~= 0 then
				break
			end
			reg_0 = FUNC_LIST[473](loc_0, loc_6, loc_1, loc_2, loc_7, loc_3)
			loc_5 = reg_0
			break
		end
		GLOBAL_LIST[0].value = rt_add_i32(loc_4, 16)
		reg_0 = loc_5
		break
	end
	return reg_0
end
FUNC_LIST[470] = --[[ __cxxabiv1::(anonymous namespace)::dyn_cast_get_derived_info(__cxxabiv1::(anonymous namespace)::derived_object_info*, void const*) ]] function(loc_0, loc_1)
	local loc_2 = 0
	local loc_3 = 0
	while true do
		loc_2 = rt_load_i32(memory_at_0, loc_1)
		loc_3 = rt_load_i32(memory_at_0, rt_sub_i32(loc_2, 8))
		rt_store_i32(memory_at_0, loc_0 + 8, loc_3)
		rt_store_i32(memory_at_0, loc_0, rt_add_i32(loc_1, loc_3))
		rt_store_i32(memory_at_0, loc_0 + 4, rt_load_i32(memory_at_0, rt_sub_i32(loc_2, 4)))
		break
	end
end
FUNC_LIST[471] = --[[ __cxxabiv1::(anonymous namespace)::dyn_cast_to_derived(void const*, void const*, __cxxabiv1::__class_type_info const*, __cxxabiv1::__class_type_info const*, long, long) ]] function(loc_0, loc_1, loc_2, loc_3, loc_4, loc_5)
	local loc_6 = 0
	local loc_7 = 0
	local reg_0
	local desired
	while true do
		loc_6 = rt_add_i32(GLOBAL_LIST[0].value, 4294967232)
		GLOBAL_LIST[0].value = loc_6
		while true do
			if rt_ge_i32(loc_5, 0) then
				while true do
					loc_7 = (if rt_sub_i32(0, loc_5) == loc_4 then loc_1 else 0)
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			if loc_5 == 4294967294 then
				break
			end
			loc_7 = rt_add_i32(loc_6, 28)
			rt_store_i64(memory_at_0, loc_7, rt_i64_ZERO)
			rt_store_i64(memory_at_0, loc_6 + 36, rt_i64_ZERO)
			rt_store_i64(memory_at_0, loc_6 + 44, rt_i64_ZERO)
			rt_store_i64(memory_at_0, loc_6 + 20, rt_i64_ZERO)
			rt_store_i32(memory_at_0, loc_6 + 16, loc_5)
			rt_store_i32(memory_at_0, loc_6 + 12, loc_2)
			rt_store_i32(memory_at_0, loc_6 + 8, loc_0)
			rt_store_i32(memory_at_0, loc_6 + 4, loc_3)
			rt_store_i32(memory_at_0, loc_6 + 60, 0)
			rt_store_i64(memory_at_0, loc_6 + 52, rt_i64_from_u32(1, 16777216))
			TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_3) + 20)](loc_3, rt_add_i32(loc_6, 4), loc_1, loc_1, 1, 0)
			loc_7 = (if rt_load_i32(memory_at_0, loc_6 + 28) == 1 then loc_1 else 0)
			break
		end
		GLOBAL_LIST[0].value = rt_sub_i32(loc_6, 4294967232)
		reg_0 = loc_7
		break
	end
	return reg_0
end
FUNC_LIST[472] = --[[ __cxxabiv1::(anonymous namespace)::dyn_cast_try_downcast(void const*, void const*, __cxxabiv1::__class_type_info const*, __cxxabiv1::__class_type_info const*, long) ]] function(loc_0, loc_1, loc_2, loc_3, loc_4)
	local loc_5 = 0
	local loc_6 = 0
	local reg_0
	while true do
		loc_5 = rt_add_i32(GLOBAL_LIST[0].value, 4294967232)
		GLOBAL_LIST[0].value = loc_5
		while true do
			if rt_lt_i32(loc_4, 0) then
				break
			end
			loc_0 = rt_sub_i32(loc_0, loc_4)
			if rt_lt_i32(loc_0, loc_1) then
				break
			end
			loc_6 = rt_add_i32(loc_5, 28)
			rt_store_i64(memory_at_0, loc_6, rt_i64_ZERO)
			rt_store_i64(memory_at_0, loc_5 + 36, rt_i64_ZERO)
			rt_store_i64(memory_at_0, loc_5 + 44, rt_i64_ZERO)
			rt_store_i64(memory_at_0, loc_5 + 20, rt_i64_ZERO)
			rt_store_i32(memory_at_0, loc_5 + 16, loc_4)
			rt_store_i32(memory_at_0, loc_5 + 12, loc_2)
			rt_store_i32(memory_at_0, loc_5 + 4, loc_3)
			rt_store_i32(memory_at_0, loc_5 + 60, 0)
			rt_store_i64(memory_at_0, loc_5 + 52, rt_i64_from_u32(1, 16777216))
			rt_store_i32(memory_at_0, loc_5 + 8, loc_0)
			TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_3) + 20)](loc_3, rt_add_i32(loc_5, 4), loc_1, loc_1, 1, 0)
			loc_6 = (if rt_load_i32(memory_at_0, loc_5 + 28) ~= 0 then loc_0 else 0)
			break
		end
		GLOBAL_LIST[0].value = rt_sub_i32(loc_5, 4294967232)
		reg_0 = loc_6
		break
	end
	return reg_0
end
FUNC_LIST[473] = --[[ __cxxabiv1::(anonymous namespace)::dyn_cast_slow(void const*, void const*, __cxxabiv1::__class_type_info const*, __cxxabiv1::__class_type_info const*, __cxxabiv1::__class_type_info const*, long) ]] function(loc_0, loc_1, loc_2, loc_3, loc_4, loc_5)
	local loc_6 = 0
	local reg_0
	local desired
	local br_map = {}
	while true do
		loc_6 = rt_add_i32(GLOBAL_LIST[0].value, 4294967232)
		GLOBAL_LIST[0].value = loc_6
		rt_store_i32(memory_at_0, loc_6 + 16, loc_5)
		rt_store_i32(memory_at_0, loc_6 + 12, loc_2)
		rt_store_i32(memory_at_0, loc_6 + 8, loc_0)
		rt_store_i32(memory_at_0, loc_6 + 4, loc_3)
		loc_5 = 0
		reg_0 = FUNC_LIST[219](rt_add_i32(loc_6, 20), 0, 39)
		rt_store_i32(memory_at_0, loc_6 + 60, 0)
		rt_store_i32_n8(memory_at_0, loc_6 + 59, 1)
		TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_4) + 24)](loc_4, rt_add_i32(loc_6, 4), loc_1, 1, 0)
		while true do
			while true do
				while true do
					if not br_map[1] then
						br_map[1] = (function()
							return { [0] = 0, 1, }
						end)()
					end
					temp = br_map[1][rt_load_i32(memory_at_0, loc_6 + 40)] or 2
					if temp < 1 then
						break
					elseif temp > 1 then
						desired = 1
						break
					else
						desired = 2
						break
					end
				end
				if desired then
					if desired == 2 then
						desired = nil
					end
					break
				end
				loc_5 = (if rt_load_i32(memory_at_0, loc_6 + 44) == 1 then (if rt_load_i32(memory_at_0, loc_6 + 32) == 1 then (if rt_load_i32(memory_at_0, loc_6 + 36) == 1 then rt_load_i32(memory_at_0, loc_6 + 24) else 0) else 0) else 0)
				desired = 1
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			if rt_load_i32(memory_at_0, loc_6 + 28) ~= 1 then
				while true do
					if rt_load_i32(memory_at_0, loc_6 + 44) ~= 0 then
						desired = 1
						break
					end
					if rt_load_i32(memory_at_0, loc_6 + 32) ~= 1 then
						desired = 1
						break
					end
					if rt_load_i32(memory_at_0, loc_6 + 36) ~= 1 then
						desired = 1
						break
					end
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_5 = rt_load_i32(memory_at_0, loc_6 + 20)
			break
		end
		GLOBAL_LIST[0].value = rt_sub_i32(loc_6, 4294967232)
		reg_0 = loc_5
		break
	end
	return reg_0
end
FUNC_LIST[474] = --[[ __cxxabiv1::__class_type_info::process_found_base_class(__cxxabiv1::__dynamic_cast_info*, void*, int) const ]] function(loc_0, loc_1, loc_2, loc_3)
	local loc_4 = 0
	local desired
	while true do
		loc_4 = rt_load_i32(memory_at_0, loc_1 + 36)
		if loc_4 == 0 then
			while true do
				rt_store_i32(memory_at_0, loc_1 + 24, loc_3)
				rt_store_i32(memory_at_0, loc_1 + 16, loc_2)
				rt_store_i32(memory_at_0, loc_1 + 36, 1)
				rt_store_i32(memory_at_0, loc_1 + 20, rt_load_i32(memory_at_0, loc_1 + 56))
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		while true do
			while true do
				if rt_load_i32(memory_at_0, loc_1 + 20) ~= rt_load_i32(memory_at_0, loc_1 + 56) then
					break
				end
				if rt_load_i32(memory_at_0, loc_1 + 16) ~= loc_2 then
					break
				end
				if rt_load_i32(memory_at_0, loc_1 + 24) ~= 2 then
					desired = 1
					break
				end
				rt_store_i32(memory_at_0, loc_1 + 24, loc_3)
				desired = 0
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			rt_store_i32_n8(memory_at_0, loc_1 + 54, 1)
			rt_store_i32(memory_at_0, loc_1 + 24, 2)
			rt_store_i32(memory_at_0, loc_1 + 36, rt_add_i32(loc_4, 1))
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		break
	end
end
FUNC_LIST[475] = --[[ __cxxabiv1::__class_type_info::has_unambiguous_public_base(__cxxabiv1::__dynamic_cast_info*, void*, int) const ]] function(loc_0, loc_1, loc_2, loc_3)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[466](loc_0, rt_load_i32(memory_at_0, loc_1 + 8), 0)
		if reg_0 ~= 0 then
			while true do
				FUNC_LIST[474](loc_1, loc_1, loc_2, loc_3)
				break
			end
		end
		break
	end
end
FUNC_LIST[476] = --[[ __cxxabiv1::__si_class_type_info::has_unambiguous_public_base(__cxxabiv1::__dynamic_cast_info*, void*, int) const ]] function(loc_0, loc_1, loc_2, loc_3)
	local reg_0
	local desired
	while true do
		reg_0 = FUNC_LIST[466](loc_0, rt_load_i32(memory_at_0, loc_1 + 8), 0)
		if reg_0 ~= 0 then
			while true do
				FUNC_LIST[474](loc_1, loc_1, loc_2, loc_3)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		loc_0 = rt_load_i32(memory_at_0, loc_0 + 8)
		TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_0) + 28)](loc_0, loc_1, loc_2, loc_3)
		break
	end
end
FUNC_LIST[477] = --[[ __cxxabiv1::__pbase_type_info::can_catch(__cxxabiv1::__shim_type_info const*, void*&) const ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local reg_0
	local desired
	while true do
		loc_3 = 1
		while true do
			if bit_and(rt_load_i32_u8(memory_at_0, loc_0 + 8), 24) == 0 then
				while true do
					loc_3 = 0
					if loc_1 == 0 then
						desired = 1
						break
					end
					reg_0 = FUNC_LIST[469](loc_1, 9428, 9524, 0)
					loc_4 = reg_0
					if loc_4 == 0 then
						desired = 1
						break
					end
					loc_3 = (if bit_and(rt_load_i32_u8(memory_at_0, loc_4 + 8), 24) ~= 0 then 1 else 0)
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			reg_0 = FUNC_LIST[466](loc_0, loc_1, loc_3)
			loc_3 = reg_0
			break
		end
		reg_0 = loc_3
		break
	end
	return reg_0
end
FUNC_LIST[478] = --[[ __cxxabiv1::__pointer_type_info::can_catch(__cxxabiv1::__shim_type_info const*, void*&) const ]] function(loc_0, loc_1, loc_2)
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local reg_0
	local desired
	while true do
		loc_4 = rt_add_i32(GLOBAL_LIST[0].value, 4294967232)
		GLOBAL_LIST[0].value = loc_4
		while true do
			reg_0 = FUNC_LIST[466](loc_1, 9792, 0)
			if reg_0 ~= 0 then
				while true do
					rt_store_i32(memory_at_0, loc_2, 0)
					loc_3 = 1
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			reg_0 = FUNC_LIST[477](loc_0, loc_1, loc_1)
			if reg_0 ~= 0 then
				while true do
					loc_3 = 1
					loc_1 = rt_load_i32(memory_at_0, loc_2)
					if loc_1 == 0 then
						desired = 1
						break
					end
					rt_store_i32(memory_at_0, loc_2, rt_load_i32(memory_at_0, loc_1))
					desired = 1
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			while true do
				if loc_1 == 0 then
					break
				end
				reg_0 = FUNC_LIST[469](loc_1, 9428, 9572, 0)
				loc_1 = reg_0
				if loc_1 == 0 then
					desired = 1
					break
				end
				loc_5 = rt_load_i32(memory_at_0, loc_2)
				if loc_5 ~= 0 then
					while true do
						rt_store_i32(memory_at_0, loc_2, rt_load_i32(memory_at_0, loc_5))
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				loc_5 = rt_load_i32(memory_at_0, loc_1 + 8)
				loc_6 = rt_load_i32(memory_at_0, loc_0 + 8)
				if bit_and(bit_and(loc_5, bit_xor(loc_6, 4294967295)), 7) ~= 0 then
					desired = 1
					break
				end
				if bit_and(bit_and(bit_xor(loc_5, 4294967295), loc_6), 96) ~= 0 then
					desired = 1
					break
				end
				loc_3 = 1
				reg_0 = FUNC_LIST[466](rt_load_i32(memory_at_0, loc_0 + 12), rt_load_i32(memory_at_0, loc_1 + 12), 0)
				if reg_0 ~= 0 then
					desired = 1
					break
				end
				reg_0 = FUNC_LIST[466](rt_load_i32(memory_at_0, loc_0 + 12), 9780, 0)
				if reg_0 ~= 0 then
					while true do
						loc_1 = rt_load_i32(memory_at_0, loc_1 + 12)
						if loc_1 == 0 then
							desired = 1
							break
						end
						reg_0 = FUNC_LIST[469](loc_1, 9428, 9624, 0)
						loc_3 = (if reg_0 == 0 then 1 else 0)
						desired = 1
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				loc_5 = rt_load_i32(memory_at_0, loc_0 + 12)
				if loc_5 == 0 then
					break
				end
				loc_3 = 0
				reg_0 = FUNC_LIST[469](loc_5, 9428, 9572, 0)
				loc_6 = reg_0
				if loc_6 ~= 0 then
					while true do
						if bit_and(rt_load_i32_u8(memory_at_0, loc_0 + 8), 1) == 0 then
							desired = 1
							break
						end
						reg_0 = FUNC_LIST[479](loc_6, rt_load_i32(memory_at_0, loc_1 + 12))
						loc_3 = reg_0
						desired = 1
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				reg_0 = FUNC_LIST[469](loc_5, 9428, 9684, 0)
				loc_6 = reg_0
				if loc_6 ~= 0 then
					while true do
						if bit_and(rt_load_i32_u8(memory_at_0, loc_0 + 8), 1) == 0 then
							desired = 1
							break
						end
						reg_0 = FUNC_LIST[480](loc_6, rt_load_i32(memory_at_0, loc_1 + 12))
						loc_3 = reg_0
						desired = 1
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				reg_0 = FUNC_LIST[469](loc_5, 9428, 9476, 0)
				loc_0 = reg_0
				if loc_0 == 0 then
					desired = 1
					break
				end
				loc_1 = rt_load_i32(memory_at_0, loc_1 + 12)
				if loc_1 == 0 then
					desired = 1
					break
				end
				reg_0 = FUNC_LIST[469](loc_1, 9428, 9476, 0)
				loc_1 = reg_0
				if loc_1 == 0 then
					desired = 1
					break
				end
				loc_3 = rt_load_i32(memory_at_0, loc_2)
				reg_0 = FUNC_LIST[219](rt_add_i32(loc_4, 8), 0, 56)
				rt_store_i32_n8(memory_at_0, loc_4 + 59, (if loc_3 ~= 0 then 1 else 0))
				rt_store_i32(memory_at_0, loc_4 + 16, 4294967295)
				rt_store_i32(memory_at_0, loc_4 + 12, loc_0)
				rt_store_i32(memory_at_0, loc_4 + 4, loc_1)
				rt_store_i32(memory_at_0, loc_4 + 52, 1)
				TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_1) + 28)](loc_1, rt_add_i32(loc_4, 4), loc_3, 1)
				loc_1 = rt_load_i32(memory_at_0, loc_4 + 28)
				if loc_1 == 1 then
					while true do
						rt_store_i32(memory_at_0, loc_2, (if loc_3 ~= 0 then rt_load_i32(memory_at_0, loc_4 + 20) else 0))
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				loc_3 = (if loc_1 == 1 then 1 else 0)
				desired = 1
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			loc_3 = 0
			break
		end
		GLOBAL_LIST[0].value = rt_sub_i32(loc_4, 4294967232)
		reg_0 = loc_3
		break
	end
	return reg_0
end
FUNC_LIST[479] = --[[ __cxxabiv1::__pointer_type_info::can_catch_nested(__cxxabiv1::__shim_type_info const*) const ]] function(loc_0, loc_1)
	local loc_2 = 0
	local loc_3 = 0
	local reg_0
	local desired
	while true do
		while true do
			while true do
				if loc_1 == 0 then
					while true do
						reg_0 = 0
						desired = 0
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
							continue
						end
						break
					end
				end
				reg_0 = FUNC_LIST[469](loc_1, 9428, 9572, 0)
				loc_1 = reg_0
				if loc_1 == 0 then
					desired = 1
					break
				end
				if bit_and(rt_load_i32(memory_at_0, loc_1 + 8), bit_xor(rt_load_i32(memory_at_0, loc_0 + 8), 4294967295)) ~= 0 then
					desired = 1
					break
				end
				reg_0 = FUNC_LIST[466](rt_load_i32(memory_at_0, loc_0 + 12), rt_load_i32(memory_at_0, loc_1 + 12), 0)
				if reg_0 ~= 0 then
					while true do
						reg_0 = 1
						desired = 0
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
							continue
						end
						break
					end
				end
				if bit_and(rt_load_i32_u8(memory_at_0, loc_0 + 8), 1) == 0 then
					desired = 1
					break
				end
				loc_3 = rt_load_i32(memory_at_0, loc_0 + 12)
				if loc_3 == 0 then
					desired = 1
					break
				end
				reg_0 = FUNC_LIST[469](loc_3, 9428, 9572, 0)
				loc_0 = reg_0
				if loc_0 ~= 0 then
					while true do
						loc_1 = rt_load_i32(memory_at_0, loc_1 + 12)
						desired = 2
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
							continue
						end
						break
					end
				end
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			reg_0 = FUNC_LIST[469](loc_3, 9428, 9684, 0)
			loc_0 = reg_0
			if loc_0 == 0 then
				break
			end
			reg_0 = FUNC_LIST[480](loc_0, rt_load_i32(memory_at_0, loc_1 + 12))
			loc_2 = reg_0
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		reg_0 = loc_2
		break
	end
	return reg_0
end
FUNC_LIST[480] = --[[ __cxxabiv1::__pointer_to_member_type_info::can_catch_nested(__cxxabiv1::__shim_type_info const*) const ]] function(loc_0, loc_1)
	local loc_2 = 0
	local reg_0
	while true do
		while true do
			if loc_1 == 0 then
				break
			end
			reg_0 = FUNC_LIST[469](loc_1, 9428, 9684, 0)
			loc_1 = reg_0
			if loc_1 == 0 then
				break
			end
			if bit_and(rt_load_i32(memory_at_0, loc_1 + 8), bit_xor(rt_load_i32(memory_at_0, loc_0 + 8), 4294967295)) ~= 0 then
				break
			end
			reg_0 = FUNC_LIST[466](rt_load_i32(memory_at_0, loc_0 + 12), rt_load_i32(memory_at_0, loc_1 + 12), 0)
			if reg_0 == 0 then
				break
			end
			reg_0 = FUNC_LIST[466](rt_load_i32(memory_at_0, loc_0 + 16), rt_load_i32(memory_at_0, loc_1 + 16), 0)
			loc_2 = reg_0
			break
		end
		reg_0 = loc_2
		break
	end
	return reg_0
end
FUNC_LIST[481] = --[[ __cxxabiv1::__class_type_info::process_static_type_above_dst(__cxxabiv1::__dynamic_cast_info*, void const*, void const*, int) const ]] function(loc_0, loc_1, loc_2, loc_3, loc_4)
	local desired
	while true do
		rt_store_i32_n8(memory_at_0, loc_1 + 53, 1)
		while true do
			if rt_load_i32(memory_at_0, loc_1 + 4) ~= loc_3 then
				break
			end
			rt_store_i32_n8(memory_at_0, loc_1 + 52, 1)
			while true do
				loc_3 = rt_load_i32(memory_at_0, loc_1 + 16)
				if loc_3 == 0 then
					while true do
						rt_store_i32(memory_at_0, loc_1 + 36, 1)
						rt_store_i32(memory_at_0, loc_1 + 24, loc_4)
						rt_store_i32(memory_at_0, loc_1 + 16, loc_2)
						if loc_4 ~= 1 then
							desired = 1
							break
						end
						if rt_load_i32(memory_at_0, loc_1 + 48) == 1 then
							desired = 2
							break
						end
						desired = 1
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				if loc_2 == loc_3 then
					while true do
						loc_3 = rt_load_i32(memory_at_0, loc_1 + 24)
						if loc_3 == 2 then
							while true do
								rt_store_i32(memory_at_0, loc_1 + 24, loc_4)
								loc_3 = loc_4
								break
							end
							if desired then
								break
							end
						end
						if rt_load_i32(memory_at_0, loc_1 + 48) ~= 1 then
							desired = 1
							break
						end
						if loc_3 == 1 then
							desired = 2
							break
						end
						desired = 1
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				rt_store_i32(memory_at_0, loc_1 + 36, rt_add_i32(rt_load_i32(memory_at_0, loc_1 + 36), 1))
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			rt_store_i32_n8(memory_at_0, loc_1 + 54, 1)
			break
		end
		break
	end
end
FUNC_LIST[482] = --[[ __cxxabiv1::__class_type_info::process_static_type_below_dst(__cxxabiv1::__dynamic_cast_info*, void const*, int) const ]] function(loc_0, loc_1, loc_2, loc_3)
	while true do
		while true do
			if rt_load_i32(memory_at_0, loc_1 + 4) ~= loc_2 then
				break
			end
			if rt_load_i32(memory_at_0, loc_1 + 28) == 1 then
				break
			end
			rt_store_i32(memory_at_0, loc_1 + 28, loc_3)
			break
		end
		break
	end
end
FUNC_LIST[483] = --[[ __cxxabiv1::__si_class_type_info::search_below_dst(__cxxabiv1::__dynamic_cast_info*, void const*, int, bool) const ]] function(loc_0, loc_1, loc_2, loc_3, loc_4)
	local reg_0
	local desired
	while true do
		reg_0 = FUNC_LIST[466](loc_0, rt_load_i32(memory_at_0, loc_1 + 8), loc_4)
		if reg_0 ~= 0 then
			while true do
				FUNC_LIST[482](loc_1, loc_1, loc_2, loc_3)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		while true do
			reg_0 = FUNC_LIST[466](loc_0, rt_load_i32(memory_at_0, loc_1), loc_4)
			if reg_0 ~= 0 then
				while true do
					while true do
						if loc_2 ~= rt_load_i32(memory_at_0, loc_1 + 16) then
							while true do
								if rt_load_i32(memory_at_0, loc_1 + 20) ~= loc_2 then
									desired = 3
									break
								end
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						if loc_3 ~= 1 then
							desired = 1
							break
						end
						rt_store_i32(memory_at_0, loc_1 + 32, 1)
						desired = 0
						break
					end
					if desired then
						break
					end
					rt_store_i32(memory_at_0, loc_1 + 32, loc_3)
					while true do
						if rt_load_i32(memory_at_0, loc_1 + 44) == 4 then
							break
						end
						rt_store_i32_n16(memory_at_0, loc_1 + 52, 0)
						loc_0 = rt_load_i32(memory_at_0, loc_0 + 8)
						TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_0) + 20)](loc_0, loc_1, loc_2, loc_2, 1, loc_4)
						if rt_load_i32_u8(memory_at_0, loc_1 + 53) == 1 then
							while true do
								rt_store_i32(memory_at_0, loc_1 + 44, 3)
								if rt_load_i32_u8(memory_at_0, loc_1 + 52) == 0 then
									desired = 3
									break
								end
								desired = 1
								break
							end
							if desired then
								if desired == 3 then
									desired = nil
								end
								break
							end
						end
						rt_store_i32(memory_at_0, loc_1 + 44, 4)
						break
					end
					if desired then
						break
					end
					rt_store_i32(memory_at_0, loc_1 + 20, loc_2)
					rt_store_i32(memory_at_0, loc_1 + 40, rt_add_i32(rt_load_i32(memory_at_0, loc_1 + 40), 1))
					if rt_load_i32(memory_at_0, loc_1 + 36) ~= 1 then
						desired = 1
						break
					end
					if rt_load_i32(memory_at_0, loc_1 + 24) ~= 2 then
						desired = 1
						break
					end
					rt_store_i32_n8(memory_at_0, loc_1 + 54, 1)
					desired = 0
					break
				end
				if desired then
					if desired == 1 then
						desired = nil
					end
					break
				end
			end
			loc_0 = rt_load_i32(memory_at_0, loc_0 + 8)
			TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_0) + 24)](loc_0, loc_1, loc_2, loc_3, loc_4)
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		break
	end
end
FUNC_LIST[484] = --[[ __cxxabiv1::__class_type_info::search_below_dst(__cxxabiv1::__dynamic_cast_info*, void const*, int, bool) const ]] function(loc_0, loc_1, loc_2, loc_3, loc_4)
	local reg_0
	local desired
	while true do
		reg_0 = FUNC_LIST[466](loc_0, rt_load_i32(memory_at_0, loc_1 + 8), loc_4)
		if reg_0 ~= 0 then
			while true do
				FUNC_LIST[482](loc_1, loc_1, loc_2, loc_3)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		while true do
			reg_0 = FUNC_LIST[466](loc_0, rt_load_i32(memory_at_0, loc_1), loc_4)
			if reg_0 == 0 then
				break
			end
			while true do
				if loc_2 ~= rt_load_i32(memory_at_0, loc_1 + 16) then
					while true do
						if rt_load_i32(memory_at_0, loc_1 + 20) ~= loc_2 then
							desired = 2
							break
						end
						break
					end
					if desired then
						if desired == 2 then
							desired = nil
						end
						break
					end
				end
				if loc_3 ~= 1 then
					desired = 1
					break
				end
				rt_store_i32(memory_at_0, loc_1 + 32, 1)
				desired = 0
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			rt_store_i32(memory_at_0, loc_1 + 20, loc_2)
			rt_store_i32(memory_at_0, loc_1 + 32, loc_3)
			rt_store_i32(memory_at_0, loc_1 + 40, rt_add_i32(rt_load_i32(memory_at_0, loc_1 + 40), 1))
			while true do
				if rt_load_i32(memory_at_0, loc_1 + 36) ~= 1 then
					break
				end
				if rt_load_i32(memory_at_0, loc_1 + 24) ~= 2 then
					break
				end
				rt_store_i32_n8(memory_at_0, loc_1 + 54, 1)
				break
			end
			if desired then
				if desired == 1 then
					desired = nil
				end
				break
			end
			rt_store_i32(memory_at_0, loc_1 + 44, 4)
			break
		end
		if desired then
			if desired == 0 then
				desired = nil
			end
			break
		end
		break
	end
end
FUNC_LIST[485] = --[[ __cxxabiv1::__si_class_type_info::search_above_dst(__cxxabiv1::__dynamic_cast_info*, void const*, void const*, int, bool) const ]] function(loc_0, loc_1, loc_2, loc_3, loc_4, loc_5)
	local reg_0
	local desired
	while true do
		reg_0 = FUNC_LIST[466](loc_0, rt_load_i32(memory_at_0, loc_1 + 8), loc_5)
		if reg_0 ~= 0 then
			while true do
				FUNC_LIST[481](loc_1, loc_1, loc_2, loc_3, loc_4)
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		loc_0 = rt_load_i32(memory_at_0, loc_0 + 8)
		TABLE_LIST[0].data[rt_load_i32(memory_at_0, rt_load_i32(memory_at_0, loc_0) + 20)](loc_0, loc_1, loc_2, loc_3, loc_4, loc_5)
		break
	end
end
FUNC_LIST[486] = --[[ __cxxabiv1::__class_type_info::search_above_dst(__cxxabiv1::__dynamic_cast_info*, void const*, void const*, int, bool) const ]] function(loc_0, loc_1, loc_2, loc_3, loc_4, loc_5)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[466](loc_0, rt_load_i32(memory_at_0, loc_1 + 8), loc_5)
		if reg_0 ~= 0 then
			while true do
				FUNC_LIST[481](loc_1, loc_1, loc_2, loc_3, loc_4)
				break
			end
		end
		break
	end
end
FUNC_LIST[487] = --[[ __cxa_is_pointer_type ]] function(loc_0)
	local reg_0
	local desired
	while true do
		if loc_0 == 0 then
			while true do
				reg_0 = 0
				desired = 0
				break
			end
			if desired then
				if desired == 0 then
					desired = nil
				end
				break
			end
		end
		reg_0 = FUNC_LIST[469](loc_0, 9428, 9572, 0)
		reg_0 = (if reg_0 ~= 0 then 1 else 0)
		break
	end
	return reg_0
end
FUNC_LIST[488] = --[[ std::exception::~exception() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[489] = --[[ std::exception::~exception().1 ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[488](loc_0)
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[490] = --[[ std::exception::what() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 1117
		break
	end
	return reg_0
end
FUNC_LIST[491] = --[[ std::bad_alloc::bad_alloc() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[237](loc_0)
		loc_0 = reg_0
		rt_store_i32(memory_at_0, loc_0, 10176)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[492] = --[[ std::bad_alloc::~bad_alloc() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[488](loc_0)
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[493] = --[[ std::bad_alloc::what() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 2016
		break
	end
	return reg_0
end
FUNC_LIST[494] = --[[ std::bad_array_new_length::bad_array_new_length() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[491](loc_0)
		loc_0 = reg_0
		rt_store_i32(memory_at_0, loc_0, 10196)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[495] = --[[ std::bad_array_new_length::~bad_array_new_length() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[488](loc_0)
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[496] = --[[ std::bad_array_new_length::what() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 1163
		break
	end
	return reg_0
end
FUNC_LIST[497] = --[[ std::logic_error::~logic_error() ]] function(loc_0)
	local reg_0
	while true do
		rt_store_i32(memory_at_0, loc_0, 10328)
		reg_0 = FUNC_LIST[498](rt_add_i32(loc_0, 4))
		reg_0 = FUNC_LIST[488](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[498] = --[[ std::__2::__libcpp_refstring::~__libcpp_refstring() ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		while true do
			reg_0 = FUNC_LIST[241](loc_0)
			if reg_0 == 0 then
				break
			end
			reg_0 = FUNC_LIST[499](rt_load_i32(memory_at_0, loc_0))
			loc_1 = reg_0
			reg_0 = FUNC_LIST[500](rt_add_i32(loc_1, 8))
			if rt_ge_i32(reg_0, 0) then
				break
			end
			FUNC_LIST[230](loc_1)
			break
		end
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[499] = --[[ std::__2::__refstring_imp::(anonymous namespace)::rep_from_data(char const*) ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = rt_sub_i32(loc_0, 12)
		break
	end
	return reg_0
end
FUNC_LIST[500] = --[[ int std::__2::(anonymous namespace)::__libcpp_atomic_add[abi:nn180100]<int, int>(int*, int, int) ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		loc_1 = rt_sub_i32(rt_load_i32(memory_at_0, loc_0), 1)
		rt_store_i32(memory_at_0, loc_0, loc_1)
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[501] = --[[ std::logic_error::~logic_error().1 ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[497](loc_0)
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[502] = --[[ std::logic_error::what() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[503](rt_add_i32(loc_0, 4))
		break
	end
	return reg_0
end
FUNC_LIST[503] = --[[ std::__2::__libcpp_refstring::c_str[abi:nn180100]() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = rt_load_i32(memory_at_0, loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[504] = --[[ std::length_error::~length_error() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[497](loc_0)
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[505] = --[[ std::type_info::~type_info() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[506] = --[[ std::bad_cast::bad_cast() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[237](loc_0)
		loc_0 = reg_0
		rt_store_i32(memory_at_0, loc_0, 10428)
		reg_0 = loc_0
		break
	end
	return reg_0
end
FUNC_LIST[507] = --[[ std::bad_cast::~bad_cast() ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[488](loc_0)
		break
	end
	return reg_0
end
FUNC_LIST[508] = --[[ std::bad_cast::~bad_cast().1 ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = FUNC_LIST[507](loc_0)
		FUNC_LIST[230](loc_0)
		break
	end
end
FUNC_LIST[509] = --[[ std::bad_cast::what() const ]] function(loc_0)
	local reg_0
	while true do
		reg_0 = 1075
		break
	end
	return reg_0
end
FUNC_LIST[510] = --[[ _emscripten_stack_restore ]] function(loc_0)
	while true do
		GLOBAL_LIST[0].value = loc_0
		break
	end
end
FUNC_LIST[511] = --[[ _emscripten_stack_alloc ]] function(loc_0)
	local loc_1 = 0
	local reg_0
	while true do
		loc_1 = bit_and(rt_sub_i32(GLOBAL_LIST[0].value, loc_0), 4294967280)
		GLOBAL_LIST[0].value = loc_1
		reg_0 = loc_1
		break
	end
	return reg_0
end
FUNC_LIST[512] = --[[ emscripten_stack_get_current ]] function()
	local reg_0
	while true do
		reg_0 = GLOBAL_LIST[0].value
		break
	end
	return reg_0
end
local function run_init_code()
    peformSleepCheck()
	TABLE_LIST[0] = { min = 195, max = 195, data = {} }
	MEMORY_LIST[0] = rt_allocator_new(258, 258)
	GLOBAL_LIST[0] = { value = 76752 }
	do
		local target = TABLE_LIST[0].data
		local offset = 1
		local data = { FUNC_LIST[12],FUNC_LIST[14],FUNC_LIST[23],FUNC_LIST[497],FUNC_LIST[488],FUNC_LIST[195],FUNC_LIST[507],FUNC_LIST[174],FUNC_LIST[25],FUNC_LIST[26],FUNC_LIST[27],FUNC_LIST[28],FUNC_LIST[29],FUNC_LIST[30],FUNC_LIST[31],FUNC_LIST[32],FUNC_LIST[33],FUNC_LIST[34],FUNC_LIST[35],FUNC_LIST[36],FUNC_LIST[37],FUNC_LIST[38],FUNC_LIST[40],FUNC_LIST[41],FUNC_LIST[44],FUNC_LIST[45],FUNC_LIST[46],FUNC_LIST[47],FUNC_LIST[48],FUNC_LIST[49],FUNC_LIST[51],FUNC_LIST[52],FUNC_LIST[53],FUNC_LIST[54],FUNC_LIST[55],FUNC_LIST[56],FUNC_LIST[57],FUNC_LIST[58],FUNC_LIST[60],FUNC_LIST[61],FUNC_LIST[62],FUNC_LIST[63],FUNC_LIST[64],FUNC_LIST[65],FUNC_LIST[66],FUNC_LIST[67],FUNC_LIST[68],FUNC_LIST[69],FUNC_LIST[70],FUNC_LIST[71],FUNC_LIST[72],FUNC_LIST[73],FUNC_LIST[74],FUNC_LIST[75],FUNC_LIST[76],FUNC_LIST[77],FUNC_LIST[78],FUNC_LIST[79],FUNC_LIST[80],FUNC_LIST[81],FUNC_LIST[82],FUNC_LIST[83],FUNC_LIST[84],FUNC_LIST[85],FUNC_LIST[86],FUNC_LIST[87],FUNC_LIST[88],FUNC_LIST[89],FUNC_LIST[90],FUNC_LIST[91],FUNC_LIST[92],FUNC_LIST[93],FUNC_LIST[94],FUNC_LIST[95],FUNC_LIST[96],FUNC_LIST[97],FUNC_LIST[98],FUNC_LIST[99],FUNC_LIST[100],FUNC_LIST[101],FUNC_LIST[102],FUNC_LIST[103],FUNC_LIST[104],FUNC_LIST[105],FUNC_LIST[106],FUNC_LIST[107],FUNC_LIST[108],FUNC_LIST[109],FUNC_LIST[110],FUNC_LIST[111],FUNC_LIST[112],FUNC_LIST[113],FUNC_LIST[114],FUNC_LIST[115],FUNC_LIST[116],FUNC_LIST[117],FUNC_LIST[118],FUNC_LIST[119],FUNC_LIST[120],FUNC_LIST[121],FUNC_LIST[122],FUNC_LIST[123],FUNC_LIST[124],FUNC_LIST[125],FUNC_LIST[126],FUNC_LIST[127],FUNC_LIST[128],FUNC_LIST[129],FUNC_LIST[130],FUNC_LIST[131],FUNC_LIST[132],FUNC_LIST[133],FUNC_LIST[134],FUNC_LIST[135],FUNC_LIST[136],FUNC_LIST[137],FUNC_LIST[138],FUNC_LIST[139],FUNC_LIST[140],FUNC_LIST[141],FUNC_LIST[142],FUNC_LIST[143],FUNC_LIST[144],FUNC_LIST[145],FUNC_LIST[146],FUNC_LIST[147],FUNC_LIST[148],FUNC_LIST[149],FUNC_LIST[150],FUNC_LIST[151],FUNC_LIST[152],FUNC_LIST[153],FUNC_LIST[154],FUNC_LIST[155],FUNC_LIST[156],FUNC_LIST[157],FUNC_LIST[158],FUNC_LIST[159],FUNC_LIST[160],FUNC_LIST[161],FUNC_LIST[162],FUNC_LIST[163],FUNC_LIST[164],FUNC_LIST[165],FUNC_LIST[166],FUNC_LIST[167],FUNC_LIST[168],FUNC_LIST[169],FUNC_LIST[170],FUNC_LIST[171],FUNC_LIST[172],FUNC_LIST[173],FUNC_LIST[175],FUNC_LIST[176],FUNC_LIST[177],FUNC_LIST[178],FUNC_LIST[179],FUNC_LIST[180],FUNC_LIST[182],FUNC_LIST[183],FUNC_LIST[194],FUNC_LIST[193],FUNC_LIST[196],FUNC_LIST[197],FUNC_LIST[271],FUNC_LIST[272],FUNC_LIST[275],FUNC_LIST[458],FUNC_LIST[461],FUNC_LIST[459],FUNC_LIST[460],FUNC_LIST[465],FUNC_LIST[462],FUNC_LIST[468],FUNC_LIST[486],FUNC_LIST[484],FUNC_LIST[475],FUNC_LIST[463],FUNC_LIST[485],FUNC_LIST[483],FUNC_LIST[476],FUNC_LIST[464],FUNC_LIST[478],FUNC_LIST[492],FUNC_LIST[493],FUNC_LIST[495],FUNC_LIST[496],FUNC_LIST[489],FUNC_LIST[490],FUNC_LIST[501],FUNC_LIST[502],FUNC_LIST[504],FUNC_LIST[508],FUNC_LIST[509], }
		table.move(data, 1, #data, offset, target)
	end
	rt_store_string(MEMORY_LIST[0], 1024,"-+   0X0x\x00-0X+0X 0X-0x+0x 0x\x00__next_prime overflow\x00std::bad_cast\x00bad any cast\x00CppPart\x00Parent\x00std::exception\x00nan\x00std::bad_function_call\x00nil\x00bad_array_new_length\x00basic_string\x00inf\x00%Lf\x00%f\x00true\x00false\x00game.Workspace\x00 if type(ret) ~= \"string\" then ret = tostring(ret) end return ret end\x00 if inst and typeof(inst) == \"Instance\" then local cs = game:GetService(\"CollectionService\") local tags = cs:GetTags(inst) for i,v in ipairs(tags) do if #v > 6 and v:sub(1,6) == \"__dID_\" then return v end end local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(inst, dID) return dID end\x00\")[1] if inst then local cln = inst:Clone() local storage = game:GetService(\"ServerStorage\"):FindFirstChild(\"__CppStorage\") if not storage then storage = Instance.new(\"Folder\") storage.Name = \"__CppStorage\" storage.Parent = game:GetService(\"ServerStorage\") end cln.Parent = storage local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(cln, dID) return dID end\x00std::bad_alloc\x00\")[1]\x00NAN\x00INF\x00\")[1] return dID\x00\") local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) game:GetService(\"CollectionService\"):AddTag(inst, dID) return dID\x00\")[1] if inst then inst.\x00\")[1] if inst then local ret = inst.\x00(null)\x00\")\x00local inst = Instance.new(\"\x00print(\"\x00error(\"\x00local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"\x00\") local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) local cs = game:GetService(\"CollectionService\") cs:AddTag(inst, dID) inst.Parent = cs:GetTagged(\"\x00game:GetService(\"CollectionService\"):GetTagged(\"\x00local inst = \x00Invalid type: \x00PARTS CREATED? Name: \x00NSt3__212basic_stringIcNS_11char_traitsIcEENS_9allocatorIcEEEE\x00\x00\x00\x00 \'\x00\x00r\n\x00\x00PN3RBX8InstanceE\x00N3RBX8InstanceE\x00\x00\x00\x00 \'\x00\x00\xcd\n\x00\x00\xa4\'\x00\x00\xbc\n\x00\x00\x00\x00\x00\x00\xe0\n\x00\x00\x00\x00\x00\x00$\x0c\x00\x00\x08\x00\x00\x00\t\x00\x00\x00\n\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\r\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00NSt3__210__function6__baseIFNS_12basic_stringIcNS_11char_traitsIcEENS_9allocatorIcEEEERKNS_3anyEEEE\x00\x00\x00 \'\x00\x00\xb6\x0b\x00\x00H\'\x00\x00$\x0b\x00\x00\x1c\x0c\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E_\x00 \'\x00\x000\x0c\x00\x00\x00\x00\x00\x00(\r\x00\x00\x08\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E0_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00\x00H\'\x00\x00\x94\x0c\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E0_\x00\x00\x00\x00 \'\x00\x008\r\x00\x00\x00\x00\x00\x004\x0e\x00\x00\x08\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00 \x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E1_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00\x00H\'\x00\x00\xa0\r\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E1_\x00\x00\x00\x00 \'\x00\x00D\x0e\x00\x00\x00\x00\x00\x00@\x0f\x00\x00\x08\x00\x00\x00!\x00\x00\x00\"\x00\x00\x00#\x00\x00\x00$\x00\x00\x00%\x00\x00\x00&\x00\x00\x00\'\x00\x00\x00(\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E2_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00\x00H\'\x00\x00\xac\x0e\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E2_\x00\x00\x00\x00 \'\x00\x00P\x0f\x00\x00\x00\x00\x00\x00L\x10\x00\x00\x08\x00\x00\x00)\x00\x00\x00*\x00\x00\x00+\x00\x00\x00,\x00\x00\x00-\x00\x00\x00.\x00\x00\x00/\x00\x00\x000\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E3_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00\x00H\'\x00\x00\xb8\x0f\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E3_\x00\x00\x00\x00 \'\x00\x00\\\x10\x00\x00\x00\x00\x00\x00X\x11\x00\x00\x08\x00\x00\x001\x00\x00\x002\x00\x00\x003\x00\x00\x004\x00\x00\x005\x00\x00\x006\x00\x00\x007\x00\x00\x008\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E4_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00\x00H\'\x00\x00\xc4\x10\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E4_\x00\x00\x00\x00 \'\x00\x00h\x11\x00\x00\x00\x00\x00\x00d\x12\x00\x00\x08\x00\x00\x009\x00\x00\x00:\x00\x00\x00;\x00\x00\x00<\x00\x00\x00=\x00\x00\x00>\x00\x00\x00?\x00\x00\x00@\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E5_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00\x00H\'\x00\x00\xd0\x11\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E5_\x00\x00\x00\x00 \'\x00\x00t\x12\x00\x00\x00\x00\x00\x00p\x13\x00\x00\x08\x00\x00\x00A\x00\x00\x00B\x00\x00\x00C\x00\x00\x00D\x00\x00\x00E\x00\x00\x00F\x00\x00\x00G\x00\x00\x00H\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E6_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00\x00H\'\x00\x00\xdc\x12\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E6_\x00\x00\x00\x00 \'\x00\x00\x80\x13\x00\x00\x00\x00\x00\x00|\x14\x00\x00\x08\x00\x00\x00I\x00\x00\x00J\x00\x00\x00K\x00\x00\x00L\x00\x00\x00M\x00\x00\x00N\x00\x00\x00O\x00\x00\x00P\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E7_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00\x00H\'\x00\x00\xe8\x13\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E7_\x00\x00\x00\x00 \'\x00\x00\x8c\x14\x00\x00\x00\x00\x00\x00\x88\x15\x00\x00\x08\x00\x00\x00Q\x00\x00\x00R\x00\x00\x00S\x00\x00\x00T\x00\x00\x00U\x00\x00\x00V\x00\x00\x00W\x00\x00\x00X\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E8_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00\x00H\'\x00\x00\xf4\x14\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E8_\x00\x00\x00\x00 \'\x00\x00\x98\x15\x00\x00\x00\x00\x00\x00\x94\x16\x00\x00\x08\x00\x00\x00Y\x00\x00\x00Z\x00\x00\x00[\x00\x00\x00\\\x00\x00\x00]\x00\x00\x00^\x00\x00\x00_\x00\x00\x00`\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E9_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00\x00H\'\x00\x00\x00\x16\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E9_\x00\x00\x00\x00 \'\x00\x00\xa4\x16\x00\x00\x00\x00\x00\x00\xa0\x17\x00\x00\x08\x00\x00\x00a\x00\x00\x00b\x00\x00\x00c\x00\x00\x00d\x00\x00\x00e\x00\x00\x00f\x00\x00\x00g\x00\x00\x00h\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E10_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00H\'\x00\x00\x0c\x17\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E10_\x00\x00\x00 \'\x00\x00\xb0\x17\x00\x00\x00\x00\x00\x00\xac\x18\x00\x00\x08\x00\x00\x00i\x00\x00\x00j\x00\x00\x00k\x00\x00\x00l\x00\x00\x00m\x00\x00\x00n\x00\x00\x00o\x00\x00\x00p\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E11_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00H\'\x00\x00\x18\x18\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E11_\x00\x00\x00 \'\x00\x00\xbc\x18\x00\x00\x00\x00\x00\x00\xb8\x19\x00\x00\x08\x00\x00\x00q\x00\x00\x00r\x00\x00\x00s\x00\x00\x00t\x00\x00\x00u\x00\x00\x00v\x00\x00\x00w\x00\x00\x00x\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E12_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00H\'\x00\x00$\x19\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E12_\x00\x00\x00 \'\x00\x00\xc8\x19\x00\x00\x00\x00\x00\x00\xc4\x1a\x00\x00\x08\x00\x00\x00y\x00\x00\x00z\x00\x00\x00{\x00\x00\x00|\x00\x00\x00}\x00\x00\x00~\x00\x00\x00\x7f\x00\x00\x00\x80\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E13_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00H\'\x00\x000\x1a\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E13_\x00\x00\x00 \'\x00\x00\xd4\x1a\x00\x00\x00\x00\x00\x00\xd0\x1b\x00\x00\x08\x00\x00\x00\x81\x00\x00\x00\x82\x00\x00\x00\x83\x00\x00\x00\x84\x00\x00\x00\x85\x00\x00\x00\x86\x00\x00\x00\x87\x00\x00\x00\x88\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E14_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00H\'\x00\x00<\x1b\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E14_\x00\x00\x00 \'\x00\x00\xe0\x1b\x00\x00\x00\x00\x00\x00\xdc\x1c\x00\x00\x08\x00\x00\x00\x89\x00\x00\x00\x8a\x00\x00\x00\x8b\x00\x00\x00\x8c\x00\x00\x00\x8d\x00\x00\x00\x8e\x00\x00\x00\x8f\x00\x00\x00\x90\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E15_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00H\'\x00\x00H\x1c\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E15_\x00\x00\x00 \'\x00\x00\xec\x1c\x00\x00\x00\x00\x00\x00\xe8\x1d\x00\x00\x08\x00\x00\x00\x91\x00\x00\x00\x92\x00\x00\x00\x93\x00\x00\x00\x94\x00\x00\x00\x95\x00\x00\x00\x96\x00\x00\x00\x97\x00\x00\x00\x98\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E16_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00H\'\x00\x00T\x1d\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E16_\x00\x00\x00 \'\x00\x00\xf8\x1d\x00\x00\x00\x00\x00\x00\xf4\x1e\x00\x00\x08\x00\x00\x00\x99\x00\x00\x00\x9a\x00\x00\x00\x9b\x00\x00\x00\x9c\x00\x00\x00\x9d\x00\x00\x00\x9e\x00\x00\x00\x9f\x00\x00\x00\xa0\x00\x00\x00NSt3__210__function6__funcIZN3RBX8Instance8toStringENS_3anyEEUlRKS4_E17_NS_9allocatorIS7_EEFNS_12basic_stringIcNS_11char_traitsIcEENS8_IcEEEES6_EEE\x00H\'\x00\x00`\x1e\x00\x00\x1c\x0c\x00\x00\x00\x00\x00\x00ZN3RBX8Instance8toStringENSt3__23anyEEUlRKS2_E17_\x00\x00\x00 \'\x00\x00\x04\x1f\x00\x00\x00\x00\x00\x00h\x1f\x00\x00\x07\x00\x00\x00\xa1\x00\x00\x00\xa2\x00\x00\x00St12bad_any_cast\x00\x00\x00\x00H\'\x00\x00T\x1f\x00\x00\xec(\x00\x00\x00\x00\x00\x00\xa4\x1f\x00\x00\x06\x00\x00\x00\xa3\x00\x00\x00\xa4\x00\x00\x00NSt3__217bad_function_callE\x00H\'\x00\x00\x88\x1f\x00\x00\x04(\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x05\x00\x00\x00\x07\x00\x00\x00\x0b\x00\x00\x00\r\x00\x00\x00\x11\x00\x00\x00\x13\x00\x00\x00\x17\x00\x00\x00\x1d\x00\x00\x00\x1f\x00\x00\x00%\x00\x00\x00)\x00\x00\x00+\x00\x00\x00/\x00\x00\x005\x00\x00\x00;\x00\x00\x00=\x00\x00\x00C\x00\x00\x00G\x00\x00\x00I\x00\x00\x00O\x00\x00\x00S\x00\x00\x00Y\x00\x00\x00a\x00\x00\x00e\x00\x00\x00g\x00\x00\x00k\x00\x00\x00m\x00\x00\x00q\x00\x00\x00\x7f\x00\x00\x00\x83\x00\x00\x00\x89\x00\x00\x00\x8b\x00\x00\x00\x95\x00\x00\x00\x97\x00\x00\x00\x9d\x00\x00\x00\xa3\x00\x00\x00\xa7\x00\x00\x00\xad\x00\x00\x00\xb3\x00\x00\x00\xb5\x00\x00\x00\xbf\x00\x00\x00\xc1\x00\x00\x00\xc5\x00\x00\x00\xc7\x00\x00\x00\xd3\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\r\x00\x00\x00\x11\x00\x00\x00\x13\x00\x00\x00\x17\x00\x00\x00\x1d\x00\x00\x00\x1f\x00\x00\x00%\x00\x00\x00)\x00\x00\x00+\x00\x00\x00/\x00\x00\x005\x00\x00\x00;\x00\x00\x00=\x00\x00\x00C\x00\x00\x00G\x00\x00\x00I\x00\x00\x00O\x00\x00\x00S\x00\x00\x00Y\x00\x00\x00a\x00\x00\x00e\x00\x00\x00g\x00\x00\x00k\x00\x00\x00m\x00\x00\x00q\x00\x00\x00y\x00\x00\x00\x7f\x00\x00\x00\x83\x00\x00\x00\x89\x00\x00\x00\x8b\x00\x00\x00\x8f\x00\x00\x00\x95\x00\x00\x00\x97\x00\x00\x00\x9d\x00\x00\x00\xa3\x00\x00\x00\xa7\x00\x00\x00\xa9\x00\x00\x00\xad\x00\x00\x00\xb3\x00\x00\x00\xb5\x00\x00\x00\xbb\x00\x00\x00\xbf\x00\x00\x00\xc1\x00\x00\x00\xc5\x00\x00\x00\xc7\x00\x00\x00\xd1\x00\x00\x00\x19\x00\x0b\x00\x19\x19\x19\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\t\x00\x00\x00\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\n\n\x19\x19\x19\x03\n\x07\x00\x01\x00\t\x0b\x18\x00\x00\t\x06\x0b\x00\x00\x0b\x00\x06\x19\x00\x00\x00\x19\x19\x19")
	rt_store_string(MEMORY_LIST[0], 8577,"\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x0b\r\x19\x19\x19\x00\r\x00\x00\x02\x00\t\x0e\x00\x00\x00\t\x00\x0e\x00\x00\x0e")
	rt_store_string(MEMORY_LIST[0], 8635,"\x0c")
	rt_store_string(MEMORY_LIST[0], 8647,"\x13\x00\x00\x00\x00\x13\x00\x00\x00\x00\t\x0c\x00\x00\x00\x00\x00\x0c\x00\x00\x0c")
	rt_store_string(MEMORY_LIST[0], 8693,"\x10")
	rt_store_string(MEMORY_LIST[0], 8705,"\x0f\x00\x00\x00\x04\x0f\x00\x00\x00\x00\t\x10\x00\x00\x00\x00\x00\x10\x00\x00\x10")
	rt_store_string(MEMORY_LIST[0], 8751,"\x12")
	rt_store_string(MEMORY_LIST[0], 8763,"\x11\x00\x00\x00\x00\x11\x00\x00\x00\x00\t\x12\x00\x00\x00\x00\x00\x12\x00\x00\x12\x00\x00\x1a\x00\x00\x00\x1a\x1a\x1a")
	rt_store_string(MEMORY_LIST[0], 8818,"\x1a\x00\x00\x00\x1a\x1a\x1a\x00\x00\x00\x00\x00\x00\t")
	rt_store_string(MEMORY_LIST[0], 8867,"\x14")
	rt_store_string(MEMORY_LIST[0], 8879,"\x17\x00\x00\x00\x00\x17\x00\x00\x00\x00\t\x14\x00\x00\x00\x00\x00\x14\x00\x00\x14")
	rt_store_string(MEMORY_LIST[0], 8925,"\x16")
	rt_store_string(MEMORY_LIST[0], 8937,"\x15\x00\x00\x00\x00\x15\x00\x00\x00\x00\t\x16\x00\x00\x00\x00\x00\x16\x00\x00\x16\x00\x000123456789ABCDEF\x00\x00\x00\x00\n\x00\x00\x00d\x00\x00\x00\xe8\x03\x00\x00\x10\'\x00\x00\xa0\x86\x01\x00@B\x0f\x00\x80\x96\x98\x00\x00\xe1\xf5\x05\x00\xca\x9a;\x00\x00\x00\x00\x00\x00\x00\x0000010203040506070809101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899")
	rt_store_string(MEMORY_LIST[0], 9240,"\n\x00\x00\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\xe8\x03\x00\x00\x00\x00\x00\x00\x10\'\x00\x00\x00\x00\x00\x00\xa0\x86\x01\x00\x00\x00\x00\x00@B\x0f\x00\x00\x00\x00\x00\x80\x96\x98\x00\x00\x00\x00\x00\x00\xe1\xf5\x05\x00\x00\x00\x00\x00\xca\x9a;\x00\x00\x00\x00\x00\xe4\x0bT\x02\x00\x00\x00\x00\xe8vH\x17\x00\x00\x00\x00\x10\xa5\xd4\xe8\x00\x00\x00\x00\xa0rN\x18\t\x00\x00\x00@z\x10\xf3Z\x00\x00\x00\x80\xc6\xa4~\x8d\x03\x00\x00\x00\xc1o\xf2\x86#\x00\x00\x00\x8a]xEc\x01\x00\x00d\xa7\xb3\xb6\xe0\r\x00\x00\xe8\x89\x04#\xc7\x8aN10__cxxabiv116__shim_type_infoE\x00\x00\x00\x00H\'\x00\x00\xb0$\x00\x00\xd8(\x00\x00N10__cxxabiv117__class_type_infoE\x00\x00\x00H\'\x00\x00\xe0$\x00\x00\xd4$\x00\x00N10__cxxabiv117__pbase_type_infoE\x00\x00\x00H\'\x00\x00\x10%\x00\x00\xd4$\x00\x00N10__cxxabiv119__pointer_type_infoE\x00H\'\x00\x00@%\x00\x004%\x00\x00N10__cxxabiv120__function_type_infoE\x00\x00\x00\x00H\'\x00\x00p%\x00\x00\xd4$\x00\x00N10__cxxabiv129__pointer_to_member_type_infoE\x00\x00\x00H\'\x00\x00\xa4%\x00\x004%\x00\x00\x00\x00\x00\x00$&\x00\x00\xa8\x00\x00\x00\xa9\x00\x00\x00\xaa\x00\x00\x00\xab\x00\x00\x00\xac\x00\x00\x00N10__cxxabiv123__fundamental_type_infoE\x00H\'\x00\x00\xfc%\x00\x00\xd4$\x00\x00v\x00\x00\x00\xe8%\x00\x000&\x00\x00Dn\x00\x00\xe8%\x00\x00<&\x00\x00b\x00\x00\x00\xe8%\x00\x00H&\x00\x00c\x00\x00\x00\xe8%\x00\x00T&\x00\x00Pc\x00\x00\xa4\'\x00\x00`&\x00\x00\x00\x00\x00\x00X&\x00\x00PKc\x00\xa4\'\x00\x00t&\x00\x00\x01\x00\x00\x00X&\x00\x00h\x00\x00\x00\xe8%\x00\x00\x88&\x00\x00s\x00\x00\x00\xe8%\x00\x00\x94&\x00\x00t\x00\x00\x00\xe8%\x00\x00\xa0&\x00\x00i\x00\x00\x00\xe8%\x00\x00\xac&\x00\x00j\x00\x00\x00\xe8%\x00\x00\xb8&\x00\x00l\x00\x00\x00\xe8%\x00\x00\xc4&\x00\x00m\x00\x00\x00\xe8%\x00\x00\xd0&\x00\x00x\x00\x00\x00\xe8%\x00\x00\xdc&\x00\x00y\x00\x00\x00\xe8%\x00\x00\xe8&\x00\x00f\x00\x00\x00\xe8%\x00\x00\xf4&\x00\x00d\x00\x00\x00\xe8%\x00\x00\x00\'\x00\x00e\x00\x00\x00\xe8%\x00\x00\x0c\'\x00\x00\x00\x00\x00\x00\x04%\x00\x00\xa8\x00\x00\x00\xad\x00\x00\x00\xaa\x00\x00\x00\xab\x00\x00\x00\xae\x00\x00\x00\xaf\x00\x00\x00\xb0\x00\x00\x00\xb1\x00\x00\x00\x00\x00\x00\x00\x90\'\x00\x00\xa8\x00\x00\x00\xb2\x00\x00\x00\xaa\x00\x00\x00\xab\x00\x00\x00\xae\x00\x00\x00\xb3\x00\x00\x00\xb4\x00\x00\x00\xb5\x00\x00\x00N10__cxxabiv120__si_class_type_infoE\x00\x00\x00\x00H\'\x00\x00h\'\x00\x00\x04%\x00\x00\x00\x00\x00\x00d%\x00\x00\xa8\x00\x00\x00\xb6\x00\x00\x00\xaa\x00\x00\x00\xab\x00\x00\x00\xb7\x00\x00\x00\x00\x00\x00\x00\x1c(\x00\x00\x05\x00\x00\x00\xb8\x00\x00\x00\xb9\x00\x00\x00\x00\x00\x00\x00D(\x00\x00\x05\x00\x00\x00\xba\x00\x00\x00\xbb\x00\x00\x00\x00\x00\x00\x00\x04(\x00\x00\x05\x00\x00\x00\xbc\x00\x00\x00\xbd\x00\x00\x00St9exception\x00\x00\x00\x00 \'\x00\x00\xf4\'\x00\x00St9bad_alloc\x00\x00\x00\x00H\'\x00\x00\x0c(\x00\x00\x04(\x00\x00St20bad_array_new_length\x00\x00\x00\x00H\'\x00\x00((\x00\x00\x1c(\x00\x00\x00\x00\x00\x00t(\x00\x00\x04\x00\x00\x00\xbe\x00\x00\x00\xbf\x00\x00\x00St11logic_error\x00H\'\x00\x00d(\x00\x00\x04(\x00\x00\x00\x00\x00\x00\xa8(\x00\x00\x04\x00\x00\x00\xc0\x00\x00\x00\xbf\x00\x00\x00St12length_error\x00\x00\x00\x00H\'\x00\x00\x94(\x00\x00t(\x00\x00\x00\x00\x00\x00\xec(\x00\x00\x07\x00\x00\x00\xc1\x00\x00\x00\xc2\x00\x00\x00St9type_info\x00\x00\x00\x00 \'\x00\x00\xc8(\x00\x00St8bad_cast\x00H\'\x00\x00\xe0(\x00\x00\x04(")
	rt_store_string(MEMORY_LIST[0], 10488,"\xd0+\x01")
end



--// Named function dictionary
local NamedFunctionList = {
    --// WebAssembly
    ["__wasm_call_ctors"] = FUNC_LIST[4];

    --// Exported functions
    ["malloc"] = FUNC_LIST[220];
["lua_call"] = FUNC_LIST[5];
["strlen"] = FUNC_LIST[192];
["main"] = FUNC_LIST[186];
}

--// Pre-init environment function setup

--// lua_call
FUNC_LIST[5] = function (code)
    code = load_string(memory_at_0, code, NamedFunctionList.strlen(code))
    local loaded = loadstring(code)
    local ret = loaded and loaded() or ""
    if type(ret) ~= "string" then
        ret = tostring(ret)
    end
    
    local addr = NamedFunctionList.malloc(#ret + 1)
    store_string(memory_at_0, addr, ret .. "\0", #ret + 1)
    return addr
end

--// Initialize
run_init_code()
memory_at_0 = MEMORY_LIST[0]
NamedFunctionList.__wasm_call_ctors()

local function convertString(s)
    if type(s) == "string" then
        local addr = NamedFunctionList.malloc(#s + 1)
        store_string(memory_at_0, addr, s .. "\0", #s + 1)
        return addr
    else
        return load_string(memory_at_0, s, NamedFunctionList.strlen(s))
    end
end

local exitCode = NamedFunctionList.main(..., #{...})

--// Return
return {
    --// Exported functions
    malloc = NamedFunctionList.malloc,
lua_call = NamedFunctionList.lua_call,
strlen = NamedFunctionList.strlen,
main = NamedFunctionList.main,

    --// Wasynth related utilities
    NamedFunctionList = NamedFunctionList;
    convertString = convertString;
    FUNC_LIST = FUNC_LIST;
exitCode = exitCode;
}