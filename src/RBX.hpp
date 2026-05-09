// RBX.h : The roblox API, header-only include file.

#pragma once

// TODO: Reference additional headers your program requires here.
#include <math.h>
#include <string>
#include <any>
#include <vector>
#include <functional>
#include <thread>
#include <typeindex>
#include <iostream>
#include <sstream>
#include <cstdio>
#include <cstdarg>
#include <type_traits>
#include <optional>
#include <algorithm>
#include <cmath>

// lua_call is the Roblox-Lua escape hatch. The actual implementation is
// injected on the Lua side (FUNC_LIST[lua_call_offset] in dist/main.lua).
// We only need a declaration here — the matching definition lives in
// src/lua_call.cpp so emcc can't inline it into call sites and bypass
// our Lua-side override.
extern "C" const char* lua_call(const char* code);

namespace RBX_Utils
{
	namespace string
	{
		std::vector<std::string> split(std::string str, std::string delimiter)
		{
			std::vector<std::string> strings;

			size_t pos = 0;
			std::string token;
			while ((pos = str.find(delimiter)) != std::string::npos)
			{
				token = str.substr(0, pos);

				strings.push_back(token);

				str.erase(0, pos + delimiter.length());
			}

			strings.push_back(str);

			return strings;
		}
	}
}

namespace RBX
{
	void print(std::string text)
	{
		lua_call((std::string("print(\"") + text + "\")").c_str());
	}

	void warn(std::string text)
	{
		lua_call((std::string("warn(\"") + text + "\")").c_str());
	}

	void error(std::string text)
	{
		lua_call((std::string("error(\"") + text + "\")").c_str());
	}

	// Standard streams: routed to Roblox print/warn via fd_write in dist/main.lua.
	// Use these as drop-in replacements for std::cout / std::cerr.
	inline std::ostream& cout = std::cout;
	inline std::ostream& cerr = std::cerr;

	inline void flush()
	{
		std::cout.flush();
		std::cerr.flush();
		std::fflush(stdout);
		std::fflush(stderr);
	}

	inline int printf(const char* fmt, ...)
	{
		std::va_list args;
		va_start(args, fmt);
		int r = std::vprintf(fmt, args);
		va_end(args);
		return r;
	}

	inline int eprintf(const char* fmt, ...)
	{
		std::va_list args;
		va_start(args, fmt);
		int r = std::vfprintf(stderr, fmt, args);
		va_end(args);
		return r;
	}

	void wait(double seconds = 0)
	{
		lua_call((std::string("wait(") + std::to_string(seconds) + ")").c_str());
	}

	double tick()
	{
		// std::stod throws on bad input — emscripten -O3 disables the
		// exception runtime, so a stray throw aborts the program. The
		// fall-back via strtod is non-throwing and returns 0 on failure.
		const char* raw = lua_call("return tick()");
		char* end = nullptr;
		return std::strtod(raw ? raw : "", &end);
	}

	// --- Roblox datatypes -------------------------------------------------
	//
	// Each datatype mirrors its Roblox counterpart: it stores plain numeric
	// fields, supports arithmetic where Roblox does, and knows how to:
	//   - emit a Lua constructor expression (`toLuaString`)
	//   - read its value from a Lua property path (`luaReadExpr` returns the
	//     snippet, `parseFrom` decodes the comma-separated tuple)
	//
	// This is what Instance::SetProperty<T>/GetProperty<T> dispatch on.
	namespace Types
	{
		namespace detail
		{
			// Non-throwing: bad input produces 0 instead of an exception so we
			// don't depend on the C++ exception runtime (emcc -O3 disables it,
			// and a thrown stod would abort via __throw_invalid_argument).
			inline double parseDouble(const std::string& s)
			{
				if (s.empty()) return 0.0;
				char* end = nullptr;
				return std::strtod(s.c_str(), &end);
			}
			inline long long parseLL(const std::string& s)
			{
				if (s.empty()) return 0;
				char* end = nullptr;
				return std::strtoll(s.c_str(), &end, 10);
			}
			inline int parseInt(const std::string& s) { return static_cast<int>(parseLL(s)); }

			inline std::vector<double> splitDoubles(const std::string& s, char sep = ',')
			{
				std::vector<double> out;
				std::string cur;
				for (char c : s)
				{
					if (c == sep)
					{
						if (!cur.empty()) { out.push_back(parseDouble(cur)); cur.clear(); }
					}
					else if (c != ' ')
					{
						cur += c;
					}
				}
				if (!cur.empty()) out.push_back(parseDouble(cur));
				return out;
			}

			inline std::string num(double v) { return std::to_string(v); }
			inline double clamp01(double v) { return v < 0 ? 0 : (v > 1 ? 1 : v); }
			inline double lerp1(double a, double b, double t) { return a + (b - a) * t; }
		}

		// ------------------------------------------------------------------
		// Vector2 / Vector2int16
		// ------------------------------------------------------------------
		struct Vector2
		{
			double x = 0, y = 0;
			Vector2() = default;
			Vector2(double x, double y) : x(x), y(y) {}

			static Vector2 zero()  { return {0, 0}; }
			static Vector2 one()   { return {1, 1}; }
			static Vector2 xAxis() { return {1, 0}; }
			static Vector2 yAxis() { return {0, 1}; }

			std::string toLuaString() const { return "Vector2.new(" + detail::num(x) + "," + detail::num(y) + ")"; }
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " return v.X..\",\"..v.Y";
			}
			static Vector2 parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				return v.size() >= 2 ? Vector2(v[0], v[1]) : Vector2();
			}

			Vector2 operator+(const Vector2& o) const { return {x + o.x, y + o.y}; }
			Vector2 operator-(const Vector2& o) const { return {x - o.x, y - o.y}; }
			Vector2 operator*(const Vector2& o) const { return {x * o.x, y * o.y}; }
			Vector2 operator/(const Vector2& o) const { return {x / o.x, y / o.y}; }
			Vector2 operator*(double k) const { return {x * k, y * k}; }
			Vector2 operator/(double k) const { return {x / k, y / k}; }
			Vector2 operator-() const { return {-x, -y}; }
			bool operator==(const Vector2& o) const { return x == o.x && y == o.y; }

			double Magnitude() const { return std::sqrt(x * x + y * y); }
			Vector2 Unit() const { double m = Magnitude(); return m == 0 ? Vector2() : *this / m; }
			double Dot(const Vector2& o) const { return x * o.x + y * o.y; }
			double Cross(const Vector2& o) const { return x * o.y - y * o.x; }
			Vector2 Lerp(const Vector2& o, double t) const
			{
				return {detail::lerp1(x, o.x, t), detail::lerp1(y, o.y, t)};
			}
			Vector2 Min(const Vector2& o) const { return {std::min(x, o.x), std::min(y, o.y)}; }
			Vector2 Max(const Vector2& o) const { return {std::max(x, o.x), std::max(y, o.y)}; }
			Vector2 Abs() const { return {std::abs(x), std::abs(y)}; }
			Vector2 Floor() const { return {std::floor(x), std::floor(y)}; }
			Vector2 Ceil() const { return {std::ceil(x), std::ceil(y)}; }
			bool FuzzyEq(const Vector2& o, double eps = 1e-5) const
			{
				return std::abs(x - o.x) < eps && std::abs(y - o.y) < eps;
			}
		};

		struct Vector2int16
		{
			int x = 0, y = 0;
			Vector2int16() = default;
			Vector2int16(int x, int y) : x(x), y(y) {}

			std::string toLuaString() const
			{
				return "Vector2int16.new(" + std::to_string(x) + "," + std::to_string(y) + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " return v.X..\",\"..v.Y";
			}
			static Vector2int16 parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				return v.size() >= 2 ? Vector2int16((int)v[0], (int)v[1]) : Vector2int16();
			}
			Vector2int16 operator+(const Vector2int16& o) const { return {x + o.x, y + o.y}; }
			Vector2int16 operator-(const Vector2int16& o) const { return {x - o.x, y - o.y}; }
			bool operator==(const Vector2int16& o) const { return x == o.x && y == o.y; }
		};

		// ------------------------------------------------------------------
		// Vector3 / Vector3int16
		// ------------------------------------------------------------------
		struct Vector3
		{
			double x = 0, y = 0, z = 0;
			Vector3() = default;
			Vector3(double x, double y, double z) : x(x), y(y), z(z) {}

			static Vector3 zero()  { return {0, 0, 0}; }
			static Vector3 one()   { return {1, 1, 1}; }
			static Vector3 xAxis() { return {1, 0, 0}; }
			static Vector3 yAxis() { return {0, 1, 0}; }
			static Vector3 zAxis() { return {0, 0, 1}; }

			std::string toLuaString() const
			{
				return "Vector3.new(" + detail::num(x) + "," + detail::num(y) + "," + detail::num(z) + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " return v.X..\",\"..v.Y..\",\"..v.Z";
			}
			static Vector3 parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				return v.size() >= 3 ? Vector3(v[0], v[1], v[2]) : Vector3();
			}

			Vector3 operator+(const Vector3& o) const { return {x + o.x, y + o.y, z + o.z}; }
			Vector3 operator-(const Vector3& o) const { return {x - o.x, y - o.y, z - o.z}; }
			Vector3 operator*(const Vector3& o) const { return {x * o.x, y * o.y, z * o.z}; }
			Vector3 operator/(const Vector3& o) const { return {x / o.x, y / o.y, z / o.z}; }
			Vector3 operator*(double k) const { return {x * k, y * k, z * k}; }
			Vector3 operator/(double k) const { return {x / k, y / k, z / k}; }
			Vector3 operator-() const { return {-x, -y, -z}; }
			bool operator==(const Vector3& o) const { return x == o.x && y == o.y && z == o.z; }

			double Magnitude() const { return std::sqrt(x * x + y * y + z * z); }
			Vector3 Unit() const { double m = Magnitude(); return m == 0 ? Vector3() : *this / m; }
			double Dot(const Vector3& o) const { return x * o.x + y * o.y + z * o.z; }
			Vector3 Cross(const Vector3& o) const
			{
				return {y * o.z - z * o.y, z * o.x - x * o.z, x * o.y - y * o.x};
			}
			Vector3 Lerp(const Vector3& o, double t) const
			{
				return {detail::lerp1(x, o.x, t), detail::lerp1(y, o.y, t), detail::lerp1(z, o.z, t)};
			}
			Vector3 Min(const Vector3& o) const { return {std::min(x, o.x), std::min(y, o.y), std::min(z, o.z)}; }
			Vector3 Max(const Vector3& o) const { return {std::max(x, o.x), std::max(y, o.y), std::max(z, o.z)}; }
			Vector3 Abs() const { return {std::abs(x), std::abs(y), std::abs(z)}; }
			Vector3 Floor() const { return {std::floor(x), std::floor(y), std::floor(z)}; }
			Vector3 Ceil() const { return {std::ceil(x), std::ceil(y), std::ceil(z)}; }
			Vector3 Sign() const
			{
				auto s = [](double v) { return (v > 0) - (v < 0); };
				return {(double)s(x), (double)s(y), (double)s(z)};
			}
			bool FuzzyEq(const Vector3& o, double eps = 1e-5) const
			{
				return std::abs(x - o.x) < eps && std::abs(y - o.y) < eps && std::abs(z - o.z) < eps;
			}
			double AngleBetween(const Vector3& o) const
			{
				double m = Magnitude() * o.Magnitude();
				return m == 0 ? 0 : std::acos(std::max(-1.0, std::min(1.0, Dot(o) / m)));
			}
		};

		struct Vector3int16
		{
			int x = 0, y = 0, z = 0;
			Vector3int16() = default;
			Vector3int16(int x, int y, int z) : x(x), y(y), z(z) {}

			std::string toLuaString() const
			{
				return "Vector3int16.new(" + std::to_string(x) + "," + std::to_string(y) + "," + std::to_string(z) + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " return v.X..\",\"..v.Y..\",\"..v.Z";
			}
			static Vector3int16 parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				return v.size() >= 3 ? Vector3int16((int)v[0], (int)v[1], (int)v[2]) : Vector3int16();
			}
			Vector3int16 operator+(const Vector3int16& o) const { return {x + o.x, y + o.y, z + o.z}; }
			Vector3int16 operator-(const Vector3int16& o) const { return {x - o.x, y - o.y, z - o.z}; }
			bool operator==(const Vector3int16& o) const { return x == o.x && y == o.y && z == o.z; }
		};

		// ------------------------------------------------------------------
		// Color3
		// ------------------------------------------------------------------
		struct Color3
		{
			double r = 0, g = 0, b = 0;
			Color3() = default;
			Color3(double r, double g, double b) : r(r), g(g), b(b) {}

			static Color3 fromRGB(int r, int g, int b)
			{
				return Color3(r / 255.0, g / 255.0, b / 255.0);
			}
			static Color3 fromHex(const std::string& hex)
			{
				size_t off = (!hex.empty() && hex[0] == '#') ? 1 : 0;
				if (hex.size() < off + 6) return {};
				auto h2 = [&](size_t i) {
					char* e = nullptr;
					return (int)std::strtol(hex.substr(i, 2).c_str(), &e, 16);
				};
				return fromRGB(h2(off), h2(off + 2), h2(off + 4));
			}
			static Color3 fromHSV(double h, double s, double v)
			{
				h = h - std::floor(h); // wrap into [0,1)
				double c = v * s;
				double hp = h * 6.0;
				double xv = c * (1 - std::abs(std::fmod(hp, 2.0) - 1));
				double rr = 0, gg = 0, bb = 0;
				if      (hp < 1) { rr = c;  gg = xv; bb = 0;  }
				else if (hp < 2) { rr = xv; gg = c;  bb = 0;  }
				else if (hp < 3) { rr = 0;  gg = c;  bb = xv; }
				else if (hp < 4) { rr = 0;  gg = xv; bb = c;  }
				else if (hp < 5) { rr = xv; gg = 0;  bb = c;  }
				else             { rr = c;  gg = 0;  bb = xv; }
				double m = v - c;
				return Color3(rr + m, gg + m, bb + m);
			}

			std::string toLuaString() const
			{
				return "Color3.new(" + detail::num(r) + "," + detail::num(g) + "," + detail::num(b) + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " return v.R..\",\"..v.G..\",\"..v.B";
			}
			static Color3 parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				return v.size() >= 3 ? Color3(v[0], v[1], v[2]) : Color3();
			}

			bool operator==(const Color3& o) const { return r == o.r && g == o.g && b == o.b; }

			Color3 Lerp(const Color3& o, double t) const
			{
				return {detail::lerp1(r, o.r, t), detail::lerp1(g, o.g, t), detail::lerp1(b, o.b, t)};
			}
			std::string ToHex() const
			{
				auto byte = [](double v) {
					int n = (int)std::round(detail::clamp01(v) * 255);
					return n;
				};
				char buf[8];
				std::snprintf(buf, sizeof(buf), "%02X%02X%02X", byte(r), byte(g), byte(b));
				return buf;
			}
		};

		// ------------------------------------------------------------------
		// UDim / UDim2 / Rect
		// ------------------------------------------------------------------
		struct UDim
		{
			double scale = 0;
			double offset = 0;
			UDim() = default;
			UDim(double scale, double offset) : scale(scale), offset(offset) {}

			std::string toLuaString() const { return "UDim.new(" + detail::num(scale) + "," + detail::num(offset) + ")"; }
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " return v.Scale..\",\"..v.Offset";
			}
			static UDim parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				return v.size() >= 2 ? UDim(v[0], v[1]) : UDim();
			}
			UDim operator+(const UDim& o) const { return {scale + o.scale, offset + o.offset}; }
			UDim operator-(const UDim& o) const { return {scale - o.scale, offset - o.offset}; }
			UDim operator-() const { return {-scale, -offset}; }
			bool operator==(const UDim& o) const { return scale == o.scale && offset == o.offset; }
			UDim Lerp(const UDim& o, double t) const
			{
				return {detail::lerp1(scale, o.scale, t), detail::lerp1(offset, o.offset, t)};
			}
		};

		struct UDim2
		{
			UDim x;
			UDim y;
			UDim2() = default;
			UDim2(UDim x, UDim y) : x(x), y(y) {}
			UDim2(double xs, double xo, double ys, double yo) : x(xs, xo), y(ys, yo) {}

			static UDim2 fromScale(double sx, double sy) { return UDim2(sx, 0, sy, 0); }
			static UDim2 fromOffset(int ox, int oy) { return UDim2(0, ox, 0, oy); }

			std::string toLuaString() const
			{
				return "UDim2.new(" + detail::num(x.scale) + "," + detail::num(x.offset) +
				       "," + detail::num(y.scale) + "," + detail::num(y.offset) + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path +
				       " return v.X.Scale..\",\"..v.X.Offset..\",\"..v.Y.Scale..\",\"..v.Y.Offset";
			}
			static UDim2 parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				return v.size() >= 4 ? UDim2(v[0], v[1], v[2], v[3]) : UDim2();
			}
			UDim2 operator+(const UDim2& o) const { return {x + o.x, y + o.y}; }
			UDim2 operator-(const UDim2& o) const { return {x - o.x, y - o.y}; }
			UDim2 operator-() const { return {-x, -y}; }
			bool operator==(const UDim2& o) const { return x == o.x && y == o.y; }
			UDim2 Lerp(const UDim2& o, double t) const { return {x.Lerp(o.x, t), y.Lerp(o.y, t)}; }
		};

		struct Rect
		{
			Vector2 min;
			Vector2 max;
			Rect() = default;
			Rect(Vector2 mn, Vector2 mx) : min(mn), max(mx) {}
			Rect(double minx, double miny, double maxx, double maxy)
				: min(minx, miny), max(maxx, maxy) {}

			double Width() const { return max.x - min.x; }
			double Height() const { return max.y - min.y; }

			std::string toLuaString() const
			{
				return "Rect.new(" + detail::num(min.x) + "," + detail::num(min.y) + "," +
				       detail::num(max.x) + "," + detail::num(max.y) + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " return v.Min.X..\",\"..v.Min.Y..\",\"..v.Max.X..\",\"..v.Max.Y";
			}
			static Rect parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				return v.size() >= 4 ? Rect(v[0], v[1], v[2], v[3]) : Rect();
			}
		};

		// ------------------------------------------------------------------
		// CFrame
		// ------------------------------------------------------------------
		struct CFrame
		{
			// Components in Roblox order:
			//   [0..2]   position (x, y, z)
			//   [3..11]  3x3 rotation, row-major: r00 r01 r02  r10 r11 r12  r20 r21 r22
			double components[12]{};

			CFrame()
			{
				components[0] = 0; components[1] = 0; components[2] = 0;
				components[3] = 1; components[7] = 1; components[11] = 1;
			}
			CFrame(double x, double y, double z) : CFrame()
			{
				components[0] = x; components[1] = y; components[2] = z;
			}
			CFrame(const Vector3& pos) : CFrame(pos.x, pos.y, pos.z) {}
			CFrame(const Vector3& pos, const Vector3& lookAtPos)
			{
				*this = CFrame::lookAt(pos, lookAtPos);
			}

			static CFrame identity() { return CFrame(); }
			static CFrame lookAt(const Vector3& eye, const Vector3& target, Vector3 up = Vector3::yAxis())
			{
				Vector3 forward = (target - eye).Unit();
				Vector3 right = forward.Cross(up).Unit();
				Vector3 trueUp = right.Cross(forward).Unit();
				CFrame cf;
				cf.components[0] = eye.x; cf.components[1] = eye.y; cf.components[2] = eye.z;
				// Roblox CFrame's "look" direction is -Z.
				cf.components[3]  = right.x;   cf.components[4]  = trueUp.x;   cf.components[5]  = -forward.x;
				cf.components[6]  = right.y;   cf.components[7]  = trueUp.y;   cf.components[8]  = -forward.y;
				cf.components[9]  = right.z;   cf.components[10] = trueUp.z;   cf.components[11] = -forward.z;
				return cf;
			}
			static CFrame fromOrientation(double rx, double ry, double rz)
			{
				// Y * X * Z (Roblox order)
				double cx = std::cos(rx), sx = std::sin(rx);
				double cy = std::cos(ry), sy = std::sin(ry);
				double cz = std::cos(rz), sz = std::sin(rz);
				CFrame cf;
				cf.components[3]  = cy * cz + sy * sx * sz;
				cf.components[4]  = cx * sz;
				cf.components[5]  = -sy * cz + cy * sx * sz;
				cf.components[6]  = -cy * sz + sy * sx * cz;
				cf.components[7]  = cx * cz;
				cf.components[8]  = sy * sz + cy * sx * cz;
				cf.components[9]  = sy * cx;
				cf.components[10] = -sx;
				cf.components[11] = cy * cx;
				return cf;
			}
			static CFrame fromAxisAngle(const Vector3& axis, double angle)
			{
				Vector3 a = axis.Unit();
				double c = std::cos(angle), s = std::sin(angle), t = 1 - c;
				CFrame cf;
				cf.components[3]  = t * a.x * a.x + c;
				cf.components[4]  = t * a.x * a.y - s * a.z;
				cf.components[5]  = t * a.x * a.z + s * a.y;
				cf.components[6]  = t * a.x * a.y + s * a.z;
				cf.components[7]  = t * a.y * a.y + c;
				cf.components[8]  = t * a.y * a.z - s * a.x;
				cf.components[9]  = t * a.x * a.z - s * a.y;
				cf.components[10] = t * a.y * a.z + s * a.x;
				cf.components[11] = t * a.z * a.z + c;
				return cf;
			}

			std::string toLuaString() const
			{
				std::string s = "CFrame.new(";
				for (int i = 0; i < 12; ++i)
				{
					if (i) s += ",";
					s += detail::num(components[i]);
				}
				return s + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " return table.concat({v:GetComponents()},\",\")";
			}
			static CFrame parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				CFrame cf;
				for (size_t i = 0; i < v.size() && i < 12; ++i) cf.components[i] = v[i];
				return cf;
			}

			Vector3 Position() const { return {components[0], components[1], components[2]}; }
			Vector3 RightVector()  const { return {components[3], components[6], components[9]}; }
			Vector3 UpVector()     const { return {components[4], components[7], components[10]}; }
			Vector3 LookVector()   const { return {-components[5], -components[8], -components[11]}; }
			double X() const { return components[0]; }
			double Y() const { return components[1]; }
			double Z() const { return components[2]; }

			CFrame Inverse() const
			{
				// Pure rotation+translation, so inverse is transpose of rotation
				// applied to negated translation.
				CFrame r;
				r.components[3]  = components[3];  r.components[4]  = components[6];  r.components[5]  = components[9];
				r.components[6]  = components[4];  r.components[7]  = components[7];  r.components[8]  = components[10];
				r.components[9]  = components[5];  r.components[10] = components[8];  r.components[11] = components[11];
				Vector3 p = -Position();
				r.components[0] = r.components[3] * p.x + r.components[4] * p.y + r.components[5] * p.z;
				r.components[1] = r.components[6] * p.x + r.components[7] * p.y + r.components[8] * p.z;
				r.components[2] = r.components[9] * p.x + r.components[10] * p.y + r.components[11] * p.z;
				return r;
			}

			Vector3 PointToWorldSpace(const Vector3& p) const
			{
				return {
					components[0] + components[3] * p.x + components[4] * p.y + components[5] * p.z,
					components[1] + components[6] * p.x + components[7] * p.y + components[8] * p.z,
					components[2] + components[9] * p.x + components[10] * p.y + components[11] * p.z,
				};
			}
			Vector3 VectorToWorldSpace(const Vector3& v) const
			{
				return {
					components[3] * v.x + components[4] * v.y + components[5] * v.z,
					components[6] * v.x + components[7] * v.y + components[8] * v.z,
					components[9] * v.x + components[10] * v.y + components[11] * v.z,
				};
			}
			Vector3 PointToObjectSpace(const Vector3& p) const { return Inverse().PointToWorldSpace(p); }
			Vector3 VectorToObjectSpace(const Vector3& v) const { return Inverse().VectorToWorldSpace(v); }

			CFrame operator*(const CFrame& o) const
			{
				CFrame r;
				const double* a = components;
				const double* b = o.components;
				// rotation = a.rot * b.rot (3x3 row-major)
				for (int i = 0; i < 3; ++i)
					for (int j = 0; j < 3; ++j)
						r.components[3 + i * 3 + j] =
							a[3 + i * 3 + 0] * b[3 + 0 * 3 + j] +
							a[3 + i * 3 + 1] * b[3 + 1 * 3 + j] +
							a[3 + i * 3 + 2] * b[3 + 2 * 3 + j];
				// position = a.pos + a.rot * b.pos
				Vector3 bp = o.Position();
				r.components[0] = a[0] + a[3] * bp.x + a[4] * bp.y + a[5] * bp.z;
				r.components[1] = a[1] + a[6] * bp.x + a[7] * bp.y + a[8] * bp.z;
				r.components[2] = a[2] + a[9] * bp.x + a[10] * bp.y + a[11] * bp.z;
				return r;
			}
			Vector3 operator*(const Vector3& v) const { return PointToWorldSpace(v); }
			CFrame operator+(const Vector3& v) const
			{
				CFrame r = *this;
				r.components[0] += v.x; r.components[1] += v.y; r.components[2] += v.z;
				return r;
			}
			CFrame operator-(const Vector3& v) const { return *this + (-v); }
		};

		// ------------------------------------------------------------------
		// BrickColor
		// ------------------------------------------------------------------
		struct BrickColor
		{
			int number = 1;
			BrickColor() = default;
			BrickColor(int n) : number(n) {}

			static BrickColor White()    { return BrickColor(1); }
			static BrickColor Gray()     { return BrickColor(194); }
			static BrickColor DarkGray() { return BrickColor(199); }
			static BrickColor Black()    { return BrickColor(26); }
			static BrickColor Red()      { return BrickColor(21); }
			static BrickColor Yellow()   { return BrickColor(24); }
			static BrickColor Green()    { return BrickColor(28); }
			static BrickColor Blue()     { return BrickColor(23); }

			std::string toLuaString() const { return "BrickColor.new(" + std::to_string(number) + ")"; }
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " return tostring(v.Number)";
			}
			static BrickColor parseFrom(const std::string& s) { return BrickColor(detail::parseInt(s)); }
		};

		// ------------------------------------------------------------------
		// Region3 / Region3int16
		// ------------------------------------------------------------------
		struct Region3
		{
			Vector3 min;
			Vector3 max;
			Region3() = default;
			Region3(Vector3 mn, Vector3 mx) : min(mn), max(mx) {}

			Vector3 Size() const { return max - min; }
			Vector3 Center() const { return (min + max) * 0.5; }
			Types::CFrame GetCFrame() const { return Types::CFrame(Center()); }

			std::string toLuaString() const
			{
				return "Region3.new(" + min.toLuaString() + "," + max.toLuaString() + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path +
				       " local a,b=v.CFrame.Position-v.Size/2,v.CFrame.Position+v.Size/2 "
				       "return a.X..\",\"..a.Y..\",\"..a.Z..\",\"..b.X..\",\"..b.Y..\",\"..b.Z";
			}
			static Region3 parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				return v.size() >= 6
					? Region3(Vector3(v[0], v[1], v[2]), Vector3(v[3], v[4], v[5]))
					: Region3();
			}
		};

		struct Region3int16
		{
			Vector3int16 min;
			Vector3int16 max;
			Region3int16() = default;
			Region3int16(Vector3int16 mn, Vector3int16 mx) : min(mn), max(mx) {}

			std::string toLuaString() const
			{
				return "Region3int16.new(" + min.toLuaString() + "," + max.toLuaString() + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path +
				       " return v.Min.X..\",\"..v.Min.Y..\",\"..v.Min.Z..\",\"..v.Max.X..\",\"..v.Max.Y..\",\"..v.Max.Z";
			}
			static Region3int16 parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				return v.size() >= 6
					? Region3int16(Vector3int16((int)v[0], (int)v[1], (int)v[2]),
					               Vector3int16((int)v[3], (int)v[4], (int)v[5]))
					: Region3int16();
			}
		};

		// ------------------------------------------------------------------
		// Ray
		// ------------------------------------------------------------------
		struct Ray
		{
			Vector3 origin;
			Vector3 direction;
			Ray() = default;
			Ray(Vector3 o, Vector3 d) : origin(o), direction(d) {}

			Vector3 ClosestPoint(const Vector3& p) const
			{
				Vector3 diff = p - origin;
				double t = std::max(0.0, diff.Dot(direction.Unit()));
				return origin + direction.Unit() * t;
			}
			double Distance(const Vector3& p) const { return (ClosestPoint(p) - p).Magnitude(); }

			std::string toLuaString() const
			{
				return "Ray.new(" + origin.toLuaString() + "," + direction.toLuaString() + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path +
				       " return v.Origin.X..\",\"..v.Origin.Y..\",\"..v.Origin.Z..\",\""
				       "..v.Direction.X..\",\"..v.Direction.Y..\",\"..v.Direction.Z";
			}
			static Ray parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				return v.size() >= 6
					? Ray(Vector3(v[0], v[1], v[2]), Vector3(v[3], v[4], v[5]))
					: Ray();
			}
		};

		// ------------------------------------------------------------------
		// NumberRange
		// ------------------------------------------------------------------
		struct NumberRange
		{
			double min = 0;
			double max = 0;
			NumberRange() = default;
			explicit NumberRange(double both) : min(both), max(both) {}
			NumberRange(double mn, double mx) : min(mn), max(mx) {}

			std::string toLuaString() const
			{
				return "NumberRange.new(" + detail::num(min) + "," + detail::num(max) + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " return v.Min..\",\"..v.Max";
			}
			static NumberRange parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				return v.size() >= 2 ? NumberRange(v[0], v[1]) : NumberRange();
			}
		};

		// ------------------------------------------------------------------
		// NumberSequence / ColorSequence
		// ------------------------------------------------------------------
		struct NumberSequenceKeypoint
		{
			double time = 0;
			double value = 0;
			double envelope = 0;
			NumberSequenceKeypoint() = default;
			NumberSequenceKeypoint(double t, double v, double env = 0) : time(t), value(v), envelope(env) {}
		};

		struct NumberSequence
		{
			std::vector<NumberSequenceKeypoint> keypoints;
			NumberSequence() = default;
			explicit NumberSequence(double constant)
			{
				keypoints.push_back({0, constant});
				keypoints.push_back({1, constant});
			}
			NumberSequence(double a, double b)
			{
				keypoints.push_back({0, a});
				keypoints.push_back({1, b});
			}
			NumberSequence(std::vector<NumberSequenceKeypoint> kps) : keypoints(std::move(kps)) {}

			std::string toLuaString() const
			{
				std::string s = "NumberSequence.new({";
				for (size_t i = 0; i < keypoints.size(); ++i)
				{
					if (i) s += ",";
					s += "NumberSequenceKeypoint.new(" + detail::num(keypoints[i].time) + "," +
					     detail::num(keypoints[i].value) + "," + detail::num(keypoints[i].envelope) + ")";
				}
				return s + "})";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				// Encodes keypoints as "t,v,e;t,v,e;..."
				return "local v=" + path + " local t={} for _,k in ipairs(v.Keypoints) do "
				       "table.insert(t,k.Time..\",\"..k.Value..\",\"..k.Envelope) end "
				       "return table.concat(t,\";\")";
			}
			static NumberSequence parseFrom(const std::string& s)
			{
				NumberSequence ns;
				std::string cur;
				auto pushKp = [&](const std::string& chunk) {
					auto v = detail::splitDoubles(chunk);
					if (v.size() >= 2)
						ns.keypoints.push_back({v[0], v[1], v.size() >= 3 ? v[2] : 0});
				};
				for (char c : s)
				{
					if (c == ';') { pushKp(cur); cur.clear(); }
					else cur += c;
				}
				if (!cur.empty()) pushKp(cur);
				return ns;
			}
		};

		struct ColorSequenceKeypoint
		{
			double time = 0;
			Color3 value;
			ColorSequenceKeypoint() = default;
			ColorSequenceKeypoint(double t, Color3 v) : time(t), value(v) {}
		};

		struct ColorSequence
		{
			std::vector<ColorSequenceKeypoint> keypoints;
			ColorSequence() = default;
			explicit ColorSequence(Color3 constant)
			{
				keypoints.push_back({0, constant});
				keypoints.push_back({1, constant});
			}
			ColorSequence(Color3 a, Color3 b)
			{
				keypoints.push_back({0, a});
				keypoints.push_back({1, b});
			}
			ColorSequence(std::vector<ColorSequenceKeypoint> kps) : keypoints(std::move(kps)) {}

			std::string toLuaString() const
			{
				std::string s = "ColorSequence.new({";
				for (size_t i = 0; i < keypoints.size(); ++i)
				{
					if (i) s += ",";
					s += "ColorSequenceKeypoint.new(" + detail::num(keypoints[i].time) + "," +
					     keypoints[i].value.toLuaString() + ")";
				}
				return s + "})";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " local t={} for _,k in ipairs(v.Keypoints) do "
				       "table.insert(t,k.Time..\",\"..k.Value.R..\",\"..k.Value.G..\",\"..k.Value.B) end "
				       "return table.concat(t,\";\")";
			}
			static ColorSequence parseFrom(const std::string& s)
			{
				ColorSequence cs;
				std::string cur;
				auto pushKp = [&](const std::string& chunk) {
					auto v = detail::splitDoubles(chunk);
					if (v.size() >= 4)
						cs.keypoints.push_back({v[0], Color3(v[1], v[2], v[3])});
				};
				for (char c : s)
				{
					if (c == ';') { pushKp(cur); cur.clear(); }
					else cur += c;
				}
				if (!cur.empty()) pushKp(cur);
				return cs;
			}
		};

		// ------------------------------------------------------------------
		// PhysicalProperties
		// ------------------------------------------------------------------
		struct PhysicalProperties
		{
			double density = 0.7;
			double friction = 0.3;
			double elasticity = 0.5;
			double frictionWeight = 1;
			double elasticityWeight = 1;

			PhysicalProperties() = default;
			PhysicalProperties(double d, double f, double e) : density(d), friction(f), elasticity(e) {}
			PhysicalProperties(double d, double f, double e, double fw, double ew)
				: density(d), friction(f), elasticity(e), frictionWeight(fw), elasticityWeight(ew) {}

			std::string toLuaString() const
			{
				return "PhysicalProperties.new(" + detail::num(density) + "," + detail::num(friction) + "," +
				       detail::num(elasticity) + "," + detail::num(frictionWeight) + "," +
				       detail::num(elasticityWeight) + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " return v.Density..\",\"..v.Friction..\",\"..v.Elasticity"
				       "..\",\"..v.FrictionWeight..\",\"..v.ElasticityWeight";
			}
			static PhysicalProperties parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				PhysicalProperties p;
				if (v.size() >= 1) p.density = v[0];
				if (v.size() >= 2) p.friction = v[1];
				if (v.size() >= 3) p.elasticity = v[2];
				if (v.size() >= 4) p.frictionWeight = v[3];
				if (v.size() >= 5) p.elasticityWeight = v[4];
				return p;
			}
		};

		// ------------------------------------------------------------------
		// EnumItem — mirrors Roblox's Enum.<Type>.<Name>
		// ------------------------------------------------------------------
		// An EnumItem is the smallest unit: it knows which EnumType it belongs
		// to ("EasingStyle"), its symbolic name ("Linear"), and the integer
		// value Roblox assigns to it. Use the structs in the `RBX::Enum`
		// namespace (e.g. `RBX::Enum::EasingStyle::Linear`) for the canonical
		// instances; constructing one directly is fine for arbitrary Enum
		// types we did not bake in (KeyCode, Material, ...).
		struct EnumItem
		{
			std::string enumType;
			std::string name;
			int value = 0;

			EnumItem() = default;
			EnumItem(std::string et, std::string n, int v)
				: enumType(std::move(et)), name(std::move(n)), value(v) {}

			std::string toLuaString() const
			{
				return "Enum." + enumType + "." + name;
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path +
				       " return v.EnumType.Name..\";\"..v.Name..\";\"..tostring(v.Value)";
			}
			static EnumItem parseFrom(const std::string& s)
			{
				EnumItem e;
				size_t a = s.find(';');
				if (a == std::string::npos) { e.name = s; return e; }
				size_t b = s.find(';', a + 1);
				e.enumType = s.substr(0, a);
				if (b == std::string::npos)
				{
					e.name = s.substr(a + 1);
				}
				else
				{
					e.name = s.substr(a + 1, b - a - 1);
					e.value = detail::parseInt(s.substr(b + 1));
				}
				return e;
			}

			bool operator==(const EnumItem& o) const
			{
				return enumType == o.enumType && name == o.name;
			}
			bool operator!=(const EnumItem& o) const { return !(*this == o); }

			// Implicit conversion to bool so `if (item)` checks for "set" state.
			explicit operator bool() const { return !enumType.empty() && !name.empty(); }
		};

		inline std::ostream& operator<<(std::ostream& os, const EnumItem& e)
		{
			return os << "Enum." << e.enumType << "." << e.name;
		}

		// ------------------------------------------------------------------
		// TweenInfo
		// ------------------------------------------------------------------
		struct TweenInfo
		{
			double time = 1.0;
			EnumItem style{"EasingStyle", "Quad", 0};
			EnumItem direction{"EasingDirection", "Out", 1};
			int repeatCount = 0;
			bool reverses = false;
			double delayTime = 0;

			TweenInfo() = default;
			TweenInfo(double t) : time(t) {}
			TweenInfo(double t, EnumItem s, EnumItem d = EnumItem{"EasingDirection", "Out", 1},
			          int rc = 0, bool rev = false, double dt = 0)
				: time(t), style(std::move(s)), direction(std::move(d)),
				  repeatCount(rc), reverses(rev), delayTime(dt) {}

			std::string toLuaString() const
			{
				return std::string("TweenInfo.new(") + detail::num(time) + "," +
				       style.toLuaString() + "," + direction.toLuaString() +
				       "," + std::to_string(repeatCount) + "," + (reverses ? "true" : "false") + "," +
				       detail::num(delayTime) + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " return v.Time..\",\"..v.RepeatCount..\",\"..v.DelayTime";
			}
			static TweenInfo parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				TweenInfo ti;
				if (v.size() >= 1) ti.time = v[0];
				if (v.size() >= 2) ti.repeatCount = (int)v[1];
				if (v.size() >= 3) ti.delayTime = v[2];
				return ti;
			}
		};

		// ------------------------------------------------------------------
		// PathWaypoint
		// ------------------------------------------------------------------
		struct PathWaypoint
		{
			Vector3 position;
			int action = 0; // Enum.PathWaypointAction; 0 = Walk, 1 = Jump, 2 = Custom

			PathWaypoint() = default;
			PathWaypoint(Vector3 p, int a = 0) : position(p), action(a) {}

			std::string toLuaString() const
			{
				const char* actionName = action == 1 ? "Jump" : (action == 2 ? "Custom" : "Walk");
				return std::string("PathWaypoint.new(") + position.toLuaString() +
				       ",Enum.PathWaypointAction." + actionName + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path +
				       " return v.Position.X..\",\"..v.Position.Y..\",\"..v.Position.Z..\",\"..v.Action.Value";
			}
			static PathWaypoint parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				return v.size() >= 4
					? PathWaypoint(Vector3(v[0], v[1], v[2]), (int)v[3])
					: PathWaypoint();
			}
		};

		// ------------------------------------------------------------------
		// FloatCurveKey
		// ------------------------------------------------------------------
		struct FloatCurveKey
		{
			double time = 0;
			double value = 0;
			std::string interpolation = "Linear"; // "Constant" | "Linear" | "Cubic"

			FloatCurveKey() = default;
			FloatCurveKey(double t, double v, std::string interp = "Linear")
				: time(t), value(v), interpolation(std::move(interp)) {}

			std::string toLuaString() const
			{
				return std::string("FloatCurveKey.new(") + detail::num(time) + "," + detail::num(value) +
				       ",Enum.KeyInterpolationMode." + interpolation + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " return v.Time..\",\"..v.Value";
			}
			static FloatCurveKey parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				return v.size() >= 2 ? FloatCurveKey(v[0], v[1]) : FloatCurveKey();
			}
		};

		// ------------------------------------------------------------------
		// Font
		// ------------------------------------------------------------------
		struct Font
		{
			std::string family = "rbxasset://fonts/families/SourceSansPro.json";
			int weight = 400;
			std::string style = "Normal"; // "Normal" | "Italic"

			Font() = default;
			Font(std::string fam, int w = 400, std::string st = "Normal")
				: family(std::move(fam)), weight(w), style(std::move(st)) {}

			std::string toLuaString() const
			{
				return std::string("Font.new(\"") + family + "\",Enum.FontWeight." + weightName() +
				       ",Enum.FontStyle." + style + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " return v.Family..\";\"..v.Weight.Value..\";\"..v.Style.Name";
			}
			static Font parseFrom(const std::string& s)
			{
				Font f;
				size_t a = s.find(';');
				if (a == std::string::npos) return f;
				size_t b = s.find(';', a + 1);
				f.family = s.substr(0, a);
				f.weight = detail::parseInt(s.substr(a + 1, b - a - 1));
				if (b != std::string::npos) f.style = s.substr(b + 1);
				return f;
			}

		private:
			const char* weightName() const
			{
				if (weight <= 100) return "Thin";
				if (weight <= 200) return "ExtraLight";
				if (weight <= 300) return "Light";
				if (weight <= 400) return "Regular";
				if (weight <= 500) return "Medium";
				if (weight <= 600) return "SemiBold";
				if (weight <= 700) return "Bold";
				if (weight <= 800) return "ExtraBold";
				return "Heavy";
			}
		};

		// ------------------------------------------------------------------
		// Axes / Faces
		// ------------------------------------------------------------------
		struct Axes
		{
			bool x = false, y = false, z = false;
			Axes() = default;
			Axes(bool x, bool y, bool z) : x(x), y(y), z(z) {}

			std::string toLuaString() const
			{
				std::string s = "Axes.new(";
				bool first = true;
				if (x) { s += "Enum.Axis.X"; first = false; }
				if (y) { if (!first) s += ","; s += "Enum.Axis.Y"; first = false; }
				if (z) { if (!first) s += ","; s += "Enum.Axis.Z"; }
				return s + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path +
				       " return (v.X and 1 or 0)..\",\"..(v.Y and 1 or 0)..\",\"..(v.Z and 1 or 0)";
			}
			static Axes parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				return v.size() >= 3 ? Axes(v[0] != 0, v[1] != 0, v[2] != 0) : Axes();
			}
		};

		struct Faces
		{
			bool top = false, bottom = false, left = false, right = false, front = false, back = false;
			Faces() = default;

			std::string toLuaString() const
			{
				std::string s = "Faces.new(";
				bool first = true;
				auto add = [&](bool flag, const char* name) {
					if (!flag) return;
					if (!first) s += ",";
					s += "Enum.NormalId.";
					s += name;
					first = false;
				};
				add(top, "Top"); add(bottom, "Bottom"); add(left, "Left");
				add(right, "Right"); add(front, "Front"); add(back, "Back");
				return s + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path +
				       " return (v.Top and 1 or 0)..\",\"..(v.Bottom and 1 or 0)..\",\""
				       "..(v.Left and 1 or 0)..\",\"..(v.Right and 1 or 0)..\",\""
				       "..(v.Front and 1 or 0)..\",\"..(v.Back and 1 or 0)";
			}
			static Faces parseFrom(const std::string& s)
			{
				auto v = detail::splitDoubles(s);
				Faces f;
				if (v.size() >= 6)
				{
					f.top = v[0] != 0; f.bottom = v[1] != 0; f.left = v[2] != 0;
					f.right = v[3] != 0; f.front = v[4] != 0; f.back = v[5] != 0;
				}
				return f;
			}
		};

		// ------------------------------------------------------------------
		// DateTime
		// ------------------------------------------------------------------
		struct DateTime
		{
			long long unixTimestampMillis = 0;

			DateTime() = default;
			explicit DateTime(long long ms) : unixTimestampMillis(ms) {}

			static DateTime fromUnixTimestamp(long long s) { return DateTime(s * 1000); }
			static DateTime fromUnixTimestampMillis(long long ms) { return DateTime(ms); }

			std::string toLuaString() const
			{
				return std::string("DateTime.fromUnixTimestampMillis(") +
				       std::to_string(unixTimestampMillis) + ")";
			}
			static std::string luaReadExpr(const std::string& path)
			{
				return "local v=" + path + " return tostring(v.UnixTimestampMillis)";
			}
			static DateTime parseFrom(const std::string& s)
			{
				return DateTime(detail::parseLL(s));
			}
		};

		// ------------------------------------------------------------------
		// Concept + stream operators
		// ------------------------------------------------------------------
		template <typename T, typename = void>
		struct is_lua_datatype : std::false_type {};

		template <typename T>
		struct is_lua_datatype<T, std::void_t<
			decltype(std::declval<const T&>().toLuaString()),
			decltype(T::luaReadExpr(std::declval<std::string>())),
			decltype(T::parseFrom(std::declval<std::string>()))>> : std::true_type {};

		template <typename T>
		inline constexpr bool is_lua_datatype_v = is_lua_datatype<T>::value;

		inline std::ostream& operator<<(std::ostream& os, const Vector2& v) { return os << "(" << v.x << ", " << v.y << ")"; }
		inline std::ostream& operator<<(std::ostream& os, const Vector2int16& v) { return os << "(" << v.x << ", " << v.y << ")"; }
		inline std::ostream& operator<<(std::ostream& os, const Vector3& v) { return os << "(" << v.x << ", " << v.y << ", " << v.z << ")"; }
		inline std::ostream& operator<<(std::ostream& os, const Vector3int16& v) { return os << "(" << v.x << ", " << v.y << ", " << v.z << ")"; }
		inline std::ostream& operator<<(std::ostream& os, const Color3& c) { return os << "Color3(" << c.r << ", " << c.g << ", " << c.b << ")"; }
		inline std::ostream& operator<<(std::ostream& os, const UDim& u) { return os << "UDim(" << u.scale << ", " << u.offset << ")"; }
		inline std::ostream& operator<<(std::ostream& os, const UDim2& u) { return os << "UDim2(" << u.x << ", " << u.y << ")"; }
		inline std::ostream& operator<<(std::ostream& os, const Rect& r) { return os << "Rect(" << r.min << ", " << r.max << ")"; }
		inline std::ostream& operator<<(std::ostream& os, const BrickColor& b) { return os << "BrickColor(" << b.number << ")"; }
		inline std::ostream& operator<<(std::ostream& os, const Region3& r) { return os << "Region3(" << r.min << ", " << r.max << ")"; }
		inline std::ostream& operator<<(std::ostream& os, const Ray& r) { return os << "Ray(" << r.origin << " -> " << r.direction << ")"; }
		inline std::ostream& operator<<(std::ostream& os, const NumberRange& n) { return os << "NumberRange(" << n.min << ", " << n.max << ")"; }
	}

	// Convenient aliases: write `RBX::Vector3` instead of `RBX::Types::Vector3`.
	using Vector2          = Types::Vector2;
	using Vector2int16     = Types::Vector2int16;
	using Vector3          = Types::Vector3;
	using Vector3int16     = Types::Vector3int16;
	using Color3           = Types::Color3;
	using UDim             = Types::UDim;
	using UDim2            = Types::UDim2;
	using Rect             = Types::Rect;
	using CFrame           = Types::CFrame;
	using BrickColor       = Types::BrickColor;
	using Region3          = Types::Region3;
	using Region3int16     = Types::Region3int16;
	using Ray              = Types::Ray;
	using NumberRange      = Types::NumberRange;
	using NumberSequence   = Types::NumberSequence;
	using NumberSequenceKeypoint = Types::NumberSequenceKeypoint;
	using ColorSequence    = Types::ColorSequence;
	using ColorSequenceKeypoint  = Types::ColorSequenceKeypoint;
	using PhysicalProperties = Types::PhysicalProperties;
	using TweenInfo        = Types::TweenInfo;
	using PathWaypoint     = Types::PathWaypoint;
	using FloatCurveKey    = Types::FloatCurveKey;
	using Font             = Types::Font;
	using Axes             = Types::Axes;
	using Faces            = Types::Faces;
	using DateTime         = Types::DateTime;
	using EnumItem         = Types::EnumItem;

	// ----------------------------------------------------------------------
	// Enum holder system — mirrors Roblox's `Enum.<Type>.<Name>` namespace.
	// ----------------------------------------------------------------------
	//
	// In Lua you write `Enum.EasingStyle.Linear` to get an EnumItem. In C++
	// the same lookup is `RBX::Enum::EasingStyle::Linear`. Each enum below
	// exposes:
	//   - one `inline static const EnumItem` per declared value
	//   - `GetEnumItems()` returning all baked-in values
	//   - `FromName(name)` / `FromValue(v)` for runtime lookup
	// For unbounded Roblox enums (KeyCode, Material, ...) only the most
	// common entries are baked in; use `FromName("Foo")` for the rest, or
	// construct an EnumItem directly.
	namespace Enum
	{
		// Helpers used by the macros below.
		namespace detail
		{
			template <typename Holder>
			inline EnumItem fromName(const std::string& name, const char* enumType)
			{
				for (const auto& it : Holder::GetEnumItems())
					if (it.name == name) return it;
				return EnumItem{enumType, name, 0};
			}
			template <typename Holder>
			inline EnumItem fromValue(int value, const char* enumType)
			{
				for (const auto& it : Holder::GetEnumItems())
					if (it.value == value) return it;
				return EnumItem{enumType, "Unknown", value};
			}
		}

#define RBX_ENUM_BEGIN(NAME) \
	struct NAME { \
		static constexpr const char* TypeName = #NAME;
#define RBX_ENUM_VALUE(NAME, ITEM, VAL) \
		inline static const EnumItem ITEM{#NAME, #ITEM, VAL};
#define RBX_ENUM_END(NAME, ...) \
		static std::vector<EnumItem> GetEnumItems() { return {__VA_ARGS__}; } \
		static EnumItem FromName(const std::string& n) { return detail::fromName<NAME>(n, #NAME); } \
		static EnumItem FromValue(int v) { return detail::fromValue<NAME>(v, #NAME); } \
	};

		RBX_ENUM_BEGIN(AccessModifierType)
	RBX_ENUM_VALUE(AccessModifierType, Allow, 0)
	RBX_ENUM_VALUE(AccessModifierType, Deny, 1)
RBX_ENUM_END(AccessModifierType, Allow, Deny)

RBX_ENUM_BEGIN(AccessoryType)
	RBX_ENUM_VALUE(AccessoryType, Unknown, 0)
	RBX_ENUM_VALUE(AccessoryType, Hat, 1)
	RBX_ENUM_VALUE(AccessoryType, Hair, 2)
	RBX_ENUM_VALUE(AccessoryType, Face, 3)
	RBX_ENUM_VALUE(AccessoryType, Neck, 4)
	RBX_ENUM_VALUE(AccessoryType, Shoulder, 5)
	RBX_ENUM_VALUE(AccessoryType, Front, 6)
	RBX_ENUM_VALUE(AccessoryType, Back, 7)
	RBX_ENUM_VALUE(AccessoryType, Waist, 8)
	RBX_ENUM_VALUE(AccessoryType, TShirt, 9)
	RBX_ENUM_VALUE(AccessoryType, Shirt, 10)
	RBX_ENUM_VALUE(AccessoryType, Pants, 11)
	RBX_ENUM_VALUE(AccessoryType, Jacket, 12)
	RBX_ENUM_VALUE(AccessoryType, Sweater, 13)
	RBX_ENUM_VALUE(AccessoryType, Shorts, 14)
	RBX_ENUM_VALUE(AccessoryType, LeftShoe, 15)
	RBX_ENUM_VALUE(AccessoryType, RightShoe, 16)
	RBX_ENUM_VALUE(AccessoryType, DressSkirt, 17)
	RBX_ENUM_VALUE(AccessoryType, Eyebrow, 18)
	RBX_ENUM_VALUE(AccessoryType, Eyelash, 19)
RBX_ENUM_END(AccessoryType, Unknown, Hat, Hair, Face, Neck, Shoulder, Front, Back, Waist, TShirt, Shirt, Pants, Jacket, Sweater, Shorts, LeftShoe, RightShoe, DressSkirt, Eyebrow, Eyelash)

RBX_ENUM_BEGIN(ActionOnAutoResumeSync)
	RBX_ENUM_VALUE(ActionOnAutoResumeSync, DontResume, 0)
	RBX_ENUM_VALUE(ActionOnAutoResumeSync, KeepStudio, 1)
	RBX_ENUM_VALUE(ActionOnAutoResumeSync, KeepLocal, 2)
RBX_ENUM_END(ActionOnAutoResumeSync, DontResume, KeepStudio, KeepLocal)

RBX_ENUM_BEGIN(ActionOnStopSync)
	RBX_ENUM_VALUE(ActionOnStopSync, AlwaysAsk, 0)
	RBX_ENUM_VALUE(ActionOnStopSync, KeepLocalFiles, 1)
	RBX_ENUM_VALUE(ActionOnStopSync, DeleteLocalFiles, 2)
RBX_ENUM_END(ActionOnStopSync, AlwaysAsk, KeepLocalFiles, DeleteLocalFiles)

RBX_ENUM_BEGIN(ActionType)
	RBX_ENUM_VALUE(ActionType, Nothing, 0)
	RBX_ENUM_VALUE(ActionType, Pause, 1)
	RBX_ENUM_VALUE(ActionType, Lose, 2)
	RBX_ENUM_VALUE(ActionType, Draw, 3)
	RBX_ENUM_VALUE(ActionType, Win, 4)
RBX_ENUM_END(ActionType, Nothing, Pause, Lose, Draw, Win)

RBX_ENUM_BEGIN(ActivePayerStatus)
	RBX_ENUM_VALUE(ActivePayerStatus, Unknown, 0)
	RBX_ENUM_VALUE(ActivePayerStatus, Never, 1)
	RBX_ENUM_VALUE(ActivePayerStatus, Lapsed, 2)
	RBX_ENUM_VALUE(ActivePayerStatus, Casual50Percent, 3)
	RBX_ENUM_VALUE(ActivePayerStatus, Intermediate35Percent, 4)
	RBX_ENUM_VALUE(ActivePayerStatus, Top15Percent, 5)
RBX_ENUM_END(ActivePayerStatus, Unknown, Never, Lapsed, Casual50Percent, Intermediate35Percent, Top15Percent)

RBX_ENUM_BEGIN(ActuatorRelativeTo)
	RBX_ENUM_VALUE(ActuatorRelativeTo, Attachment0, 0)
	RBX_ENUM_VALUE(ActuatorRelativeTo, Attachment1, 1)
	RBX_ENUM_VALUE(ActuatorRelativeTo, World, 2)
RBX_ENUM_END(ActuatorRelativeTo, Attachment0, Attachment1, World)

RBX_ENUM_BEGIN(ActuatorType)
	RBX_ENUM_VALUE(ActuatorType, None, 0)
	RBX_ENUM_VALUE(ActuatorType, Motor, 1)
	RBX_ENUM_VALUE(ActuatorType, Servo, 2)
RBX_ENUM_END(ActuatorType, None, Motor, Servo)

RBX_ENUM_BEGIN(AdAvailabilityResult)
	RBX_ENUM_VALUE(AdAvailabilityResult, IsAvailable, 1)
	RBX_ENUM_VALUE(AdAvailabilityResult, DeviceIneligible, 2)
	RBX_ENUM_VALUE(AdAvailabilityResult, ExperienceIneligible, 3)
	RBX_ENUM_VALUE(AdAvailabilityResult, InternalError, 4)
	RBX_ENUM_VALUE(AdAvailabilityResult, NoFill, 5)
	RBX_ENUM_VALUE(AdAvailabilityResult, PlayerIneligible, 6)
	RBX_ENUM_VALUE(AdAvailabilityResult, PublisherIneligible, 7)
RBX_ENUM_END(AdAvailabilityResult, IsAvailable, DeviceIneligible, ExperienceIneligible, InternalError, NoFill, PlayerIneligible, PublisherIneligible)

RBX_ENUM_BEGIN(AdEventType)
	RBX_ENUM_VALUE(AdEventType, VideoLoaded, 0)
	RBX_ENUM_VALUE(AdEventType, VideoRemoved, 1)
	RBX_ENUM_VALUE(AdEventType, UserCompletedVideo, 2)
	RBX_ENUM_VALUE(AdEventType, RewardedAdLoaded, 3)
	RBX_ENUM_VALUE(AdEventType, RewardedAdGrant, 4)
	RBX_ENUM_VALUE(AdEventType, RewardedAdUnloaded, 5)
RBX_ENUM_END(AdEventType, VideoLoaded, VideoRemoved, UserCompletedVideo, RewardedAdLoaded, RewardedAdGrant, RewardedAdUnloaded)

RBX_ENUM_BEGIN(AdFormat)
	RBX_ENUM_VALUE(AdFormat, RewardedVideo, 0)
RBX_ENUM_END(AdFormat, RewardedVideo)

RBX_ENUM_BEGIN(AdShape)
	RBX_ENUM_VALUE(AdShape, HorizontalRectangle, 1)
RBX_ENUM_END(AdShape, HorizontalRectangle)

RBX_ENUM_BEGIN(AdTeleportMethod)
	RBX_ENUM_VALUE(AdTeleportMethod, Undefined, 0)
	RBX_ENUM_VALUE(AdTeleportMethod, PortalForward, 1)
	RBX_ENUM_VALUE(AdTeleportMethod, InGameMenuBackButton, 2)
	RBX_ENUM_VALUE(AdTeleportMethod, UIBackButton, 3)
RBX_ENUM_END(AdTeleportMethod, Undefined, PortalForward, InGameMenuBackButton, UIBackButton)

RBX_ENUM_BEGIN(AdUIEventType)
	RBX_ENUM_VALUE(AdUIEventType, AdLabelClicked, 0)
	RBX_ENUM_VALUE(AdUIEventType, VolumeButtonClicked, 1)
	RBX_ENUM_VALUE(AdUIEventType, FullscreenButtonClicked, 2)
	RBX_ENUM_VALUE(AdUIEventType, PlayButtonClicked, 3)
	RBX_ENUM_VALUE(AdUIEventType, PauseButtonClicked, 4)
	RBX_ENUM_VALUE(AdUIEventType, CloseButtonClicked, 5)
	RBX_ENUM_VALUE(AdUIEventType, WhyThisAdClicked, 6)
	RBX_ENUM_VALUE(AdUIEventType, PlayEventTriggered, 7)
	RBX_ENUM_VALUE(AdUIEventType, PauseEventTriggered, 8)
RBX_ENUM_END(AdUIEventType, AdLabelClicked, VolumeButtonClicked, FullscreenButtonClicked, PlayButtonClicked, PauseButtonClicked, CloseButtonClicked, WhyThisAdClicked, PlayEventTriggered, PauseEventTriggered)

RBX_ENUM_BEGIN(AdUIType)
	RBX_ENUM_VALUE(AdUIType, None, 0)
	RBX_ENUM_VALUE(AdUIType, Image, 1)
	RBX_ENUM_VALUE(AdUIType, Video, 2)
RBX_ENUM_END(AdUIType, None, Image, Video)

RBX_ENUM_BEGIN(AdUnitStatus)
	RBX_ENUM_VALUE(AdUnitStatus, Inactive, 0)
	RBX_ENUM_VALUE(AdUnitStatus, Active, 1)
RBX_ENUM_END(AdUnitStatus, Inactive, Active)

RBX_ENUM_BEGIN(AdornCullingMode)
	RBX_ENUM_VALUE(AdornCullingMode, Automatic, 0)
	RBX_ENUM_VALUE(AdornCullingMode, Never, 1)
RBX_ENUM_END(AdornCullingMode, Automatic, Never)

RBX_ENUM_BEGIN(AdornShading)
	RBX_ENUM_VALUE(AdornShading, Default, 0)
	RBX_ENUM_VALUE(AdornShading, Shaded, 1)
	RBX_ENUM_VALUE(AdornShading, XRay, 2)
	RBX_ENUM_VALUE(AdornShading, XRayShaded, 3)
	RBX_ENUM_VALUE(AdornShading, AlwaysOnTop, 4)
RBX_ENUM_END(AdornShading, Default, Shaded, XRay, XRayShaded, AlwaysOnTop)

RBX_ENUM_BEGIN(AlignType)
	RBX_ENUM_VALUE(AlignType, Parallel, 0)
	RBX_ENUM_VALUE(AlignType, Perpendicular, 1)
	RBX_ENUM_VALUE(AlignType, PrimaryAxisParallel, 2)
	RBX_ENUM_VALUE(AlignType, PrimaryAxisPerpendicular, 3)
	RBX_ENUM_VALUE(AlignType, PrimaryAxisLookAt, 4)
	RBX_ENUM_VALUE(AlignType, AllAxes, 5)
RBX_ENUM_END(AlignType, Parallel, Perpendicular, PrimaryAxisParallel, PrimaryAxisPerpendicular, PrimaryAxisLookAt, AllAxes)

RBX_ENUM_BEGIN(AlphaMode)
	RBX_ENUM_VALUE(AlphaMode, Overlay, 0)
	RBX_ENUM_VALUE(AlphaMode, Transparency, 1)
	RBX_ENUM_VALUE(AlphaMode, TintMask, 2)
	RBX_ENUM_VALUE(AlphaMode, Opaque, 3)
RBX_ENUM_END(AlphaMode, Overlay, Transparency, TintMask, Opaque)

RBX_ENUM_BEGIN(AnalyticsCustomFieldKeys)
	RBX_ENUM_VALUE(AnalyticsCustomFieldKeys, CustomField01, 0)
	RBX_ENUM_VALUE(AnalyticsCustomFieldKeys, CustomField02, 1)
	RBX_ENUM_VALUE(AnalyticsCustomFieldKeys, CustomField03, 2)
RBX_ENUM_END(AnalyticsCustomFieldKeys, CustomField01, CustomField02, CustomField03)

RBX_ENUM_BEGIN(AnalyticsEconomyAction)
	RBX_ENUM_VALUE(AnalyticsEconomyAction, Default, 0)
	RBX_ENUM_VALUE(AnalyticsEconomyAction, Acquire, 1)
	RBX_ENUM_VALUE(AnalyticsEconomyAction, Spend, 2)
RBX_ENUM_END(AnalyticsEconomyAction, Default, Acquire, Spend)

RBX_ENUM_BEGIN(AnalyticsEconomyFlowType)
	RBX_ENUM_VALUE(AnalyticsEconomyFlowType, Sink, 0)
	RBX_ENUM_VALUE(AnalyticsEconomyFlowType, Source, 1)
RBX_ENUM_END(AnalyticsEconomyFlowType, Sink, Source)

RBX_ENUM_BEGIN(AnalyticsEconomyTransactionType)
	RBX_ENUM_VALUE(AnalyticsEconomyTransactionType, IAP, 0)
	RBX_ENUM_VALUE(AnalyticsEconomyTransactionType, Shop, 1)
	RBX_ENUM_VALUE(AnalyticsEconomyTransactionType, Gameplay, 2)
	RBX_ENUM_VALUE(AnalyticsEconomyTransactionType, ContextualPurchase, 3)
	RBX_ENUM_VALUE(AnalyticsEconomyTransactionType, TimedReward, 4)
	RBX_ENUM_VALUE(AnalyticsEconomyTransactionType, Onboarding, 5)
RBX_ENUM_END(AnalyticsEconomyTransactionType, IAP, Shop, Gameplay, ContextualPurchase, TimedReward, Onboarding)

RBX_ENUM_BEGIN(AnalyticsLogLevel)
	RBX_ENUM_VALUE(AnalyticsLogLevel, Trace, 0)
	RBX_ENUM_VALUE(AnalyticsLogLevel, Debug, 1)
	RBX_ENUM_VALUE(AnalyticsLogLevel, Information, 2)
	RBX_ENUM_VALUE(AnalyticsLogLevel, Warning, 3)
	RBX_ENUM_VALUE(AnalyticsLogLevel, Error, 4)
	RBX_ENUM_VALUE(AnalyticsLogLevel, Fatal, 5)
RBX_ENUM_END(AnalyticsLogLevel, Trace, Debug, Information, Warning, Error, Fatal)

RBX_ENUM_BEGIN(AnalyticsProgressionStatus)
	RBX_ENUM_VALUE(AnalyticsProgressionStatus, Default, 0)
	RBX_ENUM_VALUE(AnalyticsProgressionStatus, Begin, 1)
	RBX_ENUM_VALUE(AnalyticsProgressionStatus, Complete, 2)
	RBX_ENUM_VALUE(AnalyticsProgressionStatus, Abandon, 3)
	RBX_ENUM_VALUE(AnalyticsProgressionStatus, Fail, 4)
RBX_ENUM_END(AnalyticsProgressionStatus, Default, Begin, Complete, Abandon, Fail)

RBX_ENUM_BEGIN(AnalyticsProgressionType)
	RBX_ENUM_VALUE(AnalyticsProgressionType, Custom, 0)
	RBX_ENUM_VALUE(AnalyticsProgressionType, Start, 1)
	RBX_ENUM_VALUE(AnalyticsProgressionType, Fail, 2)
	RBX_ENUM_VALUE(AnalyticsProgressionType, Complete, 3)
RBX_ENUM_END(AnalyticsProgressionType, Custom, Start, Fail, Complete)

RBX_ENUM_BEGIN(AnimationClipFromVideoStatus)
	RBX_ENUM_VALUE(AnimationClipFromVideoStatus, Initializing, 0)
	RBX_ENUM_VALUE(AnimationClipFromVideoStatus, Pending, 1)
	RBX_ENUM_VALUE(AnimationClipFromVideoStatus, Processing, 2)
	RBX_ENUM_VALUE(AnimationClipFromVideoStatus, ErrorGeneric, 4)
	RBX_ENUM_VALUE(AnimationClipFromVideoStatus, Success, 6)
	RBX_ENUM_VALUE(AnimationClipFromVideoStatus, ErrorVideoTooLong, 7)
	RBX_ENUM_VALUE(AnimationClipFromVideoStatus, ErrorNoPersonDetected, 8)
	RBX_ENUM_VALUE(AnimationClipFromVideoStatus, ErrorVideoUnstable, 9)
	RBX_ENUM_VALUE(AnimationClipFromVideoStatus, Timeout, 10)
	RBX_ENUM_VALUE(AnimationClipFromVideoStatus, Cancelled, 11)
	RBX_ENUM_VALUE(AnimationClipFromVideoStatus, ErrorMultiplePeople, 12)
	RBX_ENUM_VALUE(AnimationClipFromVideoStatus, ErrorUploadingVideo, 2001)
RBX_ENUM_END(AnimationClipFromVideoStatus, Initializing, Pending, Processing, ErrorGeneric, Success, ErrorVideoTooLong, ErrorNoPersonDetected, ErrorVideoUnstable, Timeout, Cancelled, ErrorMultiplePeople, ErrorUploadingVideo)

RBX_ENUM_BEGIN(AnimationNodeBlend2DInputMode)
	RBX_ENUM_VALUE(AnimationNodeBlend2DInputMode, Cartesian, 0)
	RBX_ENUM_VALUE(AnimationNodeBlend2DInputMode, Polar, 1)
RBX_ENUM_END(AnimationNodeBlend2DInputMode, Cartesian, Polar)

RBX_ENUM_BEGIN(AnimationNodeInterruptible)
	RBX_ENUM_VALUE(AnimationNodeInterruptible, Always, 0)
	RBX_ENUM_VALUE(AnimationNodeInterruptible, Finished, 1)
	RBX_ENUM_VALUE(AnimationNodeInterruptible, Trigger, 2)
RBX_ENUM_END(AnimationNodeInterruptible, Always, Finished, Trigger)

RBX_ENUM_BEGIN(AnimationNodePhaseSync)
	RBX_ENUM_VALUE(AnimationNodePhaseSync, Synced, 0)
	RBX_ENUM_VALUE(AnimationNodePhaseSync, Unsynced, 1)
RBX_ENUM_END(AnimationNodePhaseSync, Synced, Unsynced)

RBX_ENUM_BEGIN(AnimationNodePlayMode)
	RBX_ENUM_VALUE(AnimationNodePlayMode, Loop, 0)
	RBX_ENUM_VALUE(AnimationNodePlayMode, PingPong, 1)
	RBX_ENUM_VALUE(AnimationNodePlayMode, OnceAndHold, 2)
	RBX_ENUM_VALUE(AnimationNodePlayMode, OnceAndReset, 3)
RBX_ENUM_END(AnimationNodePlayMode, Loop, PingPong, OnceAndHold, OnceAndReset)

RBX_ENUM_BEGIN(AnimationNodeTransitionType)
	RBX_ENUM_VALUE(AnimationNodeTransitionType, CrossFade, 0)
	RBX_ENUM_VALUE(AnimationNodeTransitionType, InertialBlend, 1)
	RBX_ENUM_VALUE(AnimationNodeTransitionType, DeadBlend, 2)
RBX_ENUM_END(AnimationNodeTransitionType, CrossFade, InertialBlend, DeadBlend)

RBX_ENUM_BEGIN(AnimationNodeType)
	RBX_ENUM_VALUE(AnimationNodeType, InvalidNode, 0)
	RBX_ENUM_VALUE(AnimationNodeType, AddNode, 1)
	RBX_ENUM_VALUE(AnimationNodeType, OverNode, 2)
	RBX_ENUM_VALUE(AnimationNodeType, Blend1DNode, 3)
	RBX_ENUM_VALUE(AnimationNodeType, Blend2DNode, 4)
	RBX_ENUM_VALUE(AnimationNodeType, ClipNode, 5)
	RBX_ENUM_VALUE(AnimationNodeType, GraphOutput, 6)
	RBX_ENUM_VALUE(AnimationNodeType, MaskNode, 7)
	RBX_ENUM_VALUE(AnimationNodeType, PrioritySelectNode, 8)
	RBX_ENUM_VALUE(AnimationNodeType, RandomSequenceNode, 9)
	RBX_ENUM_VALUE(AnimationNodeType, SelectNode, 10)
	RBX_ENUM_VALUE(AnimationNodeType, SequenceNode, 11)
	RBX_ENUM_VALUE(AnimationNodeType, SpeedNode, 12)
	RBX_ENUM_VALUE(AnimationNodeType, SubtractNode, 13)
RBX_ENUM_END(AnimationNodeType, InvalidNode, AddNode, OverNode, Blend1DNode, Blend2DNode, ClipNode, GraphOutput, MaskNode, PrioritySelectNode, RandomSequenceNode, SelectNode, SequenceNode, SpeedNode, SubtractNode)

RBX_ENUM_BEGIN(AnimationNodeWaitFor)
	RBX_ENUM_VALUE(AnimationNodeWaitFor, Finished, 0)
	RBX_ENUM_VALUE(AnimationNodeWaitFor, Trigger, 1)
RBX_ENUM_END(AnimationNodeWaitFor, Finished, Trigger)

RBX_ENUM_BEGIN(AnimationPriority)
	RBX_ENUM_VALUE(AnimationPriority, Idle, 0)
	RBX_ENUM_VALUE(AnimationPriority, Movement, 1)
	RBX_ENUM_VALUE(AnimationPriority, Action, 2)
	RBX_ENUM_VALUE(AnimationPriority, Action2, 3)
	RBX_ENUM_VALUE(AnimationPriority, Action3, 4)
	RBX_ENUM_VALUE(AnimationPriority, Action4, 5)
	RBX_ENUM_VALUE(AnimationPriority, Core, 1000)
RBX_ENUM_END(AnimationPriority, Idle, Movement, Action, Action2, Action3, Action4, Core)

RBX_ENUM_BEGIN(AnimatorRetargetingMode)
	RBX_ENUM_VALUE(AnimatorRetargetingMode, Default, 0)
	RBX_ENUM_VALUE(AnimatorRetargetingMode, Disabled, 1)
	RBX_ENUM_VALUE(AnimatorRetargetingMode, Enabled, 2)
RBX_ENUM_END(AnimatorRetargetingMode, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(AnnotationChannelContentPreference)
	RBX_ENUM_VALUE(AnnotationChannelContentPreference, None, 0)
	RBX_ENUM_VALUE(AnnotationChannelContentPreference, All, 1)
	RBX_ENUM_VALUE(AnnotationChannelContentPreference, Unknown, 2)
RBX_ENUM_END(AnnotationChannelContentPreference, None, All, Unknown)

RBX_ENUM_BEGIN(AnnotationEditingMode)
	RBX_ENUM_VALUE(AnnotationEditingMode, None, 0)
	RBX_ENUM_VALUE(AnnotationEditingMode, PlacingNew, 1)
	RBX_ENUM_VALUE(AnnotationEditingMode, WritingNew, 2)
RBX_ENUM_END(AnnotationEditingMode, None, PlacingNew, WritingNew)

RBX_ENUM_BEGIN(AnnotationPlaceContentPreference)
	RBX_ENUM_VALUE(AnnotationPlaceContentPreference, None, 0)
	RBX_ENUM_VALUE(AnnotationPlaceContentPreference, All, 1)
	RBX_ENUM_VALUE(AnnotationPlaceContentPreference, MentionsAndReplies, 2)
	RBX_ENUM_VALUE(AnnotationPlaceContentPreference, Unknown, 3)
RBX_ENUM_END(AnnotationPlaceContentPreference, None, All, MentionsAndReplies, Unknown)

RBX_ENUM_BEGIN(AnnotationRequestStatus)
	RBX_ENUM_VALUE(AnnotationRequestStatus, Success, 0)
	RBX_ENUM_VALUE(AnnotationRequestStatus, Loading, 1)
	RBX_ENUM_VALUE(AnnotationRequestStatus, ErrorInternalFailure, 2)
	RBX_ENUM_VALUE(AnnotationRequestStatus, ErrorNotFound, 3)
	RBX_ENUM_VALUE(AnnotationRequestStatus, ErrorModerated, 4)
RBX_ENUM_END(AnnotationRequestStatus, Success, Loading, ErrorInternalFailure, ErrorNotFound, ErrorModerated)

RBX_ENUM_BEGIN(AnnotationRequestType)
	RBX_ENUM_VALUE(AnnotationRequestType, Unknown, 0)
	RBX_ENUM_VALUE(AnnotationRequestType, Create, 1)
	RBX_ENUM_VALUE(AnnotationRequestType, Resolve, 2)
	RBX_ENUM_VALUE(AnnotationRequestType, Delete, 3)
	RBX_ENUM_VALUE(AnnotationRequestType, Edit, 4)
RBX_ENUM_END(AnnotationRequestType, Unknown, Create, Resolve, Delete, Edit)

RBX_ENUM_BEGIN(AntiAliasing)
	RBX_ENUM_VALUE(AntiAliasing, Disabled, 0)
	RBX_ENUM_VALUE(AntiAliasing, Enabled, 1)
RBX_ENUM_END(AntiAliasing, Disabled, Enabled)

RBX_ENUM_BEGIN(AppLifecycleManagerState)
	RBX_ENUM_VALUE(AppLifecycleManagerState, Detached, 0)
	RBX_ENUM_VALUE(AppLifecycleManagerState, Active, 1)
	RBX_ENUM_VALUE(AppLifecycleManagerState, Inactive, 2)
	RBX_ENUM_VALUE(AppLifecycleManagerState, Hidden, 3)
RBX_ENUM_END(AppLifecycleManagerState, Detached, Active, Inactive, Hidden)

RBX_ENUM_BEGIN(AppShellActionType)
	RBX_ENUM_VALUE(AppShellActionType, None, 0)
	RBX_ENUM_VALUE(AppShellActionType, OpenApp, 1)
	RBX_ENUM_VALUE(AppShellActionType, TapChatTab, 2)
	RBX_ENUM_VALUE(AppShellActionType, TapConversationEntry, 3)
	RBX_ENUM_VALUE(AppShellActionType, TapAvatarTab, 4)
	RBX_ENUM_VALUE(AppShellActionType, ReadConversation, 5)
	RBX_ENUM_VALUE(AppShellActionType, TapGamePageTab, 6)
	RBX_ENUM_VALUE(AppShellActionType, TapHomePageTab, 7)
	RBX_ENUM_VALUE(AppShellActionType, GamePageLoaded, 8)
	RBX_ENUM_VALUE(AppShellActionType, HomePageLoaded, 9)
	RBX_ENUM_VALUE(AppShellActionType, AvatarEditorPageLoaded, 10)
	RBX_ENUM_VALUE(AppShellActionType, HomePageInteractive, 11)
RBX_ENUM_END(AppShellActionType, None, OpenApp, TapChatTab, TapConversationEntry, TapAvatarTab, ReadConversation, TapGamePageTab, TapHomePageTab, GamePageLoaded, HomePageLoaded, AvatarEditorPageLoaded, HomePageInteractive)

RBX_ENUM_BEGIN(AppShellFeature)
	RBX_ENUM_VALUE(AppShellFeature, None, 0)
	RBX_ENUM_VALUE(AppShellFeature, Chat, 1)
	RBX_ENUM_VALUE(AppShellFeature, AvatarEditor, 2)
	RBX_ENUM_VALUE(AppShellFeature, GamePage, 3)
	RBX_ENUM_VALUE(AppShellFeature, HomePage, 4)
	RBX_ENUM_VALUE(AppShellFeature, More, 5)
	RBX_ENUM_VALUE(AppShellFeature, Landing, 6)
RBX_ENUM_END(AppShellFeature, None, Chat, AvatarEditor, GamePage, HomePage, More, Landing)

RBX_ENUM_BEGIN(AppUpdateStatus)
	RBX_ENUM_VALUE(AppUpdateStatus, Unknown, 0)
	RBX_ENUM_VALUE(AppUpdateStatus, NotSupported, 1)
	RBX_ENUM_VALUE(AppUpdateStatus, Failed, 2)
	RBX_ENUM_VALUE(AppUpdateStatus, NotAvailable, 3)
	RBX_ENUM_VALUE(AppUpdateStatus, Available, 4)
	RBX_ENUM_VALUE(AppUpdateStatus, AvailableBoundChannel, 5)
	RBX_ENUM_VALUE(AppUpdateStatus, AvailableBetaProgram, 6)
RBX_ENUM_END(AppUpdateStatus, Unknown, NotSupported, Failed, NotAvailable, Available, AvailableBoundChannel, AvailableBetaProgram)

RBX_ENUM_BEGIN(ApplyStrokeMode)
	RBX_ENUM_VALUE(ApplyStrokeMode, Contextual, 0)
	RBX_ENUM_VALUE(ApplyStrokeMode, Border, 1)
RBX_ENUM_END(ApplyStrokeMode, Contextual, Border)

RBX_ENUM_BEGIN(AspectType)
	RBX_ENUM_VALUE(AspectType, FitWithinMaxSize, 0)
	RBX_ENUM_VALUE(AspectType, ScaleWithParentSize, 1)
RBX_ENUM_END(AspectType, FitWithinMaxSize, ScaleWithParentSize)

RBX_ENUM_BEGIN(AssetCreatorType)
	RBX_ENUM_VALUE(AssetCreatorType, User, 0)
	RBX_ENUM_VALUE(AssetCreatorType, Group, 1)
RBX_ENUM_END(AssetCreatorType, User, Group)

RBX_ENUM_BEGIN(AssetFetchStatus)
	RBX_ENUM_VALUE(AssetFetchStatus, Success, 0)
	RBX_ENUM_VALUE(AssetFetchStatus, Failure, 1)
	RBX_ENUM_VALUE(AssetFetchStatus, None, 2)
	RBX_ENUM_VALUE(AssetFetchStatus, Loading, 3)
	RBX_ENUM_VALUE(AssetFetchStatus, TimedOut, 4)
RBX_ENUM_END(AssetFetchStatus, Success, Failure, None, Loading, TimedOut)

RBX_ENUM_BEGIN(AssetType)
	RBX_ENUM_VALUE(AssetType, Image, 1)
	RBX_ENUM_VALUE(AssetType, TShirt, 2)
	RBX_ENUM_VALUE(AssetType, Audio, 3)
	RBX_ENUM_VALUE(AssetType, Mesh, 4)
	RBX_ENUM_VALUE(AssetType, Lua, 5)
	RBX_ENUM_VALUE(AssetType, Hat, 8)
	RBX_ENUM_VALUE(AssetType, Place, 9)
	RBX_ENUM_VALUE(AssetType, Model, 10)
	RBX_ENUM_VALUE(AssetType, Shirt, 11)
	RBX_ENUM_VALUE(AssetType, Pants, 12)
	RBX_ENUM_VALUE(AssetType, Decal, 13)
	RBX_ENUM_VALUE(AssetType, Head, 17)
	RBX_ENUM_VALUE(AssetType, Face, 18)
	RBX_ENUM_VALUE(AssetType, Gear, 19)
	RBX_ENUM_VALUE(AssetType, Badge, 21)
	RBX_ENUM_VALUE(AssetType, Animation, 24)
	RBX_ENUM_VALUE(AssetType, Torso, 27)
	RBX_ENUM_VALUE(AssetType, RightArm, 28)
	RBX_ENUM_VALUE(AssetType, LeftArm, 29)
	RBX_ENUM_VALUE(AssetType, LeftLeg, 30)
	RBX_ENUM_VALUE(AssetType, RightLeg, 31)
	RBX_ENUM_VALUE(AssetType, Package, 32)
	RBX_ENUM_VALUE(AssetType, GamePass, 34)
	RBX_ENUM_VALUE(AssetType, Plugin, 38)
	RBX_ENUM_VALUE(AssetType, MeshPart, 40)
	RBX_ENUM_VALUE(AssetType, HairAccessory, 41)
	RBX_ENUM_VALUE(AssetType, FaceAccessory, 42)
	RBX_ENUM_VALUE(AssetType, NeckAccessory, 43)
	RBX_ENUM_VALUE(AssetType, ShoulderAccessory, 44)
	RBX_ENUM_VALUE(AssetType, FrontAccessory, 45)
	RBX_ENUM_VALUE(AssetType, BackAccessory, 46)
	RBX_ENUM_VALUE(AssetType, WaistAccessory, 47)
	RBX_ENUM_VALUE(AssetType, ClimbAnimation, 48)
	RBX_ENUM_VALUE(AssetType, DeathAnimation, 49)
	RBX_ENUM_VALUE(AssetType, FallAnimation, 50)
	RBX_ENUM_VALUE(AssetType, IdleAnimation, 51)
	RBX_ENUM_VALUE(AssetType, JumpAnimation, 52)
	RBX_ENUM_VALUE(AssetType, RunAnimation, 53)
	RBX_ENUM_VALUE(AssetType, SwimAnimation, 54)
	RBX_ENUM_VALUE(AssetType, WalkAnimation, 55)
	RBX_ENUM_VALUE(AssetType, PoseAnimation, 56)
	RBX_ENUM_VALUE(AssetType, EarAccessory, 57)
	RBX_ENUM_VALUE(AssetType, EyeAccessory, 58)
	RBX_ENUM_VALUE(AssetType, EmoteAnimation, 61)
	RBX_ENUM_VALUE(AssetType, Video, 62)
	RBX_ENUM_VALUE(AssetType, TShirtAccessory, 64)
	RBX_ENUM_VALUE(AssetType, ShirtAccessory, 65)
	RBX_ENUM_VALUE(AssetType, PantsAccessory, 66)
	RBX_ENUM_VALUE(AssetType, JacketAccessory, 67)
	RBX_ENUM_VALUE(AssetType, SweaterAccessory, 68)
	RBX_ENUM_VALUE(AssetType, ShortsAccessory, 69)
	RBX_ENUM_VALUE(AssetType, LeftShoeAccessory, 70)
	RBX_ENUM_VALUE(AssetType, RightShoeAccessory, 71)
	RBX_ENUM_VALUE(AssetType, DressSkirtAccessory, 72)
	RBX_ENUM_VALUE(AssetType, FontFamily, 73)
	RBX_ENUM_VALUE(AssetType, EyebrowAccessory, 76)
	RBX_ENUM_VALUE(AssetType, EyelashAccessory, 77)
	RBX_ENUM_VALUE(AssetType, MoodAnimation, 78)
	RBX_ENUM_VALUE(AssetType, DynamicHead, 79)
	RBX_ENUM_VALUE(AssetType, FaceMakeup, 88)
	RBX_ENUM_VALUE(AssetType, LipMakeup, 89)
	RBX_ENUM_VALUE(AssetType, EyeMakeup, 90)
	RBX_ENUM_VALUE(AssetType, VoxelFragment, 91)
RBX_ENUM_END(AssetType, Image, TShirt, Audio, Mesh, Lua, Hat, Place, Model, Shirt, Pants, Decal, Head, Face, Gear, Badge, Animation, Torso, RightArm, LeftArm, LeftLeg, RightLeg, Package, GamePass, Plugin, MeshPart, HairAccessory, FaceAccessory, NeckAccessory, ShoulderAccessory, FrontAccessory, BackAccessory, WaistAccessory, ClimbAnimation, DeathAnimation, FallAnimation, IdleAnimation, JumpAnimation, RunAnimation, SwimAnimation, WalkAnimation, PoseAnimation, EarAccessory, EyeAccessory, EmoteAnimation, Video, TShirtAccessory, ShirtAccessory, PantsAccessory, JacketAccessory, SweaterAccessory, ShortsAccessory, LeftShoeAccessory, RightShoeAccessory, DressSkirtAccessory, FontFamily, EyebrowAccessory, EyelashAccessory, MoodAnimation, DynamicHead, FaceMakeup, LipMakeup, EyeMakeup, VoxelFragment)

RBX_ENUM_BEGIN(AssetTypeVerification)
	RBX_ENUM_VALUE(AssetTypeVerification, Default, 1)
	RBX_ENUM_VALUE(AssetTypeVerification, ClientOnly, 2)
	RBX_ENUM_VALUE(AssetTypeVerification, Always, 3)
RBX_ENUM_END(AssetTypeVerification, Default, ClientOnly, Always)

RBX_ENUM_BEGIN(AudioApiRollout)
	RBX_ENUM_VALUE(AudioApiRollout, Disabled, 0)
	RBX_ENUM_VALUE(AudioApiRollout, Automatic, 1)
	RBX_ENUM_VALUE(AudioApiRollout, Enabled, 2)
RBX_ENUM_END(AudioApiRollout, Disabled, Automatic, Enabled)

RBX_ENUM_BEGIN(AudioChannelLayout)
	RBX_ENUM_VALUE(AudioChannelLayout, Mono, 0)
	RBX_ENUM_VALUE(AudioChannelLayout, Stereo, 1)
	RBX_ENUM_VALUE(AudioChannelLayout, Quad, 2)
	RBX_ENUM_VALUE(AudioChannelLayout, Surround_5, 3)
	RBX_ENUM_VALUE(AudioChannelLayout, Surround_5_1, 4)
	RBX_ENUM_VALUE(AudioChannelLayout, Surround_7_1, 5)
	RBX_ENUM_VALUE(AudioChannelLayout, Surround_7_1_4, 6)
RBX_ENUM_END(AudioChannelLayout, Mono, Stereo, Quad, Surround_5, Surround_5_1, Surround_7_1, Surround_7_1_4)

RBX_ENUM_BEGIN(AudioFilterType)
	RBX_ENUM_VALUE(AudioFilterType, Peak, 0)
	RBX_ENUM_VALUE(AudioFilterType, LowShelf, 1)
	RBX_ENUM_VALUE(AudioFilterType, HighShelf, 2)
	RBX_ENUM_VALUE(AudioFilterType, Lowpass12dB, 3)
	RBX_ENUM_VALUE(AudioFilterType, Lowpass24dB, 4)
	RBX_ENUM_VALUE(AudioFilterType, Lowpass48dB, 5)
	RBX_ENUM_VALUE(AudioFilterType, Highpass12dB, 6)
	RBX_ENUM_VALUE(AudioFilterType, Highpass24dB, 7)
	RBX_ENUM_VALUE(AudioFilterType, Highpass48dB, 8)
	RBX_ENUM_VALUE(AudioFilterType, Bandpass, 9)
	RBX_ENUM_VALUE(AudioFilterType, Notch, 10)
	RBX_ENUM_VALUE(AudioFilterType, Lowpass6dB, 11)
RBX_ENUM_END(AudioFilterType, Peak, LowShelf, HighShelf, Lowpass12dB, Lowpass24dB, Lowpass48dB, Highpass12dB, Highpass24dB, Highpass48dB, Bandpass, Notch, Lowpass6dB)

RBX_ENUM_BEGIN(AudioSimulationFidelity)
	RBX_ENUM_VALUE(AudioSimulationFidelity, None, 0)
	RBX_ENUM_VALUE(AudioSimulationFidelity, Automatic, 1)
RBX_ENUM_END(AudioSimulationFidelity, None, Automatic)

RBX_ENUM_BEGIN(AudioSubType)
	RBX_ENUM_VALUE(AudioSubType, Music, 1)
	RBX_ENUM_VALUE(AudioSubType, SoundEffect, 2)
RBX_ENUM_END(AudioSubType, Music, SoundEffect)

RBX_ENUM_BEGIN(AudioWindowSize)
	RBX_ENUM_VALUE(AudioWindowSize, Small, 0)
	RBX_ENUM_VALUE(AudioWindowSize, Medium, 1)
	RBX_ENUM_VALUE(AudioWindowSize, Large, 2)
RBX_ENUM_END(AudioWindowSize, Small, Medium, Large)

RBX_ENUM_BEGIN(AuthorityMode)
	RBX_ENUM_VALUE(AuthorityMode, Server, 0)
	RBX_ENUM_VALUE(AuthorityMode, Automatic, 1)
RBX_ENUM_END(AuthorityMode, Server, Automatic)

RBX_ENUM_BEGIN(AutoIndentRule)
	RBX_ENUM_VALUE(AutoIndentRule, Off, 0)
	RBX_ENUM_VALUE(AutoIndentRule, Absolute, 1)
	RBX_ENUM_VALUE(AutoIndentRule, Relative, 2)
RBX_ENUM_END(AutoIndentRule, Off, Absolute, Relative)

RBX_ENUM_BEGIN(AutomaticSize)
	RBX_ENUM_VALUE(AutomaticSize, None, 0)
	RBX_ENUM_VALUE(AutomaticSize, X, 1)
	RBX_ENUM_VALUE(AutomaticSize, Y, 2)
	RBX_ENUM_VALUE(AutomaticSize, XY, 3)
RBX_ENUM_END(AutomaticSize, None, X, Y, XY)

RBX_ENUM_BEGIN(AvatarAssetType)
	RBX_ENUM_VALUE(AvatarAssetType, TShirt, 2)
	RBX_ENUM_VALUE(AvatarAssetType, Hat, 8)
	RBX_ENUM_VALUE(AvatarAssetType, Shirt, 11)
	RBX_ENUM_VALUE(AvatarAssetType, Pants, 12)
	RBX_ENUM_VALUE(AvatarAssetType, Head, 17)
	RBX_ENUM_VALUE(AvatarAssetType, Face, 18)
	RBX_ENUM_VALUE(AvatarAssetType, Gear, 19)
	RBX_ENUM_VALUE(AvatarAssetType, Torso, 27)
	RBX_ENUM_VALUE(AvatarAssetType, RightArm, 28)
	RBX_ENUM_VALUE(AvatarAssetType, LeftArm, 29)
	RBX_ENUM_VALUE(AvatarAssetType, LeftLeg, 30)
	RBX_ENUM_VALUE(AvatarAssetType, RightLeg, 31)
	RBX_ENUM_VALUE(AvatarAssetType, HairAccessory, 41)
	RBX_ENUM_VALUE(AvatarAssetType, FaceAccessory, 42)
	RBX_ENUM_VALUE(AvatarAssetType, NeckAccessory, 43)
	RBX_ENUM_VALUE(AvatarAssetType, ShoulderAccessory, 44)
	RBX_ENUM_VALUE(AvatarAssetType, FrontAccessory, 45)
	RBX_ENUM_VALUE(AvatarAssetType, BackAccessory, 46)
	RBX_ENUM_VALUE(AvatarAssetType, WaistAccessory, 47)
	RBX_ENUM_VALUE(AvatarAssetType, ClimbAnimation, 48)
	RBX_ENUM_VALUE(AvatarAssetType, FallAnimation, 50)
	RBX_ENUM_VALUE(AvatarAssetType, IdleAnimation, 51)
	RBX_ENUM_VALUE(AvatarAssetType, JumpAnimation, 52)
	RBX_ENUM_VALUE(AvatarAssetType, RunAnimation, 53)
	RBX_ENUM_VALUE(AvatarAssetType, SwimAnimation, 54)
	RBX_ENUM_VALUE(AvatarAssetType, WalkAnimation, 55)
	RBX_ENUM_VALUE(AvatarAssetType, EmoteAnimation, 61)
	RBX_ENUM_VALUE(AvatarAssetType, TShirtAccessory, 64)
	RBX_ENUM_VALUE(AvatarAssetType, ShirtAccessory, 65)
	RBX_ENUM_VALUE(AvatarAssetType, PantsAccessory, 66)
	RBX_ENUM_VALUE(AvatarAssetType, JacketAccessory, 67)
	RBX_ENUM_VALUE(AvatarAssetType, SweaterAccessory, 68)
	RBX_ENUM_VALUE(AvatarAssetType, ShortsAccessory, 69)
	RBX_ENUM_VALUE(AvatarAssetType, LeftShoeAccessory, 70)
	RBX_ENUM_VALUE(AvatarAssetType, RightShoeAccessory, 71)
	RBX_ENUM_VALUE(AvatarAssetType, DressSkirtAccessory, 72)
	RBX_ENUM_VALUE(AvatarAssetType, EyebrowAccessory, 76)
	RBX_ENUM_VALUE(AvatarAssetType, EyelashAccessory, 77)
	RBX_ENUM_VALUE(AvatarAssetType, MoodAnimation, 78)
	RBX_ENUM_VALUE(AvatarAssetType, DynamicHead, 79)
	RBX_ENUM_VALUE(AvatarAssetType, FaceMakeup, 88)
	RBX_ENUM_VALUE(AvatarAssetType, LipMakeup, 89)
	RBX_ENUM_VALUE(AvatarAssetType, EyeMakeup, 90)
RBX_ENUM_END(AvatarAssetType, TShirt, Hat, Shirt, Pants, Head, Face, Gear, Torso, RightArm, LeftArm, LeftLeg, RightLeg, HairAccessory, FaceAccessory, NeckAccessory, ShoulderAccessory, FrontAccessory, BackAccessory, WaistAccessory, ClimbAnimation, FallAnimation, IdleAnimation, JumpAnimation, RunAnimation, SwimAnimation, WalkAnimation, EmoteAnimation, TShirtAccessory, ShirtAccessory, PantsAccessory, JacketAccessory, SweaterAccessory, ShortsAccessory, LeftShoeAccessory, RightShoeAccessory, DressSkirtAccessory, EyebrowAccessory, EyelashAccessory, MoodAnimation, DynamicHead, FaceMakeup, LipMakeup, EyeMakeup)

RBX_ENUM_BEGIN(AvatarChatServiceFeature)
	RBX_ENUM_VALUE(AvatarChatServiceFeature, None, 0)
	RBX_ENUM_VALUE(AvatarChatServiceFeature, UniverseAudio, 1)
	RBX_ENUM_VALUE(AvatarChatServiceFeature, UniverseVideo, 2)
	RBX_ENUM_VALUE(AvatarChatServiceFeature, PlaceAudio, 4)
	RBX_ENUM_VALUE(AvatarChatServiceFeature, PlaceVideo, 8)
	RBX_ENUM_VALUE(AvatarChatServiceFeature, UserAudioEligible, 16)
	RBX_ENUM_VALUE(AvatarChatServiceFeature, UserAudio, 32)
	RBX_ENUM_VALUE(AvatarChatServiceFeature, UserVideoEligible, 64)
	RBX_ENUM_VALUE(AvatarChatServiceFeature, UserVideo, 128)
	RBX_ENUM_VALUE(AvatarChatServiceFeature, UserBanned, 256)
	RBX_ENUM_VALUE(AvatarChatServiceFeature, UserVerifiedForVoice, 512)
RBX_ENUM_END(AvatarChatServiceFeature, None, UniverseAudio, UniverseVideo, PlaceAudio, PlaceVideo, UserAudioEligible, UserAudio, UserVideoEligible, UserVideo, UserBanned, UserVerifiedForVoice)

RBX_ENUM_BEGIN(AvatarContextMenuOption)
	RBX_ENUM_VALUE(AvatarContextMenuOption, Friend, 0)
	RBX_ENUM_VALUE(AvatarContextMenuOption, Chat, 1)
	RBX_ENUM_VALUE(AvatarContextMenuOption, Emote, 2)
	RBX_ENUM_VALUE(AvatarContextMenuOption, InspectMenu, 3)
RBX_ENUM_END(AvatarContextMenuOption, Friend, Chat, Emote, InspectMenu)

RBX_ENUM_BEGIN(AvatarGenerationError)
	RBX_ENUM_VALUE(AvatarGenerationError, None, 0)
	RBX_ENUM_VALUE(AvatarGenerationError, Unknown, 1)
	RBX_ENUM_VALUE(AvatarGenerationError, DownloadFailed, 2)
	RBX_ENUM_VALUE(AvatarGenerationError, Canceled, 3)
	RBX_ENUM_VALUE(AvatarGenerationError, Offensive, 4)
	RBX_ENUM_VALUE(AvatarGenerationError, Timeout, 5)
	RBX_ENUM_VALUE(AvatarGenerationError, JobNotFound, 6)
RBX_ENUM_END(AvatarGenerationError, None, Unknown, DownloadFailed, Canceled, Offensive, Timeout, JobNotFound)

RBX_ENUM_BEGIN(AvatarItemType)
	RBX_ENUM_VALUE(AvatarItemType, Asset, 1)
	RBX_ENUM_VALUE(AvatarItemType, Bundle, 2)
RBX_ENUM_END(AvatarItemType, Asset, Bundle)

RBX_ENUM_BEGIN(AvatarPromptResult)
	RBX_ENUM_VALUE(AvatarPromptResult, Success, 1)
	RBX_ENUM_VALUE(AvatarPromptResult, PermissionDenied, 2)
	RBX_ENUM_VALUE(AvatarPromptResult, Failed, 3)
RBX_ENUM_END(AvatarPromptResult, Success, PermissionDenied, Failed)

RBX_ENUM_BEGIN(AvatarSettingsAccessoryLimitMethod)
	RBX_ENUM_VALUE(AvatarSettingsAccessoryLimitMethod, Scale, 0)
	RBX_ENUM_VALUE(AvatarSettingsAccessoryLimitMethod, Remove, 1)
	RBX_ENUM_VALUE(AvatarSettingsAccessoryLimitMethod, PreviewScale, 2)
	RBX_ENUM_VALUE(AvatarSettingsAccessoryLimitMethod, PreviewRemove, 3)
RBX_ENUM_END(AvatarSettingsAccessoryLimitMethod, Scale, Remove, PreviewScale, PreviewRemove)

RBX_ENUM_BEGIN(AvatarSettingsAccessoryMode)
	RBX_ENUM_VALUE(AvatarSettingsAccessoryMode, PlayerChoice, 0)
	RBX_ENUM_VALUE(AvatarSettingsAccessoryMode, CustomLimit, 1)
RBX_ENUM_END(AvatarSettingsAccessoryMode, PlayerChoice, CustomLimit)

RBX_ENUM_BEGIN(AvatarSettingsAnimationClipsMode)
	RBX_ENUM_VALUE(AvatarSettingsAnimationClipsMode, PlayerChoice, 0)
	RBX_ENUM_VALUE(AvatarSettingsAnimationClipsMode, CustomClips, 1)
RBX_ENUM_END(AvatarSettingsAnimationClipsMode, PlayerChoice, CustomClips)

RBX_ENUM_BEGIN(AvatarSettingsAnimationPacksMode)
	RBX_ENUM_VALUE(AvatarSettingsAnimationPacksMode, PlayerChoice, 0)
	RBX_ENUM_VALUE(AvatarSettingsAnimationPacksMode, StandardR15, 1)
	RBX_ENUM_VALUE(AvatarSettingsAnimationPacksMode, StandardR6, 2)
RBX_ENUM_END(AvatarSettingsAnimationPacksMode, PlayerChoice, StandardR15, StandardR6)

RBX_ENUM_BEGIN(AvatarSettingsAppearanceMode)
	RBX_ENUM_VALUE(AvatarSettingsAppearanceMode, PlayerChoice, 0)
	RBX_ENUM_VALUE(AvatarSettingsAppearanceMode, CustomParts, 1)
	RBX_ENUM_VALUE(AvatarSettingsAppearanceMode, CustomBody, 2)
RBX_ENUM_END(AvatarSettingsAppearanceMode, PlayerChoice, CustomParts, CustomBody)

RBX_ENUM_BEGIN(AvatarSettingsBuildMode)
	RBX_ENUM_VALUE(AvatarSettingsBuildMode, PlayerChoice, 0)
	RBX_ENUM_VALUE(AvatarSettingsBuildMode, CustomBuild, 1)
RBX_ENUM_END(AvatarSettingsBuildMode, PlayerChoice, CustomBuild)

RBX_ENUM_BEGIN(AvatarSettingsCharacterControllerMode)
	RBX_ENUM_VALUE(AvatarSettingsCharacterControllerMode, LegacyHumanoid, 0)
	RBX_ENUM_VALUE(AvatarSettingsCharacterControllerMode, LuaCharacterController, 1)
RBX_ENUM_END(AvatarSettingsCharacterControllerMode, LegacyHumanoid, LuaCharacterController)

RBX_ENUM_BEGIN(AvatarSettingsClothingMode)
	RBX_ENUM_VALUE(AvatarSettingsClothingMode, PlayerChoice, 0)
	RBX_ENUM_VALUE(AvatarSettingsClothingMode, CustomLimit, 1)
RBX_ENUM_END(AvatarSettingsClothingMode, PlayerChoice, CustomLimit)

RBX_ENUM_BEGIN(AvatarSettingsCollisionMode)
	RBX_ENUM_VALUE(AvatarSettingsCollisionMode, Default, 0)
	RBX_ENUM_VALUE(AvatarSettingsCollisionMode, SingleCollider, 1)
	RBX_ENUM_VALUE(AvatarSettingsCollisionMode, Legacy, 2)
RBX_ENUM_END(AvatarSettingsCollisionMode, Default, SingleCollider, Legacy)

RBX_ENUM_BEGIN(AvatarSettingsCustomAccessoryMode)
	RBX_ENUM_VALUE(AvatarSettingsCustomAccessoryMode, PlayerChoice, 0)
	RBX_ENUM_VALUE(AvatarSettingsCustomAccessoryMode, CustomAccessories, 1)
RBX_ENUM_END(AvatarSettingsCustomAccessoryMode, PlayerChoice, CustomAccessories)

RBX_ENUM_BEGIN(AvatarSettingsCustomBodyType)
	RBX_ENUM_VALUE(AvatarSettingsCustomBodyType, AvatarReference, 0)
	RBX_ENUM_VALUE(AvatarSettingsCustomBodyType, BundleId, 1)
RBX_ENUM_END(AvatarSettingsCustomBodyType, AvatarReference, BundleId)

RBX_ENUM_BEGIN(AvatarSettingsCustomClothingMode)
	RBX_ENUM_VALUE(AvatarSettingsCustomClothingMode, PlayerChoice, 0)
	RBX_ENUM_VALUE(AvatarSettingsCustomClothingMode, CustomClothing, 1)
RBX_ENUM_END(AvatarSettingsCustomClothingMode, PlayerChoice, CustomClothing)

RBX_ENUM_BEGIN(AvatarSettingsHitAndTouchDetectionMode)
	RBX_ENUM_VALUE(AvatarSettingsHitAndTouchDetectionMode, UseParts, 0)
	RBX_ENUM_VALUE(AvatarSettingsHitAndTouchDetectionMode, UseCollider, 1)
RBX_ENUM_END(AvatarSettingsHitAndTouchDetectionMode, UseParts, UseCollider)

RBX_ENUM_BEGIN(AvatarSettingsJumpMode)
	RBX_ENUM_VALUE(AvatarSettingsJumpMode, JumpHeight, 0)
	RBX_ENUM_VALUE(AvatarSettingsJumpMode, JumpPower, 1)
RBX_ENUM_END(AvatarSettingsJumpMode, JumpHeight, JumpPower)

RBX_ENUM_BEGIN(AvatarSettingsLegacyCollisionMode)
	RBX_ENUM_VALUE(AvatarSettingsLegacyCollisionMode, R6Colliders, 0)
	RBX_ENUM_VALUE(AvatarSettingsLegacyCollisionMode, InnerBoxColliders, 1)
RBX_ENUM_END(AvatarSettingsLegacyCollisionMode, R6Colliders, InnerBoxColliders)

RBX_ENUM_BEGIN(AvatarSettingsScaleMode)
	RBX_ENUM_VALUE(AvatarSettingsScaleMode, PlayerChoice, 0)
	RBX_ENUM_VALUE(AvatarSettingsScaleMode, CustomScale, 1)
RBX_ENUM_END(AvatarSettingsScaleMode, PlayerChoice, CustomScale)

RBX_ENUM_BEGIN(AvatarThumbnailCustomizationType)
	RBX_ENUM_VALUE(AvatarThumbnailCustomizationType, Closeup, 1)
	RBX_ENUM_VALUE(AvatarThumbnailCustomizationType, FullBody, 2)
RBX_ENUM_END(AvatarThumbnailCustomizationType, Closeup, FullBody)

RBX_ENUM_BEGIN(AvatarUnificationMode)
	RBX_ENUM_VALUE(AvatarUnificationMode, Default, 0)
	RBX_ENUM_VALUE(AvatarUnificationMode, Disabled, 1)
	RBX_ENUM_VALUE(AvatarUnificationMode, Enabled, 2)
RBX_ENUM_END(AvatarUnificationMode, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(Axis)
	RBX_ENUM_VALUE(Axis, X, 0)
	RBX_ENUM_VALUE(Axis, Y, 1)
	RBX_ENUM_VALUE(Axis, Z, 2)
RBX_ENUM_END(Axis, X, Y, Z)

RBX_ENUM_BEGIN(BenefitType)
	RBX_ENUM_VALUE(BenefitType, DeveloperProduct, 0)
	RBX_ENUM_VALUE(BenefitType, AvatarAsset, 1)
	RBX_ENUM_VALUE(BenefitType, AvatarBundle, 2)
RBX_ENUM_END(BenefitType, DeveloperProduct, AvatarAsset, AvatarBundle)

RBX_ENUM_BEGIN(BinType)
	RBX_ENUM_VALUE(BinType, Script, 0)
	RBX_ENUM_VALUE(BinType, GameTool, 1)
	RBX_ENUM_VALUE(BinType, Grab, 2)
	RBX_ENUM_VALUE(BinType, Clone, 3)
	RBX_ENUM_VALUE(BinType, Hammer, 4)
RBX_ENUM_END(BinType, Script, GameTool, Grab, Clone, Hammer)

RBX_ENUM_BEGIN(BodyPart)
	RBX_ENUM_VALUE(BodyPart, Head, 0)
	RBX_ENUM_VALUE(BodyPart, Torso, 1)
	RBX_ENUM_VALUE(BodyPart, LeftArm, 2)
	RBX_ENUM_VALUE(BodyPart, RightArm, 3)
	RBX_ENUM_VALUE(BodyPart, LeftLeg, 4)
	RBX_ENUM_VALUE(BodyPart, RightLeg, 5)
RBX_ENUM_END(BodyPart, Head, Torso, LeftArm, RightArm, LeftLeg, RightLeg)

RBX_ENUM_BEGIN(BodyPartR15)
	RBX_ENUM_VALUE(BodyPartR15, Head, 0)
	RBX_ENUM_VALUE(BodyPartR15, UpperTorso, 1)
	RBX_ENUM_VALUE(BodyPartR15, LowerTorso, 2)
	RBX_ENUM_VALUE(BodyPartR15, LeftFoot, 3)
	RBX_ENUM_VALUE(BodyPartR15, LeftLowerLeg, 4)
	RBX_ENUM_VALUE(BodyPartR15, LeftUpperLeg, 5)
	RBX_ENUM_VALUE(BodyPartR15, RightFoot, 6)
	RBX_ENUM_VALUE(BodyPartR15, RightLowerLeg, 7)
	RBX_ENUM_VALUE(BodyPartR15, RightUpperLeg, 8)
	RBX_ENUM_VALUE(BodyPartR15, LeftHand, 9)
	RBX_ENUM_VALUE(BodyPartR15, LeftLowerArm, 10)
	RBX_ENUM_VALUE(BodyPartR15, LeftUpperArm, 11)
	RBX_ENUM_VALUE(BodyPartR15, RightHand, 12)
	RBX_ENUM_VALUE(BodyPartR15, RightLowerArm, 13)
	RBX_ENUM_VALUE(BodyPartR15, RightUpperArm, 14)
	RBX_ENUM_VALUE(BodyPartR15, RootPart, 15)
	RBX_ENUM_VALUE(BodyPartR15, Unknown, 17)
RBX_ENUM_END(BodyPartR15, Head, UpperTorso, LowerTorso, LeftFoot, LeftLowerLeg, LeftUpperLeg, RightFoot, RightLowerLeg, RightUpperLeg, LeftHand, LeftLowerArm, LeftUpperArm, RightHand, RightLowerArm, RightUpperArm, RootPart, Unknown)

RBX_ENUM_BEGIN(BorderMode)
	RBX_ENUM_VALUE(BorderMode, Outline, 0)
	RBX_ENUM_VALUE(BorderMode, Middle, 1)
	RBX_ENUM_VALUE(BorderMode, Inset, 2)
RBX_ENUM_END(BorderMode, Outline, Middle, Inset)

RBX_ENUM_BEGIN(BorderStrokePosition)
	RBX_ENUM_VALUE(BorderStrokePosition, Outer, 0)
	RBX_ENUM_VALUE(BorderStrokePosition, Center, 1)
	RBX_ENUM_VALUE(BorderStrokePosition, Inner, 2)
RBX_ENUM_END(BorderStrokePosition, Outer, Center, Inner)

RBX_ENUM_BEGIN(BreakReason)
	RBX_ENUM_VALUE(BreakReason, Other, 0)
	RBX_ENUM_VALUE(BreakReason, Error, 1)
	RBX_ENUM_VALUE(BreakReason, SpecialBreakpoint, 2)
	RBX_ENUM_VALUE(BreakReason, UserBreakpoint, 3)
RBX_ENUM_END(BreakReason, Other, Error, SpecialBreakpoint, UserBreakpoint)

RBX_ENUM_BEGIN(BreakpointRemoveReason)
	RBX_ENUM_VALUE(BreakpointRemoveReason, Requested, 0)
	RBX_ENUM_VALUE(BreakpointRemoveReason, ScriptChanged, 1)
	RBX_ENUM_VALUE(BreakpointRemoveReason, ScriptRemoved, 2)
RBX_ENUM_END(BreakpointRemoveReason, Requested, ScriptChanged, ScriptRemoved)

RBX_ENUM_BEGIN(BulkMoveMode)
	RBX_ENUM_VALUE(BulkMoveMode, FireAllEvents, 0)
	RBX_ENUM_VALUE(BulkMoveMode, FireCFrameChanged, 1)
RBX_ENUM_END(BulkMoveMode, FireAllEvents, FireCFrameChanged)

RBX_ENUM_BEGIN(BundleType)
	RBX_ENUM_VALUE(BundleType, BodyParts, 1)
	RBX_ENUM_VALUE(BundleType, Animations, 2)
	RBX_ENUM_VALUE(BundleType, Shoes, 3)
	RBX_ENUM_VALUE(BundleType, DynamicHead, 4)
	RBX_ENUM_VALUE(BundleType, DynamicHeadAvatar, 5)
RBX_ENUM_END(BundleType, BodyParts, Animations, Shoes, DynamicHead, DynamicHeadAvatar)

RBX_ENUM_BEGIN(Button)
	RBX_ENUM_VALUE(Button, Dismount, 8)
	RBX_ENUM_VALUE(Button, Jump, 32)
RBX_ENUM_END(Button, Dismount, Jump)

RBX_ENUM_BEGIN(ButtonStyle)
	RBX_ENUM_VALUE(ButtonStyle, Custom, 0)
	RBX_ENUM_VALUE(ButtonStyle, RobloxButtonDefault, 1)
	RBX_ENUM_VALUE(ButtonStyle, RobloxButton, 2)
	RBX_ENUM_VALUE(ButtonStyle, RobloxRoundButton, 3)
	RBX_ENUM_VALUE(ButtonStyle, RobloxRoundDefaultButton, 4)
	RBX_ENUM_VALUE(ButtonStyle, RobloxRoundDropdownButton, 5)
RBX_ENUM_END(ButtonStyle, Custom, RobloxButtonDefault, RobloxButton, RobloxRoundButton, RobloxRoundDefaultButton, RobloxRoundDropdownButton)

RBX_ENUM_BEGIN(CageType)
	RBX_ENUM_VALUE(CageType, Inner, 0)
	RBX_ENUM_VALUE(CageType, Outer, 1)
RBX_ENUM_END(CageType, Inner, Outer)

RBX_ENUM_BEGIN(CameraMode)
	RBX_ENUM_VALUE(CameraMode, Classic, 0)
	RBX_ENUM_VALUE(CameraMode, LockFirstPerson, 1)
RBX_ENUM_END(CameraMode, Classic, LockFirstPerson)

RBX_ENUM_BEGIN(CameraNavigationModel)
	RBX_ENUM_VALUE(CameraNavigationModel, Roblox, 0)
	RBX_ENUM_VALUE(CameraNavigationModel, IndustryCompatible, 1)
RBX_ENUM_END(CameraNavigationModel, Roblox, IndustryCompatible)

RBX_ENUM_BEGIN(CameraPanMode)
	RBX_ENUM_VALUE(CameraPanMode, Classic, 0)
	RBX_ENUM_VALUE(CameraPanMode, EdgeBump, 1)
RBX_ENUM_END(CameraPanMode, Classic, EdgeBump)

RBX_ENUM_BEGIN(CameraSpeedAdjustBinding)
	RBX_ENUM_VALUE(CameraSpeedAdjustBinding, None, 0)
	RBX_ENUM_VALUE(CameraSpeedAdjustBinding, RmbScroll, 1)
	RBX_ENUM_VALUE(CameraSpeedAdjustBinding, AltScroll, 2)
RBX_ENUM_END(CameraSpeedAdjustBinding, None, RmbScroll, AltScroll)

RBX_ENUM_BEGIN(CameraType)
	RBX_ENUM_VALUE(CameraType, Fixed, 0)
	RBX_ENUM_VALUE(CameraType, Attach, 1)
	RBX_ENUM_VALUE(CameraType, Watch, 2)
	RBX_ENUM_VALUE(CameraType, Track, 3)
	RBX_ENUM_VALUE(CameraType, Follow, 4)
	RBX_ENUM_VALUE(CameraType, Custom, 5)
	RBX_ENUM_VALUE(CameraType, Scriptable, 6)
	RBX_ENUM_VALUE(CameraType, Orbital, 7)
RBX_ENUM_END(CameraType, Fixed, Attach, Watch, Track, Follow, Custom, Scriptable, Orbital)

RBX_ENUM_BEGIN(CanCollaborateError)
	RBX_ENUM_VALUE(CanCollaborateError, Invalid, 0)
	RBX_ENUM_VALUE(CanCollaborateError, None, 1)
	RBX_ENUM_VALUE(CanCollaborateError, NotAgeVerified, 2)
	RBX_ENUM_VALUE(CanCollaborateError, OutsideAgeBucket, 3)
	RBX_ENUM_VALUE(CanCollaborateError, TooManyCollaborators, 4)
	RBX_ENUM_VALUE(CanCollaborateError, PCBlock, 5)
	RBX_ENUM_VALUE(CanCollaborateError, NotFound, 6)
	RBX_ENUM_VALUE(CanCollaborateError, OutsideOwnerAgeBucket, 7)
RBX_ENUM_END(CanCollaborateError, Invalid, None, NotAgeVerified, OutsideAgeBucket, TooManyCollaborators, PCBlock, NotFound, OutsideOwnerAgeBucket)

RBX_ENUM_BEGIN(CaptureGalleryPermission)
	RBX_ENUM_VALUE(CaptureGalleryPermission, ReadAndUpload, 0)
RBX_ENUM_END(CaptureGalleryPermission, ReadAndUpload)

RBX_ENUM_BEGIN(CaptureType)
	RBX_ENUM_VALUE(CaptureType, Screenshot, 1)
	RBX_ENUM_VALUE(CaptureType, Video, 2)
RBX_ENUM_END(CaptureType, Screenshot, Video)

RBX_ENUM_BEGIN(CatalogCategoryFilter)
	RBX_ENUM_VALUE(CatalogCategoryFilter, None, 1)
	RBX_ENUM_VALUE(CatalogCategoryFilter, Featured, 2)
	RBX_ENUM_VALUE(CatalogCategoryFilter, Collectibles, 3)
	RBX_ENUM_VALUE(CatalogCategoryFilter, CommunityCreations, 4)
	RBX_ENUM_VALUE(CatalogCategoryFilter, Premium, 5)
	RBX_ENUM_VALUE(CatalogCategoryFilter, Recommended, 6)
RBX_ENUM_END(CatalogCategoryFilter, None, Featured, Collectibles, CommunityCreations, Premium, Recommended)

RBX_ENUM_BEGIN(CatalogSortAggregation)
	RBX_ENUM_VALUE(CatalogSortAggregation, Past12Hours, 1)
	RBX_ENUM_VALUE(CatalogSortAggregation, PastDay, 2)
	RBX_ENUM_VALUE(CatalogSortAggregation, Past3Days, 3)
	RBX_ENUM_VALUE(CatalogSortAggregation, PastWeek, 4)
	RBX_ENUM_VALUE(CatalogSortAggregation, PastMonth, 5)
	RBX_ENUM_VALUE(CatalogSortAggregation, AllTime, 6)
RBX_ENUM_END(CatalogSortAggregation, Past12Hours, PastDay, Past3Days, PastWeek, PastMonth, AllTime)

RBX_ENUM_BEGIN(CatalogSortType)
	RBX_ENUM_VALUE(CatalogSortType, Relevance, 1)
	RBX_ENUM_VALUE(CatalogSortType, PriceHighToLow, 2)
	RBX_ENUM_VALUE(CatalogSortType, PriceLowToHigh, 3)
	RBX_ENUM_VALUE(CatalogSortType, MostFavorited, 5)
	RBX_ENUM_VALUE(CatalogSortType, RecentlyCreated, 6)
	RBX_ENUM_VALUE(CatalogSortType, Bestselling, 7)
RBX_ENUM_END(CatalogSortType, Relevance, PriceHighToLow, PriceLowToHigh, MostFavorited, RecentlyCreated, Bestselling)

RBX_ENUM_BEGIN(CellBlock)
	RBX_ENUM_VALUE(CellBlock, Solid, 0)
	RBX_ENUM_VALUE(CellBlock, VerticalWedge, 1)
	RBX_ENUM_VALUE(CellBlock, CornerWedge, 2)
	RBX_ENUM_VALUE(CellBlock, InverseCornerWedge, 3)
	RBX_ENUM_VALUE(CellBlock, HorizontalWedge, 4)
RBX_ENUM_END(CellBlock, Solid, VerticalWedge, CornerWedge, InverseCornerWedge, HorizontalWedge)

RBX_ENUM_BEGIN(CellMaterial)
	RBX_ENUM_VALUE(CellMaterial, Empty, 0)
	RBX_ENUM_VALUE(CellMaterial, Grass, 1)
	RBX_ENUM_VALUE(CellMaterial, Sand, 2)
	RBX_ENUM_VALUE(CellMaterial, Brick, 3)
	RBX_ENUM_VALUE(CellMaterial, Granite, 4)
	RBX_ENUM_VALUE(CellMaterial, Asphalt, 5)
	RBX_ENUM_VALUE(CellMaterial, Iron, 6)
	RBX_ENUM_VALUE(CellMaterial, Aluminum, 7)
	RBX_ENUM_VALUE(CellMaterial, Gold, 8)
	RBX_ENUM_VALUE(CellMaterial, WoodPlank, 9)
	RBX_ENUM_VALUE(CellMaterial, WoodLog, 10)
	RBX_ENUM_VALUE(CellMaterial, Gravel, 11)
	RBX_ENUM_VALUE(CellMaterial, CinderBlock, 12)
	RBX_ENUM_VALUE(CellMaterial, MossyStone, 13)
	RBX_ENUM_VALUE(CellMaterial, Cement, 14)
	RBX_ENUM_VALUE(CellMaterial, RedPlastic, 15)
	RBX_ENUM_VALUE(CellMaterial, BluePlastic, 16)
	RBX_ENUM_VALUE(CellMaterial, Water, 17)
RBX_ENUM_END(CellMaterial, Empty, Grass, Sand, Brick, Granite, Asphalt, Iron, Aluminum, Gold, WoodPlank, WoodLog, Gravel, CinderBlock, MossyStone, Cement, RedPlastic, BluePlastic, Water)

RBX_ENUM_BEGIN(CellOrientation)
	RBX_ENUM_VALUE(CellOrientation, NegZ, 0)
	RBX_ENUM_VALUE(CellOrientation, X, 1)
	RBX_ENUM_VALUE(CellOrientation, Z, 2)
	RBX_ENUM_VALUE(CellOrientation, NegX, 3)
RBX_ENUM_END(CellOrientation, NegZ, X, Z, NegX)

RBX_ENUM_BEGIN(CenterDialogType)
	RBX_ENUM_VALUE(CenterDialogType, UnsolicitedDialog, 1)
	RBX_ENUM_VALUE(CenterDialogType, PlayerInitiatedDialog, 2)
	RBX_ENUM_VALUE(CenterDialogType, ModalDialog, 3)
	RBX_ENUM_VALUE(CenterDialogType, QuitDialog, 4)
RBX_ENUM_END(CenterDialogType, UnsolicitedDialog, PlayerInitiatedDialog, ModalDialog, QuitDialog)

RBX_ENUM_BEGIN(CharacterControlMode)
	RBX_ENUM_VALUE(CharacterControlMode, Default, 0)
	RBX_ENUM_VALUE(CharacterControlMode, Legacy, 1)
	RBX_ENUM_VALUE(CharacterControlMode, NoCharacterController, 2)
	RBX_ENUM_VALUE(CharacterControlMode, LuaCharacterController, 3)
RBX_ENUM_END(CharacterControlMode, Default, Legacy, NoCharacterController, LuaCharacterController)

RBX_ENUM_BEGIN(ChatCallbackType)
	RBX_ENUM_VALUE(ChatCallbackType, OnCreatingChatWindow, 1)
	RBX_ENUM_VALUE(ChatCallbackType, OnClientSendingMessage, 2)
	RBX_ENUM_VALUE(ChatCallbackType, OnClientFormattingMessage, 3)
	RBX_ENUM_VALUE(ChatCallbackType, OnServerReceivingMessage, 17)
RBX_ENUM_END(ChatCallbackType, OnCreatingChatWindow, OnClientSendingMessage, OnClientFormattingMessage, OnServerReceivingMessage)

RBX_ENUM_BEGIN(ChatColor)
	RBX_ENUM_VALUE(ChatColor, Blue, 0)
	RBX_ENUM_VALUE(ChatColor, Green, 1)
	RBX_ENUM_VALUE(ChatColor, Red, 2)
	RBX_ENUM_VALUE(ChatColor, White, 3)
RBX_ENUM_END(ChatColor, Blue, Green, Red, White)

RBX_ENUM_BEGIN(ChatMode)
	RBX_ENUM_VALUE(ChatMode, Menu, 0)
	RBX_ENUM_VALUE(ChatMode, TextAndMenu, 1)
RBX_ENUM_END(ChatMode, Menu, TextAndMenu)

RBX_ENUM_BEGIN(ChatPrivacyMode)
	RBX_ENUM_VALUE(ChatPrivacyMode, AllUsers, 0)
	RBX_ENUM_VALUE(ChatPrivacyMode, NoOne, 1)
	RBX_ENUM_VALUE(ChatPrivacyMode, Friends, 2)
RBX_ENUM_END(ChatPrivacyMode, AllUsers, NoOne, Friends)

RBX_ENUM_BEGIN(ChatRestrictionStatus)
	RBX_ENUM_VALUE(ChatRestrictionStatus, Unknown, 0)
	RBX_ENUM_VALUE(ChatRestrictionStatus, NotRestricted, 1)
	RBX_ENUM_VALUE(ChatRestrictionStatus, Restricted, 2)
RBX_ENUM_END(ChatRestrictionStatus, Unknown, NotRestricted, Restricted)

RBX_ENUM_BEGIN(ChatStyle)
	RBX_ENUM_VALUE(ChatStyle, Classic, 0)
	RBX_ENUM_VALUE(ChatStyle, Bubble, 1)
	RBX_ENUM_VALUE(ChatStyle, ClassicAndBubble, 2)
RBX_ENUM_END(ChatStyle, Classic, Bubble, ClassicAndBubble)

RBX_ENUM_BEGIN(ChatVersion)
	RBX_ENUM_VALUE(ChatVersion, LegacyChatService, 0)
	RBX_ENUM_VALUE(ChatVersion, TextChatService, 1)
RBX_ENUM_END(ChatVersion, LegacyChatService, TextChatService)

RBX_ENUM_BEGIN(ClientAnimatorThrottlingMode)
	RBX_ENUM_VALUE(ClientAnimatorThrottlingMode, Default, 0)
	RBX_ENUM_VALUE(ClientAnimatorThrottlingMode, Disabled, 1)
	RBX_ENUM_VALUE(ClientAnimatorThrottlingMode, Enabled, 2)
RBX_ENUM_END(ClientAnimatorThrottlingMode, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(CloseReason)
	RBX_ENUM_VALUE(CloseReason, Unknown, 0)
	RBX_ENUM_VALUE(CloseReason, RobloxMaintenance, 1)
	RBX_ENUM_VALUE(CloseReason, DeveloperShutdown, 2)
	RBX_ENUM_VALUE(CloseReason, DeveloperUpdate, 3)
	RBX_ENUM_VALUE(CloseReason, ServerEmpty, 4)
	RBX_ENUM_VALUE(CloseReason, OutOfMemory, 5)
RBX_ENUM_END(CloseReason, Unknown, RobloxMaintenance, DeveloperShutdown, DeveloperUpdate, ServerEmpty, OutOfMemory)

RBX_ENUM_BEGIN(CollaboratorStatus)
	RBX_ENUM_VALUE(CollaboratorStatus, None, 0)
	RBX_ENUM_VALUE(CollaboratorStatus, Editing3D, 1)
	RBX_ENUM_VALUE(CollaboratorStatus, Scripting, 2)
	RBX_ENUM_VALUE(CollaboratorStatus, PrivateScripting, 3)
RBX_ENUM_END(CollaboratorStatus, None, Editing3D, Scripting, PrivateScripting)

RBX_ENUM_BEGIN(CollisionFidelity)
	RBX_ENUM_VALUE(CollisionFidelity, Default, 0)
	RBX_ENUM_VALUE(CollisionFidelity, Hull, 1)
	RBX_ENUM_VALUE(CollisionFidelity, Box, 2)
	RBX_ENUM_VALUE(CollisionFidelity, PreciseConvexDecomposition, 3)
RBX_ENUM_END(CollisionFidelity, Default, Hull, Box, PreciseConvexDecomposition)

RBX_ENUM_BEGIN(CommandPermission)
	RBX_ENUM_VALUE(CommandPermission, Plugin, 0)
	RBX_ENUM_VALUE(CommandPermission, LocalUser, 1)
RBX_ENUM_END(CommandPermission, Plugin, LocalUser)

RBX_ENUM_BEGIN(CompileTarget)
	RBX_ENUM_VALUE(CompileTarget, Client, 0)
	RBX_ENUM_VALUE(CompileTarget, CoreScript, 1)
	RBX_ENUM_VALUE(CompileTarget, Studio, 2)
	RBX_ENUM_VALUE(CompileTarget, CoreScriptRaw, 3)
RBX_ENUM_END(CompileTarget, Client, CoreScript, Studio, CoreScriptRaw)

RBX_ENUM_BEGIN(CompletionAcceptanceBehavior)
	RBX_ENUM_VALUE(CompletionAcceptanceBehavior, Insert, 0)
	RBX_ENUM_VALUE(CompletionAcceptanceBehavior, Replace, 1)
	RBX_ENUM_VALUE(CompletionAcceptanceBehavior, ReplaceOnEnterInsertOnTab, 2)
	RBX_ENUM_VALUE(CompletionAcceptanceBehavior, InsertOnEnterReplaceOnTab, 3)
RBX_ENUM_END(CompletionAcceptanceBehavior, Insert, Replace, ReplaceOnEnterInsertOnTab, InsertOnEnterReplaceOnTab)

RBX_ENUM_BEGIN(CompletionItemKind)
	RBX_ENUM_VALUE(CompletionItemKind, Text, 1)
	RBX_ENUM_VALUE(CompletionItemKind, Method, 2)
	RBX_ENUM_VALUE(CompletionItemKind, Function, 3)
	RBX_ENUM_VALUE(CompletionItemKind, Constructor, 4)
	RBX_ENUM_VALUE(CompletionItemKind, Field, 5)
	RBX_ENUM_VALUE(CompletionItemKind, Variable, 6)
	RBX_ENUM_VALUE(CompletionItemKind, Class, 7)
	RBX_ENUM_VALUE(CompletionItemKind, Interface, 8)
	RBX_ENUM_VALUE(CompletionItemKind, Module, 9)
	RBX_ENUM_VALUE(CompletionItemKind, Property, 10)
	RBX_ENUM_VALUE(CompletionItemKind, Unit, 11)
	RBX_ENUM_VALUE(CompletionItemKind, Value, 12)
	RBX_ENUM_VALUE(CompletionItemKind, Enum, 13)
	RBX_ENUM_VALUE(CompletionItemKind, Keyword, 14)
	RBX_ENUM_VALUE(CompletionItemKind, Snippet, 15)
	RBX_ENUM_VALUE(CompletionItemKind, Color, 16)
	RBX_ENUM_VALUE(CompletionItemKind, File, 17)
	RBX_ENUM_VALUE(CompletionItemKind, Reference, 18)
	RBX_ENUM_VALUE(CompletionItemKind, Folder, 19)
	RBX_ENUM_VALUE(CompletionItemKind, EnumMember, 20)
	RBX_ENUM_VALUE(CompletionItemKind, Constant, 21)
	RBX_ENUM_VALUE(CompletionItemKind, Struct, 22)
	RBX_ENUM_VALUE(CompletionItemKind, Event, 23)
	RBX_ENUM_VALUE(CompletionItemKind, Operator, 24)
	RBX_ENUM_VALUE(CompletionItemKind, TypeParameter, 25)
RBX_ENUM_END(CompletionItemKind, Text, Method, Function, Constructor, Field, Variable, Class, Interface, Module, Property, Unit, Value, Enum, Keyword, Snippet, Color, File, Reference, Folder, EnumMember, Constant, Struct, Event, Operator, TypeParameter)

RBX_ENUM_BEGIN(CompletionItemTag)
	RBX_ENUM_VALUE(CompletionItemTag, Deprecated, 1)
	RBX_ENUM_VALUE(CompletionItemTag, IncorrectIndexType, 2)
	RBX_ENUM_VALUE(CompletionItemTag, PluginPermissions, 3)
	RBX_ENUM_VALUE(CompletionItemTag, CommandLinePermissions, 4)
	RBX_ENUM_VALUE(CompletionItemTag, RobloxPermissions, 5)
	RBX_ENUM_VALUE(CompletionItemTag, AddParens, 6)
	RBX_ENUM_VALUE(CompletionItemTag, PutCursorInParens, 7)
	RBX_ENUM_VALUE(CompletionItemTag, TypeCorrect, 8)
	RBX_ENUM_VALUE(CompletionItemTag, ClientServerBoundaryViolation, 9)
	RBX_ENUM_VALUE(CompletionItemTag, Invalidated, 10)
	RBX_ENUM_VALUE(CompletionItemTag, PutCursorBeforeEnd, 11)
RBX_ENUM_END(CompletionItemTag, Deprecated, IncorrectIndexType, PluginPermissions, CommandLinePermissions, RobloxPermissions, AddParens, PutCursorInParens, TypeCorrect, ClientServerBoundaryViolation, Invalidated, PutCursorBeforeEnd)

RBX_ENUM_BEGIN(CompletionTriggerKind)
	RBX_ENUM_VALUE(CompletionTriggerKind, Invoked, 1)
	RBX_ENUM_VALUE(CompletionTriggerKind, TriggerCharacter, 2)
	RBX_ENUM_VALUE(CompletionTriggerKind, TriggerForIncompleteCompletions, 3)
RBX_ENUM_END(CompletionTriggerKind, Invoked, TriggerCharacter, TriggerForIncompleteCompletions)

RBX_ENUM_BEGIN(CompositeValueCurveType)
	RBX_ENUM_VALUE(CompositeValueCurveType, ColorRGB, 0)
	RBX_ENUM_VALUE(CompositeValueCurveType, ColorHSV, 1)
	RBX_ENUM_VALUE(CompositeValueCurveType, NumberRange, 2)
	RBX_ENUM_VALUE(CompositeValueCurveType, Rect, 3)
	RBX_ENUM_VALUE(CompositeValueCurveType, UDim, 4)
	RBX_ENUM_VALUE(CompositeValueCurveType, UDim2, 5)
	RBX_ENUM_VALUE(CompositeValueCurveType, Vector2, 6)
	RBX_ENUM_VALUE(CompositeValueCurveType, Vector3, 7)
RBX_ENUM_END(CompositeValueCurveType, ColorRGB, ColorHSV, NumberRange, Rect, UDim, UDim2, Vector2, Vector3)

RBX_ENUM_BEGIN(CompressionAlgorithm)
	RBX_ENUM_VALUE(CompressionAlgorithm, Zstd, 0)
RBX_ENUM_END(CompressionAlgorithm, Zstd)

RBX_ENUM_BEGIN(ComputerCameraMovementMode)
	RBX_ENUM_VALUE(ComputerCameraMovementMode, Default, 0)
	RBX_ENUM_VALUE(ComputerCameraMovementMode, Classic, 1)
	RBX_ENUM_VALUE(ComputerCameraMovementMode, Follow, 2)
	RBX_ENUM_VALUE(ComputerCameraMovementMode, Orbital, 3)
	RBX_ENUM_VALUE(ComputerCameraMovementMode, CameraToggle, 4)
RBX_ENUM_END(ComputerCameraMovementMode, Default, Classic, Follow, Orbital, CameraToggle)

RBX_ENUM_BEGIN(ComputerMovementMode)
	RBX_ENUM_VALUE(ComputerMovementMode, Default, 0)
	RBX_ENUM_VALUE(ComputerMovementMode, KeyboardMouse, 1)
	RBX_ENUM_VALUE(ComputerMovementMode, ClickToMove, 2)
RBX_ENUM_END(ComputerMovementMode, Default, KeyboardMouse, ClickToMove)

RBX_ENUM_BEGIN(ConfigSnapshotErrorState)
	RBX_ENUM_VALUE(ConfigSnapshotErrorState, None, 0)
	RBX_ENUM_VALUE(ConfigSnapshotErrorState, LoadFailed, 1)
RBX_ENUM_END(ConfigSnapshotErrorState, None, LoadFailed)

RBX_ENUM_BEGIN(ConnectionError)
	RBX_ENUM_VALUE(ConnectionError, OK, 0)
	RBX_ENUM_VALUE(ConnectionError, Unknown, 1)
	RBX_ENUM_VALUE(ConnectionError, ConnectErrors, 2)
	RBX_ENUM_VALUE(ConnectionError, AlreadyConnected, 3)
	RBX_ENUM_VALUE(ConnectionError, NoFreeIncomingConnections, 4)
	RBX_ENUM_VALUE(ConnectionError, ConnectionBanned, 5)
	RBX_ENUM_VALUE(ConnectionError, InvalidPassword, 6)
	RBX_ENUM_VALUE(ConnectionError, IncompatibleProtocolVersion, 7)
	RBX_ENUM_VALUE(ConnectionError, IPRecentlyConnected, 8)
	RBX_ENUM_VALUE(ConnectionError, OurSystemRequiresSecurity, 9)
	RBX_ENUM_VALUE(ConnectionError, SecurityKeyMismatch, 10)
	RBX_ENUM_VALUE(ConnectionError, DisconnectErrors, 256)
	RBX_ENUM_VALUE(ConnectionError, DisconnectBadhash, 257)
	RBX_ENUM_VALUE(ConnectionError, DisconnectSecurityKeyMismatch, 258)
	RBX_ENUM_VALUE(ConnectionError, DisconnectProtocolMismatch, 259)
	RBX_ENUM_VALUE(ConnectionError, DisconnectReceivePacketError, 260)
	RBX_ENUM_VALUE(ConnectionError, DisconnectReceivePacketStreamError, 261)
	RBX_ENUM_VALUE(ConnectionError, DisconnectSendPacketError, 262)
	RBX_ENUM_VALUE(ConnectionError, DisconnectIllegalTeleport, 263)
	RBX_ENUM_VALUE(ConnectionError, DisconnectDuplicatePlayer, 264)
	RBX_ENUM_VALUE(ConnectionError, DisconnectDuplicateTicket, 265)
	RBX_ENUM_VALUE(ConnectionError, DisconnectTimeout, 266)
	RBX_ENUM_VALUE(ConnectionError, DisconnectLuaKick, 267)
	RBX_ENUM_VALUE(ConnectionError, DisconnectOnRemoteSysStats, 268)
	RBX_ENUM_VALUE(ConnectionError, DisconnectHashTimeout, 269)
	RBX_ENUM_VALUE(ConnectionError, DisconnectCloudEditKick, 270)
	RBX_ENUM_VALUE(ConnectionError, DisconnectPlayerless, 271)
	RBX_ENUM_VALUE(ConnectionError, DisconnectNewSecurityKeyMismatch, 272)
	RBX_ENUM_VALUE(ConnectionError, DisconnectEvicted, 273)
	RBX_ENUM_VALUE(ConnectionError, DisconnectDevMaintenance, 274)
	RBX_ENUM_VALUE(ConnectionError, DisconnectRobloxMaintenance, 275)
	RBX_ENUM_VALUE(ConnectionError, DisconnectRejoin, 276)
	RBX_ENUM_VALUE(ConnectionError, DisconnectConnectionLost, 277)
	RBX_ENUM_VALUE(ConnectionError, DisconnectIdle, 278)
	RBX_ENUM_VALUE(ConnectionError, DisconnectRaknetErrors, 279)
	RBX_ENUM_VALUE(ConnectionError, DisconnectWrongVersion, 280)
	RBX_ENUM_VALUE(ConnectionError, DisconnectBySecurityPolicy, 281)
	RBX_ENUM_VALUE(ConnectionError, DisconnectBlockedIP, 282)
	RBX_ENUM_VALUE(ConnectionError, DisconnectClientFailure, 284)
	RBX_ENUM_VALUE(ConnectionError, DisconnectClientRequest, 285)
	RBX_ENUM_VALUE(ConnectionError, DisconnectPrivateServerKickout, 286)
	RBX_ENUM_VALUE(ConnectionError, DisconnectModeratedGame, 287)
	RBX_ENUM_VALUE(ConnectionError, ServerShutdown, 288)
	RBX_ENUM_VALUE(ConnectionError, ReplicatorTimeout, 290)
	RBX_ENUM_VALUE(ConnectionError, PlayerRemoved, 291)
	RBX_ENUM_VALUE(ConnectionError, DisconnectOutOfMemoryKeepPlayingLeave, 292)
	RBX_ENUM_VALUE(ConnectionError, DisconnectRomarkEndOfTest, 293)
	RBX_ENUM_VALUE(ConnectionError, DisconnectCollaboratorPermissionRevoked, 294)
	RBX_ENUM_VALUE(ConnectionError, DisconnectCollaboratorUnderage, 295)
	RBX_ENUM_VALUE(ConnectionError, NetworkInternal, 296)
	RBX_ENUM_VALUE(ConnectionError, NetworkSend, 297)
	RBX_ENUM_VALUE(ConnectionError, NetworkTimeout, 298)
	RBX_ENUM_VALUE(ConnectionError, NetworkMisbehavior, 299)
	RBX_ENUM_VALUE(ConnectionError, NetworkSecurity, 300)
	RBX_ENUM_VALUE(ConnectionError, ReplacementReady, 301)
	RBX_ENUM_VALUE(ConnectionError, ServerEmpty, 302)
	RBX_ENUM_VALUE(ConnectionError, PhantomFreeze, 303)
	RBX_ENUM_VALUE(ConnectionError, AndroidAnticheatKick, 304)
	RBX_ENUM_VALUE(ConnectionError, AndroidEmulatorKick, 305)
	RBX_ENUM_VALUE(ConnectionError, AndroidRootedKick, 306)
	RBX_ENUM_VALUE(ConnectionError, ScreentimeLockoutKick, 307)
	RBX_ENUM_VALUE(ConnectionError, DisconnectionNotification, 308)
	RBX_ENUM_VALUE(ConnectionError, DisconnectVerboselyModeratedGame, 309)
	RBX_ENUM_VALUE(ConnectionError, DisconnectCollaboratorNotAgeVerified, 310)
	RBX_ENUM_VALUE(ConnectionError, DisconnectCollaboratorTrustedConnectionsRequired, 311)
	RBX_ENUM_VALUE(ConnectionError, DisconnectCollaboratorOwnerActionRequired, 312)
	RBX_ENUM_VALUE(ConnectionError, DisconnectCollaboratorTooManyCollaborators, 313)
	RBX_ENUM_VALUE(ConnectionError, DisconnectCollaboratorUnknownError, 314)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchErrors, 512)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchDisabled, 515)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchError, 516)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchGameEnded, 517)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchGameFull, 518)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchUserLeft, 522)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchRestricted, 523)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchUnauthorized, 524)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchFlooded, 525)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchHashExpired, 526)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchHashException, 527)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchPartyCannotFit, 528)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchHttpError, 529)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchUserPrivacyUnauthorized, 533)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchAgeVerificationRequired, 542)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchParentalApprovalRequired, 543)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchCoreGated, 544)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchCreatorBan, 600)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchDeviceBlock, 601)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchCustomMessage, 610)
	RBX_ENUM_VALUE(ConnectionError, PlacelaunchOtherError, 611)
	RBX_ENUM_VALUE(ConnectionError, TeleportErrors, 768)
	RBX_ENUM_VALUE(ConnectionError, TeleportFailure, 769)
	RBX_ENUM_VALUE(ConnectionError, TeleportGameNotFound, 770)
	RBX_ENUM_VALUE(ConnectionError, TeleportGameEnded, 771)
	RBX_ENUM_VALUE(ConnectionError, TeleportGameFull, 772)
	RBX_ENUM_VALUE(ConnectionError, TeleportUnauthorized, 773)
	RBX_ENUM_VALUE(ConnectionError, TeleportFlooded, 774)
	RBX_ENUM_VALUE(ConnectionError, TeleportIsTeleporting, 775)
RBX_ENUM_END(ConnectionError, OK, Unknown, ConnectErrors, AlreadyConnected, NoFreeIncomingConnections, ConnectionBanned, InvalidPassword, IncompatibleProtocolVersion, IPRecentlyConnected, OurSystemRequiresSecurity, SecurityKeyMismatch, DisconnectErrors, DisconnectBadhash, DisconnectSecurityKeyMismatch, DisconnectProtocolMismatch, DisconnectReceivePacketError, DisconnectReceivePacketStreamError, DisconnectSendPacketError, DisconnectIllegalTeleport, DisconnectDuplicatePlayer, DisconnectDuplicateTicket, DisconnectTimeout, DisconnectLuaKick, DisconnectOnRemoteSysStats, DisconnectHashTimeout, DisconnectCloudEditKick, DisconnectPlayerless, DisconnectNewSecurityKeyMismatch, DisconnectEvicted, DisconnectDevMaintenance, DisconnectRobloxMaintenance, DisconnectRejoin, DisconnectConnectionLost, DisconnectIdle, DisconnectRaknetErrors, DisconnectWrongVersion, DisconnectBySecurityPolicy, DisconnectBlockedIP, DisconnectClientFailure, DisconnectClientRequest, DisconnectPrivateServerKickout, DisconnectModeratedGame, ServerShutdown, ReplicatorTimeout, PlayerRemoved, DisconnectOutOfMemoryKeepPlayingLeave, DisconnectRomarkEndOfTest, DisconnectCollaboratorPermissionRevoked, DisconnectCollaboratorUnderage, NetworkInternal, NetworkSend, NetworkTimeout, NetworkMisbehavior, NetworkSecurity, ReplacementReady, ServerEmpty, PhantomFreeze, AndroidAnticheatKick, AndroidEmulatorKick, AndroidRootedKick, ScreentimeLockoutKick, DisconnectionNotification, DisconnectVerboselyModeratedGame, DisconnectCollaboratorNotAgeVerified, DisconnectCollaboratorTrustedConnectionsRequired, DisconnectCollaboratorOwnerActionRequired, DisconnectCollaboratorTooManyCollaborators, DisconnectCollaboratorUnknownError, PlacelaunchErrors, PlacelaunchDisabled, PlacelaunchError, PlacelaunchGameEnded, PlacelaunchGameFull, PlacelaunchUserLeft, PlacelaunchRestricted, PlacelaunchUnauthorized, PlacelaunchFlooded, PlacelaunchHashExpired, PlacelaunchHashException, PlacelaunchPartyCannotFit, PlacelaunchHttpError, PlacelaunchUserPrivacyUnauthorized, PlacelaunchAgeVerificationRequired, PlacelaunchParentalApprovalRequired, PlacelaunchCoreGated, PlacelaunchCreatorBan, PlacelaunchDeviceBlock, PlacelaunchCustomMessage, PlacelaunchOtherError, TeleportErrors, TeleportFailure, TeleportGameNotFound, TeleportGameEnded, TeleportGameFull, TeleportUnauthorized, TeleportFlooded, TeleportIsTeleporting)

RBX_ENUM_BEGIN(ConnectionState)
	RBX_ENUM_VALUE(ConnectionState, Connected, 0)
	RBX_ENUM_VALUE(ConnectionState, Disconnected, 1)
RBX_ENUM_END(ConnectionState, Connected, Disconnected)

RBX_ENUM_BEGIN(ContentSourceType)
	RBX_ENUM_VALUE(ContentSourceType, None, 0)
	RBX_ENUM_VALUE(ContentSourceType, Uri, 1)
	RBX_ENUM_VALUE(ContentSourceType, Object, 2)
	RBX_ENUM_VALUE(ContentSourceType, Opaque, 3)
RBX_ENUM_END(ContentSourceType, None, Uri, Object, Opaque)

RBX_ENUM_BEGIN(ContextActionPriority)
	RBX_ENUM_VALUE(ContextActionPriority, Low, 1000)
	RBX_ENUM_VALUE(ContextActionPriority, Medium, 2000)
	RBX_ENUM_VALUE(ContextActionPriority, High, 3000)
RBX_ENUM_END(ContextActionPriority, Low, Medium, High)

RBX_ENUM_BEGIN(ContextActionResult)
	RBX_ENUM_VALUE(ContextActionResult, Sink, 0)
	RBX_ENUM_VALUE(ContextActionResult, Pass, 1)
RBX_ENUM_END(ContextActionResult, Sink, Pass)

RBX_ENUM_BEGIN(ControlMode)
	RBX_ENUM_VALUE(ControlMode, Classic, 0)
	RBX_ENUM_VALUE(ControlMode, MouseLockSwitch, 1)
RBX_ENUM_END(ControlMode, Classic, MouseLockSwitch)

RBX_ENUM_BEGIN(CoreGuiType)
	RBX_ENUM_VALUE(CoreGuiType, PlayerList, 0)
	RBX_ENUM_VALUE(CoreGuiType, Health, 1)
	RBX_ENUM_VALUE(CoreGuiType, Backpack, 2)
	RBX_ENUM_VALUE(CoreGuiType, Chat, 3)
	RBX_ENUM_VALUE(CoreGuiType, All, 4)
	RBX_ENUM_VALUE(CoreGuiType, EmotesMenu, 5)
	RBX_ENUM_VALUE(CoreGuiType, SelfView, 6)
	RBX_ENUM_VALUE(CoreGuiType, Captures, 7)
	RBX_ENUM_VALUE(CoreGuiType, AvatarSwitcher, 8)
	RBX_ENUM_VALUE(CoreGuiType, ExperienceShop, 9)
RBX_ENUM_END(CoreGuiType, PlayerList, Health, Backpack, Chat, All, EmotesMenu, SelfView, Captures, AvatarSwitcher, ExperienceShop)

RBX_ENUM_BEGIN(CreateAssetResult)
	RBX_ENUM_VALUE(CreateAssetResult, Success, 1)
	RBX_ENUM_VALUE(CreateAssetResult, PermissionDenied, 2)
	RBX_ENUM_VALUE(CreateAssetResult, UploadFailed, 3)
	RBX_ENUM_VALUE(CreateAssetResult, Unknown, 4)
RBX_ENUM_END(CreateAssetResult, Success, PermissionDenied, UploadFailed, Unknown)

RBX_ENUM_BEGIN(CreateContentResult)
	RBX_ENUM_VALUE(CreateContentResult, Success, 1)
	RBX_ENUM_VALUE(CreateContentResult, PermissionDenied, 2)
	RBX_ENUM_VALUE(CreateContentResult, UploadFailed, 3)
	RBX_ENUM_VALUE(CreateContentResult, StorageLimitExceeded, 4)
	RBX_ENUM_VALUE(CreateContentResult, Unknown, 5)
RBX_ENUM_END(CreateContentResult, Success, PermissionDenied, UploadFailed, StorageLimitExceeded, Unknown)

RBX_ENUM_BEGIN(CreateOutfitFailure)
	RBX_ENUM_VALUE(CreateOutfitFailure, InvalidName, 1)
	RBX_ENUM_VALUE(CreateOutfitFailure, OutfitLimitReached, 2)
	RBX_ENUM_VALUE(CreateOutfitFailure, Other, 3)
RBX_ENUM_END(CreateOutfitFailure, InvalidName, OutfitLimitReached, Other)

RBX_ENUM_BEGIN(CreatorType)
	RBX_ENUM_VALUE(CreatorType, User, 0)
	RBX_ENUM_VALUE(CreatorType, Group, 1)
RBX_ENUM_END(CreatorType, User, Group)

RBX_ENUM_BEGIN(CreatorTypeFilter)
	RBX_ENUM_VALUE(CreatorTypeFilter, User, 0)
	RBX_ENUM_VALUE(CreatorTypeFilter, Group, 1)
	RBX_ENUM_VALUE(CreatorTypeFilter, All, 2)
RBX_ENUM_END(CreatorTypeFilter, User, Group, All)

RBX_ENUM_BEGIN(CurrencyType)
	RBX_ENUM_VALUE(CurrencyType, Default, 0)
	RBX_ENUM_VALUE(CurrencyType, Robux, 1)
	RBX_ENUM_VALUE(CurrencyType, Tix, 2)
RBX_ENUM_END(CurrencyType, Default, Robux, Tix)

RBX_ENUM_BEGIN(CustomCameraMode)
	RBX_ENUM_VALUE(CustomCameraMode, Default, 0)
	RBX_ENUM_VALUE(CustomCameraMode, Classic, 1)
	RBX_ENUM_VALUE(CustomCameraMode, Follow, 2)
RBX_ENUM_END(CustomCameraMode, Default, Classic, Follow)

RBX_ENUM_BEGIN(DataModelExtractorFileType)
	RBX_ENUM_VALUE(DataModelExtractorFileType, PlaceFile, 0)
	RBX_ENUM_VALUE(DataModelExtractorFileType, FirstSlice, 1)
	RBX_ENUM_VALUE(DataModelExtractorFileType, NonFirstSlice, 2)
RBX_ENUM_END(DataModelExtractorFileType, PlaceFile, FirstSlice, NonFirstSlice)

RBX_ENUM_BEGIN(DataStoreRequestType)
	RBX_ENUM_VALUE(DataStoreRequestType, GetAsync, 0)
	RBX_ENUM_VALUE(DataStoreRequestType, SetIncrementAsync, 1)
	RBX_ENUM_VALUE(DataStoreRequestType, UpdateAsync, 2)
	RBX_ENUM_VALUE(DataStoreRequestType, GetSortedAsync, 3)
	RBX_ENUM_VALUE(DataStoreRequestType, SetIncrementSortedAsync, 4)
	RBX_ENUM_VALUE(DataStoreRequestType, OnUpdate, 5)
	RBX_ENUM_VALUE(DataStoreRequestType, ListAsync, 6)
	RBX_ENUM_VALUE(DataStoreRequestType, GetVersionAsync, 7)
	RBX_ENUM_VALUE(DataStoreRequestType, RemoveVersionAsync, 8)
	RBX_ENUM_VALUE(DataStoreRequestType, StandardRead, 9)
	RBX_ENUM_VALUE(DataStoreRequestType, StandardWrite, 10)
	RBX_ENUM_VALUE(DataStoreRequestType, StandardList, 11)
	RBX_ENUM_VALUE(DataStoreRequestType, StandardRemove, 12)
	RBX_ENUM_VALUE(DataStoreRequestType, OrderedRead, 13)
	RBX_ENUM_VALUE(DataStoreRequestType, OrderedWrite, 14)
	RBX_ENUM_VALUE(DataStoreRequestType, OrderedList, 15)
	RBX_ENUM_VALUE(DataStoreRequestType, OrderedRemove, 16)
RBX_ENUM_END(DataStoreRequestType, GetAsync, SetIncrementAsync, UpdateAsync, GetSortedAsync, SetIncrementSortedAsync, OnUpdate, ListAsync, GetVersionAsync, RemoveVersionAsync, StandardRead, StandardWrite, StandardList, StandardRemove, OrderedRead, OrderedWrite, OrderedList, OrderedRemove)

RBX_ENUM_BEGIN(DebuggerEndReason)
	RBX_ENUM_VALUE(DebuggerEndReason, ClientRequest, 0)
	RBX_ENUM_VALUE(DebuggerEndReason, Timeout, 1)
	RBX_ENUM_VALUE(DebuggerEndReason, InvalidHost, 2)
	RBX_ENUM_VALUE(DebuggerEndReason, Disconnected, 3)
	RBX_ENUM_VALUE(DebuggerEndReason, ServerShutdown, 4)
	RBX_ENUM_VALUE(DebuggerEndReason, ServerProtocolMismatch, 5)
	RBX_ENUM_VALUE(DebuggerEndReason, ConfigurationFailed, 6)
	RBX_ENUM_VALUE(DebuggerEndReason, RpcError, 7)
RBX_ENUM_END(DebuggerEndReason, ClientRequest, Timeout, InvalidHost, Disconnected, ServerShutdown, ServerProtocolMismatch, ConfigurationFailed, RpcError)

RBX_ENUM_BEGIN(DebuggerExceptionBreakMode)
	RBX_ENUM_VALUE(DebuggerExceptionBreakMode, Never, 0)
	RBX_ENUM_VALUE(DebuggerExceptionBreakMode, Always, 1)
	RBX_ENUM_VALUE(DebuggerExceptionBreakMode, Unhandled, 2)
RBX_ENUM_END(DebuggerExceptionBreakMode, Never, Always, Unhandled)

RBX_ENUM_BEGIN(DebuggerFrameType)
	RBX_ENUM_VALUE(DebuggerFrameType, C, 0)
	RBX_ENUM_VALUE(DebuggerFrameType, Lua, 1)
RBX_ENUM_END(DebuggerFrameType, C, Lua)

RBX_ENUM_BEGIN(DebuggerPauseReason)
	RBX_ENUM_VALUE(DebuggerPauseReason, Unknown, 0)
	RBX_ENUM_VALUE(DebuggerPauseReason, Requested, 1)
	RBX_ENUM_VALUE(DebuggerPauseReason, Breakpoint, 2)
	RBX_ENUM_VALUE(DebuggerPauseReason, Exception, 3)
	RBX_ENUM_VALUE(DebuggerPauseReason, SingleStep, 4)
	RBX_ENUM_VALUE(DebuggerPauseReason, Entrypoint, 5)
RBX_ENUM_END(DebuggerPauseReason, Unknown, Requested, Breakpoint, Exception, SingleStep, Entrypoint)

RBX_ENUM_BEGIN(DebuggerStatus)
	RBX_ENUM_VALUE(DebuggerStatus, Success, 0)
	RBX_ENUM_VALUE(DebuggerStatus, Timeout, 1)
	RBX_ENUM_VALUE(DebuggerStatus, ConnectionLost, 2)
	RBX_ENUM_VALUE(DebuggerStatus, InvalidResponse, 3)
	RBX_ENUM_VALUE(DebuggerStatus, InternalError, 4)
	RBX_ENUM_VALUE(DebuggerStatus, InvalidState, 5)
	RBX_ENUM_VALUE(DebuggerStatus, RpcError, 6)
	RBX_ENUM_VALUE(DebuggerStatus, InvalidArgument, 7)
	RBX_ENUM_VALUE(DebuggerStatus, ConnectionClosed, 8)
RBX_ENUM_END(DebuggerStatus, Success, Timeout, ConnectionLost, InvalidResponse, InternalError, InvalidState, RpcError, InvalidArgument, ConnectionClosed)

RBX_ENUM_BEGIN(DefaultScriptSyncFileType)
	RBX_ENUM_VALUE(DefaultScriptSyncFileType, Lua, 0)
	RBX_ENUM_VALUE(DefaultScriptSyncFileType, Luau, 1)
RBX_ENUM_END(DefaultScriptSyncFileType, Lua, Luau)

RBX_ENUM_BEGIN(DevCameraOcclusionMode)
	RBX_ENUM_VALUE(DevCameraOcclusionMode, Zoom, 0)
	RBX_ENUM_VALUE(DevCameraOcclusionMode, Invisicam, 1)
RBX_ENUM_END(DevCameraOcclusionMode, Zoom, Invisicam)

RBX_ENUM_BEGIN(DevComputerCameraMovementMode)
	RBX_ENUM_VALUE(DevComputerCameraMovementMode, UserChoice, 0)
	RBX_ENUM_VALUE(DevComputerCameraMovementMode, Classic, 1)
	RBX_ENUM_VALUE(DevComputerCameraMovementMode, Follow, 2)
	RBX_ENUM_VALUE(DevComputerCameraMovementMode, Orbital, 3)
	RBX_ENUM_VALUE(DevComputerCameraMovementMode, CameraToggle, 4)
RBX_ENUM_END(DevComputerCameraMovementMode, UserChoice, Classic, Follow, Orbital, CameraToggle)

RBX_ENUM_BEGIN(DevComputerMovementMode)
	RBX_ENUM_VALUE(DevComputerMovementMode, UserChoice, 0)
	RBX_ENUM_VALUE(DevComputerMovementMode, KeyboardMouse, 1)
	RBX_ENUM_VALUE(DevComputerMovementMode, ClickToMove, 2)
	RBX_ENUM_VALUE(DevComputerMovementMode, Scriptable, 3)
RBX_ENUM_END(DevComputerMovementMode, UserChoice, KeyboardMouse, ClickToMove, Scriptable)

RBX_ENUM_BEGIN(DevTouchCameraMovementMode)
	RBX_ENUM_VALUE(DevTouchCameraMovementMode, UserChoice, 0)
	RBX_ENUM_VALUE(DevTouchCameraMovementMode, Classic, 1)
	RBX_ENUM_VALUE(DevTouchCameraMovementMode, Follow, 2)
	RBX_ENUM_VALUE(DevTouchCameraMovementMode, Orbital, 3)
RBX_ENUM_END(DevTouchCameraMovementMode, UserChoice, Classic, Follow, Orbital)

RBX_ENUM_BEGIN(DevTouchMovementMode)
	RBX_ENUM_VALUE(DevTouchMovementMode, UserChoice, 0)
	RBX_ENUM_VALUE(DevTouchMovementMode, Thumbstick, 1)
	RBX_ENUM_VALUE(DevTouchMovementMode, DPad, 2)
	RBX_ENUM_VALUE(DevTouchMovementMode, Thumbpad, 3)
	RBX_ENUM_VALUE(DevTouchMovementMode, ClickToMove, 4)
	RBX_ENUM_VALUE(DevTouchMovementMode, Scriptable, 5)
	RBX_ENUM_VALUE(DevTouchMovementMode, DynamicThumbstick, 6)
RBX_ENUM_END(DevTouchMovementMode, UserChoice, Thumbstick, DPad, Thumbpad, ClickToMove, Scriptable, DynamicThumbstick)

RBX_ENUM_BEGIN(DeveloperMemoryTag)
	RBX_ENUM_VALUE(DeveloperMemoryTag, Internal, 0)
	RBX_ENUM_VALUE(DeveloperMemoryTag, HttpCache, 1)
	RBX_ENUM_VALUE(DeveloperMemoryTag, Instances, 2)
	RBX_ENUM_VALUE(DeveloperMemoryTag, Signals, 3)
	RBX_ENUM_VALUE(DeveloperMemoryTag, LuaHeap, 4)
	RBX_ENUM_VALUE(DeveloperMemoryTag, Script, 5)
	RBX_ENUM_VALUE(DeveloperMemoryTag, PhysicsCollision, 6)
	RBX_ENUM_VALUE(DeveloperMemoryTag, BaseParts, 7)
	RBX_ENUM_VALUE(DeveloperMemoryTag, GraphicsSolidModels, 8)
	RBX_ENUM_VALUE(DeveloperMemoryTag, GraphicsMeshParts, 10)
	RBX_ENUM_VALUE(DeveloperMemoryTag, GraphicsParticles, 11)
	RBX_ENUM_VALUE(DeveloperMemoryTag, GraphicsParts, 12)
	RBX_ENUM_VALUE(DeveloperMemoryTag, GraphicsSpatialHash, 13)
	RBX_ENUM_VALUE(DeveloperMemoryTag, GraphicsTerrain, 14)
	RBX_ENUM_VALUE(DeveloperMemoryTag, GraphicsTexture, 15)
	RBX_ENUM_VALUE(DeveloperMemoryTag, GraphicsTextureCharacter, 16)
	RBX_ENUM_VALUE(DeveloperMemoryTag, Sounds, 17)
	RBX_ENUM_VALUE(DeveloperMemoryTag, StreamingSounds, 18)
	RBX_ENUM_VALUE(DeveloperMemoryTag, TerrainVoxels, 19)
	RBX_ENUM_VALUE(DeveloperMemoryTag, Gui, 21)
	RBX_ENUM_VALUE(DeveloperMemoryTag, Animation, 22)
	RBX_ENUM_VALUE(DeveloperMemoryTag, Navigation, 23)
	RBX_ENUM_VALUE(DeveloperMemoryTag, GeometryCSG, 24)
	RBX_ENUM_VALUE(DeveloperMemoryTag, GraphicsSlimModels, 25)
RBX_ENUM_END(DeveloperMemoryTag, Internal, HttpCache, Instances, Signals, LuaHeap, Script, PhysicsCollision, BaseParts, GraphicsSolidModels, GraphicsMeshParts, GraphicsParticles, GraphicsParts, GraphicsSpatialHash, GraphicsTerrain, GraphicsTexture, GraphicsTextureCharacter, Sounds, StreamingSounds, TerrainVoxels, Gui, Animation, Navigation, GeometryCSG, GraphicsSlimModels)

RBX_ENUM_BEGIN(DeviceFeatureType)
	RBX_ENUM_VALUE(DeviceFeatureType, DeviceCapture, 0)
	RBX_ENUM_VALUE(DeviceFeatureType, InExperienceFAE, 1)
RBX_ENUM_END(DeviceFeatureType, DeviceCapture, InExperienceFAE)

RBX_ENUM_BEGIN(DeviceForm)
	RBX_ENUM_VALUE(DeviceForm, Console, 0)
	RBX_ENUM_VALUE(DeviceForm, Phone, 1)
	RBX_ENUM_VALUE(DeviceForm, Tablet, 2)
	RBX_ENUM_VALUE(DeviceForm, Desktop, 3)
	RBX_ENUM_VALUE(DeviceForm, VR, 4)
RBX_ENUM_END(DeviceForm, Console, Phone, Tablet, Desktop, VR)

RBX_ENUM_BEGIN(DeviceLevel)
	RBX_ENUM_VALUE(DeviceLevel, Low, 0)
	RBX_ENUM_VALUE(DeviceLevel, Medium, 1)
	RBX_ENUM_VALUE(DeviceLevel, High, 2)
RBX_ENUM_END(DeviceLevel, Low, Medium, High)

RBX_ENUM_BEGIN(DeviceSimulatorScalingMode)
	RBX_ENUM_VALUE(DeviceSimulatorScalingMode, ScaleToPhysicalSize, 0)
	RBX_ENUM_VALUE(DeviceSimulatorScalingMode, ActualResolution, 1)
	RBX_ENUM_VALUE(DeviceSimulatorScalingMode, FitToWindow, 2)
RBX_ENUM_END(DeviceSimulatorScalingMode, ScaleToPhysicalSize, ActualResolution, FitToWindow)

RBX_ENUM_BEGIN(DeviceType)
	RBX_ENUM_VALUE(DeviceType, Unknown, 0)
	RBX_ENUM_VALUE(DeviceType, Desktop, 1)
	RBX_ENUM_VALUE(DeviceType, Tablet, 2)
	RBX_ENUM_VALUE(DeviceType, Phone, 3)
	RBX_ENUM_VALUE(DeviceType, TV, 4)
RBX_ENUM_END(DeviceType, Unknown, Desktop, Tablet, Phone, TV)

RBX_ENUM_BEGIN(DialogBehaviorType)
	RBX_ENUM_VALUE(DialogBehaviorType, SinglePlayer, 0)
	RBX_ENUM_VALUE(DialogBehaviorType, MultiplePlayers, 1)
RBX_ENUM_END(DialogBehaviorType, SinglePlayer, MultiplePlayers)

RBX_ENUM_BEGIN(DialogPurpose)
	RBX_ENUM_VALUE(DialogPurpose, Quest, 0)
	RBX_ENUM_VALUE(DialogPurpose, Help, 1)
	RBX_ENUM_VALUE(DialogPurpose, Shop, 2)
RBX_ENUM_END(DialogPurpose, Quest, Help, Shop)

RBX_ENUM_BEGIN(DialogTone)
	RBX_ENUM_VALUE(DialogTone, Neutral, 0)
	RBX_ENUM_VALUE(DialogTone, Friendly, 1)
	RBX_ENUM_VALUE(DialogTone, Enemy, 2)
RBX_ENUM_END(DialogTone, Neutral, Friendly, Enemy)

RBX_ENUM_BEGIN(DigitsRigDescriptionSide)
	RBX_ENUM_VALUE(DigitsRigDescriptionSide, None, 0)
	RBX_ENUM_VALUE(DigitsRigDescriptionSide, Left, 1)
	RBX_ENUM_VALUE(DigitsRigDescriptionSide, Right, 2)
RBX_ENUM_END(DigitsRigDescriptionSide, None, Left, Right)

RBX_ENUM_BEGIN(DiscountType)
	RBX_ENUM_VALUE(DiscountType, Uncategorized, 0)
RBX_ENUM_END(DiscountType, Uncategorized)

RBX_ENUM_BEGIN(DisplayScalingMode)
	RBX_ENUM_VALUE(DisplayScalingMode, Default, 0)
	RBX_ENUM_VALUE(DisplayScalingMode, Legacy, 1)
	RBX_ENUM_VALUE(DisplayScalingMode, Responsive, 2)
RBX_ENUM_END(DisplayScalingMode, Default, Legacy, Responsive)

RBX_ENUM_BEGIN(DisplaySize)
	RBX_ENUM_VALUE(DisplaySize, Small, 0)
	RBX_ENUM_VALUE(DisplaySize, Medium, 1)
	RBX_ENUM_VALUE(DisplaySize, Large, 2)
RBX_ENUM_END(DisplaySize, Small, Medium, Large)

RBX_ENUM_BEGIN(DomainType)
	RBX_ENUM_VALUE(DomainType, EXPERIENCE, 1)
	RBX_ENUM_VALUE(DomainType, GROUP, 2)
	RBX_ENUM_VALUE(DomainType, OAUTH, 3)
RBX_ENUM_END(DomainType, EXPERIENCE, GROUP, OAUTH)

RBX_ENUM_BEGIN(DominantAxis)
	RBX_ENUM_VALUE(DominantAxis, Width, 0)
	RBX_ENUM_VALUE(DominantAxis, Height, 1)
RBX_ENUM_END(DominantAxis, Width, Height)

RBX_ENUM_BEGIN(DraftStatusCode)
	RBX_ENUM_VALUE(DraftStatusCode, OK, 0)
	RBX_ENUM_VALUE(DraftStatusCode, DraftOutdated, 1)
	RBX_ENUM_VALUE(DraftStatusCode, ScriptRemoved, 2)
	RBX_ENUM_VALUE(DraftStatusCode, DraftCommitted, 3)
RBX_ENUM_END(DraftStatusCode, OK, DraftOutdated, ScriptRemoved, DraftCommitted)

RBX_ENUM_BEGIN(DragDetectorDragStyle)
	RBX_ENUM_VALUE(DragDetectorDragStyle, TranslateLine, 0)
	RBX_ENUM_VALUE(DragDetectorDragStyle, TranslatePlane, 1)
	RBX_ENUM_VALUE(DragDetectorDragStyle, TranslatePlaneOrLine, 2)
	RBX_ENUM_VALUE(DragDetectorDragStyle, TranslateLineOrPlane, 3)
	RBX_ENUM_VALUE(DragDetectorDragStyle, TranslateViewPlane, 4)
	RBX_ENUM_VALUE(DragDetectorDragStyle, RotateAxis, 5)
	RBX_ENUM_VALUE(DragDetectorDragStyle, RotateTrackball, 6)
	RBX_ENUM_VALUE(DragDetectorDragStyle, Scriptable, 7)
	RBX_ENUM_VALUE(DragDetectorDragStyle, BestForDevice, 8)
RBX_ENUM_END(DragDetectorDragStyle, TranslateLine, TranslatePlane, TranslatePlaneOrLine, TranslateLineOrPlane, TranslateViewPlane, RotateAxis, RotateTrackball, Scriptable, BestForDevice)

RBX_ENUM_BEGIN(DragDetectorPermissionPolicy)
	RBX_ENUM_VALUE(DragDetectorPermissionPolicy, Nobody, 0)
	RBX_ENUM_VALUE(DragDetectorPermissionPolicy, Everybody, 1)
	RBX_ENUM_VALUE(DragDetectorPermissionPolicy, Scriptable, 2)
RBX_ENUM_END(DragDetectorPermissionPolicy, Nobody, Everybody, Scriptable)

RBX_ENUM_BEGIN(DragDetectorResponseStyle)
	RBX_ENUM_VALUE(DragDetectorResponseStyle, Geometric, 0)
	RBX_ENUM_VALUE(DragDetectorResponseStyle, Physical, 1)
	RBX_ENUM_VALUE(DragDetectorResponseStyle, Custom, 2)
RBX_ENUM_END(DragDetectorResponseStyle, Geometric, Physical, Custom)

RBX_ENUM_BEGIN(DraggerCoordinateSpace)
	RBX_ENUM_VALUE(DraggerCoordinateSpace, Object, 0)
	RBX_ENUM_VALUE(DraggerCoordinateSpace, World, 1)
RBX_ENUM_END(DraggerCoordinateSpace, Object, World)

RBX_ENUM_BEGIN(DraggerMovementMode)
	RBX_ENUM_VALUE(DraggerMovementMode, Geometric, 0)
	RBX_ENUM_VALUE(DraggerMovementMode, Physical, 1)
RBX_ENUM_END(DraggerMovementMode, Geometric, Physical)

RBX_ENUM_BEGIN(DraggingScrollBar)
	RBX_ENUM_VALUE(DraggingScrollBar, None, 0)
	RBX_ENUM_VALUE(DraggingScrollBar, Horizontal, 1)
	RBX_ENUM_VALUE(DraggingScrollBar, Vertical, 2)
RBX_ENUM_END(DraggingScrollBar, None, Horizontal, Vertical)

RBX_ENUM_BEGIN(EasingDirection)
	RBX_ENUM_VALUE(EasingDirection, In, 0)
	RBX_ENUM_VALUE(EasingDirection, Out, 1)
	RBX_ENUM_VALUE(EasingDirection, InOut, 2)
RBX_ENUM_END(EasingDirection, In, Out, InOut)

RBX_ENUM_BEGIN(EasingStyle)
	RBX_ENUM_VALUE(EasingStyle, Linear, 0)
	RBX_ENUM_VALUE(EasingStyle, Sine, 1)
	RBX_ENUM_VALUE(EasingStyle, Back, 2)
	RBX_ENUM_VALUE(EasingStyle, Quad, 3)
	RBX_ENUM_VALUE(EasingStyle, Quart, 4)
	RBX_ENUM_VALUE(EasingStyle, Quint, 5)
	RBX_ENUM_VALUE(EasingStyle, Bounce, 6)
	RBX_ENUM_VALUE(EasingStyle, Elastic, 7)
	RBX_ENUM_VALUE(EasingStyle, Exponential, 8)
	RBX_ENUM_VALUE(EasingStyle, Circular, 9)
	RBX_ENUM_VALUE(EasingStyle, Cubic, 10)
RBX_ENUM_END(EasingStyle, Linear, Sine, Back, Quad, Quart, Quint, Bounce, Elastic, Exponential, Circular, Cubic)

RBX_ENUM_BEGIN(EditableStatus)
	RBX_ENUM_VALUE(EditableStatus, Unknown, 0)
	RBX_ENUM_VALUE(EditableStatus, Allowed, 1)
	RBX_ENUM_VALUE(EditableStatus, Disallowed, 2)
RBX_ENUM_END(EditableStatus, Unknown, Allowed, Disallowed)

RBX_ENUM_BEGIN(ElasticBehavior)
	RBX_ENUM_VALUE(ElasticBehavior, WhenScrollable, 0)
	RBX_ENUM_VALUE(ElasticBehavior, Always, 1)
	RBX_ENUM_VALUE(ElasticBehavior, Never, 2)
RBX_ENUM_END(ElasticBehavior, WhenScrollable, Always, Never)

RBX_ENUM_BEGIN(EngineFolder)
	RBX_ENUM_VALUE(EngineFolder, Screenshots, 0)
	RBX_ENUM_VALUE(EngineFolder, Videos, 1)
	RBX_ENUM_VALUE(EngineFolder, Logs, 2)
RBX_ENUM_END(EngineFolder, Screenshots, Videos, Logs)

RBX_ENUM_BEGIN(EnviromentalPhysicsThrottle)
	RBX_ENUM_VALUE(EnviromentalPhysicsThrottle, DefaultAuto, 0)
	RBX_ENUM_VALUE(EnviromentalPhysicsThrottle, Disabled, 1)
	RBX_ENUM_VALUE(EnviromentalPhysicsThrottle, Always, 2)
	RBX_ENUM_VALUE(EnviromentalPhysicsThrottle, Skip2, 3)
	RBX_ENUM_VALUE(EnviromentalPhysicsThrottle, Skip4, 4)
	RBX_ENUM_VALUE(EnviromentalPhysicsThrottle, Skip8, 5)
	RBX_ENUM_VALUE(EnviromentalPhysicsThrottle, Skip16, 6)
RBX_ENUM_END(EnviromentalPhysicsThrottle, DefaultAuto, Disabled, Always, Skip2, Skip4, Skip8, Skip16)

RBX_ENUM_BEGIN(ExperienceAuthScope)
	RBX_ENUM_VALUE(ExperienceAuthScope, DefaultScope, 0)
	RBX_ENUM_VALUE(ExperienceAuthScope, CreatorAssetsCreate, 1)
RBX_ENUM_END(ExperienceAuthScope, DefaultScope, CreatorAssetsCreate)

RBX_ENUM_BEGIN(ExperienceEventStatus)
	RBX_ENUM_VALUE(ExperienceEventStatus, Active, 0)
	RBX_ENUM_VALUE(ExperienceEventStatus, Cancelled, 1)
	RBX_ENUM_VALUE(ExperienceEventStatus, Moderated, 2)
	RBX_ENUM_VALUE(ExperienceEventStatus, Unpublished, 3)
	RBX_ENUM_VALUE(ExperienceEventStatus, Unknown, 4)
RBX_ENUM_END(ExperienceEventStatus, Active, Cancelled, Moderated, Unpublished, Unknown)

RBX_ENUM_BEGIN(ExperienceStateCaptureSelectionMode)
	RBX_ENUM_VALUE(ExperienceStateCaptureSelectionMode, Default, 0)
	RBX_ENUM_VALUE(ExperienceStateCaptureSelectionMode, SafetyHighlightMode, 1)
RBX_ENUM_END(ExperienceStateCaptureSelectionMode, Default, SafetyHighlightMode)

RBX_ENUM_BEGIN(ExperienceStateRecordingLoadMode)
	RBX_ENUM_VALUE(ExperienceStateRecordingLoadMode, NewReplay, 0)
	RBX_ENUM_VALUE(ExperienceStateRecordingLoadMode, ContiguousSlice, 1)
	RBX_ENUM_VALUE(ExperienceStateRecordingLoadMode, NoncontiguousSlice, 2)
RBX_ENUM_END(ExperienceStateRecordingLoadMode, NewReplay, ContiguousSlice, NoncontiguousSlice)

RBX_ENUM_BEGIN(ExperienceStateRecordingLoadSourceType)
	RBX_ENUM_VALUE(ExperienceStateRecordingLoadSourceType, S3Url, 0)
	RBX_ENUM_VALUE(ExperienceStateRecordingLoadSourceType, File, 1)
RBX_ENUM_END(ExperienceStateRecordingLoadSourceType, S3Url, File)

RBX_ENUM_BEGIN(ExperienceStateRecordingPlaybackMode)
	RBX_ENUM_VALUE(ExperienceStateRecordingPlaybackMode, Undefined, 0)
	RBX_ENUM_VALUE(ExperienceStateRecordingPlaybackMode, Stopped, 1)
	RBX_ENUM_VALUE(ExperienceStateRecordingPlaybackMode, Playing, 2)
	RBX_ENUM_VALUE(ExperienceStateRecordingPlaybackMode, Rewinding, 3)
RBX_ENUM_END(ExperienceStateRecordingPlaybackMode, Undefined, Stopped, Playing, Rewinding)

RBX_ENUM_BEGIN(ExplosionType)
	RBX_ENUM_VALUE(ExplosionType, NoCraters, 0)
	RBX_ENUM_VALUE(ExplosionType, Craters, 1)
RBX_ENUM_END(ExplosionType, NoCraters, Craters)

RBX_ENUM_BEGIN(ExternalEditorMode)
	RBX_ENUM_VALUE(ExternalEditorMode, SystemDefault, 0)
	RBX_ENUM_VALUE(ExternalEditorMode, UserSelectedEditor, 1)
RBX_ENUM_END(ExternalEditorMode, SystemDefault, UserSelectedEditor)

RBX_ENUM_BEGIN(FACSDataLod)
	RBX_ENUM_VALUE(FACSDataLod, LOD0, 0)
	RBX_ENUM_VALUE(FACSDataLod, LOD1, 1)
	RBX_ENUM_VALUE(FACSDataLod, LODCount, 2)
RBX_ENUM_END(FACSDataLod, LOD0, LOD1, LODCount)

RBX_ENUM_BEGIN(FacialAgeEstimationResultType)
	RBX_ENUM_VALUE(FacialAgeEstimationResultType, Complete, 0)
	RBX_ENUM_VALUE(FacialAgeEstimationResultType, Cancel, 1)
	RBX_ENUM_VALUE(FacialAgeEstimationResultType, Error, 2)
RBX_ENUM_END(FacialAgeEstimationResultType, Complete, Cancel, Error)

RBX_ENUM_BEGIN(FacialAnimationStreamingState)
	RBX_ENUM_VALUE(FacialAnimationStreamingState, None, 0)
	RBX_ENUM_VALUE(FacialAnimationStreamingState, Audio, 1)
	RBX_ENUM_VALUE(FacialAnimationStreamingState, Video, 2)
	RBX_ENUM_VALUE(FacialAnimationStreamingState, Place, 4)
	RBX_ENUM_VALUE(FacialAnimationStreamingState, Server, 8)
RBX_ENUM_END(FacialAnimationStreamingState, None, Audio, Video, Place, Server)

RBX_ENUM_BEGIN(FacsActionUnit)
	RBX_ENUM_VALUE(FacsActionUnit, ChinRaiserUpperLip, 0)
	RBX_ENUM_VALUE(FacsActionUnit, ChinRaiser, 1)
	RBX_ENUM_VALUE(FacsActionUnit, FlatPucker, 2)
	RBX_ENUM_VALUE(FacsActionUnit, Funneler, 3)
	RBX_ENUM_VALUE(FacsActionUnit, LowerLipSuck, 4)
	RBX_ENUM_VALUE(FacsActionUnit, LipPresser, 5)
	RBX_ENUM_VALUE(FacsActionUnit, LipsTogether, 6)
	RBX_ENUM_VALUE(FacsActionUnit, MouthLeft, 7)
	RBX_ENUM_VALUE(FacsActionUnit, MouthRight, 8)
	RBX_ENUM_VALUE(FacsActionUnit, Pucker, 9)
	RBX_ENUM_VALUE(FacsActionUnit, UpperLipSuck, 10)
	RBX_ENUM_VALUE(FacsActionUnit, LeftCheekPuff, 11)
	RBX_ENUM_VALUE(FacsActionUnit, LeftDimpler, 12)
	RBX_ENUM_VALUE(FacsActionUnit, LeftLipCornerDown, 13)
	RBX_ENUM_VALUE(FacsActionUnit, LeftLowerLipDepressor, 14)
	RBX_ENUM_VALUE(FacsActionUnit, LeftLipCornerPuller, 15)
	RBX_ENUM_VALUE(FacsActionUnit, LeftLipStretcher, 16)
	RBX_ENUM_VALUE(FacsActionUnit, LeftUpperLipRaiser, 17)
	RBX_ENUM_VALUE(FacsActionUnit, RightCheekPuff, 18)
	RBX_ENUM_VALUE(FacsActionUnit, RightDimpler, 19)
	RBX_ENUM_VALUE(FacsActionUnit, RightLipCornerDown, 20)
	RBX_ENUM_VALUE(FacsActionUnit, RightLowerLipDepressor, 21)
	RBX_ENUM_VALUE(FacsActionUnit, RightLipCornerPuller, 22)
	RBX_ENUM_VALUE(FacsActionUnit, RightLipStretcher, 23)
	RBX_ENUM_VALUE(FacsActionUnit, RightUpperLipRaiser, 24)
	RBX_ENUM_VALUE(FacsActionUnit, JawDrop, 25)
	RBX_ENUM_VALUE(FacsActionUnit, JawLeft, 26)
	RBX_ENUM_VALUE(FacsActionUnit, JawRight, 27)
	RBX_ENUM_VALUE(FacsActionUnit, Corrugator, 28)
	RBX_ENUM_VALUE(FacsActionUnit, LeftBrowLowerer, 29)
	RBX_ENUM_VALUE(FacsActionUnit, LeftOuterBrowRaiser, 30)
	RBX_ENUM_VALUE(FacsActionUnit, LeftNoseWrinkler, 31)
	RBX_ENUM_VALUE(FacsActionUnit, LeftInnerBrowRaiser, 32)
	RBX_ENUM_VALUE(FacsActionUnit, RightBrowLowerer, 33)
	RBX_ENUM_VALUE(FacsActionUnit, RightOuterBrowRaiser, 34)
	RBX_ENUM_VALUE(FacsActionUnit, RightInnerBrowRaiser, 35)
	RBX_ENUM_VALUE(FacsActionUnit, RightNoseWrinkler, 36)
	RBX_ENUM_VALUE(FacsActionUnit, EyesLookDown, 37)
	RBX_ENUM_VALUE(FacsActionUnit, EyesLookLeft, 38)
	RBX_ENUM_VALUE(FacsActionUnit, EyesLookUp, 39)
	RBX_ENUM_VALUE(FacsActionUnit, EyesLookRight, 40)
	RBX_ENUM_VALUE(FacsActionUnit, LeftCheekRaiser, 41)
	RBX_ENUM_VALUE(FacsActionUnit, LeftEyeUpperLidRaiser, 42)
	RBX_ENUM_VALUE(FacsActionUnit, LeftEyeClosed, 43)
	RBX_ENUM_VALUE(FacsActionUnit, RightCheekRaiser, 44)
	RBX_ENUM_VALUE(FacsActionUnit, RightEyeUpperLidRaiser, 45)
	RBX_ENUM_VALUE(FacsActionUnit, RightEyeClosed, 46)
	RBX_ENUM_VALUE(FacsActionUnit, TongueDown, 47)
	RBX_ENUM_VALUE(FacsActionUnit, TongueOut, 48)
	RBX_ENUM_VALUE(FacsActionUnit, TongueUp, 49)
RBX_ENUM_END(FacsActionUnit, ChinRaiserUpperLip, ChinRaiser, FlatPucker, Funneler, LowerLipSuck, LipPresser, LipsTogether, MouthLeft, MouthRight, Pucker, UpperLipSuck, LeftCheekPuff, LeftDimpler, LeftLipCornerDown, LeftLowerLipDepressor, LeftLipCornerPuller, LeftLipStretcher, LeftUpperLipRaiser, RightCheekPuff, RightDimpler, RightLipCornerDown, RightLowerLipDepressor, RightLipCornerPuller, RightLipStretcher, RightUpperLipRaiser, JawDrop, JawLeft, JawRight, Corrugator, LeftBrowLowerer, LeftOuterBrowRaiser, LeftNoseWrinkler, LeftInnerBrowRaiser, RightBrowLowerer, RightOuterBrowRaiser, RightInnerBrowRaiser, RightNoseWrinkler, EyesLookDown, EyesLookLeft, EyesLookUp, EyesLookRight, LeftCheekRaiser, LeftEyeUpperLidRaiser, LeftEyeClosed, RightCheekRaiser, RightEyeUpperLidRaiser, RightEyeClosed, TongueDown, TongueOut, TongueUp)

RBX_ENUM_BEGIN(FeatureRestrictionAbuseVector)
	RBX_ENUM_VALUE(FeatureRestrictionAbuseVector, ExperienceChat, 0)
	RBX_ENUM_VALUE(FeatureRestrictionAbuseVector, Communication, 1)
RBX_ENUM_END(FeatureRestrictionAbuseVector, ExperienceChat, Communication)

RBX_ENUM_BEGIN(FieldOfViewMode)
	RBX_ENUM_VALUE(FieldOfViewMode, Vertical, 0)
	RBX_ENUM_VALUE(FieldOfViewMode, Diagonal, 1)
	RBX_ENUM_VALUE(FieldOfViewMode, MaxAxis, 2)
RBX_ENUM_END(FieldOfViewMode, Vertical, Diagonal, MaxAxis)

RBX_ENUM_BEGIN(FillDirection)
	RBX_ENUM_VALUE(FillDirection, Horizontal, 0)
	RBX_ENUM_VALUE(FillDirection, Vertical, 1)
RBX_ENUM_END(FillDirection, Horizontal, Vertical)

RBX_ENUM_BEGIN(FilterErrorType)
	RBX_ENUM_VALUE(FilterErrorType, BackslashNotEscapingAnything, 0)
	RBX_ENUM_VALUE(FilterErrorType, BadBespokeFilter, 1)
	RBX_ENUM_VALUE(FilterErrorType, BadName, 2)
	RBX_ENUM_VALUE(FilterErrorType, IncompleteOr, 3)
	RBX_ENUM_VALUE(FilterErrorType, IncompleteParenthesis, 4)
	RBX_ENUM_VALUE(FilterErrorType, InvalidDoubleStar, 5)
	RBX_ENUM_VALUE(FilterErrorType, InvalidTilde, 6)
	RBX_ENUM_VALUE(FilterErrorType, PropertyBadOperator, 7)
	RBX_ENUM_VALUE(FilterErrorType, PropertyDoesNotExist, 8)
	RBX_ENUM_VALUE(FilterErrorType, PropertyInvalidField, 9)
	RBX_ENUM_VALUE(FilterErrorType, PropertyInvalidValue, 10)
	RBX_ENUM_VALUE(FilterErrorType, PropertyUnsupportedFields, 11)
	RBX_ENUM_VALUE(FilterErrorType, PropertyUnsupportedProperty, 12)
	RBX_ENUM_VALUE(FilterErrorType, UnexpectedNameIndex, 13)
	RBX_ENUM_VALUE(FilterErrorType, UnexpectedToken, 14)
	RBX_ENUM_VALUE(FilterErrorType, UnfinishedBinaryOperator, 15)
	RBX_ENUM_VALUE(FilterErrorType, UnfinishedQuote, 16)
	RBX_ENUM_VALUE(FilterErrorType, UnknownBespokeFilter, 17)
	RBX_ENUM_VALUE(FilterErrorType, WildcardInProperty, 18)
RBX_ENUM_END(FilterErrorType, BackslashNotEscapingAnything, BadBespokeFilter, BadName, IncompleteOr, IncompleteParenthesis, InvalidDoubleStar, InvalidTilde, PropertyBadOperator, PropertyDoesNotExist, PropertyInvalidField, PropertyInvalidValue, PropertyUnsupportedFields, PropertyUnsupportedProperty, UnexpectedNameIndex, UnexpectedToken, UnfinishedBinaryOperator, UnfinishedQuote, UnknownBespokeFilter, WildcardInProperty)

RBX_ENUM_BEGIN(FilterResult)
	RBX_ENUM_VALUE(FilterResult, Accepted, 0)
	RBX_ENUM_VALUE(FilterResult, Rejected, 1)
RBX_ENUM_END(FilterResult, Accepted, Rejected)

RBX_ENUM_BEGIN(FilterType)
	RBX_ENUM_VALUE(FilterType, Exclude, 0)
	RBX_ENUM_VALUE(FilterType, Include, 1)
RBX_ENUM_END(FilterType, Exclude, Include)

RBX_ENUM_BEGIN(FinishRecordingOperation)
	RBX_ENUM_VALUE(FinishRecordingOperation, Cancel, 0)
	RBX_ENUM_VALUE(FinishRecordingOperation, Commit, 1)
	RBX_ENUM_VALUE(FinishRecordingOperation, Append, 2)
RBX_ENUM_END(FinishRecordingOperation, Cancel, Commit, Append)

RBX_ENUM_BEGIN(FluidFidelity)
	RBX_ENUM_VALUE(FluidFidelity, Automatic, 0)
	RBX_ENUM_VALUE(FluidFidelity, UseCollisionGeometry, 1)
	RBX_ENUM_VALUE(FluidFidelity, UsePreciseGeometry, 2)
RBX_ENUM_END(FluidFidelity, Automatic, UseCollisionGeometry, UsePreciseGeometry)

RBX_ENUM_BEGIN(FluidForces)
	RBX_ENUM_VALUE(FluidForces, Default, 0)
	RBX_ENUM_VALUE(FluidForces, Experimental, 1)
RBX_ENUM_END(FluidForces, Default, Experimental)

RBX_ENUM_BEGIN(Font)
	RBX_ENUM_VALUE(Font, Legacy, 0)
	RBX_ENUM_VALUE(Font, Arial, 1)
	RBX_ENUM_VALUE(Font, ArialBold, 2)
	RBX_ENUM_VALUE(Font, SourceSans, 3)
	RBX_ENUM_VALUE(Font, SourceSansBold, 4)
	RBX_ENUM_VALUE(Font, SourceSansLight, 5)
	RBX_ENUM_VALUE(Font, SourceSansItalic, 6)
	RBX_ENUM_VALUE(Font, Bodoni, 7)
	RBX_ENUM_VALUE(Font, Garamond, 8)
	RBX_ENUM_VALUE(Font, Cartoon, 9)
	RBX_ENUM_VALUE(Font, Code, 10)
	RBX_ENUM_VALUE(Font, Highway, 11)
	RBX_ENUM_VALUE(Font, SciFi, 12)
	RBX_ENUM_VALUE(Font, Arcade, 13)
	RBX_ENUM_VALUE(Font, Fantasy, 14)
	RBX_ENUM_VALUE(Font, Antique, 15)
	RBX_ENUM_VALUE(Font, SourceSansSemibold, 16)
	RBX_ENUM_VALUE(Font, Gotham, 17)
	RBX_ENUM_VALUE(Font, GothamMedium, 18)
	RBX_ENUM_VALUE(Font, GothamBold, 19)
	RBX_ENUM_VALUE(Font, GothamBlack, 20)
	RBX_ENUM_VALUE(Font, AmaticSC, 21)
	RBX_ENUM_VALUE(Font, Bangers, 22)
	RBX_ENUM_VALUE(Font, Creepster, 23)
	RBX_ENUM_VALUE(Font, DenkOne, 24)
	RBX_ENUM_VALUE(Font, Fondamento, 25)
	RBX_ENUM_VALUE(Font, FredokaOne, 26)
	RBX_ENUM_VALUE(Font, GrenzeGotisch, 27)
	RBX_ENUM_VALUE(Font, IndieFlower, 28)
	RBX_ENUM_VALUE(Font, JosefinSans, 29)
	RBX_ENUM_VALUE(Font, Jura, 30)
	RBX_ENUM_VALUE(Font, Kalam, 31)
	RBX_ENUM_VALUE(Font, LuckiestGuy, 32)
	RBX_ENUM_VALUE(Font, Merriweather, 33)
	RBX_ENUM_VALUE(Font, Michroma, 34)
	RBX_ENUM_VALUE(Font, Nunito, 35)
	RBX_ENUM_VALUE(Font, Oswald, 36)
	RBX_ENUM_VALUE(Font, PatrickHand, 37)
	RBX_ENUM_VALUE(Font, PermanentMarker, 38)
	RBX_ENUM_VALUE(Font, Roboto, 39)
	RBX_ENUM_VALUE(Font, RobotoCondensed, 40)
	RBX_ENUM_VALUE(Font, RobotoMono, 41)
	RBX_ENUM_VALUE(Font, Sarpanch, 42)
	RBX_ENUM_VALUE(Font, SpecialElite, 43)
	RBX_ENUM_VALUE(Font, TitilliumWeb, 44)
	RBX_ENUM_VALUE(Font, Ubuntu, 45)
	RBX_ENUM_VALUE(Font, BuilderSans, 46)
	RBX_ENUM_VALUE(Font, BuilderSansMedium, 47)
	RBX_ENUM_VALUE(Font, BuilderSansBold, 48)
	RBX_ENUM_VALUE(Font, BuilderSansExtraBold, 49)
	RBX_ENUM_VALUE(Font, Arimo, 50)
	RBX_ENUM_VALUE(Font, ArimoBold, 51)
	RBX_ENUM_VALUE(Font, Unknown, 100)
RBX_ENUM_END(Font, Legacy, Arial, ArialBold, SourceSans, SourceSansBold, SourceSansLight, SourceSansItalic, Bodoni, Garamond, Cartoon, Code, Highway, SciFi, Arcade, Fantasy, Antique, SourceSansSemibold, Gotham, GothamMedium, GothamBold, GothamBlack, AmaticSC, Bangers, Creepster, DenkOne, Fondamento, FredokaOne, GrenzeGotisch, IndieFlower, JosefinSans, Jura, Kalam, LuckiestGuy, Merriweather, Michroma, Nunito, Oswald, PatrickHand, PermanentMarker, Roboto, RobotoCondensed, RobotoMono, Sarpanch, SpecialElite, TitilliumWeb, Ubuntu, BuilderSans, BuilderSansMedium, BuilderSansBold, BuilderSansExtraBold, Arimo, ArimoBold, Unknown)

RBX_ENUM_BEGIN(FontSize)
	RBX_ENUM_VALUE(FontSize, Size8, 0)
	RBX_ENUM_VALUE(FontSize, Size9, 1)
	RBX_ENUM_VALUE(FontSize, Size10, 2)
	RBX_ENUM_VALUE(FontSize, Size11, 3)
	RBX_ENUM_VALUE(FontSize, Size12, 4)
	RBX_ENUM_VALUE(FontSize, Size14, 5)
	RBX_ENUM_VALUE(FontSize, Size18, 6)
	RBX_ENUM_VALUE(FontSize, Size24, 7)
	RBX_ENUM_VALUE(FontSize, Size36, 8)
	RBX_ENUM_VALUE(FontSize, Size48, 9)
	RBX_ENUM_VALUE(FontSize, Size28, 10)
	RBX_ENUM_VALUE(FontSize, Size32, 11)
	RBX_ENUM_VALUE(FontSize, Size42, 12)
	RBX_ENUM_VALUE(FontSize, Size60, 13)
	RBX_ENUM_VALUE(FontSize, Size96, 14)
RBX_ENUM_END(FontSize, Size8, Size9, Size10, Size11, Size12, Size14, Size18, Size24, Size36, Size48, Size28, Size32, Size42, Size60, Size96)

RBX_ENUM_BEGIN(FontStyle)
	RBX_ENUM_VALUE(FontStyle, Normal, 0)
	RBX_ENUM_VALUE(FontStyle, Italic, 1)
RBX_ENUM_END(FontStyle, Normal, Italic)

RBX_ENUM_BEGIN(FontWeight)
	RBX_ENUM_VALUE(FontWeight, Thin, 100)
	RBX_ENUM_VALUE(FontWeight, ExtraLight, 200)
	RBX_ENUM_VALUE(FontWeight, Light, 300)
	RBX_ENUM_VALUE(FontWeight, Regular, 400)
	RBX_ENUM_VALUE(FontWeight, Medium, 500)
	RBX_ENUM_VALUE(FontWeight, SemiBold, 600)
	RBX_ENUM_VALUE(FontWeight, Bold, 700)
	RBX_ENUM_VALUE(FontWeight, ExtraBold, 800)
	RBX_ENUM_VALUE(FontWeight, Heavy, 900)
RBX_ENUM_END(FontWeight, Thin, ExtraLight, Light, Regular, Medium, SemiBold, Bold, ExtraBold, Heavy)

RBX_ENUM_BEGIN(ForceLimitMode)
	RBX_ENUM_VALUE(ForceLimitMode, Magnitude, 0)
	RBX_ENUM_VALUE(ForceLimitMode, PerAxis, 1)
RBX_ENUM_END(ForceLimitMode, Magnitude, PerAxis)

RBX_ENUM_BEGIN(FormFactor)
	RBX_ENUM_VALUE(FormFactor, Symmetric, 0)
	RBX_ENUM_VALUE(FormFactor, Brick, 1)
	RBX_ENUM_VALUE(FormFactor, Plate, 2)
	RBX_ENUM_VALUE(FormFactor, Custom, 3)
RBX_ENUM_END(FormFactor, Symmetric, Brick, Plate, Custom)

RBX_ENUM_BEGIN(FrameStyle)
	RBX_ENUM_VALUE(FrameStyle, Custom, 0)
	RBX_ENUM_VALUE(FrameStyle, ChatBlue, 1)
	RBX_ENUM_VALUE(FrameStyle, RobloxSquare, 2)
	RBX_ENUM_VALUE(FrameStyle, RobloxRound, 3)
	RBX_ENUM_VALUE(FrameStyle, ChatGreen, 4)
	RBX_ENUM_VALUE(FrameStyle, ChatRed, 5)
	RBX_ENUM_VALUE(FrameStyle, DropShadow, 6)
RBX_ENUM_END(FrameStyle, Custom, ChatBlue, RobloxSquare, RobloxRound, ChatGreen, ChatRed, DropShadow)

RBX_ENUM_BEGIN(FramerateManagerMode)
	RBX_ENUM_VALUE(FramerateManagerMode, Automatic, 0)
	RBX_ENUM_VALUE(FramerateManagerMode, On, 1)
	RBX_ENUM_VALUE(FramerateManagerMode, Off, 2)
RBX_ENUM_END(FramerateManagerMode, Automatic, On, Off)

RBX_ENUM_BEGIN(FriendRequestEvent)
	RBX_ENUM_VALUE(FriendRequestEvent, Issue, 0)
	RBX_ENUM_VALUE(FriendRequestEvent, Revoke, 1)
	RBX_ENUM_VALUE(FriendRequestEvent, Accept, 2)
	RBX_ENUM_VALUE(FriendRequestEvent, Deny, 3)
RBX_ENUM_END(FriendRequestEvent, Issue, Revoke, Accept, Deny)

RBX_ENUM_BEGIN(FriendStatus)
	RBX_ENUM_VALUE(FriendStatus, Unknown, 0)
	RBX_ENUM_VALUE(FriendStatus, NotFriend, 1)
	RBX_ENUM_VALUE(FriendStatus, Friend, 2)
	RBX_ENUM_VALUE(FriendStatus, FriendRequestSent, 3)
	RBX_ENUM_VALUE(FriendStatus, FriendRequestReceived, 4)
RBX_ENUM_END(FriendStatus, Unknown, NotFriend, Friend, FriendRequestSent, FriendRequestReceived)

RBX_ENUM_BEGIN(FunctionalTestResult)
	RBX_ENUM_VALUE(FunctionalTestResult, Passed, 0)
	RBX_ENUM_VALUE(FunctionalTestResult, Warning, 1)
	RBX_ENUM_VALUE(FunctionalTestResult, Error, 2)
RBX_ENUM_END(FunctionalTestResult, Passed, Warning, Error)

RBX_ENUM_BEGIN(GameAvatarType)
	RBX_ENUM_VALUE(GameAvatarType, R6, 0)
	RBX_ENUM_VALUE(GameAvatarType, R15, 1)
	RBX_ENUM_VALUE(GameAvatarType, PlayerChoice, 2)
RBX_ENUM_END(GameAvatarType, R6, R15, PlayerChoice)

RBX_ENUM_BEGIN(GamepadType)
	RBX_ENUM_VALUE(GamepadType, Unknown, 0)
	RBX_ENUM_VALUE(GamepadType, PS4, 1)
	RBX_ENUM_VALUE(GamepadType, PS5, 2)
	RBX_ENUM_VALUE(GamepadType, XboxOne, 3)
RBX_ENUM_END(GamepadType, Unknown, PS4, PS5, XboxOne)

RBX_ENUM_BEGIN(GearGenreSetting)
	RBX_ENUM_VALUE(GearGenreSetting, AllGenres, 0)
	RBX_ENUM_VALUE(GearGenreSetting, MatchingGenreOnly, 1)
RBX_ENUM_END(GearGenreSetting, AllGenres, MatchingGenreOnly)

RBX_ENUM_BEGIN(GearType)
	RBX_ENUM_VALUE(GearType, MeleeWeapons, 0)
	RBX_ENUM_VALUE(GearType, RangedWeapons, 1)
	RBX_ENUM_VALUE(GearType, Explosives, 2)
	RBX_ENUM_VALUE(GearType, PowerUps, 3)
	RBX_ENUM_VALUE(GearType, NavigationEnhancers, 4)
	RBX_ENUM_VALUE(GearType, MusicalInstruments, 5)
	RBX_ENUM_VALUE(GearType, SocialItems, 6)
	RBX_ENUM_VALUE(GearType, BuildingTools, 7)
	RBX_ENUM_VALUE(GearType, Transport, 8)
RBX_ENUM_END(GearType, MeleeWeapons, RangedWeapons, Explosives, PowerUps, NavigationEnhancers, MusicalInstruments, SocialItems, BuildingTools, Transport)

RBX_ENUM_BEGIN(Genre)
	RBX_ENUM_VALUE(Genre, All, 0)
	RBX_ENUM_VALUE(Genre, TownAndCity, 1)
	RBX_ENUM_VALUE(Genre, Fantasy, 2)
	RBX_ENUM_VALUE(Genre, SciFi, 3)
	RBX_ENUM_VALUE(Genre, Ninja, 4)
	RBX_ENUM_VALUE(Genre, Scary, 5)
	RBX_ENUM_VALUE(Genre, Pirate, 6)
	RBX_ENUM_VALUE(Genre, Adventure, 7)
	RBX_ENUM_VALUE(Genre, Sports, 8)
	RBX_ENUM_VALUE(Genre, Funny, 9)
	RBX_ENUM_VALUE(Genre, WildWest, 10)
	RBX_ENUM_VALUE(Genre, War, 11)
	RBX_ENUM_VALUE(Genre, SkatePark, 12)
	RBX_ENUM_VALUE(Genre, Tutorial, 13)
RBX_ENUM_END(Genre, All, TownAndCity, Fantasy, SciFi, Ninja, Scary, Pirate, Adventure, Sports, Funny, WildWest, War, SkatePark, Tutorial)

RBX_ENUM_BEGIN(GraphicsMode)
	RBX_ENUM_VALUE(GraphicsMode, Automatic, 1)
	RBX_ENUM_VALUE(GraphicsMode, Direct3D11, 2)
	RBX_ENUM_VALUE(GraphicsMode, OpenGL, 4)
	RBX_ENUM_VALUE(GraphicsMode, Metal, 5)
	RBX_ENUM_VALUE(GraphicsMode, Vulkan, 6)
	RBX_ENUM_VALUE(GraphicsMode, NoGraphics, 9)
RBX_ENUM_END(GraphicsMode, Automatic, Direct3D11, OpenGL, Metal, Vulkan, NoGraphics)

RBX_ENUM_BEGIN(GraphicsOptimizationMode)
	RBX_ENUM_VALUE(GraphicsOptimizationMode, Performance, 0)
	RBX_ENUM_VALUE(GraphicsOptimizationMode, Balanced, 1)
	RBX_ENUM_VALUE(GraphicsOptimizationMode, Quality, 2)
RBX_ENUM_END(GraphicsOptimizationMode, Performance, Balanced, Quality)

RBX_ENUM_BEGIN(GroupMembershipStatus)
	RBX_ENUM_VALUE(GroupMembershipStatus, None, 0)
	RBX_ENUM_VALUE(GroupMembershipStatus, Joined, 1)
	RBX_ENUM_VALUE(GroupMembershipStatus, JoinRequestPending, 2)
	RBX_ENUM_VALUE(GroupMembershipStatus, AlreadyMember, 3)
RBX_ENUM_END(GroupMembershipStatus, None, Joined, JoinRequestPending, AlreadyMember)

RBX_ENUM_BEGIN(GuiState)
	RBX_ENUM_VALUE(GuiState, Idle, 0)
	RBX_ENUM_VALUE(GuiState, Hover, 1)
	RBX_ENUM_VALUE(GuiState, Press, 2)
	RBX_ENUM_VALUE(GuiState, NonInteractable, 3)
RBX_ENUM_END(GuiState, Idle, Hover, Press, NonInteractable)

RBX_ENUM_BEGIN(GuiType)
	RBX_ENUM_VALUE(GuiType, Core, 0)
	RBX_ENUM_VALUE(GuiType, Custom, 1)
	RBX_ENUM_VALUE(GuiType, PlayerNameplates, 2)
	RBX_ENUM_VALUE(GuiType, CustomBillboards, 3)
	RBX_ENUM_VALUE(GuiType, CoreBillboards, 4)
RBX_ENUM_END(GuiType, Core, Custom, PlayerNameplates, CustomBillboards, CoreBillboards)

RBX_ENUM_BEGIN(HandlesStyle)
	RBX_ENUM_VALUE(HandlesStyle, Resize, 0)
	RBX_ENUM_VALUE(HandlesStyle, Movement, 1)
RBX_ENUM_END(HandlesStyle, Resize, Movement)

RBX_ENUM_BEGIN(HapticEffectType)
	RBX_ENUM_VALUE(HapticEffectType, Custom, 0)
	RBX_ENUM_VALUE(HapticEffectType, UIHover, 1)
	RBX_ENUM_VALUE(HapticEffectType, UIClick, 2)
	RBX_ENUM_VALUE(HapticEffectType, UINotification, 3)
	RBX_ENUM_VALUE(HapticEffectType, GameplayExplosion, 4)
	RBX_ENUM_VALUE(HapticEffectType, GameplayCollision, 5)
RBX_ENUM_END(HapticEffectType, Custom, UIHover, UIClick, UINotification, GameplayExplosion, GameplayCollision)

RBX_ENUM_BEGIN(HashAlgorithm)
	RBX_ENUM_VALUE(HashAlgorithm, Blake2b, 0)
	RBX_ENUM_VALUE(HashAlgorithm, Blake3, 1)
	RBX_ENUM_VALUE(HashAlgorithm, Md5, 2)
	RBX_ENUM_VALUE(HashAlgorithm, Sha1, 3)
	RBX_ENUM_VALUE(HashAlgorithm, Sha256, 4)
RBX_ENUM_END(HashAlgorithm, Blake2b, Blake3, Md5, Sha1, Sha256)

RBX_ENUM_BEGIN(HighlightDepthMode)
	RBX_ENUM_VALUE(HighlightDepthMode, AlwaysOnTop, 0)
	RBX_ENUM_VALUE(HighlightDepthMode, Occluded, 1)
RBX_ENUM_END(HighlightDepthMode, AlwaysOnTop, Occluded)

RBX_ENUM_BEGIN(HorizontalAlignment)
	RBX_ENUM_VALUE(HorizontalAlignment, Center, 0)
	RBX_ENUM_VALUE(HorizontalAlignment, Left, 1)
	RBX_ENUM_VALUE(HorizontalAlignment, Right, 2)
RBX_ENUM_END(HorizontalAlignment, Center, Left, Right)

RBX_ENUM_BEGIN(HoverAnimateSpeed)
	RBX_ENUM_VALUE(HoverAnimateSpeed, VerySlow, 0)
	RBX_ENUM_VALUE(HoverAnimateSpeed, Slow, 1)
	RBX_ENUM_VALUE(HoverAnimateSpeed, Medium, 2)
	RBX_ENUM_VALUE(HoverAnimateSpeed, Fast, 3)
	RBX_ENUM_VALUE(HoverAnimateSpeed, VeryFast, 4)
RBX_ENUM_END(HoverAnimateSpeed, VerySlow, Slow, Medium, Fast, VeryFast)

RBX_ENUM_BEGIN(HttpCachePolicy)
	RBX_ENUM_VALUE(HttpCachePolicy, None, 0)
	RBX_ENUM_VALUE(HttpCachePolicy, Full, 1)
	RBX_ENUM_VALUE(HttpCachePolicy, DataOnly, 2)
	RBX_ENUM_VALUE(HttpCachePolicy, Default, 3)
	RBX_ENUM_VALUE(HttpCachePolicy, InternalRedirectRefresh, 4)
RBX_ENUM_END(HttpCachePolicy, None, Full, DataOnly, Default, InternalRedirectRefresh)

RBX_ENUM_BEGIN(HttpCompression)
	RBX_ENUM_VALUE(HttpCompression, None, 0)
	RBX_ENUM_VALUE(HttpCompression, Gzip, 1)
RBX_ENUM_END(HttpCompression, None, Gzip)

RBX_ENUM_BEGIN(HttpContentType)
	RBX_ENUM_VALUE(HttpContentType, ApplicationJson, 0)
	RBX_ENUM_VALUE(HttpContentType, ApplicationXml, 1)
	RBX_ENUM_VALUE(HttpContentType, ApplicationUrlEncoded, 2)
	RBX_ENUM_VALUE(HttpContentType, TextPlain, 3)
	RBX_ENUM_VALUE(HttpContentType, TextXml, 4)
RBX_ENUM_END(HttpContentType, ApplicationJson, ApplicationXml, ApplicationUrlEncoded, TextPlain, TextXml)

RBX_ENUM_BEGIN(HttpError)
	RBX_ENUM_VALUE(HttpError, OK, 0)
	RBX_ENUM_VALUE(HttpError, InvalidUrl, 1)
	RBX_ENUM_VALUE(HttpError, DnsResolve, 2)
	RBX_ENUM_VALUE(HttpError, ConnectFail, 3)
	RBX_ENUM_VALUE(HttpError, OutOfMemory, 4)
	RBX_ENUM_VALUE(HttpError, TimedOut, 5)
	RBX_ENUM_VALUE(HttpError, TooManyRedirects, 6)
	RBX_ENUM_VALUE(HttpError, InvalidRedirect, 7)
	RBX_ENUM_VALUE(HttpError, NetFail, 8)
	RBX_ENUM_VALUE(HttpError, Aborted, 9)
	RBX_ENUM_VALUE(HttpError, SslConnectFail, 10)
	RBX_ENUM_VALUE(HttpError, SslVerificationFail, 11)
	RBX_ENUM_VALUE(HttpError, Unknown, 12)
	RBX_ENUM_VALUE(HttpError, ConnectionClosed, 13)
	RBX_ENUM_VALUE(HttpError, ServerProtocolError, 14)
	RBX_ENUM_VALUE(HttpError, CreatorEnvironmentsNotSupportedByService, 15)
	RBX_ENUM_VALUE(HttpError, InactivityTimeout, 16)
	RBX_ENUM_VALUE(HttpError, TooManyOutstandingRequests, 17)
RBX_ENUM_END(HttpError, OK, InvalidUrl, DnsResolve, ConnectFail, OutOfMemory, TimedOut, TooManyRedirects, InvalidRedirect, NetFail, Aborted, SslConnectFail, SslVerificationFail, Unknown, ConnectionClosed, ServerProtocolError, CreatorEnvironmentsNotSupportedByService, InactivityTimeout, TooManyOutstandingRequests)

RBX_ENUM_BEGIN(HttpRequestType)
	RBX_ENUM_VALUE(HttpRequestType, Default, 0)
	RBX_ENUM_VALUE(HttpRequestType, MarketplaceService, 2)
	RBX_ENUM_VALUE(HttpRequestType, Players, 7)
	RBX_ENUM_VALUE(HttpRequestType, Chat, 15)
	RBX_ENUM_VALUE(HttpRequestType, Avatar, 16)
	RBX_ENUM_VALUE(HttpRequestType, Analytics, 23)
	RBX_ENUM_VALUE(HttpRequestType, Localization, 25)
RBX_ENUM_END(HttpRequestType, Default, MarketplaceService, Players, Chat, Avatar, Analytics, Localization)

RBX_ENUM_BEGIN(HumanoidCollisionType)
	RBX_ENUM_VALUE(HumanoidCollisionType, OuterBox, 0)
	RBX_ENUM_VALUE(HumanoidCollisionType, InnerBox, 1)
RBX_ENUM_END(HumanoidCollisionType, OuterBox, InnerBox)

RBX_ENUM_BEGIN(HumanoidDisplayDistanceType)
	RBX_ENUM_VALUE(HumanoidDisplayDistanceType, Viewer, 0)
	RBX_ENUM_VALUE(HumanoidDisplayDistanceType, Subject, 1)
	RBX_ENUM_VALUE(HumanoidDisplayDistanceType, None, 2)
RBX_ENUM_END(HumanoidDisplayDistanceType, Viewer, Subject, None)

RBX_ENUM_BEGIN(HumanoidHealthDisplayType)
	RBX_ENUM_VALUE(HumanoidHealthDisplayType, DisplayWhenDamaged, 0)
	RBX_ENUM_VALUE(HumanoidHealthDisplayType, AlwaysOn, 1)
	RBX_ENUM_VALUE(HumanoidHealthDisplayType, AlwaysOff, 2)
RBX_ENUM_END(HumanoidHealthDisplayType, DisplayWhenDamaged, AlwaysOn, AlwaysOff)

RBX_ENUM_BEGIN(HumanoidRigType)
	RBX_ENUM_VALUE(HumanoidRigType, R6, 0)
	RBX_ENUM_VALUE(HumanoidRigType, R15, 1)
RBX_ENUM_END(HumanoidRigType, R6, R15)

RBX_ENUM_BEGIN(HumanoidStateType)
	RBX_ENUM_VALUE(HumanoidStateType, FallingDown, 0)
	RBX_ENUM_VALUE(HumanoidStateType, Ragdoll, 1)
	RBX_ENUM_VALUE(HumanoidStateType, GettingUp, 2)
	RBX_ENUM_VALUE(HumanoidStateType, Jumping, 3)
	RBX_ENUM_VALUE(HumanoidStateType, Swimming, 4)
	RBX_ENUM_VALUE(HumanoidStateType, Freefall, 5)
	RBX_ENUM_VALUE(HumanoidStateType, Flying, 6)
	RBX_ENUM_VALUE(HumanoidStateType, Landed, 7)
	RBX_ENUM_VALUE(HumanoidStateType, Running, 8)
	RBX_ENUM_VALUE(HumanoidStateType, RunningNoPhysics, 10)
	RBX_ENUM_VALUE(HumanoidStateType, StrafingNoPhysics, 11)
	RBX_ENUM_VALUE(HumanoidStateType, Climbing, 12)
	RBX_ENUM_VALUE(HumanoidStateType, Seated, 13)
	RBX_ENUM_VALUE(HumanoidStateType, PlatformStanding, 14)
	RBX_ENUM_VALUE(HumanoidStateType, Dead, 15)
	RBX_ENUM_VALUE(HumanoidStateType, Physics, 16)
	RBX_ENUM_VALUE(HumanoidStateType, None, 18)
RBX_ENUM_END(HumanoidStateType, FallingDown, Ragdoll, GettingUp, Jumping, Swimming, Freefall, Flying, Landed, Running, RunningNoPhysics, StrafingNoPhysics, Climbing, Seated, PlatformStanding, Dead, Physics, None)

RBX_ENUM_BEGIN(IKCollisionsMode)
	RBX_ENUM_VALUE(IKCollisionsMode, NoCollisions, 0)
	RBX_ENUM_VALUE(IKCollisionsMode, OtherMechanismsAnchored, 1)
	RBX_ENUM_VALUE(IKCollisionsMode, IncludeContactedMechanisms, 2)
RBX_ENUM_END(IKCollisionsMode, NoCollisions, OtherMechanismsAnchored, IncludeContactedMechanisms)

RBX_ENUM_BEGIN(IKControlConstraintSupport)
	RBX_ENUM_VALUE(IKControlConstraintSupport, Default, 0)
	RBX_ENUM_VALUE(IKControlConstraintSupport, Disabled, 1)
	RBX_ENUM_VALUE(IKControlConstraintSupport, Enabled, 2)
RBX_ENUM_END(IKControlConstraintSupport, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(IKControlType)
	RBX_ENUM_VALUE(IKControlType, Transform, 0)
	RBX_ENUM_VALUE(IKControlType, Position, 1)
	RBX_ENUM_VALUE(IKControlType, Rotation, 2)
	RBX_ENUM_VALUE(IKControlType, LookAt, 3)
RBX_ENUM_END(IKControlType, Transform, Position, Rotation, LookAt)

RBX_ENUM_BEGIN(IXPLoadingStatus)
	RBX_ENUM_VALUE(IXPLoadingStatus, None, 0)
	RBX_ENUM_VALUE(IXPLoadingStatus, Pending, 1)
	RBX_ENUM_VALUE(IXPLoadingStatus, Initialized, 2)
	RBX_ENUM_VALUE(IXPLoadingStatus, ErrorInvalidUser, 3)
	RBX_ENUM_VALUE(IXPLoadingStatus, ErrorConnection, 4)
	RBX_ENUM_VALUE(IXPLoadingStatus, ErrorJsonParse, 5)
	RBX_ENUM_VALUE(IXPLoadingStatus, ErrorTimedOut, 6)
RBX_ENUM_END(IXPLoadingStatus, None, Pending, Initialized, ErrorInvalidUser, ErrorConnection, ErrorJsonParse, ErrorTimedOut)

RBX_ENUM_BEGIN(ImageAlphaType)
	RBX_ENUM_VALUE(ImageAlphaType, Default, 1)
	RBX_ENUM_VALUE(ImageAlphaType, LockCanvasAlpha, 2)
	RBX_ENUM_VALUE(ImageAlphaType, LockCanvasColor, 3)
RBX_ENUM_END(ImageAlphaType, Default, LockCanvasAlpha, LockCanvasColor)

RBX_ENUM_BEGIN(ImageCombineType)
	RBX_ENUM_VALUE(ImageCombineType, BlendSourceOver, 1)
	RBX_ENUM_VALUE(ImageCombineType, Overwrite, 2)
	RBX_ENUM_VALUE(ImageCombineType, Add, 3)
	RBX_ENUM_VALUE(ImageCombineType, Multiply, 4)
	RBX_ENUM_VALUE(ImageCombineType, AlphaBlend, 5)
RBX_ENUM_END(ImageCombineType, BlendSourceOver, Overwrite, Add, Multiply, AlphaBlend)

RBX_ENUM_BEGIN(InOut)
	RBX_ENUM_VALUE(InOut, Edge, 0)
	RBX_ENUM_VALUE(InOut, Inset, 1)
	RBX_ENUM_VALUE(InOut, Center, 2)
RBX_ENUM_END(InOut, Edge, Inset, Center)

RBX_ENUM_BEGIN(InfoType)
	RBX_ENUM_VALUE(InfoType, Asset, 0)
	RBX_ENUM_VALUE(InfoType, Product, 1)
	RBX_ENUM_VALUE(InfoType, GamePass, 2)
	RBX_ENUM_VALUE(InfoType, Subscription, 3)
	RBX_ENUM_VALUE(InfoType, Bundle, 4)
RBX_ENUM_END(InfoType, Asset, Product, GamePass, Subscription, Bundle)

RBX_ENUM_BEGIN(InitialDockState)
	RBX_ENUM_VALUE(InitialDockState, Top, 0)
	RBX_ENUM_VALUE(InitialDockState, Bottom, 1)
	RBX_ENUM_VALUE(InitialDockState, Left, 2)
	RBX_ENUM_VALUE(InitialDockState, Right, 3)
	RBX_ENUM_VALUE(InitialDockState, Float, 4)
RBX_ENUM_END(InitialDockState, Top, Bottom, Left, Right, Float)

RBX_ENUM_BEGIN(InputActionType)
	RBX_ENUM_VALUE(InputActionType, Bool, 0)
	RBX_ENUM_VALUE(InputActionType, Direction1D, 1)
	RBX_ENUM_VALUE(InputActionType, Direction2D, 2)
	RBX_ENUM_VALUE(InputActionType, Direction3D, 3)
	RBX_ENUM_VALUE(InputActionType, ViewportPosition, 4)
RBX_ENUM_END(InputActionType, Bool, Direction1D, Direction2D, Direction3D, ViewportPosition)

RBX_ENUM_BEGIN(InputSink)
	RBX_ENUM_VALUE(InputSink, None, 0)
	RBX_ENUM_VALUE(InputSink, Activate, 1)
	RBX_ENUM_VALUE(InputSink, All, 100)
RBX_ENUM_END(InputSink, None, Activate, All)

RBX_ENUM_BEGIN(InputType)
	RBX_ENUM_VALUE(InputType, NoInput, 0)
	RBX_ENUM_VALUE(InputType, Constant, 12)
	RBX_ENUM_VALUE(InputType, Sin, 13)
RBX_ENUM_END(InputType, NoInput, Constant, Sin)

RBX_ENUM_BEGIN(InstanceFileSyncStatus)
	RBX_ENUM_VALUE(InstanceFileSyncStatus, NotSynced, 0)
	RBX_ENUM_VALUE(InstanceFileSyncStatus, Errored, 1)
	RBX_ENUM_VALUE(InstanceFileSyncStatus, SyncedAsRoot, 2)
	RBX_ENUM_VALUE(InstanceFileSyncStatus, SyncedAsDescendant, 3)
	RBX_ENUM_VALUE(InstanceFileSyncStatus, AncestorErrored, 4)
RBX_ENUM_END(InstanceFileSyncStatus, NotSynced, Errored, SyncedAsRoot, SyncedAsDescendant, AncestorErrored)

RBX_ENUM_BEGIN(IntermediateMeshGenerationResult)
	RBX_ENUM_VALUE(IntermediateMeshGenerationResult, HighQualityMesh, 0)
RBX_ENUM_END(IntermediateMeshGenerationResult, HighQualityMesh)

RBX_ENUM_BEGIN(InterpolationThrottlingMode)
	RBX_ENUM_VALUE(InterpolationThrottlingMode, Default, 0)
	RBX_ENUM_VALUE(InterpolationThrottlingMode, Disabled, 1)
	RBX_ENUM_VALUE(InterpolationThrottlingMode, Enabled, 2)
RBX_ENUM_END(InterpolationThrottlingMode, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(InviteState)
	RBX_ENUM_VALUE(InviteState, Placed, 0)
	RBX_ENUM_VALUE(InviteState, Accepted, 1)
	RBX_ENUM_VALUE(InviteState, Declined, 2)
	RBX_ENUM_VALUE(InviteState, Missed, 3)
RBX_ENUM_END(InviteState, Placed, Accepted, Declined, Missed)

RBX_ENUM_BEGIN(ItemLineAlignment)
	RBX_ENUM_VALUE(ItemLineAlignment, Automatic, 0)
	RBX_ENUM_VALUE(ItemLineAlignment, Start, 1)
	RBX_ENUM_VALUE(ItemLineAlignment, Center, 2)
	RBX_ENUM_VALUE(ItemLineAlignment, End, 3)
	RBX_ENUM_VALUE(ItemLineAlignment, Stretch, 4)
RBX_ENUM_END(ItemLineAlignment, Automatic, Start, Center, End, Stretch)

RBX_ENUM_BEGIN(JoinSource)
	RBX_ENUM_VALUE(JoinSource, CreatedItemAttribution, 1)
RBX_ENUM_END(JoinSource, CreatedItemAttribution)

RBX_ENUM_BEGIN(JointCreationMode)
	RBX_ENUM_VALUE(JointCreationMode, All, 0)
	RBX_ENUM_VALUE(JointCreationMode, Surface, 1)
	RBX_ENUM_VALUE(JointCreationMode, None, 2)
RBX_ENUM_END(JointCreationMode, All, Surface, None)

RBX_ENUM_BEGIN(KeyCode)
	RBX_ENUM_VALUE(KeyCode, Unknown, 0)
	RBX_ENUM_VALUE(KeyCode, Backspace, 8)
	RBX_ENUM_VALUE(KeyCode, Tab, 9)
	RBX_ENUM_VALUE(KeyCode, Clear, 12)
	RBX_ENUM_VALUE(KeyCode, Return, 13)
	RBX_ENUM_VALUE(KeyCode, Pause, 19)
	RBX_ENUM_VALUE(KeyCode, Escape, 27)
	RBX_ENUM_VALUE(KeyCode, Space, 32)
	RBX_ENUM_VALUE(KeyCode, QuotedDouble, 34)
	RBX_ENUM_VALUE(KeyCode, Hash, 35)
	RBX_ENUM_VALUE(KeyCode, Dollar, 36)
	RBX_ENUM_VALUE(KeyCode, Percent, 37)
	RBX_ENUM_VALUE(KeyCode, Ampersand, 38)
	RBX_ENUM_VALUE(KeyCode, Quote, 39)
	RBX_ENUM_VALUE(KeyCode, LeftParenthesis, 40)
	RBX_ENUM_VALUE(KeyCode, RightParenthesis, 41)
	RBX_ENUM_VALUE(KeyCode, Asterisk, 42)
	RBX_ENUM_VALUE(KeyCode, Plus, 43)
	RBX_ENUM_VALUE(KeyCode, Comma, 44)
	RBX_ENUM_VALUE(KeyCode, Minus, 45)
	RBX_ENUM_VALUE(KeyCode, Period, 46)
	RBX_ENUM_VALUE(KeyCode, Slash, 47)
	RBX_ENUM_VALUE(KeyCode, Zero, 48)
	RBX_ENUM_VALUE(KeyCode, One, 49)
	RBX_ENUM_VALUE(KeyCode, Two, 50)
	RBX_ENUM_VALUE(KeyCode, Three, 51)
	RBX_ENUM_VALUE(KeyCode, Four, 52)
	RBX_ENUM_VALUE(KeyCode, Five, 53)
	RBX_ENUM_VALUE(KeyCode, Six, 54)
	RBX_ENUM_VALUE(KeyCode, Seven, 55)
	RBX_ENUM_VALUE(KeyCode, Eight, 56)
	RBX_ENUM_VALUE(KeyCode, Nine, 57)
	RBX_ENUM_VALUE(KeyCode, Colon, 58)
	RBX_ENUM_VALUE(KeyCode, Semicolon, 59)
	RBX_ENUM_VALUE(KeyCode, LessThan, 60)
	RBX_ENUM_VALUE(KeyCode, Equals, 61)
	RBX_ENUM_VALUE(KeyCode, GreaterThan, 62)
	RBX_ENUM_VALUE(KeyCode, Question, 63)
	RBX_ENUM_VALUE(KeyCode, At, 64)
	RBX_ENUM_VALUE(KeyCode, LeftBracket, 91)
	RBX_ENUM_VALUE(KeyCode, BackSlash, 92)
	RBX_ENUM_VALUE(KeyCode, RightBracket, 93)
	RBX_ENUM_VALUE(KeyCode, Caret, 94)
	RBX_ENUM_VALUE(KeyCode, Underscore, 95)
	RBX_ENUM_VALUE(KeyCode, Backquote, 96)
	RBX_ENUM_VALUE(KeyCode, A, 97)
	RBX_ENUM_VALUE(KeyCode, B, 98)
	RBX_ENUM_VALUE(KeyCode, C, 99)
	RBX_ENUM_VALUE(KeyCode, D, 100)
	RBX_ENUM_VALUE(KeyCode, E, 101)
	RBX_ENUM_VALUE(KeyCode, F, 102)
	RBX_ENUM_VALUE(KeyCode, G, 103)
	RBX_ENUM_VALUE(KeyCode, H, 104)
	RBX_ENUM_VALUE(KeyCode, I, 105)
	RBX_ENUM_VALUE(KeyCode, J, 106)
	RBX_ENUM_VALUE(KeyCode, K, 107)
	RBX_ENUM_VALUE(KeyCode, L, 108)
	RBX_ENUM_VALUE(KeyCode, M, 109)
	RBX_ENUM_VALUE(KeyCode, N, 110)
	RBX_ENUM_VALUE(KeyCode, O, 111)
	RBX_ENUM_VALUE(KeyCode, P, 112)
	RBX_ENUM_VALUE(KeyCode, Q, 113)
	RBX_ENUM_VALUE(KeyCode, R, 114)
	RBX_ENUM_VALUE(KeyCode, S, 115)
	RBX_ENUM_VALUE(KeyCode, T, 116)
	RBX_ENUM_VALUE(KeyCode, U, 117)
	RBX_ENUM_VALUE(KeyCode, V, 118)
	RBX_ENUM_VALUE(KeyCode, W, 119)
	RBX_ENUM_VALUE(KeyCode, X, 120)
	RBX_ENUM_VALUE(KeyCode, Y, 121)
	RBX_ENUM_VALUE(KeyCode, Z, 122)
	RBX_ENUM_VALUE(KeyCode, LeftCurly, 123)
	RBX_ENUM_VALUE(KeyCode, Pipe, 124)
	RBX_ENUM_VALUE(KeyCode, RightCurly, 125)
	RBX_ENUM_VALUE(KeyCode, Tilde, 126)
	RBX_ENUM_VALUE(KeyCode, Delete, 127)
	RBX_ENUM_VALUE(KeyCode, World0, 160)
	RBX_ENUM_VALUE(KeyCode, World1, 161)
	RBX_ENUM_VALUE(KeyCode, World2, 162)
	RBX_ENUM_VALUE(KeyCode, World3, 163)
	RBX_ENUM_VALUE(KeyCode, World4, 164)
	RBX_ENUM_VALUE(KeyCode, World5, 165)
	RBX_ENUM_VALUE(KeyCode, World6, 166)
	RBX_ENUM_VALUE(KeyCode, World7, 167)
	RBX_ENUM_VALUE(KeyCode, World8, 168)
	RBX_ENUM_VALUE(KeyCode, World9, 169)
	RBX_ENUM_VALUE(KeyCode, World10, 170)
	RBX_ENUM_VALUE(KeyCode, World11, 171)
	RBX_ENUM_VALUE(KeyCode, World12, 172)
	RBX_ENUM_VALUE(KeyCode, World13, 173)
	RBX_ENUM_VALUE(KeyCode, World14, 174)
	RBX_ENUM_VALUE(KeyCode, World15, 175)
	RBX_ENUM_VALUE(KeyCode, World16, 176)
	RBX_ENUM_VALUE(KeyCode, World17, 177)
	RBX_ENUM_VALUE(KeyCode, World18, 178)
	RBX_ENUM_VALUE(KeyCode, World19, 179)
	RBX_ENUM_VALUE(KeyCode, World20, 180)
	RBX_ENUM_VALUE(KeyCode, World21, 181)
	RBX_ENUM_VALUE(KeyCode, World22, 182)
	RBX_ENUM_VALUE(KeyCode, World23, 183)
	RBX_ENUM_VALUE(KeyCode, World24, 184)
	RBX_ENUM_VALUE(KeyCode, World25, 185)
	RBX_ENUM_VALUE(KeyCode, World26, 186)
	RBX_ENUM_VALUE(KeyCode, World27, 187)
	RBX_ENUM_VALUE(KeyCode, World28, 188)
	RBX_ENUM_VALUE(KeyCode, World29, 189)
	RBX_ENUM_VALUE(KeyCode, World30, 190)
	RBX_ENUM_VALUE(KeyCode, World31, 191)
	RBX_ENUM_VALUE(KeyCode, World32, 192)
	RBX_ENUM_VALUE(KeyCode, World33, 193)
	RBX_ENUM_VALUE(KeyCode, World34, 194)
	RBX_ENUM_VALUE(KeyCode, World35, 195)
	RBX_ENUM_VALUE(KeyCode, World36, 196)
	RBX_ENUM_VALUE(KeyCode, World37, 197)
	RBX_ENUM_VALUE(KeyCode, World38, 198)
	RBX_ENUM_VALUE(KeyCode, World39, 199)
	RBX_ENUM_VALUE(KeyCode, World40, 200)
	RBX_ENUM_VALUE(KeyCode, World41, 201)
	RBX_ENUM_VALUE(KeyCode, World42, 202)
	RBX_ENUM_VALUE(KeyCode, World43, 203)
	RBX_ENUM_VALUE(KeyCode, World44, 204)
	RBX_ENUM_VALUE(KeyCode, World45, 205)
	RBX_ENUM_VALUE(KeyCode, World46, 206)
	RBX_ENUM_VALUE(KeyCode, World47, 207)
	RBX_ENUM_VALUE(KeyCode, World48, 208)
	RBX_ENUM_VALUE(KeyCode, World49, 209)
	RBX_ENUM_VALUE(KeyCode, World50, 210)
	RBX_ENUM_VALUE(KeyCode, World51, 211)
	RBX_ENUM_VALUE(KeyCode, World52, 212)
	RBX_ENUM_VALUE(KeyCode, World53, 213)
	RBX_ENUM_VALUE(KeyCode, World54, 214)
	RBX_ENUM_VALUE(KeyCode, World55, 215)
	RBX_ENUM_VALUE(KeyCode, World56, 216)
	RBX_ENUM_VALUE(KeyCode, World57, 217)
	RBX_ENUM_VALUE(KeyCode, World58, 218)
	RBX_ENUM_VALUE(KeyCode, World59, 219)
	RBX_ENUM_VALUE(KeyCode, World60, 220)
	RBX_ENUM_VALUE(KeyCode, World61, 221)
	RBX_ENUM_VALUE(KeyCode, World62, 222)
	RBX_ENUM_VALUE(KeyCode, World63, 223)
	RBX_ENUM_VALUE(KeyCode, World64, 224)
	RBX_ENUM_VALUE(KeyCode, World65, 225)
	RBX_ENUM_VALUE(KeyCode, World66, 226)
	RBX_ENUM_VALUE(KeyCode, World67, 227)
	RBX_ENUM_VALUE(KeyCode, World68, 228)
	RBX_ENUM_VALUE(KeyCode, World69, 229)
	RBX_ENUM_VALUE(KeyCode, World70, 230)
	RBX_ENUM_VALUE(KeyCode, World71, 231)
	RBX_ENUM_VALUE(KeyCode, World72, 232)
	RBX_ENUM_VALUE(KeyCode, World73, 233)
	RBX_ENUM_VALUE(KeyCode, World74, 234)
	RBX_ENUM_VALUE(KeyCode, World75, 235)
	RBX_ENUM_VALUE(KeyCode, World76, 236)
	RBX_ENUM_VALUE(KeyCode, World77, 237)
	RBX_ENUM_VALUE(KeyCode, World78, 238)
	RBX_ENUM_VALUE(KeyCode, World79, 239)
	RBX_ENUM_VALUE(KeyCode, World80, 240)
	RBX_ENUM_VALUE(KeyCode, World81, 241)
	RBX_ENUM_VALUE(KeyCode, World82, 242)
	RBX_ENUM_VALUE(KeyCode, World83, 243)
	RBX_ENUM_VALUE(KeyCode, World84, 244)
	RBX_ENUM_VALUE(KeyCode, World85, 245)
	RBX_ENUM_VALUE(KeyCode, World86, 246)
	RBX_ENUM_VALUE(KeyCode, World87, 247)
	RBX_ENUM_VALUE(KeyCode, World88, 248)
	RBX_ENUM_VALUE(KeyCode, World89, 249)
	RBX_ENUM_VALUE(KeyCode, World90, 250)
	RBX_ENUM_VALUE(KeyCode, World91, 251)
	RBX_ENUM_VALUE(KeyCode, World92, 252)
	RBX_ENUM_VALUE(KeyCode, World93, 253)
	RBX_ENUM_VALUE(KeyCode, World94, 254)
	RBX_ENUM_VALUE(KeyCode, World95, 255)
	RBX_ENUM_VALUE(KeyCode, KeypadZero, 256)
	RBX_ENUM_VALUE(KeyCode, KeypadOne, 257)
	RBX_ENUM_VALUE(KeyCode, KeypadTwo, 258)
	RBX_ENUM_VALUE(KeyCode, KeypadThree, 259)
	RBX_ENUM_VALUE(KeyCode, KeypadFour, 260)
	RBX_ENUM_VALUE(KeyCode, KeypadFive, 261)
	RBX_ENUM_VALUE(KeyCode, KeypadSix, 262)
	RBX_ENUM_VALUE(KeyCode, KeypadSeven, 263)
	RBX_ENUM_VALUE(KeyCode, KeypadEight, 264)
	RBX_ENUM_VALUE(KeyCode, KeypadNine, 265)
	RBX_ENUM_VALUE(KeyCode, KeypadPeriod, 266)
	RBX_ENUM_VALUE(KeyCode, KeypadDivide, 267)
	RBX_ENUM_VALUE(KeyCode, KeypadMultiply, 268)
	RBX_ENUM_VALUE(KeyCode, KeypadMinus, 269)
	RBX_ENUM_VALUE(KeyCode, KeypadPlus, 270)
	RBX_ENUM_VALUE(KeyCode, KeypadEnter, 271)
	RBX_ENUM_VALUE(KeyCode, KeypadEquals, 272)
	RBX_ENUM_VALUE(KeyCode, Up, 273)
	RBX_ENUM_VALUE(KeyCode, Down, 274)
	RBX_ENUM_VALUE(KeyCode, Right, 275)
	RBX_ENUM_VALUE(KeyCode, Left, 276)
	RBX_ENUM_VALUE(KeyCode, Insert, 277)
	RBX_ENUM_VALUE(KeyCode, Home, 278)
	RBX_ENUM_VALUE(KeyCode, End, 279)
	RBX_ENUM_VALUE(KeyCode, PageUp, 280)
	RBX_ENUM_VALUE(KeyCode, PageDown, 281)
	RBX_ENUM_VALUE(KeyCode, F1, 282)
	RBX_ENUM_VALUE(KeyCode, F2, 283)
	RBX_ENUM_VALUE(KeyCode, F3, 284)
	RBX_ENUM_VALUE(KeyCode, F4, 285)
	RBX_ENUM_VALUE(KeyCode, F5, 286)
	RBX_ENUM_VALUE(KeyCode, F6, 287)
	RBX_ENUM_VALUE(KeyCode, F7, 288)
	RBX_ENUM_VALUE(KeyCode, F8, 289)
	RBX_ENUM_VALUE(KeyCode, F9, 290)
	RBX_ENUM_VALUE(KeyCode, F10, 291)
	RBX_ENUM_VALUE(KeyCode, F11, 292)
	RBX_ENUM_VALUE(KeyCode, F12, 293)
	RBX_ENUM_VALUE(KeyCode, F13, 294)
	RBX_ENUM_VALUE(KeyCode, F14, 295)
	RBX_ENUM_VALUE(KeyCode, F15, 296)
	RBX_ENUM_VALUE(KeyCode, NumLock, 300)
	RBX_ENUM_VALUE(KeyCode, CapsLock, 301)
	RBX_ENUM_VALUE(KeyCode, ScrollLock, 302)
	RBX_ENUM_VALUE(KeyCode, RightShift, 303)
	RBX_ENUM_VALUE(KeyCode, LeftShift, 304)
	RBX_ENUM_VALUE(KeyCode, RightControl, 305)
	RBX_ENUM_VALUE(KeyCode, LeftControl, 306)
	RBX_ENUM_VALUE(KeyCode, RightAlt, 307)
	RBX_ENUM_VALUE(KeyCode, LeftAlt, 308)
	RBX_ENUM_VALUE(KeyCode, RightMeta, 309)
	RBX_ENUM_VALUE(KeyCode, LeftMeta, 310)
	RBX_ENUM_VALUE(KeyCode, LeftSuper, 311)
	RBX_ENUM_VALUE(KeyCode, RightSuper, 312)
	RBX_ENUM_VALUE(KeyCode, Mode, 313)
	RBX_ENUM_VALUE(KeyCode, Compose, 314)
	RBX_ENUM_VALUE(KeyCode, Help, 315)
	RBX_ENUM_VALUE(KeyCode, Print, 316)
	RBX_ENUM_VALUE(KeyCode, SysReq, 317)
	RBX_ENUM_VALUE(KeyCode, Break, 318)
	RBX_ENUM_VALUE(KeyCode, Menu, 319)
	RBX_ENUM_VALUE(KeyCode, Power, 320)
	RBX_ENUM_VALUE(KeyCode, Euro, 321)
	RBX_ENUM_VALUE(KeyCode, Undo, 322)
	RBX_ENUM_VALUE(KeyCode, ButtonX, 1000)
	RBX_ENUM_VALUE(KeyCode, ButtonY, 1001)
	RBX_ENUM_VALUE(KeyCode, ButtonA, 1002)
	RBX_ENUM_VALUE(KeyCode, ButtonB, 1003)
	RBX_ENUM_VALUE(KeyCode, ButtonR1, 1004)
	RBX_ENUM_VALUE(KeyCode, ButtonL1, 1005)
	RBX_ENUM_VALUE(KeyCode, ButtonR2, 1006)
	RBX_ENUM_VALUE(KeyCode, ButtonL2, 1007)
	RBX_ENUM_VALUE(KeyCode, ButtonR3, 1008)
	RBX_ENUM_VALUE(KeyCode, ButtonL3, 1009)
	RBX_ENUM_VALUE(KeyCode, ButtonStart, 1010)
	RBX_ENUM_VALUE(KeyCode, ButtonSelect, 1011)
	RBX_ENUM_VALUE(KeyCode, DPadLeft, 1012)
	RBX_ENUM_VALUE(KeyCode, DPadRight, 1013)
	RBX_ENUM_VALUE(KeyCode, DPadUp, 1014)
	RBX_ENUM_VALUE(KeyCode, DPadDown, 1015)
	RBX_ENUM_VALUE(KeyCode, Thumbstick1, 1016)
	RBX_ENUM_VALUE(KeyCode, Thumbstick2, 1017)
	RBX_ENUM_VALUE(KeyCode, Thumbstick1Up, 1018)
	RBX_ENUM_VALUE(KeyCode, Thumbstick1Down, 1019)
	RBX_ENUM_VALUE(KeyCode, Thumbstick1Left, 1020)
	RBX_ENUM_VALUE(KeyCode, Thumbstick1Right, 1021)
	RBX_ENUM_VALUE(KeyCode, Thumbstick2Up, 1022)
	RBX_ENUM_VALUE(KeyCode, Thumbstick2Down, 1023)
	RBX_ENUM_VALUE(KeyCode, Thumbstick2Left, 1024)
	RBX_ENUM_VALUE(KeyCode, Thumbstick2Right, 1025)
	RBX_ENUM_VALUE(KeyCode, MouseLeftButton, 1026)
	RBX_ENUM_VALUE(KeyCode, MouseRightButton, 1027)
	RBX_ENUM_VALUE(KeyCode, MouseMiddleButton, 1028)
	RBX_ENUM_VALUE(KeyCode, MouseBackButton, 1029)
	RBX_ENUM_VALUE(KeyCode, MouseNoButton, 1030)
	RBX_ENUM_VALUE(KeyCode, MouseX, 1031)
	RBX_ENUM_VALUE(KeyCode, MouseY, 1032)
	RBX_ENUM_VALUE(KeyCode, MousePosition, 1033)
	RBX_ENUM_VALUE(KeyCode, TouchPosition, 1034)
	RBX_ENUM_VALUE(KeyCode, MouseWheel, 1035)
	RBX_ENUM_VALUE(KeyCode, TrackpadPan, 1040)
	RBX_ENUM_VALUE(KeyCode, TrackpadPinch, 1045)
	RBX_ENUM_VALUE(KeyCode, MouseDelta, 1048)
	RBX_ENUM_VALUE(KeyCode, TouchDelta, 1049)
	RBX_ENUM_VALUE(KeyCode, TouchPinch, 1050)
RBX_ENUM_END(KeyCode, Unknown, Backspace, Tab, Clear, Return, Pause, Escape, Space, QuotedDouble, Hash, Dollar, Percent, Ampersand, Quote, LeftParenthesis, RightParenthesis, Asterisk, Plus, Comma, Minus, Period, Slash, Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Colon, Semicolon, LessThan, Equals, GreaterThan, Question, At, LeftBracket, BackSlash, RightBracket, Caret, Underscore, Backquote, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, LeftCurly, Pipe, RightCurly, Tilde, Delete, World0, World1, World2, World3, World4, World5, World6, World7, World8, World9, World10, World11, World12, World13, World14, World15, World16, World17, World18, World19, World20, World21, World22, World23, World24, World25, World26, World27, World28, World29, World30, World31, World32, World33, World34, World35, World36, World37, World38, World39, World40, World41, World42, World43, World44, World45, World46, World47, World48, World49, World50, World51, World52, World53, World54, World55, World56, World57, World58, World59, World60, World61, World62, World63, World64, World65, World66, World67, World68, World69, World70, World71, World72, World73, World74, World75, World76, World77, World78, World79, World80, World81, World82, World83, World84, World85, World86, World87, World88, World89, World90, World91, World92, World93, World94, World95, KeypadZero, KeypadOne, KeypadTwo, KeypadThree, KeypadFour, KeypadFive, KeypadSix, KeypadSeven, KeypadEight, KeypadNine, KeypadPeriod, KeypadDivide, KeypadMultiply, KeypadMinus, KeypadPlus, KeypadEnter, KeypadEquals, Up, Down, Right, Left, Insert, Home, End, PageUp, PageDown, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, NumLock, CapsLock, ScrollLock, RightShift, LeftShift, RightControl, LeftControl, RightAlt, LeftAlt, RightMeta, LeftMeta, LeftSuper, RightSuper, Mode, Compose, Help, Print, SysReq, Break, Menu, Power, Euro, Undo, ButtonX, ButtonY, ButtonA, ButtonB, ButtonR1, ButtonL1, ButtonR2, ButtonL2, ButtonR3, ButtonL3, ButtonStart, ButtonSelect, DPadLeft, DPadRight, DPadUp, DPadDown, Thumbstick1, Thumbstick2, Thumbstick1Up, Thumbstick1Down, Thumbstick1Left, Thumbstick1Right, Thumbstick2Up, Thumbstick2Down, Thumbstick2Left, Thumbstick2Right, MouseLeftButton, MouseRightButton, MouseMiddleButton, MouseBackButton, MouseNoButton, MouseX, MouseY, MousePosition, TouchPosition, MouseWheel, TrackpadPan, TrackpadPinch, MouseDelta, TouchDelta, TouchPinch)

RBX_ENUM_BEGIN(KeyInterpolationMode)
	RBX_ENUM_VALUE(KeyInterpolationMode, Constant, 0)
	RBX_ENUM_VALUE(KeyInterpolationMode, Linear, 1)
	RBX_ENUM_VALUE(KeyInterpolationMode, Cubic, 2)
RBX_ENUM_END(KeyInterpolationMode, Constant, Linear, Cubic)

RBX_ENUM_BEGIN(KeywordFilterType)
	RBX_ENUM_VALUE(KeywordFilterType, Include, 0)
	RBX_ENUM_VALUE(KeywordFilterType, Exclude, 1)
RBX_ENUM_END(KeywordFilterType, Include, Exclude)

RBX_ENUM_BEGIN(Language)
	RBX_ENUM_VALUE(Language, Default, 0)
RBX_ENUM_END(Language, Default)

RBX_ENUM_BEGIN(LeftRight)
	RBX_ENUM_VALUE(LeftRight, Left, 0)
	RBX_ENUM_VALUE(LeftRight, Center, 1)
	RBX_ENUM_VALUE(LeftRight, Right, 2)
RBX_ENUM_END(LeftRight, Left, Center, Right)

RBX_ENUM_BEGIN(LexemeType)
	RBX_ENUM_VALUE(LexemeType, Eof, 0)
	RBX_ENUM_VALUE(LexemeType, Name, 1)
	RBX_ENUM_VALUE(LexemeType, QuotedString, 2)
	RBX_ENUM_VALUE(LexemeType, Number, 3)
	RBX_ENUM_VALUE(LexemeType, And, 4)
	RBX_ENUM_VALUE(LexemeType, Or, 5)
	RBX_ENUM_VALUE(LexemeType, Equal, 6)
	RBX_ENUM_VALUE(LexemeType, TildeEqual, 7)
	RBX_ENUM_VALUE(LexemeType, GreaterThan, 8)
	RBX_ENUM_VALUE(LexemeType, GreaterThanEqual, 9)
	RBX_ENUM_VALUE(LexemeType, LessThan, 10)
	RBX_ENUM_VALUE(LexemeType, LessThanEqual, 11)
	RBX_ENUM_VALUE(LexemeType, Colon, 12)
	RBX_ENUM_VALUE(LexemeType, Dot, 13)
	RBX_ENUM_VALUE(LexemeType, LeftParenthesis, 14)
	RBX_ENUM_VALUE(LexemeType, RightParenthesis, 15)
	RBX_ENUM_VALUE(LexemeType, Star, 16)
	RBX_ENUM_VALUE(LexemeType, DoubleStar, 17)
	RBX_ENUM_VALUE(LexemeType, ReservedSpecial, 18)
RBX_ENUM_END(LexemeType, Eof, Name, QuotedString, Number, And, Or, Equal, TildeEqual, GreaterThan, GreaterThanEqual, LessThan, LessThanEqual, Colon, Dot, LeftParenthesis, RightParenthesis, Star, DoubleStar, ReservedSpecial)

RBX_ENUM_BEGIN(LightingStyle)
	RBX_ENUM_VALUE(LightingStyle, Realistic, 0)
	RBX_ENUM_VALUE(LightingStyle, Soft, 1)
RBX_ENUM_END(LightingStyle, Realistic, Soft)

RBX_ENUM_BEGIN(Limb)
	RBX_ENUM_VALUE(Limb, Head, 0)
	RBX_ENUM_VALUE(Limb, Torso, 1)
	RBX_ENUM_VALUE(Limb, LeftArm, 2)
	RBX_ENUM_VALUE(Limb, RightArm, 3)
	RBX_ENUM_VALUE(Limb, LeftLeg, 4)
	RBX_ENUM_VALUE(Limb, RightLeg, 5)
	RBX_ENUM_VALUE(Limb, Unknown, 6)
RBX_ENUM_END(Limb, Head, Torso, LeftArm, RightArm, LeftLeg, RightLeg, Unknown)

RBX_ENUM_BEGIN(LineJoinMode)
	RBX_ENUM_VALUE(LineJoinMode, Round, 0)
	RBX_ENUM_VALUE(LineJoinMode, Bevel, 1)
	RBX_ENUM_VALUE(LineJoinMode, Miter, 2)
RBX_ENUM_END(LineJoinMode, Round, Bevel, Miter)

RBX_ENUM_BEGIN(ListDisplayMode)
	RBX_ENUM_VALUE(ListDisplayMode, Horizontal, 0)
	RBX_ENUM_VALUE(ListDisplayMode, Vertical, 1)
RBX_ENUM_END(ListDisplayMode, Horizontal, Vertical)

RBX_ENUM_BEGIN(ListenerLocation)
	RBX_ENUM_VALUE(ListenerLocation, Default, 0)
	RBX_ENUM_VALUE(ListenerLocation, None, 1)
	RBX_ENUM_VALUE(ListenerLocation, Character, 2)
	RBX_ENUM_VALUE(ListenerLocation, Camera, 3)
RBX_ENUM_END(ListenerLocation, Default, None, Character, Camera)

RBX_ENUM_BEGIN(ListenerType)
	RBX_ENUM_VALUE(ListenerType, Camera, 0)
	RBX_ENUM_VALUE(ListenerType, CFrame, 1)
	RBX_ENUM_VALUE(ListenerType, ObjectPosition, 2)
	RBX_ENUM_VALUE(ListenerType, ObjectCFrame, 3)
RBX_ENUM_END(ListenerType, Camera, CFrame, ObjectPosition, ObjectCFrame)

RBX_ENUM_BEGIN(LiveEditingAtomicUpdateResponse)
	RBX_ENUM_VALUE(LiveEditingAtomicUpdateResponse, Success, 0)
	RBX_ENUM_VALUE(LiveEditingAtomicUpdateResponse, FailureGuidNotFound, 1)
	RBX_ENUM_VALUE(LiveEditingAtomicUpdateResponse, FailureHashMismatch, 2)
	RBX_ENUM_VALUE(LiveEditingAtomicUpdateResponse, FailureOperationIllegal, 3)
RBX_ENUM_END(LiveEditingAtomicUpdateResponse, Success, FailureGuidNotFound, FailureHashMismatch, FailureOperationIllegal)

RBX_ENUM_BEGIN(LiveEditingBroadcastMessageType)
	RBX_ENUM_VALUE(LiveEditingBroadcastMessageType, Normal, 0)
	RBX_ENUM_VALUE(LiveEditingBroadcastMessageType, Warning, 1)
	RBX_ENUM_VALUE(LiveEditingBroadcastMessageType, Error, 2)
RBX_ENUM_END(LiveEditingBroadcastMessageType, Normal, Warning, Error)

RBX_ENUM_BEGIN(LoadCharacterLayeredClothing)
	RBX_ENUM_VALUE(LoadCharacterLayeredClothing, Default, 0)
	RBX_ENUM_VALUE(LoadCharacterLayeredClothing, Disabled, 1)
	RBX_ENUM_VALUE(LoadCharacterLayeredClothing, Enabled, 2)
RBX_ENUM_END(LoadCharacterLayeredClothing, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(LoadDynamicHeads)
	RBX_ENUM_VALUE(LoadDynamicHeads, Default, 0)
	RBX_ENUM_VALUE(LoadDynamicHeads, Disabled, 1)
	RBX_ENUM_VALUE(LoadDynamicHeads, Enabled, 2)
RBX_ENUM_END(LoadDynamicHeads, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(LocationType)
	RBX_ENUM_VALUE(LocationType, Character, 0)
	RBX_ENUM_VALUE(LocationType, Camera, 1)
	RBX_ENUM_VALUE(LocationType, ObjectPosition, 2)
RBX_ENUM_END(LocationType, Character, Camera, ObjectPosition)

RBX_ENUM_BEGIN(LuauTypeCheckMode)
	RBX_ENUM_VALUE(LuauTypeCheckMode, Default, 0)
	RBX_ENUM_VALUE(LuauTypeCheckMode, NoCheck, 1)
	RBX_ENUM_VALUE(LuauTypeCheckMode, Nonstrict, 2)
	RBX_ENUM_VALUE(LuauTypeCheckMode, Strict, 3)
RBX_ENUM_END(LuauTypeCheckMode, Default, NoCheck, Nonstrict, Strict)

RBX_ENUM_BEGIN(MakeupType)
	RBX_ENUM_VALUE(MakeupType, Face, 0)
	RBX_ENUM_VALUE(MakeupType, Lip, 1)
	RBX_ENUM_VALUE(MakeupType, Eye, 2)
RBX_ENUM_END(MakeupType, Face, Lip, Eye)

RBX_ENUM_BEGIN(MarketplaceBulkPurchasePromptStatus)
	RBX_ENUM_VALUE(MarketplaceBulkPurchasePromptStatus, Completed, 1)
	RBX_ENUM_VALUE(MarketplaceBulkPurchasePromptStatus, Aborted, 2)
	RBX_ENUM_VALUE(MarketplaceBulkPurchasePromptStatus, Error, 3)
RBX_ENUM_END(MarketplaceBulkPurchasePromptStatus, Completed, Aborted, Error)

RBX_ENUM_BEGIN(MarketplaceItemPurchaseStatus)
	RBX_ENUM_VALUE(MarketplaceItemPurchaseStatus, Success, 1)
	RBX_ENUM_VALUE(MarketplaceItemPurchaseStatus, SystemError, 2)
	RBX_ENUM_VALUE(MarketplaceItemPurchaseStatus, AlreadyOwned, 3)
	RBX_ENUM_VALUE(MarketplaceItemPurchaseStatus, InsufficientRobux, 4)
	RBX_ENUM_VALUE(MarketplaceItemPurchaseStatus, QuantityLimitExceeded, 5)
	RBX_ENUM_VALUE(MarketplaceItemPurchaseStatus, QuotaExceeded, 6)
	RBX_ENUM_VALUE(MarketplaceItemPurchaseStatus, NotForSale, 7)
	RBX_ENUM_VALUE(MarketplaceItemPurchaseStatus, NotAvailableForPurchaser, 8)
	RBX_ENUM_VALUE(MarketplaceItemPurchaseStatus, PriceMismatch, 9)
	RBX_ENUM_VALUE(MarketplaceItemPurchaseStatus, SoldOut, 10)
	RBX_ENUM_VALUE(MarketplaceItemPurchaseStatus, PurchaserIsSeller, 11)
	RBX_ENUM_VALUE(MarketplaceItemPurchaseStatus, InsufficientMembership, 12)
	RBX_ENUM_VALUE(MarketplaceItemPurchaseStatus, PlaceInvalid, 13)
RBX_ENUM_END(MarketplaceItemPurchaseStatus, Success, SystemError, AlreadyOwned, InsufficientRobux, QuantityLimitExceeded, QuotaExceeded, NotForSale, NotAvailableForPurchaser, PriceMismatch, SoldOut, PurchaserIsSeller, InsufficientMembership, PlaceInvalid)

RBX_ENUM_BEGIN(MarketplaceProductType)
	RBX_ENUM_VALUE(MarketplaceProductType, AvatarAsset, 1)
	RBX_ENUM_VALUE(MarketplaceProductType, AvatarBundle, 2)
RBX_ENUM_END(MarketplaceProductType, AvatarAsset, AvatarBundle)

RBX_ENUM_BEGIN(MarkupKind)
	RBX_ENUM_VALUE(MarkupKind, PlainText, 0)
	RBX_ENUM_VALUE(MarkupKind, Markdown, 1)
RBX_ENUM_END(MarkupKind, PlainText, Markdown)

RBX_ENUM_BEGIN(MatchmakingType)
	RBX_ENUM_VALUE(MatchmakingType, Default, 1)
	RBX_ENUM_VALUE(MatchmakingType, XboxOnly, 2)
	RBX_ENUM_VALUE(MatchmakingType, PlayStationOnly, 3)
RBX_ENUM_END(MatchmakingType, Default, XboxOnly, PlayStationOnly)

RBX_ENUM_BEGIN(Material)
	RBX_ENUM_VALUE(Material, Plastic, 256)
	RBX_ENUM_VALUE(Material, SmoothPlastic, 272)
	RBX_ENUM_VALUE(Material, Neon, 288)
	RBX_ENUM_VALUE(Material, Wood, 512)
	RBX_ENUM_VALUE(Material, WoodPlanks, 528)
	RBX_ENUM_VALUE(Material, Marble, 784)
	RBX_ENUM_VALUE(Material, Basalt, 788)
	RBX_ENUM_VALUE(Material, Slate, 800)
	RBX_ENUM_VALUE(Material, CrackedLava, 804)
	RBX_ENUM_VALUE(Material, Concrete, 816)
	RBX_ENUM_VALUE(Material, Limestone, 820)
	RBX_ENUM_VALUE(Material, Granite, 832)
	RBX_ENUM_VALUE(Material, Pavement, 836)
	RBX_ENUM_VALUE(Material, Brick, 848)
	RBX_ENUM_VALUE(Material, Pebble, 864)
	RBX_ENUM_VALUE(Material, Cobblestone, 880)
	RBX_ENUM_VALUE(Material, Rock, 896)
	RBX_ENUM_VALUE(Material, Sandstone, 912)
	RBX_ENUM_VALUE(Material, CorrodedMetal, 1040)
	RBX_ENUM_VALUE(Material, DiamondPlate, 1056)
	RBX_ENUM_VALUE(Material, Foil, 1072)
	RBX_ENUM_VALUE(Material, Metal, 1088)
	RBX_ENUM_VALUE(Material, Grass, 1280)
	RBX_ENUM_VALUE(Material, LeafyGrass, 1284)
	RBX_ENUM_VALUE(Material, Sand, 1296)
	RBX_ENUM_VALUE(Material, Fabric, 1312)
	RBX_ENUM_VALUE(Material, Snow, 1328)
	RBX_ENUM_VALUE(Material, Mud, 1344)
	RBX_ENUM_VALUE(Material, Ground, 1360)
	RBX_ENUM_VALUE(Material, Asphalt, 1376)
	RBX_ENUM_VALUE(Material, Salt, 1392)
	RBX_ENUM_VALUE(Material, Ice, 1536)
	RBX_ENUM_VALUE(Material, Glacier, 1552)
	RBX_ENUM_VALUE(Material, Glass, 1568)
	RBX_ENUM_VALUE(Material, ForceField, 1584)
	RBX_ENUM_VALUE(Material, Air, 1792)
	RBX_ENUM_VALUE(Material, Water, 2048)
	RBX_ENUM_VALUE(Material, Cardboard, 2304)
	RBX_ENUM_VALUE(Material, Carpet, 2305)
	RBX_ENUM_VALUE(Material, CeramicTiles, 2306)
	RBX_ENUM_VALUE(Material, ClayRoofTiles, 2307)
	RBX_ENUM_VALUE(Material, RoofShingles, 2308)
	RBX_ENUM_VALUE(Material, Leather, 2309)
	RBX_ENUM_VALUE(Material, Plaster, 2310)
	RBX_ENUM_VALUE(Material, Rubber, 2311)
RBX_ENUM_END(Material, Plastic, SmoothPlastic, Neon, Wood, WoodPlanks, Marble, Basalt, Slate, CrackedLava, Concrete, Limestone, Granite, Pavement, Brick, Pebble, Cobblestone, Rock, Sandstone, CorrodedMetal, DiamondPlate, Foil, Metal, Grass, LeafyGrass, Sand, Fabric, Snow, Mud, Ground, Asphalt, Salt, Ice, Glacier, Glass, ForceField, Air, Water, Cardboard, Carpet, CeramicTiles, ClayRoofTiles, RoofShingles, Leather, Plaster, Rubber)

RBX_ENUM_BEGIN(MaterialPattern)
	RBX_ENUM_VALUE(MaterialPattern, Regular, 0)
	RBX_ENUM_VALUE(MaterialPattern, Organic, 1)
RBX_ENUM_END(MaterialPattern, Regular, Organic)

RBX_ENUM_BEGIN(MembershipType)
	RBX_ENUM_VALUE(MembershipType, None, 0)
	RBX_ENUM_VALUE(MembershipType, BuildersClub, 1)
	RBX_ENUM_VALUE(MembershipType, TurboBuildersClub, 2)
	RBX_ENUM_VALUE(MembershipType, OutrageousBuildersClub, 3)
	RBX_ENUM_VALUE(MembershipType, Premium, 4)
RBX_ENUM_END(MembershipType, None, BuildersClub, TurboBuildersClub, OutrageousBuildersClub, Premium)

RBX_ENUM_BEGIN(MeshPartDetailLevel)
	RBX_ENUM_VALUE(MeshPartDetailLevel, DistanceBased, 0)
	RBX_ENUM_VALUE(MeshPartDetailLevel, Level00, 1)
	RBX_ENUM_VALUE(MeshPartDetailLevel, Level01, 2)
	RBX_ENUM_VALUE(MeshPartDetailLevel, Level02, 3)
	RBX_ENUM_VALUE(MeshPartDetailLevel, Level03, 4)
	RBX_ENUM_VALUE(MeshPartDetailLevel, Level04, 5)
	RBX_ENUM_VALUE(MeshPartDetailLevel, Level05, 6)
	RBX_ENUM_VALUE(MeshPartDetailLevel, Level06, 7)
	RBX_ENUM_VALUE(MeshPartDetailLevel, Level07, 8)
	RBX_ENUM_VALUE(MeshPartDetailLevel, Level08, 9)
	RBX_ENUM_VALUE(MeshPartDetailLevel, Level09, 10)
RBX_ENUM_END(MeshPartDetailLevel, DistanceBased, Level00, Level01, Level02, Level03, Level04, Level05, Level06, Level07, Level08, Level09)

RBX_ENUM_BEGIN(MeshPartHeadsAndAccessories)
	RBX_ENUM_VALUE(MeshPartHeadsAndAccessories, Default, 0)
	RBX_ENUM_VALUE(MeshPartHeadsAndAccessories, Disabled, 1)
	RBX_ENUM_VALUE(MeshPartHeadsAndAccessories, Enabled, 2)
RBX_ENUM_END(MeshPartHeadsAndAccessories, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(MeshScaleUnit)
	RBX_ENUM_VALUE(MeshScaleUnit, Stud, 0)
	RBX_ENUM_VALUE(MeshScaleUnit, Meter, 1)
	RBX_ENUM_VALUE(MeshScaleUnit, CM, 2)
	RBX_ENUM_VALUE(MeshScaleUnit, MM, 3)
	RBX_ENUM_VALUE(MeshScaleUnit, Foot, 4)
	RBX_ENUM_VALUE(MeshScaleUnit, Inch, 5)
RBX_ENUM_END(MeshScaleUnit, Stud, Meter, CM, MM, Foot, Inch)

RBX_ENUM_BEGIN(MeshType)
	RBX_ENUM_VALUE(MeshType, Head, 0)
	RBX_ENUM_VALUE(MeshType, Torso, 1)
	RBX_ENUM_VALUE(MeshType, Wedge, 2)
	RBX_ENUM_VALUE(MeshType, Sphere, 3)
	RBX_ENUM_VALUE(MeshType, Cylinder, 4)
	RBX_ENUM_VALUE(MeshType, FileMesh, 5)
	RBX_ENUM_VALUE(MeshType, Brick, 6)
	RBX_ENUM_VALUE(MeshType, Prism, 7)
	RBX_ENUM_VALUE(MeshType, Pyramid, 8)
	RBX_ENUM_VALUE(MeshType, ParallelRamp, 9)
	RBX_ENUM_VALUE(MeshType, RightAngleRamp, 10)
	RBX_ENUM_VALUE(MeshType, CornerWedge, 11)
RBX_ENUM_END(MeshType, Head, Torso, Wedge, Sphere, Cylinder, FileMesh, Brick, Prism, Pyramid, ParallelRamp, RightAngleRamp, CornerWedge)

RBX_ENUM_BEGIN(MessageType)
	RBX_ENUM_VALUE(MessageType, MessageOutput, 0)
	RBX_ENUM_VALUE(MessageType, MessageInfo, 1)
	RBX_ENUM_VALUE(MessageType, MessageWarning, 2)
	RBX_ENUM_VALUE(MessageType, MessageError, 3)
RBX_ENUM_END(MessageType, MessageOutput, MessageInfo, MessageWarning, MessageError)

RBX_ENUM_BEGIN(ModelLevelOfDetail)
	RBX_ENUM_VALUE(ModelLevelOfDetail, Automatic, 0)
	RBX_ENUM_VALUE(ModelLevelOfDetail, StreamingMesh, 1)
	RBX_ENUM_VALUE(ModelLevelOfDetail, Disabled, 2)
	RBX_ENUM_VALUE(ModelLevelOfDetail, SLIM, 4)
RBX_ENUM_END(ModelLevelOfDetail, Automatic, StreamingMesh, Disabled, SLIM)

RBX_ENUM_BEGIN(ModelStreamingBehavior)
	RBX_ENUM_VALUE(ModelStreamingBehavior, Default, 0)
	RBX_ENUM_VALUE(ModelStreamingBehavior, Legacy, 1)
	RBX_ENUM_VALUE(ModelStreamingBehavior, Improved, 2)
RBX_ENUM_END(ModelStreamingBehavior, Default, Legacy, Improved)

RBX_ENUM_BEGIN(ModelStreamingMode)
	RBX_ENUM_VALUE(ModelStreamingMode, Default, 0)
	RBX_ENUM_VALUE(ModelStreamingMode, Atomic, 1)
	RBX_ENUM_VALUE(ModelStreamingMode, Persistent, 2)
	RBX_ENUM_VALUE(ModelStreamingMode, PersistentPerPlayer, 3)
	RBX_ENUM_VALUE(ModelStreamingMode, Nonatomic, 4)
RBX_ENUM_END(ModelStreamingMode, Default, Atomic, Persistent, PersistentPerPlayer, Nonatomic)

RBX_ENUM_BEGIN(ModerationResultCategory)
	RBX_ENUM_VALUE(ModerationResultCategory, ViolationDetected, 0)
	RBX_ENUM_VALUE(ModerationResultCategory, Borderline, 1)
	RBX_ENUM_VALUE(ModerationResultCategory, NoViolationDetected, 2)
RBX_ENUM_END(ModerationResultCategory, ViolationDetected, Borderline, NoViolationDetected)

RBX_ENUM_BEGIN(ModerationResultLabel)
	RBX_ENUM_VALUE(ModerationResultLabel, ChildExploitation, 0)
	RBX_ENUM_VALUE(ModerationResultLabel, SuicideSelfInjuryAndHarmfulBehavior, 1)
	RBX_ENUM_VALUE(ModerationResultLabel, ThreatsBullyingAndHarassment, 2)
	RBX_ENUM_VALUE(ModerationResultLabel, TerrorismAndViolentExtremism, 3)
	RBX_ENUM_VALUE(ModerationResultLabel, DiscriminationSlursAndHateSpeech, 4)
	RBX_ENUM_VALUE(ModerationResultLabel, RealWorldSensitiveEvents, 5)
	RBX_ENUM_VALUE(ModerationResultLabel, ViolentContentAndGore, 6)
	RBX_ENUM_VALUE(ModerationResultLabel, RomanticAndSexualContent, 7)
	RBX_ENUM_VALUE(ModerationResultLabel, IllegalAndRegulatedGoodsAndActivities, 8)
	RBX_ENUM_VALUE(ModerationResultLabel, Profanity, 9)
	RBX_ENUM_VALUE(ModerationResultLabel, Other, 100)
RBX_ENUM_END(ModerationResultLabel, ChildExploitation, SuicideSelfInjuryAndHarmfulBehavior, ThreatsBullyingAndHarassment, TerrorismAndViolentExtremism, DiscriminationSlursAndHateSpeech, RealWorldSensitiveEvents, ViolentContentAndGore, RomanticAndSexualContent, IllegalAndRegulatedGoodsAndActivities, Profanity, Other)

RBX_ENUM_BEGIN(ModerationStatus)
	RBX_ENUM_VALUE(ModerationStatus, ReviewedApproved, 1)
	RBX_ENUM_VALUE(ModerationStatus, ReviewedRejected, 2)
	RBX_ENUM_VALUE(ModerationStatus, NotReviewed, 3)
	RBX_ENUM_VALUE(ModerationStatus, NotApplicable, 4)
	RBX_ENUM_VALUE(ModerationStatus, Invalid, 5)
RBX_ENUM_END(ModerationStatus, ReviewedApproved, ReviewedRejected, NotReviewed, NotApplicable, Invalid)

RBX_ENUM_BEGIN(ModifierKey)
	RBX_ENUM_VALUE(ModifierKey, Shift, 0)
	RBX_ENUM_VALUE(ModifierKey, Ctrl, 1)
	RBX_ENUM_VALUE(ModifierKey, Alt, 2)
	RBX_ENUM_VALUE(ModifierKey, Meta, 3)
RBX_ENUM_END(ModifierKey, Shift, Ctrl, Alt, Meta)

RBX_ENUM_BEGIN(MouseBehavior)
	RBX_ENUM_VALUE(MouseBehavior, Default, 0)
	RBX_ENUM_VALUE(MouseBehavior, LockCenter, 1)
	RBX_ENUM_VALUE(MouseBehavior, LockCurrentPosition, 2)
RBX_ENUM_END(MouseBehavior, Default, LockCenter, LockCurrentPosition)

RBX_ENUM_BEGIN(MoveState)
	RBX_ENUM_VALUE(MoveState, Stopped, 0)
	RBX_ENUM_VALUE(MoveState, Coasting, 1)
	RBX_ENUM_VALUE(MoveState, Pushing, 2)
	RBX_ENUM_VALUE(MoveState, Stopping, 3)
	RBX_ENUM_VALUE(MoveState, AirFree, 4)
RBX_ENUM_END(MoveState, Stopped, Coasting, Pushing, Stopping, AirFree)

RBX_ENUM_BEGIN(MuteState)
	RBX_ENUM_VALUE(MuteState, Unmuted, 0)
	RBX_ENUM_VALUE(MuteState, Muted, 1)
RBX_ENUM_END(MuteState, Unmuted, Muted)

RBX_ENUM_BEGIN(NameOcclusion)
	RBX_ENUM_VALUE(NameOcclusion, NoOcclusion, 0)
	RBX_ENUM_VALUE(NameOcclusion, EnemyOcclusion, 1)
	RBX_ENUM_VALUE(NameOcclusion, OccludeAll, 2)
RBX_ENUM_END(NameOcclusion, NoOcclusion, EnemyOcclusion, OccludeAll)

RBX_ENUM_BEGIN(NegateOperationHiddenHistory)
	RBX_ENUM_VALUE(NegateOperationHiddenHistory, None, 0)
	RBX_ENUM_VALUE(NegateOperationHiddenHistory, NegatedUnion, 1)
	RBX_ENUM_VALUE(NegateOperationHiddenHistory, NegatedIntersection, 2)
RBX_ENUM_END(NegateOperationHiddenHistory, None, NegatedUnion, NegatedIntersection)

RBX_ENUM_BEGIN(NetworkOwnership)
	RBX_ENUM_VALUE(NetworkOwnership, Automatic, 0)
	RBX_ENUM_VALUE(NetworkOwnership, Manual, 1)
	RBX_ENUM_VALUE(NetworkOwnership, OnContact, 2)
RBX_ENUM_END(NetworkOwnership, Automatic, Manual, OnContact)

RBX_ENUM_BEGIN(NetworkStatus)
	RBX_ENUM_VALUE(NetworkStatus, Unknown, 0)
	RBX_ENUM_VALUE(NetworkStatus, Connected, 1)
	RBX_ENUM_VALUE(NetworkStatus, Disconnected, 2)
RBX_ENUM_END(NetworkStatus, Unknown, Connected, Disconnected)

RBX_ENUM_BEGIN(NoiseType)
	RBX_ENUM_VALUE(NoiseType, SimplexGabor, 0)
RBX_ENUM_END(NoiseType, SimplexGabor)

RBX_ENUM_BEGIN(NormalId)
	RBX_ENUM_VALUE(NormalId, Right, 0)
	RBX_ENUM_VALUE(NormalId, Top, 1)
	RBX_ENUM_VALUE(NormalId, Back, 2)
	RBX_ENUM_VALUE(NormalId, Left, 3)
	RBX_ENUM_VALUE(NormalId, Bottom, 4)
	RBX_ENUM_VALUE(NormalId, Front, 5)
RBX_ENUM_END(NormalId, Right, Top, Back, Left, Bottom, Front)

RBX_ENUM_BEGIN(NotificationButtonType)
	RBX_ENUM_VALUE(NotificationButtonType, Primary, 0)
	RBX_ENUM_VALUE(NotificationButtonType, Secondary, 1)
RBX_ENUM_END(NotificationButtonType, Primary, Secondary)

RBX_ENUM_BEGIN(OperationType)
	RBX_ENUM_VALUE(OperationType, Null, 0)
	RBX_ENUM_VALUE(OperationType, Union, 1)
	RBX_ENUM_VALUE(OperationType, Subtraction, 2)
	RBX_ENUM_VALUE(OperationType, Intersection, 3)
	RBX_ENUM_VALUE(OperationType, Primitive, 4)
RBX_ENUM_END(OperationType, Null, Union, Subtraction, Intersection, Primitive)

RBX_ENUM_BEGIN(OrientationAlignmentMode)
	RBX_ENUM_VALUE(OrientationAlignmentMode, OneAttachment, 0)
	RBX_ENUM_VALUE(OrientationAlignmentMode, TwoAttachment, 1)
RBX_ENUM_END(OrientationAlignmentMode, OneAttachment, TwoAttachment)

RBX_ENUM_BEGIN(OutfitSource)
	RBX_ENUM_VALUE(OutfitSource, All, 1)
	RBX_ENUM_VALUE(OutfitSource, Created, 2)
	RBX_ENUM_VALUE(OutfitSource, Purchased, 3)
RBX_ENUM_END(OutfitSource, All, Created, Purchased)

RBX_ENUM_BEGIN(OutfitType)
	RBX_ENUM_VALUE(OutfitType, All, 1)
	RBX_ENUM_VALUE(OutfitType, Avatar, 2)
	RBX_ENUM_VALUE(OutfitType, DynamicHead, 3)
	RBX_ENUM_VALUE(OutfitType, Shoes, 4)
RBX_ENUM_END(OutfitType, All, Avatar, DynamicHead, Shoes)

RBX_ENUM_BEGIN(OutputLayoutMode)
	RBX_ENUM_VALUE(OutputLayoutMode, Horizontal, 0)
	RBX_ENUM_VALUE(OutputLayoutMode, Vertical, 1)
RBX_ENUM_END(OutputLayoutMode, Horizontal, Vertical)

RBX_ENUM_BEGIN(OverrideMouseIconBehavior)
	RBX_ENUM_VALUE(OverrideMouseIconBehavior, None, 0)
	RBX_ENUM_VALUE(OverrideMouseIconBehavior, ForceShow, 1)
	RBX_ENUM_VALUE(OverrideMouseIconBehavior, ForceHide, 2)
RBX_ENUM_END(OverrideMouseIconBehavior, None, ForceShow, ForceHide)

RBX_ENUM_BEGIN(PackagePermission)
	RBX_ENUM_VALUE(PackagePermission, None, 0)
	RBX_ENUM_VALUE(PackagePermission, NoAccess, 1)
	RBX_ENUM_VALUE(PackagePermission, Revoked, 2)
	RBX_ENUM_VALUE(PackagePermission, UseView, 3)
	RBX_ENUM_VALUE(PackagePermission, Edit, 4)
	RBX_ENUM_VALUE(PackagePermission, Own, 5)
RBX_ENUM_END(PackagePermission, None, NoAccess, Revoked, UseView, Edit, Own)

RBX_ENUM_BEGIN(PartType)
	RBX_ENUM_VALUE(PartType, Ball, 0)
	RBX_ENUM_VALUE(PartType, Block, 1)
	RBX_ENUM_VALUE(PartType, Cylinder, 2)
	RBX_ENUM_VALUE(PartType, Wedge, 3)
	RBX_ENUM_VALUE(PartType, CornerWedge, 4)
RBX_ENUM_END(PartType, Ball, Block, Cylinder, Wedge, CornerWedge)

RBX_ENUM_BEGIN(ParticleEmitterShape)
	RBX_ENUM_VALUE(ParticleEmitterShape, Box, 0)
	RBX_ENUM_VALUE(ParticleEmitterShape, Sphere, 1)
	RBX_ENUM_VALUE(ParticleEmitterShape, Cylinder, 2)
	RBX_ENUM_VALUE(ParticleEmitterShape, Disc, 3)
RBX_ENUM_END(ParticleEmitterShape, Box, Sphere, Cylinder, Disc)

RBX_ENUM_BEGIN(ParticleEmitterShapeInOut)
	RBX_ENUM_VALUE(ParticleEmitterShapeInOut, Outward, 0)
	RBX_ENUM_VALUE(ParticleEmitterShapeInOut, Inward, 1)
	RBX_ENUM_VALUE(ParticleEmitterShapeInOut, InAndOut, 2)
RBX_ENUM_END(ParticleEmitterShapeInOut, Outward, Inward, InAndOut)

RBX_ENUM_BEGIN(ParticleEmitterShapeStyle)
	RBX_ENUM_VALUE(ParticleEmitterShapeStyle, Volume, 0)
	RBX_ENUM_VALUE(ParticleEmitterShapeStyle, Surface, 1)
RBX_ENUM_END(ParticleEmitterShapeStyle, Volume, Surface)

RBX_ENUM_BEGIN(ParticleFlipbookLayout)
	RBX_ENUM_VALUE(ParticleFlipbookLayout, None, 0)
	RBX_ENUM_VALUE(ParticleFlipbookLayout, Grid2x2, 1)
	RBX_ENUM_VALUE(ParticleFlipbookLayout, Grid4x4, 2)
	RBX_ENUM_VALUE(ParticleFlipbookLayout, Grid8x8, 3)
	RBX_ENUM_VALUE(ParticleFlipbookLayout, Custom, 4)
RBX_ENUM_END(ParticleFlipbookLayout, None, Grid2x2, Grid4x4, Grid8x8, Custom)

RBX_ENUM_BEGIN(ParticleFlipbookMode)
	RBX_ENUM_VALUE(ParticleFlipbookMode, Loop, 0)
	RBX_ENUM_VALUE(ParticleFlipbookMode, OneShot, 1)
	RBX_ENUM_VALUE(ParticleFlipbookMode, PingPong, 2)
	RBX_ENUM_VALUE(ParticleFlipbookMode, Random, 3)
RBX_ENUM_END(ParticleFlipbookMode, Loop, OneShot, PingPong, Random)

RBX_ENUM_BEGIN(ParticleFlipbookTextureCompatible)
	RBX_ENUM_VALUE(ParticleFlipbookTextureCompatible, NotCompatible, 0)
	RBX_ENUM_VALUE(ParticleFlipbookTextureCompatible, Compatible, 1)
	RBX_ENUM_VALUE(ParticleFlipbookTextureCompatible, Unknown, 2)
RBX_ENUM_END(ParticleFlipbookTextureCompatible, NotCompatible, Compatible, Unknown)

RBX_ENUM_BEGIN(ParticleOrientation)
	RBX_ENUM_VALUE(ParticleOrientation, FacingCamera, 0)
	RBX_ENUM_VALUE(ParticleOrientation, FacingCameraWorldUp, 1)
	RBX_ENUM_VALUE(ParticleOrientation, VelocityParallel, 2)
	RBX_ENUM_VALUE(ParticleOrientation, VelocityPerpendicular, 3)
RBX_ENUM_END(ParticleOrientation, FacingCamera, FacingCameraWorldUp, VelocityParallel, VelocityPerpendicular)

RBX_ENUM_BEGIN(PathStatus)
	RBX_ENUM_VALUE(PathStatus, Success, 0)
	RBX_ENUM_VALUE(PathStatus, ClosestNoPath, 1)
	RBX_ENUM_VALUE(PathStatus, ClosestOutOfRange, 2)
	RBX_ENUM_VALUE(PathStatus, FailStartNotEmpty, 3)
	RBX_ENUM_VALUE(PathStatus, FailFinishNotEmpty, 4)
	RBX_ENUM_VALUE(PathStatus, NoPath, 5)
RBX_ENUM_END(PathStatus, Success, ClosestNoPath, ClosestOutOfRange, FailStartNotEmpty, FailFinishNotEmpty, NoPath)

RBX_ENUM_BEGIN(PathWaypointAction)
	RBX_ENUM_VALUE(PathWaypointAction, Walk, 0)
	RBX_ENUM_VALUE(PathWaypointAction, Jump, 1)
	RBX_ENUM_VALUE(PathWaypointAction, Custom, 2)
RBX_ENUM_END(PathWaypointAction, Walk, Jump, Custom)

RBX_ENUM_BEGIN(PathfindingUseImprovedSearch)
	RBX_ENUM_VALUE(PathfindingUseImprovedSearch, Default, 0)
	RBX_ENUM_VALUE(PathfindingUseImprovedSearch, Disabled, 1)
	RBX_ENUM_VALUE(PathfindingUseImprovedSearch, Enabled, 2)
RBX_ENUM_END(PathfindingUseImprovedSearch, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(PeoplePageLayout)
	RBX_ENUM_VALUE(PeoplePageLayout, Card, 0)
	RBX_ENUM_VALUE(PeoplePageLayout, List, 1)
RBX_ENUM_END(PeoplePageLayout, Card, List)

RBX_ENUM_BEGIN(PerformanceOverlayMode)
	RBX_ENUM_VALUE(PerformanceOverlayMode, Overdraw, 0)
	RBX_ENUM_VALUE(PerformanceOverlayMode, Transparent, 1)
	RBX_ENUM_VALUE(PerformanceOverlayMode, Decals, 2)
	RBX_ENUM_VALUE(PerformanceOverlayMode, Lights, 3)
RBX_ENUM_END(PerformanceOverlayMode, Overdraw, Transparent, Decals, Lights)

RBX_ENUM_BEGIN(PermissionLevelShown)
	RBX_ENUM_VALUE(PermissionLevelShown, Game, 0)
	RBX_ENUM_VALUE(PermissionLevelShown, RobloxGame, 1)
	RBX_ENUM_VALUE(PermissionLevelShown, RobloxScript, 2)
	RBX_ENUM_VALUE(PermissionLevelShown, Studio, 3)
	RBX_ENUM_VALUE(PermissionLevelShown, Roblox, 4)
RBX_ENUM_END(PermissionLevelShown, Game, RobloxGame, RobloxScript, Studio, Roblox)

RBX_ENUM_BEGIN(PhysicalConstraintType)
	RBX_ENUM_VALUE(PhysicalConstraintType, AnimationConstraint, 0)
	RBX_ENUM_VALUE(PhysicalConstraintType, Motor6D, 1)
RBX_ENUM_END(PhysicalConstraintType, AnimationConstraint, Motor6D)

RBX_ENUM_BEGIN(PhysicsSimulationRate)
	RBX_ENUM_VALUE(PhysicsSimulationRate, Fixed240Hz, 0)
	RBX_ENUM_VALUE(PhysicsSimulationRate, Fixed120Hz, 1)
	RBX_ENUM_VALUE(PhysicsSimulationRate, Fixed60Hz, 2)
RBX_ENUM_END(PhysicsSimulationRate, Fixed240Hz, Fixed120Hz, Fixed60Hz)

RBX_ENUM_BEGIN(PhysicsSteppingMethod)
	RBX_ENUM_VALUE(PhysicsSteppingMethod, Default, 0)
	RBX_ENUM_VALUE(PhysicsSteppingMethod, Fixed, 1)
	RBX_ENUM_VALUE(PhysicsSteppingMethod, Adaptive, 2)
RBX_ENUM_END(PhysicsSteppingMethod, Default, Fixed, Adaptive)

RBX_ENUM_BEGIN(PlaceContentPreference)
	RBX_ENUM_VALUE(PlaceContentPreference, None, 0)
	RBX_ENUM_VALUE(PlaceContentPreference, All, 1)
	RBX_ENUM_VALUE(PlaceContentPreference, MentionsAndReplies, 2)
	RBX_ENUM_VALUE(PlaceContentPreference, Unknown, 3)
RBX_ENUM_END(PlaceContentPreference, None, All, MentionsAndReplies, Unknown)

RBX_ENUM_BEGIN(PlacePublishType)
	RBX_ENUM_VALUE(PlacePublishType, None, 0)
	RBX_ENUM_VALUE(PlacePublishType, Publish, 1)
	RBX_ENUM_VALUE(PlacePublishType, Save, 2)
RBX_ENUM_END(PlacePublishType, None, Publish, Save)

RBX_ENUM_BEGIN(Platform)
	RBX_ENUM_VALUE(Platform, Windows, 0)
	RBX_ENUM_VALUE(Platform, OSX, 1)
	RBX_ENUM_VALUE(Platform, IOS, 2)
	RBX_ENUM_VALUE(Platform, Android, 3)
	RBX_ENUM_VALUE(Platform, XBoxOne, 4)
	RBX_ENUM_VALUE(Platform, PS4, 5)
	RBX_ENUM_VALUE(Platform, PS3, 6)
	RBX_ENUM_VALUE(Platform, XBox360, 7)
	RBX_ENUM_VALUE(Platform, WiiU, 8)
	RBX_ENUM_VALUE(Platform, NX, 9)
	RBX_ENUM_VALUE(Platform, Ouya, 10)
	RBX_ENUM_VALUE(Platform, AndroidTV, 11)
	RBX_ENUM_VALUE(Platform, Chromecast, 12)
	RBX_ENUM_VALUE(Platform, Linux, 13)
	RBX_ENUM_VALUE(Platform, SteamOS, 14)
	RBX_ENUM_VALUE(Platform, WebOS, 15)
	RBX_ENUM_VALUE(Platform, DOS, 16)
	RBX_ENUM_VALUE(Platform, BeOS, 17)
	RBX_ENUM_VALUE(Platform, UWP, 18)
	RBX_ENUM_VALUE(Platform, PS5, 19)
	RBX_ENUM_VALUE(Platform, MetaOS, 20)
	RBX_ENUM_VALUE(Platform, Web, 21)
	RBX_ENUM_VALUE(Platform, None, 22)
RBX_ENUM_END(Platform, Windows, OSX, IOS, Android, XBoxOne, PS4, PS3, XBox360, WiiU, NX, Ouya, AndroidTV, Chromecast, Linux, SteamOS, WebOS, DOS, BeOS, UWP, PS5, MetaOS, Web, None)

RBX_ENUM_BEGIN(PlaybackState)
	RBX_ENUM_VALUE(PlaybackState, Begin, 0)
	RBX_ENUM_VALUE(PlaybackState, Delayed, 1)
	RBX_ENUM_VALUE(PlaybackState, Playing, 2)
	RBX_ENUM_VALUE(PlaybackState, Paused, 3)
	RBX_ENUM_VALUE(PlaybackState, Completed, 4)
	RBX_ENUM_VALUE(PlaybackState, Cancelled, 5)
RBX_ENUM_END(PlaybackState, Begin, Delayed, Playing, Paused, Completed, Cancelled)

RBX_ENUM_BEGIN(PlayerActions)
	RBX_ENUM_VALUE(PlayerActions, CharacterForward, 0)
	RBX_ENUM_VALUE(PlayerActions, CharacterBackward, 1)
	RBX_ENUM_VALUE(PlayerActions, CharacterLeft, 2)
	RBX_ENUM_VALUE(PlayerActions, CharacterRight, 3)
	RBX_ENUM_VALUE(PlayerActions, CharacterJump, 4)
RBX_ENUM_END(PlayerActions, CharacterForward, CharacterBackward, CharacterLeft, CharacterRight, CharacterJump)

RBX_ENUM_BEGIN(PlayerCharacterDestroyBehavior)
	RBX_ENUM_VALUE(PlayerCharacterDestroyBehavior, Default, 0)
	RBX_ENUM_VALUE(PlayerCharacterDestroyBehavior, Disabled, 1)
	RBX_ENUM_VALUE(PlayerCharacterDestroyBehavior, Enabled, 2)
RBX_ENUM_END(PlayerCharacterDestroyBehavior, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(PlayerChatType)
	RBX_ENUM_VALUE(PlayerChatType, All, 0)
	RBX_ENUM_VALUE(PlayerChatType, Team, 1)
	RBX_ENUM_VALUE(PlayerChatType, Whisper, 2)
RBX_ENUM_END(PlayerChatType, All, Team, Whisper)

RBX_ENUM_BEGIN(PlayerDataErrorState)
	RBX_ENUM_VALUE(PlayerDataErrorState, LoadFailed, 0)
	RBX_ENUM_VALUE(PlayerDataErrorState, FlushFailed, 1)
	RBX_ENUM_VALUE(PlayerDataErrorState, ReleaseFailed, 2)
	RBX_ENUM_VALUE(PlayerDataErrorState, None, 3)
RBX_ENUM_END(PlayerDataErrorState, LoadFailed, FlushFailed, ReleaseFailed, None)

RBX_ENUM_BEGIN(PlayerDataLoadFailureBehavior)
	RBX_ENUM_VALUE(PlayerDataLoadFailureBehavior, Failure, 0)
	RBX_ENUM_VALUE(PlayerDataLoadFailureBehavior, FallbackToDefault, 1)
	RBX_ENUM_VALUE(PlayerDataLoadFailureBehavior, Kick, 2)
RBX_ENUM_END(PlayerDataLoadFailureBehavior, Failure, FallbackToDefault, Kick)

RBX_ENUM_BEGIN(PlayerExitReason)
	RBX_ENUM_VALUE(PlayerExitReason, Unknown, 0)
	RBX_ENUM_VALUE(PlayerExitReason, PlatformKick, 1)
	RBX_ENUM_VALUE(PlayerExitReason, CreatorKick, 2)
RBX_ENUM_END(PlayerExitReason, Unknown, PlatformKick, CreatorKick)

RBX_ENUM_BEGIN(PlayerPlatformSpenderStatus)
	RBX_ENUM_VALUE(PlayerPlatformSpenderStatus, Unknown, 0)
	RBX_ENUM_VALUE(PlayerPlatformSpenderStatus, Active, 1)
	RBX_ENUM_VALUE(PlayerPlatformSpenderStatus, OtherPayer, 2)
RBX_ENUM_END(PlayerPlatformSpenderStatus, Unknown, Active, OtherPayer)

RBX_ENUM_BEGIN(PluginConnectionTargetType)
	RBX_ENUM_VALUE(PluginConnectionTargetType, Edit, 0)
	RBX_ENUM_VALUE(PluginConnectionTargetType, Test, 1)
RBX_ENUM_END(PluginConnectionTargetType, Edit, Test)

RBX_ENUM_BEGIN(PoseEasingDirection)
	RBX_ENUM_VALUE(PoseEasingDirection, In, 0)
	RBX_ENUM_VALUE(PoseEasingDirection, Out, 1)
	RBX_ENUM_VALUE(PoseEasingDirection, InOut, 2)
RBX_ENUM_END(PoseEasingDirection, In, Out, InOut)

RBX_ENUM_BEGIN(PoseEasingStyle)
	RBX_ENUM_VALUE(PoseEasingStyle, Linear, 0)
	RBX_ENUM_VALUE(PoseEasingStyle, Constant, 1)
	RBX_ENUM_VALUE(PoseEasingStyle, Elastic, 2)
	RBX_ENUM_VALUE(PoseEasingStyle, Cubic, 3)
	RBX_ENUM_VALUE(PoseEasingStyle, Bounce, 4)
	RBX_ENUM_VALUE(PoseEasingStyle, CubicV2, 5)
RBX_ENUM_END(PoseEasingStyle, Linear, Constant, Elastic, Cubic, Bounce, CubicV2)

RBX_ENUM_BEGIN(PositionAlignmentMode)
	RBX_ENUM_VALUE(PositionAlignmentMode, OneAttachment, 0)
	RBX_ENUM_VALUE(PositionAlignmentMode, TwoAttachment, 1)
RBX_ENUM_END(PositionAlignmentMode, OneAttachment, TwoAttachment)

RBX_ENUM_BEGIN(PredictionMode)
	RBX_ENUM_VALUE(PredictionMode, Automatic, 0)
	RBX_ENUM_VALUE(PredictionMode, On, 1)
	RBX_ENUM_VALUE(PredictionMode, Off, 2)
RBX_ENUM_END(PredictionMode, Automatic, On, Off)

RBX_ENUM_BEGIN(PredictionStatus)
	RBX_ENUM_VALUE(PredictionStatus, Authoritative, 0)
	RBX_ENUM_VALUE(PredictionStatus, Predicted, 1)
	RBX_ENUM_VALUE(PredictionStatus, None, 2)
RBX_ENUM_END(PredictionStatus, Authoritative, Predicted, None)

RBX_ENUM_BEGIN(PreferredInput)
	RBX_ENUM_VALUE(PreferredInput, KeyboardAndMouse, 0)
	RBX_ENUM_VALUE(PreferredInput, Gamepad, 1)
	RBX_ENUM_VALUE(PreferredInput, Touch, 2)
RBX_ENUM_END(PreferredInput, KeyboardAndMouse, Gamepad, Touch)

RBX_ENUM_BEGIN(PreferredTextSize)
	RBX_ENUM_VALUE(PreferredTextSize, Medium, 1)
	RBX_ENUM_VALUE(PreferredTextSize, Large, 2)
	RBX_ENUM_VALUE(PreferredTextSize, Larger, 3)
	RBX_ENUM_VALUE(PreferredTextSize, Largest, 4)
RBX_ENUM_END(PreferredTextSize, Medium, Large, Larger, Largest)

RBX_ENUM_BEGIN(PrefetchDownloadStatus)
	RBX_ENUM_VALUE(PrefetchDownloadStatus, NotStarted, 0)
	RBX_ENUM_VALUE(PrefetchDownloadStatus, InProgress, 1)
	RBX_ENUM_VALUE(PrefetchDownloadStatus, Completed, 2)
	RBX_ENUM_VALUE(PrefetchDownloadStatus, Failed, 3)
RBX_ENUM_END(PrefetchDownloadStatus, NotStarted, InProgress, Completed, Failed)

RBX_ENUM_BEGIN(PrimalPhysicsSolver)
	RBX_ENUM_VALUE(PrimalPhysicsSolver, Default, 0)
	RBX_ENUM_VALUE(PrimalPhysicsSolver, Experimental, 1)
	RBX_ENUM_VALUE(PrimalPhysicsSolver, Disabled, 2)
RBX_ENUM_END(PrimalPhysicsSolver, Default, Experimental, Disabled)

RBX_ENUM_BEGIN(PrimitiveType)
	RBX_ENUM_VALUE(PrimitiveType, Null, 0)
	RBX_ENUM_VALUE(PrimitiveType, Ball, 1)
	RBX_ENUM_VALUE(PrimitiveType, Cylinder, 2)
	RBX_ENUM_VALUE(PrimitiveType, Block, 3)
	RBX_ENUM_VALUE(PrimitiveType, Wedge, 4)
	RBX_ENUM_VALUE(PrimitiveType, CornerWedge, 5)
RBX_ENUM_END(PrimitiveType, Null, Ball, Cylinder, Block, Wedge, CornerWedge)

RBX_ENUM_BEGIN(PrivilegeType)
	RBX_ENUM_VALUE(PrivilegeType, Banned, 0)
	RBX_ENUM_VALUE(PrivilegeType, Visitor, 10)
	RBX_ENUM_VALUE(PrivilegeType, Member, 128)
	RBX_ENUM_VALUE(PrivilegeType, Admin, 240)
	RBX_ENUM_VALUE(PrivilegeType, Owner, 255)
RBX_ENUM_END(PrivilegeType, Banned, Visitor, Member, Admin, Owner)

RBX_ENUM_BEGIN(ProductLocationRestriction)
	RBX_ENUM_VALUE(ProductLocationRestriction, AvatarShop, 0)
	RBX_ENUM_VALUE(ProductLocationRestriction, AllowedGames, 1)
	RBX_ENUM_VALUE(ProductLocationRestriction, AllGames, 2)
RBX_ENUM_END(ProductLocationRestriction, AvatarShop, AllowedGames, AllGames)

RBX_ENUM_BEGIN(ProductPurchaseChannel)
	RBX_ENUM_VALUE(ProductPurchaseChannel, InExperience, 1)
	RBX_ENUM_VALUE(ProductPurchaseChannel, ExperienceDetailsPage, 2)
	RBX_ENUM_VALUE(ProductPurchaseChannel, AdReward, 3)
	RBX_ENUM_VALUE(ProductPurchaseChannel, CommerceProduct, 4)
RBX_ENUM_END(ProductPurchaseChannel, InExperience, ExperienceDetailsPage, AdReward, CommerceProduct)

RBX_ENUM_BEGIN(ProductPurchaseDecision)
	RBX_ENUM_VALUE(ProductPurchaseDecision, NotProcessedYet, 0)
	RBX_ENUM_VALUE(ProductPurchaseDecision, PurchaseGranted, 1)
RBX_ENUM_END(ProductPurchaseDecision, NotProcessedYet, PurchaseGranted)

RBX_ENUM_BEGIN(PromptCreateAssetResult)
	RBX_ENUM_VALUE(PromptCreateAssetResult, Success, 1)
	RBX_ENUM_VALUE(PromptCreateAssetResult, PermissionDenied, 2)
	RBX_ENUM_VALUE(PromptCreateAssetResult, Timeout, 3)
	RBX_ENUM_VALUE(PromptCreateAssetResult, UploadFailed, 4)
	RBX_ENUM_VALUE(PromptCreateAssetResult, NoUserInput, 5)
	RBX_ENUM_VALUE(PromptCreateAssetResult, UnknownFailure, 6)
	RBX_ENUM_VALUE(PromptCreateAssetResult, UGCValidationFailed, 7)
	RBX_ENUM_VALUE(PromptCreateAssetResult, ModeratedName, 8)
	RBX_ENUM_VALUE(PromptCreateAssetResult, PurchaseFailure, 9)
	RBX_ENUM_VALUE(PromptCreateAssetResult, TokenInvalid, 10)
RBX_ENUM_END(PromptCreateAssetResult, Success, PermissionDenied, Timeout, UploadFailed, NoUserInput, UnknownFailure, UGCValidationFailed, ModeratedName, PurchaseFailure, TokenInvalid)

RBX_ENUM_BEGIN(PromptCreateAvatarResult)
	RBX_ENUM_VALUE(PromptCreateAvatarResult, Success, 1)
	RBX_ENUM_VALUE(PromptCreateAvatarResult, PermissionDenied, 2)
	RBX_ENUM_VALUE(PromptCreateAvatarResult, Timeout, 3)
	RBX_ENUM_VALUE(PromptCreateAvatarResult, UploadFailed, 4)
	RBX_ENUM_VALUE(PromptCreateAvatarResult, NoUserInput, 5)
	RBX_ENUM_VALUE(PromptCreateAvatarResult, InvalidHumanoidDescription, 6)
	RBX_ENUM_VALUE(PromptCreateAvatarResult, UGCValidationFailed, 7)
	RBX_ENUM_VALUE(PromptCreateAvatarResult, ModeratedName, 8)
	RBX_ENUM_VALUE(PromptCreateAvatarResult, MaxOutfits, 9)
	RBX_ENUM_VALUE(PromptCreateAvatarResult, PurchaseFailure, 10)
	RBX_ENUM_VALUE(PromptCreateAvatarResult, UnknownFailure, 11)
	RBX_ENUM_VALUE(PromptCreateAvatarResult, TokenInvalid, 12)
RBX_ENUM_END(PromptCreateAvatarResult, Success, PermissionDenied, Timeout, UploadFailed, NoUserInput, InvalidHumanoidDescription, UGCValidationFailed, ModeratedName, MaxOutfits, PurchaseFailure, UnknownFailure, TokenInvalid)

RBX_ENUM_BEGIN(PromptExperienceDetailsResult)
	RBX_ENUM_VALUE(PromptExperienceDetailsResult, PromptClosed, 0)
	RBX_ENUM_VALUE(PromptExperienceDetailsResult, TeleportAttempted, 1)
RBX_ENUM_END(PromptExperienceDetailsResult, PromptClosed, TeleportAttempted)

RBX_ENUM_BEGIN(PromptLinkSharingResult)
	RBX_ENUM_VALUE(PromptLinkSharingResult, Success, 1)
	RBX_ENUM_VALUE(PromptLinkSharingResult, PlayerLeft, 2)
	RBX_ENUM_VALUE(PromptLinkSharingResult, InvalidLaunchData, 3)
RBX_ENUM_END(PromptLinkSharingResult, Success, PlayerLeft, InvalidLaunchData)

RBX_ENUM_BEGIN(PromptPublishAssetResult)
	RBX_ENUM_VALUE(PromptPublishAssetResult, Success, 1)
	RBX_ENUM_VALUE(PromptPublishAssetResult, PermissionDenied, 2)
	RBX_ENUM_VALUE(PromptPublishAssetResult, Timeout, 3)
	RBX_ENUM_VALUE(PromptPublishAssetResult, UploadFailed, 4)
	RBX_ENUM_VALUE(PromptPublishAssetResult, NoUserInput, 5)
	RBX_ENUM_VALUE(PromptPublishAssetResult, UnknownFailure, 6)
RBX_ENUM_END(PromptPublishAssetResult, Success, PermissionDenied, Timeout, UploadFailed, NoUserInput, UnknownFailure)

RBX_ENUM_BEGIN(PropertyStatus)
	RBX_ENUM_VALUE(PropertyStatus, Ok, 0)
	RBX_ENUM_VALUE(PropertyStatus, Warning, 1)
	RBX_ENUM_VALUE(PropertyStatus, Error, 2)
RBX_ENUM_END(PropertyStatus, Ok, Warning, Error)

RBX_ENUM_BEGIN(ProximityPromptExclusivity)
	RBX_ENUM_VALUE(ProximityPromptExclusivity, OnePerButton, 0)
	RBX_ENUM_VALUE(ProximityPromptExclusivity, OneGlobally, 1)
	RBX_ENUM_VALUE(ProximityPromptExclusivity, AlwaysShow, 2)
RBX_ENUM_END(ProximityPromptExclusivity, OnePerButton, OneGlobally, AlwaysShow)

RBX_ENUM_BEGIN(ProximityPromptInputType)
	RBX_ENUM_VALUE(ProximityPromptInputType, Keyboard, 0)
	RBX_ENUM_VALUE(ProximityPromptInputType, Gamepad, 1)
	RBX_ENUM_VALUE(ProximityPromptInputType, Touch, 2)
RBX_ENUM_END(ProximityPromptInputType, Keyboard, Gamepad, Touch)

RBX_ENUM_BEGIN(ProximityPromptStyle)
	RBX_ENUM_VALUE(ProximityPromptStyle, Default, 0)
	RBX_ENUM_VALUE(ProximityPromptStyle, Custom, 1)
RBX_ENUM_END(ProximityPromptStyle, Default, Custom)

RBX_ENUM_BEGIN(PurchaseOption)
	RBX_ENUM_VALUE(PurchaseOption, TimedOption, 1)
	RBX_ENUM_VALUE(PurchaseOption, Permanent, 2)
RBX_ENUM_END(PurchaseOption, TimedOption, Permanent)

RBX_ENUM_BEGIN(QualityLevel)
	RBX_ENUM_VALUE(QualityLevel, Automatic, 0)
	RBX_ENUM_VALUE(QualityLevel, Level01, 1)
	RBX_ENUM_VALUE(QualityLevel, Level02, 2)
	RBX_ENUM_VALUE(QualityLevel, Level03, 3)
	RBX_ENUM_VALUE(QualityLevel, Level04, 4)
	RBX_ENUM_VALUE(QualityLevel, Level05, 5)
	RBX_ENUM_VALUE(QualityLevel, Level06, 6)
	RBX_ENUM_VALUE(QualityLevel, Level07, 7)
	RBX_ENUM_VALUE(QualityLevel, Level08, 8)
	RBX_ENUM_VALUE(QualityLevel, Level09, 9)
	RBX_ENUM_VALUE(QualityLevel, Level10, 10)
	RBX_ENUM_VALUE(QualityLevel, Level11, 11)
	RBX_ENUM_VALUE(QualityLevel, Level12, 12)
	RBX_ENUM_VALUE(QualityLevel, Level13, 13)
	RBX_ENUM_VALUE(QualityLevel, Level14, 14)
	RBX_ENUM_VALUE(QualityLevel, Level15, 15)
	RBX_ENUM_VALUE(QualityLevel, Level16, 16)
	RBX_ENUM_VALUE(QualityLevel, Level17, 17)
	RBX_ENUM_VALUE(QualityLevel, Level18, 18)
	RBX_ENUM_VALUE(QualityLevel, Level19, 19)
	RBX_ENUM_VALUE(QualityLevel, Level20, 20)
	RBX_ENUM_VALUE(QualityLevel, Level21, 21)
RBX_ENUM_END(QualityLevel, Automatic, Level01, Level02, Level03, Level04, Level05, Level06, Level07, Level08, Level09, Level10, Level11, Level12, Level13, Level14, Level15, Level16, Level17, Level18, Level19, Level20, Level21)

RBX_ENUM_BEGIN(R15CollisionType)
	RBX_ENUM_VALUE(R15CollisionType, OuterBox, 0)
	RBX_ENUM_VALUE(R15CollisionType, InnerBox, 1)
RBX_ENUM_END(R15CollisionType, OuterBox, InnerBox)

RBX_ENUM_BEGIN(RaycastFilterType)
	RBX_ENUM_VALUE(RaycastFilterType, Exclude, 0)
	RBX_ENUM_VALUE(RaycastFilterType, Include, 1)
RBX_ENUM_END(RaycastFilterType, Exclude, Include)

RBX_ENUM_BEGIN(ReadCapturesFromGalleryResult)
	RBX_ENUM_VALUE(ReadCapturesFromGalleryResult, Success, 0)
	RBX_ENUM_VALUE(ReadCapturesFromGalleryResult, NeedPermission, 1)
RBX_ENUM_END(ReadCapturesFromGalleryResult, Success, NeedPermission)

RBX_ENUM_BEGIN(ReceiptDecision)
	RBX_ENUM_VALUE(ReceiptDecision, NotProcessedYet, 0)
	RBX_ENUM_VALUE(ReceiptDecision, Processed, 1)
RBX_ENUM_END(ReceiptDecision, NotProcessedYet, Processed)

RBX_ENUM_BEGIN(ReceiptType)
	RBX_ENUM_VALUE(ReceiptType, DeveloperProduct, 0)
	RBX_ENUM_VALUE(ReceiptType, RobuxTransferSender, 1)
	RBX_ENUM_VALUE(ReceiptType, RobuxTransferReceiver, 2)
RBX_ENUM_END(ReceiptType, DeveloperProduct, RobuxTransferSender, RobuxTransferReceiver)

RBX_ENUM_BEGIN(RecommendationActionType)
	RBX_ENUM_VALUE(RecommendationActionType, AddReaction, 0)
	RBX_ENUM_VALUE(RecommendationActionType, RemoveReaction, 1)
	RBX_ENUM_VALUE(RecommendationActionType, Share, 2)
	RBX_ENUM_VALUE(RecommendationActionType, Report, 3)
	RBX_ENUM_VALUE(RecommendationActionType, Comment, 4)
	RBX_ENUM_VALUE(RecommendationActionType, Play, 5)
	RBX_ENUM_VALUE(RecommendationActionType, Purchase, 6)
RBX_ENUM_END(RecommendationActionType, AddReaction, RemoveReaction, Share, Report, Comment, Play, Purchase)

RBX_ENUM_BEGIN(RecommendationDepartureIntent)
	RBX_ENUM_VALUE(RecommendationDepartureIntent, Neutral, 0)
	RBX_ENUM_VALUE(RecommendationDepartureIntent, Positive, 1)
	RBX_ENUM_VALUE(RecommendationDepartureIntent, Negative, 2)
RBX_ENUM_END(RecommendationDepartureIntent, Neutral, Positive, Negative)

RBX_ENUM_BEGIN(RecommendationImpressionType)
	RBX_ENUM_VALUE(RecommendationImpressionType, View, 0)
	RBX_ENUM_VALUE(RecommendationImpressionType, NotViewable, 1)
RBX_ENUM_END(RecommendationImpressionType, View, NotViewable)

RBX_ENUM_BEGIN(RecommendationItemContentType)
	RBX_ENUM_VALUE(RecommendationItemContentType, Static, 0)
	RBX_ENUM_VALUE(RecommendationItemContentType, Dynamic, 1)
	RBX_ENUM_VALUE(RecommendationItemContentType, Interactive, 2)
RBX_ENUM_END(RecommendationItemContentType, Static, Dynamic, Interactive)

RBX_ENUM_BEGIN(RecommendationItemVisibility)
	RBX_ENUM_VALUE(RecommendationItemVisibility, Private, 0)
	RBX_ENUM_VALUE(RecommendationItemVisibility, Public, 1)
RBX_ENUM_END(RecommendationItemVisibility, Private, Public)

RBX_ENUM_BEGIN(RecommendationPreferenceTargetType)
	RBX_ENUM_VALUE(RecommendationPreferenceTargetType, User, 0)
	RBX_ENUM_VALUE(RecommendationPreferenceTargetType, Universe, 1)
	RBX_ENUM_VALUE(RecommendationPreferenceTargetType, CustomTag, 2)
RBX_ENUM_END(RecommendationPreferenceTargetType, User, Universe, CustomTag)

RBX_ENUM_BEGIN(RecommendationPreferenceType)
	RBX_ENUM_VALUE(RecommendationPreferenceType, AddFollow, 0)
	RBX_ENUM_VALUE(RecommendationPreferenceType, RemoveFollow, 1)
	RBX_ENUM_VALUE(RecommendationPreferenceType, AddMute, 2)
	RBX_ENUM_VALUE(RecommendationPreferenceType, RemoveMute, 3)
RBX_ENUM_END(RecommendationPreferenceType, AddFollow, RemoveFollow, AddMute, RemoveMute)

RBX_ENUM_BEGIN(RejectCharacterDeletions)
	RBX_ENUM_VALUE(RejectCharacterDeletions, Default, 0)
	RBX_ENUM_VALUE(RejectCharacterDeletions, Disabled, 1)
	RBX_ENUM_VALUE(RejectCharacterDeletions, Enabled, 2)
RBX_ENUM_END(RejectCharacterDeletions, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(RenderFidelity)
	RBX_ENUM_VALUE(RenderFidelity, Automatic, 0)
	RBX_ENUM_VALUE(RenderFidelity, Precise, 1)
	RBX_ENUM_VALUE(RenderFidelity, Performance, 2)
RBX_ENUM_END(RenderFidelity, Automatic, Precise, Performance)

RBX_ENUM_BEGIN(RenderPriority)
	RBX_ENUM_VALUE(RenderPriority, First, 0)
	RBX_ENUM_VALUE(RenderPriority, Input, 100)
	RBX_ENUM_VALUE(RenderPriority, Camera, 200)
	RBX_ENUM_VALUE(RenderPriority, Character, 300)
	RBX_ENUM_VALUE(RenderPriority, Last, 2000)
RBX_ENUM_END(RenderPriority, First, Input, Camera, Character, Last)

RBX_ENUM_BEGIN(RenderingCacheOptimizationMode)
	RBX_ENUM_VALUE(RenderingCacheOptimizationMode, Default, 0)
	RBX_ENUM_VALUE(RenderingCacheOptimizationMode, Disabled, 1)
	RBX_ENUM_VALUE(RenderingCacheOptimizationMode, Enabled, 2)
RBX_ENUM_END(RenderingCacheOptimizationMode, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(RenderingTestComparisonMethod)
	RBX_ENUM_VALUE(RenderingTestComparisonMethod, psnr, 0)
	RBX_ENUM_VALUE(RenderingTestComparisonMethod, diff, 1)
RBX_ENUM_END(RenderingTestComparisonMethod, psnr, diff)

RBX_ENUM_BEGIN(ReplicateInstanceDestroySetting)
	RBX_ENUM_VALUE(ReplicateInstanceDestroySetting, Default, 0)
	RBX_ENUM_VALUE(ReplicateInstanceDestroySetting, Disabled, 1)
	RBX_ENUM_VALUE(ReplicateInstanceDestroySetting, Enabled, 2)
RBX_ENUM_END(ReplicateInstanceDestroySetting, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(ResamplerMode)
	RBX_ENUM_VALUE(ResamplerMode, Default, 0)
	RBX_ENUM_VALUE(ResamplerMode, Pixelated, 1)
RBX_ENUM_END(ResamplerMode, Default, Pixelated)

RBX_ENUM_BEGIN(ReservedHighlightId)
	RBX_ENUM_VALUE(ReservedHighlightId, Standard, 0)
	RBX_ENUM_VALUE(ReservedHighlightId, Active, 131072)
	RBX_ENUM_VALUE(ReservedHighlightId, Hover, 262144)
	RBX_ENUM_VALUE(ReservedHighlightId, Selection, 524288)
	RBX_ENUM_VALUE(ReservedHighlightId, NegatedPart, 1048576)
RBX_ENUM_END(ReservedHighlightId, Standard, Active, Hover, Selection, NegatedPart)

RBX_ENUM_BEGIN(RestPose)
	RBX_ENUM_VALUE(RestPose, Default, 0)
	RBX_ENUM_VALUE(RestPose, RotationsReset, 1)
	RBX_ENUM_VALUE(RestPose, Custom, 2)
RBX_ENUM_END(RestPose, Default, RotationsReset, Custom)

RBX_ENUM_BEGIN(RestPoseModel)
	RBX_ENUM_VALUE(RestPoseModel, FromRigInACE, 0)
	RBX_ENUM_VALUE(RestPoseModel, FromRigInFile, 1)
RBX_ENUM_END(RestPoseModel, FromRigInACE, FromRigInFile)

RBX_ENUM_BEGIN(ReturnKeyType)
	RBX_ENUM_VALUE(ReturnKeyType, Default, 0)
	RBX_ENUM_VALUE(ReturnKeyType, Done, 1)
	RBX_ENUM_VALUE(ReturnKeyType, Go, 2)
	RBX_ENUM_VALUE(ReturnKeyType, Next, 3)
	RBX_ENUM_VALUE(ReturnKeyType, Search, 4)
	RBX_ENUM_VALUE(ReturnKeyType, Send, 5)
RBX_ENUM_END(ReturnKeyType, Default, Done, Go, Next, Search, Send)

RBX_ENUM_BEGIN(ReverbType)
	RBX_ENUM_VALUE(ReverbType, NoReverb, 0)
	RBX_ENUM_VALUE(ReverbType, GenericReverb, 1)
	RBX_ENUM_VALUE(ReverbType, PaddedCell, 2)
	RBX_ENUM_VALUE(ReverbType, Room, 3)
	RBX_ENUM_VALUE(ReverbType, Bathroom, 4)
	RBX_ENUM_VALUE(ReverbType, LivingRoom, 5)
	RBX_ENUM_VALUE(ReverbType, StoneRoom, 6)
	RBX_ENUM_VALUE(ReverbType, Auditorium, 7)
	RBX_ENUM_VALUE(ReverbType, ConcertHall, 8)
	RBX_ENUM_VALUE(ReverbType, Cave, 9)
	RBX_ENUM_VALUE(ReverbType, Arena, 10)
	RBX_ENUM_VALUE(ReverbType, Hangar, 11)
	RBX_ENUM_VALUE(ReverbType, CarpettedHallway, 12)
	RBX_ENUM_VALUE(ReverbType, Hallway, 13)
	RBX_ENUM_VALUE(ReverbType, StoneCorridor, 14)
	RBX_ENUM_VALUE(ReverbType, Alley, 15)
	RBX_ENUM_VALUE(ReverbType, Forest, 16)
	RBX_ENUM_VALUE(ReverbType, City, 17)
	RBX_ENUM_VALUE(ReverbType, Mountains, 18)
	RBX_ENUM_VALUE(ReverbType, Quarry, 19)
	RBX_ENUM_VALUE(ReverbType, Plain, 20)
	RBX_ENUM_VALUE(ReverbType, ParkingLot, 21)
	RBX_ENUM_VALUE(ReverbType, SewerPipe, 22)
	RBX_ENUM_VALUE(ReverbType, UnderWater, 23)
RBX_ENUM_END(ReverbType, NoReverb, GenericReverb, PaddedCell, Room, Bathroom, LivingRoom, StoneRoom, Auditorium, ConcertHall, Cave, Arena, Hangar, CarpettedHallway, Hallway, StoneCorridor, Alley, Forest, City, Mountains, Quarry, Plain, ParkingLot, SewerPipe, UnderWater)

RBX_ENUM_BEGIN(ReviewableContentState)
	RBX_ENUM_VALUE(ReviewableContentState, Pending, 0)
	RBX_ENUM_VALUE(ReviewableContentState, Completed, 1)
	RBX_ENUM_VALUE(ReviewableContentState, Failed, 2)
RBX_ENUM_END(ReviewableContentState, Pending, Completed, Failed)

RBX_ENUM_BEGIN(RibbonTool)
	RBX_ENUM_VALUE(RibbonTool, Select, 0)
	RBX_ENUM_VALUE(RibbonTool, Scale, 1)
	RBX_ENUM_VALUE(RibbonTool, Rotate, 2)
	RBX_ENUM_VALUE(RibbonTool, Move, 3)
	RBX_ENUM_VALUE(RibbonTool, Transform, 4)
	RBX_ENUM_VALUE(RibbonTool, ColorPicker, 5)
	RBX_ENUM_VALUE(RibbonTool, MaterialPicker, 6)
	RBX_ENUM_VALUE(RibbonTool, Group, 7)
	RBX_ENUM_VALUE(RibbonTool, Ungroup, 8)
	RBX_ENUM_VALUE(RibbonTool, None, 9)
	RBX_ENUM_VALUE(RibbonTool, PivotEditor, 10)
RBX_ENUM_END(RibbonTool, Select, Scale, Rotate, Move, Transform, ColorPicker, MaterialPicker, Group, Ungroup, None, PivotEditor)

RBX_ENUM_BEGIN(RigLabel)
	RBX_ENUM_VALUE(RigLabel, Invalid, 0)
	RBX_ENUM_VALUE(RigLabel, Root, 2)
	RBX_ENUM_VALUE(RigLabel, LeftHip, 3)
	RBX_ENUM_VALUE(RigLabel, LeftKnee, 4)
	RBX_ENUM_VALUE(RigLabel, LeftAnkle, 5)
	RBX_ENUM_VALUE(RigLabel, RightHip, 7)
	RBX_ENUM_VALUE(RigLabel, RightKnee, 8)
	RBX_ENUM_VALUE(RigLabel, RightAnkle, 9)
	RBX_ENUM_VALUE(RigLabel, Waist, 11)
	RBX_ENUM_VALUE(RigLabel, LeftShoulder, 12)
	RBX_ENUM_VALUE(RigLabel, LeftElbow, 13)
	RBX_ENUM_VALUE(RigLabel, LeftWrist, 14)
	RBX_ENUM_VALUE(RigLabel, RightShoulder, 16)
	RBX_ENUM_VALUE(RigLabel, RightElbow, 17)
	RBX_ENUM_VALUE(RigLabel, RightWrist, 18)
	RBX_ENUM_VALUE(RigLabel, Neck, 20)
	RBX_ENUM_VALUE(RigLabel, Spine, 23)
	RBX_ENUM_VALUE(RigLabel, Chest, 24)
	RBX_ENUM_VALUE(RigLabel, HeadBase, 25)
	RBX_ENUM_VALUE(RigLabel, LeftClavicle, 26)
	RBX_ENUM_VALUE(RigLabel, RightClavicle, 27)
	RBX_ENUM_VALUE(RigLabel, LeftToeBase, 28)
	RBX_ENUM_VALUE(RigLabel, RightToeBase, 29)
	RBX_ENUM_VALUE(RigLabel, Thumb1, 30)
	RBX_ENUM_VALUE(RigLabel, Thumb2, 31)
	RBX_ENUM_VALUE(RigLabel, Thumb3, 32)
	RBX_ENUM_VALUE(RigLabel, Index1, 33)
	RBX_ENUM_VALUE(RigLabel, Index2, 34)
	RBX_ENUM_VALUE(RigLabel, Index3, 35)
	RBX_ENUM_VALUE(RigLabel, Middle1, 36)
	RBX_ENUM_VALUE(RigLabel, Middle2, 37)
	RBX_ENUM_VALUE(RigLabel, Middle3, 38)
	RBX_ENUM_VALUE(RigLabel, Ring1, 39)
	RBX_ENUM_VALUE(RigLabel, Ring2, 40)
	RBX_ENUM_VALUE(RigLabel, Ring3, 41)
	RBX_ENUM_VALUE(RigLabel, Pinky1, 42)
	RBX_ENUM_VALUE(RigLabel, Pinky2, 43)
	RBX_ENUM_VALUE(RigLabel, Pinky3, 44)
RBX_ENUM_END(RigLabel, Invalid, Root, LeftHip, LeftKnee, LeftAnkle, RightHip, RightKnee, RightAnkle, Waist, LeftShoulder, LeftElbow, LeftWrist, RightShoulder, RightElbow, RightWrist, Neck, Spine, Chest, HeadBase, LeftClavicle, RightClavicle, LeftToeBase, RightToeBase, Thumb1, Thumb2, Thumb3, Index1, Index2, Index3, Middle1, Middle2, Middle3, Ring1, Ring2, Ring3, Pinky1, Pinky2, Pinky3)

RBX_ENUM_BEGIN(RigScale)
	RBX_ENUM_VALUE(RigScale, Default, 0)
	RBX_ENUM_VALUE(RigScale, Rthro, 1)
	RBX_ENUM_VALUE(RigScale, RthroNarrow, 2)
RBX_ENUM_END(RigScale, Default, Rthro, RthroNarrow)

RBX_ENUM_BEGIN(RigType)
	RBX_ENUM_VALUE(RigType, R15, 0)
	RBX_ENUM_VALUE(RigType, CustomHumanoid, 1)
	RBX_ENUM_VALUE(RigType, Custom, 2)
	RBX_ENUM_VALUE(RigType, None, 3)
RBX_ENUM_END(RigType, R15, CustomHumanoid, Custom, None)

RBX_ENUM_BEGIN(RollOffMode)
	RBX_ENUM_VALUE(RollOffMode, Inverse, 0)
	RBX_ENUM_VALUE(RollOffMode, Linear, 1)
	RBX_ENUM_VALUE(RollOffMode, LinearSquare, 2)
	RBX_ENUM_VALUE(RollOffMode, InverseTapered, 3)
RBX_ENUM_END(RollOffMode, Inverse, Linear, LinearSquare, InverseTapered)

RBX_ENUM_BEGIN(RolloutState)
	RBX_ENUM_VALUE(RolloutState, Default, 0)
	RBX_ENUM_VALUE(RolloutState, Disabled, 1)
	RBX_ENUM_VALUE(RolloutState, Enabled, 2)
RBX_ENUM_END(RolloutState, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(RotationOrder)
	RBX_ENUM_VALUE(RotationOrder, XYZ, 0)
	RBX_ENUM_VALUE(RotationOrder, XZY, 1)
	RBX_ENUM_VALUE(RotationOrder, YZX, 2)
	RBX_ENUM_VALUE(RotationOrder, YXZ, 3)
	RBX_ENUM_VALUE(RotationOrder, ZXY, 4)
	RBX_ENUM_VALUE(RotationOrder, ZYX, 5)
RBX_ENUM_END(RotationOrder, XYZ, XZY, YZX, YXZ, ZXY, ZYX)

RBX_ENUM_BEGIN(RotationType)
	RBX_ENUM_VALUE(RotationType, MovementRelative, 0)
	RBX_ENUM_VALUE(RotationType, CameraRelative, 1)
RBX_ENUM_END(RotationType, MovementRelative, CameraRelative)

RBX_ENUM_BEGIN(RsvpStatus)
	RBX_ENUM_VALUE(RsvpStatus, None, 0)
	RBX_ENUM_VALUE(RsvpStatus, Going, 1)
	RBX_ENUM_VALUE(RsvpStatus, NotGoing, 2)
RBX_ENUM_END(RsvpStatus, None, Going, NotGoing)

RBX_ENUM_BEGIN(RtlTextSupport)
	RBX_ENUM_VALUE(RtlTextSupport, Default, 0)
	RBX_ENUM_VALUE(RtlTextSupport, Disabled, 1)
	RBX_ENUM_VALUE(RtlTextSupport, Enabled, 2)
RBX_ENUM_END(RtlTextSupport, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(RunContext)
	RBX_ENUM_VALUE(RunContext, Legacy, 0)
	RBX_ENUM_VALUE(RunContext, Server, 1)
	RBX_ENUM_VALUE(RunContext, Client, 2)
	RBX_ENUM_VALUE(RunContext, Plugin, 3)
RBX_ENUM_END(RunContext, Legacy, Server, Client, Plugin)

RBX_ENUM_BEGIN(RunState)
	RBX_ENUM_VALUE(RunState, Stopped, 0)
	RBX_ENUM_VALUE(RunState, Running, 1)
	RBX_ENUM_VALUE(RunState, Paused, 2)
RBX_ENUM_END(RunState, Stopped, Running, Paused)

RBX_ENUM_BEGIN(RuntimeUndoBehavior)
	RBX_ENUM_VALUE(RuntimeUndoBehavior, Aggregate, 0)
	RBX_ENUM_VALUE(RuntimeUndoBehavior, Snapshot, 1)
	RBX_ENUM_VALUE(RuntimeUndoBehavior, Hybrid, 2)
RBX_ENUM_END(RuntimeUndoBehavior, Aggregate, Snapshot, Hybrid)

RBX_ENUM_BEGIN(SafeAreaCompatibility)
	RBX_ENUM_VALUE(SafeAreaCompatibility, None, 0)
	RBX_ENUM_VALUE(SafeAreaCompatibility, FullscreenExtension, 1)
RBX_ENUM_END(SafeAreaCompatibility, None, FullscreenExtension)

RBX_ENUM_BEGIN(SalesTypeFilter)
	RBX_ENUM_VALUE(SalesTypeFilter, All, 1)
	RBX_ENUM_VALUE(SalesTypeFilter, Collectibles, 2)
	RBX_ENUM_VALUE(SalesTypeFilter, Premium, 3)
	RBX_ENUM_VALUE(SalesTypeFilter, TimedOptions, 4)
RBX_ENUM_END(SalesTypeFilter, All, Collectibles, Premium, TimedOptions)

RBX_ENUM_BEGIN(SandboxedInstanceMode)
	RBX_ENUM_VALUE(SandboxedInstanceMode, Default, 0)
	RBX_ENUM_VALUE(SandboxedInstanceMode, Experimental, 1)
RBX_ENUM_END(SandboxedInstanceMode, Default, Experimental)

RBX_ENUM_BEGIN(SaveAvatarThumbnailCustomizationFailure)
	RBX_ENUM_VALUE(SaveAvatarThumbnailCustomizationFailure, BadThumbnailType, 1)
	RBX_ENUM_VALUE(SaveAvatarThumbnailCustomizationFailure, BadYRotDeg, 2)
	RBX_ENUM_VALUE(SaveAvatarThumbnailCustomizationFailure, BadFieldOfViewDeg, 3)
	RBX_ENUM_VALUE(SaveAvatarThumbnailCustomizationFailure, BadDistanceScale, 4)
	RBX_ENUM_VALUE(SaveAvatarThumbnailCustomizationFailure, Other, 5)
	RBX_ENUM_VALUE(SaveAvatarThumbnailCustomizationFailure, Throttled, 6)
RBX_ENUM_END(SaveAvatarThumbnailCustomizationFailure, BadThumbnailType, BadYRotDeg, BadFieldOfViewDeg, BadDistanceScale, Other, Throttled)

RBX_ENUM_BEGIN(SaveFilter)
	RBX_ENUM_VALUE(SaveFilter, SaveWorld, 0)
	RBX_ENUM_VALUE(SaveFilter, SaveGame, 1)
	RBX_ENUM_VALUE(SaveFilter, SaveAll, 2)
RBX_ENUM_END(SaveFilter, SaveWorld, SaveGame, SaveAll)

RBX_ENUM_BEGIN(SavedQualitySetting)
	RBX_ENUM_VALUE(SavedQualitySetting, Automatic, 0)
	RBX_ENUM_VALUE(SavedQualitySetting, QualityLevel1, 1)
	RBX_ENUM_VALUE(SavedQualitySetting, QualityLevel2, 2)
	RBX_ENUM_VALUE(SavedQualitySetting, QualityLevel3, 3)
	RBX_ENUM_VALUE(SavedQualitySetting, QualityLevel4, 4)
	RBX_ENUM_VALUE(SavedQualitySetting, QualityLevel5, 5)
	RBX_ENUM_VALUE(SavedQualitySetting, QualityLevel6, 6)
	RBX_ENUM_VALUE(SavedQualitySetting, QualityLevel7, 7)
	RBX_ENUM_VALUE(SavedQualitySetting, QualityLevel8, 8)
	RBX_ENUM_VALUE(SavedQualitySetting, QualityLevel9, 9)
	RBX_ENUM_VALUE(SavedQualitySetting, QualityLevel10, 10)
RBX_ENUM_END(SavedQualitySetting, Automatic, QualityLevel1, QualityLevel2, QualityLevel3, QualityLevel4, QualityLevel5, QualityLevel6, QualityLevel7, QualityLevel8, QualityLevel9, QualityLevel10)

RBX_ENUM_BEGIN(ScaleType)
	RBX_ENUM_VALUE(ScaleType, Stretch, 0)
	RBX_ENUM_VALUE(ScaleType, Slice, 1)
	RBX_ENUM_VALUE(ScaleType, Tile, 2)
	RBX_ENUM_VALUE(ScaleType, Fit, 3)
	RBX_ENUM_VALUE(ScaleType, Crop, 4)
RBX_ENUM_END(ScaleType, Stretch, Slice, Tile, Fit, Crop)

RBX_ENUM_BEGIN(ScopeCheckResult)
	RBX_ENUM_VALUE(ScopeCheckResult, ConsentAccepted, 0)
	RBX_ENUM_VALUE(ScopeCheckResult, InvalidScopes, 1)
	RBX_ENUM_VALUE(ScopeCheckResult, Timeout, 2)
	RBX_ENUM_VALUE(ScopeCheckResult, NoUserInput, 3)
	RBX_ENUM_VALUE(ScopeCheckResult, BackendError, 4)
	RBX_ENUM_VALUE(ScopeCheckResult, UnexpectedError, 5)
	RBX_ENUM_VALUE(ScopeCheckResult, InvalidArgument, 6)
	RBX_ENUM_VALUE(ScopeCheckResult, ConsentDenied, 7)
RBX_ENUM_END(ScopeCheckResult, ConsentAccepted, InvalidScopes, Timeout, NoUserInput, BackendError, UnexpectedError, InvalidArgument, ConsentDenied)

RBX_ENUM_BEGIN(ScreenInsets)
	RBX_ENUM_VALUE(ScreenInsets, None, 0)
	RBX_ENUM_VALUE(ScreenInsets, DeviceSafeInsets, 1)
	RBX_ENUM_VALUE(ScreenInsets, CoreUISafeInsets, 2)
	RBX_ENUM_VALUE(ScreenInsets, TopbarSafeInsets, 3)
RBX_ENUM_END(ScreenInsets, None, DeviceSafeInsets, CoreUISafeInsets, TopbarSafeInsets)

RBX_ENUM_BEGIN(ScreenOrientation)
	RBX_ENUM_VALUE(ScreenOrientation, LandscapeLeft, 0)
	RBX_ENUM_VALUE(ScreenOrientation, LandscapeRight, 1)
	RBX_ENUM_VALUE(ScreenOrientation, LandscapeSensor, 2)
	RBX_ENUM_VALUE(ScreenOrientation, Portrait, 3)
	RBX_ENUM_VALUE(ScreenOrientation, Sensor, 4)
RBX_ENUM_END(ScreenOrientation, LandscapeLeft, LandscapeRight, LandscapeSensor, Portrait, Sensor)

RBX_ENUM_BEGIN(ScreenshotCaptureResult)
	RBX_ENUM_VALUE(ScreenshotCaptureResult, Success, 0)
	RBX_ENUM_VALUE(ScreenshotCaptureResult, OtherError, 1)
	RBX_ENUM_VALUE(ScreenshotCaptureResult, NoDeviceSupport, 2)
	RBX_ENUM_VALUE(ScreenshotCaptureResult, NoSpaceOnDevice, 3)
RBX_ENUM_END(ScreenshotCaptureResult, Success, OtherError, NoDeviceSupport, NoSpaceOnDevice)

RBX_ENUM_BEGIN(ScrollBarInset)
	RBX_ENUM_VALUE(ScrollBarInset, None, 0)
	RBX_ENUM_VALUE(ScrollBarInset, ScrollBar, 1)
	RBX_ENUM_VALUE(ScrollBarInset, Always, 2)
RBX_ENUM_END(ScrollBarInset, None, ScrollBar, Always)

RBX_ENUM_BEGIN(ScrollingDirection)
	RBX_ENUM_VALUE(ScrollingDirection, X, 1)
	RBX_ENUM_VALUE(ScrollingDirection, Y, 2)
	RBX_ENUM_VALUE(ScrollingDirection, XY, 4)
RBX_ENUM_END(ScrollingDirection, X, Y, XY)

RBX_ENUM_BEGIN(SecurityCapability)
	RBX_ENUM_VALUE(SecurityCapability, RunClientScript, 0)
	RBX_ENUM_VALUE(SecurityCapability, RunServerScript, 1)
	RBX_ENUM_VALUE(SecurityCapability, AccessOutsideWrite, 2)
	RBX_ENUM_VALUE(SecurityCapability, AssetRequire, 3)
	RBX_ENUM_VALUE(SecurityCapability, LoadString, 4)
	RBX_ENUM_VALUE(SecurityCapability, ScriptGlobals, 5)
	RBX_ENUM_VALUE(SecurityCapability, CreateInstances, 6)
	RBX_ENUM_VALUE(SecurityCapability, Basic, 7)
	RBX_ENUM_VALUE(SecurityCapability, Audio, 8)
	RBX_ENUM_VALUE(SecurityCapability, DataStore, 9)
	RBX_ENUM_VALUE(SecurityCapability, Network, 10)
	RBX_ENUM_VALUE(SecurityCapability, Physics, 11)
	RBX_ENUM_VALUE(SecurityCapability, UI, 12)
	RBX_ENUM_VALUE(SecurityCapability, CSG, 13)
	RBX_ENUM_VALUE(SecurityCapability, Chat, 14)
	RBX_ENUM_VALUE(SecurityCapability, Animation, 15)
	RBX_ENUM_VALUE(SecurityCapability, Avatar, 16)
	RBX_ENUM_VALUE(SecurityCapability, Input, 17)
	RBX_ENUM_VALUE(SecurityCapability, Environment, 18)
	RBX_ENUM_VALUE(SecurityCapability, RemoteEvent, 19)
	RBX_ENUM_VALUE(SecurityCapability, LegacySound, 20)
	RBX_ENUM_VALUE(SecurityCapability, Players, 21)
	RBX_ENUM_VALUE(SecurityCapability, CapabilityControl, 22)
	RBX_ENUM_VALUE(SecurityCapability, Plugin, 23)
	RBX_ENUM_VALUE(SecurityCapability, LocalUser, 24)
	RBX_ENUM_VALUE(SecurityCapability, WritePlayer, 25)
	RBX_ENUM_VALUE(SecurityCapability, RobloxScript, 26)
	RBX_ENUM_VALUE(SecurityCapability, RobloxEngine, 27)
	RBX_ENUM_VALUE(SecurityCapability, Unassigned, 28)
	RBX_ENUM_VALUE(SecurityCapability, InternalTest, 29)
	RBX_ENUM_VALUE(SecurityCapability, PluginOrOpenCloud, 30)
	RBX_ENUM_VALUE(SecurityCapability, Assistant, 31)
	RBX_ENUM_VALUE(SecurityCapability, RemoteCommand, 32)
	RBX_ENUM_VALUE(SecurityCapability, AssetRead, 33)
	RBX_ENUM_VALUE(SecurityCapability, AssetManagement, 34)
	RBX_ENUM_VALUE(SecurityCapability, DynamicGeneration, 35)
	RBX_ENUM_VALUE(SecurityCapability, PlatformAvatarEditing, 36)
	RBX_ENUM_VALUE(SecurityCapability, AssetCreateUpdate, 37)
	RBX_ENUM_VALUE(SecurityCapability, Capture, 38)
	RBX_ENUM_VALUE(SecurityCapability, SensitiveInput, 39)
	RBX_ENUM_VALUE(SecurityCapability, Monetization, 40)
	RBX_ENUM_VALUE(SecurityCapability, LoadOwnedAsset, 41)
	RBX_ENUM_VALUE(SecurityCapability, Social, 42)
	RBX_ENUM_VALUE(SecurityCapability, ServerCommunication, 43)
	RBX_ENUM_VALUE(SecurityCapability, Logging, 44)
	RBX_ENUM_VALUE(SecurityCapability, PromptExternalPurchase, 45)
	RBX_ENUM_VALUE(SecurityCapability, Groups, 46)
	RBX_ENUM_VALUE(SecurityCapability, Teleport, 47)
	RBX_ENUM_VALUE(SecurityCapability, Consequences, 48)
	RBX_ENUM_VALUE(SecurityCapability, Material, 49)
	RBX_ENUM_VALUE(SecurityCapability, AvatarBehavior, 50)
	RBX_ENUM_VALUE(SecurityCapability, AvatarAppearance, 51)
	RBX_ENUM_VALUE(SecurityCapability, LoadUnownedAsset, 52)
RBX_ENUM_END(SecurityCapability, RunClientScript, RunServerScript, AccessOutsideWrite, AssetRequire, LoadString, ScriptGlobals, CreateInstances, Basic, Audio, DataStore, Network, Physics, UI, CSG, Chat, Animation, Avatar, Input, Environment, RemoteEvent, LegacySound, Players, CapabilityControl, Plugin, LocalUser, WritePlayer, RobloxScript, RobloxEngine, Unassigned, InternalTest, PluginOrOpenCloud, Assistant, RemoteCommand, AssetRead, AssetManagement, DynamicGeneration, PlatformAvatarEditing, AssetCreateUpdate, Capture, SensitiveInput, Monetization, LoadOwnedAsset, Social, ServerCommunication, Logging, PromptExternalPurchase, Groups, Teleport, Consequences, Material, AvatarBehavior, AvatarAppearance, LoadUnownedAsset)

RBX_ENUM_BEGIN(SelectionBehavior)
	RBX_ENUM_VALUE(SelectionBehavior, Escape, 0)
	RBX_ENUM_VALUE(SelectionBehavior, Stop, 1)
RBX_ENUM_END(SelectionBehavior, Escape, Stop)

RBX_ENUM_BEGIN(SelectionRenderMode)
	RBX_ENUM_VALUE(SelectionRenderMode, Outlines, 0)
	RBX_ENUM_VALUE(SelectionRenderMode, BoundingBoxes, 1)
	RBX_ENUM_VALUE(SelectionRenderMode, Both, 2)
RBX_ENUM_END(SelectionRenderMode, Outlines, BoundingBoxes, Both)

RBX_ENUM_BEGIN(SelfViewPosition)
	RBX_ENUM_VALUE(SelfViewPosition, LastPosition, 0)
	RBX_ENUM_VALUE(SelfViewPosition, TopLeft, 1)
	RBX_ENUM_VALUE(SelfViewPosition, TopRight, 2)
	RBX_ENUM_VALUE(SelfViewPosition, BottomLeft, 3)
	RBX_ENUM_VALUE(SelfViewPosition, BottomRight, 4)
RBX_ENUM_END(SelfViewPosition, LastPosition, TopLeft, TopRight, BottomLeft, BottomRight)

RBX_ENUM_BEGIN(SensorMode)
	RBX_ENUM_VALUE(SensorMode, Floor, 0)
	RBX_ENUM_VALUE(SensorMode, Ladder, 1)
	RBX_ENUM_VALUE(SensorMode, ClassicFloor, 2)
	RBX_ENUM_VALUE(SensorMode, ClassicLadder, 3)
RBX_ENUM_END(SensorMode, Floor, Ladder, ClassicFloor, ClassicLadder)

RBX_ENUM_BEGIN(SensorUpdateType)
	RBX_ENUM_VALUE(SensorUpdateType, OnRead, 0)
	RBX_ENUM_VALUE(SensorUpdateType, Manual, 1)
RBX_ENUM_END(SensorUpdateType, OnRead, Manual)

RBX_ENUM_BEGIN(ServerLiveEditingMode)
	RBX_ENUM_VALUE(ServerLiveEditingMode, Uninitialized, 0)
	RBX_ENUM_VALUE(ServerLiveEditingMode, Enabled, 1)
	RBX_ENUM_VALUE(ServerLiveEditingMode, Disabled, 2)
RBX_ENUM_END(ServerLiveEditingMode, Uninitialized, Enabled, Disabled)

RBX_ENUM_BEGIN(ServiceVisibility)
	RBX_ENUM_VALUE(ServiceVisibility, Always, 0)
	RBX_ENUM_VALUE(ServiceVisibility, Off, 1)
	RBX_ENUM_VALUE(ServiceVisibility, WithChildren, 2)
RBX_ENUM_END(ServiceVisibility, Always, Off, WithChildren)

RBX_ENUM_BEGIN(Severity)
	RBX_ENUM_VALUE(Severity, Error, 1)
	RBX_ENUM_VALUE(Severity, Warning, 2)
	RBX_ENUM_VALUE(Severity, Information, 3)
	RBX_ENUM_VALUE(Severity, Hint, 4)
RBX_ENUM_END(Severity, Error, Warning, Information, Hint)

RBX_ENUM_BEGIN(ShowAdResult)
	RBX_ENUM_VALUE(ShowAdResult, ShowCompleted, 1)
	RBX_ENUM_VALUE(ShowAdResult, AdNotReady, 2)
	RBX_ENUM_VALUE(ShowAdResult, AdAlreadyShowing, 3)
	RBX_ENUM_VALUE(ShowAdResult, InternalError, 4)
	RBX_ENUM_VALUE(ShowAdResult, ShowInterrupted, 5)
	RBX_ENUM_VALUE(ShowAdResult, InsufficientMemory, 6)
RBX_ENUM_END(ShowAdResult, ShowCompleted, AdNotReady, AdAlreadyShowing, InternalError, ShowInterrupted, InsufficientMemory)

RBX_ENUM_BEGIN(SignalBehavior)
	RBX_ENUM_VALUE(SignalBehavior, Default, 0)
	RBX_ENUM_VALUE(SignalBehavior, Immediate, 1)
	RBX_ENUM_VALUE(SignalBehavior, Deferred, 2)
	RBX_ENUM_VALUE(SignalBehavior, AncestryDeferred, 3)
RBX_ENUM_END(SignalBehavior, Default, Immediate, Deferred, AncestryDeferred)

RBX_ENUM_BEGIN(SizeConstraint)
	RBX_ENUM_VALUE(SizeConstraint, RelativeXY, 0)
	RBX_ENUM_VALUE(SizeConstraint, RelativeXX, 1)
	RBX_ENUM_VALUE(SizeConstraint, RelativeYY, 2)
RBX_ENUM_END(SizeConstraint, RelativeXY, RelativeXX, RelativeYY)

RBX_ENUM_BEGIN(SolidPrimitiveType)
	RBX_ENUM_VALUE(SolidPrimitiveType, Capsule, 5)
	RBX_ENUM_VALUE(SolidPrimitiveType, Cone, 6)
	RBX_ENUM_VALUE(SolidPrimitiveType, RoundedBox, 7)
RBX_ENUM_END(SolidPrimitiveType, Capsule, Cone, RoundedBox)

RBX_ENUM_BEGIN(SolverConvergenceMetricType)
	RBX_ENUM_VALUE(SolverConvergenceMetricType, IterationBased, 0)
	RBX_ENUM_VALUE(SolverConvergenceMetricType, AlgorithmAgnostic, 1)
RBX_ENUM_END(SolverConvergenceMetricType, IterationBased, AlgorithmAgnostic)

RBX_ENUM_BEGIN(SolverConvergenceVisualizationMode)
	RBX_ENUM_VALUE(SolverConvergenceVisualizationMode, Disabled, 0)
	RBX_ENUM_VALUE(SolverConvergenceVisualizationMode, PerIsland, 1)
	RBX_ENUM_VALUE(SolverConvergenceVisualizationMode, PerEdge, 2)
RBX_ENUM_END(SolverConvergenceVisualizationMode, Disabled, PerIsland, PerEdge)

RBX_ENUM_BEGIN(SortDirection)
	RBX_ENUM_VALUE(SortDirection, Ascending, 0)
	RBX_ENUM_VALUE(SortDirection, Descending, 1)
RBX_ENUM_END(SortDirection, Ascending, Descending)

RBX_ENUM_BEGIN(SortOrder)
	RBX_ENUM_VALUE(SortOrder, Name, 0)
	RBX_ENUM_VALUE(SortOrder, Custom, 1)
	RBX_ENUM_VALUE(SortOrder, LayoutOrder, 2)
RBX_ENUM_END(SortOrder, Name, Custom, LayoutOrder)

RBX_ENUM_BEGIN(SpecialKey)
	RBX_ENUM_VALUE(SpecialKey, Insert, 0)
	RBX_ENUM_VALUE(SpecialKey, Home, 1)
	RBX_ENUM_VALUE(SpecialKey, End, 2)
	RBX_ENUM_VALUE(SpecialKey, PageUp, 3)
	RBX_ENUM_VALUE(SpecialKey, PageDown, 4)
	RBX_ENUM_VALUE(SpecialKey, ChatHotkey, 5)
RBX_ENUM_END(SpecialKey, Insert, Home, End, PageUp, PageDown, ChatHotkey)

RBX_ENUM_BEGIN(StartCorner)
	RBX_ENUM_VALUE(StartCorner, TopLeft, 0)
	RBX_ENUM_VALUE(StartCorner, TopRight, 1)
	RBX_ENUM_VALUE(StartCorner, BottomLeft, 2)
	RBX_ENUM_VALUE(StartCorner, BottomRight, 3)
RBX_ENUM_END(StartCorner, TopLeft, TopRight, BottomLeft, BottomRight)

RBX_ENUM_BEGIN(StateObjectFieldType)
	RBX_ENUM_VALUE(StateObjectFieldType, Boolean, 0)
	RBX_ENUM_VALUE(StateObjectFieldType, CFrame, 1)
	RBX_ENUM_VALUE(StateObjectFieldType, Color3, 2)
	RBX_ENUM_VALUE(StateObjectFieldType, Float, 3)
	RBX_ENUM_VALUE(StateObjectFieldType, Instance, 4)
	RBX_ENUM_VALUE(StateObjectFieldType, Random, 5)
	RBX_ENUM_VALUE(StateObjectFieldType, Vector2, 6)
	RBX_ENUM_VALUE(StateObjectFieldType, Vector3, 7)
	RBX_ENUM_VALUE(StateObjectFieldType, INVALID, 8)
RBX_ENUM_END(StateObjectFieldType, Boolean, CFrame, Color3, Float, Instance, Random, Vector2, Vector3, INVALID)

RBX_ENUM_BEGIN(Status)
	RBX_ENUM_VALUE(Status, Poison, 0)
	RBX_ENUM_VALUE(Status, Confusion, 1)
RBX_ENUM_END(Status, Poison, Confusion)

RBX_ENUM_BEGIN(StepFrequency)
	RBX_ENUM_VALUE(StepFrequency, Hz60, 0)
	RBX_ENUM_VALUE(StepFrequency, Hz30, 1)
	RBX_ENUM_VALUE(StepFrequency, Hz15, 2)
	RBX_ENUM_VALUE(StepFrequency, Hz10, 3)
	RBX_ENUM_VALUE(StepFrequency, Hz5, 4)
	RBX_ENUM_VALUE(StepFrequency, Hz1, 5)
RBX_ENUM_END(StepFrequency, Hz60, Hz30, Hz15, Hz10, Hz5, Hz1)

RBX_ENUM_BEGIN(StreamOutBehavior)
	RBX_ENUM_VALUE(StreamOutBehavior, Default, 0)
	RBX_ENUM_VALUE(StreamOutBehavior, LowMemory, 1)
	RBX_ENUM_VALUE(StreamOutBehavior, Opportunistic, 2)
RBX_ENUM_END(StreamOutBehavior, Default, LowMemory, Opportunistic)

RBX_ENUM_BEGIN(StreamingIntegrityMode)
	RBX_ENUM_VALUE(StreamingIntegrityMode, Default, 0)
	RBX_ENUM_VALUE(StreamingIntegrityMode, Disabled, 1)
	RBX_ENUM_VALUE(StreamingIntegrityMode, MinimumRadiusPause, 2)
	RBX_ENUM_VALUE(StreamingIntegrityMode, PauseOutsideLoadedArea, 3)
RBX_ENUM_END(StreamingIntegrityMode, Default, Disabled, MinimumRadiusPause, PauseOutsideLoadedArea)

RBX_ENUM_BEGIN(StreamingPauseMode)
	RBX_ENUM_VALUE(StreamingPauseMode, Default, 0)
	RBX_ENUM_VALUE(StreamingPauseMode, Disabled, 1)
	RBX_ENUM_VALUE(StreamingPauseMode, ClientPhysicsPause, 2)
RBX_ENUM_END(StreamingPauseMode, Default, Disabled, ClientPhysicsPause)

RBX_ENUM_BEGIN(StrokeSizingMode)
	RBX_ENUM_VALUE(StrokeSizingMode, FixedSize, 0)
	RBX_ENUM_VALUE(StrokeSizingMode, ScaledSize, 1)
RBX_ENUM_END(StrokeSizingMode, FixedSize, ScaledSize)

RBX_ENUM_BEGIN(StudioCaptureBufferStatus)
	RBX_ENUM_VALUE(StudioCaptureBufferStatus, NotStarted, 0)
	RBX_ENUM_VALUE(StudioCaptureBufferStatus, Pending, 1)
	RBX_ENUM_VALUE(StudioCaptureBufferStatus, Ready, 2)
	RBX_ENUM_VALUE(StudioCaptureBufferStatus, Error, 3)
RBX_ENUM_END(StudioCaptureBufferStatus, NotStarted, Pending, Ready, Error)

RBX_ENUM_BEGIN(StudioCaptureScreenshotFormat)
	RBX_ENUM_VALUE(StudioCaptureScreenshotFormat, RGBA8, 0)
	RBX_ENUM_VALUE(StudioCaptureScreenshotFormat, PNG, 1)
RBX_ENUM_END(StudioCaptureScreenshotFormat, RGBA8, PNG)

RBX_ENUM_BEGIN(StudioCloseMode)
	RBX_ENUM_VALUE(StudioCloseMode, None, 0)
	RBX_ENUM_VALUE(StudioCloseMode, CloseStudio, 1)
	RBX_ENUM_VALUE(StudioCloseMode, CloseDoc, 2)
	RBX_ENUM_VALUE(StudioCloseMode, LogOut, 3)
RBX_ENUM_END(StudioCloseMode, None, CloseStudio, CloseDoc, LogOut)

RBX_ENUM_BEGIN(StudioDataModelType)
	RBX_ENUM_VALUE(StudioDataModelType, Edit, 0)
	RBX_ENUM_VALUE(StudioDataModelType, PlayClient, 1)
	RBX_ENUM_VALUE(StudioDataModelType, PlayServer, 2)
	RBX_ENUM_VALUE(StudioDataModelType, Standalone, 3)
	RBX_ENUM_VALUE(StudioDataModelType, None, 4)
RBX_ENUM_END(StudioDataModelType, Edit, PlayClient, PlayServer, Standalone, None)

RBX_ENUM_BEGIN(StudioPlaceUpdateFailureReason)
	RBX_ENUM_VALUE(StudioPlaceUpdateFailureReason, Other, 0)
	RBX_ENUM_VALUE(StudioPlaceUpdateFailureReason, TeamCreateConflict, 1)
RBX_ENUM_END(StudioPlaceUpdateFailureReason, Other, TeamCreateConflict)

RBX_ENUM_BEGIN(StudioScriptEditorColorCategories)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Default, 0)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Operator, 1)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Number, 2)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, String, 3)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Comment, 4)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Keyword, 5)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Builtin, 6)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Method, 7)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Property, 8)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Nil, 9)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Bool, 10)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Function, 11)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Local, 12)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Self, 13)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, LuauKeyword, 14)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, FunctionName, 15)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, TODO, 16)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Background, 17)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, SelectionText, 18)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, SelectionBackground, 19)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, FindSelectionBackground, 20)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, MatchingWordBackground, 21)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Warning, 22)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Error, 23)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Info, 24)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Hint, 25)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Whitespace, 26)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, ActiveLine, 27)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, DebuggerCurrentLine, 28)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, DebuggerErrorLine, 29)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Ruler, 30)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Bracket, 31)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, Type, 32)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, MenuPrimaryText, 33)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, MenuSecondaryText, 34)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, MenuSelectedText, 35)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, MenuBackground, 36)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, MenuSelectedBackground, 37)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, MenuScrollbarBackground, 38)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, MenuScrollbarHandle, 39)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, MenuBorder, 40)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, DocViewCodeBackground, 41)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, AICOOverlayText, 42)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, AICOOverlayButtonBackground, 43)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, AICOOverlayButtonBackgroundHover, 44)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, AICOOverlayButtonBackgroundPressed, 45)
	RBX_ENUM_VALUE(StudioScriptEditorColorCategories, IndentationRuler, 46)
RBX_ENUM_END(StudioScriptEditorColorCategories, Default, Operator, Number, String, Comment, Keyword, Builtin, Method, Property, Nil, Bool, Function, Local, Self, LuauKeyword, FunctionName, TODO, Background, SelectionText, SelectionBackground, FindSelectionBackground, MatchingWordBackground, Warning, Error, Info, Hint, Whitespace, ActiveLine, DebuggerCurrentLine, DebuggerErrorLine, Ruler, Bracket, Type, MenuPrimaryText, MenuSecondaryText, MenuSelectedText, MenuBackground, MenuSelectedBackground, MenuScrollbarBackground, MenuScrollbarHandle, MenuBorder, DocViewCodeBackground, AICOOverlayText, AICOOverlayButtonBackground, AICOOverlayButtonBackgroundHover, AICOOverlayButtonBackgroundPressed, IndentationRuler)

RBX_ENUM_BEGIN(StudioScriptEditorColorPresets)
	RBX_ENUM_VALUE(StudioScriptEditorColorPresets, RobloxDefault, 0)
	RBX_ENUM_VALUE(StudioScriptEditorColorPresets, Extra1, 1)
	RBX_ENUM_VALUE(StudioScriptEditorColorPresets, Extra2, 2)
	RBX_ENUM_VALUE(StudioScriptEditorColorPresets, Custom, 3)
RBX_ENUM_END(StudioScriptEditorColorPresets, RobloxDefault, Extra1, Extra2, Custom)

RBX_ENUM_BEGIN(StudioStyleGuideColor)
	RBX_ENUM_VALUE(StudioStyleGuideColor, MainBackground, 0)
	RBX_ENUM_VALUE(StudioStyleGuideColor, Titlebar, 1)
	RBX_ENUM_VALUE(StudioStyleGuideColor, Dropdown, 2)
	RBX_ENUM_VALUE(StudioStyleGuideColor, Tooltip, 3)
	RBX_ENUM_VALUE(StudioStyleGuideColor, Notification, 4)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScrollBar, 5)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScrollBarBackground, 6)
	RBX_ENUM_VALUE(StudioStyleGuideColor, TabBar, 7)
	RBX_ENUM_VALUE(StudioStyleGuideColor, Tab, 8)
	RBX_ENUM_VALUE(StudioStyleGuideColor, FilterButtonDefault, 9)
	RBX_ENUM_VALUE(StudioStyleGuideColor, FilterButtonHover, 10)
	RBX_ENUM_VALUE(StudioStyleGuideColor, FilterButtonChecked, 11)
	RBX_ENUM_VALUE(StudioStyleGuideColor, FilterButtonAccent, 12)
	RBX_ENUM_VALUE(StudioStyleGuideColor, FilterButtonBorder, 13)
	RBX_ENUM_VALUE(StudioStyleGuideColor, FilterButtonBorderAlt, 14)
	RBX_ENUM_VALUE(StudioStyleGuideColor, RibbonTab, 15)
	RBX_ENUM_VALUE(StudioStyleGuideColor, RibbonTabTopBar, 16)
	RBX_ENUM_VALUE(StudioStyleGuideColor, Button, 17)
	RBX_ENUM_VALUE(StudioStyleGuideColor, MainButton, 18)
	RBX_ENUM_VALUE(StudioStyleGuideColor, RibbonButton, 19)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ViewPortBackground, 20)
	RBX_ENUM_VALUE(StudioStyleGuideColor, InputFieldBackground, 21)
	RBX_ENUM_VALUE(StudioStyleGuideColor, Item, 22)
	RBX_ENUM_VALUE(StudioStyleGuideColor, TableItem, 23)
	RBX_ENUM_VALUE(StudioStyleGuideColor, CategoryItem, 24)
	RBX_ENUM_VALUE(StudioStyleGuideColor, GameSettingsTableItem, 25)
	RBX_ENUM_VALUE(StudioStyleGuideColor, GameSettingsTooltip, 26)
	RBX_ENUM_VALUE(StudioStyleGuideColor, EmulatorBar, 27)
	RBX_ENUM_VALUE(StudioStyleGuideColor, EmulatorDropDown, 28)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ColorPickerFrame, 29)
	RBX_ENUM_VALUE(StudioStyleGuideColor, CurrentMarker, 30)
	RBX_ENUM_VALUE(StudioStyleGuideColor, Border, 31)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DropShadow, 32)
	RBX_ENUM_VALUE(StudioStyleGuideColor, Shadow, 33)
	RBX_ENUM_VALUE(StudioStyleGuideColor, Light, 34)
	RBX_ENUM_VALUE(StudioStyleGuideColor, Dark, 35)
	RBX_ENUM_VALUE(StudioStyleGuideColor, Mid, 36)
	RBX_ENUM_VALUE(StudioStyleGuideColor, MainText, 37)
	RBX_ENUM_VALUE(StudioStyleGuideColor, SubText, 38)
	RBX_ENUM_VALUE(StudioStyleGuideColor, TitlebarText, 39)
	RBX_ENUM_VALUE(StudioStyleGuideColor, BrightText, 40)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DimmedText, 41)
	RBX_ENUM_VALUE(StudioStyleGuideColor, LinkText, 42)
	RBX_ENUM_VALUE(StudioStyleGuideColor, WarningText, 43)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ErrorText, 44)
	RBX_ENUM_VALUE(StudioStyleGuideColor, InfoText, 45)
	RBX_ENUM_VALUE(StudioStyleGuideColor, SensitiveText, 46)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptSideWidget, 47)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptBackground, 48)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptText, 49)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptSelectionText, 50)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptSelectionBackground, 51)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptFindSelectionBackground, 52)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptMatchingWordSelectionBackground, 53)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptOperator, 54)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptNumber, 55)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptString, 56)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptComment, 57)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptKeyword, 58)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptBuiltInFunction, 59)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptWarning, 60)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptError, 61)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptInformation, 62)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptHint, 63)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptWhitespace, 64)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptRuler, 65)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DocViewCodeBackground, 66)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DebuggerCurrentLine, 67)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DebuggerErrorLine, 68)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffFilePathText, 69)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffTextHunkInfo, 70)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffTextNoChange, 71)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffTextAddition, 72)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffTextDeletion, 73)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffTextSeparatorBackground, 74)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffTextNoChangeBackground, 75)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffTextAdditionBackground, 76)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffTextDeletionBackground, 77)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffLineNum, 78)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffLineNumSeparatorBackground, 79)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffLineNumNoChangeBackground, 80)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffLineNumAdditionBackground, 81)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffLineNumDeletionBackground, 82)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffFilePathBackground, 83)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffFilePathBorder, 84)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ChatIncomingBgColor, 85)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ChatIncomingTextColor, 86)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ChatOutgoingBgColor, 87)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ChatOutgoingTextColor, 88)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ChatModeratedMessageColor, 89)
	RBX_ENUM_VALUE(StudioStyleGuideColor, Separator, 90)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ButtonBorder, 91)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ButtonText, 92)
	RBX_ENUM_VALUE(StudioStyleGuideColor, InputFieldBorder, 93)
	RBX_ENUM_VALUE(StudioStyleGuideColor, CheckedFieldBackground, 94)
	RBX_ENUM_VALUE(StudioStyleGuideColor, CheckedFieldBorder, 95)
	RBX_ENUM_VALUE(StudioStyleGuideColor, CheckedFieldIndicator, 96)
	RBX_ENUM_VALUE(StudioStyleGuideColor, HeaderSection, 97)
	RBX_ENUM_VALUE(StudioStyleGuideColor, Midlight, 98)
	RBX_ENUM_VALUE(StudioStyleGuideColor, StatusBar, 99)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DialogButton, 100)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DialogButtonText, 101)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DialogButtonBorder, 102)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DialogMainButton, 103)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DialogMainButtonText, 104)
	RBX_ENUM_VALUE(StudioStyleGuideColor, InfoBarWarningBackground, 105)
	RBX_ENUM_VALUE(StudioStyleGuideColor, InfoBarWarningText, 106)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptEditorCurrentLine, 107)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptMethod, 108)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptProperty, 109)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptNil, 110)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptBool, 111)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptFunction, 112)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptLocal, 113)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptSelf, 114)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptLuauKeyword, 115)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptFunctionName, 116)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptTodo, 117)
	RBX_ENUM_VALUE(StudioStyleGuideColor, ScriptBracket, 118)
	RBX_ENUM_VALUE(StudioStyleGuideColor, AttributeCog, 119)
	RBX_ENUM_VALUE(StudioStyleGuideColor, AICOOverlayText, 128)
	RBX_ENUM_VALUE(StudioStyleGuideColor, AICOOverlayButtonBackground, 129)
	RBX_ENUM_VALUE(StudioStyleGuideColor, AICOOverlayButtonBackgroundHover, 130)
	RBX_ENUM_VALUE(StudioStyleGuideColor, AICOOverlayButtonBackgroundPressed, 131)
	RBX_ENUM_VALUE(StudioStyleGuideColor, OnboardingCover, 132)
	RBX_ENUM_VALUE(StudioStyleGuideColor, OnboardingHighlight, 133)
	RBX_ENUM_VALUE(StudioStyleGuideColor, OnboardingShadow, 134)
	RBX_ENUM_VALUE(StudioStyleGuideColor, BreakpointMarker, 136)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffLineNumHover, 137)
	RBX_ENUM_VALUE(StudioStyleGuideColor, DiffLineNumSeparatorBackgroundHover, 138)
RBX_ENUM_END(StudioStyleGuideColor, MainBackground, Titlebar, Dropdown, Tooltip, Notification, ScrollBar, ScrollBarBackground, TabBar, Tab, FilterButtonDefault, FilterButtonHover, FilterButtonChecked, FilterButtonAccent, FilterButtonBorder, FilterButtonBorderAlt, RibbonTab, RibbonTabTopBar, Button, MainButton, RibbonButton, ViewPortBackground, InputFieldBackground, Item, TableItem, CategoryItem, GameSettingsTableItem, GameSettingsTooltip, EmulatorBar, EmulatorDropDown, ColorPickerFrame, CurrentMarker, Border, DropShadow, Shadow, Light, Dark, Mid, MainText, SubText, TitlebarText, BrightText, DimmedText, LinkText, WarningText, ErrorText, InfoText, SensitiveText, ScriptSideWidget, ScriptBackground, ScriptText, ScriptSelectionText, ScriptSelectionBackground, ScriptFindSelectionBackground, ScriptMatchingWordSelectionBackground, ScriptOperator, ScriptNumber, ScriptString, ScriptComment, ScriptKeyword, ScriptBuiltInFunction, ScriptWarning, ScriptError, ScriptInformation, ScriptHint, ScriptWhitespace, ScriptRuler, DocViewCodeBackground, DebuggerCurrentLine, DebuggerErrorLine, DiffFilePathText, DiffTextHunkInfo, DiffTextNoChange, DiffTextAddition, DiffTextDeletion, DiffTextSeparatorBackground, DiffTextNoChangeBackground, DiffTextAdditionBackground, DiffTextDeletionBackground, DiffLineNum, DiffLineNumSeparatorBackground, DiffLineNumNoChangeBackground, DiffLineNumAdditionBackground, DiffLineNumDeletionBackground, DiffFilePathBackground, DiffFilePathBorder, ChatIncomingBgColor, ChatIncomingTextColor, ChatOutgoingBgColor, ChatOutgoingTextColor, ChatModeratedMessageColor, Separator, ButtonBorder, ButtonText, InputFieldBorder, CheckedFieldBackground, CheckedFieldBorder, CheckedFieldIndicator, HeaderSection, Midlight, StatusBar, DialogButton, DialogButtonText, DialogButtonBorder, DialogMainButton, DialogMainButtonText, InfoBarWarningBackground, InfoBarWarningText, ScriptEditorCurrentLine, ScriptMethod, ScriptProperty, ScriptNil, ScriptBool, ScriptFunction, ScriptLocal, ScriptSelf, ScriptLuauKeyword, ScriptFunctionName, ScriptTodo, ScriptBracket, AttributeCog, AICOOverlayText, AICOOverlayButtonBackground, AICOOverlayButtonBackgroundHover, AICOOverlayButtonBackgroundPressed, OnboardingCover, OnboardingHighlight, OnboardingShadow, BreakpointMarker, DiffLineNumHover, DiffLineNumSeparatorBackgroundHover)

RBX_ENUM_BEGIN(StudioStyleGuideModifier)
	RBX_ENUM_VALUE(StudioStyleGuideModifier, Default, 0)
	RBX_ENUM_VALUE(StudioStyleGuideModifier, Selected, 1)
	RBX_ENUM_VALUE(StudioStyleGuideModifier, Pressed, 2)
	RBX_ENUM_VALUE(StudioStyleGuideModifier, Disabled, 3)
	RBX_ENUM_VALUE(StudioStyleGuideModifier, Hover, 4)
RBX_ENUM_END(StudioStyleGuideModifier, Default, Selected, Pressed, Disabled, Hover)

RBX_ENUM_BEGIN(Style)
	RBX_ENUM_VALUE(Style, AlternatingSupports, 0)
	RBX_ENUM_VALUE(Style, BridgeStyleSupports, 1)
	RBX_ENUM_VALUE(Style, NoSupports, 2)
RBX_ENUM_END(Style, AlternatingSupports, BridgeStyleSupports, NoSupports)

RBX_ENUM_BEGIN(SubscriptionExpirationReason)
	RBX_ENUM_VALUE(SubscriptionExpirationReason, ProductInactive, 0)
	RBX_ENUM_VALUE(SubscriptionExpirationReason, ProductDeleted, 1)
	RBX_ENUM_VALUE(SubscriptionExpirationReason, SubscriberCancelled, 2)
	RBX_ENUM_VALUE(SubscriptionExpirationReason, SubscriberRefunded, 3)
	RBX_ENUM_VALUE(SubscriptionExpirationReason, Lapsed, 4)
RBX_ENUM_END(SubscriptionExpirationReason, ProductInactive, ProductDeleted, SubscriberCancelled, SubscriberRefunded, Lapsed)

RBX_ENUM_BEGIN(SubscriptionPaymentStatus)
	RBX_ENUM_VALUE(SubscriptionPaymentStatus, Paid, 0)
	RBX_ENUM_VALUE(SubscriptionPaymentStatus, Refunded, 1)
RBX_ENUM_END(SubscriptionPaymentStatus, Paid, Refunded)

RBX_ENUM_BEGIN(SubscriptionPeriod)
	RBX_ENUM_VALUE(SubscriptionPeriod, Month, 0)
RBX_ENUM_END(SubscriptionPeriod, Month)

RBX_ENUM_BEGIN(SubscriptionState)
	RBX_ENUM_VALUE(SubscriptionState, NeverSubscribed, 0)
	RBX_ENUM_VALUE(SubscriptionState, SubscribedWillRenew, 1)
	RBX_ENUM_VALUE(SubscriptionState, SubscribedWillNotRenew, 2)
	RBX_ENUM_VALUE(SubscriptionState, SubscribedRenewalPaymentPending, 3)
	RBX_ENUM_VALUE(SubscriptionState, Expired, 4)
RBX_ENUM_END(SubscriptionState, NeverSubscribed, SubscribedWillRenew, SubscribedWillNotRenew, SubscribedRenewalPaymentPending, Expired)

RBX_ENUM_BEGIN(SurfaceConstraint)
	RBX_ENUM_VALUE(SurfaceConstraint, None, 0)
	RBX_ENUM_VALUE(SurfaceConstraint, Hinge, 1)
	RBX_ENUM_VALUE(SurfaceConstraint, SteppingMotor, 2)
	RBX_ENUM_VALUE(SurfaceConstraint, Motor, 3)
RBX_ENUM_END(SurfaceConstraint, None, Hinge, SteppingMotor, Motor)

RBX_ENUM_BEGIN(SurfaceGuiShape)
	RBX_ENUM_VALUE(SurfaceGuiShape, Flat, 0)
	RBX_ENUM_VALUE(SurfaceGuiShape, CurvedHorizontally, 1)
RBX_ENUM_END(SurfaceGuiShape, Flat, CurvedHorizontally)

RBX_ENUM_BEGIN(SurfaceGuiSizingMode)
	RBX_ENUM_VALUE(SurfaceGuiSizingMode, FixedSize, 0)
	RBX_ENUM_VALUE(SurfaceGuiSizingMode, PixelsPerStud, 1)
RBX_ENUM_END(SurfaceGuiSizingMode, FixedSize, PixelsPerStud)

RBX_ENUM_BEGIN(SurfaceType)
	RBX_ENUM_VALUE(SurfaceType, Smooth, 0)
	RBX_ENUM_VALUE(SurfaceType, Glue, 1)
	RBX_ENUM_VALUE(SurfaceType, Weld, 2)
	RBX_ENUM_VALUE(SurfaceType, Studs, 3)
	RBX_ENUM_VALUE(SurfaceType, Inlet, 4)
	RBX_ENUM_VALUE(SurfaceType, Universal, 5)
	RBX_ENUM_VALUE(SurfaceType, Hinge, 6)
	RBX_ENUM_VALUE(SurfaceType, Motor, 7)
	RBX_ENUM_VALUE(SurfaceType, SteppingMotor, 8)
	RBX_ENUM_VALUE(SurfaceType, SmoothNoOutlines, 10)
RBX_ENUM_END(SurfaceType, Smooth, Glue, Weld, Studs, Inlet, Universal, Hinge, Motor, SteppingMotor, SmoothNoOutlines)

RBX_ENUM_BEGIN(SwipeDirection)
	RBX_ENUM_VALUE(SwipeDirection, Right, 0)
	RBX_ENUM_VALUE(SwipeDirection, Left, 1)
	RBX_ENUM_VALUE(SwipeDirection, Up, 2)
	RBX_ENUM_VALUE(SwipeDirection, Down, 3)
	RBX_ENUM_VALUE(SwipeDirection, None, 4)
RBX_ENUM_END(SwipeDirection, Right, Left, Up, Down, None)

RBX_ENUM_BEGIN(SystemThemeValue)
	RBX_ENUM_VALUE(SystemThemeValue, error, 0)
	RBX_ENUM_VALUE(SystemThemeValue, light, 1)
	RBX_ENUM_VALUE(SystemThemeValue, dark, 2)
	RBX_ENUM_VALUE(SystemThemeValue, systemLight, 3)
	RBX_ENUM_VALUE(SystemThemeValue, systemDark, 4)
RBX_ENUM_END(SystemThemeValue, error, light, dark, systemLight, systemDark)

RBX_ENUM_BEGIN(TableMajorAxis)
	RBX_ENUM_VALUE(TableMajorAxis, RowMajor, 0)
	RBX_ENUM_VALUE(TableMajorAxis, ColumnMajor, 1)
RBX_ENUM_END(TableMajorAxis, RowMajor, ColumnMajor)

RBX_ENUM_BEGIN(TeamCreateErrorState)
	RBX_ENUM_VALUE(TeamCreateErrorState, PlaceSizeTooLarge, 0)
	RBX_ENUM_VALUE(TeamCreateErrorState, PlaceSizeApproachingLimit, 1)
	RBX_ENUM_VALUE(TeamCreateErrorState, PlaceUploadFailing, 2)
	RBX_ENUM_VALUE(TeamCreateErrorState, NoError, 3)
RBX_ENUM_END(TeamCreateErrorState, PlaceSizeTooLarge, PlaceSizeApproachingLimit, PlaceUploadFailing, NoError)

RBX_ENUM_BEGIN(Technology)
	RBX_ENUM_VALUE(Technology, Legacy, 0)
	RBX_ENUM_VALUE(Technology, Voxel, 1)
	RBX_ENUM_VALUE(Technology, Compatibility, 2)
	RBX_ENUM_VALUE(Technology, ShadowMap, 3)
	RBX_ENUM_VALUE(Technology, Future, 4)
	RBX_ENUM_VALUE(Technology, Unified, 5)
RBX_ENUM_END(Technology, Legacy, Voxel, Compatibility, ShadowMap, Future, Unified)

RBX_ENUM_BEGIN(TelemetryBackend)
	RBX_ENUM_VALUE(TelemetryBackend, UNSPECIFIED, 0)
	RBX_ENUM_VALUE(TelemetryBackend, EventIngest, 1)
	RBX_ENUM_VALUE(TelemetryBackend, Points, 2)
	RBX_ENUM_VALUE(TelemetryBackend, Teletune, 3)
	RBX_ENUM_VALUE(TelemetryBackend, EphemeralCounter, 4)
	RBX_ENUM_VALUE(TelemetryBackend, EphemeralStat, 5)
	RBX_ENUM_VALUE(TelemetryBackend, Counter, 6)
	RBX_ENUM_VALUE(TelemetryBackend, Stat, 7)
RBX_ENUM_END(TelemetryBackend, UNSPECIFIED, EventIngest, Points, Teletune, EphemeralCounter, EphemeralStat, Counter, Stat)

RBX_ENUM_BEGIN(TelemetryStandardizedField)
	RBX_ENUM_VALUE(TelemetryStandardizedField, AddDatacenterId, 0)
	RBX_ENUM_VALUE(TelemetryStandardizedField, AddPlaceId, 1)
	RBX_ENUM_VALUE(TelemetryStandardizedField, AddUniverseId, 2)
	RBX_ENUM_VALUE(TelemetryStandardizedField, AddPlaceInstanceId, 3)
	RBX_ENUM_VALUE(TelemetryStandardizedField, AddPlaySessionId, 4)
	RBX_ENUM_VALUE(TelemetryStandardizedField, AddCurrentContextName, 5)
	RBX_ENUM_VALUE(TelemetryStandardizedField, AddOsInfo, 6)
	RBX_ENUM_VALUE(TelemetryStandardizedField, AddArchitectureInfo, 7)
	RBX_ENUM_VALUE(TelemetryStandardizedField, AddCpuInfo, 8)
	RBX_ENUM_VALUE(TelemetryStandardizedField, AddMemoryInfo, 9)
	RBX_ENUM_VALUE(TelemetryStandardizedField, AddSessionInfo, 10)
RBX_ENUM_END(TelemetryStandardizedField, AddDatacenterId, AddPlaceId, AddUniverseId, AddPlaceInstanceId, AddPlaySessionId, AddCurrentContextName, AddOsInfo, AddArchitectureInfo, AddCpuInfo, AddMemoryInfo, AddSessionInfo)

RBX_ENUM_BEGIN(TeleportMethod)
	RBX_ENUM_VALUE(TeleportMethod, TeleportToSpawnByName, 0)
	RBX_ENUM_VALUE(TeleportMethod, TeleportToPlaceInstance, 1)
	RBX_ENUM_VALUE(TeleportMethod, TeleportToPrivateServer, 2)
	RBX_ENUM_VALUE(TeleportMethod, TeleportPartyAsync, 3)
	RBX_ENUM_VALUE(TeleportMethod, TeleportToVIPServer, 4)
	RBX_ENUM_VALUE(TeleportMethod, TeleportToInstanceBack, 5)
	RBX_ENUM_VALUE(TeleportMethod, TeleportUnknown, 6)
RBX_ENUM_END(TeleportMethod, TeleportToSpawnByName, TeleportToPlaceInstance, TeleportToPrivateServer, TeleportPartyAsync, TeleportToVIPServer, TeleportToInstanceBack, TeleportUnknown)

RBX_ENUM_BEGIN(TeleportResult)
	RBX_ENUM_VALUE(TeleportResult, Success, 0)
	RBX_ENUM_VALUE(TeleportResult, Failure, 1)
	RBX_ENUM_VALUE(TeleportResult, GameNotFound, 2)
	RBX_ENUM_VALUE(TeleportResult, GameEnded, 3)
	RBX_ENUM_VALUE(TeleportResult, GameFull, 4)
	RBX_ENUM_VALUE(TeleportResult, Unauthorized, 5)
	RBX_ENUM_VALUE(TeleportResult, Flooded, 6)
	RBX_ENUM_VALUE(TeleportResult, IsTeleporting, 7)
RBX_ENUM_END(TeleportResult, Success, Failure, GameNotFound, GameEnded, GameFull, Unauthorized, Flooded, IsTeleporting)

RBX_ENUM_BEGIN(TeleportState)
	RBX_ENUM_VALUE(TeleportState, RequestedFromServer, 0)
	RBX_ENUM_VALUE(TeleportState, Started, 1)
	RBX_ENUM_VALUE(TeleportState, WaitingForServer, 2)
	RBX_ENUM_VALUE(TeleportState, Failed, 3)
	RBX_ENUM_VALUE(TeleportState, InProgress, 4)
RBX_ENUM_END(TeleportState, RequestedFromServer, Started, WaitingForServer, Failed, InProgress)

RBX_ENUM_BEGIN(TeleportType)
	RBX_ENUM_VALUE(TeleportType, ToPlace, 0)
	RBX_ENUM_VALUE(TeleportType, ToInstance, 1)
	RBX_ENUM_VALUE(TeleportType, ToReservedServer, 2)
	RBX_ENUM_VALUE(TeleportType, ToVIPServer, 3)
	RBX_ENUM_VALUE(TeleportType, ToInstanceBack, 4)
RBX_ENUM_END(TeleportType, ToPlace, ToInstance, ToReservedServer, ToVIPServer, ToInstanceBack)

RBX_ENUM_BEGIN(TerrainAcquisitionMethod)
	RBX_ENUM_VALUE(TerrainAcquisitionMethod, None, 0)
	RBX_ENUM_VALUE(TerrainAcquisitionMethod, Legacy, 1)
	RBX_ENUM_VALUE(TerrainAcquisitionMethod, Template, 2)
	RBX_ENUM_VALUE(TerrainAcquisitionMethod, Generate, 3)
	RBX_ENUM_VALUE(TerrainAcquisitionMethod, Import, 4)
	RBX_ENUM_VALUE(TerrainAcquisitionMethod, Convert, 5)
	RBX_ENUM_VALUE(TerrainAcquisitionMethod, EditAddTool, 6)
	RBX_ENUM_VALUE(TerrainAcquisitionMethod, EditSeaLevelTool, 7)
	RBX_ENUM_VALUE(TerrainAcquisitionMethod, EditReplaceTool, 8)
	RBX_ENUM_VALUE(TerrainAcquisitionMethod, RegionFillTool, 9)
	RBX_ENUM_VALUE(TerrainAcquisitionMethod, RegionPasteTool, 10)
	RBX_ENUM_VALUE(TerrainAcquisitionMethod, Other, 11)
RBX_ENUM_END(TerrainAcquisitionMethod, None, Legacy, Template, Generate, Import, Convert, EditAddTool, EditSeaLevelTool, EditReplaceTool, RegionFillTool, RegionPasteTool, Other)

RBX_ENUM_BEGIN(TerrainFace)
	RBX_ENUM_VALUE(TerrainFace, Top, 0)
	RBX_ENUM_VALUE(TerrainFace, Side, 1)
	RBX_ENUM_VALUE(TerrainFace, Bottom, 2)
RBX_ENUM_END(TerrainFace, Top, Side, Bottom)

RBX_ENUM_BEGIN(TerrainLiquidMergeOperation)
	RBX_ENUM_VALUE(TerrainLiquidMergeOperation, None, 0)
	RBX_ENUM_VALUE(TerrainLiquidMergeOperation, Source, 1)
	RBX_ENUM_VALUE(TerrainLiquidMergeOperation, Union, 2)
	RBX_ENUM_VALUE(TerrainLiquidMergeOperation, Difference, 3)
	RBX_ENUM_VALUE(TerrainLiquidMergeOperation, Intersect, 4)
RBX_ENUM_END(TerrainLiquidMergeOperation, None, Source, Union, Difference, Intersect)

RBX_ENUM_BEGIN(TerrainSolidMergeOperation)
	RBX_ENUM_VALUE(TerrainSolidMergeOperation, None, 0)
	RBX_ENUM_VALUE(TerrainSolidMergeOperation, Paint, 1)
	RBX_ENUM_VALUE(TerrainSolidMergeOperation, Source, 2)
	RBX_ENUM_VALUE(TerrainSolidMergeOperation, Union, 3)
	RBX_ENUM_VALUE(TerrainSolidMergeOperation, Dig, 4)
	RBX_ENUM_VALUE(TerrainSolidMergeOperation, Difference, 5)
	RBX_ENUM_VALUE(TerrainSolidMergeOperation, Intersect, 6)
	RBX_ENUM_VALUE(TerrainSolidMergeOperation, Cut, 7)
	RBX_ENUM_VALUE(TerrainSolidMergeOperation, Place, 8)
RBX_ENUM_END(TerrainSolidMergeOperation, None, Paint, Source, Union, Dig, Difference, Intersect, Cut, Place)

RBX_ENUM_BEGIN(TextChatMessageStatus)
	RBX_ENUM_VALUE(TextChatMessageStatus, Unknown, 1)
	RBX_ENUM_VALUE(TextChatMessageStatus, Success, 2)
	RBX_ENUM_VALUE(TextChatMessageStatus, Sending, 3)
	RBX_ENUM_VALUE(TextChatMessageStatus, TextFilterFailed, 4)
	RBX_ENUM_VALUE(TextChatMessageStatus, Floodchecked, 5)
	RBX_ENUM_VALUE(TextChatMessageStatus, InvalidPrivacySettings, 6)
	RBX_ENUM_VALUE(TextChatMessageStatus, InvalidTextChannelPermissions, 7)
	RBX_ENUM_VALUE(TextChatMessageStatus, MessageTooLong, 8)
	RBX_ENUM_VALUE(TextChatMessageStatus, ModerationTimeout, 9)
RBX_ENUM_END(TextChatMessageStatus, Unknown, Success, Sending, TextFilterFailed, Floodchecked, InvalidPrivacySettings, InvalidTextChannelPermissions, MessageTooLong, ModerationTimeout)

RBX_ENUM_BEGIN(TextDirection)
	RBX_ENUM_VALUE(TextDirection, Auto, 0)
	RBX_ENUM_VALUE(TextDirection, LeftToRight, 1)
	RBX_ENUM_VALUE(TextDirection, RightToLeft, 2)
RBX_ENUM_END(TextDirection, Auto, LeftToRight, RightToLeft)

RBX_ENUM_BEGIN(TextFilterContext)
	RBX_ENUM_VALUE(TextFilterContext, PublicChat, 1)
	RBX_ENUM_VALUE(TextFilterContext, PrivateChat, 2)
RBX_ENUM_END(TextFilterContext, PublicChat, PrivateChat)

RBX_ENUM_BEGIN(TextInputType)
	RBX_ENUM_VALUE(TextInputType, Default, 0)
	RBX_ENUM_VALUE(TextInputType, NoSuggestions, 1)
	RBX_ENUM_VALUE(TextInputType, Number, 2)
	RBX_ENUM_VALUE(TextInputType, Email, 3)
	RBX_ENUM_VALUE(TextInputType, Phone, 4)
	RBX_ENUM_VALUE(TextInputType, Password, 5)
	RBX_ENUM_VALUE(TextInputType, PasswordShown, 6)
	RBX_ENUM_VALUE(TextInputType, Username, 7)
	RBX_ENUM_VALUE(TextInputType, OneTimePassword, 8)
	RBX_ENUM_VALUE(TextInputType, NewPassword, 9)
	RBX_ENUM_VALUE(TextInputType, NewPasswordShown, 10)
RBX_ENUM_END(TextInputType, Default, NoSuggestions, Number, Email, Phone, Password, PasswordShown, Username, OneTimePassword, NewPassword, NewPasswordShown)

RBX_ENUM_BEGIN(TextTruncate)
	RBX_ENUM_VALUE(TextTruncate, None, 0)
	RBX_ENUM_VALUE(TextTruncate, AtEnd, 1)
	RBX_ENUM_VALUE(TextTruncate, SplitWord, 2)
RBX_ENUM_END(TextTruncate, None, AtEnd, SplitWord)

RBX_ENUM_BEGIN(TextXAlignment)
	RBX_ENUM_VALUE(TextXAlignment, Left, 0)
	RBX_ENUM_VALUE(TextXAlignment, Right, 1)
	RBX_ENUM_VALUE(TextXAlignment, Center, 2)
RBX_ENUM_END(TextXAlignment, Left, Right, Center)

RBX_ENUM_BEGIN(TextYAlignment)
	RBX_ENUM_VALUE(TextYAlignment, Top, 0)
	RBX_ENUM_VALUE(TextYAlignment, Center, 1)
	RBX_ENUM_VALUE(TextYAlignment, Bottom, 2)
RBX_ENUM_END(TextYAlignment, Top, Center, Bottom)

RBX_ENUM_BEGIN(TextureMode)
	RBX_ENUM_VALUE(TextureMode, Stretch, 0)
	RBX_ENUM_VALUE(TextureMode, Wrap, 1)
	RBX_ENUM_VALUE(TextureMode, Static, 2)
RBX_ENUM_END(TextureMode, Stretch, Wrap, Static)

RBX_ENUM_BEGIN(TextureQueryType)
	RBX_ENUM_VALUE(TextureQueryType, NonHumanoid, 0)
	RBX_ENUM_VALUE(TextureQueryType, NonHumanoidOrphaned, 1)
	RBX_ENUM_VALUE(TextureQueryType, Humanoid, 2)
	RBX_ENUM_VALUE(TextureQueryType, HumanoidOrphaned, 3)
RBX_ENUM_END(TextureQueryType, NonHumanoid, NonHumanoidOrphaned, Humanoid, HumanoidOrphaned)

RBX_ENUM_BEGIN(ThreadPoolConfig)
	RBX_ENUM_VALUE(ThreadPoolConfig, Auto, 0)
	RBX_ENUM_VALUE(ThreadPoolConfig, Threads1, 1)
	RBX_ENUM_VALUE(ThreadPoolConfig, Threads2, 2)
	RBX_ENUM_VALUE(ThreadPoolConfig, Threads3, 3)
	RBX_ENUM_VALUE(ThreadPoolConfig, Threads4, 4)
	RBX_ENUM_VALUE(ThreadPoolConfig, Threads8, 8)
	RBX_ENUM_VALUE(ThreadPoolConfig, Threads16, 16)
	RBX_ENUM_VALUE(ThreadPoolConfig, PerCore1, 101)
	RBX_ENUM_VALUE(ThreadPoolConfig, PerCore2, 102)
	RBX_ENUM_VALUE(ThreadPoolConfig, PerCore3, 103)
	RBX_ENUM_VALUE(ThreadPoolConfig, PerCore4, 104)
RBX_ENUM_END(ThreadPoolConfig, Auto, Threads1, Threads2, Threads3, Threads4, Threads8, Threads16, PerCore1, PerCore2, PerCore3, PerCore4)

RBX_ENUM_BEGIN(ThrottlingPriority)
	RBX_ENUM_VALUE(ThrottlingPriority, Default, 0)
	RBX_ENUM_VALUE(ThrottlingPriority, ElevatedOnServer, 1)
	RBX_ENUM_VALUE(ThrottlingPriority, Extreme, 2)
RBX_ENUM_END(ThrottlingPriority, Default, ElevatedOnServer, Extreme)

RBX_ENUM_BEGIN(ThumbnailSize)
	RBX_ENUM_VALUE(ThumbnailSize, Size48x48, 0)
	RBX_ENUM_VALUE(ThumbnailSize, Size180x180, 1)
	RBX_ENUM_VALUE(ThumbnailSize, Size420x420, 2)
	RBX_ENUM_VALUE(ThumbnailSize, Size60x60, 3)
	RBX_ENUM_VALUE(ThumbnailSize, Size100x100, 4)
	RBX_ENUM_VALUE(ThumbnailSize, Size150x150, 5)
	RBX_ENUM_VALUE(ThumbnailSize, Size352x352, 6)
RBX_ENUM_END(ThumbnailSize, Size48x48, Size180x180, Size420x420, Size60x60, Size100x100, Size150x150, Size352x352)

RBX_ENUM_BEGIN(ThumbnailType)
	RBX_ENUM_VALUE(ThumbnailType, HeadShot, 0)
	RBX_ENUM_VALUE(ThumbnailType, AvatarBust, 1)
	RBX_ENUM_VALUE(ThumbnailType, AvatarThumbnail, 2)
RBX_ENUM_END(ThumbnailType, HeadShot, AvatarBust, AvatarThumbnail)

RBX_ENUM_BEGIN(TickCountSampleMethod)
	RBX_ENUM_VALUE(TickCountSampleMethod, Fast, 0)
	RBX_ENUM_VALUE(TickCountSampleMethod, Benchmark, 1)
	RBX_ENUM_VALUE(TickCountSampleMethod, Precise, 2)
RBX_ENUM_END(TickCountSampleMethod, Fast, Benchmark, Precise)

RBX_ENUM_BEGIN(TonemapperPreset)
	RBX_ENUM_VALUE(TonemapperPreset, Default, 0)
	RBX_ENUM_VALUE(TonemapperPreset, Retro, 1)
RBX_ENUM_END(TonemapperPreset, Default, Retro)

RBX_ENUM_BEGIN(TopBottom)
	RBX_ENUM_VALUE(TopBottom, Top, 0)
	RBX_ENUM_VALUE(TopBottom, Center, 1)
	RBX_ENUM_VALUE(TopBottom, Bottom, 2)
RBX_ENUM_END(TopBottom, Top, Center, Bottom)

RBX_ENUM_BEGIN(TouchCameraMovementMode)
	RBX_ENUM_VALUE(TouchCameraMovementMode, Default, 0)
	RBX_ENUM_VALUE(TouchCameraMovementMode, Classic, 1)
	RBX_ENUM_VALUE(TouchCameraMovementMode, Follow, 2)
	RBX_ENUM_VALUE(TouchCameraMovementMode, Orbital, 3)
RBX_ENUM_END(TouchCameraMovementMode, Default, Classic, Follow, Orbital)

RBX_ENUM_BEGIN(TouchMovementMode)
	RBX_ENUM_VALUE(TouchMovementMode, Default, 0)
	RBX_ENUM_VALUE(TouchMovementMode, Thumbstick, 1)
	RBX_ENUM_VALUE(TouchMovementMode, DPad, 2)
	RBX_ENUM_VALUE(TouchMovementMode, Thumbpad, 3)
	RBX_ENUM_VALUE(TouchMovementMode, ClickToMove, 4)
	RBX_ENUM_VALUE(TouchMovementMode, DynamicThumbstick, 5)
RBX_ENUM_END(TouchMovementMode, Default, Thumbstick, DPad, Thumbpad, ClickToMove, DynamicThumbstick)

RBX_ENUM_BEGIN(TrackerError)
	RBX_ENUM_VALUE(TrackerError, Ok, 0)
	RBX_ENUM_VALUE(TrackerError, NoService, 1)
	RBX_ENUM_VALUE(TrackerError, InitFailed, 2)
	RBX_ENUM_VALUE(TrackerError, NoVideo, 3)
	RBX_ENUM_VALUE(TrackerError, VideoError, 4)
	RBX_ENUM_VALUE(TrackerError, VideoNoPermission, 5)
	RBX_ENUM_VALUE(TrackerError, VideoUnsupported, 6)
	RBX_ENUM_VALUE(TrackerError, NoAudio, 7)
	RBX_ENUM_VALUE(TrackerError, AudioError, 8)
	RBX_ENUM_VALUE(TrackerError, AudioNoPermission, 9)
	RBX_ENUM_VALUE(TrackerError, UnsupportedDevice, 10)
RBX_ENUM_END(TrackerError, Ok, NoService, InitFailed, NoVideo, VideoError, VideoNoPermission, VideoUnsupported, NoAudio, AudioError, AudioNoPermission, UnsupportedDevice)

RBX_ENUM_BEGIN(TrackerExtrapolationFlagMode)
	RBX_ENUM_VALUE(TrackerExtrapolationFlagMode, ForceDisabled, 0)
	RBX_ENUM_VALUE(TrackerExtrapolationFlagMode, ExtrapolateFacsAndPose, 1)
	RBX_ENUM_VALUE(TrackerExtrapolationFlagMode, ExtrapolateFacsOnly, 2)
	RBX_ENUM_VALUE(TrackerExtrapolationFlagMode, Auto, 3)
RBX_ENUM_END(TrackerExtrapolationFlagMode, ForceDisabled, ExtrapolateFacsAndPose, ExtrapolateFacsOnly, Auto)

RBX_ENUM_BEGIN(TrackerFaceTrackingStatus)
	RBX_ENUM_VALUE(TrackerFaceTrackingStatus, FaceTrackingSuccess, 0)
	RBX_ENUM_VALUE(TrackerFaceTrackingStatus, FaceTrackingNoFaceFound, 1)
	RBX_ENUM_VALUE(TrackerFaceTrackingStatus, FaceTrackingUnknown, 2)
	RBX_ENUM_VALUE(TrackerFaceTrackingStatus, FaceTrackingLost, 3)
	RBX_ENUM_VALUE(TrackerFaceTrackingStatus, FaceTrackingHasTrackingError, 4)
	RBX_ENUM_VALUE(TrackerFaceTrackingStatus, FaceTrackingIsOccluded, 5)
	RBX_ENUM_VALUE(TrackerFaceTrackingStatus, FaceTrackingUninitialized, 6)
RBX_ENUM_END(TrackerFaceTrackingStatus, FaceTrackingSuccess, FaceTrackingNoFaceFound, FaceTrackingUnknown, FaceTrackingLost, FaceTrackingHasTrackingError, FaceTrackingIsOccluded, FaceTrackingUninitialized)

RBX_ENUM_BEGIN(TrackerLodFlagMode)
	RBX_ENUM_VALUE(TrackerLodFlagMode, ForceFalse, 0)
	RBX_ENUM_VALUE(TrackerLodFlagMode, ForceTrue, 1)
	RBX_ENUM_VALUE(TrackerLodFlagMode, Auto, 2)
RBX_ENUM_END(TrackerLodFlagMode, ForceFalse, ForceTrue, Auto)

RBX_ENUM_BEGIN(TrackerLodValueMode)
	RBX_ENUM_VALUE(TrackerLodValueMode, Force0, 0)
	RBX_ENUM_VALUE(TrackerLodValueMode, Force1, 1)
	RBX_ENUM_VALUE(TrackerLodValueMode, Auto, 2)
RBX_ENUM_END(TrackerLodValueMode, Force0, Force1, Auto)

RBX_ENUM_BEGIN(TrackerMode)
	RBX_ENUM_VALUE(TrackerMode, None, 0)
	RBX_ENUM_VALUE(TrackerMode, Audio, 1)
	RBX_ENUM_VALUE(TrackerMode, Video, 2)
	RBX_ENUM_VALUE(TrackerMode, AudioVideo, 3)
RBX_ENUM_END(TrackerMode, None, Audio, Video, AudioVideo)

RBX_ENUM_BEGIN(TrackerPromptEvent)
	RBX_ENUM_VALUE(TrackerPromptEvent, LODCameraRecommendDisable, 0)
RBX_ENUM_END(TrackerPromptEvent, LODCameraRecommendDisable)

RBX_ENUM_BEGIN(TrackerType)
	RBX_ENUM_VALUE(TrackerType, None, 0)
	RBX_ENUM_VALUE(TrackerType, Face, 1)
	RBX_ENUM_VALUE(TrackerType, UpperBody, 2)
RBX_ENUM_END(TrackerType, None, Face, UpperBody)

RBX_ENUM_BEGIN(TriStateBoolean)
	RBX_ENUM_VALUE(TriStateBoolean, Unknown, 0)
	RBX_ENUM_VALUE(TriStateBoolean, True, 1)
	RBX_ENUM_VALUE(TriStateBoolean, False, 2)
RBX_ENUM_END(TriStateBoolean, Unknown, True, False)

RBX_ENUM_BEGIN(TweenStatus)
	RBX_ENUM_VALUE(TweenStatus, Canceled, 0)
	RBX_ENUM_VALUE(TweenStatus, Completed, 1)
RBX_ENUM_END(TweenStatus, Canceled, Completed)

RBX_ENUM_BEGIN(UICaptureMode)
	RBX_ENUM_VALUE(UICaptureMode, All, 0)
	RBX_ENUM_VALUE(UICaptureMode, None, 1)
RBX_ENUM_END(UICaptureMode, All, None)

RBX_ENUM_BEGIN(UIDragDetectorBoundingBehavior)
	RBX_ENUM_VALUE(UIDragDetectorBoundingBehavior, Automatic, 0)
	RBX_ENUM_VALUE(UIDragDetectorBoundingBehavior, EntireObject, 1)
	RBX_ENUM_VALUE(UIDragDetectorBoundingBehavior, HitPoint, 2)
RBX_ENUM_END(UIDragDetectorBoundingBehavior, Automatic, EntireObject, HitPoint)

RBX_ENUM_BEGIN(UIDragDetectorDragRelativity)
	RBX_ENUM_VALUE(UIDragDetectorDragRelativity, Absolute, 0)
	RBX_ENUM_VALUE(UIDragDetectorDragRelativity, Relative, 1)
RBX_ENUM_END(UIDragDetectorDragRelativity, Absolute, Relative)

RBX_ENUM_BEGIN(UIDragDetectorDragSpace)
	RBX_ENUM_VALUE(UIDragDetectorDragSpace, Parent, 0)
	RBX_ENUM_VALUE(UIDragDetectorDragSpace, LayerCollector, 1)
	RBX_ENUM_VALUE(UIDragDetectorDragSpace, Reference, 2)
RBX_ENUM_END(UIDragDetectorDragSpace, Parent, LayerCollector, Reference)

RBX_ENUM_BEGIN(UIDragDetectorDragStyle)
	RBX_ENUM_VALUE(UIDragDetectorDragStyle, TranslatePlane, 0)
	RBX_ENUM_VALUE(UIDragDetectorDragStyle, TranslateLine, 1)
	RBX_ENUM_VALUE(UIDragDetectorDragStyle, Rotate, 2)
	RBX_ENUM_VALUE(UIDragDetectorDragStyle, Scriptable, 3)
RBX_ENUM_END(UIDragDetectorDragStyle, TranslatePlane, TranslateLine, Rotate, Scriptable)

RBX_ENUM_BEGIN(UIDragDetectorResponseStyle)
	RBX_ENUM_VALUE(UIDragDetectorResponseStyle, Offset, 0)
	RBX_ENUM_VALUE(UIDragDetectorResponseStyle, Scale, 1)
	RBX_ENUM_VALUE(UIDragDetectorResponseStyle, CustomOffset, 2)
	RBX_ENUM_VALUE(UIDragDetectorResponseStyle, CustomScale, 3)
RBX_ENUM_END(UIDragDetectorResponseStyle, Offset, Scale, CustomOffset, CustomScale)

RBX_ENUM_BEGIN(UIDragSpeedAxisMapping)
	RBX_ENUM_VALUE(UIDragSpeedAxisMapping, XY, 0)
	RBX_ENUM_VALUE(UIDragSpeedAxisMapping, XX, 1)
	RBX_ENUM_VALUE(UIDragSpeedAxisMapping, YY, 2)
RBX_ENUM_END(UIDragSpeedAxisMapping, XY, XX, YY)

RBX_ENUM_BEGIN(UIFlexAlignment)
	RBX_ENUM_VALUE(UIFlexAlignment, None, 0)
	RBX_ENUM_VALUE(UIFlexAlignment, Fill, 1)
	RBX_ENUM_VALUE(UIFlexAlignment, SpaceAround, 2)
	RBX_ENUM_VALUE(UIFlexAlignment, SpaceBetween, 3)
	RBX_ENUM_VALUE(UIFlexAlignment, SpaceEvenly, 4)
RBX_ENUM_END(UIFlexAlignment, None, Fill, SpaceAround, SpaceBetween, SpaceEvenly)

RBX_ENUM_BEGIN(UIFlexMode)
	RBX_ENUM_VALUE(UIFlexMode, None, 0)
	RBX_ENUM_VALUE(UIFlexMode, Grow, 1)
	RBX_ENUM_VALUE(UIFlexMode, Shrink, 2)
	RBX_ENUM_VALUE(UIFlexMode, Fill, 3)
	RBX_ENUM_VALUE(UIFlexMode, Custom, 4)
RBX_ENUM_END(UIFlexMode, None, Grow, Shrink, Fill, Custom)

RBX_ENUM_BEGIN(UITheme)
	RBX_ENUM_VALUE(UITheme, Light, 0)
	RBX_ENUM_VALUE(UITheme, Dark, 1)
RBX_ENUM_END(UITheme, Light, Dark)

RBX_ENUM_BEGIN(UiMessageType)
	RBX_ENUM_VALUE(UiMessageType, UiMessageError, 0)
	RBX_ENUM_VALUE(UiMessageType, UiMessageInfo, 1)
RBX_ENUM_END(UiMessageType, UiMessageError, UiMessageInfo)

RBX_ENUM_BEGIN(UpdateState)
	RBX_ENUM_VALUE(UpdateState, UpdateNotAvailable, 0)
	RBX_ENUM_VALUE(UpdateState, UpdateAvailable, 1)
	RBX_ENUM_VALUE(UpdateState, UpdateInProgress, 2)
	RBX_ENUM_VALUE(UpdateState, UpdateReady, 3)
	RBX_ENUM_VALUE(UpdateState, UpdateFailed, 4)
RBX_ENUM_END(UpdateState, UpdateNotAvailable, UpdateAvailable, UpdateInProgress, UpdateReady, UpdateFailed)

RBX_ENUM_BEGIN(UploadCaptureResult)
	RBX_ENUM_VALUE(UploadCaptureResult, Success, 0)
	RBX_ENUM_VALUE(UploadCaptureResult, NeedPermission, 1)
	RBX_ENUM_VALUE(UploadCaptureResult, CaptureModerated, 2)
	RBX_ENUM_VALUE(UploadCaptureResult, CaptureNotInGallery, 3)
	RBX_ENUM_VALUE(UploadCaptureResult, IneligibleCapture, 4)
	RBX_ENUM_VALUE(UploadCaptureResult, UploadQuotaReached, 5)
	RBX_ENUM_VALUE(UploadCaptureResult, UploadPending, 6)
	RBX_ENUM_VALUE(UploadCaptureResult, UploadFailed, 7)
RBX_ENUM_END(UploadCaptureResult, Success, NeedPermission, CaptureModerated, CaptureNotInGallery, IneligibleCapture, UploadQuotaReached, UploadPending, UploadFailed)

RBX_ENUM_BEGIN(UsageContext)
	RBX_ENUM_VALUE(UsageContext, Default, 0)
	RBX_ENUM_VALUE(UsageContext, Preview, 1)
RBX_ENUM_END(UsageContext, Default, Preview)

RBX_ENUM_BEGIN(UserCFrame)
	RBX_ENUM_VALUE(UserCFrame, Head, 0)
	RBX_ENUM_VALUE(UserCFrame, LeftHand, 1)
	RBX_ENUM_VALUE(UserCFrame, RightHand, 2)
	RBX_ENUM_VALUE(UserCFrame, Floor, 3)
RBX_ENUM_END(UserCFrame, Head, LeftHand, RightHand, Floor)

RBX_ENUM_BEGIN(UserInputState)
	RBX_ENUM_VALUE(UserInputState, Begin, 0)
	RBX_ENUM_VALUE(UserInputState, Change, 1)
	RBX_ENUM_VALUE(UserInputState, End, 2)
	RBX_ENUM_VALUE(UserInputState, Cancel, 3)
	RBX_ENUM_VALUE(UserInputState, None, 4)
RBX_ENUM_END(UserInputState, Begin, Change, End, Cancel, None)

RBX_ENUM_BEGIN(UserInputType)
	RBX_ENUM_VALUE(UserInputType, MouseButton1, 0)
	RBX_ENUM_VALUE(UserInputType, MouseButton2, 1)
	RBX_ENUM_VALUE(UserInputType, MouseButton3, 2)
	RBX_ENUM_VALUE(UserInputType, MouseWheel, 3)
	RBX_ENUM_VALUE(UserInputType, MouseMovement, 4)
	RBX_ENUM_VALUE(UserInputType, Touch, 7)
	RBX_ENUM_VALUE(UserInputType, Keyboard, 8)
	RBX_ENUM_VALUE(UserInputType, Focus, 9)
	RBX_ENUM_VALUE(UserInputType, Accelerometer, 10)
	RBX_ENUM_VALUE(UserInputType, Gyro, 11)
	RBX_ENUM_VALUE(UserInputType, Gamepad1, 12)
	RBX_ENUM_VALUE(UserInputType, Gamepad2, 13)
	RBX_ENUM_VALUE(UserInputType, Gamepad3, 14)
	RBX_ENUM_VALUE(UserInputType, Gamepad4, 15)
	RBX_ENUM_VALUE(UserInputType, Gamepad5, 16)
	RBX_ENUM_VALUE(UserInputType, Gamepad6, 17)
	RBX_ENUM_VALUE(UserInputType, Gamepad7, 18)
	RBX_ENUM_VALUE(UserInputType, Gamepad8, 19)
	RBX_ENUM_VALUE(UserInputType, TextInput, 20)
	RBX_ENUM_VALUE(UserInputType, InputMethod, 21)
	RBX_ENUM_VALUE(UserInputType, None, 22)
RBX_ENUM_END(UserInputType, MouseButton1, MouseButton2, MouseButton3, MouseWheel, MouseMovement, Touch, Keyboard, Focus, Accelerometer, Gyro, Gamepad1, Gamepad2, Gamepad3, Gamepad4, Gamepad5, Gamepad6, Gamepad7, Gamepad8, TextInput, InputMethod, None)

RBX_ENUM_BEGIN(VRComfortSetting)
	RBX_ENUM_VALUE(VRComfortSetting, Comfort, 0)
	RBX_ENUM_VALUE(VRComfortSetting, Normal, 1)
	RBX_ENUM_VALUE(VRComfortSetting, Expert, 2)
	RBX_ENUM_VALUE(VRComfortSetting, Custom, 3)
RBX_ENUM_END(VRComfortSetting, Comfort, Normal, Expert, Custom)

RBX_ENUM_BEGIN(VRControllerModelMode)
	RBX_ENUM_VALUE(VRControllerModelMode, Disabled, 0)
	RBX_ENUM_VALUE(VRControllerModelMode, Transparent, 1)
RBX_ENUM_END(VRControllerModelMode, Disabled, Transparent)

RBX_ENUM_BEGIN(VRDeviceType)
	RBX_ENUM_VALUE(VRDeviceType, Unknown, 0)
	RBX_ENUM_VALUE(VRDeviceType, OculusRift, 1)
	RBX_ENUM_VALUE(VRDeviceType, HTCVive, 2)
	RBX_ENUM_VALUE(VRDeviceType, ValveIndex, 3)
	RBX_ENUM_VALUE(VRDeviceType, OculusQuest, 4)
RBX_ENUM_END(VRDeviceType, Unknown, OculusRift, HTCVive, ValveIndex, OculusQuest)

RBX_ENUM_BEGIN(VRLaserPointerMode)
	RBX_ENUM_VALUE(VRLaserPointerMode, Disabled, 0)
	RBX_ENUM_VALUE(VRLaserPointerMode, Pointer, 1)
	RBX_ENUM_VALUE(VRLaserPointerMode, DualPointer, 2)
RBX_ENUM_END(VRLaserPointerMode, Disabled, Pointer, DualPointer)

RBX_ENUM_BEGIN(VRSafetyBubbleMode)
	RBX_ENUM_VALUE(VRSafetyBubbleMode, NoOne, 0)
	RBX_ENUM_VALUE(VRSafetyBubbleMode, OnlyFriends, 1)
	RBX_ENUM_VALUE(VRSafetyBubbleMode, Anyone, 2)
RBX_ENUM_END(VRSafetyBubbleMode, NoOne, OnlyFriends, Anyone)

RBX_ENUM_BEGIN(VRScaling)
	RBX_ENUM_VALUE(VRScaling, World, 0)
	RBX_ENUM_VALUE(VRScaling, Off, 1)
RBX_ENUM_END(VRScaling, World, Off)

RBX_ENUM_BEGIN(VRSessionState)
	RBX_ENUM_VALUE(VRSessionState, Undefined, 0)
	RBX_ENUM_VALUE(VRSessionState, Idle, 1)
	RBX_ENUM_VALUE(VRSessionState, Visible, 2)
	RBX_ENUM_VALUE(VRSessionState, Focused, 3)
	RBX_ENUM_VALUE(VRSessionState, Stopping, 4)
RBX_ENUM_END(VRSessionState, Undefined, Idle, Visible, Focused, Stopping)

RBX_ENUM_BEGIN(VRTouchpad)
	RBX_ENUM_VALUE(VRTouchpad, Left, 0)
	RBX_ENUM_VALUE(VRTouchpad, Right, 1)
RBX_ENUM_END(VRTouchpad, Left, Right)

RBX_ENUM_BEGIN(VRTouchpadMode)
	RBX_ENUM_VALUE(VRTouchpadMode, Touch, 0)
	RBX_ENUM_VALUE(VRTouchpadMode, VirtualThumbstick, 1)
	RBX_ENUM_VALUE(VRTouchpadMode, ABXY, 2)
RBX_ENUM_END(VRTouchpadMode, Touch, VirtualThumbstick, ABXY)

RBX_ENUM_BEGIN(VelocityConstraintMode)
	RBX_ENUM_VALUE(VelocityConstraintMode, Line, 0)
	RBX_ENUM_VALUE(VelocityConstraintMode, Plane, 1)
	RBX_ENUM_VALUE(VelocityConstraintMode, Vector, 2)
RBX_ENUM_END(VelocityConstraintMode, Line, Plane, Vector)

RBX_ENUM_BEGIN(VerticalAlignment)
	RBX_ENUM_VALUE(VerticalAlignment, Center, 0)
	RBX_ENUM_VALUE(VerticalAlignment, Top, 1)
	RBX_ENUM_VALUE(VerticalAlignment, Bottom, 2)
RBX_ENUM_END(VerticalAlignment, Center, Top, Bottom)

RBX_ENUM_BEGIN(VerticalScrollBarPosition)
	RBX_ENUM_VALUE(VerticalScrollBarPosition, Right, 0)
	RBX_ENUM_VALUE(VerticalScrollBarPosition, Left, 1)
RBX_ENUM_END(VerticalScrollBarPosition, Right, Left)

RBX_ENUM_BEGIN(VibrationMotor)
	RBX_ENUM_VALUE(VibrationMotor, Large, 0)
	RBX_ENUM_VALUE(VibrationMotor, Small, 1)
	RBX_ENUM_VALUE(VibrationMotor, LeftTrigger, 2)
	RBX_ENUM_VALUE(VibrationMotor, RightTrigger, 3)
	RBX_ENUM_VALUE(VibrationMotor, LeftHand, 4)
	RBX_ENUM_VALUE(VibrationMotor, RightHand, 5)
RBX_ENUM_END(VibrationMotor, Large, Small, LeftTrigger, RightTrigger, LeftHand, RightHand)

RBX_ENUM_BEGIN(VideoCaptureResult)
	RBX_ENUM_VALUE(VideoCaptureResult, Success, 0)
	RBX_ENUM_VALUE(VideoCaptureResult, OtherError, 1)
	RBX_ENUM_VALUE(VideoCaptureResult, ScreenSizeChanged, 2)
	RBX_ENUM_VALUE(VideoCaptureResult, TimeLimitReached, 3)
RBX_ENUM_END(VideoCaptureResult, Success, OtherError, ScreenSizeChanged, TimeLimitReached)

RBX_ENUM_BEGIN(VideoCaptureStartedResult)
	RBX_ENUM_VALUE(VideoCaptureStartedResult, Success, 0)
	RBX_ENUM_VALUE(VideoCaptureStartedResult, OtherError, 1)
	RBX_ENUM_VALUE(VideoCaptureStartedResult, CapturingAlready, 2)
	RBX_ENUM_VALUE(VideoCaptureStartedResult, NoDeviceSupport, 3)
	RBX_ENUM_VALUE(VideoCaptureStartedResult, NoSpaceOnDevice, 4)
RBX_ENUM_END(VideoCaptureStartedResult, Success, OtherError, CapturingAlready, NoDeviceSupport, NoSpaceOnDevice)

RBX_ENUM_BEGIN(VideoDeviceCaptureQuality)
	RBX_ENUM_VALUE(VideoDeviceCaptureQuality, Default, 0)
	RBX_ENUM_VALUE(VideoDeviceCaptureQuality, Low, 1)
	RBX_ENUM_VALUE(VideoDeviceCaptureQuality, Medium, 2)
	RBX_ENUM_VALUE(VideoDeviceCaptureQuality, High, 3)
RBX_ENUM_END(VideoDeviceCaptureQuality, Default, Low, Medium, High)

RBX_ENUM_BEGIN(VideoError)
	RBX_ENUM_VALUE(VideoError, Ok, 0)
	RBX_ENUM_VALUE(VideoError, Eof, 1)
	RBX_ENUM_VALUE(VideoError, EAgain, 2)
	RBX_ENUM_VALUE(VideoError, BadParameter, 3)
	RBX_ENUM_VALUE(VideoError, AllocFailed, 4)
	RBX_ENUM_VALUE(VideoError, CodecInitFailed, 5)
	RBX_ENUM_VALUE(VideoError, CodecCloseFailed, 6)
	RBX_ENUM_VALUE(VideoError, DecodeFailed, 7)
	RBX_ENUM_VALUE(VideoError, ParsingFailed, 8)
	RBX_ENUM_VALUE(VideoError, Unsupported, 9)
	RBX_ENUM_VALUE(VideoError, Generic, 10)
	RBX_ENUM_VALUE(VideoError, DownloadFailed, 11)
	RBX_ENUM_VALUE(VideoError, StreamNotFound, 12)
	RBX_ENUM_VALUE(VideoError, EncodeFailed, 13)
	RBX_ENUM_VALUE(VideoError, CreateFailed, 14)
	RBX_ENUM_VALUE(VideoError, NoPermission, 15)
	RBX_ENUM_VALUE(VideoError, NoService, 16)
	RBX_ENUM_VALUE(VideoError, ReleaseFailed, 17)
	RBX_ENUM_VALUE(VideoError, Unknown, 18)
RBX_ENUM_END(VideoError, Ok, Eof, EAgain, BadParameter, AllocFailed, CodecInitFailed, CodecCloseFailed, DecodeFailed, ParsingFailed, Unsupported, Generic, DownloadFailed, StreamNotFound, EncodeFailed, CreateFailed, NoPermission, NoService, ReleaseFailed, Unknown)

RBX_ENUM_BEGIN(VideoSampleSize)
	RBX_ENUM_VALUE(VideoSampleSize, Small, 0)
	RBX_ENUM_VALUE(VideoSampleSize, Medium, 1)
	RBX_ENUM_VALUE(VideoSampleSize, Large, 2)
	RBX_ENUM_VALUE(VideoSampleSize, Full, 3)
RBX_ENUM_END(VideoSampleSize, Small, Medium, Large, Full)

RBX_ENUM_BEGIN(ViewMode)
	RBX_ENUM_VALUE(ViewMode, None, 0)
	RBX_ENUM_VALUE(ViewMode, GeometryComplexity, 1)
	RBX_ENUM_VALUE(ViewMode, Transparent, 2)
	RBX_ENUM_VALUE(ViewMode, Decal, 3)
RBX_ENUM_END(ViewMode, None, GeometryComplexity, Transparent, Decal)

RBX_ENUM_BEGIN(VirtualCursorMode)
	RBX_ENUM_VALUE(VirtualCursorMode, Default, 0)
	RBX_ENUM_VALUE(VirtualCursorMode, Disabled, 1)
	RBX_ENUM_VALUE(VirtualCursorMode, Enabled, 2)
RBX_ENUM_END(VirtualCursorMode, Default, Disabled, Enabled)

RBX_ENUM_BEGIN(VirtualInputMode)
	RBX_ENUM_VALUE(VirtualInputMode, None, 0)
	RBX_ENUM_VALUE(VirtualInputMode, Recording, 1)
	RBX_ENUM_VALUE(VirtualInputMode, Playing, 2)
RBX_ENUM_END(VirtualInputMode, None, Recording, Playing)

RBX_ENUM_BEGIN(VoiceChatDistanceAttenuationType)
	RBX_ENUM_VALUE(VoiceChatDistanceAttenuationType, Inverse, 0)
	RBX_ENUM_VALUE(VoiceChatDistanceAttenuationType, Legacy, 1)
RBX_ENUM_END(VoiceChatDistanceAttenuationType, Inverse, Legacy)

RBX_ENUM_BEGIN(VoiceChatState)
	RBX_ENUM_VALUE(VoiceChatState, Idle, 0)
	RBX_ENUM_VALUE(VoiceChatState, Joining, 1)
	RBX_ENUM_VALUE(VoiceChatState, JoiningRetry, 2)
	RBX_ENUM_VALUE(VoiceChatState, Joined, 3)
	RBX_ENUM_VALUE(VoiceChatState, Leaving, 4)
	RBX_ENUM_VALUE(VoiceChatState, Ended, 5)
	RBX_ENUM_VALUE(VoiceChatState, Failed, 6)
RBX_ENUM_END(VoiceChatState, Idle, Joining, JoiningRetry, Joined, Leaving, Ended, Failed)

RBX_ENUM_BEGIN(VoiceClientLeaveReasons)
	RBX_ENUM_VALUE(VoiceClientLeaveReasons, Unknown, 0)
	RBX_ENUM_VALUE(VoiceClientLeaveReasons, ClientNetworkDisconnected, 1)
	RBX_ENUM_VALUE(VoiceClientLeaveReasons, PlayerLeft, 2)
	RBX_ENUM_VALUE(VoiceClientLeaveReasons, ClientShutdown, 3)
	RBX_ENUM_VALUE(VoiceClientLeaveReasons, PublishFailed, 4)
	RBX_ENUM_VALUE(VoiceClientLeaveReasons, RejoinReceived, 5)
	RBX_ENUM_VALUE(VoiceClientLeaveReasons, VoiceReboot, 6)
	RBX_ENUM_VALUE(VoiceClientLeaveReasons, ImguiDebugLeave, 7)
	RBX_ENUM_VALUE(VoiceClientLeaveReasons, LuaInitiated, 8)
RBX_ENUM_END(VoiceClientLeaveReasons, Unknown, ClientNetworkDisconnected, PlayerLeft, ClientShutdown, PublishFailed, RejoinReceived, VoiceReboot, ImguiDebugLeave, LuaInitiated)

RBX_ENUM_BEGIN(VoiceControlPath)
	RBX_ENUM_VALUE(VoiceControlPath, Publish, 0)
	RBX_ENUM_VALUE(VoiceControlPath, Subscribe, 1)
	RBX_ENUM_VALUE(VoiceControlPath, Join, 2)
RBX_ENUM_END(VoiceControlPath, Publish, Subscribe, Join)

RBX_ENUM_BEGIN(VoiceRccReconnectReason)
	RBX_ENUM_VALUE(VoiceRccReconnectReason, Unknown, 0)
	RBX_ENUM_VALUE(VoiceRccReconnectReason, Migration, 1)
	RBX_ENUM_VALUE(VoiceRccReconnectReason, CloseRoom, 2)
	RBX_ENUM_VALUE(VoiceRccReconnectReason, FAEUpdate, 3)
RBX_ENUM_END(VoiceRccReconnectReason, Unknown, Migration, CloseRoom, FAEUpdate)

RBX_ENUM_BEGIN(VolumetricAudio)
	RBX_ENUM_VALUE(VolumetricAudio, Disabled, 0)
	RBX_ENUM_VALUE(VolumetricAudio, Automatic, 1)
	RBX_ENUM_VALUE(VolumetricAudio, Enabled, 2)
RBX_ENUM_END(VolumetricAudio, Disabled, Automatic, Enabled)

RBX_ENUM_BEGIN(WaterDirection)
	RBX_ENUM_VALUE(WaterDirection, NegX, 0)
	RBX_ENUM_VALUE(WaterDirection, X, 1)
	RBX_ENUM_VALUE(WaterDirection, NegY, 2)
	RBX_ENUM_VALUE(WaterDirection, Y, 3)
	RBX_ENUM_VALUE(WaterDirection, NegZ, 4)
	RBX_ENUM_VALUE(WaterDirection, Z, 5)
RBX_ENUM_END(WaterDirection, NegX, X, NegY, Y, NegZ, Z)

RBX_ENUM_BEGIN(WaterForce)
	RBX_ENUM_VALUE(WaterForce, None, 0)
	RBX_ENUM_VALUE(WaterForce, Small, 1)
	RBX_ENUM_VALUE(WaterForce, Medium, 2)
	RBX_ENUM_VALUE(WaterForce, Strong, 3)
	RBX_ENUM_VALUE(WaterForce, Max, 4)
RBX_ENUM_END(WaterForce, None, Small, Medium, Strong, Max)

RBX_ENUM_BEGIN(WebSocketState)
	RBX_ENUM_VALUE(WebSocketState, Connecting, 0)
	RBX_ENUM_VALUE(WebSocketState, Open, 1)
	RBX_ENUM_VALUE(WebSocketState, Closing, 2)
	RBX_ENUM_VALUE(WebSocketState, Closed, 3)
RBX_ENUM_END(WebSocketState, Connecting, Open, Closing, Closed)

RBX_ENUM_BEGIN(WebStreamClientState)
	RBX_ENUM_VALUE(WebStreamClientState, Connecting, 0)
	RBX_ENUM_VALUE(WebStreamClientState, Open, 1)
	RBX_ENUM_VALUE(WebStreamClientState, Error, 2)
	RBX_ENUM_VALUE(WebStreamClientState, Closed, 3)
RBX_ENUM_END(WebStreamClientState, Connecting, Open, Error, Closed)

RBX_ENUM_BEGIN(WebStreamClientType)
	RBX_ENUM_VALUE(WebStreamClientType, SSE, 0)
	RBX_ENUM_VALUE(WebStreamClientType, RawStream, 1)
	RBX_ENUM_VALUE(WebStreamClientType, WebSocket, 2)
RBX_ENUM_END(WebStreamClientType, SSE, RawStream, WebSocket)

RBX_ENUM_BEGIN(WeldConstraintPreserve)
	RBX_ENUM_VALUE(WeldConstraintPreserve, All, 0)
	RBX_ENUM_VALUE(WeldConstraintPreserve, None, 1)
	RBX_ENUM_VALUE(WeldConstraintPreserve, Touching, 2)
RBX_ENUM_END(WeldConstraintPreserve, All, None, Touching)

RBX_ENUM_BEGIN(WhenUserFirstPlayed)
	RBX_ENUM_VALUE(WhenUserFirstPlayed, Unknown, 0)
	RBX_ENUM_VALUE(WhenUserFirstPlayed, Days0To30, 1)
	RBX_ENUM_VALUE(WhenUserFirstPlayed, Days31To90, 2)
	RBX_ENUM_VALUE(WhenUserFirstPlayed, Days91To180, 3)
	RBX_ENUM_VALUE(WhenUserFirstPlayed, Days181To365, 4)
	RBX_ENUM_VALUE(WhenUserFirstPlayed, Days366Plus, 5)
RBX_ENUM_END(WhenUserFirstPlayed, Unknown, Days0To30, Days31To90, Days91To180, Days181To365, Days366Plus)

RBX_ENUM_BEGIN(WhisperChatPrivacyMode)
	RBX_ENUM_VALUE(WhisperChatPrivacyMode, AllUsers, 0)
	RBX_ENUM_VALUE(WhisperChatPrivacyMode, NoOne, 1)
RBX_ENUM_END(WhisperChatPrivacyMode, AllUsers, NoOne)

RBX_ENUM_BEGIN(WrapLayerAutoSkin)
	RBX_ENUM_VALUE(WrapLayerAutoSkin, Disabled, 0)
	RBX_ENUM_VALUE(WrapLayerAutoSkin, EnabledPreserve, 1)
	RBX_ENUM_VALUE(WrapLayerAutoSkin, EnabledOverride, 2)
RBX_ENUM_END(WrapLayerAutoSkin, Disabled, EnabledPreserve, EnabledOverride)

RBX_ENUM_BEGIN(WrapLayerDebugMode)
	RBX_ENUM_VALUE(WrapLayerDebugMode, None, 0)
	RBX_ENUM_VALUE(WrapLayerDebugMode, BoundCage, 1)
	RBX_ENUM_VALUE(WrapLayerDebugMode, LayerCage, 2)
	RBX_ENUM_VALUE(WrapLayerDebugMode, BoundCageAndLinks, 3)
	RBX_ENUM_VALUE(WrapLayerDebugMode, Reference, 4)
	RBX_ENUM_VALUE(WrapLayerDebugMode, Rbf, 5)
	RBX_ENUM_VALUE(WrapLayerDebugMode, OuterCage, 6)
	RBX_ENUM_VALUE(WrapLayerDebugMode, ReferenceMeshAfterMorph, 7)
	RBX_ENUM_VALUE(WrapLayerDebugMode, HSROuterDetail, 8)
	RBX_ENUM_VALUE(WrapLayerDebugMode, HSROuter, 9)
	RBX_ENUM_VALUE(WrapLayerDebugMode, HSRInner, 10)
	RBX_ENUM_VALUE(WrapLayerDebugMode, HSRInnerReverse, 11)
	RBX_ENUM_VALUE(WrapLayerDebugMode, LayerCageFittedToBase, 12)
	RBX_ENUM_VALUE(WrapLayerDebugMode, LayerCageFittedToPrev, 13)
	RBX_ENUM_VALUE(WrapLayerDebugMode, PreWrapDeformerOuterCage, 14)
	RBX_ENUM_VALUE(WrapLayerDebugMode, SkinningTransfer, 15)
RBX_ENUM_END(WrapLayerDebugMode, None, BoundCage, LayerCage, BoundCageAndLinks, Reference, Rbf, OuterCage, ReferenceMeshAfterMorph, HSROuterDetail, HSROuter, HSRInner, HSRInnerReverse, LayerCageFittedToBase, LayerCageFittedToPrev, PreWrapDeformerOuterCage, SkinningTransfer)

RBX_ENUM_BEGIN(WrapTargetDebugMode)
	RBX_ENUM_VALUE(WrapTargetDebugMode, None, 0)
	RBX_ENUM_VALUE(WrapTargetDebugMode, TargetCageOriginal, 1)
	RBX_ENUM_VALUE(WrapTargetDebugMode, TargetCageCompressed, 2)
	RBX_ENUM_VALUE(WrapTargetDebugMode, TargetCageInterface, 3)
	RBX_ENUM_VALUE(WrapTargetDebugMode, TargetLayerCageOriginal, 4)
	RBX_ENUM_VALUE(WrapTargetDebugMode, TargetLayerCageCompressed, 5)
	RBX_ENUM_VALUE(WrapTargetDebugMode, TargetLayerInterface, 6)
	RBX_ENUM_VALUE(WrapTargetDebugMode, Rbf, 7)
	RBX_ENUM_VALUE(WrapTargetDebugMode, OuterCageDetail, 8)
	RBX_ENUM_VALUE(WrapTargetDebugMode, PreWrapDeformerCage, 9)
RBX_ENUM_END(WrapTargetDebugMode, None, TargetCageOriginal, TargetCageCompressed, TargetCageInterface, TargetLayerCageOriginal, TargetLayerCageCompressed, TargetLayerInterface, Rbf, OuterCageDetail, PreWrapDeformerCage)

RBX_ENUM_BEGIN(ZIndexBehavior)
	RBX_ENUM_VALUE(ZIndexBehavior, Global, 0)
	RBX_ENUM_VALUE(ZIndexBehavior, Sibling, 1)
RBX_ENUM_END(ZIndexBehavior, Global, Sibling)

#undef RBX_ENUM_BEGIN
#undef RBX_ENUM_VALUE
#undef RBX_ENUM_END
	}

	class Instance;

	// ----------------------------------------------------------------------
	// Property/Event/Connection framework — declared before Instance so that
	// Instance itself can carry Property<T> members (Name, Parent, ...).
	// Method bodies reference Instance methods that are template-instantiated
	// at first use, so the only thing that must exist *here* is a forward
	// declaration of Instance (which we have).
	// ----------------------------------------------------------------------

	class Connection
	{
	public:
		Connection() = default;
		explicit Connection(std::function<void()> disconnect) : disconnect_(std::move(disconnect)) {}

		void Disconnect()
		{
			if (disconnect_) { disconnect_(); disconnect_ = nullptr; }
		}
		bool IsConnected() const { return static_cast<bool>(disconnect_); }

	private:
		std::function<void()> disconnect_;
	};

	class EventProxy
	{
	public:
		EventProxy(Instance* owner, const char* name) : owner_(owner), name_(name) {}

		// definitions live below Instance so they can call its methods.
		inline Connection Connect(std::function<void(std::vector<std::string>)> callback);
		Connection Connect(std::function<void()> callback)
		{
			return Connect([cb = std::move(callback)](std::vector<std::string>) { cb(); });
		}
		inline std::vector<std::string> Wait();

	private:
		Instance* owner_;
		const char* name_;
	};

	template <typename T>
	class Property
	{
	public:
		Property(Instance* owner, const char* name) : owner_(owner), name_(name) {}

		// read: `T x = part->Position;`
		inline operator T() const;
		inline T get() const;

		// write: `part->Position = ...;`
		inline Property& operator=(const T& value);
		// Forwarding overload for convertible-but-distinct types (e.g.
		// `name = "CppPart"` where T=std::string and U=const char*). Must
		// exclude U==T or this would be a better match than the
		// non-template version for rvalues and recurse forever.
		template <typename U,
		          typename = std::enable_if_t<
		              !std::is_same_v<std::decay_t<U>, Property>
		              && !std::is_same_v<std::decay_t<U>, T>>>
		Property& operator=(U&& value)
		{
			return operator=(T(std::forward<U>(value)));
		}

		Property(const Property&) = delete;
		Property(Property&&) = delete;

	private:
		Instance* owner_;
		const char* name_;
	};

	template <typename T>
	inline std::ostream& operator<<(std::ostream& os, const Property<T>& p)
	{
		return os << static_cast<T>(p.get());
	}

	class Instance
	{
	public:
		// Universal Instance properties (every Roblox object has these).
		Property<std::string> Name{this, "Name"};
		Property<Instance*>   Parent{this, "Parent"};
		Property<bool>        Archivable{this, "Archivable"};
		Property<std::string> ClassName{this, "ClassName"}; // read-only on the engine side

		// Universal events.
		EventProxy AncestryChanged{this, "AncestryChanged"};
		EventProxy ChildAdded{this, "ChildAdded"};
		EventProxy ChildRemoved{this, "ChildRemoved"};
		EventProxy DescendantAdded{this, "DescendantAdded"};
		EventProxy DescendantRemoving{this, "DescendantRemoving"};
		EventProxy Destroying{this, "Destroying"};

		Instance(std::string className, Instance *parent = nullptr, bool fromDebugId = false)
		{
			if (!fromDebugId)
			{
				owned_ = true;
				if (parent)
				{
					DebugID = lua_call((std::string("local inst = Instance.new(\"") + className + "\") local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) local cs = game:GetService(\"CollectionService\") cs:AddTag(inst, dID) inst.Parent = cs:GetTagged(\"" + parent->GetDebugId() + "\")[1] return dID").c_str());
				}
				else
				{
					DebugID = lua_call((std::string("local inst = Instance.new(\"") + className + "\") local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) game:GetService(\"CollectionService\"):AddTag(inst, dID) return dID").c_str());
				}
			}
			else
			{
				owned_ = false;
				DebugID = className;
			}
		}

		void Destroy()
		{
			lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then inst:Destroy() end").c_str());
		}

		Instance *Clone()
		{
			std::string id = lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then local cln = inst:Clone() local storage = game:GetService(\"ServerStorage\"):FindFirstChild(\"__CppStorage\") if not storage then storage = Instance.new(\"Folder\") storage.Name = \"__CppStorage\" storage.Parent = game:GetService(\"ServerStorage\") end cln.Parent = storage local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(cln, dID) return dID end").c_str());
			Instance* p = FromDebugId(id);
			if (p) p->owned_ = true; // we created the clone, so we own it
			return p;
		}

		std::string GetDebugId()
		{
			return DebugID;
		}

		static Instance *GetInstance(std::string path)
		{
			std::string debugId = lua_call((std::string("local inst = ") + path + " if inst and typeof(inst) == \"Instance\" then local cs = game:GetService(\"CollectionService\") local tags = cs:GetTags(inst) for i,v in ipairs(tags) do if #v > 6 and v:sub(1,6) == \"__dID_\" then return v end end local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(inst, dID) return dID end").c_str());
			if (debugId == "" || debugId == "nil")
			{
				return nullptr;
			}
			else
			{
				return FromDebugId(debugId);
			}
		}

		static Instance *FromDebugId(std::string debugId)
		{
			return new Instance(debugId, nullptr, true);
		}

		// void ConnectEvent(std::string name, std::function<void(std::vector<std::any>)> callback) {
		std::function<void()> ConnectEvent(std::string name, std::function<void(std::vector<std::string>)> callback)
		{

			// Make a new thread
			std::thread *t = new std::thread(([=, this]()
											  {
				std::string ret = lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then local event = inst." + name + " local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(event, dID) if event then local ret = {} local rawRet = {event:Wait()} for i,v in ipairs(rawRet) do if typeof(v) == \"Instance\" then local dID2 = nil for i,v in ipairs(cs:GetTags(v)) do if #v > 6 and v:sub(1,6) == \"__dID_\" then dID2 = v break end end if not dID2 then dID2 = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(v, dID2) end table.insert(ret, dID2) else table.insert(ret, tostring(v)) end end return table.concat(ret, \"\x1B\") end end").c_str());
				callback(RBX_Utils::string::split(ret, "\x1B")); }));

			t->detach();

			// Return a disconnect function that kills the thread
			return [=, this]()
			{
				delete t;
			};
			//return []() {};
		}

		// std::vector<std::string> WaitEvent(std::string name) {
		std::vector<std::string> WaitEvent(std::string name)
		{
			std::string ret = lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then local event = inst." + name + " local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(event, dID) if event then local ret = {} local rawRet = {event:Wait()} for i,v in ipairs(rawRet) do if typeof(v) == \"Instance\" then local dID2 = nil for i,v in ipairs(cs:GetTags(v)) do if #v > 6 and v:sub(1,6) == \"__dID_\" then dID2 = v break end end if not dID2 then dID2 = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(v, dID2) end table.insert(ret, dID2) else table.insert(ret, tostring(v)) end end return table.concat(ret, \"\x1B\") end end").c_str());
			return RBX_Utils::string::split(ret, "\x1B");
		}

		// Strongly-typed property setter. Picks the right Lua serialization at
		// compile time based on T. Supports primitives, std::string / C strings,
		// Instance*, and any RBX::Types datatype (Vector3, Color3, ...).
		template <typename T>
		void SetProperty(std::string name, const T& value)
		{
			SetPropertyRaw(std::move(name), valueToLua(value));
		}

		void SetPropertyRaw(std::string name, std::string value)
		{
			lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then inst." + name + " = " + value + " end").c_str());
		}

		// Templated getter. Use as `inst->GetProperty<RBX::Vector3>("Position")`.
		// For primitives and strings the dispatch is direct; for datatypes it
		// builds a custom Lua read snippet so we get exact components instead
		// of the lossy `tostring` representation.
		template <typename T>
		T GetProperty(std::string name)
		{
			using U = std::remove_cv_t<std::remove_reference_t<T>>;

			if constexpr (std::is_same_v<U, std::string>)
			{
				return GetPropertyRaw(name);
			}
			else if constexpr (std::is_same_v<U, bool>)
			{
				std::string s = GetPropertyRaw(name);
				return s == "true";
			}
			else if constexpr (std::is_same_v<U, Instance*>)
			{
				std::string path = std::string("game:GetService(\"CollectionService\"):GetTagged(\"") + DebugID + "\")[1]." + name;
				return GetInstance(path);
			}
			else if constexpr (Types::is_lua_datatype_v<U>)
			{
				std::string path = std::string("game:GetService(\"CollectionService\"):GetTagged(\"") + DebugID + "\")[1]." + name;
				std::string s = lua_call(U::luaReadExpr(path).c_str());
				return U::parseFrom(s);
			}
			else if constexpr (std::is_floating_point_v<U>)
			{
				return static_cast<U>(Types::detail::parseDouble(GetPropertyRaw(name)));
			}
			else if constexpr (std::is_integral_v<U>)
			{
				return static_cast<U>(Types::detail::parseLL(GetPropertyRaw(name)));
			}
			else
			{
				static_assert(sizeof(T) == 0, "GetProperty<T>: unsupported type T");
			}
		}

		std::string GetPropertyRaw(std::string name)
		{
			return lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then local ret = inst." + name + " if type(ret) ~= \"string\" then ret = tostring(ret) end return ret end").c_str());
		}

		std::vector<std::any> CallMethod(std::string name, std::vector<std::any> args = {})
		{
			std::string argsString = "";
			for (int i = 0; i < args.size(); i++)
			{
				std::string valueString = toString(args[i]);
				argsString += valueString;
				if (i != args.size() - 1)
				{
					argsString += ", ";
				}
			}
			std::string valueString = lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then local ret = inst:" + name + "(" + argsString + ") if typeof(ret) == \"table\" then local ret2 = {} for i,v in pairs(ret) do if typeof(v) == \"Instance\" then local dID2 = nil for i,v in ipairs(cs:GetTags(v)) do if #v > 6 and v:sub(1,6) == \"__dID_\" then dID2 = v break end end if not dID2 then dID2 = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(v, dID2) end table.insert(ret2, dID2) else table.insert(ret2, tostring(v)) end end return table.concat(ret2, \"\x1B\") end if type(ret) ~= \"string\" then ret = tostring(ret) end return ret end end").c_str());
			if (valueString == "nil")
			{
				return {nullptr};
			}
			else if (valueString == "true")
			{
				return {true};
			}
			else if (valueString == "false")
			{
				return {false};
			}
			else if (valueString.find('.') != std::string::npos)
			{
				return {Types::detail::parseDouble(valueString)};
			}
			else
			{
				if (!std::isdigit(valueString[0]))
				{
					if (valueString.find("\x1B") != std::string::npos)
					{
						std::vector<std::string> values;
						std::string currentString = "";
						for (int i = 0; i < valueString.length(); i++)
						{
							if (valueString[i] == '\x1B')
							{
								values.push_back(currentString);
								currentString = "";
							}
							else
							{
								currentString += valueString[i];
							}
						}
						values.push_back(currentString);
						std::vector<std::any> returnValues;
						for (int i = 0; i < values.size(); i++)
						{
							if (values[i] == "nil")
							{
								returnValues.push_back(nullptr);
							}
							else if (values[i] == "true")
							{
								returnValues.push_back(true);
							}
							else if (values[i] == "false")
							{
								returnValues.push_back(false);
							}
							else if (values[i].find('.') != std::string::npos)
							{
								returnValues.push_back(Types::detail::parseDouble(values[i]));
							}
							else
							{
								returnValues.push_back(Types::detail::parseInt(values[i]));
							}
						}
						return returnValues;
					}
					return {valueString};
				}
				return {Types::detail::parseInt(valueString)};
			}
		}

		~Instance()
		{
			// Only destroy the underlying Roblox instance if we created it.
			// References obtained via GetInstance / FromDebugId are wrappers
			// around pre-existing world instances, so dropping the wrapper
			// must not delete the world object.
			if (owned_)
			{
				Destroy();
			}
		}

		// Convert a C++ value to its Lua source representation.
		// Public so callers can build raw method-call expressions safely.
		template <typename T>
		static std::string valueToLua(const T& value)
		{
			using U = std::remove_cv_t<std::remove_reference_t<T>>;

			if constexpr (std::is_same_v<U, std::nullptr_t>)
			{
				return "nil";
			}
			else if constexpr (std::is_same_v<U, bool>)
			{
				return value ? "true" : "false";
			}
			else if constexpr (std::is_same_v<U, std::string>)
			{
				return "\"" + escapeForLua(value) + "\"";
			}
			else if constexpr (std::is_same_v<U, const char*> || std::is_same_v<U, char*>)
			{
				return "\"" + escapeForLua(value) + "\"";
			}
			else if constexpr (std::is_array_v<U> &&
				std::is_same_v<std::remove_cv_t<std::remove_extent_t<U>>, char>)
			{
				return "\"" + escapeForLua(value) + "\"";
			}
			else if constexpr (std::is_same_v<U, Instance*>)
			{
				return value
					? "game:GetService(\"CollectionService\"):GetTagged(\"" + value->GetDebugId() + "\")[1]"
					: std::string("nil");
			}
			else if constexpr (Types::is_lua_datatype_v<U>)
			{
				return value.toLuaString();
			}
			else if constexpr (std::is_arithmetic_v<U>)
			{
				return std::to_string(value);
			}
			else
			{
				static_assert(sizeof(T) == 0, "valueToLua: unsupported type T");
				return "nil";
			}
		}

	private:
		std::string DebugID;
		bool owned_ = false;

		static std::string escapeForLua(const std::string& s)
		{
			std::string out;
			out.reserve(s.size());
			for (char c : s)
			{
				switch (c)
				{
					case '\\': out += "\\\\"; break;
					case '"':  out += "\\\""; break;
					case '\n': out += "\\n"; break;
					case '\r': out += "\\r"; break;
					case '\t': out += "\\t"; break;
					default:   out += c;
				}
			}
			return out;
		}

		std::string toString(std::any value)
		{
			using ConvertFunction = std::function<std::string(const std::any &)>;

			static const std::unordered_map<std::type_index, ConvertFunction> converters = {
				{typeid(std::nullptr_t), [](const std::any &val)
				 { return "nil"; }},

				{typeid(const char *), [](const std::any &val)
				 { return "\"" + std::string(std::any_cast<const char *>(val)) + "\""; }},

				{typeid(char *), [](const std::any &val)
				 { return "\"" + std::string(std::any_cast<char *>(val)) + "\""; }},

				{typeid(std::string), [](const std::any &val)
				 { return "\"" + std::any_cast<std::string>(val) + "\""; }},

				{typeid(char), [](const std::any &val)
				 { return std::to_string(std::any_cast<char>(val)); }},
				{typeid(unsigned char), [](const std::any &val)
				 { return std::to_string(std::any_cast<unsigned char>(val)); }},

				{typeid(short), [](const std::any &val)
				 { return std::to_string(std::any_cast<short>(val)); }},
				{typeid(unsigned short), [](const std::any &val)
				 { return std::to_string(std::any_cast<unsigned short>(val)); }},

				{typeid(int), [](const std::any &val)
				 { return std::to_string(std::any_cast<int>(val)); }},
				{typeid(unsigned int), [](const std::any &val)
				 { return std::to_string(std::any_cast<unsigned int>(val)); }},

				{typeid(long), [](const std::any &val)
				 { return std::to_string(std::any_cast<long>(val)); }},
				{typeid(unsigned long), [](const std::any &val)
				 { return std::to_string(std::any_cast<unsigned long>(val)); }},

				{typeid(long long), [](const std::any &val)
				 { return std::to_string(std::any_cast<long long>(val)); }},
				{typeid(unsigned long long), [](const std::any &val)
				 { return std::to_string(std::any_cast<unsigned long long>(val)); }},

				{typeid(float), [](const std::any &val)
				 { return std::to_string(std::any_cast<float>(val)); }},
				{typeid(double), [](const std::any &val)
				 { return std::to_string(std::any_cast<double>(val)); }},

				{typeid(long double), [](const std::any &val)
				 { return std::to_string(std::any_cast<long double>(val)); }},

				{typeid(bool), [](const std::any &val)
				 { return std::any_cast<bool>(val) ? "true" : "false"; }},

				{typeid(Instance *), [](const std::any &val)
				 { return "game:GetService(\"CollectionService\"):GetTagged(\"" + std::any_cast<Instance *>(val)->GetDebugId() + "\")[1]"; }},
			};

			const auto &type = value.type();
			const auto &it = converters.find(type);

			if (it != converters.end())
			{
				return it->second(value);
			}
			else
			{
				error("Invalid type: " + std::string(type.name()));
				return "";
			}
		}
	};

	Instance* game = Instance::GetInstance("game");

	// Convenience accessor for any Roblox service. Caller does not own the
	// returned pointer (it wraps a long-lived service instance), so dropping
	// it does not Destroy the service.
	inline Instance* GetService(const std::string& name)
	{
		return Instance::GetInstance("game:GetService(\"" + name + "\")");
	}

	inline std::ostream& operator<<(std::ostream& os, Instance* inst)
	{
		if (!inst) return os << "Instance(nil)";
		return os << "Instance(" << inst->GetDebugId() << ")";
	}

	// ----------------------------------------------------------------------
	// Out-of-line definitions for the proxies declared before Instance.
	// ----------------------------------------------------------------------

	inline Connection EventProxy::Connect(std::function<void(std::vector<std::string>)> callback)
	{
		auto disc = owner_->ConnectEvent(name_, std::move(callback));
		return Connection(disc);
	}
	inline std::vector<std::string> EventProxy::Wait() { return owner_->WaitEvent(name_); }

	template <typename T>
	inline Property<T>::operator T() const { return owner_->template GetProperty<T>(name_); }
	template <typename T>
	inline T Property<T>::get() const { return owner_->template GetProperty<T>(name_); }
	template <typename T>
	inline Property<T>& Property<T>::operator=(const T& value)
	{
		owner_->template SetProperty<T>(name_, value);
		return *this;
	}

	// Macros used by hand-written classes and the generator. They produce a
	// class that:
	//   - inherits from PARENT
	//   - exposes RBXClassName as a static constant
	//   - provides default and parented constructors that pass the right
	//     class name down to Instance
	//   - provides a wrap-existing constructor for FromDebugId
	//   - exposes a protected delegating constructor so further subclasses
	//     can specify their own class name
#define RBX_CLASS_BEGIN(NAME, PARENT) \
	class NAME : public PARENT { \
	public: \
		static constexpr const char* RBXClassName = #NAME; \
		NAME(Instance* parent = nullptr) : PARENT(#NAME, parent) {} \
		NAME(const std::string& debugId, std::nullptr_t, bool fromDebugId) \
			: PARENT(debugId, nullptr, fromDebugId) {} \
		static NAME* FromDebugId(const std::string& id) { return new NAME(id, nullptr, true); } \
	protected: \
		NAME(const std::string& subClass, Instance* parent = nullptr) : PARENT(subClass, parent) {} \
	public:

#define RBX_PROP(TYPE, NAME)  Property<TYPE> NAME{this, #NAME};
#define RBX_EVENT(NAME)       EventProxy   NAME{this, #NAME};
#define RBX_CLASS_END         };
}

// Once the framework is in place, optionally pull in the hand-written class
// hierarchy. Wrap in a feature flag so the file stays usable on its own.
#ifndef RBX_NO_CLASSES
#  if __has_include("RBX_Classes.hpp")
#    include "RBX_Classes.hpp"
#  endif
#endif
