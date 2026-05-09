# RBX-CPP
 RBX-CPP allows you to program C++ in roblox with the help of emscripten and wasm2luau (from Wasynth).
## How to use
### Prerequisites
- [Emscripten](https://emscripten.org/docs/getting_started/downloads.html)
- [wasm2luau](https://github.com/Rerumu/Wasynth)
- [python3](https://www.python.org/downloads/)

### Compiling
1. Clone the repository
2. Run `python3 build.py` in the root directory
3. Copy the contents of `dist/*.lua` to your roblox project

### Running
1. Make a new ModuleScript in roblox
2. Copy the contents of `dist/*.lua` to the ModuleScript
3. Require the ModuleScript in a Script and call its exported function(s)

### Exporting
To export a function, edit the CMakeLists.txt file inside `src` and add the function name to the `EXPORTED_FUNCTIONS` variable. Then, recompile the project. The builder will automatically add the functions to the returned table in `dist/main.lua`!

Remember to use `module.convertString("<string here>")` when passing strings to the exported functions! (Numbers should work as-is.)

## Example
```lua
local module = require(script:WaitForChild("ModuleScript"))
module.main()
```

```cpp
#include "RBX.hpp"

extern "C" {
    int main() {
        RBX::Instance* instance = new RBX::Instance("Part", RBX::Instance::GetInstance("game.Workspace"));
        instance->SetProperty("Name", "CppPart");
        instance->SetProperty("Parent", RBX::Instance::GetInstance("game.Workspace"));

        RBX::Instance* cloned = instance->Clone();
        cloned->SetPropertyRaw("Parent", "game.Workspace");

        // Idiomatic C++ streams — routed to Roblox print()/warn() via fd_write.
        std::cout << "PARTS CREATED? Name: " << instance->GetPropertyRaw("Name") << std::endl;
        std::cerr << "this lands in warn()" << std::endl;

        RBX::printf("clone debug id: %s\n", cloned->GetDebugId().c_str());
        return 0;
    }
}
```

### Standard streams
`std::cout` / `std::printf` route to Roblox `print`. `std::cerr` / `fprintf(stderr, ...)` route to `warn`. Output is line-buffered: full lines flush as they appear, and any pending tail is flushed automatically when `main` returns. Call `module.flushStreams()` from Lua to flush manually.

### stdin / argv / env
`int main(int argc, char** argv)` works. The Lua module no longer auto-runs `main` on `require`; call `module.run(...)` to invoke it. Strings passed to `run` become `argv`. Pre-seed `stdin` and the environment via `setStdin` / `appendStdin` / `setEnv`:

```lua
local m = require(script.ModuleScript)
m.setStdin("hello\nworld\n")     -- std::cin / fread / fgets pull from this
m.setEnv({ USER = "malte", DEBUG = "1" })  -- std::getenv("USER") sees "malte"
local exitCode = m.run("--verbose", "input.txt")
```

`appendStdin(text)` adds to the existing buffer (useful for streaming data in across multiple `run` calls). `fd_close`/`fd_seek` are no-ops; only line-oriented stdin reads are tested. To run again with different args, just call `run` again — the WASM module is persistent.

## Documentation
### RBX::Instance
#### Constructors
```cpp
Instance(std::string className, Instance* parent = nullptr);
```
The created Instance is *owned* — when the C++ object is `delete`d, the underlying Roblox instance is destroyed. Wrappers obtained via `GetInstance` / `GetService` are *unowned*; they are safe to drop without affecting the world.

#### Property access (typed)
```cpp
template <typename T>
void SetProperty(std::string name, const T& value);

template <typename T>
T GetProperty(std::string name);
```
`T` may be: any arithmetic type, `bool`, `std::string`, `const char*`, `Instance*`, or any `RBX::Types` datatype (`Vector3`, `Vector2`, `Color3`, `UDim`, `UDim2`, `CFrame`, `BrickColor`).

```cpp
part->SetProperty("Position", RBX::Vector3(0, 10, 0));
auto pos = part->GetProperty<RBX::Vector3>("Position");
```

#### Other methods
```cpp
Instance* Clone();              // returned wrapper is owned
void Destroy();                 // destroys in Roblox
std::string GetPropertyRaw(std::string name);
void SetPropertyRaw(std::string name, std::string luaExpr);
std::string GetDebugId();
static Instance* GetInstance(std::string path); // unowned
static Instance* FromDebugId(std::string id);   // unowned
```

### RBX::GetService
```cpp
Instance* GetService(const std::string& name);
```
Equivalent to `game:GetService(name)`. The returned wrapper is unowned.

### Datatypes (`RBX::Types`)
Each datatype is a plain C++ struct that knows how to (de)serialize itself for Lua. They are exposed under `RBX::` directly (e.g. `RBX::Vector3`).

| Type | Notes |
|------|-------|
| `Vector2`, `Vector2int16` | `+ - * /`, `==`, `Magnitude/Unit/Dot/Cross/Lerp/Min/Max/Abs/Floor/Ceil/FuzzyEq`, statics `zero/one/xAxis/yAxis` |
| `Vector3`, `Vector3int16` | as above plus `Sign/AngleBetween`, statics `zero/one/xAxis/yAxis/zAxis` |
| `Color3` | `fromRGB/fromHex/fromHSV`, `Lerp`, `ToHex` |
| `UDim`, `UDim2` | `+ - -unary`, `Lerp`; `UDim2::fromScale/fromOffset` |
| `Rect` | `(min, max)` or `(minx, miny, maxx, maxy)`, `Width/Height` |
| `CFrame` | `lookAt`, `fromOrientation`, `fromAxisAngle`, `Inverse`, `Position/RightVector/UpVector/LookVector`, `PointToWorldSpace/ToObjectSpace`, `*` (compose, transform Vector3), `+/- Vector3` |
| `BrickColor` | named statics `White/Gray/Black/Red/...` |
| `Region3`, `Region3int16` | `Size/Center/GetCFrame` |
| `Ray` | `ClosestPoint/Distance` |
| `NumberRange` | `(min, max)` |
| `NumberSequence`, `NumberSequenceKeypoint` | constant or two-stop ctors, or `vector<keypoint>` |
| `ColorSequence`, `ColorSequenceKeypoint` | as above |
| `PhysicalProperties` | `(density, friction, elasticity[, fw, ew])` |
| `TweenInfo` | `(time, EasingStyle, EasingDirection, repeatCount, reverses, delayTime)` |
| `PathWaypoint` | `(position, action)` |
| `FloatCurveKey` | `(time, value, "Linear"/"Constant"/"Cubic")` |
| `Font` | `(family, weight, "Normal"/"Italic")` |
| `Axes` | `(x, y, z)` bools |
| `Faces` | `top/bottom/left/right/front/back` bools |
| `DateTime` | `fromUnixTimestamp/fromUnixTimestampMillis` |

### Enums (`RBX::Enum`)
Mirrors Roblox's `Enum.<Type>.<Name>` system. Each EnumType is a struct with `inline static const EnumItem` members; an `EnumItem` holds `enumType`, `name`, and `value`, and serializes to `Enum.<Type>.<Name>` for Lua.

```cpp
inst->SetProperty("Style", RBX::Enum::EasingStyle::Bounce);
RBX::TweenInfo info(0.5, RBX::Enum::EasingStyle::Quad, RBX::Enum::EasingDirection::InOut);

// Round-trip through GetProperty:
auto state = inst->GetProperty<RBX::EnumItem>("PlaybackState");
if (state == RBX::Enum::PlaybackState::Playing) { /* ... */ }

// Each EnumType also exposes:
auto items = RBX::Enum::EasingStyle::GetEnumItems();
auto byName  = RBX::Enum::EasingStyle::FromName("Linear");
auto byValue = RBX::Enum::EasingStyle::FromValue(0);
```

Baked-in EnumTypes: `EasingStyle`, `EasingDirection`, `NormalId`, `Axis`, `FillDirection`, `HorizontalAlignment`, `VerticalAlignment`, `SortOrder`, `TextXAlignment`, `TextYAlignment`, `ScaleType`, `ZIndexBehavior`, `PlaybackState`, `TweenStatus`, `UserInputState`, `UserInputType`, `PartType`, `HumanoidStateType`, `PathWaypointAction`, `KeyInterpolationMode`, `FontStyle`, `FontWeight`, plus a subset of `KeyCode` and `Material`. For values not baked in, use `Enum::<Type>::FromName("Foo")` or build an `EnumItem{"Type", "Name", value}` directly.

#### Dumping every Roblox Enum
`tools/dump_enums.lua` walks `Enum:GetEnums()` in a live Roblox runtime and emits ready-to-paste `RBX_ENUM_BEGIN/VALUE/END` blocks. Run it from Studio's command bar; copy the output between the macro definitions and the `#undef`s in the `namespace Enum { ... }` block of `src/RBX.hpp`. Items whose names aren't valid C++ identifiers (or that collide with C++ keywords) are skipped and listed as comments at the bottom.

### Class hierarchy
The full Roblox `Instance` tree (Workspace, BasePart, Players, GuiObject, every service, every constraint, every UI control, ~880 classes) is mirrored in `src/RBX_Classes.hpp` and auto-included by `RBX.hpp`. Each class is a typed wrapper with member-style property access:

```cpp
auto* part = new RBX::Part(RBX::GetService<RBX::Workspace>());
part->Anchored = true;
part->Size = RBX::Vector3(4, 1, 4);
part->Color = RBX::Color3::fromRGB(200, 50, 50);

RBX::Vector3 here = part->Position;          // implicit Get<Vector3>
part->Position = here + RBX::Vector3(0, 5, 0);

auto conn = part->Touched.Connect([](auto args) {
    std::cout << "touched by " << args[0] << std::endl;
});
// conn.Disconnect();
```

Behind the scenes, `Property<T>` calls `Instance::SetProperty<T>` / `GetProperty<T>` and `EventProxy` wraps `ConnectEvent`/`WaitEvent`. `RBX::Cast<T>(inst)` reinterprets an `Instance*` as a typed subclass; `RBX::GetService<T>()` does the equivalent of `game:GetService(T::RBXClassName)` and returns a typed wrapper.

#### Regenerating the hierarchy
`src/RBX_Classes.hpp` is produced by `tools/generate_classes.py` from Roblox's API-Dump.json. To refresh it for a newer engine version:

```bash
python tools/generate_classes.py --download --out src/RBX_Classes.hpp
# or, with a local dump:
python tools/generate_classes.py path/to/API-Dump.json --out src/RBX_Classes.hpp
```

Functions/methods aren't auto-typed (that requires per-signature marshalling); use `Instance::CallMethod` for those.

All scalar/vector datatypes stream to `std::ostream`, so `std::cout << myVector3` works.

### RBX::print
```cpp
void print(std::string message);
```

### RBX::warn
```cpp
void warn(std::string message);
```

### RBX::error
```cpp
void error(std::string message);
```

### RBX::cout / RBX::cerr
```cpp
std::ostream& cout; // alias for std::cout — Roblox print()
std::ostream& cerr; // alias for std::cerr — Roblox warn()
```

### RBX::printf / RBX::eprintf / RBX::flush
```cpp
int printf(const char* fmt, ...);  // → print
int eprintf(const char* fmt, ...); // → warn
void flush();                      // flush any partial line
```