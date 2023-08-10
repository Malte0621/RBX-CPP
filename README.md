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

## Example
```lua
local module = require(script:WaitForChild("ModuleScript"))
module.main()
```

```cpp
#include "RBX.h"

extern "C" {
	int main() {
		RBX::Instance* instance = new RBX::Instance("Part", RBX::Instance::GetInstance("game.Workspace"));
		instance->SetPropertyRaw("Name", "\"CppPart\"");

		RBX::Instance* cloned = instance->Clone();
		cloned->SetPropertyRaw("Parent", "game.Workspace");

		RBX::print(std::string("PARTS CREATED? Name: ") + instance->GetPropertyRaw("Name"));
	}
}
```

## Documentation
### RBX::Instance
#### Constructors
```cpp
Instance(std::string className, Instance* parent = nullptr);
```
```cpp
Instance(std::string className, std::string parent = "game");
```
#### Methods
```cpp
Instance* Clone();
```
```cpp
void Destroy();
```
```cpp
std::string GetPropertyRaw(std::string propertyName);
```
```cpp
void SetPropertyRaw(std::string propertyName, std::string value);
```
```cpp
std::string GetProperty(std::string propertyName);
```
```cpp
void SetProperty(std::string propertyName, std::string value);
```

### RBX::Instance::GetInstance
```cpp
Instance* GetInstance(std::string path);
```

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