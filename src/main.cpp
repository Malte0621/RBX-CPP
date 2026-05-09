// main.cpp : Defines the entry point for the application.
//

#include "RBX.hpp"
#include <cstdlib>
#include <iostream>
#include <string>

int main(int argc, char** argv) {
	// argv comes from module.run(...) on the Lua side.
	std::cout << "argc=" << argc;
	for (int i = 0; i < argc; ++i)
		std::cout << " argv[" << i << "]=" << argv[i];
	std::cout << std::endl;

	// Env vars come from module.setEnv({...}).
	if (const char* user = std::getenv("USER"))
		std::cout << "USER env: " << user << std::endl;

	// Stdin comes from module.setStdin(text) / module.appendStdin(text).
	std::string line;
	if (std::getline(std::cin, line))
		std::cout << "first stdin line: " << line << std::endl;

	auto* workspace = RBX::GetService<RBX::Workspace>();

	auto* part = RBX::New<RBX::Part>(workspace);
	part->Name     = "CppPart";
	part->Anchored = true;
	part->Position = RBX::Vector3(0, 10, 0);
	part->Size     = RBX::Vector3(4, 1, 4);
	part->Color    = RBX::Color3::fromRGB(200, 50, 50);

	auto* cloned = RBX::Cast<RBX::Part>(part->Clone());
	cloned->Name     = "CppPart_Clone";
	cloned->Position = RBX::Vector3(part->Position) + RBX::Vector3(0, 5, 0);
	cloned->Parent   = workspace;

	std::cout << "spawned " << part << " at " << RBX::Vector3(part->Position) << std::endl;
	std::cerr << "clone landed at " << RBX::Vector3(cloned->Position) << std::endl;

	return 0;
}