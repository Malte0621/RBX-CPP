// main.cpp : Defines the entry point for the application.
//

#include "RBX.hpp"

extern "C" {
	int main() {
		RBX::Instance* instance = new RBX::Instance("Part", RBX::Workspace);
		instance->SetProperty("Name", "CppPart");
		instance->SetProperty("Parent", RBX::Workspace);
		// instance->SetPropertyRaw("Name", "\"CppPart\"");

		RBX::Instance* cloned = instance->Clone();
		cloned->SetProperty("Parent", RBX::Workspace);
		//cloned->SetPropertyRaw("Parent", "game.Workspace");

		RBX::print(std::string("PARTS CREATED? Name: ") + instance->GetPropertyRaw("Name"));
	}
}