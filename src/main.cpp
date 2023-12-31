﻿// main.cpp : Defines the entry point for the application.
//

#include "RBX.hpp"

extern "C" {
	int main() {
		RBX::Instance* instance = new RBX::Instance("Part", RBX::Instance::GetInstance("game.Workspace"));
		instance->SetProperty("Name", "CppPart");
		instance->SetProperty("Parent", RBX::Instance::GetInstance("game.Workspace"));
		// instance->SetPropertyRaw("Name", "\"CppPart\"");

		RBX::Instance* cloned = instance->Clone();
		cloned->SetPropertyRaw("Parent", "game.Workspace");

		RBX::print(std::string("PARTS CREATED? Name: ") + instance->GetPropertyRaw("Name"));
	}
}