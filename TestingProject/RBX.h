// RBX.h : The roblox API, header-only include file.

#pragma once

// TODO: Reference additional headers your program requires here.
#include <math.h>
#include <string>
#include <any>
#include <vector>

extern "C" {
	const char* lua_call(const char* code) {
		return ""; // Gets replaced from the lua side.
	}
}

namespace RBX {
	void print(std::string text) {
		lua_call((std::string("print(\"") + text + "\")").c_str());
	}
	
	void warn(std::string text) {
		lua_call((std::string("warn(\"") + text + "\")").c_str());
	}

	void error(std::string text) {
		lua_call((std::string("error(\"") + text + "\")").c_str());
	}

	void wait(double seconds) {
		lua_call((std::string("wait(") + std::to_string(seconds) + ")").c_str());
	}

	double tick() {
		return std::stod(lua_call("return tick()"));
	}

	class Instance;

	class Instance {
	public:
		Instance(std::string className, Instance* parent = nullptr, bool fromDebugId = false) {
			if (!fromDebugId) {
				if (parent) {
					DebugID = lua_call((std::string("local inst = Instance.new(\"") + className + "\") local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) local cs = game:GetService(\"CollectionService\") cs:AddTag(inst, dID) inst.Parent = cs:GetTagged(\"" + parent->GetDebugId() + "\")[1] return dID").c_str());
				}
				else {
					DebugID = lua_call((std::string("local inst = Instance.new(\"") + className + "\") local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) game:GetService(\"CollectionService\"):AddTag(inst, dID) return dID").c_str());
				}
			}
			else {
				DebugID = className;
			}
		}

		void Destroy() {
			lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then inst:Destroy() end").c_str());
		}

		Instance* Clone() {
			return FromDebugId(lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then local cln = inst:Clone() local storage = game:GetService(\"ServerStorage\"):FindFirstChild(\"__CppStorage\") if not storage then storage = Instance.new(\"Folder\") storage.Name = \"__CppStorage\" storage.Parent = game:GetService(\"ServerStorage\") end cln.Parent = storage local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(cln, dID) return dID end").c_str()));
		}
		
		std::string GetDebugId() {
			return DebugID;
		}

		static Instance* GetInstance(std::string path) {
			std::string debugId = lua_call((std::string("local inst = ") + path + " if inst and typeof(inst) == \"Instance\" then local cs = game:GetService(\"CollectionService\") local tags = cs:GetTags(inst) for i,v in ipairs(tags) do if #v > 6 and v:sub(1,6) == \"__dID_\" then return v end end local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(inst, dID) return dID end").c_str());
			if (debugId == "" || debugId == "nil") {
				return nullptr;
			}
			else {
				return FromDebugId(debugId);
			}
		}

		void SetProperty(std::string name, std::any value) {
			std::string valueString = "";
			if (value.type() == typeid(std::string)) {
				valueString = "\"" + std::any_cast<std::string>(value) + "\"";
			}
			else if (value.type() == typeid(int)) {
				valueString = std::to_string(std::any_cast<int>(value));
			}
			else if (value.type() == typeid(float)) {
				valueString = std::to_string(std::any_cast<float>(value));
			}
			else if (value.type() == typeid(double)) {
				valueString = std::to_string(std::any_cast<double>(value));
			}
			else if (value.type() == typeid(bool)) {
				valueString = std::any_cast<bool>(value) ? "true" : "false";
			} else if (value.type() == typeid(Instance*)) {
				valueString = "game:GetService(\"CollectionService\"):GetTagged(\"" + std::any_cast<Instance*>(value)->GetDebugId() + "\")[1]";
			}
			else {
				error("Invalid type for SetProperty");
			}
			
			lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then inst." + name + " = " + valueString + " end").c_str());
		}

		void SetPropertyRaw(std::string name, std::string value) {
			lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then inst." + name + " = " + value + " end").c_str());
		}

		std::any GetProperty(std::string name) {
			std::string valueString = lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then local ret = inst." + name + " if type(ret) == \"string\" then ret = '\"' .. ret .. '\"' else ret = tostring(ret) end return ret end").c_str());
			if (valueString == "nil") {
				return nullptr;
			}
			else if (valueString == "true") {
				return true;
			}
			else if (valueString == "false") {
				return false;
			}
			else if (valueString[0] == '"') {
				return valueString.substr(1, valueString.length() - 2);
			}
			else if (valueString.find('.') != std::string::npos) {
				return std::stod(valueString);
			}
			else {
				return std::stoi(valueString);
			}
		}

		std::string GetPropertyRaw(std::string name) {
			return lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then local ret = inst." + name + " if type(ret) ~= \"string\" then ret = tostring(ret) end return ret end").c_str());	
		}

		std::any CallMethod(std::string name, std::vector<std::any> args) {
			std::string argsString = "";
			for (int i = 0; i < args.size(); i++) {
				std::string valueString = "";
				if (args[i].type() == typeid(std::string)) {
					valueString = "\"" + std::any_cast<std::string>(args[i]) + "\"";
				}
				else if (args[i].type() == typeid(int)) {
					valueString = std::to_string(std::any_cast<int>(args[i]));
				}
				else if (args[i].type() == typeid(float)) {
					valueString = std::to_string(std::any_cast<float>(args[i]));
				}
				else if (args[i].type() == typeid(double)) {
					valueString = std::to_string(std::any_cast<double>(args[i]));
				}
				else if (args[i].type() == typeid(bool)) {
					valueString = std::any_cast<bool>(args[i]) ? "true" : "false";
				}
				else if (args[i].type() == typeid(Instance*)) {
					valueString = "game:GetService(\"CollectionService\"):GetTagged(\"" + std::any_cast<Instance*>(args[i])->GetDebugId() + "\")[1]";
				}
				else {
					error("Invalid type for CallMethod");
				}
				argsString += valueString;
				if (i != args.size() - 1) {
					argsString += ", ";
				}
			}
			std::string valueString = lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then local ret = inst:" + name + "(" + argsString + ") if type(ret) == \"string\" then ret = '\"' .. ret .. '\"' else ret = tostring(ret) end return ret end end").c_str());
			if (valueString == "nil") {
				return nullptr;
			}
			else if (valueString == "true") {
				return true;
			}
			else if (valueString == "false") {
				return false;
			}
			else if (valueString[0] == '"') {
				return valueString.substr(1, valueString.length() - 2);
			}
			else if (valueString.find('.') != std::string::npos) {
				return std::stod(valueString);
			}
			else {
				return std::stoi(valueString);
			}
		}
		
		~Instance() {
			Destroy();
		}
	private:
		std::string DebugID;

		static Instance* FromDebugId(std::string debugId) {
			return new Instance(debugId, nullptr, true);
		}
	};
}