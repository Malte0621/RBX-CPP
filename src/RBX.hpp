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

extern "C"
{
	const char *lua_call(const char *code)
	{
		return ""; // Gets replaced from the lua side.
	}
}

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

	void wait(double seconds = 0)
	{
		lua_call((std::string("wait(") + std::to_string(seconds) + ")").c_str());
	}

	double tick()
	{
		return std::stod(lua_call("return tick()"));
	}

	class Instance;

	class Instance
	{
	public:
		Instance(std::string className, Instance *parent = nullptr, bool fromDebugId = false)
		{
			if (!fromDebugId)
			{
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
				DebugID = className;
			}
		}

		void Destroy()
		{
			lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then inst:Destroy() end").c_str());
		}

		Instance *Clone()
		{
			return FromDebugId(lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then local cln = inst:Clone() local storage = game:GetService(\"ServerStorage\"):FindFirstChild(\"__CppStorage\") if not storage then storage = Instance.new(\"Folder\") storage.Name = \"__CppStorage\" storage.Parent = game:GetService(\"ServerStorage\") end cln.Parent = storage local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(cln, dID) return dID end").c_str()));
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
			std::thread *t = new std::thread(([=]()
											  {
				std::string ret = lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then local event = inst." + name + " local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(event, dID) if event then local ret = {} local rawRet = {event:Wait()} for i,v in ipairs(rawRet) do if typeof(v) == \"Instance\" then local dID2 = nil for i,v in ipairs(cs:GetTags(v)) if #v > 6 and v:sub(1,6) == \"__dID_\" then dID2 = v break end end if not dID2 then dID2 = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(v, dID2) end table.insert(ret, dID2) else table.insert(ret, tostring(v)) end end return table.concat(ret, \"\x1B\") end end").c_str());
				callback(RBX_Utils::string::split(ret, "\x1B")); }));

			t->detach();

			// Return a disconnect function that kills the thread
			return [=]()
			{
				delete t;
			};
		}

		// std::vector<std::string> WaitEvent(std::string name) {
		std::vector<std::string> WaitEvent(std::string name)
		{
			std::string ret = lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then local event = inst." + name + " local dID = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(event, dID) if event then local ret = {} local rawRet = {event:Wait()} for i,v in ipairs(rawRet) do if typeof(v) == \"Instance\" then local dID2 = nil for i,v in ipairs(cs:GetTags(v)) if #v > 6 and v:sub(1,6) == \"__dID_\" then dID2 = v break end end if not dID2 then dID2 = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(v, dID2) end table.insert(ret, dID2) else table.insert(ret, tostring(v)) end end return table.concat(ret, \"\x1B\") end end").c_str());
			return RBX_Utils::string::split(ret, "\x1B");
		}

		void SetProperty(std::string name, std::any value)
		{
			std::string valueString = toString(value);
			lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then inst." + name + " = " + valueString + " end").c_str());
		}

		void SetPropertyRaw(std::string name, std::string value)
		{
			lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then inst." + name + " = " + value + " end").c_str());
		}

		std::any GetProperty(std::string name)
		{
			std::string valueString = lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then local ret = inst." + name + " if type(ret) == \"string\" then ret = '\"' .. ret .. '\"' else ret = tostring(ret) end return ret end").c_str());
			if (valueString == "nil")
			{
				return nullptr;
			}
			else if (valueString == "true")
			{
				return true;
			}
			else if (valueString == "false")
			{
				return false;
			}
			else if (valueString[0] == '"')
			{
				return valueString.substr(1, valueString.length() - 2);
			}
			else if (valueString.find('.') != std::string::npos)
			{
				return std::stod(valueString);
			}
			else
			{
				return std::stoi(valueString);
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
			std::string valueString = lua_call((std::string("local cs = game:GetService(\"CollectionService\") local inst = cs:GetTagged(\"") + DebugID + "\")[1] if inst then local ret = inst:" + name + "(" + argsString + ") if typeof(ret) == \"table\" then local ret2 = {} for i,v in pairs(ret) do if typeof(v) == \"Instance\" then local dID2 = nil for i,v in ipairs(cs:GetTags(v)) if #v > 6 and v:sub(1,6) == \"__dID_\" then dID2 = v break end end if not dID2 then dID2 = \"__dID_\" .. game:GetService(\"HttpService\"):GenerateGUID(false) cs:AddTag(v, dID2) end table.insert(ret2, dID2) else table.insert(ret2, tostring(v)) end end return table.concat(ret2, \"\x1B\") end if type(ret) ~= \"string\" then ret = tostring(ret) end return ret end end").c_str());
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
				return {std::stod(valueString)};
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
								returnValues.push_back(std::stod(values[i]));
							}
							else
							{
								returnValues.push_back(std::stoi(values[i]));
							}
						}
						return returnValues;
					}
					return {valueString};
				}
				return {std::stoi(valueString)};
			}
		}

		~Instance()
		{
			Destroy();
		}

	private:
		std::string DebugID;

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
}