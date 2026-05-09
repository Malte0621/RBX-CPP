// Stub definition for lua_call. Lives in its own translation unit so emcc
// can't inline the body into callers — the wasm side has to emit a real
// `call $lua_call` instruction, which wasynth then turns into
// `FUNC_LIST[lua_call_offset](...)` that our Lua glue overrides.

extern "C" __attribute__((noinline, used))
const char* lua_call(const char* code)
{
	return code ? code : "";
}
