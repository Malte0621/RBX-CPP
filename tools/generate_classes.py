#!/usr/bin/env python3
"""
generate_classes.py — emit src/RBX_Classes_full.hpp from a Roblox API-Dump.json.

Usage:
    python tools/generate_classes.py path/to/API-Dump.json [--out src/RBX_Classes_full.hpp]
    python tools/generate_classes.py --download

The script understands the JSON dump shape published at
    https://setup.rbxcdn.com/version-<hash>-API-Dump.json
(the same one the Roblox-Client-Tracker repo mirrors at
    https://raw.githubusercontent.com/MaximumADHD/Roblox-Client-Tracker/roblox/API-Dump.json).

It produces a header that uses the RBX_CLASS_BEGIN / RBX_PROP / RBX_EVENT
macros from src/RBX.hpp. Replace src/RBX_Classes.hpp with the generated
file (or include both — the macros use #pragma once).
"""

from __future__ import annotations

import argparse
import json
import keyword
import os
import sys
import urllib.request
from collections import defaultdict
from pathlib import Path
from typing import Any

# ---- Configuration -------------------------------------------------------

API_DUMP_URL = (
    "https://raw.githubusercontent.com/MaximumADHD/"
    "Roblox-Client-Tracker/roblox/API-Dump.json"
)

# Map API-dump value-type names to C++ types we expose. Anything not listed
# falls through to a raw string so SetProperty/GetProperty can still
# round-trip via tostring (the user can always reach for SetPropertyRaw).
PRIMITIVE_TYPE_MAP: dict[str, str] = {
    "bool":     "bool",
    "int":      "int",
    "int64":    "long long",
    "float":    "double",
    "double":   "double",
    "string":   "std::string",
    "Content":  "std::string",
    "BinaryString": "std::string",
    "ProtectedString": "std::string",
    "OptionalCoordinateFrame": "::RBX::Types::CFrame",
}

DATATYPE_TYPE_MAP: dict[str, str] = {
    name: f"::RBX::Types::{name}" for name in [
        "Vector2", "Vector2int16", "Vector3", "Vector3int16",
        "Color3", "UDim", "UDim2", "Rect", "CFrame", "BrickColor",
        "Region3", "Region3int16", "Ray", "NumberRange",
        "NumberSequence", "ColorSequence",
        "PhysicalProperties", "PathWaypoint", "Font",
        "Axes", "Faces", "DateTime", "TweenInfo", "FloatCurveKey",
    ]
}

# Property/event names that would collide with members synthesized by
# RBX_CLASS_BEGIN, with C++ keywords, or with template machinery in scope.
RESERVED_MEMBER_NAMES: set[str] = {
    "RBXClassName", "FromDebugId", "GetDebugId", "Destroy", "Clone",
    "GetProperty", "SetProperty", "GetPropertyRaw", "SetPropertyRaw",
    "ConnectEvent", "WaitEvent", "CallMethod",
    # C++ keywords that frequently appear (e.g. an event named "delete").
    *keyword.kwlist,  # python keywords; harmless overlap with most C++ keywords
    # explicit C++ keyword set
    "alignas", "alignof", "and", "and_eq", "asm", "auto", "bitand", "bitor",
    "bool", "break", "case", "catch", "char", "class", "compl", "concept",
    "const", "consteval", "constexpr", "constinit", "continue", "decltype",
    "default", "delete", "do", "double", "dynamic_cast", "else", "enum",
    "explicit", "export", "extern", "false", "float", "for", "friend",
    "goto", "if", "inline", "int", "long", "mutable", "namespace", "new",
    "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq",
    "private", "protected", "public", "register", "reinterpret_cast",
    "requires", "return", "short", "signed", "sizeof", "static",
    "static_assert", "static_cast", "struct", "switch", "template", "this",
    "thread_local", "throw", "true", "try", "typedef", "typeid", "typename",
    "union", "unsigned", "using", "virtual", "void", "volatile", "wchar_t",
    "while", "xor", "xor_eq",
}

# Tags we treat as "skip this member entirely". Deprecated/Hidden are still
# emitted so users can talk to them; NotScriptable/Security signals we drop.
DROP_MEMBER_TAGS: set[str] = {"NotScriptable"}


# ---- Helpers -------------------------------------------------------------

def is_valid_ident(name: str) -> bool:
    if not name:
        return False
    if not (name[0].isalpha() or name[0] == "_"):
        return False
    return all(c.isalnum() or c == "_" for c in name)


def map_value_type(value_type: dict) -> str | None:
    """Return a C++ type for a Roblox ValueType, or None if we should skip."""
    cat = value_type.get("Category", "")
    name = value_type.get("Name", "")

    if cat == "Primitive":
        return PRIMITIVE_TYPE_MAP.get(name, "std::string")
    if cat == "DataType":
        return DATATYPE_TYPE_MAP.get(name, "std::string")
    if cat == "Enum":
        return "::RBX::Types::EnumItem"
    if cat == "Class":
        return "Instance*"
    if cat == "Group":
        # Tuple types (rare on properties); fall back to raw string.
        return "std::string"
    return "std::string"


def member_is_public(member: dict) -> bool:
    sec = member.get("Security")
    if isinstance(sec, dict):
        if sec.get("Read") not in (None, "None"):
            return False
        if sec.get("Write") not in (None, "None"):
            return False
    elif isinstance(sec, str) and sec != "None":
        return False
    return True


def should_skip_member(member: dict) -> bool:
    tags = set(member.get("Tags") or [])
    if tags & DROP_MEMBER_TAGS:
        return True
    if not member_is_public(member):
        return True
    return False


# ---- Generator -----------------------------------------------------------

class Generator:
    def __init__(self, dump: dict[str, Any]):
        self.dump = dump
        self.classes_by_name: dict[str, dict] = {
            c["Name"]: c for c in dump["Classes"]
        }
        self.children: dict[str, list[str]] = defaultdict(list)
        for c in dump["Classes"]:
            self.children[c.get("Superclass") or ""].append(c["Name"])
        for kids in self.children.values():
            kids.sort()

        # Track property/event names per class chain to avoid shadowing a
        # parent member: C++ would happily compile a same-named override but
        # the inherited proxy wins by name, breaking the typed API.
        self.inherited_names: dict[str, set[str]] = {}

    def topo_order(self) -> list[str]:
        """Yield class names parent-first."""
        order: list[str] = []
        seen: set[str] = set()

        def walk(parent: str):
            for name in self.children.get(parent, []):
                if name in seen:
                    continue
                seen.add(name)
                order.append(name)
                walk(name)

        # Roots = classes whose Superclass isn't another class in the dump.
        # The canonical root is "<<<ROOT>>>" (Instance has Superclass==null
        # in some dumps and "<<<ROOT>>>" in others).
        for c in self.dump["Classes"]:
            sup = c.get("Superclass")
            if sup not in self.classes_by_name and c["Name"] not in seen:
                seen.add(c["Name"])
                order.append(c["Name"])
                walk(c["Name"])
        return order

    def collect_inherited_names(self, class_name: str) -> set[str]:
        if class_name in self.inherited_names:
            return self.inherited_names[class_name]
        cls = self.classes_by_name.get(class_name)
        if not cls:
            self.inherited_names[class_name] = set()
            return self.inherited_names[class_name]
        sup = cls.get("Superclass")
        names = set(self.collect_inherited_names(sup)) if sup in self.classes_by_name else set()
        for m in cls.get("Members") or []:
            mt = m.get("MemberType")
            if mt in ("Property", "Event") and is_valid_ident(m.get("Name", "")):
                names.add(m["Name"])
        self.inherited_names[class_name] = names
        return names

    def emit_class(self, class_name: str, lines: list[str]) -> None:
        cls = self.classes_by_name[class_name]
        sup = cls.get("Superclass")
        if not sup or sup not in self.classes_by_name:
            # Top-level classes (Instance) are defined by hand in RBX.hpp;
            # skip emitting them here.
            if class_name == "Instance":
                return
            sup = "Instance"

        # Skip generating Instance itself — it lives in RBX.hpp.
        if class_name == "Instance":
            return

        parent_names = self.collect_inherited_names(sup)

        # Names that would collide if used as a member of THIS class:
        #   - the class's own name (C++ forbids member-same-as-class)
        #   - any class name in the dump (would shadow the type, breaking
        #     subsequent `Property<SomeClass*>` lookups)
        #   - reserved/keyword names
        collision_names = (
            {class_name}
            | set(self.classes_by_name.keys())
            | RESERVED_MEMBER_NAMES
        )

        props: list[tuple[str, str]] = []
        events: list[str] = []
        skipped: list[str] = []

        for m in cls.get("Members") or []:
            mt = m.get("MemberType")
            name = m.get("Name", "")
            if not is_valid_ident(name):
                skipped.append(f"{name} ({mt}): invalid identifier")
                continue
            if name in collision_names:
                skipped.append(f"{name} ({mt}): name collision")
                continue
            if name in parent_names:
                # Inherited; don't redeclare or we shadow the parent proxy.
                continue
            if should_skip_member(m):
                continue

            if mt == "Property":
                cpp_type = map_value_type(m.get("ValueType", {}))
                if cpp_type is None:
                    continue
                props.append((cpp_type, name))
            elif mt == "Event":
                events.append(name)
            # Functions are intentionally ignored: they require typed
            # marshalling we don't implement here. Users still have access
            # to Instance::CallMethod for raw invocations.

        lines.append(f"\tRBX_CLASS_BEGIN({class_name}, {sup})")
        for cpp_type, name in props:
            lines.append(f"\t\tRBX_PROP({cpp_type}, {name})")
        if props and events:
            lines.append("")
        for name in events:
            lines.append(f"\t\tRBX_EVENT({name})")
        for note in skipped:
            lines.append(f"\t\t// skipped: {note}")
        lines.append("\tRBX_CLASS_END")
        lines.append("")

    def emit(self) -> str:
        lines: list[str] = []
        lines.append("// =====================================================================")
        lines.append("// RBX_Classes_full.hpp — auto-generated by tools/generate_classes.py.")
        lines.append("// Drop-in replacement for RBX_Classes.hpp covering every class in the")
        lines.append("// API-Dump.json this was generated from.")
        lines.append("// =====================================================================")
        lines.append("")
        lines.append("#pragma once")
        lines.append("")
        lines.append("#ifndef RBX_HPP_INCLUDED_FROM_CLASSES")
        lines.append("#  include \"RBX.hpp\"")
        lines.append("#endif")
        lines.append("")
        lines.append("namespace RBX")
        lines.append("{")

        # Forward-declare every class so cross-references in ValueType=Class
        # don't matter (we only emit Instance* anyway).
        lines.append("\t// Forward declarations -------------------------------------------------")
        for name in sorted(self.classes_by_name):
            if name == "Instance":
                continue
            lines.append(f"\tclass {name};")
        lines.append("")

        # Emit cast/service helpers (idempotent with RBX_Classes.hpp's defs;
        # if both files are included the user must keep one).
        lines.append("\ttemplate <typename T> inline T* Cast(Instance* inst)")
        lines.append("\t{")
        lines.append("\t\treturn inst ? T::FromDebugId(inst->GetDebugId()) : nullptr;")
        lines.append("\t}")
        lines.append("\ttemplate <typename T> inline T* GetService()")
        lines.append("\t{")
        lines.append("\t\tauto* raw = Instance::GetInstance(")
        lines.append("\t\t\tstd::string(\"game:GetService(\\\"\") + T::RBXClassName + \"\\\")\");")
        lines.append("\t\tif (!raw) return nullptr;")
        lines.append("\t\tauto* typed = T::FromDebugId(raw->GetDebugId());")
        lines.append("\t\tdelete raw;")
        lines.append("\t\treturn typed;")
        lines.append("\t}")
        lines.append("")

        for name in self.topo_order():
            self.emit_class(name, lines)

        lines.append("} // namespace RBX")
        return "\n".join(lines) + "\n"


# ---- Entry point ---------------------------------------------------------

def load_dump(path: str | None, download: bool) -> dict:
    if download or not path:
        print(f"Downloading API-Dump.json from {API_DUMP_URL}", file=sys.stderr)
        with urllib.request.urlopen(API_DUMP_URL) as r:
            return json.load(r)
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("dump", nargs="?", help="path to API-Dump.json")
    ap.add_argument("--download", action="store_true",
                    help="fetch the latest dump from the Client-Tracker mirror")
    ap.add_argument("--out", default="src/RBX_Classes_full.hpp",
                    help="output header path (default: src/RBX_Classes_full.hpp)")
    args = ap.parse_args()

    if not args.dump and not args.download:
        ap.error("provide a dump path or pass --download")

    dump = load_dump(args.dump, args.download)
    gen = Generator(dump)
    out = gen.emit()

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(out, encoding="utf-8")

    print(f"Wrote {out_path} ({len(out)} chars, "
          f"{len(dump['Classes'])} classes)", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
