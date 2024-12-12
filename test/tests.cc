#ifdef EMSCRIPTEN
#include <emscripten.h>
#endif

#include <iostream>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>

#include "../src/bjvm.h"

bool EndsWith(const std::string& s, const std::string& suffix) {
  if (s.size() < suffix.size()) {
    return false;
  }
  return s.substr(s.size() - suffix.size()) == suffix;
}

bool HasSuffix(std::string_view str, std::string_view suffix) {
  // Credit: https://stackoverflow.com/a/20446239/13458117
  return str.size() >= suffix.size() &&
         str.compare(str.size() - suffix.size(), suffix.size(), suffix) == 0;
}

double get_time() {
#ifdef EMSCRIPTEN
  return emscripten_get_now();
#else
  return std::chrono::duration_cast<std::chrono::milliseconds>(
             std::chrono::system_clock::now().time_since_epoch())
      .count();
#endif
}

std::vector<uint8_t> ReadFile(const std::string& file) {
#ifdef EMSCRIPTEN
  void* length_and_data = EM_ASM_PTR({
    const fs = require('fs');
    const buffer = fs.readFileSync(UTF8ToString($0));
    const length = buffer.length;

    const result = _malloc(length + 4);
    Module.HEAPU32[result >> 2] = length;
    Module.HEAPU8.set(buffer, result + 4);

    return result;
  }, file.c_str());

  uint32_t length = *reinterpret_cast<uint32_t*>(length_and_data);
  uint8_t* data = reinterpret_cast<uint8_t*>(length_and_data) + 4;

  std::vector result(data, data + length);
  free(length_and_data);
  return result;
#else // !EMSCRIPTEN
  std::vector<uint8_t> result;
  if (std::filesystem::exists(file)) {
    std::ifstream ifs(file, std::ios::binary | std::ios::ate);
    std::ifstream::pos_type pos = ifs.tellg();

    result.resize(pos);
    ifs.seekg(0, std::ios::beg);
    ifs.read(reinterpret_cast<char*>(result.data()), pos);
  } else {
    throw std::runtime_error("Classpath file not found: " + file);
  }
  return result;
#endif
}

std::vector<std::string> ListDirectory(const std::string& path, bool recursive) {
#ifdef EMSCRIPTEN
  void* length_and_data = EM_ASM_PTR({
     // Credit: https://stackoverflow.com/a/5827895
     const fs = require('fs');
     const path = require('path');
     function *walkSync(dir, recursive) {
       const files = fs.readdirSync(dir, { withFileTypes: true });
       for (const file of files) {
         if (file.isDirectory() && recursive) {
           yield* walkSync(path.join(dir, file.name), recursive);
         } else {
           yield path.join(dir, file.name);
         }
       }
     }

     var s = "";
     for (const filePath of walkSync(UTF8ToString($0), $1)) s += filePath + "\n";

      const length = s.length;
      const result = _malloc(length + 4);
      Module.HEAPU32[result >> 2] = length;
      Module.HEAPU8.set(new TextEncoder().encode(s), result + 4);

      return result;
  }, path.c_str(), recursive);


  uint32_t length = *reinterpret_cast<uint32_t*>(length_and_data);
  uint8_t* data = reinterpret_cast<uint8_t*>(length_and_data) + 4;

  std::string s(data, data + length);
  free(length_and_data);

  std::vector<std::string> result;
  size_t start = 0;

  for (size_t i = 0; i < s.size(); i++) {
    if (s[i] == '\n') {
      result.push_back(s.substr(start, i - start));
      start = i + 1;
    }
  }

  return result;
#else // !EMSCRIPTEN
  using namespace std::filesystem;

  // Recursively list files (TODO: fix recursive = false)
  std::vector<std::string> result;

  for (const auto& entry : recursive_directory_iterator(path)) {
    if (entry.is_regular_file()) {
      result.push_back(entry.path().string());
    }
  }

  return result;
#endif
}
TEST_CASE("Test classfile parsing") {
  bool fuzz = true;

  // list all java files in the jre8 directory
  auto files = ListDirectory("jre8", true);
  double total_millis = 0;

#pragma omp parallel for
  for (const auto& file : files) {
    if (!EndsWith(file, ".class")) {
      continue;
    }
    auto read = ReadFile(file);
    double start = get_time();

    std::cout << "Reading " << file << "\n";
    bjvm_parsed_classfile* cf = nullptr;
    char* error = bjvm_parse_classfile(read.data(), read.size(), &cf);
    if (error != nullptr) {
      std::cerr << "Error parsing classfile: " << error << '\n';
      free(error);
      abort();
    }
    if (cf) {
      bjvm_free_classfile(cf);
    }

    if (fuzz) {
      std::cerr << "Fuzzing classfile: " << file << '\n';

      for (int i = 0; i < read.size(); ++i) {
        for (int j = 0; j < 256; ++j) {
          read[i] += 1;
          char* error = bjvm_parse_classfile(read.data(), read.size(), &cf);
          if (error) {
            free(error);
          } else {
            bjvm_free_classfile(cf);
          }
        }
      }

      break;
    }

    total_millis += get_time() - start;
  }

  std::cout << "Total time: " << total_millis << "ms\n";
}

int register_classes(bjvm_vm* vm) {
  auto files = ListDirectory("jre8", true);
  int file_count = 0;
  for (auto file : files) {
    if (!EndsWith(file, ".class")) {
      continue;
    }
    auto read = ReadFile(file);
    wchar_t filename[1000] = {};
    file = file.substr(5); // remove "jre8/"
    mbstowcs(filename, file.c_str(), file.size());
    bjvm_vm_register_classfile(vm, filename, read.data(), read.size());
    file_count++;
  }
  return file_count;
}

TEST_CASE("Class file management") {
  bjvm_vm_options options = {};
  bjvm_vm* vm = bjvm_create_vm(options);

  int file_count = register_classes(vm);
  size_t len;
  const uint8_t* bytes;
  REQUIRE(bjvm_vm_read_classfile(vm, L"java/lang/Object.class", &bytes, &len) == 0);
  REQUIRE(len > 0);
  REQUIRE(*(uint32_t*)bytes == 0xBEBAFECA);
  REQUIRE(bjvm_vm_read_classfile(vm, L"java/lang/Object.clas", nullptr, &len) != 0);
  REQUIRE(bjvm_vm_read_classfile(vm, L"java/lang/Object.classe", nullptr, &len) != 0);
  bjvm_vm_list_classfiles(vm, nullptr, &len);
  REQUIRE(len == file_count);
  std::vector<wchar_t*> strings(len);
  bjvm_vm_list_classfiles(vm, strings.data(), &len);
  bool found = false;
  for (int i = 0; i < len; ++i) {
    found = found || wcscmp(strings[i], L"java/lang/ClassLoader.class") == 0;
    free(strings[i]);
  }
  REQUIRE(found);
  bjvm_free_vm(vm);
}

TEST_CASE("Compressed bitset") {
  for (int size = 1; size < 256; ++size) {
    std::vector<uint8_t> reference(size);
    auto bitset = bjvm_init_compressed_bitset(size);

    for (int i = 0; i < 1000; ++i) {
      int index = rand() % size;
      switch (rand() % 4) {
        case 0: {
          int* set_bits = nullptr, length = 0, capacity = 0;
          set_bits = bjvm_list_compressed_bitset_bits(bitset, set_bits, &length, &capacity);
          for (int i = 0; i < length; ++i) {
            if (!reference[set_bits[i]]) REQUIRE(false);
          }
          free(set_bits);
          break;
        }
        case 1: {
          bool test = bjvm_test_set_compressed_bitset(&bitset, index);
          if (test != reference[index])
            REQUIRE(test == reference[index]);
          reference[index] = true;
          break;
        }
        case 2: {
          bool test = bjvm_test_reset_compressed_bitset(&bitset, index);
          if (test != reference[index])
            REQUIRE(test == reference[index]);
          reference[index] = false;
          break;
        }
        case 3: {
          bool test = bjvm_test_compressed_bitset(bitset, index);
          if (test != reference[index])
            REQUIRE(test == reference[index]);
          break;
        }
      }
    }

    bjvm_free_compressed_bitset(bitset);
  }
}

TEST_CASE("parse_field_descriptor valid cases") {
  const wchar_t* fields = L"Lcom/example/Example;[I[[[JLjava/lang/String;[[Ljava/lang/Object;BVCZ";
  bjvm_parsed_field_descriptor com_example_Example, Iaaa, Jaa, java_lang_String, java_lang_Object, B, V, C, Z;
  REQUIRE(!parse_field_descriptor(&fields, wcslen(fields), &com_example_Example));
  REQUIRE(!parse_field_descriptor(&fields, wcslen(fields), &Iaaa));
  REQUIRE(!parse_field_descriptor(&fields, wcslen(fields), &Jaa));
  REQUIRE(!parse_field_descriptor(&fields, wcslen(fields), &java_lang_String));
  REQUIRE(!parse_field_descriptor(&fields, wcslen(fields), &java_lang_Object));
  REQUIRE(!parse_field_descriptor(&fields, wcslen(fields), &B));
  REQUIRE(!parse_field_descriptor(&fields, wcslen(fields), &V));
  REQUIRE(!parse_field_descriptor(&fields, wcslen(fields), &C));
  REQUIRE(!parse_field_descriptor(&fields, wcslen(fields), &Z));

  REQUIRE(compare_utf8_entry(&com_example_Example.class_name, "com/example/Example"));
  REQUIRE(com_example_Example.dimensions == 0);
  REQUIRE(com_example_Example.kind == BJVM_TYPE_KIND_REFERENCE);

  REQUIRE(Iaaa.kind == BJVM_PRIMITIVE_INT);
  REQUIRE(Iaaa.dimensions == 1);

  REQUIRE(Jaa.kind == BJVM_PRIMITIVE_LONG);
  REQUIRE(Jaa.dimensions == 3);

  REQUIRE(compare_utf8_entry(&java_lang_String.class_name, "java/lang/String"));
  REQUIRE(java_lang_String.dimensions == 0);

  REQUIRE(compare_utf8_entry(&java_lang_Object.class_name, "java/lang/Object"));
  REQUIRE(java_lang_Object.dimensions == 2);

  REQUIRE(B.kind == BJVM_PRIMITIVE_BYTE);
  REQUIRE(C.kind == BJVM_PRIMITIVE_CHAR);
  REQUIRE(V.kind == BJVM_PRIMITIVE_VOID);
  REQUIRE(Z.kind == BJVM_PRIMITIVE_BOOLEAN);

  free_field_descriptor(com_example_Example);
  free_field_descriptor(java_lang_Object);
  free_field_descriptor(java_lang_String);
}

TEST_CASE("VM initialization") {
  bjvm_vm_options options = {};
  bjvm_vm* vm = bjvm_create_vm(options);

  //register_classes(vm);

  bjvm_free_vm(vm);
}