//
// Created by Cowpox on 12/10/24.
//

#ifndef BJVM_H
#define BJVM_H

char* parse_classfile(uint8_t* bytes, size_t len, bjvm_classfile** result);

#endif //BJVM_H
