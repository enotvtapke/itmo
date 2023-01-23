#pragma once

#include "codec.h"

#include <cmath>

/*
 * Fields
 * Order Token : Token(14)
 * Buy/Sell Indicator : Alpha(1)
 * Quantity : Integer(4)
 * Order Book : Integer(4)
 * Price : Price(4) // depends on ord_type
 * Firm : Alphanum(4)
 * User : Alphanum(6)
 * Capacity : Alphanum(1)
 * Time in Force : Alphanum(1)
 */
inline unsigned char * encode_text(unsigned char * start, const std::string & str, const size_t field_size)
{
    return encode(start, str, field_size);
}

inline unsigned char * encode_char(unsigned char * start, const char ch)
{
    return encode(start, static_cast<uint8_t>(ch));
}

inline unsigned char * encode_integer(unsigned char * start, const uint32_t value)
{
    return encode(start, value);
}

inline unsigned char * encode_double(unsigned char * start, const double value, const double epsilon = 1e-5)
{
    const double order = 10000;
    // beware: no precision loss check
    return encode(start, static_cast<uint32_t>(value * order + std::copysign(epsilon, value)));
}

namespace {

inline constexpr size_t char_size = 1;
inline constexpr size_t integer_size = 4;
inline constexpr size_t double_size = 4;

} // namespace

#define FIELD(name, protocol_type, ctype)                                                \
    inline unsigned char * encode_field_##name(unsigned char * start, const ctype value) \
    {                                                                                    \
        return encode_##protocol_type(start, value);                                     \
    }

#define VAR_FIELD(name, size)                                                                  \
    inline unsigned char * encode_field_##name(unsigned char * start, const std::string & str) \
    {                                                                                          \
        return encode_text(start, str, size);                                                  \
    }

#include "fields.inl"

#define FIELD(name, protocol_type, _) inline constexpr size_t name##_field_size = protocol_type##_size;
#define VAR_FIELD(name, size) inline constexpr size_t name##_field_size = size;

#include "fields.inl"

inline void set_opt_field_bit(unsigned char * bitfield_start, unsigned bitfield_num, unsigned bit)
{
    *(bitfield_start + bitfield_num - 1) |= bit;
}