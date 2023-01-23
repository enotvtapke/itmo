#include "requests.h"

#include <string>

namespace {

void encode_new_order_opt_fields(unsigned char * bitfield_start,
                                 const char time_in_force,
                                 const char capacity)
{
#define FIELD(name, bitfield_num, bit) \
    set_opt_field_bit(bitfield_start, bitfield_num, bit);
#include "enter_order_opt_fields.inl"
    auto * p = bitfield_start + new_order_bitfield_num() + 3;
#define FIELD(name, bitfield_num, bit) \
    p = encode_field_##name(p, name);
#include "enter_order_opt_fields.inl"
}

uint8_t encode_request_type(const RequestType type)
{
    switch (type) {
    case RequestType::EnterOrder:
        return 'O';
    }
    return 0;
}

unsigned char * add_request_header(unsigned char * start, const RequestType type)
{
    return encode(start, encode_request_type(type));
}

char convert_side(const Side side)
{
    switch (side) {
    case Side::Buy: return 'B';
    case Side::Sell: return 'S';
    }
    return 0;
}

double encode_price(double price, const OrdType ord_type)
{
    switch (ord_type) {
    case OrdType::Market: return 214748.3647;
    case OrdType::Limit: return price;
    }
    return 0;
}

char convert_time_in_force(const TimeInForce time_in_force)
{
    switch (time_in_force) {
    case TimeInForce::Day: return '0';
    case TimeInForce::IOC: return '3';
    }
    return 0;
}

char convert_capacity(const Capacity capacity)
{
    switch (capacity) {
    case Capacity::Agency: return '1';
    case Capacity::Principal: return '2';
    case Capacity::RisklessPrincipal: return '7';
    }
    return 0;
}

} // anonymous namespace

std::vector<unsigned char> create_enter_order_request(
        const std::string & cl_ord_id,
        const Side side,
        const double volume,
        const double price,
        const std::string & symbol,
        const OrdType ord_type,
        const TimeInForce time_in_force,
        const Capacity capacity,
        const std::string & firm,
        const std::string & user)
{
    static_assert(calculate_size(RequestType::EnterOrder) == 44, "Wrong New Order message size");

    std::vector<unsigned char> msg(calculate_size(RequestType::EnterOrder));
    auto * p = add_request_header(&msg[0], RequestType::EnterOrder);
    p = encode_field_cl_ord_id(p, cl_ord_id);
    p = encode_field_side(p, convert_side(side));
    p = encode_field_volume(p, volume);
    p = encode_field_symbol(p, std::stoul(symbol));
    p = encode_field_price(p, encode_price(price, ord_type));
    p = encode_field_firm(p, firm);
    p = encode_field_user(p, user);
    encode_new_order_opt_fields(p,
                                convert_time_in_force(time_in_force),
                                convert_capacity(capacity));
    return msg;
}
