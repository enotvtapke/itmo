#pragma once

#include "Packet.h"

#include <atomic>
#include <mutex>
#include <set>
#include <thread>

namespace md_handler {

class IService;

class MdHandler
{
    static constexpr uint64_t RESEND_DELAY = 15000; // delay in microseconds
    void handle(const Packet & packet);
    void handle_heartbeat(const Packet & packet);
    bool has_gap();

public:
    MdHandler(IService & service);
    void handle_packet(const Packet & packet);
    void handle_resend(const Packet & packet);

private:
    IService & m_service;
    std::set<uint32_t> m_storage;
    uint32_t m_last_handled{0};
    uint32_t m_last_recovered{0};
    std::mutex m_mutex;
};

} // namespace md_handler