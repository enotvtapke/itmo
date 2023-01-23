#include "MdHandler.h"

#include "IService.h"

namespace md_handler {

#define noop (void)0;

void spin(uint64_t delay_us)
{
    auto start = std::chrono::high_resolution_clock::now();
    for (;;) {
        noop;
        auto end = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start);
        if (static_cast<uint64_t>(duration.count()) >= delay_us) {
            break;
        }
    }
}

#undef noop

MdHandler::MdHandler(IService & service)
    : m_service(service)
{
}

void MdHandler::handle(const Packet & packet)
{
    for (std::size_t i = 0; i < packet.get_msg_count(); i++) {
        if (packet.get_seq_num() + i > m_last_handled) {
            m_storage.insert(packet.get_seq_num() + i);
        }
    }
    while (!m_storage.empty() && *m_storage.begin() == m_last_handled + 1) {
        m_last_handled++;
        m_service.handle_message(m_last_handled);
        m_storage.erase(m_storage.begin());
    }
}

void MdHandler::handle_heartbeat(const Packet & packet)
{
    std::unique_lock<std::mutex> lock(m_mutex);
    if (packet.get_seq_num() > m_last_handled && packet.get_seq_num() > m_last_recovered) {
        lock.unlock();
        spin(RESEND_DELAY);
        lock.lock();
        if (packet.get_seq_num() > m_last_handled && packet.get_seq_num() > m_last_recovered) {
            uint32_t resend_index = std::max(m_last_recovered, m_last_handled);
            m_last_recovered = packet.get_seq_num();
            m_service.resend_messages(resend_index + 1, packet.get_seq_num() - resend_index);
        }
    }
}

bool MdHandler::has_gap()
{
    return !m_storage.empty() &&
            *m_storage.begin() - 1 > m_last_handled &&
            *m_storage.begin() - 1 > m_last_recovered;
}

void MdHandler::handle_packet(const Packet & packet)
{
    if (packet.get_msg_count() == 0) {
        handle_heartbeat(packet);
        return;
    }
    std::unique_lock<std::mutex> lock(m_mutex);
    handle(packet);
    if (has_gap()) {
        lock.unlock();
        spin(RESEND_DELAY);
        lock.lock();
        if (has_gap()) {
            uint32_t resend_index = std::max(m_last_recovered, m_last_handled);
            m_last_recovered = *m_storage.begin() - 1;
            m_service.resend_messages(resend_index + 1, *m_storage.begin() - resend_index - 1);
        }
    }
    // TODO implement

    // to request missed messages
    // m_service.resend_messages(start_seq_num, msg_count);

    // to handle a message in order
    // m_service.handle_message(seq_num);
}

void MdHandler::handle_resend(const Packet & packet)
{
    std::unique_lock<std::mutex> lock(m_mutex);
    handle(packet);
    // TODO implement
}

} // namespace md_handler
