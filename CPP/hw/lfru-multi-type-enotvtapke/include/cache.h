#pragma once

#include <cstddef>
#include <iostream>
#include <list>
#include <new>
#include <ostream>
#include <unordered_map>

template <class Key, class KeyProvider, class Allocator>
class Cache
{
public:
    template <class... AllocArgs>
    Cache(const std::size_t cache_size, AllocArgs &&... alloc_args)
        : m_max_top_size(cache_size)
        , m_max_low_size(cache_size)
        , m_alloc(std::forward<AllocArgs>(alloc_args)...)
    {
    }

    std::size_t size() const
    {
        return m_top_queue.size() + m_low_queue.size();
    }

    bool empty() const
    {
        return m_top_queue.empty() && m_low_queue.empty();
    }

    template <class T>
    T & get(const Key & key);

    std::ostream & print(std::ostream & strm) const;
    std::ostream & print_queue(std::ostream & strm, std::list<std::pair<Key, KeyProvider *>> queue) const;

    friend std::ostream & operator<<(std::ostream & strm, const Cache & cache)
    {
        return cache.print(strm);
    }

private:
    const std::size_t m_max_top_size;
    const std::size_t m_max_low_size;
    std::list<std::pair<Key, KeyProvider *>> m_top_queue;
    std::unordered_map<Key, typename std::list<std::pair<Key, KeyProvider *>>::iterator> m_top_map;
    std::list<std::pair<Key, KeyProvider *>> m_low_queue;
    std::unordered_map<Key, typename std::list<std::pair<Key, KeyProvider *>>::iterator> m_low_map;
    Allocator m_alloc;
};

template <class Key, class KeyProvider, class Allocator>
template <class T>
inline T & Cache<Key, KeyProvider, Allocator>::get(const Key & key)
{
    if (m_top_map.count(key) != 0) {
        m_top_queue.push_front(std::move(*m_top_map.at(key)));
        m_top_queue.erase(m_top_map.at(key));
        m_top_map[key] = m_top_queue.begin();
    }
    else {
        if (m_low_map.count(key) != 0) {
            auto it = m_low_map.at(key);
            auto elem = *it;
            m_low_map.erase(elem.first);
            m_low_queue.erase(it);
            if (m_max_top_size == m_top_queue.size()) {
                m_low_queue.push_front(std::move(m_top_queue.back()));
                m_low_map.insert(std::make_pair(m_low_queue.front().first, m_low_queue.begin()));

                m_top_map.erase(m_top_queue.back().first);
                m_top_queue.pop_back();
            }
            m_top_queue.push_front(elem);
            m_top_map.insert(std::make_pair(key, m_top_queue.begin()));
        }
        else {
            if (m_max_low_size == m_low_queue.size()) {
                m_low_map.erase(m_low_queue.back().first);
                m_alloc.template destroy<KeyProvider>(m_low_queue.back().second);
                m_low_queue.pop_back();
            }
            m_low_queue.emplace_front(key, m_alloc.template create<T>(key));
            m_low_map.insert(std::make_pair(key, m_low_queue.begin()));
            return static_cast<T &>(*m_low_queue.front().second);
        }
    }
    return static_cast<T &>(*m_top_queue.front().second);
}

template <class Key, class KeyProvider, class Allocator>
inline std::ostream & Cache<Key, KeyProvider, Allocator>::print(std::ostream & strm) const
{
    strm << "Priority: ";
    print_queue(strm, m_top_queue);

    strm << "\nRegular: ";
    print_queue(strm, m_low_queue);
    return strm << "\n";
}

template <class Key, class KeyProvider, class Allocator>
inline std::ostream & Cache<Key, KeyProvider, Allocator>::print_queue(std::ostream & strm, std::list<std::pair<Key, KeyProvider *>> queue) const
{
    if (queue.empty()) {
        strm << "<empty>";
    }
    else {
        bool first = true;
        for (auto const v : queue) {
            if (first) {
                first = false;
                strm << *v.second;
            }
            else {
                strm << ", " << *v.second;
            }
        }
    }
    return strm;
}