#include "pool.h"

#include <assert.h>
#include <cstddef>
#include <iostream>
#include <map>
#include <new>
#include <utility>
#include <vector>

using std::size_t;

namespace pool {

class Pool
{
public:
    Pool(const size_t size, const std::initializer_list<std::size_t> sizes)
        : m_slab_size(size)
        , m_obj_sizes(sizes.size())
        , m_slab_index_map()
        , m_storage(size * sizes.size())
        , m_used_map(sizes.size())
    {
        size_t i = 0;
        for (const size_t obj_size : sizes) {
            m_slab_index_map[obj_size] = i;
            m_obj_sizes[i] = obj_size;
            m_used_map[i] = std::vector<bool>(size / obj_size);
            i++;
        }
    }

    size_t get_slab_size() const
    {
        return m_slab_size;
    }

    void * allocate(size_t n);

    void deallocate(const void * ptr);

private:
    static constexpr size_t npos = static_cast<size_t>(-1);

    size_t find_empty_place(size_t n) const;

    const size_t m_slab_size;
    std::vector<size_t> m_obj_sizes;
    std::map<size_t, size_t> m_slab_index_map;
    std::vector<std::byte> m_storage;
    std::vector<std::vector<bool>> m_used_map;
};

size_t Pool::find_empty_place(size_t n) const
{
    auto used_map = m_used_map[m_slab_index_map.at(n)];
    for (size_t i = 0; i < used_map.size(); ++i) {
        if (!used_map[i]) {
            return i;
        }
    }
    return npos;
}

void * Pool::allocate(const size_t n)
{
    const size_t pos = find_empty_place(n);
    if (pos != npos) {
        const size_t slab_index = m_slab_index_map.at(n);
        m_used_map[slab_index][pos] = true;
        return &m_storage[slab_index * m_slab_size + pos * n];
    }
    throw std::bad_alloc{};
}

void Pool::deallocate(const void * ptr)
{
    auto b_ptr = static_cast<const std::byte *>(ptr);
    const auto begin = &m_storage[0];
    if (b_ptr >= begin) {
        const size_t slab_index = (b_ptr - begin) / m_slab_size;
        const size_t obj_size = m_obj_sizes[slab_index];
        const size_t offset = (b_ptr - &m_storage[slab_index * m_slab_size]) / obj_size;
        assert(((b_ptr - &m_storage[slab_index * m_slab_size]) % obj_size) == 0);
        if (offset < m_used_map[slab_index].size()) {
            m_used_map[slab_index][offset] = false;
        }
    }
}

Pool * create_pool(const size_t size, const std::initializer_list<std::size_t> sizes)
{
    return new Pool(size, sizes);
}

void destroy_pool(Pool * pool)
{
    delete pool;
}

size_t pool_slab_size(const Pool & pool)
{
    return pool.get_slab_size();
}

void * allocate(Pool & pool, const size_t n)
{
    return pool.allocate(n);
}

void deallocate(Pool & pool, const void * ptr)
{
    pool.deallocate(ptr);
}

} // namespace pool