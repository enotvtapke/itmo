#pragma once

#include <cstddef>
#include <functional>
#include <initializer_list>
#include <memory>
#include <new>

namespace pool {

class Pool;

Pool * create_pool(std::size_t size, std::initializer_list<std::size_t> sizes);

void destroy_pool(Pool * pool);

std::size_t pool_slab_size(const Pool & pool);

void * allocate(Pool & pool, std::size_t n);

void deallocate(Pool & pool, const void * ptr);

} // namespace pool

class PoolAllocator
{
public:
    PoolAllocator(const std::size_t size, std::initializer_list<std::size_t> sizes)
        : m_pool(pool::create_pool(size, sizes), pool::destroy_pool)
    {
    }

    void * allocate(const std::size_t n);

    void deallocate(const void * ptr);

private:
    std::unique_ptr<pool::Pool, decltype(&pool::destroy_pool)> m_pool;
};

inline void * PoolAllocator::allocate(const std::size_t n)
{
    return pool::allocate(*m_pool, n);
}

inline void PoolAllocator::deallocate(const void * ptr)
{
    pool::deallocate(*m_pool, ptr);
}
