#pragma once

#include <functional>
#include <iostream>
#include <memory>

template <class Key, class Value, class Less = std::less<Key>>
class BPTree
{
    static constexpr std::size_t block_size = 4096;

    class InternalNode;
    class Leaf;
    class Node
    {
        friend class BPTree;
        virtual std::tuple<Leaf &, std::size_t> lower_bound(const Key &) = 0;
        virtual std::tuple<Leaf &, std::size_t> upper_bound(const Key &) = 0;
        virtual std::tuple<Node *, Leaf &, std::size_t, bool> insert(Node * root, const std::pair<Key, Value> &) = 0;
        virtual std::tuple<Node *, Leaf &, std::size_t, bool> insert(Node * root, std::pair<Key, Value> &&) = 0;
        virtual std::tuple<Node *, Leaf &, std::size_t, bool> erase(Node * root, const Key &) = 0;
        virtual std::ostream & print(std::ostream & strm) const = 0;
        virtual void set_parent(InternalNode *) = 0;
        virtual std::tuple<Leaf &, std::size_t> end() = 0;

    public:
        virtual ~Node() = default;
    };

    class InternalNode : public Node
    {
    public:
        InternalNode(const std::size_t size, std::unique_ptr<Node> a)
            : m_size(size)
            , m_left(std::move(a))
        {
            m_left->set_parent(this);
        }

        InternalNode(std::unique_ptr<Node> a, const Key & key, std::unique_ptr<Node> b)
            : m_size(1)
            , m_left(std::move(a))
        {
            m_data[0] = std::make_pair(key, std::move(b));
            m_left->set_parent(this);
            m_data[0].second->set_parent(this);
        }

        std::tuple<Leaf &, std::size_t> lower_bound(const Key & key) override
        {
            const auto rend = std::make_reverse_iterator(m_data.begin());
            const auto it = std::lower_bound(std::make_reverse_iterator(m_data.begin() + m_size), rend, key, [](const auto & lhs, const auto & rhs) {
                return lhs.first > rhs;
            });
            if (it != rend) {
                return it->second->lower_bound(key);
            }
            return m_left->lower_bound(key);
        };

        std::tuple<Leaf &, std::size_t> upper_bound(const Key & key) override
        {
            const auto rend = std::make_reverse_iterator(m_data.begin());
            const auto it = std::lower_bound(std::make_reverse_iterator(m_data.begin() + m_size), rend, key, [](const auto & lhs, const auto & rhs) {
                return lhs.first > rhs;
            });
            if (it != rend) {
                return it->second->upper_bound(key);
            }
            return m_left->upper_bound(key);
        }

        std::tuple<Leaf &, std::size_t> end() override
        {
            return m_data[m_size - 1].second->end();
        }

        std::tuple<Node *, Leaf &, std::size_t, bool> insert(Node * root, const std::pair<Key, Value> & v) override
        {
            const auto rend = std::make_reverse_iterator(m_data.begin());
            const auto it = std::lower_bound(std::make_reverse_iterator(m_data.begin() + m_size), rend, v, [](const auto & lhs, const auto & rhs) {
                return lhs.first > rhs.first;
            });
            if (it != rend) {
                return it->second->insert(root, v);
            }
            return m_left->insert(root, v);
        }

        std::tuple<Node *, Leaf &, std::size_t, bool> insert(Node * root, std::pair<Key, Value> && v) override
        {
            const auto rend = std::make_reverse_iterator(m_data.begin());
            const auto it = std::lower_bound(std::make_reverse_iterator(m_data.begin() + m_size), rend, v, [](const auto & lhs, const auto & rhs) {
                return lhs.first > rhs.first;
            });
            if (it != rend) {
                return it->second->insert(root, std::move(v));
            }
            return m_left->insert(root, std::move(v));
        }

        std::tuple<Node *, Leaf &, std::size_t, bool> erase(Node * root, const Key & key) override
        {
            const auto rend = std::make_reverse_iterator(m_data.begin());
            const auto it = std::lower_bound(std::make_reverse_iterator(m_data.begin() + m_size), rend, key, [](const auto & lhs, const auto & rhs) {
                return lhs.first > rhs;
            });
            if (it != rend) {
                return it->second->erase(root, key);
            }
            return m_left->erase(root, key);
        }

        void change_key(const Key & key, Key & new_key)
        {
            const auto rend = std::make_reverse_iterator(m_data.begin());
            const auto it = std::lower_bound(std::make_reverse_iterator(m_data.begin() + m_size), rend, key, [](const auto & lhs, const auto & rhs) {
                return lhs.first > rhs;
            });
            if (it != rend) {
                it->first = new_key;
            }
        }

        Node * push(Node * root, Key key, std::unique_ptr<Node> && new_child)
        {
            const auto it = std::lower_bound(m_data.begin(), m_data.begin() + m_size, key, [](const auto & lhs, const auto & rhs) {
                return lhs.first < rhs;
            });
            const std::size_t i = it - m_data.begin();

            if (i == m_size || it->first != key) {
                std::move_backward(it, m_data.begin() + m_size, m_data.begin() + m_size + 1);
                *it = std::make_pair(std::move(key), std::move(new_child));
                it->second->set_parent(this);
                ++m_size;
                if (m_size <= max_elements) {
                    return root;
                }
                else {
                    return split(root);
                }
            }
            return root;
        }

        std::pair<Key, std::unique_ptr<Node>> pop_back()
        {
            return std::move(m_data[--m_size]);
        }

        void push_back(std::pair<Key, std::unique_ptr<Node>> && v)
        {
            m_data[m_size++] = std::move(v);
        }

        std::pair<Key, std::unique_ptr<Node>> pop_front()
        {
            std::pair<Key, std::unique_ptr<Node>> t = std::move(*m_data.begin());
            for (auto j = m_data.begin(); j < m_data.begin() + m_size - 1; j++) {
                *j = std::move(*(j + 1));
            }
            m_parent->change_key(m_data[0].first, m_data[0].first);
            --m_size;
            return t;
        }

        void push_front(std::pair<Key, std::unique_ptr<Node>> && v)
        {
            std::move_backward(m_data.begin(), m_data.begin() + m_size, m_data.begin() + m_size + 1);
            *m_data.begin() = std::move(v);
            ++m_size;
            m_parent->change_key(m_data[1].first, m_data[0].first);
        }

        Node * pop(Node * root, Key key)
        {
            const auto it = std::lower_bound(m_data.begin(), m_data.begin() + m_size, key, [](const auto & lhs, const auto & rhs) {
                return lhs.first < rhs;
            });
            for (auto j = it; j < it + m_size - 1; j++) {
                *j = std::move(*(j + 1));
            }
            --m_size;
            if (m_parent == nullptr && m_size == 0) {
                Node * new_root = m_left.get();
                m_left.release();
                delete this;
                return new_root;
            }
            if (m_size < (elements + 1) / 2 && m_parent != nullptr) {
                if (m_prev != nullptr && m_prev->m_size > (elements + 1) / 2) {
                    push_front(m_prev->pop_back());
                }
                else if (m_next != nullptr && m_next->m_size > (elements + 1) / 2) {
                    push_back(m_next->pop_front());
                }
                else {
                    if (m_next != nullptr) {
                        root = merge(root, m_next);
                    }
                    else {
                        root = m_prev->merge(root, this);
                    }
                }
            }
            else {
                return root;
            }
            return root;
        }

        std::ostream & print(std::ostream & strm) const override
        {
            strm << "{ (" << m_size << ")";
            m_left->print(strm);
            for (std::size_t i = 0; i < m_size; i++) {
                strm << "| " << m_data[i].first << " | ";
                m_data[i].second->print(strm);
            }
            strm << "} ";
            return strm << "\n";
        }

        friend std::ostream & operator<<(std::ostream & strm, const InternalNode & node)
        {
            return node.print(strm);
        }

    private:
        void set_parent(InternalNode * parent) override { m_parent = parent; }

        void adopt_childs()
        {
            for (std::size_t i = 0; i < m_size; i++) {
                m_data[i].second->set_parent(this);
            }
        };

        Node * merge(Node * root, InternalNode * node)
        {
            const Key & split_key = node->m_data[0].first;
            const std::size_t new_size = m_size + node->m_size;
            const Key & key = std::get<0>(upper_bound(m_data[m_size - 1].first)).m_data[0].first;
            m_data[m_size] = std::make_pair(key, std::move(node->m_left));
            m_size++;
            std::move(node->m_data.begin(), node->m_data.begin() + node->m_size, m_data.begin() + m_size);
            m_next = node->m_next;
            m_size = new_size;
            if (node->m_next != nullptr) {
                node->m_next->m_prev = this;
            }
            if (m_parent != nullptr) {
                root = m_parent->pop(root, split_key);
            }
            return root;
        }

        Node * split(Node * root)
        {
            const std::size_t new_size = m_size / 2;
            const std::size_t rest_size = m_size - new_size - 1;
            auto new_node = std::make_unique<InternalNode>(rest_size, std::move(m_data[new_size].second));
            std::move(m_data.begin() + new_size + 1, m_data.end(), new_node->m_data.begin());
            new_node->adopt_childs();
            new_node->m_next = m_next;
            m_next = &(*new_node);
            m_size = new_size;
            Key split_key = std::move(m_data[m_size].first);
            if (m_parent != nullptr) {
                root = m_parent->push(root, std::move(split_key), std::move(new_node));
            }
            else {
                root = new InternalNode(std::unique_ptr<Node>(this), split_key, std::move(new_node));
            }
            return root;
        };

        using Pair = std::pair<Key, std::unique_ptr<Node>>;
        std::size_t m_size = 0;
        std::unique_ptr<Node> m_left;
        InternalNode * m_parent = nullptr;
        InternalNode * m_prev = nullptr;
        InternalNode * m_next = nullptr;
        static constexpr std::size_t elements = (block_size - sizeof(m_size) - sizeof(m_left) - sizeof(m_parent) - sizeof(m_prev) - sizeof(m_next)) / sizeof(Pair);
        static constexpr std::size_t max_elements = elements - 1;
        std::array<Pair, elements> m_data;
    };

    class Leaf : public Node
    {
        friend class InternalNode;
        using Pair = std::pair<Key, Value>;

    public:
        std::tuple<Leaf &, std::size_t> lower_bound(const Key & key) override
        {
            const auto it = std::lower_bound(m_data.begin(), m_data.begin() + m_size, key, [](const auto & lhs, const auto & rhs) {
                return lhs.first < rhs;
            });
            const std::size_t i = it - m_data.begin();
            if (i < m_size) {

                return {*this, i};
            }

            return {*this, m_size};
        }

        std::tuple<Leaf &, std::size_t> upper_bound(const Key & key) override
        {
            const auto it = std::upper_bound(m_data.begin(), m_data.begin() + m_size, key, [](const auto & lhs, const auto & rhs) {
                return Less{}(lhs, rhs.first);
            });
            const std::size_t i = it - m_data.begin();
            if (i < m_size) {

                return {*this, i};
            }

            return {*this, m_size};
        }

        std::tuple<Leaf &, std::size_t> end() override
        {
            return {*this, m_size};
        }

        std::tuple<Node *, Leaf &, std::size_t, bool> insert(Node * root, const std::pair<Key, Value> & v) override
        {
            const auto it = std::lower_bound(m_data.begin(), m_data.begin() + m_size, v, [](const auto & lhs, const auto & rhs) {
                return lhs.first < rhs.first;
            });
            const std::size_t i = it - m_data.begin();
            if (i == m_size || it->first != v.first) {
                std::move_backward(it, m_data.begin() + m_size, m_data.begin() + m_size + 1);
                *it = v;
                ++m_size;
                if (m_size <= max_elements) {
                    return {root, *this, i, true};
                }
                else {
                    root = split(root);
                    if (i < m_size) {
                        return {root, *this, i, true};
                    }
                    else {
                        return {root, *m_next, i - m_size, true};
                    }
                }
            }
            else {
                return {root, *this, i, false};
            }
        }

        std::tuple<Node *, Leaf &, std::size_t, bool> insert(Node * root, std::pair<Key, Value> && v) override
        {

            const auto it = std::lower_bound(m_data.begin(), m_data.begin() + m_size, v, [](const auto & lhs, const auto & rhs) {
                return lhs.first < rhs.first;
            });

            const std::size_t i = it - m_data.begin();

            if (i == m_size || it->first != v.first) {

                std::move_backward(it, m_data.begin() + m_size, m_data.begin() + m_size + 1);
                *it = std::move(v);

                ++m_size;
                if (m_size <= max_elements) {
                    return {root, *this, i, true};
                }
                else {

                    root = split(root);

                    if (i < m_size) {
                        return {root, *this, i, true};
                    }
                    else {

                        return {root, *m_next, i - m_size, true};
                    }
                }
            }
            else {
                return {root, *this, i, false};
            }
        }

        std::pair<Key, Value> pop_back()
        {
            return m_data[--m_size];
        }

        void push_back(std::pair<Key, Value> && v)
        {
            m_data[m_size++] = std::move(v);
        }

        std::pair<Key, Value> pop_front()
        {
            std::pair<Key, Value> t = std::move(*m_data.begin());
            for (auto j = m_data.begin(); j < m_data.begin() + m_size - 1; j++) {
                *j = std::move(*(j + 1));
            }
            m_parent->change_key(m_data[0].first, m_data[0].first);
            --m_size;
            return t;
        }

        void push_front(std::pair<Key, Value> && v)
        {
            std::move_backward(m_data.begin(), m_data.begin() + m_size, m_data.begin() + m_size + 1);
            *m_data.begin() = std::move(v);
            ++m_size;
            m_parent->change_key(m_data[1].first, m_data[0].first);
        }

        std::tuple<Node *, Leaf &, std::size_t, bool> erase(Node * root, const Key & key) override
        {
            const auto it = std::lower_bound(m_data.begin(), m_data.begin() + m_size, key, [](const auto & lhs, const auto & rhs) {
                return lhs.first < rhs;
            });
            std::size_t i = it - m_data.begin();

            if (it != m_data.begin() + m_size && it->first == key) {
                return erase(root, i);
            }
            else {
                return {root, *this, i, false};
            }
        }

        std::tuple<Node *, Leaf &, std::size_t, bool> erase(Node * root, std::size_t index)
        {

            const auto it = m_data.begin() + index;
            for (auto j = it; j < m_data.begin() + m_size - 1; j++) {
                *(j) = std::move(*(j + 1));
            }
            m_size--;
            if (m_size < (elements + 1) / 2 && m_parent != nullptr) {
                if (m_prev != nullptr && m_prev->m_size > (elements + 1) / 2) {
                    push_front(m_prev->pop_back());
                    return {root, *this, index + 1, true};
                }
                else if (m_next != nullptr && m_next->m_size > (elements + 1) / 2) {
                    push_back(m_next->pop_front());
                    return {root, *this, index, true};
                }
                else {
                    if (m_next != nullptr) {
                        root = merge(root, m_next);
                        return {root, *this, index, true};
                    }
                    else if (m_prev != nullptr) {
                        Leaf * t = m_prev;
                        index = m_prev->m_size + index;
                        root = m_prev->merge(root, this);
                        return {root, *t, index, true};
                    }
                }
            }
            return {root, *this, index, true};
        }

        Leaf * next()
        {
            return m_next;
        }
        const Leaf * next() const
        {
            return m_next;
        }

        std::size_t size() const { return m_size; }

        Pair & operator[](const std::size_t i) { return m_data[i]; }
        const Pair & operator[](const std::size_t i) const
        {
            return m_data[i];
        }

        std::ostream & print(std::ostream & strm) const override
        {
            strm << "< (" << m_size << ")";
            for (std::size_t i = 0; i < m_size; i++) {
                strm << "[" << m_data[i].first << ": " << m_data[i].second << "] ";
            }
            strm << "> ";
            return strm;
        }

        friend std::ostream & operator<<(std::ostream & strm, const Leaf & leaf)
        {
            return leaf.print(strm);
        }

    private:
        Node * merge(Node * root, Leaf * node)
        {
            const std::size_t new_size = m_size + node->m_size;
            std::move(node->m_data.begin(), node->m_data.begin() + node->m_size, m_data.begin() + m_size);
            const Key & merge_key = (m_data.begin() + m_size)->first;
            m_size = new_size;
            this->m_next->m_prev = this;
            m_next = node->m_next;
            if (node->m_next != nullptr) {
                node->m_next->m_prev = this;
            }

            node->m_size = 0;
            root = m_parent->pop(root, merge_key);
            return root;
        }

        Node * split(Node * root)
        {
            const std::size_t new_size = m_size / 2;
            const std::size_t rest_size = m_size - new_size;
            auto new_leaf = std::make_unique<Leaf>();

            std::move(m_data.begin() + new_size, m_data.end(), new_leaf->m_data.begin());
            new_leaf->m_size = rest_size;
            new_leaf->m_next = m_next;
            new_leaf->m_prev = this;
            m_next = new_leaf.get();
            m_size = new_size;
            const Key & split_key = m_next->m_data[0].first;

            if (m_parent != nullptr) {

                root = m_parent->push(root, split_key, std::move(new_leaf));
            }
            else {
                root = new InternalNode(std::unique_ptr<Node>(this), split_key, std::move(new_leaf));
            }
            return root;
        };

        void set_parent(InternalNode * parent) override
        {
            m_parent = parent;
        }

        unsigned short m_size = 0;
        InternalNode * m_parent = nullptr;
        Leaf * m_next = nullptr;
        Leaf * m_prev = nullptr;
        static constexpr std::size_t elements = (block_size - sizeof(m_parent) - sizeof(m_next) - sizeof(m_prev)) / sizeof(Pair);
        static constexpr std::size_t max_elements = elements - 1;
        std::array<Pair, elements> m_data;
    };

    template <bool is_const>
    class iterator_impl
    {
        friend class BPTree;

        template <class T>
        using add_constness = std::conditional_t<is_const, std::add_const_t<T>, T>;

        void check_end()
        {
            if (m_index == m_current->size()) {
                m_current = m_current->next();
                m_index = 0;
            }
        }

        iterator_impl(Leaf & current, const std::size_t index = 0)
            : m_current(&current)
            , m_index(index)
        {
            check_end();
        }

    public:
        using difference_type = std::ptrdiff_t;
        using value_type = add_constness<std::pair<Key, Value>>;
        using pointer = value_type *;
        using reference = value_type &;
        using iterator_category = std::forward_iterator_tag;

        template <bool v>
        iterator_impl(const iterator_impl<v> & other)
            : m_current(other.m_current)
            , m_index(other.m_index)
        {
        }

        iterator_impl()
            : m_current(nullptr)
            , m_index(0)
        {
        }

        iterator_impl & operator++()
        {
            ++m_index;
            check_end();
            return *this;
        }

        iterator_impl operator++(int)
        {
            auto tmp = *this;
            operator++();
            return tmp;
        }

        reference operator*() const
        {
            return (*m_current)[m_index];
        }

        pointer operator->() const
        {
            return &(*m_current)[m_index];
        }

        friend bool operator==(const iterator_impl & lhs, const iterator_impl & rhs)
        {
            return lhs.m_current == rhs.m_current && lhs.m_index == rhs.m_index;
        }

        friend bool operator!=(const iterator_impl & lhs, const iterator_impl & rhs)
        {
            return !(lhs == rhs);
        }

        std::ostream & print(std::ostream & strm) const
        {
            if (m_current != nullptr) {
                return strm << *m_current << " at " << m_index;
            }
            else {
                return strm << "<END>";
            }
        }

        friend std::ostream & operator<<(std::ostream & strm, const iterator_impl & iter)
        {
            return iter.print(strm);
        }

    private:
        add_constness<Leaf> * m_current = nullptr;
        std::size_t m_index = 0;
    };

public:
    using key_type = Key;
    using mapped_type = Value;
    using value_type = std::pair<const Key, Value>;
    using reference = value_type &;
    using const_reference = const value_type &;
    using pointer = value_type *;
    using const_pointer = const value_type *;
    using size_type = std::size_t;

    using iterator = iterator_impl<false>;
    using const_iterator = iterator_impl<true>;

    BPTree()
        : m_root(new Leaf)
        , m_first(*static_cast<Leaf *>(m_root)){};
    BPTree(std::initializer_list<std::pair<const Key, Value>> values)
        : BPTree()
    {
        insert(values);
    };
    BPTree(const BPTree & other)
        : m_root(other.m_root)
        , m_first(other.m_first){};
    BPTree(BPTree && other)
        : m_root(std::move(other.m_root))
        , m_first(std::move(other.m_first)){};

    ~BPTree()
    {
        delete m_root;
    }

    iterator begin() { return iterator(m_first); };
    const_iterator cbegin() const { return const_iterator(m_first); };
    const_iterator begin() const { return const_iterator(m_first); };
    iterator end() { return iterator(); };
    const_iterator cend() const { return const_iterator(); };
    const_iterator end() const { return const_iterator(); };

    bool empty() const { return begin() == end(); };
    size_type size() const { return std::distance(begin(), end()); };
    void clear()
    {
        delete m_root;
        m_root = new Leaf;
        m_first = *static_cast<Leaf *>(m_root);
    };

    size_type count(const Key & key) const
    {
        if (find(key) != end()) {
            return 1;
        }
        return 0;
    };
    bool contains(const Key & key) const
    {
        return count(key) != 0;
    };
    std::pair<iterator, iterator> equal_range(const Key & key)
    {
        return {lower_bound(key), upper_bound(key)};
    }

    std::pair<const_iterator, const_iterator> equal_range(const Key & key) const
    {
        return {lower_bound(key), upper_bound(key)};
    }

    iterator lower_bound(const Key & key)
    {
        const auto [leaf, index] = m_root->lower_bound(key);

        iterator(leaf, index);

        return {leaf, index};
    };
    const_iterator lower_bound(const Key & key) const
    {
        return const_cast<BPTree *>(this)->lower_bound(key);
    };
    iterator upper_bound(const Key & key)
    {
        const auto [leaf, index] = m_root->upper_bound(key);

        iterator(leaf, index);

        return {leaf, index};
    };
    const_iterator upper_bound(const Key & key) const
    {
        return const_cast<BPTree *>(this)->upper_bound(key);
    };

    // 'at' method throws std::out_of_range if there is no such key
    Value & at(const Key & key)
    {
        const auto it = find(key);
        if (it == end()) {
            throw std::out_of_range("No such key");
        }
        else {
            return it->second;
        }
    };
    const Value & at(const Key & key) const
    {
        const auto it = find(key);
        if (it == cend()) {
            throw std::out_of_range("No such key");
        }
        else {
            return it->second;
        }
    };

    // '[]' operator inserts a new element if there is no such key
    Value & operator[](const Key & key)
    {
        return insert(key, Value()).first->second;
    };
    Value & operator[](Key && key)
    {
        return insert(std::move(key), Value()).first->second;
    };

    std::pair<iterator, bool> insert(const Key & key, const Value & value)
    {
        return insert(std::make_pair(key, value));
    }; // NB: a digression from std::map
    std::pair<iterator, bool> insert(const Key & key, Value && value)
    {
        return insert(std::make_pair(key, std::move(value)));
    }; // NB: a digression from std::map
    std::pair<iterator, bool> insert(Key && key, Value && value)
    {
        return insert(std::make_pair(std::move(key), std::move(value)));
    }; // NB: a digression from std::map

    void insert(std::initializer_list<value_type> values)
    {
        for (const value_type value : values) {
            insert(value);
        }
    }

    template <class InputIt>
    void insert(InputIt first, InputIt last)
    {
        for (auto it = first; it != last; it++) {
            insert(*it);
        }
    }

    iterator erase(const_iterator it)
    {
        const auto [new_root, leaf, index, inserted] = m_root->erase(m_root, it->first);
        m_root = new_root;
        return iterator(leaf, index);
    };

    iterator erase(const_iterator begin, const_iterator end)
    {
        auto key = end->first;
        auto it = begin;
        while (it->first != key) {
            it = erase(it);
        }
        return iterator(*const_cast<Leaf *>(it.m_current), it.m_index);
    };

    size_type erase(const Key & key)
    {
        const auto [new_root, leaf, index, inserted] = m_root->erase(m_root, key);
        m_root = new_root;
        return inserted;
    };

    iterator find(const Key & key)
    {
        const auto [leaf, index] = m_root->lower_bound(key);
        if (leaf[index].first == key) {
            return {leaf, index};
        }
        else {
            return end();
        }
    }

    const_iterator find(const Key & key) const
    {
        const auto [leaf, index] = m_root->lower_bound(key);
        if (leaf[index].first == key) {
            return {leaf, index};
        }
        else {
            return cend();
        }
    }

    std::ostream & print(std::ostream & strm) const
    {
        return m_root->print(strm);
    }

    friend std::ostream & operator<<(std::ostream & strm, const BPTree & tree)
    {
        return tree.print(strm);
    }

private:
    Node * m_root;
    Leaf & m_first;

    std::pair<iterator, bool> insert(std::pair<Key, Value> && value)
    {
        const auto [new_root, leaf, index, inserted] = m_root->insert(m_root, std::move(value));
        m_root = new_root;
        return {iterator(leaf, index), inserted};
    }

    std::pair<iterator, bool> insert(const std::pair<Key, Value> & value)
    {
        const auto [new_root, leaf, index, inserted] = m_root->insert(m_root, value);
        m_root = new_root;
        return {iterator(leaf, index), inserted};
    }
};
