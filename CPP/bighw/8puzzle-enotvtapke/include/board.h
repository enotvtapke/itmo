#pragma once

#include <optional>
#include <string>
#include <vector>

enum class moves
{
    left = 0,
    right = 2,
    up = 1,
    down = 3
};

class Board
{
    friend class Solver;
    std::size_t m_size{};
    std::vector<unsigned> m_data;

public:
    friend struct std::hash<Board>;
    static Board create_goal(unsigned size);

    static Board create_random(unsigned size);

    Board() = default;

    explicit Board(const std::vector<std::vector<unsigned>> & data);
    explicit Board(const std::vector<unsigned> & data, std::size_t size);
    explicit Board(std::vector<unsigned> && data, std::size_t size);

    std::size_t size() const;

    bool is_goal() const;

    unsigned hamming() const;

    unsigned manhattan() const;

    std::string to_string() const;

    bool is_solvable() const;

    std::optional<Board> move(moves direction) const;

    const unsigned * operator[](const std::size_t i) const
    {
        return &m_data[m_size * i];
    }

    friend bool operator==(const Board & lhs, const Board & rhs)
    {
        return lhs.m_data == rhs.m_data;
    }

    friend bool operator<(const Board & lhs, const Board & rhs)
    {
        return lhs.m_data < rhs.m_data;
    }

    friend bool operator!=(const Board & lhs, const Board & rhs)
    {
        return !(lhs == rhs);
    }

    friend std::ostream & operator<<(std::ostream & out, const Board & board)
    {
        return out << board.to_string();
    }

private:
    unsigned manhattan(int i) const;
    std::size_t inversions() const;
    std::size_t find_blank() const;
};

namespace std {
template <>
struct hash<Board>
{
    std::size_t operator()(const Board & board) const
    {
        std::size_t seed = board.m_data.size();
        for (const unsigned i : board.m_data) {
            seed ^= i + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        }
        return seed;
    }
};
} // namespace std