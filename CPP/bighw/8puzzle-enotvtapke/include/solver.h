#pragma once

#include "board.h"

#include <array>
#include <iostream>
#include <memory>
#include <unordered_set>

class Solver
{
    class Node
    {
        friend class Solver;
        friend struct std::hash<Node>;

    public:
        Node(Board && board, std::size_t distance, int move)
            : m_distance(distance + 1)
            , m_board(std::move(board))
            , m_move(move)
        {
        }
        Node(const Board & board)
            : m_board(board)
        {
        }
        Node(Board && board)
            : m_board(std::move(board))
        {
        }
        std::string to_string() const
        {
            return m_board.to_string();
        }

        friend bool operator>(const Node & lhs, const Node & rhs)
        {
            return lhs.f() > rhs.f();
        }
        friend bool operator<(const Node & lhs, const Node & rhs)
        {
            return lhs.f() < rhs.f();
        }
        friend bool operator==(const Node & lhs, const Node & rhs)
        {
            return lhs.m_board == rhs.m_board;
        }

    private:
        std::size_t m_distance = 0;
        Board m_board;
        int m_move = -1;

        unsigned g() const
        {
            return m_distance;
        }
        unsigned h() const
        {
            return m_board.manhattan();
        }
        unsigned f() const
        {
            return g() + h();
        }
    };
    friend struct std::hash<Solver::Node>;

    class Solution
    {
        friend class Solver;

    public:
        Solution(std::size_t num_of_moves)
            : m_moves(num_of_moves)
        {
        }

        std::size_t moves() const { return m_moves.empty() ? 0 : m_moves.size() - 1; }

        using const_iterator = std::vector<Board>::const_iterator;

        const_iterator begin() const { return m_moves.begin(); }

        const_iterator end() const { return m_moves.end(); }

    private:
        std::vector<Board> m_moves;
    };

public:
    static Solution solve(const Board & initial);
    static Solution reconstruct_path(Node end, const std::unordered_set<Node> & used);
};

namespace std {
template <>
struct hash<Solver::Node>
{
    hash<Board> board_hash{};
    std::size_t operator()(const Solver::Node & node) const
    {
        return board_hash(node.m_board);
    }
};
} // namespace std