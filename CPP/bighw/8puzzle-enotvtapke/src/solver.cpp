#include "solver.h"

#include <algorithm>
#include <queue>

Solver::Solution Solver::reconstruct_path(Node end, const std::unordered_set<Node> & used)
{
    std::size_t i = end.m_distance;
    Solution sol(i + 1);
    while (i > 0) {
        sol.m_moves[i] = end.m_board;
        i--;
        moves inv_move = moves((end.m_move + 2) % 4);
        auto it = used.find(Node(end.m_board.move(inv_move).value()));
        end = *it;
    }
    sol.m_moves[0] = end.m_board;
    return sol;
}

Solver::Solution Solver::solve(const Board & board)
{
    if (board.is_goal()) {
        Solution sol(1);
        sol.m_moves[0] = board;
        return sol;
    }
    if (!board.is_solvable()) {
        return Solution(0);
    }
    std::priority_queue<Node, std::vector<Node>, std::greater<Node>> queue;
    queue.push(Node(board));
    std::unordered_set<Node> used;
    used.insert(queue.top());
    while (!queue.empty()) {
        const Node current = (queue.top());
        if (current.m_board.is_goal()) {
            return reconstruct_path(current, used);
        }
        queue.pop();
        used.insert(current);
        for (int move = 0; move <= 3; move++) {
            if ((current.m_move == (move + 2) % 4)) {
                continue;
            }
            std::optional<Board> new_board = current.m_board.move(moves(move));
            if (!new_board) {
                continue;
            }
            Node new_node(std::move(new_board.value()), current.m_distance, move);
            auto it = used.find(new_node);
            if (it == used.end()) {
                queue.push(std::move(new_node));
            }
            else if (new_node.m_distance < it->m_distance) {
                queue.push(std::move(new_node));
                used.erase(it);
            }
        }
    }
    return Solution(0);
}