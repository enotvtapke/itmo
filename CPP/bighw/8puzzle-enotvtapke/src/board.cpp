#include "board.h"

#include <algorithm>
#include <cstdlib>
#include <numeric>
#include <random>

Board Board::create_goal(const unsigned size)
{
    std::vector<unsigned> data(size * size);
    std::iota(data.begin(), data.end(), 1);
    data[size * size - 1] = 0;
    return Board(std::move(data), size);
}

static std::random_device randomDevice{};

Board Board::create_random(const unsigned size)
{
    std::vector<unsigned> data(size * size);
    std::iota(data.begin(), data.end(), 0);
    std::shuffle(data.begin(), data.end(), std::mt19937(randomDevice()));
    return Board(std::move(data), size);
}

Board::Board(const std::vector<unsigned> & data, std::size_t size)
    : m_size(size)
    , m_data(data)
{
}

Board::Board(std::vector<unsigned> && data, std::size_t size)
    : m_size(size)
    , m_data(std::move(data))
{
}

Board::Board(const std::vector<std::vector<unsigned>> & data)
    : m_size(data.size())
    , m_data(m_size * m_size)
{
    for (size_t i = 0; i < m_size; i++) {
        for (size_t j = 0; j < m_size; j++) {
            m_data[i * m_size + j] = data[i][j];
        }
    }
}

std::size_t Board::size() const
{
    return m_size;
}

bool Board::is_goal() const
{
    for (size_t i = 0; i < m_size * m_size; i++) {
        if (m_data[i] != (i + 1) % (m_size * m_size)) {
            return false;
        }
    }
    return true;
}

unsigned Board::hamming() const
{
    std::size_t count = 0;
    for (size_t i = 0; i < m_size * m_size; i++) {
        if (m_data[i] != (i + 1) % (m_size * m_size)) {
            count++;
        }
    }
    return count;
}

unsigned Board::manhattan(int i) const
{
    int v;
    if (m_data[i] == 0) {
        return 0;
    }
    else {
        v = m_data[i] - 1;
    }
    int size = m_size;
    int j = i % size;
    i = i / size;
    return std::abs(v / size - i) + std::abs(v % size - j);
}

unsigned Board::manhattan() const
{
    unsigned distance = 0;
    for (size_t i = 0; i < m_size * m_size; i++) {
        distance += manhattan(i);
    }
    return distance;
}

std::string Board::to_string() const
{
    if (m_size == 0) {
        return "<empty>";
    }
    std::string out;
    for (size_t i = 0; i < m_size * m_size; i++) {
        out += std::to_string(m_data[i]);
        out += ' ';
        if (i % m_size == m_size - 1) {
            out += '\n';
        }
    }
    return out;
}

std::size_t Board::inversions() const
{
    std::size_t count = 0;
    for (size_t i = 0; i < m_size * m_size; i++) {
        for (size_t j = i + 1; j < m_size * m_size; j++) {
            if (m_data[i] != 0 && m_data[j] != 0 && m_data[i] > m_data[j]) {
                count++;
            }
        }
    }
    return count;
}

size_t Board::find_blank() const
{
    for (size_t i = 0; i < m_size * m_size; i++) {
        if (m_data[i] == 0) {
            return i;
        }
    }
    return 0;
}

bool Board::is_solvable() const
{
    if (m_size == 0 || m_size == 1) {
        return true;
    }
    std::size_t inv = inversions();
    if (m_size % 2 != 0 && inv % 2 == 0) {
        return true;
    }
    else if (m_size % 2 == 0) {
        std::size_t pos = find_blank();
        std::size_t blank_row = pos / m_size;
        if (((m_size - blank_row) % 2 == 0 && inv % 2 != 0) || ((m_size - blank_row) % 2 != 0 && inv % 2 == 0)) {
            return true;
        }
    }
    return false;
}

std::optional<Board> Board::move(moves direction) const
{
    std::size_t pos = find_blank();
    std::size_t blank_row = pos / m_size;
    std::size_t blank_col = pos % m_size;

    std::size_t new_row = blank_row;
    std::size_t new_col = blank_col;
    switch (direction) {
    case moves::up:
        if (blank_row == 0) {
            return std::nullopt;
        }
        new_row = blank_row - 1;
        break;
    case moves::down:
        if (blank_row == m_size - 1) {
            return std::nullopt;
        }
        new_row = blank_row + 1;
        break;
    case moves::left:
        if (blank_col == 0) {
            return std::nullopt;
        }
        new_col = blank_col - 1;
        break;
    case moves::right:
        if (blank_col == m_size - 1) {
            return std::nullopt;
        }
        new_col = blank_col + 1;
        break;
    }
    Board new_board = Board(this->m_data, m_size);
    new_board.m_data[pos] = new_board.m_data[new_row * m_size + new_col];
    new_board.m_data[new_row * m_size + new_col] = 0;
    return new_board;
}
