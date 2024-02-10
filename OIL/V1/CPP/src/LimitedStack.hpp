/*
** ZIPLEIX PROJECT, 2024
** OIL [WSL : Ubuntu]
** File description:
** LimitedStack
*/

#pragma once

#include <stack>

template <typename T>
class LimitedStack {
    public:
        LimitedStack(size_t maxSize) : maxSize(maxSize) {}

        void push(const T& item) {
            if (stack.size() >= maxSize) {
                throw std::out_of_range("Stack overflow: Attempt to push an item to a full stack.");
            }
            stack.push(item);
        }

        void pop() {
            if (stack.empty()) {
                throw std::out_of_range("Stack underflow: Attempt to pop an item from an empty stack.");
            }
            stack.pop();
        }

        T& top() {
            if (stack.empty()) {
                throw std::out_of_range("Stack underflow: Attempt to access the top of an empty stack.");
            }
            return stack.top();
        }

        const T& top() const {
            if (stack.empty()) {
                throw std::out_of_range("Stack underflow: Attempt to access the top of an empty stack.");
            }
            return stack.top();
        }

        bool empty() const {
            return stack.empty();
        }

        size_t size() const {
            return stack.size();
        }

    protected:
    private:
        std::stack<T> stack;
        size_t maxSize;
};
