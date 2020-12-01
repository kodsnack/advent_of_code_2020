#pragma once
#include <deque>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace westerstrom
{
	template <typename V> struct Graph
	{
		using vertex_t = V;
		std::unordered_map<V, std::vector<V>> edges;
	};

	template <typename Graph, typename VisitFunc, typename NextFunc>
	auto bfs(Graph& g, typename Graph::vertex_t start, VisitFunc&& v, NextFunc&& nextFn)
	{
		std::unordered_set<typename Graph::vertex_t> visited;
		int n = 0;
		std::deque<std::pair<typename Graph::vertex_t, int>> q;
		q.push_front({start, 0});
		while(!q.empty())
		{
			auto [c, n] = q.front();
			q.pop_front();

			if(!visited.contains(c))
			{
				if(v(c, n))
				{
					return make_pair(c, n);
				}
				visited.insert(c);

				for(auto next : nextFn(c, n))
				{
					q.push_front({next, n + 1});
				}
			}
		}
	}
} // namespace westerstrom
