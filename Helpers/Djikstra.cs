using System.Drawing;

namespace Helpers
{
    // Copied from last year (and tweaked)
    public class Djikstra
    {
        public static int CalculateShortestPath(int[,] heights, Point starting, Point ending, Func<Point, Point, int> costFunc)
        {
            var width = heights.GetLength(0);
            var height = heights.GetLength(1);
            var tentativeDists = new int?[width, height];
            var visited = new bool[width, height];

            visited[starting.X, starting.Y] = true;           // Starting position
            //heights[starting.X, starting.Y] = 0;
            tentativeDists[starting.X, starting.Y] = 0;
            var pq = new PriorityQueue<Point, int>();

            var currentNode = starting;
            while (true)
            {
                foreach (var n in Grid.GetNeighbourCoordinates(tentativeDists, currentNode))
                {
                    if (visited[n.X, n.Y]) continue;

                    if (tentativeDists[n.X, n.Y] == null)
                    {
                        var cost = tentativeDists[currentNode.X, currentNode.Y] + costFunc(currentNode, n);
                        tentativeDists[n.X, n.Y] = cost;
                        pq.Enqueue(n, cost.Value);
                        continue;
                    }

                    var costFromThisNode = tentativeDists[currentNode.X, currentNode.Y] + costFunc(currentNode, n);
                    if (costFromThisNode < tentativeDists[n.X, n.Y])
                    {
                        tentativeDists[n.X, n.Y] = costFromThisNode;
                        pq.Enqueue(n, costFromThisNode.Value);
                    }
                }

                visited[currentNode.X, currentNode.Y] = true;

                if (currentNode.X == ending.X && currentNode.Y == ending.Y)
                {
                    // This is the destination
                    var shortestDistance = tentativeDists[currentNode.X, currentNode.Y].Value;

                    return shortestDistance;
                }
                // else locate the smallest dist to go to next x,y
                currentNode = FindNextNode(pq);
            }
        }

        private static Point FindNextNode(PriorityQueue<Point, int> pq)
            => pq.Dequeue();

        private static int[,] Times(int[,] input, int times)
        {
            var width = input.GetLength(0);
            var height = input.GetLength(1);

            var result = new int[width * times, height * times];

            for (int x = 0; x < width; x++)
                for (int y = 0; y < height; y++)
                    result[x, y] = input[x, y];

            for (int x = 0; x < width * times; x++)
            {
                for (int y = 0; y < height * times; y++)
                {
                    var equivX = x % width;
                    var equivY = y % height;

                    var xTileOffset = x / width;
                    var yTileOffset = y / height;

                    result[x, y] = result[equivX, equivY] + xTileOffset + yTileOffset;

                    // Wrap
                    if (result[x, y] > 9)
                        result[x, y] = result[x, y] % 10 + 1;
                }
            }

            return result;
        }
    }
}
