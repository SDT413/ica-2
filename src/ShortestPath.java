import java.util.*;

public class ShortestPath {

    public static int shortestPath(Map<String, List<String>> graph, String start, String end) {
        Queue<String> queue = new LinkedList<>();
        Map<String, Integer> distances = new HashMap<>();
        queue.offer(start);
        distances.put(start, 0);
        while (!queue.isEmpty()) {
            String curr = queue.poll();
            for (String neighbor : graph.get(curr)) {
                if (!distances.containsKey(neighbor)) {
                    queue.offer(neighbor);
                    distances.put(neighbor, distances.get(curr) + 1);
                    if (neighbor.equals(end)) {
                        return distances.get(end);
                    }
                }
            }
        }
        return -1; // unreachable
    }

    public static void main(String[] args) {
        Map<String, List<String>> graph = new HashMap<>();
        graph.put("Prague", Arrays.asList("Brno", "Dresden", "Vienna"));
        graph.put("Brno", Arrays.asList("Olomouc", "Prague"));
        graph.put("Dresden", Arrays.asList("Berlin", "Prague"));
        graph.put("Vienna", Arrays.asList("Munich", "Prague"));
        graph.put("Olomouc", List.of("Brno"));
        graph.put("Berlin", List.of("Dresden"));
        graph.put("Munich", List.of("Vienna"));
        int shortest = shortestPath(graph, "Munich", "Prague");
        System.out.println(shortest); // output: 2
    }
}