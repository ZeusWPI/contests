import java.io.*;
import java.util.*;

/**
 * Created by gertjandemulder
 */
public class Main {

    public static void main(String[] args) {
        userInterface();
    }

    public static void userInterface() {
        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

            int aantalTests = Integer.parseInt(br.readLine());
            do {
                    String[] levelsAndElevators = br.readLine().split(" ");
                int levels = Integer.parseInt(levelsAndElevators[0]);
                int elevators = Integer.parseInt(levelsAndElevators[1]);
                Graph g = new Graph(levels);

                for(int i = 0; i < elevators;i++)
                {
                    String[] elevatorDescription = br.readLine().split(" ");

                    int OV = Integer.parseInt(elevatorDescription[0]);
                    int BV = Integer.parseInt(elevatorDescription[1]);
                    int SG = Integer.parseInt(elevatorDescription[2]);

                    for(int j = OV; j<BV;j+=SG) {
                        int dest =j+SG;
                        g.addDuplexEdge(j,dest,i+1);
                    }
                }

                String[] route = br.readLine().split(" ");
                int start = Integer.parseInt(route[0]);
                int end = Integer.parseInt(route[1]);

                Radlift.BFS(g,start,end);

                --aantalTests;
            } while (aantalTests != 0);


        } catch (Exception ioe) {
            System.out.println(ioe);
        }
    }
}

class Radlift {
    static void BFS(Graph g, int source, int destination) {
        BreadthFirstSearch bfs = new BreadthFirstSearch(g);
        bfs.algorithm(source, destination);
        String line="";

        for(int i=bfs.trajectList.size()-1;i>=0;i--) {
            line+=bfs.trajectList.get(i);
        }

        System.out.println(line);
    }
}

class BreadthFirstSearch {
    Queue<Vertex> Q;
    ArrayList<RouteCombination> trajectList;
    Graph g;

    public BreadthFirstSearch(Graph g) {
        this.Q = new LinkedList<Vertex>();
        this.g = g;
        this.trajectList = new ArrayList<RouteCombination>();
    }

    void algorithm( int source, int destination) {
        Q.add(g.getVertex(source));
        Vertex v = null;
        while(!Q.isEmpty()) {

            v = Q.poll();
            v.setVertexState(Vertex.state.DISCOVERED);

            Map<Vertex,Edge> adj=  g.getAdjacentVertices(v);

            for(Map.Entry<Vertex,Edge> entry:adj.entrySet()) {
                Vertex key = entry.getKey();
                Edge val = entry.getValue();

                if(key.getVertexState()== Vertex.state.DISCOVERED){
                    int posDist = v.getDistance();
                    posDist+=val.getStep();
                    if(posDist<key.getDistance()) {

                        key.setPre(v);
                        key.setDistance(posDist);
                        key.setUsedEdge(val);
                    }
                }

                if(key.getVertexState()== Vertex.state.UNKNOWN) {
                    key.setVertexState(Vertex.state.DISCOVERED);
                    key.setPre(v);
                    int currDistance = key.getDistance();
                    currDistance+=val.getStep();
                    key.setDistance(currDistance);
                    key.setUsedEdge(val);
                    Q.add(key);
                }
            }
            v.setVertexState(Vertex.state.FINISHED);
        }
        Vertex d = g.getVertex(destination);
        this.backtrackPath(d);
    }

    void backtrackPath(Vertex destination) {
        if(destination!=null) {
            Edge e = destination.getUsedEdge();
            if(e!=null) {

                int elev = destination.getUsedEdge().getTrack();
                int cabine = -1;
                if(destination.getV()<destination.getPre().getV()){
                    //down
                     cabine=2*elev;

                }else {
                    //up
                    cabine=2*elev-1;
                }

                if(this.trajectList.size()>0) {
                    RouteCombination lastElement = this.trajectList.get(this.trajectList.size()-1);
                    if(lastElement.cabine!=cabine) this.trajectList.add(new RouteCombination(cabine, destination.getV()));
                }else this.trajectList.add(new RouteCombination(cabine, destination.getV()));
            }
            backtrackPath(destination.getPre());
        }
    }
}

class RouteCombination {
   public int level, cabine;

    public RouteCombination(int cabine, int level) {
        this.cabine = cabine;
        this.level = level;
    }

    public String toString() {
        return "("+cabine+","+level+")";
    }
}
class Graph {
    private int v;
    Map<Vertex,ArrayList<Edge>> adjMap;

    public Graph(int v) {
        this.adjMap = new HashMap<Vertex, ArrayList<Edge>>();
        for(int i=0; i<=v;i++) adjMap.put(new Vertex(i),new ArrayList<Edge>());

    }


    void addEdge(int v, Edge e) {
        adjMap.get(new Vertex(v)).add(e);
    }
    void addDuplexEdge(int a, int b, int track) {
        Vertex vA = this.getVertex(a);
        Vertex vB = this.getVertex(b);

        Edge e = new Edge(a,Math.abs((a-b)));
        e.setTrack(track);
        adjMap.get(vB).add(e);

        e = new Edge(b,Math.abs((a-b)));
        e.setTrack(track);
        adjMap.get(vA).add(e);


    }

    Map<Vertex,Edge> getAdjacentVertices(Vertex target) {
        ArrayList<Vertex> adjList = new ArrayList<Vertex>();
        Map<Vertex,Edge> adjMap = new HashMap<Vertex, Edge>();
        for(Edge e: this.adjMap.get(target)) {
            adjList.add(this.getVertex(e.getDestination()));
            adjMap.put(this.getVertex(e.getDestination()),e);
        }
        return adjMap;
    }

    //log(n+m)
    List<Integer> inEdges(int v) {
        List<Integer> edges = new ArrayList<Integer>();
        for (int j = 0; j < this.v; j++)
            if (adjMap.containsKey(new Vertex(v))){
                edges.add(j);
            }
        return edges;
    }

    ArrayList<Edge> outEdges(int v) {
        Vertex vi = new Vertex(v);
        ArrayList<Edge> neighbours = new ArrayList<Edge>();


        return this.adjMap.get(new Vertex(v));
    }



    public Vertex getVertex(int v) {
        for(Vertex k:this.adjMap.keySet()) if(k.equals(new Vertex(v))) return k;
        return null;
    }
    public int getSize() {
        return v;
    }
}
class Vertex  {
    public enum state {UNKNOWN, DISCOVERED, FINISHED};
    private Vertex pre;
    private int v;
    private Edge usedEdge;

    private state vertexState;
    private int distance;

    public Vertex(int v) {
        this.v = v;
        this.distance = 0;
        this.vertexState = state.UNKNOWN;
        this.pre = null;
        this.usedEdge=null;

    }

    public void setPre(Vertex pre) {
        this.pre = pre;
    }

    public Vertex getPre() {
        return pre;
    }

    public Edge getUsedEdge() {
        return usedEdge;
    }

    public void setUsedEdge(Edge usedEdge) {
        this.usedEdge = usedEdge;
    }

    public int getV() {
        return v;
    }

    public void setVertexState(state vertexState) {
        this.vertexState = vertexState;
    }

    public state getVertexState() {
        return vertexState;
    }

    public void setDistance(int d) {
        this.distance =d;
    }

    public Integer getDistance() {
        return this.distance;
    }

    /**
     * Overrides
     */
    public int hashCode() {

        return this.v;
    }

    public String toString() {
        return "Vertex{" +
          "v=" + v +
          ", distance=" + distance +
          '}';
    }

    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Vertex other = (Vertex) obj;
        if (v != other.v)
            return false;
        return true;
    }
}

class Edge {
    private int destination;
    private int step;
    private int track;

    public Edge(int destination,int step) {
        this.destination = destination;
        this.step = step;

    }

    public void setTrack(int track) {
        this.track = track;
    }

    public int getTrack() {
        return track;
    }

    public int getStep() {

        return step;
    }

    public int getDestination() {
        return destination;
    }

    public String toString() {
        return "Edge{" +
          "destination=" + destination +
          ", step=" + step +
          '}';
    }
}
