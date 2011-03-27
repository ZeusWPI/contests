/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Pieter
 */
public class Main {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());
        for(int i = 0; i < n; i++) {
            int result = run(reader);
            if(result == 1440) System.out.println("NOOIT");
            else System.out.println(result);
        }
    }

    public static int run(BufferedReader reader) throws IOException {
        int n = Integer.parseInt(reader.readLine());
        List<Bus> busses = new ArrayList<Bus>(n);
        Set<Integer> places = new HashSet<Integer>();
        for(int i = 0; i < n; i++) {
            String line = reader.readLine();
            String[] busInfo = line.split(" ");
            Bus bus = new Bus(i, Arrays.copyOfRange(busInfo, 1, busInfo.length));
            places.addAll(bus.route);
            busses.add(bus);
        }

        int t = 0;
        while(t < 1440) {
            // find all with equal location
            for(Integer place : places) {
                Set<Integer> info = new HashSet<Integer>();
                List<Bus> bussesAtPlace = new ArrayList<Bus>();
                for(Bus bus : busses) {
                    if(bus.positionForTime(t).equals(place)) {

                  // System.out.println("bus at " + bus.positionForTime(t));
                        bussesAtPlace.add(bus);
                        info.addAll(bus.info);
                    }
                }

                for(Bus bus : bussesAtPlace) {
                    bus.info = info;
                }
            }

            boolean done = true;
            int i = 0;
            while(done && i < busses.size()) {
                if(busses.get(i).info.size() != n) done = false;
                i++;
            }
            if(done == true) return t;

            t++;
        }
        return t;
    }

    public static class Bus {
        public Set<Integer> info;
        public List<Integer> route;

        public Bus(Integer i, String[] inputRoute) {
            info = new HashSet<Integer>();
            info.add(i);

            route = new ArrayList<Integer>();
            for(int j = 0; j < inputRoute.length; j++) {
                route.add(Integer.parseInt(inputRoute[j]));
            }
        }

        public Integer positionForTime(int t) {
            return route.get(t % route.size());
        }
    }
}
