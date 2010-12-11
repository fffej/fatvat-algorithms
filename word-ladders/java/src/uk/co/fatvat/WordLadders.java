package uk.co.fatvat;

import java.io.*;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class WordLadders {

    private final static String wordPath =  "/usr/share/dict/british-english";

    private void dijkstra() {
        int pathsAlreadyDetermined;
        int remainingVertices;

        int d[]; // best estimates
        int pi; // predecessors


    }

    private static Set<String> readWords () throws Exception {
        final BufferedReader bis = new BufferedReader(new FileReader(wordPath));
        final Set<String> words = new HashSet<String>();

        String line;
        while (null != (line = bis.readLine())) {
            line = line.toLowerCase().trim();
            if (isValidWord(line)) {
                words.add(line);
            }
        }

        return words;
    }

    private static boolean isValidWord(String line) {
        final String validChars = "abcdefghijklmnopqrstuvxyz";
        boolean isValid = true;
        for (char c : line.toCharArray()) {
            if (-1 == validChars.indexOf(c)) {
                isValid = false;
                break;
            }
        }

        return isValid;
    }

    public static void main(String[] args) throws Exception {
        System.out.println(readWords().size());

        if (args.length != 2) {
            System.out.println("Supply two arguments, a start word and an end word");
            System.exit(-1);
        }

        final String start = args[0];
        final String end = args[1];

    }
}
