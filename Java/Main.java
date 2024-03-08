import java.util.*;

public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        // Зчитуємо підрядок, який шукаємо
        String substring = scanner.nextLine();

        // Зчитуємо рядки з stdin
        List<String> lines = new ArrayList<>();
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            if (line.isEmpty()) break;
            lines.add(line);
        }
        scanner.close();

        // Знаходимо входження підрядка в кожному рядку та зберігаємо результати
        List<Map.Entry<Integer, Integer>> occurrences = new ArrayList<>();
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            int lastIndex = 0;
            int count = 0;
            while ((lastIndex = line.indexOf(substring, lastIndex)) != -1) {
                count++;
                lastIndex += substring.length();
            }
            if (count > 0) {
                occurrences.add(new AbstractMap.SimpleEntry<>(count, i));
            }
        }


        // Виводимо відсортовані результати
        for (Map.Entry<Integer, Integer> entry : occurrences) {
            System.out.println(entry.getKey() + " " + entry.getValue());
        }
    }
}
