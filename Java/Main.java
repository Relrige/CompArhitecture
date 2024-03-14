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
        mergeSort(occurrences);

        // Виводимо відсортовані результати
        for (Map.Entry<Integer, Integer> entry : occurrences) {
            System.out.println(entry.getKey() + " " + entry.getValue());
        }
    }

    private static void merge(List<Map.Entry<Integer, Integer>> list, List<Map.Entry<Integer, Integer>> left, List<Map.Entry<Integer, Integer>> right) {
        int i = 0, j = 0, k = 0;
        while (i < left.size() && j < right.size()) {
            if (left.get(i).getKey() <= right.get(j).getKey()) {
                list.set(k++, left.get(i++));
            } else {
                list.set(k++, right.get(j++));
            }
        }
        while (i < left.size()) {
            list.set(k++, left.get(i++));
        }
        while (j < right.size()) {
            list.set(k++, right.get(j++));
        }
    }

    // Рекурсивна реалізація злиття двох списків
    private static void mergeSort(List<Map.Entry<Integer, Integer>> list) {
        if (list.size() > 1) {
            int mid = list.size() / 2;
            List<Map.Entry<Integer, Integer>> left = new ArrayList<>(list.subList(0, mid));
            List<Map.Entry<Integer, Integer>> right = new ArrayList<>(list.subList(mid, list.size()));

            mergeSort(left);
            mergeSort(right);

            merge(list, left, right);
        }
    }
}
