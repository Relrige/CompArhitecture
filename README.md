<h1>Author: Stas Kulakevych</h1>
Variant 1
Task Description
Прочитати з stdin N рядків до появи EOF (макс довжина рядка 255 символів, максимум 100 рядків), у масив з рядків. Рядки розділяються АБО послідовністю байтів 0x0D та 0x0A (CR LF), або одним символом - 0x0D чи 0x0A. Як зберігати рядки, неважливо (ASCIIZ або Pascal string або якось ще). Або не зберігати взагалі, якщо робити наступний крок line-by-line. Знайти всі входження вказанного підрядка (1й аргумент командного рядка) в кожному з рядків. Може бути більше одного входження на рядок. Входження не мають перетинатися, наприклад кількість входжень підрядока "aa" в "aaa" = 1. Відсортувати знайдені результати алгоритмом merge sort по кількості входженнь (asc), та вивести в консоль (stdout) "<кількість входжень> <індекс рядка у текстовому файлі починаючи з 0>".
