#Voting_analysis
Скрипт R для аналізу результатів голосування народних депутатів.  
Необхідна версія **R: >=3.1.0**  
Необхідні бібліотеки: **sqldf**  

###Початок роботи
Завантажте файл voting_analysis.r у робочу директорію, після чого введіть команду source("voting_analysis.r")  
####Якщо ви хочете завантажити новий масив голосувань
В вашій робочий директорії має міститися файл з назвами файлів з початковими даними голосувань  у форматі, який створює цей скрипт:   
https://github.com/chesnolabs/voting
По замовчанню цей файл має назву **filenames.csv**.   
По рядках мають міститися назви таких файли:  
1. Файл з депутатськими ID.  
2. Файл з ID фракцій.  
3. Файл з персональними голосуваннями народних депутатів.  
4. Файл з голосуваннями фракцій.  
5. Файл з ID голосувань, їхніми назвами, часом і т.д.  
Звичайно, всі ці файли мають також міститися у робочій директорії.  
Після цього вам необхідні запустити функцію **votan.init()**. Якщо ви не змінювали назву файлу filenames.csv, запускайте функцію без жодних аргументів. Функція може працювати досить довго, проте запускати її слід один раз для кожного масиву голосувань.  
####Якщо ви працюєте з готовим масивом голосувань
Ви повинні мати у робочі директорії папку **votan**, яка містить всі дані голосувань у форматі .Rda. Запускайте функцію loadData безжодних аргументів.  
##Функції
####Порівняння голосувань двох депутатів - compare_deputies
compare_deputies (d1, d2 , startDate = NULL, endDate = NULL, law_like_votings = FALSE, absence_as_against = TRUE)  
**d1** - ID першого депутата  
**d2** - ID другого депутата  
**startDate**, **endDate** - перша та остання дата голосувань двох депутатів у форматі "dd.mm.yyyy". Якщо лишити параметри без значення, береться відповідно найперша та остання дата у масиві голосувань.   
**law_like_votings** - за значення TRUE до уваги беруться лише голосування за законопроекти (внесення до порядку денного, за основу, в цілому) та поправки. По замовчанню FALSE.  
**absence_as_against** - за значення TRUE голосування "Відсутній/Відсутня" вважається непідтримкою рішення. За значення FALSE голосування "Відсутній/Відсутня" виключаються з аналізу. По замовчанню TRUE.  
**РЕЗУЛЬТАТ: ** вектор із 4 числовими значеннями. *for_with* - кількість спільних голосувань "За" двох депутатів, *for_all* - загальна кількість голосів "За" депутата d1, *not_for_with* - кількість голосувань, в яких обидва депутати не підтримали рішення, *not_for_all* - загальна кількість голосувань, які не підтримав депутат d1.  
####Порівняння голосування депутатів із голосуванням фракції - compare_with_faction
compare_with_faction(deps = NULL, fid = NULL, startDate = NULL, endDate = NULL, law_like_votings = FALSE, absence_as_against = TRUE)  
**deps** - вектор із списком id депутатів, які порівнюються.  
**fid** - айді фракції, з якою порівнюються голосування депутатів  
**startDate**, **endDate** - перша та остання дата голосувань двох депутатів у форматі "dd.mm.yyyy". Якщо лишити параметри без значення, береться відповідно найперша та остання дата у масиві голосувань.   
**law_like_votings** - за значення TRUE до уваги беруться лише голосування за законопроекти (внесення до порядку денного, за основу, в цілому) та поправки. По замовчанню FALSE.  
**absence_as_against** - за значення TRUE голосування "Відсутній/Відсутня" вважається непідтримкою рішення. За значення FALSE голосування "Відсутній/Відсутня" виключаються з аналізу. По замовчанню TRUE.  
**РЕЗУЛЬТАТ: ** об’єкт класу data.frame із 5 стовбчиками. *deputy* - ініціали депутата;  *for_with* - кількість спільних голосувань "За" депутата і фракції, *for_all* - загальна кількість голосів "За" депутата , *not_for_with* - кількість голосувань, в яких депутат і фракція не підтримали рішення, *not_for_all* - загальна кількість голосувань, які не підтримав депутат.  
####Порівняння голосувань групи депутатів між собою - compare_shortlist
compare_shortlist (shortlist = NULL, startDate = NULL, endDate = NULL, law_like_votings = FALSE, absence_as_against = TRUE)  
**shortlist** - вектор із списком id депутатів, які порівнюються.  
**startDate**, **endDate** - перша та остання дата голосувань двох депутатів у форматі "dd.mm.yyyy". Якщо лишити параметри без значення, береться відповідно найперша та остання дата у масиві голосувань.  
**law_like_votings** - за значення TRUE до уваги беруться лише голосування за законопроекти (внесення до порядку денного, за основу, в цілому) та поправки. По замовчанню FALSE.  
**absence_as_against** - за значення TRUE голосування "Відсутній/Відсутня" вважається непідтримкою рішення. За значення FALSE голосування "Відсутній/Відсутня" виключаються з аналізу. По замовчанню TRUE.  
**РЕЗУЛЬТАТ:** об’єкт класу data.frame. Кількість стовбчиків та кількість рядків відповідає кількості депутатів у групі. Назвами рядків та стовбчиків є ініціали депутатів. Кожний елемент data.frame є класу list з першим та єдиним елементом - числовим вектором з 4 результатами порівняння голосувань двох депутатів.   *for_with* - кількість спільних голосувань "За" двох депутатів, *for_all* - загальна кількість голосів "За" депутата, чиє ім’я має рядок, *not_for_with* - кількість голосувань, в яких обидва депутати не підтримали рішення, *not_for_all* - загальна кількість голосувань, які не підтримав депутата, чиє ім’я має рядок.  
####Порівняння двох фракцій - compare_factions
compare_factions( f1 = NULL, f2 = NULL, startDate = NULL, endDate = NULL, law_like_votings = FALSE)  
**f1** - ID першої фракції  
**f2** - ID другої фракції  
**startDate**, **endDate** - перша та остання дата голосувань двох депутатів у форматі "dd.mm.yyyy". Якщо лишити параметри без значення, береться відповідно найперша та остання дата у масиві голосувань.   
**law_like_votings** - за значення TRUE до уваги беруться лише голосування за законопроекти (внесення до порядку денного, за основу, в цілому) та поправки. По замовчанню FALSE.  
**РЕЗУЛЬТАТ: ** вектор із 4 числовими значеннями. *for_with* - кількість спільних голосувань "За" двох фракцій, *for_all* - загальна кількість голосів "За" фракції f1, *not_for_with* - кількість голосувань, в яких обидвs фракції не підтримали рішення, *not_for_all* - загальна кількість голосувань, які не підтримала фракція f1.  
