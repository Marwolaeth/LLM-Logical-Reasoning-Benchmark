library(dplyr)
library(reactable)
library(tidyr)
library(htmltools)

# Предполагаем, что ваш исходный набор данных называется 'data_results'
# data_results <- read.csv("your_data.csv") # Загрузите ваши данные

# --- Шаг 1: Агрегация данных ---

# 1. Посчитать общее количество ответов для каждого вопроса (premise_id)
# 2. Посчитать количество каждого варианта ответа (a, b, c)
# 3. Вычислить процентное соотношение

aggregated_data <- samples_df %>%
    mutate(premise_id = readr::parse_integer(id)) |>
    # Если вы хотите фильтровать перед агрегацией, используйте:
    # filter(model == "phi4", prompt == "formal") %>% 
    
    group_by(premise_id, premise, target) %>%
    summarise(
        total_runs = n(),
        # Подсчет количества ответов 'a', 'b', 'c'
        a_count = sum(result == "a"),
        b_count = sum(result == "b"),
        c_count = sum(result == "c"),
        .groups = 'drop'
    ) %>%
    mutate(
        # Вычисление процентов
        a_pct = (a_count / total_runs) * 100,
        b_pct = (b_count / total_runs) * 100,
        c_pct = (c_count / total_runs) * 100
    ) %>%
    select(premise_id, premise, target, total_runs, a_pct, b_pct, c_pct)


# --- Шаг 2: Кастомная функция для создания ячейки с диаграммой ---

# Функция, которая создает HTML-элемент для столбчатой диаграммы
# с условным форматированием и отображением процента.
create_html_bar_cell <- function(value, target_answer, current_answer_letter) {
    # 1. Определяем цвет
    is_correct <- target_answer == current_answer_letter
    bar_color <- if (is_correct) "#4CAF50" else "#F44336" # Green or Red
    text_color <- if (is_correct) "#1B5E20" else "#B71C1C" # Darker Green or Darker Red
    
    # 2. Форматируем значение
    percentage_text <- paste0(round(value, 1), "%")
    
    # 3. Создаем HTML-структуру
    htmltools::tagList(
        tags$div(style = list(display = "flex", alignItems = "center", width = "100%"),
                 # Столбец (бар)
                 tags$div(
                     style = list(
                         width = paste0(value, "%"),
                         height = "12px",
                         backgroundColor = bar_color,
                         borderRadius = "2px",
                         marginRight = "5px",
                         transition = "width 0.3s"
                     )
                 ),
                 # Текст процента
                 tags$span(
                     style = list(
                         color = text_color,
                         fontSize = "12px",
                         fontWeight = "bold",
                         minWidth = "40px" # Фиксированная ширина для выравнивания
                     ),
                     percentage_text
                 )
        )
    )
}

# Функция-обертка для colDef
create_bar_col_html <- function(col_name, answer_letter) {
    reactable::colDef(
        name = paste0("Вариант ", toupper(answer_letter), " (%)"),
        cell = function(value, index) {
            # Получаем правильный ответ (target) для текущей строки
            target <- aggregated_data$target[index] 
            
            # Генерируем HTML
            create_html_bar_cell(
                value = value,
                target_answer = target,
                current_answer_letter = answer_letter
            )
        },
        # Ширина колонки
        width = 180
    )
}


# --- Шаг 3: Создание таблицы Reactable ---

reactable(
    aggregated_data,
    filterable = TRUE, 
    searchable = TRUE, 
    defaultPageSize = 10,
    columns = list(
        premise_id = colDef(name = "ID", width = 50),
        premise = colDef(name = "Посылка", minWidth = 250),
        target = colDef(name = "Цель", width = 70, align = "center", style = list(fontWeight = "bold")),
        total_runs = colDef(show = FALSE), 
        
        # Применяем новую HTML-функцию
        a_pct = create_bar_col_html("a_pct", "a"),
        b_pct = create_bar_col_html("b_pct", "b"),
        c_pct = create_bar_col_html("c_pct", "c")
    )
)

#####
library(dplyr)
library(reactable)
library(htmltools)

# --- Функция для создания динамической колонки с агрегацией и визуализацией ---

create_dynamic_bar_col <- function(answer_letter) {
    
    # 1. JavaScript Aggregation Function (Расчет процента)
    # Возвращает число от 0 до 100
    js_agg <- JS(
        paste0("function(values, rows) {
      if (rows.length === 0) return 0;
      let count = 0;
      // 'rows' содержит все сырые данные для текущей группы
      rows.forEach(function(row) {
        if (row['result'] === '", answer_letter, "') {
          count++;
        }
      });
      return (count / rows.length) * 100;
    }")
    )
    
    # 2. JavaScript Aggregated Renderer (Отображение HTML-диаграммы)
    # Принимает рассчитанный процент (value) и данные группы (rows),
    # и возвращает HTML-строку.
    js_aggregated_renderer <- JS(
        paste0("function(value, rows) {
      if (rows.length === 0) return '';
      
      // Получаем правильный ответ (target) из первой строки группы
      const targetAnswer = rows[0]['target'];
      const currentLetter = '", answer_letter, "'; 
      
      // Логика условного форматирования
      const isCorrect = targetAnswer === currentLetter;
      const barColor = isCorrect ? '#4CAF50' : '#F44336'; // Green or Red
      const textColor = isCorrect ? '#1B5E20' : '#B71C1C'; // Darker Green or Darker Red
      
      const percentageText = value.toFixed(1) + '%';
      const barWidth = value + '%';

      // Конструируем HTML-строку
      return '<div style=\"display: flex; align-items: center; width: 100%;\">' +
               '<div style=\"width: ' + barWidth + '; height: 12px; background-color: ' + barColor + '; border-radius: 2px; margin-right: 5px; transition: width 0.3s;\"></div>' +
               '<span style=\"color: ' + textColor + '; font-size: 12px; font-weight: bold; min-width: 40px;\">' + percentageText + '</span>' +
             '</div>';
    }")
    )
    
    # 3. Возвращаем colDef
    reactable::colDef(
        name = paste0("Вариант ", toupper(answer_letter), " (%)"),
        aggregate = js_agg, # JS для расчета
        aggregated = js_aggregated_renderer, # JS для отображения
        width = 180
    )
}


# --- Шаг 3: Создание Reactable с Группировкой и Фильтрацией ---

data_results <- samples_df |>
    mutate(premise_id = readr::parse_integer(id)) |>
    select(premise_id, target, result, score, model:premise)

# Предполагаем, что 'data_results' - это ваш сырой набор данных
reactable(
    data_results,
    
    # 1. Группировка: Агрегируем по модели, инструкции и вопросу
    groupBy = c("model", "prompt", "premise_id", "premise", "target"),
    
    # 2. Включаем фильтрацию
    filterable = TRUE, 
    searchable = TRUE, 
    defaultPageSize = 10,
    
    columns = list(
        # Колонки, по которым мы группируем:
        model = colDef(name = "Модель", aggregate = "unique"),
        prompt = colDef(name = "Инструкция", aggregate = "unique"),
        premise_id = colDef(name = "ID", width = 50, aggregate = "unique"),
        premise = colDef(name = "Посылка", minWidth = 200, aggregate = "unique"),
        target = colDef(name = "Цель", width = 70, align = "center", style = list(fontWeight = "bold"), aggregate = "unique"),
        
        # Колонки для агрегации:
        result = colDef(show = FALSE), 
        
        # Колонки для визуализации агрегированных процентов:
        a_pct = create_dynamic_bar_col("a"),
        b_pct = create_dynamic_bar_col("b"),
        c_pct = create_dynamic_bar_col("c")
    )
)
