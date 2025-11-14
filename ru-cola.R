### AI GENERATED ###
# Установка ellmer (если необходимо)
# install.packages("ellmer")

library(ellmer)
library(stringr) # Для удобной работы со строками

# --- Настройка базового LLM ----
# Предполагается, что у вас настроен ключ API (например, переменная среды OPENAI_API_KEY)

# Создаем базовый объект чата.
# Мы будем клонировать его для каждого агента, чтобы избежать загрязнения истории.
# В реальной реализации замените "gpt-4.1-nano" на нужную вам модель (например, "gpt-3.5-turbo").
# ВНИМАНИЕ: Для сложных задач, как COLA, рекомендуется использовать более мощные модели (GPT-4).
openrouter_key <- function() {
    list(Authorization = paste(
        'Bearer', Sys.getenv('OPENROUTER_API_KEY')
    ))
}

chat_base <- chat_openrouter(
    model = 'openai/gpt-oss-20b:free',
    credentials = openrouter_key
)

# Этап 1: Многомерный анализ текста (Эксперты) ----
## 1.1. Лингвистический эксперт ----
analyze_linguistic <- function(chat_base, tweet) {
    # Клонируем чат для нового, чистого взаимодействия
    chat <- chat_base$clone()
    
    prompt <- interpolate(
        "Вы лингвист. Точно и кратко объясните лингвистические элементы в сообщении и то, как эти элементы влияют на смысл, включая грамматическую структуру, время и склонение, риторические приёмы, лексический выбор и так далее. Больше ничего не делайте. Сообщение: {{tweet}}",
        tweet = tweet
    )
    
    return(chat$chat(prompt, echo = "none"))
}

## 1.2. Специалист по предметной области ----
analyze_domain <- function(chat_base, tweet, target, role = "social commentator") {
    chat <- chat_base$clone()
    
    prompt <- interpolate(
        "Вы {{role}}. Точно и кратко объясните ключевые элементы, содержащиеся в цитате, такие как персоны, события, партии, религии и т.д. Также объясните их связь с {{target}} (если существует). Больше ничего не делайте. Цитата: {{tweet}}",
        role = role,
        target = target,
        tweet = tweet
    )
    
    return(chat$chat(prompt, echo = "none"))
}

## 1.3. Ветеран социальных сетей ----
analyze_social_media <- function(chat_base, tweet) {
    chat <- chat_base$clone()
    
    prompt <- interpolate(
        "Вы активный пользователь социальных сетей и хорошо знакомы со способом выражения в интернете. Проанализируйте следующее сообщение, сосредоточившись на содержании, эмоциональном тоне, подразумеваемом смысле и так далее. Больше ничего не делайте. Сообщение: {{tweet}}",
        tweet = tweet
    )
    
    return(chat$chat(prompt, echo = "none"))
}

# Этап 2: Дебаты с улучшенным логическим выводом (Дебатеры) ----
# Функция для проведения дебатов по конкретной позиции
debate_stance <- function(chat_base, tweet, target, stance, analysis_results) {
    chat <- chat_base$clone()
    
    # Объединяем результаты анализа в одну строку для промпта
    LingResponse <- analysis_results$linguistic
    ExpertResponse <- analysis_results$domain
    UserResponse <- analysis_results$social_media
    
    prompt <- interpolate(
        "Твит: {{tweet}}. Лингвистический анализ:{{LingResponse}}. Анализ специалиста по предметной области:{{ExpertResponse}}. Анализ активного пользователя социальных сетей: {{UserResponse}}. Вы считаете, что позиция, стоящая за твитом, {{stance}} по отношению к {{target}}. Определите три главных доказательства из анализов, которые лучше всего поддерживают вашу точку зрения, и аргументируйте свою позицию.",
        tweet = tweet,
        target = target,
        stance = stance,
        LingResponse = LingResponse,
        ExpertResponse = ExpertResponse,
        UserResponse = UserResponse
    )
    
    return(chat$chat(prompt, echo = "none"))
}

# Этап 3: Заключение о позиции (Судья) ----
# 3. Судья
determine_stance <- function(chat_base, tweet, target, debate_results) {
    # Клонируем чат для нового, чистого взаимодействия
    chat <- chat_base$clone()
    
    # Аргументы дебатов
    FavorResponse <- debate_results$favor
    AgainstResponse <- debate_results$against
    NeutralResponse <- debate_results$neutral
    
    # 1. Определение схемы структурированного вывода
    # Используем type_enum для ограничения ответа только тремя допустимыми значениями
    stance_type <- type_enum(
        values = c('За', 'Против', 'Нейтрально'),
        description = "Окончательно определённая позиция автора по отношению к цели."
    )
    
    # 2. Подготовка промпта
    # Промпт упрощен, так как формат вывода теперь задается схемой, а не текстовой инструкцией.
    prompt <- interpolate(
        "На основе следующего твита и аргументов, представленных сторонниками позиций 'За', 'Против' и 'Нейтрально', определите наиболее точную позицию автора по отношению к цели '{{target}}'.
    
    Твит: {{tweet}}
    
    Аргументы 'За': {{FavorResponse}}
    Аргументы 'Против': {{AgainstResponse}}
    Аргументы 'Нейтрально': {{NeutralResponse}}
    
    Предоставьте окончательную позицию как одно значение в соответствии с требуемой схемой.",
        tweet = tweet,
        target = target,
        FavorResponse = FavorResponse,
        AgainstResponse = AgainstResponse,
        NeutralResponse = NeutralResponse
    )
    
    # 3. Использование chat_structured для получения гарантированно структурированного результата
    final_stance <- chat$chat_structured(
        prompt,
        type = stance_type
    )
    
    # Поскольку мы запросили single enum, final_stance будет вектором R с одним из значений: 
    # 'Favor', 'Against', или 'Neutral'. Дополнительная обработка не требуется.
    return(final_stance)
}

# Общая функция-обертка COLA ----
cola_single_detection <- function(chat_base, tweet, target, domain_role = "social commentator") {
    
    cat(paste0("--- Анализ текста для цели '", target, "' ---\n"))
    cat(paste0("Текст: ", tweet, "\n\n"))
    
    # ==========================================================
    # ЭТАП 1: Многомерный анализ текста
    # ==========================================================
    cat("1. Проведение многомерного анализа...\n")
    
    analysis_results <- list(
        linguistic = analyze_linguistic(chat_base, tweet),
        domain = analyze_domain(chat_base, tweet, target, domain_role),
        social_media = analyze_social_media(chat_base, tweet)
    )
    
    # cat("   Лингвистический анализ: ", analysis_results$linguistic, "\n")
    # cat("   Доменный анализ: ", analysis_results$domain, "\n")
    # cat("   Анализ соцсетей: ", analysis_results$social_media, "\n")
    
    # ==========================================================
    # ЭТАП 2: Дебаты с улучшенным логическим выводом
    # ==========================================================
    cat("2. Проведение дебатов...\n")
    
    debate_results <- list(
        favor = debate_stance(chat_base, tweet, target, "in favor", analysis_results),
        against = debate_stance(chat_base, tweet, target, "against", analysis_results),
        neutral = debate_stance(chat_base, tweet, target, "neutral", analysis_results)
    )
    
    # cat("   Аргумент 'За': ", debate_results$favor, "\n")
    # cat("   Аргумент 'Против': ", debate_results$against, "\n")
    # cat("   Аргумент 'Нейтрально': ", debate_results$neutral, "\n")
    
    # ==========================================================
    # ЭТАП 3: Заключение о позиции
    # ==========================================================
    cat("3. Вынесение окончательного решения...\n")
    
    final_stance <- determine_stance(chat_base, tweet, target, debate_results)
    
    cat(paste0("--- ОКОНЧАТЕЛЬНАЯ ПОЗИЦИЯ: ", final_stance, " ---\n\n"))
    
    return(list(
        tweet = tweet,
        target = target,
        stance = final_stance,
        analysis = analysis_results,
        debates = debate_results
    ))
}

# Функция для обработки нескольких текстов ----
cola_batch_detection <- function(chat_base, data_list, domain_role = "social commentator") {
    
    results <- lapply(data_list, function(item) {
        cola_single_detection(
            chat_base = chat_base,
            tweet = item$tweet,
            target = item$target,
            domain_role = domain_role
        )
    })
    
    # Преобразование результатов в удобный data frame
    final_df <- do.call(rbind, lapply(results, function(r) {
        data.frame(
            Tweet = r$tweet,
            Target = r$target,
            Stance = r$stance,
            stringsAsFactors = FALSE
        )
    }))
    
    return(list(
        summary_table = final_df,
        full_results = results
    ))
}

# Пример использования ----
# 1. Определяем тестовые данные ----
test_data <- list(
    list(
        tweet = "Налог на углерод — это просто ещё один способ для правительства контролировать нашу жизнь и подавлять экономический рост. Это убийца рабочих мест!",
        target = "Изменение климата — реальная проблема"
    ),
    list(
        tweet = "Я слышал её речь сегодня. Она говорила с такой страстью и ясностью о необходимости реформ. Настоящий лидер.",
        target = "Хиллари Клинтон"
    )
)


# 2. Запускаем пакетный анализ COLA ----
# (Убедитесь, что chat_base инициализирован выше)
# ВНИМАНИЕ: Этот процесс будет выполнять много запросов к API (3 эксперта + 3 дебатера + 1 судья = 7 запросов на текст).

# cola_results <- cola_batch_detection(chat_base, test_data)
# print(cola_results$summary_table)

cola_results$full_results

# --- Пример ожидаемого вывода (без фактического запуска, так как это требует API) ---

# cola_results$summary_table
#                                                                                               Tweet                  Target    Stance
# 1 The carbon tax is just another way for the government to control our lives and stifle economic growth. It's a job killer! Climate Change is Real Concern Against
# 2                                    I saw her speech today. She spoke with such passion and clarity about the need for reform. A true leader.  Hillary Clinton  Favor
