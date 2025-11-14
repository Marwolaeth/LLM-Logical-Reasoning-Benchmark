library(vitals)
library(ellmer)
library(tibble)
library(dplyr)
library(readr)
library(ggplot2)

options(ellmer_timeout_s = 13000)

## Загрузка данных ----
data_path <- 'data/voinarovsky-test-mc.csv'
voinarovskiy <- read_csv2(data_path) |>
    # Делаем правильный вариант фактором
    ## Разделяем порядковый номер (id) вопроса и текст посылки
    dplyr::mutate(
        correct = factor(correct, levels = c('a', 'b', 'c')),
        id = stringr::str_extract(premise, '^\\d{1,2}'),
        premise = stringr::str_remove(premise, '^\\d{1,2}\\.\\s')
    ) |>
    dplyr::relocate(id, .before = 0)

## Выбор подходящих моделей ----

### Ollama ----
selected_models <- c(
    'phi4',
    'llama3.1:8b',
    'lostinspiration/nuextract-1.5',
    'ALIENTELLIGENCE/structureddataextraction',
    'iodose/nuextract-v1.5',
    'aya-expanse:8b',
    'mistral:7b',
    'qwen3:4b',
    'wizardlm2',
    'openhermes',
    'llama3.2',
    'Osmosis/Osmosis-Structure-0.6B',
    'wizard-math',
    'vanta-research/apollo-v1-7b',
    'granite3.1-moe:3b'
)

ollama_models <- models_ollama()$id |> base::intersect(selected_models)

### vLLM ----
VLLM_SERVER <- 'http://127.0.0.1:1234'
VLLM_BASE <- 'http://127.0.0.1:1234/v1'

vllm_models <- models_vllm(base_url = VLLM_SERVER)

vllm_models <- vllm_models[which(!grepl('embed', vllm_models$id)),'id']

### OpenRouter ----
openrouter_models <- c(
    'qwen/qwen2.5-vl-32b-instruct:free',
    'meta-llama/llama-4-scout:free',
    'openai/gpt-oss-20b:free',
    'z-ai/glm-4.5-air:free',
    'meituan/longcat-flash-chat:free',
    'alibaba/tongyi-deepresearch-30b-a3b:free',
    'nvidia/nemotron-nano-9b-v2:free',
    'qwen/qwen3-235b-a22b:free',
    'meta-llama/llama-4-maverick:free',
    'deepseek/deepseek-r1:free'
)

## Загрузка инструкций ----

### Системные ----
selected_prompts <- c(
    'formal',
    'professional',
    # 'expert_default',
    'simple',
    'extreme-human',
    # 'expanded',
    'cosmic-superintelligence',
    'trivial'
)

system_prompts <- purrr::map_dfr(
    selected_prompts,
    \(pname) {
        ptext <- readr::read_file(
            file.path(
                'prompts',
                'logic-test',
                paste(pname, 'md', sep = '.')   
            )
        )
        tibble::tibble(
            name = pname,
            prompt = ptext
        )
    }
)

### Пользовательская ----
user_prompt_template <- file.path('prompts', 'logic-user-prompt-template.md')

tasks <- ellmer::interpolate_file(
    user_prompt_template,
    !!!voinarovskiy
)

# Проверим
tasks[30]

## Схема ответа ----
schema <- type_object(
    answer = type_enum(
        c('a', 'b', 'c'),
        description = 
            'Какая из гипотез (a, b, c) является логическим следствием посылки',
        required = TRUE
    ),
    # explanation = type_string(
    #     description = 'Краткое пояснение (1-4 предложения), почему был выбран именно этот вариант.',
    #     required = FALSE
    # ),
    .description = 'Ответ на вопрос логического теста'
)

### Эксперимент ----

#### Ollama ----
chat <- chat_ollama(
    system_prompt = system_prompts[['prompt']][[1]],
    # model = 'Osmosis/Osmosis-Structure-0.6B',
    model = 'wizard-math',
    api_args = list(temperature = 0)
)

ans <- chat$chat_structured(tasks[3], type = schema, echo = 'text')

res <- ellmer::parallel_chat_structured(
    chat = chat,
    prompts = tasks,
    type = schema
)

dplyr::bind_cols(res, correct = voinarovskiy$correct)

mean(res$answer == voinarovskiy$correct)

#### VLLM ----
chat <- chat_vllm(
    base_url = VLLM_BASE,
    system_prompt = system_prompts[2, 'prompt', drop = TRUE],
    model = 'scout-4b',
    api_args = list(temperature = 0),
    echo = 'all'
)

chat$chat_structured(tasks[[3]], type = schema)

res <- ellmer::parallel_chat_structured(
    chat = chat,
    prompts = tasks,
    type = schema
)

#### OpenRouter ----
openrouter_key <- function() {
    list(Authorization = paste(
        'Bearer', Sys.getenv('OPENROUTER_API_KEY')
    ))
}

chat <- chat_openrouter(
    system_prompt = system_prompts[['prompt']][[3]],
    model = 'nvidia/nemotron-nano-9b-v2:free',
    credentials = openrouter_key
)

# chat$chat('Если в огороде бузина, то в Киеве дядька. Дядька не в Киеве. Есть ли в огороде бузина?')

Sys.getenv('OPENROUTER_API_KEY')

chat$chat_structured(tasks[3], type = schema)

packageVersion('ellmer')

httr2::last_request() |> print(redact_headers = FALSE)

# GOTO 123

## Набор данных для оценки ----
### Для оценки: только инструкция с вопросом и вариантами и верный вариант
df <- tibble::tibble(
    input = tasks,
    target = voinarovskiy[['correct']]
)

## Определение решателя (solver) ----
### Параллельное выполнение ----
fill_turns <- function(user, assistant, chat) {
    new_chat <- chat$clone()
    new_chat$add_turn(
        user = user,
        assistant = assistant
    )
    return(new_chat)
}

parallel_chat_structured <- function (
        chat,
        prompts,
        type,
        convert = TRUE,
        include_tokens = FALSE,
        include_cost = FALSE,
        max_active = 10,
        rpm = 500,
        on_error = c("return", "continue", "stop")
) {
    chat <- ellmer:::as_chat(chat)
    turns <- ellmer:::as_user_turns(prompts)
    ellmer:::check_bool(convert)
    on_error <- rlang::arg_match(on_error)
    provider <- chat$get_provider()
    needs_wrapper <- ellmer:::type_needs_wrapper(type, provider)
    user_turns <- ellmer:::as_user_turns(prompts)
    existing <- chat$get_turns(include_system_prompt = TRUE)
    conversations <- ellmer:::append_turns(list(existing), user_turns)
    turns <- ellmer:::parallel_turns(
        provider = provider,
        conversations = conversations,
        tools = chat$get_tools(),
        type = ellmer:::wrap_type_if_needed(type, needs_wrapper),
        max_active = max_active,
        rpm = rpm,
        on_error = on_error
    )
    ellmer:::log_turns(provider, turns)
    result <- ellmer:::multi_convert(
        provider,
        turns,
        type,
        convert = convert,
        include_tokens = include_tokens,
        include_cost = include_cost
    )
    
    chats <- purrr::map2(
        user_turns,
        turns,
        ~ fill_turns(.x, .y, chat = chat)
    )
    result$chat <- chats
    return(result)
}

generate_structured_parallel <- function(solver_chat = NULL, schema = NULL, ...) {
    chat <- solver_chat
    schm <- schema
    
    function(inputs, schema = schm, ..., solver_chat = chat) {
        vitals:::check_inherits(solver_chat, 'Chat')
        vitals:::check_inherits(schema, 'ellmer::TypeObject')
        vitals:::check_logical('answer' %in% names(schema@properties))
        
        ch <- solver_chat$clone()
        
        res <- parallel_chat_structured(
            chat = ch,
            prompts = inputs,
            type = schema,
            ...,
            convert = TRUE,
            on_error = 'continue'
        )
        
        list(
            result = res$answer, 
            solver_chat = res$chat
        )
    }
}

### Тест задания ----
# cht <-chat_ollama(
#     model = 'llama3.2',
#     system_prompt = system_prompts[['prompt']][[1]],
#     api_args = list(temperature = 0)
# )
# 
# set.seed(111)
# tsk <- Task$new(
#     dataset = slice_sample(df, n = 10),
#     solver = generate_structured(schema = schema, solver_chat = cht),
#     scorer = detect_exact()
# )
# 
# tictoc::tic()
# tsk$eval()
# tictoc::toc()
# 
# tsk$get_samples()
# tsk$metrics

## Функция оценки ----

# Переопределим функцию, получающую ключ OpenRouter
openrouter_key <- function() {
    list(Authorization = paste(
        'Bearer', Sys.getenv('OPENROUTER_API_KEY')
    ))
}

# Создадим функцию запуска задачи, которую мы применим к каждой из доступных моделей
model_logic_test <- function(
        model_name,
        provider = c('ollama', 'vllm', 'openrouter'),
        dataset,
        system_prompt,
        task_name = NULL,
        epochs = 1L,
        ...
) {
    provider <- match.arg(provider, choices = c('ollama', 'vllm', 'openrouter'))
    
    # Проверим, правильно ли указан провайдер, чтобы не влететь на ошибку
    models_list <- get(paste(provider, 'models', sep = '_'))
    if (!(model_name %in% models_list)) {
        stop('Wrong provider specifiaction!')
    }
    
    # Chat functions
    if (provider == 'openrouter') {
        requests_per_minute <- 19 # https://openrouter.ai/docs/api-reference/limits
        max_active <- 2
        api_key <- openrouter_key
    } else {
        requests_per_minute <- 500
        max_active <- 10
        api_key <- NULL
    }
    
    ## Создадим объект типа `Chat` с соответствующей моделью и
    ### нашим заранее заготовленным системным промптом
    cht <- ellmer::chat(
        name = paste(provider, model_name, sep = '/'),
        if (provider == 'vllm') base_url = VLLM_BASE,
        system_prompt = system_prompt,
        api_args = list(temperature = 0),
        credentials = api_key,
        echo = 'all'
    )
    
    cat(paste0("Оценка модели: ", model_name, "\n"))
    
    ## Задаем осмысленное имя задаче с учетомтого, что наш набор данных и
    ### инструкции — про логику
    if (is.null(task_name)) {
        task_name = glue::glue('logic-{model_name}-x{epochs}')
    }
    
    # Создаем задачу для текущей модели
    tsk <- vitals::Task$new(
        dataset = dataset,
        solver = generate_structured_parallel(
            schema = schema,
            solver_chat = cht,
            rpm = requests_per_minute,
            max_active = max_active,
            ...
        ),
        scorer = detect_exact(),
        name = task_name
    )
    
    # Выполняем оценку
    return(tsk$eval(view = FALSE, epochs = epochs))
}

test <- model_logic_test(
    model = 'nanoagent-135m',
    provider = 'vllm',
    system_prompt = system_prompts[['prompt']][[1]],
    dataset = df
)

test <- model_logic_test(
    model = 'nvidia/nemotron-nano-9b-v2:free',
    provider = 'openrouter',
    system_prompt = system_prompts[['prompt']][[2]],
    dataset = df
)

test
.last_task$view()
mean(test$get_samples()$result == test$get_samples()$target)

test$get_samples()$solver_metadata[[1]]

## Оценка ----
vitals::vitals_log_dir_set("./logs")

all_models <- c(vllm_models, ollama_models, openrouter_models)
results_list <- list()

for (model_name in openrouter_models) {
    cat(rep('#', 40), '\n')
    cat(rep('#', 40), '\n')
    cat(rep('-', 40), '\n')
    # provider <- ifelse(model_name %in% vllm_models, 'vllm', 'ollama')
    provider <- 'openrouter'
    
    for (prompt_obj in purrr::transpose(as.list(system_prompts))) {
        prompt_name <- prompt_obj$name
        
        cat('Инструкция: ', prompt_name, '\n')
        cat(rep('-', 40), '\n')
        
        ## Проверим, нет ли уже (вдруг) такого теста
        task_id <- glue::glue('{model_name}-{prompt_name}')
        if (!is.null(results_list[[task_id]])) next
        
        cat(rep('-', 40), '\n')
        
        ## Запустим оценку и замерим время
        tictoc::tic()
        evaluation_results <- model_logic_test(
            model_name,
            provider = provider,
            dataset = df,
            system_prompt = prompt_obj$prompt,
            epochs = 1L,
            task_name = glue::glue(
                'logic-mc-{model_name}-{prompt_name}-x1'
            )
        )
        stopwatch <- tictoc::toc()
        
        # Заполним метаданные, не предусмотренные библиотекой
        evaluation_results[['model_name']] <- model_name
        evaluation_results[['time']] <- (stopwatch$toc - stopwatch$tic)
        evaluation_results[['prompt_type']] <- prompt_name
        
        # Добавляем результаты в список
        results_list[[task_id]] <- evaluation_results
        save(results_list, file = 'output/logic-mc-openrouter.RData')
    }
}

# save(results_list, file = 'output/logic-mc.RData')

results_list_current <- results_list
load('output/logic-mc.RData')

results_list <- c(results_list, results_list_current)

## Анализ ----

### Модели и инструкции ----
comparison_df <- tibble(
    model = character(),
    prompt = character(),
    accuracy = numeric(),
    time = numeric(),
    tokens_input = numeric(),
    tokens_output = numeric()
)

for (task_id in names(results_list)) {
    current_results <- results_list[[task_id]]
    
    cost <- current_results$get_cost()
    
    comparison_df <- comparison_df %>%
        add_row(
            model = current_results$model_name,
            prompt = current_results$prompt_type,
            accuracy = current_results$metrics,
            time = current_results$time,
            tokens_input = cost$input,
            tokens_output = cost$output
        )
}

print("Результаты сравнения моделей:")
comparison_df <- comparison_df |>
    mutate(tokens_total = tokens_input + tokens_output)
print(comparison_df)

save(comparison_df, file = 'output/logic-mc-df.RData')

#### Время ----
ggplot(
    comparison_df,
    aes(x = time, y = accuracy, colour = model)
) +
    geom_point(alpha = .75) +
    stat_smooth(aes(group = model), method = 'lm') +
    theme_light()

#### Модели ----
comparison_df |>
    group_by(model) |>
    summarise(numerical_summary_df(accuracy, quantiles = seq(.25, .75, .25))) |>
    jsonlite::toJSON()

ggplot(
    comparison_df,
    aes(x = model, y = accuracy, size = time)
) +
    geom_point(
        alpha = .5,
        colour = 'royalblue',
        position = position_jitterdodge(jitter.width = .26, jitter.height = .05)
    ) +
    stat_summary(fun.data = 'mean_cl_boot', position = position_jitterdodge()) +
    # scale_size_continuous(transform = 'log10') +
    guides(size = 'none') +
    labs(
        title = 'Large Language Models: Logical Entailment Task Evaluation',
        x = 'Model',
        y = 'Accuracy Score',
        # size = 'Execution Time (s)',
        caption = 'Each point represents the accuracy for a specific model and one of the five custom prompt variants. Point size represents the execution time in seconds.'
    ) +
    theme_light() +
    theme(legend.position = 'top', legend.direction = 'horizontal') +
    coord_flip()

#### Инструкции ----
comparison_df |>
    group_by(prompt) |>
    summarise(numerical_summary_df(accuracy, quantiles = seq(.25, .75, .25))) |>
    jsonlite::toJSON()

ggplot(
    comparison_df,
    aes(x = prompt, y = accuracy, size = time)
) +
    geom_point(
        alpha = .5,
        colour = 'royalblue',
        position = position_jitterdodge(jitter.width = .26, jitter.height = .08)
    ) +
    stat_summary(fun.data = 'mean_cl_boot', position = position_jitterdodge()) +
    guides(size = 'none') +
    labs(
        title = 'Large Language Models: Logical Entailment Task Evaluation',
        x = 'Prompt Variant',
        y = 'Accuracy Score',
        size = 'Execution time (s)',
        caption = 'Each point represents the accuracy for a specific custom prompt variant and one of the eleven models. Point size represents the execution time in seconds.'
    ) +
    theme_light() +
    theme(legend.position = 'top', legend.direction = 'horizontal') +
    coord_flip()

ggplot(
    comparison_df,
    aes(x = prompt, y = time)
) +
    geom_point(
        alpha = .5,
        colour = 'royalblue',
        position = position_jitterdodge(jitter.width = .26, jitter.height = .08)
    ) +
    stat_summary(fun.data = 'mean_cl_boot', position = position_jitterdodge()) +
    labs(
        title = 'Large Language Models: Logical Entailment Task Evaluation',
        x = 'Prompt Variant',
        y = 'Execution time (s)',
        caption = 'Each point represents the execution time of a singletest for a specific custom prompt variant and one of the eleven models.'
    ) +
    theme_light() +
    coord_flip()

#### Вопросы ----
results_list[[1]]$get_samples()

samples_df <- purrr::map(
    results_list,
    \(r) {
        r$get_samples() |>
            dplyr::mutate(
                id = as.character(id),
                model = r$model_name,
                prompt = r$prompt_type
            ) |>
            dplyr::left_join(select(voinarovskiy, id, premise))
    }
) |>
    bind_rows(.id = 'task_id')

save(samples_df, file = 'output/logic-mc-samples.RData')

samples_df |>
    summarise(accuracy = mean(score == 'C'), .by = premise)

samples_df |>
    summarise(
        accuracy = mean(score == 'C'),
        a = mean(result == 'a'),
        b = mean(result == 'b'),
        c = mean(result == 'c'),
        target = first(target),
        .by = premise
    )

samples_df |>
    select(task_id, target, result, score, model:premise, premise_id = id)
