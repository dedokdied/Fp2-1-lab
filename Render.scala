// преобразование результатов и ошибок в текст
// это единственный слой, который знает, как именно ошибка должна выглядеть
// для пользователя, а домен и сервисы работают только с типизированными значениями

object Render:
  def error(error: AppError): String =
    error match
      case AppError.EmptyPolicyName =>
        "Имя не может быть пустым"

      case AppError.PolicyNotFound(name) =>
        s"Политика не найдена: $name"

      case AppError.PolicyAlreadyExists(name) =>
        s"Политика уже существует: $name"

      case AppError.MissingIncludedPolicy(policy, include) =>
        s"Политика '$policy' включает несуществующую политику: $include"

      case AppError.CyclicPolicyInclude(name) =>
        s"Обнаружена циклическая зависимость в политике: $name"

      case AppError.RuleNotFound(policy, ruleId) =>
        s"В политике '$policy' нет правила с ID '$ruleId'"

      case AppError.RuleAlreadyExists(policy, ruleId) =>
        s"Правило с ID '$ruleId' уже есть в политике '$policy'"

      case AppError.DuplicateRuleId(policy, ruleId) =>
        s"В политике '$policy' повторяется идентификатор правила: $ruleId"

      case AppError.InvalidRule(policy, ruleId, problem) =>
        s"Политика '$policy', правило '$ruleId': ${ruleProblem(problem)}"

      case AppError.ConfigReadFailed(path, reason) =>
        s"Не удалось прочитать конфигурацию '$path': $reason"

      case AppError.ConfigWriteFailed(path, reason) =>
        s"Не удалось сохранить конфигурацию '$path': $reason"

      case AppError.ConfigMalformed(block, reason) =>
        s"Некорректный конфигурационный файл ($block): $reason"

      case AppError.InvalidPath(path, reason) =>
        s"Некорректный путь '$path': $reason"

      case AppError.SourceNotFound(path) =>
        s"Источник не найден: $path"

      case AppError.OutputInsideSource(_, _) =>
        "Выходной каталог находится внутри обрабатываемого каталога"

      case AppError.FileReadFailed(path, reason) =>
        s"Не удалось прочитать файл '$path': $reason"

      case AppError.FileWriteFailed(path, reason) =>
        s"Не удалось записать файл '$path': $reason"

  def ruleProblem(problem: RuleProblem): String =
    problem match
      case RuleProblem.EmptyRuleId =>
        "пустой идентификатор правила"
      case RuleProblem.UnknownBuiltinKind(kind) =>
        s"неизвестный встроенный тип: $kind"
      case RuleProblem.InvalidRegex(_) =>
        "некорректное регулярное выражение"
      case RuleProblem.EmptyExactValue =>
        "пустое точное значение"
      case RuleProblem.EmptyDictionary =>
        "пустой словарь"
      case RuleProblem.NegativeMaskBounds(_, _) =>
        "параметры маски не могут быть отрицательными"
      case RuleProblem.MissingPseudonymPlaceholder(_) =>
        "в шаблоне псевдонима отсутствует {number}"
      case RuleProblem.EmptyListReplacement =>
        "пустой список замены"

  def warning(warning: Warning): String =
    warning match
      case Warning.RuleOverridden(ruleId) =>
        s"Правило '$ruleId' было переопределено"

  def policyList(policies: Seq[Policy]): Seq[String] =
    if policies.isEmpty then Seq("Политик пока нет")
    else
      policies.map { policy =>
        val includesText =
          if policy.includes.isEmpty then ""
          else s" включает: ${policy.includes.mkString(", ")}"
        s"- ${policy.name}$includesText"
      }

  def policy(policy: Policy): Seq[String] =
    val header = Seq(
      s"Политика: ${policy.name}",
      s"Описание: ${policy.description}"
    )
    val includesText =
      if policy.includes.isEmpty then Seq.empty
      else Seq(s"Включает: ${policy.includes.mkString(", ")}")
    val rulesText =
      if policy.rules.isEmpty then Seq("Правил нет")
      else policy.rules.flatMap(rule)

    header ++ includesText ++ rulesText

  def rule(rule: Rule): Seq[String] =
    Seq(
      s"  - ${rule.id}: ${rule.description}",
      s"    поиск: ${ConfigLoader.matcherToText(rule.matcher)}",
      s"    замена: ${ConfigLoader.replacementToText(rule.replacement)}",
      s"    приоритет: ${rule.priority}, активно: ${rule.enabled}"
    )

  def ruleChoice(index: Int, rule: Rule): String =
    s"${index + 1}) ${rule.id} — приоритет ${rule.priority}, активно: ${rule.enabled}"

  def conflict(conflict: Conflict): String =
    conflict.kind match
      case ConflictKind.OverlapResolved =>
        s"Найдено пересечение правил: ${conflict.ruleIds.mkString(", ")}"
      case ConflictKind.AmbiguousOverlap =>
        s"Невозможно выбрать правило: ${conflict.ruleIds.mkString(", ")}"

  def report(report: OperationReport): Seq[String] =
    Seq(
      "Обработка завершена.",
      s"Политика: ${report.policyName}",
      s"Начало: ${report.startedAt}",
      s"Завершение: ${report.finishedAt}",
      s"Источник: ${report.source}",
      s"Результат: ${report.output}",
      s"Файлов найдено: ${report.filesFound}",
      s"Обработано файлов: ${report.filesProcessed}",
      s"Пропущено файлов: ${report.filesSkipped}",
      s"Ошибок: ${report.filesFailed}",
      s"Создано замен: ${report.replacedTotal}",
      s"Конфликтов: ${report.conflictsTotal}"
    )

  def filePreview(fileResult: FileResult): Seq[String] =
    val header = Seq(s"Файл: ${fileResult.relativePath}")

    fileResult.preview match
      case None =>
        header :+ "  Нет данных для предпросмотра"

      case Some(process) =>
        val appliedText =
          if process.applied.isEmpty then Seq("  Совпадений не найдено")
          else
            val shown = process.applied.take(10).map { applied =>
              s"  ${applied.original} -> ${applied.replacementText}"
            }
            val rest =
              if process.applied.size > 10 then Seq(s"  ... ещё ${process.applied.size - 10}")
              else Seq.empty
            shown ++ rest

        val conflictsText = process.conflicts.take(5).map(conflictItem =>
          s"  Конфликт: ${conflictItem.ruleIds.mkString(", ")}"
        )

        header ++ appliedText ++ conflictsText

  def fileStatus(fileResult: FileResult): String =
    fileResult.error match
      case Some(value) => Render.error(value)
      case None => "Пропущен"
