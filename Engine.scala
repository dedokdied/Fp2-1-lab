import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.Try

// журнал работы программы
// сообщения не выводятся в консоль и не влияют на бизнес-логику.
object Logger:
  private def now(): String =
    LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")) // текущее время

  def log(logFile: String, message: String): Unit = //
    Try { // пробуем записать файл
      val path = Paths.get(logFile)
      if path.getParent != null then Files.createDirectories(path.getParent) // создаем папку если нет
      Files.write(
        path,
        s"[${now()}] $message\n".getBytes(StandardCharsets.UTF_8), // добавляем время и сообщение
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND,
        StandardOpenOption.WRITE
      )
      ()
    }
    () // если что-то упало, то игнорируем, чтобы не ронять всю программу

// состояние нумерации псевдонимов
// каждая операция возвращает новое состояние, поэтому
// одинаковые значения получают одинаковые псевдонимы во всех файлах
final case class ReplacementState private (
    private val nextNumber: Int, // следующий номер для нового псевдониа
    private val numbers: Map[String, Int], // список: оригинал - номер
    private val listAssignments: Map[String, String], // список: оригинал - значение из списка
    private val listCounter: Int // счетчик для циклического выбора из списка
):
  def numberFor(original: String): (Int, ReplacementState) = // получение номера для значения
    numbers.get(original) match
      case Some(number) => // если уже встречалось, то возвращаем тот же номер
        (number, this)
      case None => // новое значение - новый номер
        (
          nextNumber,
          copy(nextNumber = nextNumber + 1, numbers = numbers.updated(original, nextNumber))
        )

  def listValueFor(original: String, values: Seq[String]): (String, ReplacementState) = // получаем значение из списка
    listAssignments.get(original) match
      case Some(value) =>
        (value, this) // уже назначено - возвращаем его же
      case None =>
        val value = values(listCounter % values.size) // если не назначено, то берем следующее по кругу
        (
          value,
          copy(
            listCounter = listCounter + 1,
            listAssignments = listAssignments.updated(original, value)
          )
        )

object ReplacementState:
  val empty: ReplacementState = ReplacementState(1, Map.empty, Map.empty, 0) // начальное состояние

// раскрытие политик
object Engine:
  private val supportedBuiltinKinds = Set("email", "phone", "ipv4", "url", "password", "token") // встроенные типы

  private def now(): String = // время для отчетов
    LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))

  // раскрытие политики
  def expandPolicy(name: String, config: Config): Either[AppError, ExpandedPolicy] = // раскрыть политику, вложенные тоже
    expand(name, config, Set.empty) // сначала начинаем с пустым


  private def expand(
      name: String,
      config: Config,
      visited: Set[String] // уже посещенные политики
  ): Either[AppError, ExpandedPolicy] =
    if visited.contains(name) then Left(AppError.CyclicPolicyInclude(name)) // если политика уже была рассмотрена, то ошибка
    else
      config.findPolicy(name).toRight(AppError.PolicyNotFound(name)).flatMap { policy =>
        // в конфиге мы ищем политику по имени, если нашла, то право - удачно, если нет, то ошибка
        val nextVisited = visited + name
        val includeResults = policy.includes.map(include => expand(include, config, nextVisited)) // раскрываем вложенные

        includeResults.collectFirst { case Left(error) => error } match // ищем ошибку
          case Some(error) =>
            Left(error) // если есть ошибка - возвращаем
          case None =>
            val included = includeResults.collect { case Right(expanded) => expanded } // собираем политики
            val (rules, overrideWarnings) =
              overrideRules(included.flatMap(_.rules) ++ policy.rules) // собираем их правила
            val warnings = included.flatMap(_.warnings) ++ overrideWarnings
            Right(ExpandedPolicy(name, rules, warnings))
      }

  // правило с одинаковым id из более поздней политики заменяет предыдущее
  // нужно, если в политике есть одинаковые правила
  private def overrideRules(rules: Seq[Rule]): (Seq[Rule], Seq[Warning]) =
    rules.foldLeft((Seq.empty[Rule], Seq.empty[Warning])) { // проходим по всем правилам
      case ((accumulated, warnings), rule) =>
        val overridden = accumulated.exists(_.id == rule.id)
        val nextWarnings =
          if overridden then warnings :+ Warning.RuleOverridden(rule.id) else warnings // предупреждение делаем
        (accumulated.filterNot(_.id == rule.id) :+ rule, nextWarnings) // удаляем старое, добавляем новое
    }

  // проверка политик и правил

  // возвращает все найденные проблемы, так пользователь видит полную картину за один запуск.
  def validatePolicy(name: String, config: Config): Seq[AppError] = // проверка политики на ошибки
    val policyErrors = config.policies.flatMap(policy => validatePolicyStructure(policy, config))
    val expansionError = expandPolicy(name, config).left.toOption
    // смотрим ошибки раскрытия, берем раскрываем политику, мы хотим увидеть ТОЛЬКО ошибки, если у нас ошибок нет, то оно
    // передаст NONE и все, а если есть, то передаст конкретику ошибки
    (policyErrors ++ expansionError).distinct // обьекдинение ошибок и убираем дубликаты

  private def validatePolicyStructure(policy: Policy, config: Config): Seq[AppError] = // проверить структуру политик
    val duplicateIds = policy.rules
      .groupBy(_.id) // группируем по ид
      .collect { case (id, rules) if rules.size > 1 => id } // берем те, где больше 1 правила
      .toSeq
      .sorted

    val duplicateErrors = duplicateIds.map(id => AppError.DuplicateRuleId(policy.name, id)) //

    val includeErrors = policy.includes
      .filter(include => config.findPolicy(include).isEmpty) // вложенная политика не существует
      .map(include => AppError.MissingIncludedPolicy(policy.name, include))

    val ruleErrors = policy.rules.flatMap(rule => validateRule(policy.name, rule)) // проверка ошибок в каждом правиле

    duplicateErrors ++ includeErrors ++ ruleErrors

  private def validateRule(policy: String, rule: Rule): Seq[AppError] = // проверить одно правило
    ruleProblems(rule).map(problem => AppError.InvalidRule(policy, rule.id, problem))

  private def ruleProblems(rule: Rule): Seq[RuleProblem] = // найти проблемы в правиле
    val idProblems =
      if rule.id.trim.isEmpty then Seq(RuleProblem.EmptyRuleId) else Seq.empty // если ид правила пустое

    val matcherProblems = rule.matcher match
      case BuiltinMatcher(kind) if !supportedBuiltinKinds.contains(kind.toLowerCase) =>
        Seq(RuleProblem.UnknownBuiltinKind(kind)) // неизвестный встроенный тип
      case RegexMatcher(pattern) if pattern.isEmpty || !isValidRegex(pattern) =>
        Seq(RuleProblem.InvalidRegex(pattern)) // битая регулярка
      case ExactMatcher(value) if value.isEmpty =>
        Seq(RuleProblem.EmptyExactValue) // пустое точное значение
      case DictionaryMatcher(values) if values.isEmpty =>
        Seq(RuleProblem.EmptyDictionary) // пустой словарь
      case _ =>
        Seq.empty

    val replacementProblems = rule.replacement match
      case MaskReplacement(visibleStart, visibleEnd, _) if visibleStart < 0 || visibleEnd < 0 =>
        Seq(RuleProblem.NegativeMaskBounds(visibleStart, visibleEnd)) // отрицательные границы маски
      case PseudonymReplacement(template) if !template.contains("{number}") =>
        Seq(RuleProblem.MissingPseudonymPlaceholder(template)) // нет номера в шаблоне
      case ListReplacement(values) if values.isEmpty =>
        Seq(RuleProblem.EmptyListReplacement) // пустой список замены
      case _ =>
        Seq.empty

    idProblems ++ matcherProblems ++ replacementProblems

  private def isValidRegex(pattern: String): Boolean = // проверить регулярку
    Try(Pattern.compile(pattern)).isSuccess // если она компилируется - все ок регулярка рабочая.

  // обработка текста

  def processText(text: String, rules: Seq[Rule]): ProcessResult = // обработать текст без состояния
    processText(text, rules, ReplacementState.empty)._1

  def processText(
      text: String,
      rules: Seq[Rule],
      state: ReplacementState
  ): (ProcessResult, ReplacementState) = // обработать текст с состоянием
    val allMatches = rules.filter(_.enabled).flatMap(rule => findMatches(rule, text)) // найти все совпадения
    val (selected, conflicts) = resolveMatches(allMatches) // разрешение конфликтов

    val (replacements, nextState) =
      selected.foldLeft((Vector.empty[(FoundMatch, String)], state)) { // проходим по выбранным совпадениям
        case ((accumulated, currentState), found) =>
          val (replacementText, updatedState) =
            makeReplacement(found.original, found.replacement, currentState) // генерируем текст замены
          (accumulated :+ (found -> replacementText), updatedState)
      }

    val applied = replacements
      .map { case (found, replacementText) =>
        AppliedMatch(found.ruleId, found.start, found.end, found.original, replacementText)
      }
      .sortBy(_.start) // сортируем по позиции начала

    val counts = applied
      .groupBy(_.ruleId) // группируем по ид правила
      .view
      .mapValues(_.size) // считаем по количеству
      .toMap

    val result = ProcessResult(
      outputText = applyReplacements(text, replacements), // итоговый текст
      applied = applied, // список примененных замен
      conflicts = conflicts, // список конфликтов
      countsByRule = counts // количество по каждому правилу
    )
    (result, nextState)

  // замены уже не пересекаются, поэтому текст собирается из кусков исходной
  // строки и текстов замен
  private def applyReplacements(
      text: String,
      replacements: Vector[(FoundMatch, String)]
  ): String = // применить замены к тексту
    val sorted = replacements.sortBy { case (found, _) => found.start } // сортируем по позиции (по началу совпадения)
    val (parts, lastIndex) = sorted.foldLeft((Vector.empty[String], 0)) {
      case ((accumulated, index), (found, replacementText)) =>
        (accumulated :+ text.substring(index, found.start) :+ replacementText, found.end) // берем кусок замены + замены
    }
    (parts :+ text.substring(lastIndex)).mkString // добавляем остаток текста

  // поиск совпадений

  private def findMatches(rule: Rule, text: String): Seq[FoundMatch] = // найти сопадения для правила (мы смотрим какой поиск совпадений он использует)
    if !rule.enabled then Seq.empty // если правило выключено, то правило возвращает пустое
    else
      rule.matcher match
        case BuiltinMatcher(kind) =>
          builtinPattern(kind) match
            case Some((pattern, group)) => findPattern(pattern, group, text, rule)
            case None => Seq.empty

        case RegexMatcher(pattern) =>
          if isValidRegex(pattern) then findPattern(Pattern.compile(pattern), 0, text, rule)
          else Seq.empty

        case ExactMatcher(value) =>
          findExactValue(value, text, rule)

        case DictionaryMatcher(values) =>
          values.flatMap(value => findExactValue(value, text, rule))

  private def builtinPattern(kind: String): Option[(Pattern, Int)] = // получаем регулярку для встроенного типа
    kind.toLowerCase match
      case "email" =>
        Some((Pattern.compile("\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"), 0))

      case "phone" =>
        Some((Pattern.compile("(?:\\+7|8)?[\\s(]*\\d{3}[\\s)]*\\d{3}[\\s-]*\\d{2}[\\s-]*\\d{2}"), 0))

      case "ipv4" =>
        Some((Pattern.compile("\\b(?:\\d{1,3}\\.){3}\\d{1,3}\\b"), 0))

      case "url" =>
        Some((Pattern.compile("\\bhttps?://[^\\s<>\"']+"), 0))

      case "password" =>
        Some((Pattern.compile("(?i)(?:password|passwd|pwd)\\s*[=:]\\s*([^\\s;,\"]+)"), 1))

      case "token" =>
        Some(
          (
            Pattern.compile(
              "(?i)(?:token|api[_-]?key|secret|access[_-]?key|authorization)\\s*[=:]\\s*([^\\s;,\"]+)"
            ),
            1
          )
        )

      case _ =>
        None

  private def findPattern( // мы берем регулярки из текста и находим все места, где эта регулярка подходит
      pattern: Pattern,
      group: Int,
      text: String,
      rule: Rule
  ): Seq[FoundMatch] =
    val matcher = pattern.matcher(text)

    Iterator
      .continually(matcher) // отдавай один и тот же matcher бесконечно
      .takeWhile(_.find()) // зови find() пока находит совпадение не нашёл - стоп
      .flatMap { current => // для каждого найденного совпадения обработай None - выбросить, Some - оставить
        val useGroup =
          group > 0 && current.groupCount() >= group && current.group(group) != null
        val start = if useGroup then current.start(group) else current.start()
        val end = if useGroup then current.end(group) else current.end()
        val original = if useGroup then current.group(group) else current.group()

        Option(original)
          .filter(_.nonEmpty)
          .map(value => FoundMatch(rule.id, rule.priority, start, end, value, rule.replacement))
      }
      .toSeq

  private def findExactValue(value: String, text: String, rule: Rule): Seq[FoundMatch] = // ищем точное значение
    if value.isEmpty then Seq.empty // если значение пустое, то коллекция пустая
    else
      Iterator
        .iterate(text.indexOf(value))(index => text.indexOf(value, index + value.length)) // бесконечные итерации
      // (начало)(шаг), первый элемент это начало, все слеюущие вычисляем черещ шаг
        .takeWhile(_ >= 0)
      // берем элементы пока индекс не меньше 0, когда верет -1 - остановка
        .map(index => FoundMatch(rule.id, rule.priority, index, index + value.length, value, rule.replacement))
      // начало и конец совпадений
        .toSeq
        // сборка в список

  // разрешение конфликтов

  private def resolveMatches(matches: Seq[FoundMatch]): (Seq[FoundMatch], Seq[Conflict]) = // разрешить конфликт
    // в итоге будет 2 коллекции, найденных совпадений и коллекция конфликтов
    val sorted = matches.distinct.sortBy(m => (m.start, -m.priority, -(m.end - m.start))).toList
    // убираем дубликаты, дальше сортируем по:
    // началу, приоритету (от большего к меньшему), по длине совпадения
    val resolved = clusters(sorted).map(resolveCluster)
    (resolved.flatMap(_._1), resolved.flatMap(_._2))
    // 1 и 2 элементы памы флетмапим

  // Если два правила нашли куски текста, которые цепляются друг за друга, они попадают в один кластер.

  // совпадения группируются, только если они действительно пересекаются
  // соседние совпадения (конец одного равен началу другого) - не конфликт.
  private def clusters(sorted: List[FoundMatch]): List[List[FoundMatch]] = // разбиваем на кластеры перескающихся совпадений
    @tailrec
    def loop(
        rest: List[FoundMatch],
        accumulated: List[List[FoundMatch]]
    ): List[List[FoundMatch]] = {
      // возвращаем список групп внутри каждой группы
      rest match
        case Nil =>
          accumulated.reverse // если список пустой, то все разобрано
        case head :: tail =>
          // первое и остальные совпадения
          val (group, remaining) = takeCluster(tail, head.end, List(head))
          // собираем в кластер
          loop(remaining, group :: accumulated)
    }

    loop(sorted, Nil) // запуск

  @tailrec
  private def takeCluster( // сборка кластера из пересекабщихся совпадений
      rest: List[FoundMatch], // не разобранные совпадения
      currentEnd: Int,//правая граница кластера
      accumulated: List[FoundMatch] // уже собранные совпадения кластера
  ): (List[FoundMatch], List[FoundMatch]) =
    rest match // есть ли еще совпадения для проверки
      case found :: tail if found.start < currentEnd =>
        takeCluster(tail, math.max(currentEnd, found.end), found :: accumulated)
        // если пересекаются, то добавляем в кластер
      case _ =>
        (accumulated.reverse, rest)

  private def resolveCluster( // выбор победителя из одной группы пересекающихс совпадений
      cluster: List[FoundMatch] // список переекающихся совпадений
  ): (Option[FoundMatch], Option[Conflict]) =
    // отдает побеителя и конфликт
    val maxPriority = cluster.map(_.priority).max
    val byPriority = cluster.filter(_.priority == maxPriority)
    val maxLength = byPriority.map(found => found.end - found.start).max
    val best = byPriority.filter(found => found.end - found.start == maxLength)

    best match
      case winner :: Nil =>
        val losers = cluster.filterNot(_ == winner).map(_.ruleId).distinct
        // проигравшие правила ищем
        val conflict =
          if losers.isEmpty then None // если нет проигравших, то конфликтов тоже нет
          else
            Some(
              Conflict(
                kind = ConflictKind.OverlapResolved,
                start = winner.start,
                end = winner.end,
                ruleIds = (winner.ruleId +: losers).distinct
              )
            )
        (Some(winner), conflict)

      case _ => // однозначного победителя нет
        val first = best.head
        (
          None, // без победителя
          Some(
            Conflict(
              kind = ConflictKind.AmbiguousOverlap,
              start = first.start,
              end = first.end,
              ruleIds = best.map(_.ruleId).distinct
            )
          )
        )

  // генерация текста замены

  private def makeReplacement( // решаем на что заменить найденный текст
      original: String,
      replacement: Replacement, // тип замены
      state: ReplacementState
  ): (String, ReplacementState) = // возвращаем строку замены и новое состояние
    replacement match // смотрим какой тип замены указан
      case ConstantReplacement(value) =>
        (value, state)

      case DeleteReplacement =>
        ("", state)

      case MaskReplacement(visibleStart, visibleEnd, maskChar) =>
        (mask(original, visibleStart, visibleEnd, maskChar), state)

      case PseudonymReplacement(template) =>
        val (number, nextState) = state.numberFor(original)
        (template.replace("{number}", number.toString), nextState)

      case ListReplacement(values) =>
        if values.isEmpty then ("[REDACTED]", state)
        else state.listValueFor(original, values)

  private def mask(original: String, visibleStart: Int, visibleEnd: Int, maskChar: Char): String = {
    // сколько символов оставить в начале и в конце
    if original.isEmpty then original // если пусто, то возвращаем
    else
      val start = math.max(0, math.min(visibleStart, original.length))
      // узнаем сколько оставить в начале
      val end = math.max(0, math.min(visibleEnd, original.length - start))
      // укзнаем сколько оставить в конце
      val hidden = original.length - start - end
      // сколько прячем

      if hidden <= 0 then original
      else original.take(start) + maskChar.toString * hidden + original.takeRight(end)
  }

  // обработка файлов и каталогов

  def anonymize( // главная функция анонимизации
      policyName: String,
      sourcePath: String, // откуда берем данные
      config: Config,
      writeFiles: Boolean // пишем файлы или только предпросмотр
  ): Either[AppError, OperationReport] =
    val started = now()

    expandPolicy(policyName, config).flatMap { expanded =>
      // если все ок то идем дальше, если ошибка, то ошибка
      toAbsolutePath(sourcePath).flatMap { source => // если путь кривой то лево
        if !fileExists(source) then Left(AppError.SourceNotFound(source.toString))
        else
          toAbsolutePath(config.settings.outputDirectory).flatMap { outputRoot =>
            val outputInsideSource =
              Try(Files.isDirectory(source)).getOrElse(false) &&
                Try(outputRoot.startsWith(source)).getOrElse(false)
                // не находится ли выходная папка внутри исходной

            if outputInsideSource then {
              Left(AppError.OutputInsideSource(source.toString, outputRoot.toString))
              // если вывод внутри источника
            } else
              collectAllFiles(source).map { allFiles => // собираем все файлы из источника
                val supportedFiles =
                  allFiles.filter(file => isSupported(file, config.settings.supportedExtensions))

                Logger.log(
                  config.settings.logFile,
                  s"Начата обработка: политика=$policyName, источник=$source, файлов найдено=${allFiles.size}"
                )

                val (fileResults, _) = processFiles(
                  supportedFiles,
                  source,
                  outputRoot,
                  expanded.rules,
                  config.settings,
                  writeFiles
                )
                // обработка файлов

                val report = buildReport(
                  policyName = policyName,
                  startedAt = started,
                  finishedAt = now(),
                  source = source.toString,
                  output = if writeFiles then outputRoot.toString else "(предпросмотр)",
                  allFiles = allFiles,
                  supportedFiles = supportedFiles,
                  fileResults = fileResults
                )
                // сборка отчета

                Logger.log(
                  config.settings.logFile,
                  s"Обработка завершена: политика=$policyName, обработано=${report.filesProcessed}, " +
                    s"пропущено=${report.filesSkipped}, ошибок=${report.filesFailed}"
                )

                report
              }
          }
      }
    }

  // состояние замен протаскивается через файлы, чтобы нумерация псевдонимов
  // была единой для всего запуска
  private def processFiles( // состояние передаётся из файла в файл, чтобы одинаковый email/пароль заменялся одинаково во всех файлах
      files: Seq[Path],
      source: Path, // исходный путь
      outputRoot: Path, // куда писать результат
      rules: Seq[Rule],
      settings: AppSettings,
      writeFiles: Boolean // предпросмотр
  ): (Seq[FileResult], ReplacementState) =
    files.foldLeft((Vector.empty[FileResult], ReplacementState.empty)) {
      // мы идем по файлам и копим результат, изначально список пустой, состояние замены тоже
      case ((results, state), file) =>
        val (result, nextState) =
          processFile(file, source, outputRoot, rules, settings, writeFiles, state)
        (results :+ result, nextState)
    }

  private def processFile( // обработка одного файла
      file: Path,
      source: Path,
      outputRoot: Path,
      rules: Seq[Rule],
      settings: AppSettings,
      writeFiles: Boolean,
      state: ReplacementState
  ): (FileResult, ReplacementState) =
    val relative = {
      if Try(Files.isDirectory(source)).getOrElse(false) then source.relativize(file).toString
      else file.getFileName.toString
      // путь файла
    }

    val outPath = outputRoot.resolve(relative) // путь куда записываем отработанный файл

    readText(file) match
      case Left(error) => // если файл не прочитался, то ошибка
        (
          FileResult(
            relativePath = relative,
            sourcePath = file.toString,
            outputPath = outPath.toString,
            replaced = 0,
            conflicts = 0,
            error = Some(error),
            skipped = false,
            preview = None
          ),
          state
        )

      case Right(text) =>
        val (result, nextState) = processText(text, rules, state)

        if !writeFiles then // если включен режим предпросмотра, то файлы не пишем
          (
            FileResult(
              relativePath = relative,
              sourcePath = file.toString,
              outputPath = outPath.toString,
              replaced = result.applied.size,
              conflicts = result.conflicts.size,
              error = None,
              skipped = false,
              preview = Some(result)
            ),
            nextState
          )
        else if outputExists(outPath) && !settings.overwriteExistingFiles then {
          // если это не предпросмотр, но на выходе уже есть файл с таким названием и перезапись запрещена
          (
            FileResult(
              relativePath = relative,
              sourcePath = file.toString,
              outputPath = outPath.toString,
              replaced = result.applied.size,
              conflicts = result.conflicts.size,
              error = None,
              skipped = true,
              preview = Some(result)
            ),
            nextState
          )
        } else
          writeText(outPath, result.outputText) match
            // пробуем записать текст в файл
            case Left(error) =>
              (
                FileResult(
                  relativePath = relative,
                  sourcePath = file.toString,
                  outputPath = outPath.toString,
                  replaced = 0,
                  conflicts = 0,
                  error = Some(error),
                  skipped = false,
                  preview = None
                ),
                nextState
              )

            case Right(_) =>
              (
                FileResult(
                  relativePath = relative,
                  sourcePath = file.toString,
                  outputPath = outPath.toString,
                  replaced = result.applied.size,
                  conflicts = result.conflicts.size,
                  error = None,
                  skipped = false,
                  preview = Some(result)
                ),
                nextState
              )

  private def readText(file: Path): Either[AppError, String] = // читаем текст
    Try(new String(Files.readAllBytes(file), StandardCharsets.UTF_8))
      .toEither
      .left
      .map(error => AppError.FileReadFailed(file.toString, ErrorReason.fromThrowable(error)))

  private def writeText(path: Path, text: String): Either[AppError, Unit] = // создает папки и записывает текст
    Try {
      if path.getParent != null then Files.createDirectories(path.getParent)
      Files.write(path, text.getBytes(StandardCharsets.UTF_8))
      ()
    }.toEither.left.map(error => AppError.FileWriteFailed(path.toString, ErrorReason.fromThrowable(error)))

  private def collectAllFiles(source: Path): Either[AppError, Seq[Path]] = // возвращает список файлов для обработки
    Try {
      if Files.isRegularFile(source) then Seq(source)
      else if Files.isDirectory(source) then
        val stream = Files.walk(source)
        try stream.iterator.asScala.filter(path => Files.isRegularFile(path)).toVector
        finally stream.close()
      else Seq.empty
    }.toEither.left.map(error => AppError.FileReadFailed(source.toString, ErrorReason.fromThrowable(error)))

  private def isSupported(path: Path, extensions: Seq[String]): Boolean = // можно ли обрабатывать файл по расширению
    if extensions.isEmpty then true
    else
      val name = path.getFileName.toString.toLowerCase
      extensions.exists(extension => name.endsWith(extension.toLowerCase))

  private def fileExists(path: Path): Boolean = // существует ли путь
    Try(Files.exists(path)).getOrElse(false)

  private def toAbsolutePath(raw: String): Either[AppError, Path] =
    Try(Paths.get(raw).toAbsolutePath.normalize)
      .toEither
      .left
      .map(error => AppError.InvalidPath(raw, ErrorReason.fromThrowable(error)))
      // нормализуем и делаем путь абсолютным

  private def outputExists(path: Path): Boolean = // существует ли путь
    Try(Files.exists(path)).getOrElse(false)

  private def buildReport( // сборка итогового отчета 
      policyName: String,
      startedAt: String,
      finishedAt: String,
      source: String,
      output: String,
      allFiles: Seq[Path],
      supportedFiles: Seq[Path],
      fileResults: Seq[FileResult]
  ): OperationReport =
    val filesFound = allFiles.size
    val filesSkippedByExtension = filesFound - supportedFiles.size
    val filesSkippedExisting = fileResults.count(_.skipped)
    val filesProcessed = fileResults.count(result => result.error.isEmpty && !result.skipped)
    val filesFailed = fileResults.count(_.error.nonEmpty)
    val replacedTotal = fileResults
      .filter(result => result.error.isEmpty && !result.skipped)
      .map(_.replaced)
      .sum
    val conflictsTotal = fileResults.map(_.conflicts).sum

    OperationReport(
      policyName = policyName,
      startedAt = startedAt,
      finishedAt = finishedAt,
      source = source,
      output = output,
      filesFound = filesFound,
      filesProcessed = filesProcessed,
      filesSkipped = filesSkippedByExtension + filesSkippedExisting,
      filesFailed = filesFailed,
      replacedTotal = replacedTotal,
      conflictsTotal = conflictsTotal,
      fileResults = fileResults
    )
